! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "config.h"

!>\brief Controllo di qualità spaziale.
!! Questo modulo effettua un controllo spaziale dei dati distribuiti
!! su stazioni che devono essere il piu` possibili equidistribuite
!! nello spazio. Vengono costruiti con una routine NCAR dei triangoli
!! che uniscono tre stazioni adiacenti in maniera che essi siano il
!! piu` possibile equilateri. Questo fa in maniera che da una stazione
!! partano vari raggi (da un minimo di 2 a n) che rappresentano
!! al tempo stesso i vari orientamenti nello spazio e le stazioni 
!! possibilmente piu` vicine. Vengono quindi estratti tutti i vertici
!! dei triangoli appartenenti a una stazione, ordinati per poi
!! togliere i valori doppi e considerati per calcolare i vari gradienti
!! nelle varie direzioni. SOLO SE TUTTI QUESTI GRADIENTI
!! SONO TUTTI DELLO STESSO SEGNO IL GRADIENTE MINIMO VERRA`
!! CONSIDERATO PER DETERMINARE L'ATTENDIBILITÀ DEL DATO.
!!
!! Il controllo e` piu` scadente sulle stazioni che appartengono
!! al poligono che racchiude tutte le stazioni in quanto il controllo
!! su di esse viene effettuato su un angolo inferiore ai 180 gradi.
!!
!! Esse potrebbero eventualmente essere escluse dal controllo, opzione 
!! attualmente non in uso.
!!
!! Le coordinate delle stazioni vengono proiettate su un piano 
!! secondo le specifiche passate al metodo init.
!!
!! ATTENDIBILITÀ DEL DATO
!!
!! Il dato della stazione in esame e` considerato errato se il vettore dei 
!! dati contiene almeno tre stazioni con dati presenti e se sono 
!! verificate entrambe le condizioni:
!! 
!! a) Tutte le stazioni attorno alla stazione in esame hanno, con essa, 
!! un gradiente superiore a 
!!
!! 	SOGLIA/DIST + GRADMAX
!!
!! dove SOGLIA e GRADMAX sono parametri passati in input e DIST e` la 
!! distanza sulla terra tra la stazione in esame e la stazione sulla 
!! poligonale che viene utilizzata.
!!
!! b) I gradienti sono tutti dello stesso segno.
!! La stazione e` un massimo o un minimo.
!!
!! N.B.
!! Se una stazione risulta circondata da dati mancanti nessun controllo 
!! viene fatto su di essa e la confidenza risulta assente e quella eventualmente calcolata
!! precedentemente viene cancellata.
!!
!!23/9/1998
!!
!!modificato l'algoritmo:
!!e' stata definita una soglia per la distanza, oltre la quale le stazioni
!!vengono considerate scorrelate e la coppia di stazioni non viene 
!!considerata per il controllo, al pari di un dato mancante.
!!
!!\ingroup qc

!> \todo ottimizzare la lettura degli extreme nel caso il periodo da controllare sia a cavallo di due anni.
!!\todo Bisognerebbe validare il volume sottoposto al controllo per vedere se ha i requisiti.
!!
!! Programma Esempio del controllo spaziale :
!! \include  v7d_qcspa.f90


module modqcspa

use geo_coord_class
use geo_proj_class
use space_utilities
use vol7d_class
use modqc
use modqccli
use file_utilities
use log4fortran
use char_utilities
use datetime_class
!use array_utilities
!use io_units
#ifdef HAVE_DBALLE
use vol7d_dballe_class
#endif

implicit none


character (len=255),parameter:: subcategoryspa="QCspa"

integer, parameter :: spa_nvar=1
CHARACTER(len=10) :: spa_btable(spa_nvar)=(/"B12101"/) !< variable wmo code table for normalization.
!> standard coefficients for orizontal gradient normalization
real, parameter :: spa_a(spa_nvar) = (/1.e5/)
!> standard coefficients for orizontal gradient normalization
real, parameter :: spa_b(spa_nvar) = (/273.15/)

!>\brief Oggetto principale per il controllo di qualità
type :: qcspatype
  type (vol7d),pointer :: v7d => null() !< Volume dati da controllare
  integer,pointer :: data_id_in(:,:,:,:,:) => null()  !< Indici dati del DB in input
  integer,pointer :: data_id_out(:,:,:,:,:) => null() !< Indici dati del DB in output
  integer :: category !< log4fortran
  integer :: ndp !< number of points
  type(xy),pointer :: co(:) => null()
  type (triangles) :: tri !< triangles
  type (qcclitype) :: qccli !< qccli part for normalization
  type (vol7d) :: clima !< Clima spaziale di tutte le variabili da controllare
  character(len=20):: operation !< Operation to execute ("gradient"/"run")
  !logical :: writeheader  !< have to write header in gradient files
  !integer :: grunit !< unit used internally to write gradient
end type qcspatype


!>\brief Inizializzazione
interface init
  module procedure qcspainit
end interface

!>\brief  Allocazione di memoria
interface alloc
  module procedure qcspaalloc
end interface

!>\brief Cancellazione
interface delete
  module procedure qcspadelete
end interface

PRIVATE
PUBLIC spa_nvar, spa_btable, spa_a, spa_b, qcspatype, init, alloc, delete, &
 qcspatri, quaconspa


contains

!>\brief Init del controllo di qualità spaziale.
!!Effettua la lettura dei file e altre operazioni di inizializzazione.

subroutine qcspainit(qcspa,v7d,var, timei, timef, coordmin, coordmax, data_id_in,extremepath,spatialpath, &
#ifdef HAVE_DBALLE
 dsne,usere,passworde,&
 dsnspa,userspa,passwordspa,&
#endif
 height2level,operation,categoryappend)

type(qcspatype),intent(in out) :: qcspa !< Oggetto per il controllo spaziale
type (vol7d),intent(in),target:: v7d !< Il volume Vol7d da controllare
character(len=*),INTENT(in) :: var(:)!< variabili da importare secondo la tabella B locale o relativi alias
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'importazione
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
!>estremi temporali (inizio e fine) dell'estrazione per l'importazione
TYPE(datetime),INTENT(in),optional :: timei, timef
integer,intent(in),optional,target:: data_id_in(:,:,:,:,:) !< Indici dei dati in DB
character(len=*),intent(in),optional :: extremepath !< file con il volume del extreme
character(len=*),intent(in),optional :: spatialpath !< file with spatial ndi
logical ,intent(in),optional :: height2level   !< use conventional level starting from station height
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
character(len=*), optional :: operation !< Operation to execute ("gradient"/"run")

#ifdef HAVE_DBALLE
type (vol7d_dballe) :: v7d_dballespa
character(len=*),intent(in),optional :: dsne
character(len=*),intent(in),optional :: usere
character(len=*),intent(in),optional :: passworde
character(len=*),intent(in),optional :: dsnspa
character(len=*),intent(in),optional :: userspa
character(len=*),intent(in),optional :: passwordspa
character(len=512) :: ldsnspa
character(len=512) :: luserspa
character(len=512) :: lpasswordspa
#endif

integer :: iuni,i
TYPE(vol7d_network)  :: network
character(len=512) :: filepathspa
character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategoryspa)//"."//trim(categoryappend))
qcspa%category=l4f_category_get(a_name)

call delete(qcspa%tri)

nullify ( qcspa%data_id_in )
nullify ( qcspa%data_id_out )


! riporto il volume dati nel mio oggetto
qcspa%v7d => v7d

qcspa%operation=optio_c(operation,20)
filepathspa=optio_c(spatialpath,512)

!check options
if (qcspa%operation  /= "gradient" .and. qcspa%operation  /= "run") then
  call l4f_category_log(qcspa%category,L4F_ERROR,"operation is wrong: "//qcspa%operation)
  call raise_error()
end if

!inglobe id
if (present(data_id_in))then
  qcspa%data_id_in => data_id_in
end if

! load extreme
call init(qcspa%qccli,v7d,var, timei, timef, data_id_in,&
 macropath=cmiss, climapath=cmiss, extremepath=extremepath, &
#ifdef HAVE_DBALLE
 dsncli=cmiss,dsnextreme=dsne,user=usere,password=passworde,&
#endif
 height2level=height2level,categoryappend=categoryappend)


! now load spatial clima 

if (qcspa%operation == "run") then

  call init(network,"qcspa-ndi")

#ifdef HAVE_DBALLE
  call optio(dsnspa,ldsnspa)
  call optio(userspa,luserspa)
  call optio(passwordspa,lpasswordspa)

  if (c_e(filepathspa) .and. (c_e(ldsnspa).or.c_e(luserspa).or.c_e(lpasswordspa))) then
    call l4f_category_log(qcspa%category,L4F_ERROR,"filepath  defined together with dba options")
    call raise_error()
  end if

  if (.not. c_e(ldsnspa)) then

#endif

    if (.not. c_e(filepathspa)) then
      filepathspa=get_package_filepath('qcspa-ndi.v7d', filetype_data)
    end if

    if (c_e(filepathspa))then

      select case (trim(lowercase(suffixname(filepathspa))))

      case("v7d")
        iuni=getunit()
        call import(qcspa%clima,filename=filepathspa,unit=iuni)
        close (unit=iuni)
        
#ifdef HAVE_DBALLE
      case("bufr")
        call init(v7d_dballespa,file=.true.,filename=filepathspa,categoryappend=trim(a_name)//".clima")
        call import(v7d_dballespa,var=var, &
         varkind=(/("r",i=1,size(var))/),attr=(/"*B33209"/),attrkind=(/"b"/),network=network)
        call copy(v7d_dballespa%vol7d,qcspa%clima)
        call delete(v7d_dballespa)
#endif

      case default
        call l4f_category_log(qcspa%category,L4F_ERROR,&
         "file type not supported (use .v7d or .bufr suffix only): "//trim(filepathspa))
        call raise_error()
      end select

    else
      call l4f_category_log(qcspa%category,L4F_WARN,"spatial clima volume not iniziatized: spatial QC will not be possible")
      call init(qcspa%clima)
      call raise_fatal_error()
    end if

#ifdef HAVE_DBALLE
  else

    call l4f_category_log(qcspa%category,L4F_DEBUG,"init v7d_dballespa")
    call init(v7d_dballespa,dsn=ldsnspa,user=luserspa,password=lpasswordspa,write=.false.,&
     file=.false.,categoryappend=trim(a_name)//".spa")
    call l4f_category_log(qcspa%category,L4F_DEBUG,"import v7d_dballespa")
    call import(v7d_dballespa,var=var, &
     varkind=(/("r",i=1,size(var))/),attr=(/"*B33209"/),attrkind=(/"b"/),network=network)
    call copy(v7d_dballespa%vol7d,qcspa%clima)
    call delete(v7d_dballespa)
    
  end if
#endif
end if


return
end subroutine qcspainit


subroutine qcspatri(qcspa,proj_type, lov, zone, xoff, yoff, &
 longitude_south_pole, latitude_south_pole, angle_rotation, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor, &
 latin1, latin2, lad, projection_center_flag, &
 ellips_smaj_axis, ellips_flatt, ellips_type)

type(qcspatype),intent(in out) :: qcspa !< Oggetto per il controllo climatico
CHARACTER(len=*),INTENT(in),OPTIONAL :: proj_type !< type of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lov !< line of view, also known as reference longitude or orientation of the grid (polar projections)
INTEGER,INTENT(in),OPTIONAL :: zone !< Earth zone (mainly for UTM), sets lov to the correct zone central meridian
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xoff !< offset on x axis (false easting)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: yoff !< offset on y axis (false northing)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_south_pole !< longitude of the southern pole of projection 
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_south_pole !< latitude of the southern pole of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: angle_rotation !< angle of rotation of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_stretch_pole !< longitude of the pole of stretching
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_stretch_pole !< latitude of the pole of stretching
DOUBLE PRECISION,INTENT(in),OPTIONAL :: stretch_factor !< stretching factor
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latin1 !< first standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latin2 !< second standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lad !< latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,INTENT(in),OPTIONAL :: projection_center_flag !< flag indicating which pole is represented
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_smaj_axis !< Earth semi-major axis
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_flatt !< Earth flattening
INTEGER,INTENT(in),OPTIONAL :: ellips_type !< number in the interval [1,nellips] indicating a predefined ellipsoid, alternative to the previous arguments


integer :: status
TYPE(geo_proj) :: geoproj
REAL(kind=fp_geo) :: lat(size(qcspa%v7d%ana)),lon(size(qcspa%v7d%ana))
CHARACTER(len=80) :: proj_type_l ! local type of projection
double precision :: lov_l, latin1_l,latin2_l
integer :: projection_center_flag_l

proj_type_l = optio_c(proj_type,80)

lov_l = optio_d(lov)
latin1_l = optio_d(latin1)
latin2_l = optio_d(latin2)
projection_center_flag_l=optio_l(projection_center_flag)

if (.not. c_e(proj_type_l)) then
  proj_type_l = "lambert"
  lov_l = 10.D0
  latin1_l = 60.D0
  latin2_l = 30.D0
  projection_center_flag_l=1
end if

geoproj = geo_proj_new(proj_type_l, lov_l, zone, xoff, yoff, &
 longitude_south_pole, latitude_south_pole, angle_rotation, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor, &
 latin1_l, latin2_l, lad, projection_center_flag_l, &
 ellips_smaj_axis, ellips_flatt, ellips_type)

call getval(qcspa%v7d%ana%coord, lon, lat)

!print*,"size",size(lon),size(lat)
!print*,lat,lon
call proj(geoproj,lon,lat,qcspa%co%x,qcspa%co%y)
!print*,"size x y ",size(qcspa%x),size(qcspa%y)
!print*,qcspa%x,qcspa%y

!triangulate
status = triangles_compute(qcspa%co,qcspa%tri)

!qcspa%nt,qcspa%ipt,qcspa%nl,qcspa%ipl)

if (status /= 0) then
  call l4f_category_log(qcspa%category,L4F_ERROR,"contng error status="//t2c(status))
  !call raise_error()
end if

end subroutine qcspatri


!>\brief Allocazioni di memoria
subroutine qcspaalloc(qcspa)
                                ! pseudo costruttore con distruttore automatico

type(qcspatype),intent(in out) :: qcspa !< Oggetto per il controllo climatico

integer :: istatt
integer :: sh(5)

! se ti sei dimenticato di deallocare ci penso io
call  qcspadealloc(qcspa)


!!$if (associated (qcspa%v7d%dativar%r )) then
!!$  nv=size(qcspa%v7d%dativar%r)
!!$
!!$  allocate(qcspa%valminr(nv),stat=istat)
!!$  istatt=istatt+istat
!!$  allocate(qcspa%valmaxr(nv),stat=istat)
!!$  istatt=istatt+istat
!!$
!!$  if (istatt /= 0) ier=1
!!$
!!$end if

if (associated(qcspa%data_id_in))then
  sh=shape(qcspa%data_id_in)
  allocate (qcspa%data_id_out(sh(1),sh(2),sh(3),sh(4),sh(5)),stat=istatt)
  if (istatt /= 0)then
    call l4f_category_log(qcspa%category,L4F_ERROR,"allocate error")
    call raise_error("allocate error")
  else
    qcspa%data_id_out=imiss
  end if
end if

if (associated(qcspa%v7d%ana))then
  qcspa%ndp=size(qcspa%v7d%ana)
  qcspa%tri = triangles_new(qcspa%ndp)
  allocate(qcspa%co(qcspa%ndp))
end if

end subroutine qcspaalloc


!>\brief Deallocazione della memoria
subroutine qcspadealloc(qcspa)
                                ! pseudo distruttore

type(qcspatype),intent(in out) :: qcspa !< Oggetto per l controllo climatico

!!$if ( associated ( qcspa%valminr)) then
!!$  deallocate(qcspa%valminr)
!!$end if
!!$
!!$if ( associated ( qcspa%valmaxr)) then
!!$  deallocate(qcspa%valmaxr)
!!$end if

if (associated(qcspa%data_id_out)) then
  deallocate (qcspa%data_id_out)
  nullify (qcspa%data_id_out)
end if
call delete(qcspa%tri)
if (associated(qcspa%co)) deallocate(qcspa%co)

end subroutine qcspadealloc


!>\brief Cancellazione


subroutine qcspadelete(qcspa)
                                ! decostruttore a mezzo
type(qcspatype),intent(in out) :: qcspa !< Oggetto per il controllo climatico

call qcspadealloc(qcspa)

call delete(qcspa%qccli)

qcspa%ndp=imiss

!delete logger
call l4f_category_delete(qcspa%category)

return
end subroutine qcspadelete


!>\brief Controllo di Qualità spaziale.
!!Questo è il vero e proprio controllo di qualità spaziale.

SUBROUTINE quaconspa (qcspa,noborder,battrinv,battrcli,battrout,&
 anamask,timemask,levelmask,timerangemask,varmask,networkmask)


type(qcspatype),intent(in out) :: qcspa !< Oggetto per il controllo di qualità
logical,intent(in),optional :: noborder !< Exclude border from QC
character (len=10) ,intent(in),optional :: battrinv !< attributo invalidated in input
character (len=10) ,intent(in),optional :: battrcli !< attributo con la confidenza climatologica in input
character (len=10) ,intent(in),optional :: battrout !< attributo con la confidenza spaziale in output
logical ,intent(in),optional :: anamask(:) !< Filtro sulle anagrafiche
logical ,intent(in),optional :: timemask(:) !< Filtro sul tempo
logical ,intent(in),optional :: levelmask(:) !< Filtro sui livelli
logical ,intent(in),optional :: timerangemask(:) !< filtro sui timerange
logical ,intent(in),optional :: varmask(:) !< Filtro sulle variabili
logical ,intent(in),optional :: networkmask(:) !< Filtro sui network

                                !REAL(kind=fp_geo) :: lat,lon
                                !local
integer :: indbattrinv,indbattrcli,indbattrout
logical :: anamaskl(size(qcspa%v7d%ana)), timemaskl(size(qcspa%v7d%time)), levelmaskl(size(qcspa%v7d%level)), &
 timerangemaskl(size(qcspa%v7d%timerange)), varmaskl(size(qcspa%v7d%dativar%r)), networkmaskl(size(qcspa%v7d%network)) 

integer :: indana ,  indtime ,indlevel ,indtimerange ,inddativarr, indnetwork
integer :: indcana ,  indctime ,indclevel ,indctimerange ,indcdativarr, indcnetwork
real :: datoqui,datola,datila(size(qcspa%v7d%time)),climaquii, climaquif
                                !integer, allocatable :: indcanav(:)

                                !TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time
!YPE(vol7d_level):: level
TYPE(vol7d_network):: network
type(timedelta) :: deltato,deltat 

integer :: ivert(50),i,ipos,ineg,it,itrov,iv,ivb,kk,iindtime,grunit
double precision :: distmin=1000.d0,distscol=100000.d0
double precision :: dist,grad,gradmin
integer (kind=int_b) :: flag
!!$CHARACTER(len=vol7d_ana_lenident) :: ident
character(len=512) :: filename
logical :: exist
integer :: ind

                                !call qcspa_validate (qcspa)

if (size(qcspa%v7d%ana) < 3 ) then
  call l4f_category_log(qcspa%category,L4F_WARN,"number of station < 3; do nothing")
  return
end if

!localize optional parameter
if (present(battrinv))then
  indbattrinv = index_c(qcspa%v7d%datiattr%b(:)%btable, battrinv)
else
  indbattrinv = index_c(qcspa%v7d%datiattr%b(:)%btable, qcattrvarsbtables(1))
end if

if (present(battrcli))then
  indbattrcli = index_c(qcspa%v7d%datiattr%b(:)%btable, battrcli)
else
  indbattrcli = index_c(qcspa%v7d%datiattr%b(:)%btable, qcattrvarsbtables(2))
end if

if (present(battrout))then
  indbattrout = index_c(qcspa%v7d%datiattr%b(:)%btable, battrout)
else
  indbattrout = index_c(qcspa%v7d%datiattr%b(:)%btable, qcattrvarsbtables(4))
end if


! some checks on input
!if (indbattrinv <=0 .or. indbattrcli <= 0 .or. indbattrout <= 0 ) then
if (indbattrout <= 0 ) then

  call l4f_category_log(qcspa%category,L4F_ERROR,"error finding attribute index for output")
  call raise_error()

end if

!!$if (qcspa%operation == "gradient") then
!!$
!!$  !check for gradient operation
!!$  if ( size(qcspa%v7d%level)      > 1 .or.&
!!$       size(qcspa%v7d%timerange)  > 1 .or.&
!!$       size(qcspa%v7d%dativar%r)  > 1 ) then
!!$    call l4f_category_log(qcspa%category,L4F_ERROR,"gradient operation manage one level/timerange/var only")
!!$    call raise_error()
!!$  end if
!!$
!!$end if

! set other local variable from optional parameter
if(present(anamask)) then
  anamaskl = anamask
else
  anamaskl = .true.
endif
if(present(timemask)) then
  timemaskl = timemask
else
  timemaskl = .true.
endif
if(present(levelmask)) then
  levelmaskl = levelmask
else
  levelmaskl = .true.
endif
if(present(timerangemask)) then
  timerangemaskl = timerangemask
else
  timerangemaskl = .true.
endif
if(present(varmask)) then
  varmaskl = varmask
else
  varmaskl = .true.
endif
if(present(networkmask)) then
  networkmaskl = networkmask
else
  networkmaskl = .true.
endif

! do not touch data that do not pass QC
qcspa%v7d%voldatiattrb(:,:,:,:,:,:,indbattrout)=ibmiss
 
!print *,"prima normalize"
!print *,qcspa%v7d%voldatir
! normalize data in space and time
call vol7d_normalize_data(qcspa%qccli)
!print *,"dopo normalize"
!print *,qcspa%v7d%voldatir

! triangulate
call qcspatri(qcspa)



! compute some index for spatial clima
!! compute the conventional generic datetime
!!cyclicdt = cyclicdatetime_new(chardate="/////////") !TMMGGhhmm
time=cyclicdatetime_to_conventional(cyclicdatetime_new(chardate="/////////"))  !TMMGGhhmm
!!call init(time, year=1007, month=1, day=1, hour=01, minute=01)


if (qcspa%operation == "run") then
  call init(network,"qcspa-ndi")
  !!indcana          = firsttrue(qcspa%clima%ana     == ana)
  indcnetwork      = index(qcspa%clima%network               , network)
  indctime         = index(qcspa%clima%time                  ,  time)
end if

do indtime=1,size(qcspa%v7d%time)
  if (.not.timemaskl(indtime)) cycle
  call l4f_category_log(qcspa%category,L4F_INFO,&
   "Check time:"//t2c(qcspa%v7d%time(indtime)) )

  do indlevel=1,size(qcspa%v7d%level)
    do indtimerange=1,size(qcspa%v7d%timerange)
      do inddativarr=1,size(qcspa%v7d%dativar%r)

        ind=index_c(spa_btable,qcspa%v7d%dativar%r(inddativarr)%btable)

        if (qcspa%operation == "gradient") then
                                ! open file to write gradient

          filename=trim(to_char(qcspa%v7d%level(indlevel)))//&
           "_"//trim(to_char(qcspa%v7d%timerange(indtimerange)))//&
           "_"//trim(qcspa%v7d%dativar%r(inddativarr)%btable)//&
           ".grad"

          call l4f_category_log(qcspa%category,L4F_INFO,"try to open gradient file; filename below")
          call l4f_category_log(qcspa%category,L4F_INFO,filename)

          inquire(file=filename, exist=exist)
          
          grunit=getunit()
          if (grunit /= -1) then
                                !open (unit=grunit, file=t2c(timei)//"_"//t2c(timef)//".grad",STATUS='UNKNOWN', form='FORMATTED')
            open (grunit, file=filename ,STATUS='UNKNOWN', form='FORMATTED',position='APPEND')
          end if
                                ! say we have to write header in file
          if  (.not. exist) then
            call l4f_category_log(qcspa%category,L4F_INFO,"write header in gradient file")
            write (grunit,*) &
             qcspa%v7d%level(indlevel), &
             qcspa%v7d%timerange(indtimerange), &
             qcspa%v7d%dativar%r(inddativarr)
          end if
        end if


        do indnetwork=1,size(qcspa%v7d%network)
          do indana=1,size(qcspa%v7d%ana)

!!$            call l4f_log(L4F_INFO,"Index:"// t2c(indana)//t2c(indnetwork)//t2c(indlevel)//&
!!$             t2c(indtimerange)//t2c(inddativarr)//t2c(indtime))

            if (.not. anamaskl(indana).or. .not. levelmaskl(indlevel) .or. &
             .not. timerangemaskl(indtimerange) .or. .not. varmaskl(inddativarr) .or. .not. networkmaskl(indnetwork)) cycle
            
            datoqui = qcspa%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
            if (.not. c_e(datoqui)) cycle

            ! invalidated
            if (indbattrinv > 0) then
              if( invalidated(qcspa%v7d%voldatiattrb&
               (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) then
                call l4f_category_log(qcspa%category,L4F_WARN,&
                 "It's better to do a reform on ana to v7d after peeling, before spatial QC")
                cycle
              end if
            end if

            ! gross error check
            if (indbattrcli > 0) then
              if( .not. vdge(qcspa%v7d%voldatiattrb&
               (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrcli))) then
                call l4f_category_log(qcspa%category,L4F_WARN,&
                 "It's better to do a reform on ana to v7d after peeling, before spatial QC")
                cycle
              end if
            end if



            !!call init(anavar,"B07031" )
            !call init(anavar,"B07030" )
            !indanavar = 0
            !if (associated (qcspa%v7d%anavar%r)) then
            !  indanavar        = index(qcspa%v7d%anavar%r, anavar)
            !end if
            !if (indanavar <= 0 )cycle
            !altezza= qcspa%v7d%volanar(indana,indanavar,indnetwork)
                                !              call spa_level(altezza,level)

            if (qcspa%operation == "run") then

              indclevel        = index(qcspa%clima%level                 ,  qcspa%v7d%level(indlevel))
              indctimerange    = index(qcspa%clima%timerange             ,  qcspa%v7d%timerange(indtimerange))

                                ! attenzione attenzione TODO
                                ! se leggo da bufr il default è char e non reale
              indcdativarr     = index(qcspa%clima%dativar%r, qcspa%v7d%dativar%r(inddativarr))


#ifdef DEBUG
              call l4f_log(L4F_DEBUG,"Index:"// to_char(indctime)//to_char(indclevel)//&
               to_char(indctimerange)//to_char(indcdativarr)//to_char(indcnetwork))
#endif
              if ( indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
               .or. indcnetwork <= 0 ) cycle
            end if

            if (optio_log(noborder) .and. any(indana == qcspa%tri%ipl(:3*qcspa%tri%nl:3))) cycle

                                !	ITROV e` il numero di triangoli in cui e` presente il dato
            ITROV=0
                                !		cicla per tutti i triangoli
            DO IT=1,qcspa%tri%NT
                                !			se la stazione considerata e` in prima posizione
                                !			memorizza gli altri due vertici
              IF(qcspa%tri%IPT(3*IT-2).EQ.INDANA)THEN
                ITROV=ITROV+1
                IVERT(2*ITROV)=qcspa%tri%IPT(3*IT)
                IVERT(2*ITROV-1)=qcspa%tri%IPT(3*IT-1)
                cycle
              END IF
                                !			se la stazione considerata e` in seconda posizione
                                !			memorizza gli altri due vertici
              IF(qcspa%tri%IPT(3*IT-1).EQ.INDANA)THEN
                ITROV=ITROV+1
                IVERT(2*ITROV)=qcspa%tri%IPT(3*IT)
                IVERT(2*ITROV-1)=qcspa%tri%IPT(3*IT-2)
                cycle
              END IF
                                !			se la stazione considerata e` in terza posizione
                                !			memorizza gli altri due vertici
              IF(qcspa%tri%IPT(3*IT).EQ.INDANA)THEN
                ITROV=ITROV+1
                IVERT(2*ITROV)=qcspa%tri%IPT(3*IT-1)
                IVERT(2*ITROV-1)=qcspa%tri%IPT(3*IT-2)
                cycle
              END IF
            END DO
                                !	ITROV ora diviene il numero di vertici nell'intorno
                                !	della stazione trovati
            ITROV=ITROV*2

                                !	WRITE(*,*)'NUMERO VERTICI',ITROV
                                !	WRITE(*,*)'VERTICI TROVATI = ',IVERT

                                !	ordina i vettori dei vertici secondo valori decrescenti

            call sort(ivert(:itrov))

!!$  DO I=1,ITROV-1
!!$    DO KK=I+1,ITROV
!!$      IF(IVERT(I).LT.IVERT(KK))THEN
!!$        IC=IVERT(KK)
!!$        IVERT(KK)=IVERT(I)
!!$        IVERT(I)=IC
!!$      ENDIF
!!$    END DO
!!$  END DO
                                !	toglie i valori doppi dal vettore dei vertici
            IV=1
            DO KK=2,ITROV
              IF(IVERT(IV).NE.IVERT(KK))THEN
                IV=IV+1
                IVERT(IV)=IVERT(KK)
              ENDIF
            END DO
            IF (IV.GT.ITROV)IV=ITROV

                                !	WRITE(*,*)'NUMERO VERTICI puliti',IV
                                !	WRITE(*,*)'VERTICI PULITI = ',IVERT

                                !	inizia il controllo sulla stazione testando i gradienti
                                !	WRITE(*,*)'STAZIONE  ',INDANA
            Ipos=0
            Ineg=0
            IVB=0
            gradmin=huge(gradmin)
            DO I=1, IV
                                !find the nearest data in time
              datola = qcspa%v7d%voldatir  (ivert(i) ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )

                                ! invalidated
              if (indbattrinv > 0) then
                if( invalidated(qcspa%v7d%voldatiattrb&
                 (ivert(i),indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) then
                  datola=rmiss
                end if
              end if

                                ! gross error check
              if (indbattrcli > 0) then
                if( .not. vdge(qcspa%v7d%voldatiattrb&
                 (ivert(i),indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrcli))) then
                  datola=rmiss
                end if
              end if

              datila = qcspa%v7d%voldatir  (ivert(i) ,: ,indlevel ,indtimerange ,inddativarr, indnetwork )

              if (.not. c_e(datola))then
                deltato=timedelta_miss
                do iindtime=1,size(qcspa%v7d%time)
                  if (.not. c_e(datila(iindtime))) cycle
                                ! invalidated
                  if (indbattrinv > 0 ) then
                    if (invalidated(qcspa%v7d%voldatiattrb&
                     (ivert(i),iindtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) cycle
                  end if
                                ! gross error check
                  if (indbattrcli > 0 )then
                    if (.not. vdge(qcspa%v7d%voldatiattrb&
                     (ivert(i),iindtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrcli))) cycle
                  end if

                  if (iindtime < indtime) then
                    deltat=qcspa%v7d%time(indtime)-qcspa%v7d%time(iindtime)
                  else if (iindtime > indtime) then
                    deltat=qcspa%v7d%time(iindtime)-qcspa%v7d%time(indtime)
                  else
                    call l4f_category_log(qcspa%category,L4F_WARN,"somethings go wrong on ipotesys make in spatial QC")
                  end if

                  if (deltat < deltato) then
                    datola = datila(iindtime)
                    deltato = deltat
                  end if
                end do
              end if
              
              IF(.NOT.C_E(datola)) cycle
                                !	distanza tra le due stazioni
              dist = DISTANZA (qcspa%co(INDANA),qcspa%co(IVERT(I)))
              IF (DIST.EQ.0.)THEN
                call l4f_category_log(qcspa%category,L4F_ERROR,"distance from two station == 0.")
                call raise_error()
              END IF

#ifdef DEBUG
              call l4f_log (L4F_DEBUG,"distanza: "//t2c(dist))
#endif

              dist=max(dist,distmin)
                                !	    modifica 23/9/1998
                                !           se la distanza supera distscol, stazioni scorrelate - salta -
              if (dist > distscol) cycle
              IVB=IVB+1
                                !	valore del gradiente nella direzione delle due stazioni
              GRAD=(datoqui-datola)/(DIST)
              IF (GRAD >= 0.d0) Ipos=Ipos+1           ! se il gradiente e` positivo incrementa il contatore di positivi
              IF (GRAD <= 0.d0) Ineg=Ineg+1           ! se il gradiente e` negativo incrementa il contatore di negativi

              gradmin=min(gradmin,abs(grad))

            END DO

#ifdef DEBUG
            call l4f_log (L4F_DEBUG,"ivb: "//t2c(ivb))
#endif

            IF(IVB < 3) cycle      ! do nothing if valid gradients < 3

            IF (ipos == ivb .or. ineg == ivb)THEN  ! se tutti i gradienti sono dello stesso segno

              gradmin=sign(gradmin,dble(ipos-ineg))

              if (qcspa%operation == "gradient") then
                write(grunit,*)gradmin
              end if

              !  we normalize gradmin or denormalize climaqui after
              !              gradmin=gradmin*spa_a(ind) + spa_b(ind)


#ifdef DEBUG
              call l4f_log (L4F_DEBUG,"gradmin: "//t2c(gradmin))
#endif

              flag=bmiss

                                !ATTENZIONE TODO : inddativarr È UNA GRANDE SEMPLIFICAZIONE NON VERA SE TIPI DI DATO DIVERSI !!!!
              if (qcspa%operation == "run") then

                do indcana=1,size(qcspa%clima%ana)-1
                  climaquii=(qcspa%clima%voldatir(indcana  &
                   ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)&
                    - spa_b(ind))/spa_a(ind) ! denormalize

                  climaquif=(qcspa%clima%voldatir(indcana+1 &
                   ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)&
                    - spa_b(ind))/spa_a(ind) ! denormalize

#ifdef DEBUG
                  call l4f_log (L4F_DEBUG,"climaquii: "//t2c(climaquii))
                  call l4f_log (L4F_DEBUG,"climaquif: "//t2c(climaquif))
#endif

                  if ( c_e(climaquii) .and. c_e(climaquif )) then
                    
                    if ( (gradmin >= climaquii .and. gradmin < climaquif) .or. &
                     (indcana == 1 .and. gradmin < climaquif) .or. &
                     (indcana == size(qcspa%clima%ana)-1 .and. gradmin >= climaquii) ) then
                      
                      flag=qcspa%clima%voldatiattrb(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1)
                      
                      if ( associated ( qcspa%data_id_in)) then
#ifdef DEBUG
                        call l4f_log (L4F_DEBUG,"id: "//t2c(&
                         qcspa%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)))
#endif
                        qcspa%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)=&
                         qcspa%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)
                      end if
                    end if
                  end if
                end do
#ifdef DEBUG
                call l4f_log (L4F_INFO,"datoqui: "//t2c(datoqui))
                call l4f_log (L4F_INFO,"flag qcspa: "//t2c(flag))
#endif

              end if
            else
              flag=100_int_b
            end if
            if (qcspa%operation == "run") then
              !TODO controllare se flag = missing comporta rimozione della precedente flag; risposta: SI quando sarà chiusa https://github.com/ARPA-SIMC/dballe/issues/44
              qcspa%v7d%voldatiattrb(   indana, indtime, indlevel, indtimerange, inddativarr, indnetwork, indbattrout)=flag
            end if
          end do
        end do

        if (qcspa%operation == "gradient") then
          close (unit=grunit)
        end if
        
      end do
    end do
  end do
end do

!!$print*,"risultato"
!!$print *,qcspa%v7d%voldatiattrb(:,:,:,:,:,:,indbattrout)
!!$print*,"fine risultato"

return

contains

elemental double precision function DISTANZA (co1,co2)
type(xy), intent(in) :: co1,co2


distanza = sqrt((co2%x-co1%x)**2 + (co2%y-co1%y)**2)

end function DISTANZA

end subroutine quaconspa


end module modqcspa


!> \example v7d_qcspa.F90
!! Sample program for module qcspa

