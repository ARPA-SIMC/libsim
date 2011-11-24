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
!! Le coordinate delle stazioni vengono proiettate secondo le specifiche
!! passate al metodo init.
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
!! viene fatto su di essa e la confidenza rimane inalterata.
!!
!!23/9/1998
!!
!!modificato l'algoritmo:
!!e' stata definita una soglia per la distanza, oltre la quale le stazioni
!!vengono considerate scorrelate e la coppia di stazioni non viene 
!!considerata per il controllo, al pari di un dato mancante.
!!
!!\ingroup qc

!> \todo ottimizzare la lettura del clima nel caso il periodo da controllare sia a cavallo di due anni.
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

public

character (len=255),parameter:: subcategory="QCspa"

!>\brief Oggetto principale per il controllo di qualità
type :: qcspatype
  type (vol7d),pointer :: v7d => null() !< Volume dati da controllare
  type (vol7d) :: clima !< Clima di tutte le variabili da controllare
  integer,pointer :: data_id_in(:,:,:,:,:) => null()  !< Indici dati del DB in input
  integer,pointer :: data_id_out(:,:,:,:,:) => null() !< Indici dati del DB in output
  integer :: category !< log4fortran
  integer :: ndp !< number of points
  double precision,pointer :: x(:) => null(), y(:) => null()
  type (triangles) :: tri !< triangles
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


contains

!>\brief Init del controllo di qualità climatico.
!!Effettua la lettura dei file e altre operazioni di inizializzazione.

subroutine qcspainit(qcspa,v7d,var, timei, timef, coordmin, coordmax, data_id_in,climapath,&
#ifdef HAVE_DBALLE
 dsn,user,password,&
#endif
categoryappend)

type(qcspatype),intent(in out) :: qcspa !< Oggetto per il controllo climatico
type (vol7d),intent(in),target:: v7d !< Il volume Vol7d da controllare
character(len=*),INTENT(in) :: var(:)!< variabili da importare secondo la tabella B locale o relativi alias
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'importazione
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
!>estremi temporali (inizio e fine) dell'estrazione per l'importazione
TYPE(datetime),INTENT(in),optional :: timei, timef
integer,intent(in),optional,target:: data_id_in(:,:,:,:,:) !< Indici dei dati in DB
character(len=*),intent(in),optional :: climapath !< file con il volume del clima
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

#ifdef HAVE_DBALLE
type (vol7d_dballe) :: v7d_dballetmp
character(len=*),intent(in),optional :: dsn
character(len=*),intent(in),optional :: user
character(len=*),intent(in),optional :: password
character(len=512) :: ldsn
character(len=512) :: luser
character(len=512) :: lpassword
TYPE(datetime) :: ltimei, ltimef
integer :: yeari, yearf, monthi, monthf, dayi, dayf,&
 houri, minutei, mseci, hourf, minutef, msecf
#endif
 
integer :: iuni,i
character(len=512) :: filepath
character(len=512) :: a_name


call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
qcspa%category=l4f_category_get(a_name)

call delete(qcspa%tri)

nullify ( qcspa%data_id_in )
nullify ( qcspa%data_id_out )

! riporto il volume dati nel mio oggetto
qcspa%v7d => v7d

if (present(data_id_in))then
  qcspa%data_id_in => data_id_in
end if

call init(qcspa%clima)

call optio(climapath,filepath)

#ifdef HAVE_DBALLE

ltimei=datetime_miss
ltimef=datetime_miss
if (present (timei)) ltimei=timei
if (present (timef)) ltimef=timef
 
call getval  (ltimei+timedelta_new(minute=30), yeari, monthi, dayi, houri, minutei, mseci)
call getval  (ltimef+timedelta_new(minute=30), yearf, monthf, dayf, hourf, minutef, msecf)

if ( yeari == yearf .and. monthi == monthf .and. dayi == dayf) then

  call init(ltimei, 1001, monthi, 1, houri, minutei, mseci) 
  call init(ltimef, 1001, monthf, 1, hourf, minutef, msecf) 
  ltimei = ltimei +timedelta_new(minute=30)
  ltimef = ltimef +timedelta_new(minute=30)

else
                                ! if you span years or months or days I read all the climat dataset (should be optimized not so easy)
  ltimei=datetime_miss
  ltimef=datetime_miss

end if

call optio(dsn,ldsn)
call optio(user,luser)
call optio(password,lpassword)

if (c_e(filepath) .and. (c_e(ldsn).or.c_e(luser).or.c_e(lpassword))) then
  call l4f_category_log(qcspa%category,L4F_ERROR,"climapath and dba option defined together")
  call raise_error("climapath and dba option defined together")
end if

if (.not. c_e(ldsn)) then

#endif

  if (.not. c_e(filepath)) then

                                ! bufr import do not support attributes so it do not work for now
                                ! filepath=get_package_filepath('climaprec.bufr', filetype_data)
                                ! filepath="climaprec.bufr"
                                !#else
    filepath=get_package_filepath('climaprec.v7d', filetype_data)

  end if

  select case (trim(lowercase(suffixname(filepath))))

  case("v7d")
    iuni=getunit()
    call import(qcspa%clima,filename=filepath,unit=iuni)
    close (unit=iuni)

#ifdef HAVE_DBALLE
  case("bufr")
    call init(v7d_dballetmp,file=.true.,filename=filepath,categoryappend=trim(a_name)//".clima")
                                !call import(v7d_dballetmp)
    call import(v7d_dballetmp,var=var,coordmin=coordmin, coordmax=coordmax, timei=ltimei, timef=ltimef, &
     varkind=(/("r",i=1,size(var))/),attr=(/"*B33192"/),attrkind=(/"b"/))
    call copy(v7d_dballetmp%vol7d,qcspa%clima)
    call delete(v7d_dballetmp)
#endif

  case default
    call l4f_category_log(qcspa%category,L4F_ERROR,"file type not supported (user .v7d or .bufr suffix only): "//trim(filepath))
    call raise_error("file type not supported (user .v7d or .bufr suffix only): "//trim(filepath))
  end select

#ifdef HAVE_DBALLE
else

  call l4f_category_log(qcspa%category,L4F_DEBUG,"init v7d_dballetmp")
  call init(v7d_dballetmp,dsn=dsn,user=user,password=password,write=.false.,file=.false.,categoryappend=trim(a_name)//".clima")
  call l4f_category_log(qcspa%category,L4F_DEBUG,"import v7d_dballetmp")
  call import(v7d_dballetmp,var=var,coordmin=coordmin, coordmax=coordmax, timei=ltimei, timef=ltimef, &
   varkind=(/("r",i=1,size(var))/),attr=(/"*B33192"/),attrkind=(/"b"/))
  call copy(v7d_dballetmp%vol7d,qcspa%clima)
  call delete(v7d_dballetmp)

end if
#endif

qcspa%ndp=size(qcspa%v7d%ana)

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
call proj(geoproj,lon,lat,qcspa%x,qcspa%y)
!print*,"size x y ",size(qcspa%x),size(qcspa%y)
!print*,qcspa%x,qcspa%y

!triangulate
status = triangles_compute(qcspa%x,qcspa%y,qcspa%tri)

!qcspa%nt,qcspa%ipt,qcspa%nl,qcspa%ipl)

if (status /= 0) then
  call l4f_category_log(qcspa%category,L4F_ERROR,"contng error status="//t2c(status))
  !call raise_error("climapath and dba option defined together")
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

if (c_e(qcspa%ndp))then
  qcspa%tri = triangles_new(qcspa%ndp)
  allocate(qcspa%x(qcspa%ndp),qcspa%y(qcspa%ndp))
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

if (associated(qcspa%data_id_out))  deallocate (qcspa%data_id_out)
call delete(qcspa%tri)
if (associated(qcspa%x)) deallocate(qcspa%x)
if (associated(qcspa%y)) deallocate(qcspa%y)

end subroutine qcspadealloc


!>\brief Cancellazione


subroutine qcspadelete(qcspa)
                                ! decostruttore a mezzo
type(qcspatype),intent(in out) :: qcspa !< Oggetto per l controllo climatico

call qcspadealloc(qcspa)

call delete(qcspa%clima)

qcspa%ndp=imiss

!delete logger
call l4f_category_delete(qcspa%category)

return
end subroutine qcspadelete


!>\brief Controllo di Qualità spaziale.
!!Questo è il vero e proprio controllo di qualità spaziale.

SUBROUTINE quaconspa (qcspa,noborder,tbattrin,tbattrout,&
 anamask,timemask,levelmask,timerangemask,varmask,networkmask)


type(qcspatype),intent(in out) :: qcspa !< Oggetto per il controllo di qualità
logical,intent(in),optional :: noborder !< Exclude border from QC
character (len=10) ,intent(in),optional :: tbattrin !< attributo con la confidenza in input
character (len=10) ,intent(in),optional :: tbattrout !< attributo con la confidenza in output
logical ,intent(in),optional :: anamask(:) !< Filtro sulle anagrafiche
logical ,intent(in),optional :: timemask(:) !< Filtro sul tempo
logical ,intent(in),optional :: levelmask(:) !< Filtro sui livelli
logical ,intent(in),optional :: timerangemask(:) !< filtro sui timerange
logical ,intent(in),optional :: varmask(:) !< Filtro sulle variabili
logical ,intent(in),optional :: networkmask(:) !< Filtro sui network

CHARACTER(len=vol7d_ana_lenident) :: ident
                                !REAL(kind=fp_geo) :: lat,lon
integer :: mese, ora
                                !local
integer :: indtbattrin,indtbattrout
logical :: anamaskl(size(qcspa%v7d%ana)), timemaskl(size(qcspa%v7d%time)), levelmaskl(size(qcspa%v7d%level)), &
 timerangemaskl(size(qcspa%v7d%timerange)), varmaskl(size(qcspa%v7d%dativar%r)), networkmaskl(size(qcspa%v7d%network)) 

integer :: indana , indanavar, indtime ,indlevel ,indtimerange ,inddativarr, indnetwork
integer :: indcana,           indctime,indclevel,indctimerange,indcdativarr,indcnetwork
real :: datoqui,datola,climaquii,climaquif, altezza,datila(size(qcspa%v7d%time))
integer :: iarea
                                !integer, allocatable :: indcanav(:)

                                !TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time, nintime
TYPE(vol7d_level):: level
type(vol7d_var)  :: anavar
type(timedelta) :: deltato,deltat 

integer :: ivert(50),i,ipos,ineg,it,itrov,iv,ivb,kk,iindtime
double precision :: distmin=1000.d0,distscol=300000.d0
double precision :: dist,grad,gradmin
integer (kind=int_b) :: flag

                                !call qcspa_validate (qcspa)

if (present(tbattrin))then
  indtbattrin = index_c(qcspa%v7d%dativarattr%r(:)%btable, tbattrin)
else
  indtbattrin=1
end if

if (present(tbattrout))then
  indtbattrout = index_c(qcspa%v7d%dativarattr%r(:)%btable, tbattrout)
else
  indtbattrout=2
end if

if (indtbattrin <=0 .or. indtbattrout <= 0 ) then

  call l4f_category_log(qcspa%category,L4F_ERROR,"error finding attribute index in/out")
  call raise_error("error finding attribute index in/out")

end if

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

qcspa%v7d%voldatiattrb(:,:,:,:,:,:,indtbattrout)=ibmiss


do indana=1,size(qcspa%v7d%ana)

  call qcspatri(qcspa)

                                !  iarea= supermacroa(qcspa%in_macroa(indana))
  iarea= 1
  write(ident,'("BOX-",i2.2,"*")')iarea   ! macro-area
                                !lat=0.0d0
                                !lon=0.0d0
                                !write(ident,'("BOX-",2i2.2)')iarea,lperc   ! macro-area e percentile
                                !call init(ana,lat=lat,lon=lon,ident=ident)

                                !allocate (indcanav(count(match(qcspa%clima%ana(:)%ident,ident))))
                                !indcanav=match(qcspa%clima%ana(:)%ident,ident))))

  do indnetwork=1,size(qcspa%v7d%network)
    do indlevel=1,size(qcspa%v7d%level)
      do indtimerange=1,size(qcspa%v7d%timerange)
        do inddativarr=1,size(qcspa%v7d%dativar%r)
          do indtime=1,size(qcspa%v7d%time)

!!$            call l4f_log(L4F_INFO,"Index:"// t2c(indana)//t2c(indnetwork)//t2c(indlevel)//&
!!$             t2c(indtimerange)//t2c(inddativarr)//t2c(indtime))
!!$
!!$  forall (indnetwork=1:size(qcspa%v7d%network), &
!!$   indlevel=1:size(qcspa%v7d%level), &
!!$   indtimerange=1:size(qcspa%v7d%timerange), &
!!$   inddativarr=1:size(qcspa%v7d%dativar%r), &
!!$   indtime=1:size(qcspa%v7d%time))

            if (anamaskl(indana).and.timemaskl(indtime).and.levelmaskl(indlevel).and. &
             timerangemaskl(indtimerange).and.varmaskl(inddativarr).and.networkmaskl(indnetwork).and.&
             c_e(qcspa%v7d%voldatir(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork))) cycle

            if( invalidated(qcspa%v7d%voldatiattrb&
             (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrin))) cycle

            nintime=qcspa%v7d%time(indtime)+timedelta_new(minute=30)
            CALL getval(nintime, month=mese, hour=ora)
            call init(time, year=1001, month=mese, day=1, hour=ora, minute=00)

            !!call init(anavar,"B07031" )
            !call init(anavar,"B07030" )
            !indanavar = 0
            !if (associated (qcspa%v7d%anavar%r)) then
            !  indanavar        = index(qcspa%v7d%anavar%r, anavar)
            !end if
            !if (indanavar <= 0 )cycle
            !altezza= qcspa%v7d%volanar(indana,indanavar,indnetwork)
                                !              call spa_level(altezza,level)
            level=qcspa%v7d%level(indlevel)


            !indcnetwork      = 1
            !!indcana          = firsttrue(qcspa%clima%ana     == ana)
            !indctime         = index(qcspa%clima%time                  ,  time)
            !indclevel        = index(qcspa%clima%level                 ,  level)
            !indctimerange    = index(qcspa%clima%timerange             ,  qcspa%v7d%timerange(indtimerange))

                                ! attenzione attenzione TODO
                                ! se leggo da bufr il default è char e non reale
            !indcdativarr     = index(qcspa%clima%dativar%r, qcspa%v7d%dativar%r(inddativarr))

!!$                                print *,"dato  ",qcspa%v7d%timerange(indtimerange) 
!!$                                print *,"clima ",qcspa%clima%timerange
!!$                                call l4f_log(L4F_INFO,"Index:"// to_char(indcana)//to_char(indctime)//to_char(indclevel)//&
!!$                                 to_char(indctimerange)//to_char(indcdativarr)//to_char(indcnetwork))

            !!if (indcana <= 0 .or. indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
            !! .or. indcnetwork <= 0 ) cycle
            !if (indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
            ! .or. indcnetwork <= 0 ) cycle

            datoqui = qcspa%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
            if (.not. c_e(datoqui)) cycle

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
              datila = qcspa%v7d%voldatir  (ivert(i) ,: ,indlevel ,indtimerange ,inddativarr, indnetwork )
              if (.not. c_e(datola)) then
                deltato=timedelta_miss
                do iindtime=1,size(qcspa%v7d%time)
                  if (c_e(datila(iindtime)))then
                    if (iindtime < indtime) then
                      deltat=qcspa%v7d%time(indtime)-qcspa%v7d%time(iindtime)
                    else if (iindtime > indtime) then
                      deltat=qcspa%v7d%time(iindtime)-qcspa%v7d%time(indtime)
                    else
                      cycle
                    end if
                    if (deltat < deltato) datola = datila(iindtime)
                  end if
                end do
              end if

              IF(.NOT.C_E(datola)) cycle
                                !	distanza tra le due stazioni
              dist = DISTANZA (qcspa%x(INDANA),qcspa%y(INDANA),qcspa%x(IVERT(I)),qcspa%y(IVERT(I)))
              IF (DIST.EQ.0.)THEN
                call l4f_category_log(qcspa%category,L4F_ERROR,"distance from two station == 0.")
                call raise_error()
              END IF
              dist=max(dist,distmin)
                                !	    modifica 23/9/1998
                                !           se la distanza supera distscol, stazioni scorrelate - salta -
              if (dist > distscol) cycle
              IVB=IVB+1
                                !	valore del gradiente nella direzione delle due stazioni
              GRAD=(datoqui-datola)/DIST
              IF (GRAD >= 0.d0) Ipos=Ipos+1           ! se il gradiente e` positivo incrementa il contatore di positivi
              IF (GRAD <= 0.d0) Ineg=Ineg+1           ! se il gradiente e` negativo incrementa il contatore di negativi
              gradmin=min(gradmin,grad)

            END DO

            IF(IVB < 3) cycle      ! do nothing if valid gradients < 3
                                   
            IF (ipos == ivb .or. ineg == ivb)THEN  ! se tutti i gradienti sono dello stesso segno
              write(10,*)gradmin
              FLAG=50_int_b
            ELSE
              FLAG=100_int_b
            END IF


!!$                do indcana=1,size(qcspa%clima%ana)-1
!!$
!!$                  climaquii=qcspa%clima%voldatir(indcana  ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
!!$                  climaquif=qcspa%clima%voldatir(indcana+1,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
!!$
!            call l4f_log (L4F_INFO,"ident: "//qcspa%clima%ana(indcana)%ident//ident)
!!$
!!$                  if ( match(qcspa%clima%ana(indcana)%ident,ident) .and. c_e(climaquii) .and. c_e(climaquif)) then
!!$

!!$                    print *, "son qua",trim(qcspa%clima%ana(indcana)%ident),trim(ident)
!!$                where (match(qcspa%clima%ana(:)%ident,ident).and. &
!!$                 c_e(qcspa%clima%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)))
!!$                  call l4f_log (L4F_INFO,"macroarea,iarea,mese,altezza,level "//&
!!$                   trim(to_char(qcspa%in_macroa(indana)))//" "//trim(to_char(iarea))&
!!$                   //" "//trim(to_char(mese))//" "//trim(to_char(altezza))//" "//trim(to_char(level)))

!!$
!!$                    if ( (datoqui >= climaquii .and. datoqui < climaquif) .or. &
!!$                         (indcana == 1 .and. datoqui < climaquif) .or. &
!!$                         (indcana == size(qcspa%clima%ana)-1 .and. datoqui >= climaquii) ) then

#ifdef DEBUG
!!$                      if(qcspa%clima%voldatiattrb(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1) < 10 )then
!!$                        call l4f_log (L4F_DEBUG,"data ndi:                   "//t2c(datoqui)//"->"//&
!!$                         t2c(qcspa%clima%voldatiattrb(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1))&
!!$                         //" : "//t2c(qcspa%v7d%time(indtime)))
!!$                        call l4f_log (L4F_DEBUG,"limits: "//t2c(indcana)//":"//t2c(qcspa%clima%ana(indcana)% ident)//&
!!$                         " : "//t2c(climaquii)//" - "//t2c(climaquif)//" : "//t2c(qcspa%clima%time(indctime))) 
!!$                      end if
#endif

                                !ATTENZIONE TODO : inddativarr È UNA GRANDE SEMPLIFICAZIONE NON VERA SE TIPI DI DATO DIVERSI !!!!
                                !                      qcspa%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=&
                                !                      qcspa%clima%voldatiattrb(indcana  ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1)
            qcspa%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=flag

            if ( associated ( qcspa%data_id_in)) then
#ifdef DEBUG
              call l4f_log (L4F_DEBUG,"id: "//t2c(&
               qcspa%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)))
#endif
              qcspa%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)=&
               qcspa%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)
            end if
!!$                end where
          end do
        end do
      end do
    end do
  end do
!!$          end forall
end do

!!$print*,"risultato"
!!$print *,qcspa%v7d%voldatiattrb(:,:,:,:,:,:,indtbattrout)
!!$print*,"fine risultato"

return

contains

elemental double precision function DISTANZA (x1,y1,x2,y2)
double precision, intent(in) :: x1,y1,x2,y2


distanza = sqrt((x2-x1)**2 + (y2-y1)**2)

end function DISTANZA

end subroutine quaconspa


end module modqcspa


!> \example esempio_qcspa.f90
!! Un programma esempio del modulo qcspa

