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

!>\brief Controllo di qualità climatico.
!! Questo modulo permette di effettuare una valutazione della probabilità che un certo intervallo 
!! di misura ha di verificarsi. Per fare ciò si utilizzano una serie di percentili precedentemente calcolati.
!! Il clima (NDI per percentili) sono suddivisi per macroarea, altezza dal livello del mare e mese, giorno e ora dell'anno.
!!
!! definizione delle macroaree:
!! le macroaree sono tre bsate sulle macroaree definite piu'
!! generalmente al SIM; queste prime macroaree sono definite dal file di
!! default polipciv4.dat. Attribuendo una numerazione che parte da Sud e Est e scorre prima verso Nord
!! le nuove aree vengono cosi' definite:
!!  \arg area clima  1 -> macroarea SIM  7,8
!!  \arg area clima  2 -> macroarea SIM  3,5,6
!!  \arg area clima  3 -> macroarea SIM  1,2,4 
!!
!!Le altezze invece vengono cosi' definite:
!!  classe altezza = (altezza+150)/250
!! ottenendo un indice da 1 a 10 (inserendo altezze in metri).
!! Questo indice viene utilizzato per selezionare un livello tipico utilizzato nella descrizione del clima 
!! con leveltype=102:
!! \arg livello1(ncli_level) = (/-100,100,250,500,750,1000,1250,1500,1750,2000/)
!! \arg livello2(ncli_level) = (/100,250,500,750,1000,1250,1500,1750,2000,2250/)
!!
!! Area e percentile vengono utilizzati per costruire l'ident dell'anagrafica del Vol7d del clima.
!! Il clima infatti è memorizzato su file nel formato binario di Vol7d utilizzando come anno per i 
!! dati il 1001 e giorno 01; per ora alcuni dati sono nel file clima.v7d.
!! Se è a disposizione un DataBase tipo DB-All.e definendo un dsn la init tenterà di leggere solo la parte di dati necessaria.
!! Ecco come viene definito l'ident del clima:
!! \arg write(ident,'("BOX-",2i2.2)')iarea,perc   ! macro-area e percentile
!! Il network utilizzato per il clima è il numero 1000
!! In questo modo è possibile inserire nel Vol7d del clima qualsiasi parametro per leveltype e timerange.
!! Il clima viene letto dalla init.
!! Dopo l'allocazione di memoria le successive operazioni svolte da qccli sono principalmente le seguenti:
!! \arg non trattare in alcun modo i dati invalidati (manualmente)
!! \arg selezionare i dati per cui è possibile effettuare il controllo (area, variabile,confidenza, etc)
!! \arg ai dati selezionati viene attribuita una confidenza pari al NDI del percentile a cui appartiene il dato. 
!!
!! Per considerare valido un dato in ingresso (da sottoporre al controllo) è utilizzato un attributo dei dati 
!! che deve contenere la flag di eventuale invalidazione (manuale); la confidenza al dato calcolata viene scritta
!! in un attributo del dato. Questi due attributi possono essere specificati nella chiamata oppure assunti come 
!! default relativamente al primo e secondo attributo ai dati del volume.
!!
!! Oltre all'attributo con la confidenza, se presente, viene scritto anche l'id relativo ai dati a cui sono
!! state attribuite le confidenze, operazione necessaria per l'ottimizzazione della riscrittura dei dati.
!!
!!\ingroup qc

!> \todo ottimizzare la lettura del clima nel caso il periodo da controllare sia a cavallo di due anni.
!!\todo Bisognerebbe validare il volume sottoposto al controllo per vedere se ha i requisiti.
!!\todo La lettura da file formato bufr non funziona non gestendo la lettura degli attributi.
!!
!! Programma Esempio del controllo climatico:
!! \include  v7d_qccli.f90


module modqccli

use geo_coord_class
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

character (len=255),parameter:: subcategory="QCcli"

integer, parameter :: cli_nlevel=10
!> standard heigth for climatological use (low level)
integer, parameter :: cli_level1(cli_nlevel) = (/-100,100,250,500,750,1000,1250,1500,1750,2000/)
!> standard heigth for climatological use (hight level)
integer, parameter :: cli_level2(cli_nlevel) = (/100,250,500,750,1000,1250,1500,1750,2000,2250/)

!> conventional coordinate for superarea location
integer, parameter :: cli_nsuperarea=3
real(kind=fp_geo), parameter :: cli_superarea_lat(cli_nsuperarea)=(/44.76,44.50,44.34/)
real(kind=fp_geo), parameter :: cli_superarea_lon(cli_nsuperarea)=(/9.83,11.00,11.93/)


!>\brief Oggetto principale per il controllo di qualità
type :: qcclitype

  type (vol7d),pointer :: v7d !< Volume dati da controllare
  type (vol7d) :: clima !< Clima di tutte le variabili da controllare
  type (vol7d) :: extreme !< Valori estremi di tutte le variabili da controllare
  integer,pointer :: data_id_in(:,:,:,:,:) !< Indici dati del DB in input
  integer,pointer :: data_id_out(:,:,:,:,:) !< Indici dati del DB in output
  integer, pointer :: in_macroa(:) !< Maacroarea di appartenenza delle stazioni
  TYPE(geo_coordvect),POINTER :: macroa(:) !< serie di coordinate che definiscono le macroaree
  integer :: category !< log4fortran
end type qcclitype


!>\brief Inizializzazione
interface init
  module procedure qccliinit
end interface

!>\brief  Allocazione di memoria
interface alloc
  module procedure qcclialloc
end interface

!>\brief Cancellazione
interface delete
  module procedure qcclidelete
end interface


contains

!>\brief Init del controllo di qualità climatico.
!!Effettua la lettura dei file e altre operazioni di inizializzazione.

subroutine qccliinit(qccli,v7d,var, timei, timef, coordmin, coordmax, data_id_in,&
 macropath, climapath, extremepath, &
#ifdef HAVE_DBALLE
 dsncli,dsnextreme,user,password,&
#endif
categoryappend)

type(qcclitype),intent(in out) :: qccli !< Oggetto per il controllo climatico
type (vol7d),intent(in),target:: v7d !< Il volume Vol7d da controllare
character(len=*),INTENT(in) :: var(:)!< variabili da importare secondo la tabella B locale o relativi alias
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'importazione
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
!>estremi temporali (inizio e fine) dell'estrazione per l'importazione
TYPE(datetime),INTENT(in),optional :: timei, timef
integer,intent(in),optional,target:: data_id_in(:,:,:,:,:) !< Indici dei dati in DB
character(len=*),intent(in),optional :: macropath !< file delle macroaree
character(len=*),intent(in),optional :: climapath !< file con il volume del clima
character(len=*),intent(in),optional :: extremepath !< file con il volume del clima
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

#ifdef HAVE_DBALLE
type (vol7d_dballe) :: v7d_dballecli
character(len=*),intent(in),optional :: dsncli
character(len=*),intent(in),optional :: user
character(len=*),intent(in),optional :: password
character(len=512) :: ldsncli
character(len=512) :: luser
character(len=512) :: lpassword
type (vol7d_dballe) :: v7d_dballeextreme
character(len=*),intent(in),optional :: dsnextreme
character(len=512) :: ldsnextreme
TYPE(datetime) :: ltimei, ltimef
integer :: yeari, yearf, monthi, monthf, dayi, dayf,&
 houri, minutei, mseci, hourf, minutef, msecf
#endif
 
integer :: iuni,i
character(len=512) :: filepath
character(len=512) :: filepathclima
character(len=512) :: filepathextreme
character(len=512) :: a_name


call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
qccli%category=l4f_category_get(a_name)

nullify ( qccli%in_macroa )
nullify ( qccli%data_id_in )
nullify ( qccli%data_id_out )

! riporto il volume dati nel mio oggetto
qccli%v7d => v7d

if (present(data_id_in))then
  qccli%data_id_in => data_id_in
end if


if (present(macropath))then
  filepath=macropath
else
 filepath=get_package_filepath('polipciv4.dat', filetype_data)
end if

CALL import(qccli%macroa, shpfilesim=filepath)
call init(qccli%clima)

call optio(climapath,filepathclima)
call optio(extremepath,filepathextreme)

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

call optio(dsncli,ldsncli)
call optio(user,luser)
call optio(password,lpassword)

if ((c_e(filepathclima) .or. c_e (filepathextreme)) .and. (c_e(ldsncli).or.c_e(luser).or.c_e(lpassword))) then
  call l4f_category_log(qccli%category,L4F_ERROR,"climapath or extremepath defined together with dba options")
  call raise_error()
end if

if (.not. c_e(ldsncli)) then

#endif

  if (.not. c_e(filepathclima)) then
    filepathclima=get_package_filepath('climaprec.v7d', filetype_data)
  end if

  select case (trim(lowercase(suffixname(filepathclima))))

  case("v7d")
    iuni=getunit()
    call import(qccli%clima,filename=filepathclima,unit=iuni)
    close (unit=iuni)

#ifdef HAVE_DBALLE
  case("bufr")
    call init(v7d_dballecli,file=.true.,filename=filepathclima,categoryappend=trim(a_name)//".clima")
                                !call import(v7d_dballecli)
    call import(v7d_dballecli,var=var,coordmin=coordmin, coordmax=coordmax, timei=ltimei, timef=ltimef, &
     varkind=(/("r",i=1,size(var))/),attr=(/"*B33192"/),attrkind=(/"b"/))
    call copy(v7d_dballecli%vol7d,qccli%clima)
    call delete(v7d_dballecli)
#endif

  case default
    call l4f_category_log(qccli%category,L4F_ERROR,&
     "file type not supported (user .v7d or .bufr suffix only): "//trim(filepathclima))
    call raise_error()
  end select

#ifdef HAVE_DBALLE
else

  call l4f_category_log(qccli%category,L4F_DEBUG,"init v7d_dballecli")
  call init(v7d_dballecli,dsn=ldsncli,user=luser,password=lpassword,write=.false.,&
   file=.false.,categoryappend=trim(a_name)//".clima")
  call l4f_category_log(qccli%category,L4F_DEBUG,"import v7d_dballecli")
  call import(v7d_dballecli,var=var,coordmin=coordmin, coordmax=coordmax, timei=ltimei, timef=ltimef, &
   varkind=(/("r",i=1,size(var))/),attr=(/"*B33192"/),attrkind=(/"b"/))
  call copy(v7d_dballecli%vol7d,qccli%clima)
  call delete(v7d_dballecli)

end if
#endif


#ifdef HAVE_DBALLE

call optio(dsnextreme,ldsnextreme)

if (.not. c_e(ldsnextreme)) then

#endif

  if (.not. c_e(filepathextreme)) then
    filepathextreme=get_package_filepath('climaprec.v7d', filetype_data)
  end if

  select case (trim(lowercase(suffixname(filepathextreme))))

  case("v7d")
    iuni=getunit()
    call import(qccli%clima,filename=filepathextreme,unit=iuni)
    close (unit=iuni)

#ifdef HAVE_DBALLE
  case("bufr")
    call init(v7d_dballeextreme,file=.true.,filename=filepathextreme,categoryappend=trim(a_name)//".clima")
                                !call import(v7d_dballeextreme)
    call import(v7d_dballeextreme,var=var,coordmin=coordmin, coordmax=coordmax, timei=ltimei, timef=ltimef, &
     varkind=(/("r",i=1,size(var))/),attr=(/"*B33192"/),attrkind=(/"b"/))
    call copy(v7d_dballeextreme%vol7d,qccli%clima)
    call delete(v7d_dballeextreme)
#endif

  case default
    call l4f_category_log(qccli%category,L4F_ERROR,&
     "file type not supported (user .v7d or .bufr suffix only): "//trim(filepathextreme))
    call raise_error()
  end select

#ifdef HAVE_DBALLE
else

  call l4f_category_log(qccli%category,L4F_DEBUG,"init v7d_dballeextreme")
  call init(v7d_dballeextreme,dsn=ldsnextreme,user=luser,password=lpassword,&
   write=.false.,file=.false.,categoryappend=trim(a_name)//".clima")
  call l4f_category_log(qccli%category,L4F_DEBUG,"import v7d_dballeextreme")
  call import(v7d_dballeextreme,var=var,coordmin=coordmin, coordmax=coordmax, timei=ltimei, timef=ltimef, &
   varkind=(/("r",i=1,size(var))/),attr=(/"*B33192"/),attrkind=(/"b"/))
  call copy(v7d_dballeextreme%vol7d,qccli%clima)
  call delete(v7d_dballeextreme)

end if
#endif


return
end subroutine qccliinit


!>\brief Allocazioni di memoria
subroutine qcclialloc(qccli)
                                ! pseudo costruttore con distruttore automatico

type(qcclitype),intent(in out) :: qccli !< Oggetto per il controllo climatico

integer :: istatt
integer :: sh(5)

! se ti sei dimenticato di deallocare ci penso io
call  qcclidealloc(qccli)


!!$if (associated (qccli%v7d%dativar%r )) then
!!$  nv=size(qccli%v7d%dativar%r)
!!$
!!$  allocate(qccli%valminr(nv),stat=istat)
!!$  istatt=istatt+istat
!!$  allocate(qccli%valmaxr(nv),stat=istat)
!!$  istatt=istatt+istat
!!$
!!$  if (istatt /= 0) ier=1
!!$
!!$end if

if (associated (qccli%v7d%ana )) then
  allocate (qccli%in_macroa(size(qccli%v7d%ana )),stat=istatt)
  if (istatt /= 0) then
    call l4f_category_log(qccli%category,L4F_ERROR,"allocate error")
    call raise_error("allocate error")
  end if
end if

if (associated(qccli%data_id_in))then
  sh=shape(qccli%data_id_in)
  allocate (qccli%data_id_out(sh(1),sh(2),sh(3),sh(4),sh(5)),stat=istatt)
  if (istatt /= 0)then
    call l4f_category_log(qccli%category,L4F_ERROR,"allocate error")
    call raise_error("allocate error")
  else
    qccli%data_id_out=imiss
  end if
end if

return

end subroutine qcclialloc


!>\brief Deallocazione della memoria

subroutine qcclidealloc(qccli)
                                ! pseudo distruttore

type(qcclitype),intent(in out) :: qccli !< Oggetto per l controllo climatico

!!$if ( associated ( qccli%valminr)) then
!!$  deallocate(qccli%valminr)
!!$end if
!!$
!!$if ( associated ( qccli%valmaxr)) then
!!$  deallocate(qccli%valmaxr)
!!$end if

if (associated (qccli%in_macroa)) then
  deallocate (qccli%in_macroa)
end if

if (associated(qccli%data_id_out))then
  deallocate (qccli%data_id_out)
end if

return
end subroutine qcclidealloc


!>\brief Cancellazione


subroutine qcclidelete(qccli)
                                ! decostruttore a mezzo
type(qcclitype),intent(in out) :: qccli !< Oggetto per l controllo climatico

call qcclidealloc(qccli)

call delete(qccli%clima)

!delete logger
call l4f_category_delete(qccli%category)

return
end subroutine qcclidelete


!>\brief Controllo di Qualità climatico.
!!Questo è il vero e proprio controllo di qualità climatico.
!!Avendo a disposizione un volume dati climatico 
!!contenente i percentili suddivisi per area, altezza sul livello
!!del mare, per mese dell'anno viene selezionato il percentile e sulla base di questo 
!!vengono assegnate le opportune confidenze.


SUBROUTINE quaconcli (qccli,battrinv,tbattrout,&
 anamask,timemask,levelmask,timerangemask,varmask,networkmask)


type(qcclitype),intent(in out) :: qccli !< Oggetto per il controllo di qualità
character (len=10) ,intent(in),optional :: battrinv !< attributo invalidated in input/output
character (len=10) ,intent(in),optional :: tbattrout !< attributo con la confidenza climatologica in output
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
integer :: i,j,indbattrinv,indtbattrout
logical :: anamaskl(size(qccli%v7d%ana)), timemaskl(size(qccli%v7d%time)), levelmaskl(size(qccli%v7d%level)), &
 timerangemaskl(size(qccli%v7d%timerange)), varmaskl(size(qccli%v7d%dativar%r)), networkmaskl(size(qccli%v7d%network)) 

integer :: indana , indanavar, indtime ,indlevel ,indtimerange ,inddativarr, indnetwork
integer :: indcana,           indctime,indclevel,indctimerange,indcdativarr,indcnetwork
real :: datoqui,climaquii,climaquif, altezza
integer :: iarea
!integer, allocatable :: indcanav(:)


!TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time, nintime
TYPE(vol7d_level):: level
type(vol7d_var)  :: anavar


!call qccli_validate (qccli)

! valuto in quale macroarea sono le stazioni
qccli%in_macroa = imiss

DO i = 1, SIZE(qccli%v7d%ana)
  DO j = 1, SIZE(qccli%macroa)
    IF (inside(qccli%v7d%ana(i)%coord, qccli%macroa(j))) THEN
      qccli%in_macroa(i) = j
      EXIT
    ENDIF
  ENDDO
ENDDO


if (present(battrinv))then
  indbattrinv = index_c(qccli%v7d%dativarattr%r(:)%btable, battrinv)
else
  indbattrinv = index_c(qccli%v7d%dativarattr%r(:)%btable, '*B33196')
end if

if (present(tbattrout))then
  indtbattrout = index_c(qccli%v7d%dativarattr%r(:)%btable, tbattrout)
else
  indtbattrout =  index_c(qccli%v7d%dativarattr%r(:)%btable, '*B33192')
end if

if ( indtbattrout <= 0 ) then

  call l4f_category_log(qccli%category,L4F_ERROR,"error finding attribute index in/out")
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

qccli%v7d%voldatiattrb(:,:,:,:,:,:,indtbattrout)=ibmiss


do indana=1,size(qccli%v7d%ana)

  iarea= supermacroa(qccli%in_macroa(indana))
  write(ident,'("BOX-",i2.2,"*")')iarea   ! macro-area
                                !lat=0.0d0
                                !lon=0.0d0
                                !write(ident,'("BOX-",2i2.2)')iarea,lperc   ! macro-area e percentile
                                !call init(ana,lat=lat,lon=lon,ident=ident)
              
                                !allocate (indcanav(count(match(qccli%clima%ana(:)%ident,ident))))
                                !indcanav=match(qccli%clima%ana(:)%ident,ident))))
  
  do indnetwork=1,size(qccli%v7d%network)
    do indlevel=1,size(qccli%v7d%level)
      do indtimerange=1,size(qccli%v7d%timerange)
        do inddativarr=1,size(qccli%v7d%dativar%r)
          do indtime=1,size(qccli%v7d%time)

!!$            call l4f_log(L4F_INFO,"Index:"// t2c(indana)//t2c(indnetwork)//t2c(indlevel)//&
!!$             t2c(indtimerange)//t2c(inddativarr)//t2c(indtime))
!!$
!!$  forall (indnetwork=1:size(qccli%v7d%network), &
!!$   indlevel=1:size(qccli%v7d%level), &
!!$   indtimerange=1:size(qccli%v7d%timerange), &
!!$   inddativarr=1:size(qccli%v7d%dativar%r), &
!!$   indtime=1:size(qccli%v7d%time))

            if (anamaskl(indana).and.timemaskl(indtime).and.levelmaskl(indlevel).and. &
             timerangemaskl(indtimerange).and.varmaskl(inddativarr).and.networkmaskl(indnetwork).and.&
             c_e(qccli%v7d%voldatir(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork)))then

              if (indbattrinv > 0) then
                if( invalidated(qccli%v7d%voldatiattrb&
                 (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) cycle
              end if

              nintime=qccli%v7d%time(indtime)+timedelta_new(minute=30)
              CALL getval(nintime, month=mese, hour=ora)
              call init(time, year=1001, month=mese, day=1, hour=ora, minute=00)

              call init(anavar,"B07030" )
              indanavar = -1
              if (associated (qccli%v7d%anavar%r)) then
                indanavar        = index(qccli%v7d%anavar%r, anavar)
              end if
              if (indanavar <= 0 )cycle
              altezza= qccli%v7d%volanar(indana,indanavar,indnetwork)
              call cli_level(altezza,level)

              indcnetwork      = 1
              
                                !indcana          = firsttrue(qccli%clima%ana     == ana)
              
              indctime         = index(qccli%clima%time                  ,  time)
              indclevel        = index(qccli%clima%level                 ,  level)
              indctimerange    = index(qccli%clima%timerange             ,  qccli%v7d%timerange(indtimerange))
              
                                ! attenzione attenzione TODO
                                ! se leggo da bufr il default è char e non reale

              indcdativarr     = index(qccli%clima%dativar%r, qccli%v7d%dativar%r(inddativarr))
              
!!$                                print *,"dato  ",qccli%v7d%timerange(indtimerange) 
!!$                                print *,"clima ",qccli%clima%timerange
!!$                                call l4f_log(L4F_INFO,"Index:"// to_char(indcana)//to_char(indctime)//to_char(indclevel)//&
!!$                                 to_char(indctimerange)//to_char(indcdativarr)//to_char(indcnetwork))
              
                                !if (indcana <= 0 .or. indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
                                ! .or. indcnetwork <= 0 ) cycle
              if (indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
               .or. indcnetwork <= 0 ) cycle
              
              datoqui = qccli%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
              
              if (c_e(datoqui)) then

                do indcana=1,size(qccli%clima%ana)-1

                  climaquii=qccli%clima%voldatir(indcana  ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                  climaquif=qccli%clima%voldatir(indcana+1,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)

!!$                  call l4f_log (L4F_INFO,"ident: "//qccli%clima%ana(indcana)%ident//ident)

                  if ( match(qccli%clima%ana(indcana)%ident,ident) .and. c_e(climaquii) .and. c_e(climaquif)) then

!!$                    print *, "son qua",trim(qccli%clima%ana(indcana)%ident),trim(ident)
!!$                where (match(qccli%clima%ana(:)%ident,ident).and. &
!!$                 c_e(qccli%clima%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)))
!!$                  call l4f_log (L4F_INFO,"macroarea,iarea,mese,altezza,level "//&
!!$                   trim(to_char(qccli%in_macroa(indana)))//" "//trim(to_char(iarea))&
!!$                   //" "//trim(to_char(mese))//" "//trim(to_char(altezza))//" "//trim(to_char(level)))
!!$                  

                    if ( (datoqui >= climaquii .and. datoqui < climaquif) .or. &
                         (indcana == 1 .and. datoqui < climaquif) .or. &
                         (indcana == size(qccli%clima%ana)-1 .and. datoqui >= climaquii) ) then
                    
#ifdef DEBUG
                      if(qccli%clima%voldatiattrb(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1) < 10 )then
                        call l4f_log (L4F_DEBUG,"data ndi:                   "//t2c(datoqui)//"->"//&
                         t2c(qccli%clima%voldatiattrb(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1))&
                         //" : "//t2c(qccli%v7d%time(indtime)))
                        call l4f_log (L4F_DEBUG,"limits: "//t2c(indcana)//":"//t2c(qccli%clima%ana(indcana)% ident)//&
                         " : "//t2c(climaquii)//" - "//t2c(climaquif)//" : "//t2c(qccli%clima%time(indctime))) 
                      end if
#endif
                  
                      !ATTENZIONE TODO : inddativarr È UNA GRANDE SEMPLIFICAZIONE NON VERA SE TIPI DI DATO DIVERSI !!!!
                      qccli%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=&
                      qccli%clima%voldatiattrb(indcana  ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1)
                    
                  
                      if ( associated ( qccli%data_id_in)) then
#ifdef DEBUG
                        call l4f_log (L4F_DEBUG,"id: "//t2c(&
                         qccli%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)))
#endif
                        qccli%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)=&
                         qccli%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)
                      end if
                    end if
!!$                end where
                  end if
                end do
              end if
            end if
          end do
        end do
      end do
    end do
  end do
!!$          end forall
end do

!!$print*,"risultato"
!!$print *,qccli%v7d%voldatiattrb(:,:,:,:,:,:,indtbattrout)
!!$print*,"fine risultato"


return

end subroutine quaconcli


!>\brief Return a conventional level for climatological definition.
!! Starting from heigth of station in meter return a level where is defined (I hope)
!! the climatological value 
subroutine cli_level(heigth,level)

real,intent(in) :: heigth !< heigth of station in meter
TYPE(vol7d_level),intent(out):: level !< level where is defined the climatological value (layer)

integer :: i

i=imiss

if (c_e(heigth)) then
  i=firsttrue(cli_level1 <= heigth .and. heigth <= cli_level2 )
end if

if (i >= 1 .and. i <= 10 ) then
  call init(level, 102,cli_level1(i)*1000,102,cli_level2(i)*1000)
else
  if (c_e(i)) CALL l4f_log(L4F_DEBUG,"cli_level: strange level, heigth: "//to_char(heigth))
  call init(level)
end if

end subroutine cli_level


!> Initialize level according to climate definition at SIMC 
subroutine cli_level_generate(level)

TYPE(vol7d_level),intent(out):: level(:) !< level where is defined the climatological value (layer)

integer :: i

if (size(level) /= cli_nlevel ) then
  call l4f_log(L4F_ERROR,"cli_level_generate: level dimension /= "//trim(to_char(cli_nlevel)))
  call raise_error("cli_level_generate: level dimension /= "//trim(to_char(cli_nlevel)))
end if

do i=1,cli_nlevel
  call init(level(i), 102,cli_level1(i)*1000,102,cli_level2(i)*1000)
end do

end subroutine cli_level_generate


!!$subroutine qccli_validate(qccli)
!!$type(qcclitype),intent(in) :: qccli
!!$
!!$!todo da validare
!!$
!!$return
!!$end subroutine qccli_validate


!> Rielabora le macroarea facendole Valentine/Elements thinking
integer function supermacroa(macroa)

integer, intent(in) :: macroa 
                                ! rielabora le macroarea facendole Valentine thinking

supermacroa=imiss

if (macroa == 1 .or. macroa == 2 .or. macroa == 4 ) supermacroa=3
if (macroa == 3 .or. macroa == 5 .or. macroa == 6 ) supermacroa=2
if (macroa == 7 .or. macroa == 8 ) supermacroa=1

!!$  ! rielabora le macroarea facendole Valentine thinking
!!$
!!$  if (qccli%in_macroa(indana) == 1 .or. qccli%in_macroa(indana) == 2 .or. qccli%in_macroa(indana) == 4 ) iarea=3
!!$  if (qccli%in_macroa(indana) == 3 .or. qccli%in_macroa(indana) == 5 .or.  qccli%in_macroa(indana) == 6 ) iarea=2
!!$  if (qccli%in_macroa(indana) == 7 .or.  qccli%in_macroa(indana) == 8 ) iarea=1

end function supermacroa

end module modqccli


!> \example esempio_qccli.f90
!! Un programma esempio del modulo qccli

