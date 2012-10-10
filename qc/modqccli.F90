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
!! le macroaree sono tre basate sulle macroaree definite piu'
!! generalmente al SIM; queste prime macroaree sono definite dal file di
!! default macroaree_er.shp. Attribuendo una numerazione che parte da Sud e Est e scorre prima verso Nord
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
!! Il clima infatti è memorizzato su file nel formato binario di Vol7d o in database che offre migliori performances se si lavora su brevi periodi.
!! The minumun pass in time is defined to be 1 hour.
!! The following conventional code values are used to specify which data was taken into account in the  computation:
!! year=1001 : dayly  values of a specified month (depends by day and month)
!! year=1002 : dayly,hourly  values of a specified month (depends by day and month and hour)
!! year=1003 : 10 day period of a specified month (depends by day(1,11,21) and month)
!! year=1004 : 10 day period of a specified month,hourly (depends by day(1,11,21) and month and hour)
!! year=1005 : mounthly values (depend by month)
!! year=1006 : mounthly,hourly values (depend by month and hour)
!! year=1007 : yearly values (no other time dependence)
!! year=1008 : yearly,hourly values (depend by year and hour)
!! The other conventional month hour and minute should be 01 when they are not significative, day should be 1 or, if year=1003 or year=1004 is used, 1,11 or 21.
!! 
!! Ecco come viene definito l'ident del clima:
!! \arg write(ident,'("#",i2.2,2i3.3)')clev,iarea,desc   ! livello climatico, macro-area e descrittore
!! dove clev è un livello convenzionale per le elaborazioni climatiche in Emilia Romagna, 
!! iarea è codice del bounding box o cluster e descrittore rappresenta una specifica dei dati.
!!
!! Il network utilizzato nel volume del clima descrive le variabili contenute e gli eventuali attributi:
!! qcclima-perc     : percentili  (descrittore in ident rappresenta il valore percentuale del percentile)
!! qcclima-ndi      : ndi         (descrittore in ident rappresenta il valore ordinale del ndi)
!! qcclima-extreme  : estremi assoluti         (descrittore in ident = 1 -> minimo; =2 -> massimo)
!!
!! Il dataset qcclima-perc viene scritto utilizzando come variabile la stessa usata per il calcolo;
!! il valore scritto rappresenta il valore del percentile. Nessun attributo è necessario.
!!
!! Il dataset qcclima-ndi viene scritto utilizzando come variabile la stessa usata per il calcolo;
!! il valore scritto rappresenta l'estremo inferiore dell'intervallo su cui è calcolato NDI.
!! Il relativo valore di NDI è riportato come attributo (B33209). L'estremo superiore dell'ultimo NDI non è 
!! riportato in quanto non utile. 
!!
!! In questo modo è possibile inserire nel Vol7d del clima qualsiasi variabile e timerange.
!!
!! Il clima viene letto dalla init.
!! Se è a disposizione un DataBase tipo DB-All.e definendo un dsn la init tenterà di leggere solo la parte di dati necessaria.
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

!> \todo ottimizzare la lettura del clima nel caso il periodo da controllare sia a cavallo di due anni o mesi o giorni.
!!\todo Bisognerebbe validare il volume sottoposto al controllo per vedere se ha i requisiti.
!!\todo La lettura da file formato bufr non funziona non gestendo la lettura degli attributi.
!!
!! Programma Esempio del controllo climatico:
!! \include  v7d_qccli.f90

!! README
!! di fatto sono necessari due file v7d o due dsn : uno per il clima e uno per gli estremi del gross error check.
!! Nel volume extreme ci devono essere due stazioni (differente ident) uno per il valore minimo e uno per il valore massimo.
!! esiste uno switch hright2level per attivare le altezze convenzionali
!! bisogna gestire bene le aree e non supporre che ci sia e sia =1

module modqccli

use geo_coord_class
use vol7d_class
use modqc
use file_utilities
use log4fortran
use char_utilities
use datetime_class
use simple_stat
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
real(kind=fp_geo), parameter :: cli_superarea_lat(cli_nsuperarea)=(/44.76d0,44.50d0,44.34d0/)
real(kind=fp_geo), parameter :: cli_superarea_lon(cli_nsuperarea)=(/9.83d0,11.00d0,11.93d0/)


!>\brief Oggetto principale per il controllo di qualità
type :: qcclitype

  type (vol7d),pointer :: v7d !< Volume dati da controllare
  type (vol7d) :: clima !< Clima di tutte le variabili da controllare
  type (vol7d) :: extreme !< Valori estremi di tutte le variabili da controllare
  integer,pointer :: data_id_in(:,:,:,:,:) !< Indici dati del DB in input
  integer,pointer :: data_id_out(:,:,:,:,:) !< Indici dati del DB in output
  integer, pointer :: in_macroa(:) !< Macroarea di appartenenza delle stazioni
  TYPE(geo_coordvect),POINTER :: macroa(:) !< serie di coordinate che definiscono le macroaree
  integer :: category !< log4fortran
  logical :: height2level   !< use conventional level starting from station height

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
 height2level,categoryappend)

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
logical ,intent(in),optional :: height2level   !< use conventional level starting from station height
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
 
integer :: iuni,i,j
character(len=512) :: filepath
character(len=512) :: filepathclima
character(len=512) :: filepathextreme
character(len=512) :: a_name
TYPE(geo_coord) :: lcoordmin,lcoordmax 
TYPE(vol7d_network)  :: network


call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
qccli%category=l4f_category_get(a_name)

qccli%height2level=optio_log(height2level)

call init(lcoordmin)
call init(lcoordmax)

!!$!  mmm... I am not sure here .... this will be removed
!!$if (qccli%height2level) then
!!$  if (present(coordmin)) lcoordmin=coordmin
!!$  if (present(coordmax)) lcoordmax=coordmax
!!$end if

nullify ( qccli%in_macroa )
nullify ( qccli%data_id_in )
nullify ( qccli%data_id_out )

! riporto il volume dati nel mio oggetto
qccli%v7d => v7d

if (present(data_id_in))then
  qccli%data_id_in => data_id_in
end if

if (qccli%height2level) then
                                !shape file for Emilia Romagna clima !
  filepath=get_package_filepath('share:macroaree_er.shp', filetype_data)
else
                                !shape file for Europa clima !
  filepath=get_package_filepath('share:ens_v8_ll.shp', filetype_data)
end if

if (present(macropath))then
  if (c_e(macropath)) then
    filepath=macropath
  end if
end if

CALL import(qccli%macroa, shpfile=filepath)
call init(qccli%clima)

call optio(climapath,filepathclima)
call optio(extremepath,filepathextreme)

#ifdef HAVE_DBALLE

call init(network,"qcclima-ndi")

ltimei=datetime_miss
ltimef=datetime_miss
if (present (timei)) then
  ltimei=timei
  ltimei=ltimei+timedelta_new(minute=30)
end if

if (present (timef)) then
  ltimef=timef
  ltimef=ltimef+timedelta_new(minute=30)
end if

call getval(ltimei, year=yeari, month=monthi, day=dayi, hour=houri, minute=minutei, msec=mseci)
call getval(ltimef, year=yearf, month=monthf, day=dayf, hour=hourf, minute=minutef, msec=msecf)

if (  c_e(yeari) .and. c_e(yearf) .and. yeari == yearf .and. monthi == monthf ) then

!  call init(ltimei, 1001, monthi, 1, houri, minutei, mseci) 
!  call init(ltimef, 1001, monthf, 1, hourf, minutef, msecf) 
  ltimei=cyclicdatetime_to_conventional(cyclicdatetime_new(month=monthi))
  ltimef=cyclicdatetime_to_conventional(cyclicdatetime_new(month=monthf))

else
                                ! if you span years or months I read all the climat dataset (should be optimized not so easy)
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
    filepathclima=get_package_filepath('qcclima-perc.v7d', filetype_data)
  end if

  if (c_e(filepathclima))then

    select case (trim(lowercase(suffixname(filepathclima))))

    case("v7d")
      iuni=getunit()
      call import(qccli%clima,filename=filepathclima,unit=iuni)
      close (unit=iuni)

#ifdef HAVE_DBALLE
    case("bufr")
      call init(v7d_dballecli,file=.true.,filename=filepathclima,categoryappend=trim(a_name)//".clima")
                                !call import(v7d_dballecli)
      call import(v7d_dballecli,var=var,coordmin=lcoordmin, coordmax=lcoordmax, timei=ltimei, timef=ltimef, &
       varkind=(/("r",i=1,size(var))/),attr=(/"*B33209"/),attrkind=(/"b"/),network=network)
      call copy(v7d_dballecli%vol7d,qccli%clima)
      call delete(v7d_dballecli)
#endif

    case default
      call l4f_category_log(qccli%category,L4F_ERROR,&
       "file type not supported (use .v7d or .bufr suffix only): "//trim(filepathclima))
      call raise_error()
    end select

  else
    call l4f_category_log(qccli%category,L4F_WARN,"clima volume not iniziatized: QC will not be possible")
!    call raise_fatal_error()
  end if

#ifdef HAVE_DBALLE
else

  call l4f_category_log(qccli%category,L4F_DEBUG,"init v7d_dballecli")
  call init(v7d_dballecli,dsn=ldsncli,user=luser,password=lpassword,write=.false.,&
   file=.false.,categoryappend=trim(a_name)//".clima")
  call l4f_category_log(qccli%category,L4F_DEBUG,"import v7d_dballecli")
  call import(v7d_dballecli,var=var,coordmin=lcoordmin, coordmax=lcoordmax, timei=ltimei, timef=ltimef, &
   varkind=(/("r",i=1,size(var))/),attr=(/"*B33209"/),attrkind=(/"b"/),network=network)
  call copy(v7d_dballecli%vol7d,qccli%clima)
  call delete(v7d_dballecli)

end if
#endif

#ifdef HAVE_DBALLE
call delete(ltimei)
call delete(ltimef)

if ( c_e(yeari) .and. c_e(yearf) .and. yeari == yearf .and. monthi == monthf ) then

  if ( dayi == dayf .and. houri == hourf .and. minutei == minutef .and. mseci == msecf ) then

    ltimei=cyclicdatetime_to_conventional(cyclicdatetime_new(month=monthi, hour=houri))
    ltimef=cyclicdatetime_to_conventional(cyclicdatetime_new(month=monthf, hour=hourf))

  else

    ltimei=cyclicdatetime_to_conventional(cyclicdatetime_new(month=monthi, hour=00))
    ltimef=cyclicdatetime_to_conventional(cyclicdatetime_new(month=monthf, hour=23))

  end if

else
                                ! if you span years or months or days I read all the climat dataset (should be optimized not so easy)
  ltimei=datetime_miss
  ltimef=datetime_miss

end if

call init(network,"qcclima-perc")
call optio(dsnextreme,ldsnextreme)

if (.not. c_e(ldsnextreme)) then

#endif

  if (.not. c_e(filepathextreme)) then
    filepathextreme=get_package_filepath('qcclima-extreme.v7d', filetype_data)
  end if

  if (c_e(filepathextreme)) then

    select case (trim(lowercase(suffixname(filepathextreme))))

    case("v7d")
      iuni=getunit()
      call import(qccli%extreme,filename=filepathextreme,unit=iuni)
      close (unit=iuni)

#ifdef HAVE_DBALLE
    case("bufr")
      call init(v7d_dballeextreme,file=.true.,filename=filepathextreme,categoryappend=trim(a_name)//".climaextreme")
                                !call import(v7d_dballeextreme)
      call import(v7d_dballeextreme,var=var,coordmin=lcoordmin, coordmax=lcoordmax, timei=ltimei, timef=ltimef, &
       varkind=(/("r",i=1,size(var))/),attr=(/"*B33192"/),attrkind=(/"b"/),network=network)
      call copy(v7d_dballeextreme%vol7d,qccli%extreme)
      call delete(v7d_dballeextreme)
#endif

    case default

      if (c_e(filepathextreme)) then
        call l4f_category_log(qccli%category,L4F_ERROR,&
         "file type not supported (user .v7d or .bufr suffix only): "//trim(filepathextreme))
        call raise_error()
      end if
    end select

  else
    call l4f_category_log(qccli%category,L4F_WARN,"extreme volume not iniziatized: QC or normalize data will not be possible")
!    call raise_fatal_error()
  end if


#ifdef HAVE_DBALLE
else

  call l4f_category_log(qccli%category,L4F_DEBUG,"init v7d_dballeextreme")
  call init(v7d_dballeextreme,dsn=ldsnextreme,user=luser,password=lpassword,&
   write=.false.,file=.false.,categoryappend=trim(a_name)//".climaextreme")
  call l4f_category_log(qccli%category,L4F_DEBUG,"import v7d_dballeextreme")

  call import(v7d_dballeextreme,var=var,coordmin=lcoordmin, coordmax=lcoordmax, timei=ltimei, timef=ltimef, &
   varkind=(/("r",i=1,size(var))/),attr=(/"*B33192"/),attrkind=(/"b"/),network=network)
  call copy(v7d_dballeextreme%vol7d,qccli%extreme)
  call delete(v7d_dballeextreme)

end if

call delete(ltimei)
call delete(ltimef)
#endif


call qcclialloc(qccli)


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



!!!!!!!!!!!!!!   TODO !!!!!!!!!!!!!!!!!!!!!!!!!!

!> Modulo 1: Calcolo dei parametri di normalizzazione dei dati
!! I parametri di normalizzazione sono il 25°, il 50° e il 75° percentile 
!! (p25,p50,p75)
!!oppure  (p15.87,p50.,p84.13)
!! Tali parametri verranno calcolati per ogni mese, per ogni ora, per ogni area.
!! Modulo 2: Normalizzazione dati
!! Ciascun dato D verrà normalizzato come segue:
!! DN = (D-p50)*2/(p75-p25)
!! oppure DN = (D-p50)*2/(p16-p84)
!! dove DN è il valore normalizzato.
!! La scelta dei parametri di normalizzazione dipende dal mese, dall'ora, 
!! dall'area.

SUBROUTINE vol7d_normalize_data(qccli, battrinv)
 
TYPE(qcclitype),INTENT(inout) :: qccli !< volume providing data to be computed, it is modified by the method
character (len=10) ,intent(in),optional :: battrinv !< attributo invalidated in input/output

real :: datoqui, perc25, perc50,perc75
integer :: indana , indanavar, indtime ,indlevel ,indtimerange ,inddativarr, indnetwork
integer :: indcana,           indctime,indclevel,indctimerange,indcdativarr,indcnetwork
integer :: indbattrinv
TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time, nintime
TYPE(vol7d_level):: level
type(vol7d_var)  :: anavar
integer :: mese, ora, desc, iarea, clev
character(len=1) ::  canc = "#"

CHARACTER(len=vol7d_ana_lenident) :: ident


indbattrinv=0
if (associated(qccli%v7d%dativarattr%b))then
  if (present(battrinv))then
    indbattrinv = index_c(qccli%v7d%dativarattr%b(:)%btable, battrinv)
  else
    indbattrinv = index_c(qccli%v7d%dativarattr%b(:)%btable, '*B33196')
  end if
end if

do indana=1,size(qccli%v7d%ana)

!  iarea= supermacroa(qccli%in_macroa(indana))

  if (.not. associated (qccli%in_macroa)) then
    call l4f_category_log(qccli%category,L4F_WARN,"macroarea data not iniziatized: normalize data not possible")
!    call raise_fatal_error()
    return
  end if

  iarea= qccli%in_macroa(indana)

  do indnetwork=1,size(qccli%v7d%network)
    do indlevel=1,size(qccli%v7d%level)
      do indtimerange=1,size(qccli%v7d%timerange)
        do inddativarr=1,size(qccli%v7d%dativar%r)
          do indtime=1,size(qccli%v7d%time)

            datoqui = qccli%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
              
            if (c_e(datoqui)) then

              if (indbattrinv > 0) then
                if( invalidated(qccli%v7d%voldatiattrb&
                 (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) cycle
              end if

              nintime=qccli%v7d%time(indtime)+timedelta_new(minute=30)
              CALL getval(nintime, month=mese, hour=ora)

              time=cyclicdatetime_to_conventional(cyclicdatetime_new(month=mese, hour=ora))
              !call init(time, year=1001, month=mese, day=1, hour=ora, minute=01)

!!$              call init(anavar,"B07030" )
!!$              indanavar = -1
!!$              if (associated (qccli%v7d%anavar%r)) then
!!$                indanavar        = index(qccli%v7d%anavar%r, anavar)
!!$              end if
!!$              if (indanavar <= 0 )cycle

!!!!!  TODO !
              if (qccli%height2level) then
                call l4f_category_log(qccli%category,L4F_ERROR,"height2level not managed in vol7d_normalize_data")
!                call raise_fatal_error()
              end if
!!$              ! use conventional level starting from station height
!!$              if (optio_log(height2level)) then
!!$                altezza= this%volanar(indana,indanavar,indnetwork)
!!$                call cli_level(altezza,level)
!!$              else
!!$                level=this%level(indlevel)
!!$              end if

              level=qccli%v7d%level(indlevel)

              indcnetwork      = 1
              
                                !indcana          = firsttrue(qccli%extreme%ana     == ana)
              
              indctime         = index(qccli%extreme%time                  ,  time)
              indclevel        = index(qccli%extreme%level                 ,  level)
              indctimerange    = index(qccli%extreme%timerange             ,  qccli%v7d%timerange(indtimerange))
              
                                ! attenzione attenzione TODO
                                ! se leggo da bufr il default è char e non reale

              indcdativarr     = index(qccli%extreme%dativar%r, qccli%v7d%dativar%r(inddativarr))
              
                                print *,"dato  ",qccli%v7d%timerange(indtimerange) 
                                print *,"extreme ",qccli%extreme%timerange
!!$                                call l4f_log(L4F_INFO,"Index:"// to_char(indcana)//to_char(indctime)//to_char(indclevel)//&
!!$                                 to_char(indctimerange)//to_char(indcdativarr)//to_char(indcnetwork))
              
                                !if (indcana <= 0 .or. indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
                                ! .or. indcnetwork <= 0 ) cycle
              if (indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
               .or. indcnetwork <= 0 ) cycle
              

                                ! find percentile in volume
              perc25=rmiss
              perc50=rmiss
              perc75=rmiss

              if (associated(qccli%extreme%voldatir)) then
                desc=25
                write(ident,'("#",i2.2,2i3.3)')0,iarea,desc   ! macro-area e descrittore
                call init(ana,lat=0.0_fp_geo,lon=0.0_fp_geo,ident=ident)
                print *,qccli%extreme%ana%ident,ana%ident
                call display(qccli%extreme%ana(1))
                call display(ana)
                indcana=index(qccli%extreme%ana,ana)
                call l4f_log(L4F_INFO,"Index25:"// to_char(indcana)//to_char(indctime)//to_char(indclevel)//&
                 to_char(indctimerange)//to_char(indcdativarr)//to_char(indcnetwork))
                if (indcana > 0 )then
                  perc25=qccli%extreme%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                end if
                desc=50
                write(ident,'("#",i2.2,2i3.3)')0,iarea,desc   ! macro-area e descrittore
                call init(ana,lat=0.d0,lon=0.d0,ident=ident)
                indcana=index(qccli%extreme%ana,ana)
                if (indcana > 0 )then
                  perc50=qccli%extreme%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                end if
              
                desc=75
                write(ident,'("#",i2.2,2i3.3)')0,iarea,desc   ! macro-area e descrittore
                call init(ana,lat=0.d0,lon=0.d0,ident=ident)
                indcana=index(qccli%extreme%ana,ana)
                if (indcana > 0 )then
                  perc75=qccli%extreme%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                end if
              end if
              
              call l4f_log(L4F_DEBUG,"vol7d_normalize_data perc25 perc50 perc75: "//t2c(perc25)//t2c(perc50)//t2c(perc75))

              if ( c_e(perc25) .and. c_e(perc50) .and. c_e(perc75) ) then
                                ! normalize

                datoqui = (datoqui - perc50) / (perc75 - perc25) + base_value(qccli%v7d%dativar%r(inddativarr)%btable)
                qccli%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork ) = datoqui

              end if
            end if
          end do
        end do
      end do
    end do
  end do

  read  (qccli%v7d%ana(indana)%ident,'(a1,i2.2,2i3.3)') canc, clev, iarea, desc
  clev=0
  iarea=0
  write (qccli%v7d%ana(indana)%ident,'(a1,i2.2,2i3.3)') canc, clev, iarea, desc

end do

end SUBROUTINE vol7d_normalize_data


real function base_value(btable)
character (len=10) ,intent(in):: btable

character (len=10)  :: btables(1)     =(/"B12101"/)
real                :: base_values(1) =(/273.15  /)
integer :: ind

ind = index_c(btables,btable)

if (ind > 0) then
  base_value = base_values(ind)
else
  call l4f_log(L4F_WARN,"modqccli_base_value: variable "//btable//" do not have base value")
  base_value = 0.
end if

return 

end function base_value


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
REAL(kind=fp_geo) :: latc,lonc
integer :: mese, ora
                                !local
integer :: indbattrinv,indtbattrout
logical :: anamaskl(size(qccli%v7d%ana)), timemaskl(size(qccli%v7d%time)), levelmaskl(size(qccli%v7d%level)), &
 timerangemaskl(size(qccli%v7d%timerange)), varmaskl(size(qccli%v7d%dativar%r)), networkmaskl(size(qccli%v7d%network)) 

integer :: indana , indanavar, indtime ,indlevel ,indtimerange ,inddativarr, indnetwork
integer :: indcana,           indctime,indclevel,indctimerange,indcdativarr,indcnetwork
real :: datoqui,climaquii,climaquif, altezza, extremequii,extremequif,perc25,perc50,perc75
integer :: iarea,desc
!integer, allocatable :: indcanav(:)


TYPE(vol7d_network)  :: network
TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time, nintime
TYPE(vol7d_level):: level
type(vol7d_var)  :: anavar

!call qccli_validate (qccli)

indbattrinv=0
if (associated(qccli%v7d%datiattr%b))then
  if (present(battrinv))then
    indbattrinv = index_c(qccli%v7d%datiattr%b(:)%btable, battrinv)
  else
    indbattrinv = index_c(qccli%v7d%datiattr%b(:)%btable, '*B33196')
  end if
end if

if (present(tbattrout))then
  indtbattrout = index_c(qccli%v7d%datiattr%b(:)%btable, tbattrout)
else
  indtbattrout =  index_c(qccli%v7d%datiattr%b(:)%btable, '*B33192')
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

!  if (qccli%height2level) then
!    iarea= supermacroa(qccli%in_macroa(indana))
!  else
  iarea= qccli%in_macroa(indana)

  if (.not. c_e(iarea)) cycle

!  end if
!  write(ident,'("BOX",2i3.3)')iarea,desc   ! macro-area e descrittore
                                !lat=0.0d0
                                !lon=0.0d0
                                !write(ident,'("BOX-",2i2.2)')iarea,lperc   ! macro-area e percentile
                                !call init(ana,lat=lat,lon=lon,ident=ident)
              
                                !allocate (indcanav(count(match(qccli%clima%ana(:)%ident,ident))))
                                !indcanav=match(qccli%clima%ana(:)%ident,ident))))
  
  if (qccli%height2level) then
    latc=cli_superarea_lat(iarea)
    lonc=cli_superarea_lon(iarea)
  else
    latc=0.d0
    lonc=0.d0
  end if
  
  do indnetwork=1,size(qccli%v7d%network)
    do indlevel=1,size(qccli%v7d%level)
      do indtimerange=1,size(qccli%v7d%timerange)
        do inddativarr=1,size(qccli%v7d%dativar%r)
          do indtime=1,size(qccli%v7d%time)

#ifdef DEBUG
            call l4f_log(L4F_DEBUG,"Index:"// t2c(indana)//t2c(indnetwork)//t2c(indlevel)//&
             t2c(indtimerange)//t2c(inddativarr)//t2c(indtime))
#endif
!!$            print *,"elaboro data : "//t2c(qccli%v7d%time(indtime))
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
                 (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indbattrinv))) then

                                ! gross error check allready done
#ifdef DEBUG
                  call l4f_log (L4F_DEBUG,"qccli: skip station for a preceding invalidated flag")
#endif
                  cycle
                end if
              end if

              nintime=qccli%v7d%time(indtime)+timedelta_new(minute=30)
              CALL getval(nintime, month=mese, hour=ora)

              time=cyclicdatetime_to_conventional(cyclicdatetime_new(month=mese, hour=ora))
              !call init(time, year=1001, month=mese, day=1, hour=ora, minute=00)

!!$              print *,"data convenzionale per percentili: ",t2c(time)

              ! use conventional level starting from station height
              if (qccli%height2level) then
                call init(anavar,"B07030" )
                indanavar = -1
                if (associated (qccli%v7d%anavar%r)) then
                  indanavar        = index(qccli%v7d%anavar%r, anavar)
                end if
                if (indanavar <= 0 )cycle

                altezza= qccli%v7d%volanar(indana,indanavar,indnetwork)
                call cli_level(altezza,level)
              else
                level=qccli%v7d%level(indlevel)
              end if

              call init(network,"qcclima-perc")

              indcnetwork      = index(qccli%extreme%network               ,  network)
              indctime         = index(qccli%extreme%time                  ,  time)
              indclevel        = index(qccli%extreme%level                 ,  level)
              indctimerange    = index(qccli%extreme%timerange             ,  qccli%v7d%timerange(indtimerange))
              
                                ! attenzione attenzione TODO
                                ! se leggo da bufr il default è char e non reale
              indcdativarr     = index(qccli%extreme%dativar%r, qccli%v7d%dativar%r(inddativarr))
              
!!$                                print *,"dato  ",qccli%v7d%timerange(indtimerange) 
!!$                                print *,"clima ",qccli%clima%timerange
!!$                                call l4f_log(L4F_INFO,"Index:"// to_char(indctime)//to_char(indclevel)//&
!!$                                 to_char(indctimerange)//to_char(indcdativarr)//to_char(indcnetwork))
              
                                !if (indcana <= 0 .or. indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
                                ! .or. indcnetwork <= 0 ) cycle

!!$              print *,"vector time"
!!$              do i=1,size(qccli%extreme%time)
!!$                call display(qccli%extreme%time(i))
!!$              end do
!!$              print *,"time"
!!$              call display(time)
!!$              call display(level)
!!$              call display(qccli%v7d%timerange(indtimerange))
!!$              print *,"indici percentili",indctime,indclevel,indctimerange,indcdativarr,indcnetwork
              if (indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
               .or. indcnetwork <= 0 ) cycle
              
              datoqui = qccli%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )

              if (c_e(datoqui)) then

                ! find extreme in volume
                extremequii=rmiss
                extremequif=rmiss
                perc25=rmiss
                perc50=rmiss
                perc75=rmiss

!!$                do i=1, size(qccli%extreme%ana)
!!$                  print *,i
!!$                  call display(qccli%extreme%ana(i))
!!$                end do

                if (associated(qccli%extreme%voldatir)) then


!!$                  do i =1,size(qccli%extreme%ana)
!!$                    call display(qccli%extreme%ana(i))
!!$                  end do

                  desc=25  ! minimum
                  write(ident,'("#",i2.2,2i3.3)')0,iarea,desc   ! macro-area e descrittore
                  call init(ana,ident=ident,lat=latc,lon=lonc)
                  indcana=index(qccli%extreme%ana,ana)
                  if (indcana > 0 )then
                    perc25=qccli%extreme%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                  end if

                  desc=50  ! mediana
                  write(ident,'("#",i2.2,2i3.3)')0,iarea,desc   ! macro-area e descrittore
                  call init(ana,ident=ident,lat=latc,lon=lonc)
                  indcana=index(qccli%extreme%ana,ana)
!!$                  call display(ana)
!!$                  print *,"indcana 50 ",indcana
                  if (indcana > 0 )then
                    perc50=qccli%extreme%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                  end if

                  desc=75  ! maximum
                  write(ident,'("#",i2.2,2i3.3)')0,iarea,desc   ! macro-area e descrittore
                  call init(ana,ident=ident,lat=latc,lon=lonc)
                  indcana=index(qccli%extreme%ana,ana)
                  if (indcana > 0 )then
                    perc75=qccli%extreme%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                  end if
                end if

                if ( .not. c_e(perc25) .or. .not. c_e(perc50) .or. .not. c_e(perc75)) cycle

                print *, "datoqui: ",datoqui,"clima ->",perc25,perc50,perc75

                                !http://it.wikipedia.org/wiki/Funzione_di_ripartizione_della_variabile_casuale_normale
                                ! 3.65 for 0.01% each side ( 0.02% total )
                extremequii=perc50 - (perc75 - perc25) *1.3 * 3.65  ! 1.3 to go to standard deviation and 3.65 to make 3.65 sigma 
                extremequif=perc50 + (perc75 - perc25) *1.3 * 3.65  ! 1.3 to go to standard deviation and 3.65 to make 3.65 sigma 

#ifdef DEBUG
                call l4f_log (L4F_INFO,"qccli: gross error check "//t2c(extremequii)//">"//t2c(datoqui)//"<"//t2c(extremequif))
#endif


                if ( datoqui <= extremequii .or. extremequif <= datoqui ) then
                                ! make gross error check

                                !ATTENZIONE TODO : inddativarr È UNA GRANDE SEMPLIFICAZIONE NON VERA SE TIPI DI DATO DIVERSI !!!!
#ifdef DEBUG
                          call l4f_log (L4F_INFO,"qccli: gross error check flag set to bad")
#endif
                  qccli%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=0

                  if ( associated ( qccli%data_id_in)) then
#ifdef DEBUG
                    call l4f_log (L4F_DEBUG,"id: "//t2c(&
                     qccli%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)))
#endif
                    qccli%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)=&
                     qccli%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)
                  end if


                else if (.not. vd(qccli%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,&
                 inddativarr,indnetwork,indtbattrout))) then

                                ! gross error check allready done
#ifdef DEBUG
                          call l4f_log (L4F_DEBUG,"qccli: skip station for a preceding gross error check flagged bad")
#endif
                else

                                ! normalize wihout call the subroutine to be more fast and do not change data volume 
                  datoqui = (datoqui - perc50) / (perc75 - perc25) + base_value(qccli%v7d%dativar%r(inddativarr)%btable)
!                  print *,"normalizzato=",datoqui

                  call init(network,"qcclima-ndi")
                  ! reset the level to standard input data for everyone
                  level=qccli%v7d%level(indlevel)
                  time=cyclicdatetime_to_conventional(cyclicdatetime_new(month=mese))

                  indcnetwork      = index(qccli%clima%network     ,  network)
                  indctime         = index(qccli%clima%time        ,  time)
                  indclevel        = index(qccli%clima%level       ,  level)
                  indctimerange    = index(qccli%clima%timerange   ,  qccli%v7d%timerange(indtimerange))
              
                                ! attenzione attenzione TODO
                                ! se leggo da bufr il default è char e non reale
                  indcdativarr     = index(qccli%clima%dativar%r, qccli%v7d%dativar%r(inddativarr))


!!$                  print *,"indici clima", indctime, indclevel, indctimerange, indcdativarr, indcnetwork
                  if (indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
                   .or. indcnetwork <= 0 ) cycle

                                !climat check

                  do desc=1,size(qccli%clima%ana)-1

                    climaquii=rmiss
                    climaquif=rmiss

                    write(ident,'("#",i2.2,2i3.3)')0,0,desc*10   ! macro-area e descrittore
                    call init(ana,ident=ident,lat=0d0,lon=0d0)
                    indcana=index(qccli%clima%ana,ana)
!                    call display(ana)
!                    print*,indcana
                    if (indcana > 0 )then
                      climaquii=qccli%clima%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                    end if

                    write(ident,'("#",i2.2,2i3.3)')0,0,(desc+1)*10   ! macro-area e descrittore
                    call init(ana,ident=ident,lat=0d0,lon=0d0)
                    indcana=index(qccli%clima%ana,ana)
                    if (indcana > 0 )then
                      climaquif=qccli%clima%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)
                    end if

!!$                  call l4f_log (L4F_INFO,"ident: "//qccli%clima%ana(indcana)%ident//ident)
                    !if ( match(qccli%clima%ana(indcana)%ident,ident) .and. c_e(climaquii) .and. c_e(climaquif)) then
                    if ( c_e(climaquii) .and. c_e(climaquif )) then

!!$                    print *, "son qua",trim(qccli%clima%ana(indcana)%ident),trim(ident)
!!$                where (match(qccli%clima%ana(:)%ident,ident).and. &
!!$                 c_e(qccli%clima%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)))
!!$                  call l4f_log (L4F_INFO,"macroarea,iarea,mese,altezza,level "//&
!!$                   trim(to_char(qccli%in_macroa(indana)))//" "//trim(to_char(iarea))&
!!$                   //" "//trim(to_char(mese))//" "//trim(to_char(altezza))//" "//trim(to_char(level)))
!!$                  

!                      print*,"ndi=",climaquii,datoqui,climaquif

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
                  

                        if (c_e(qccli%clima%voldatiattrb(indcana  &
                         ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1))) then

                                !ATTENZIONE TODO : inddativarr È UNA GRANDE SEMPLIFICAZIONE NON VERA SE TIPI DI DATO DIVERSI !!!!
                          qccli%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=&
                           max (qccli%clima%voldatiattrb(indcana  ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1)&
                           , 1_int_b) ! 0 reserved for gross error check


#ifdef DEBUG
                          call l4f_log (L4F_DEBUG,"qccli: clima check "//t2c(datoqui)//" confidence: "//&
                           t2c(max (qccli%clima%voldatiattrb(indcana  ,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,1)&
                           , 1_int_b)) //" : "//t2c(qccli%v7d%time(indtime)))
#endif

              
                          if ( associated ( qccli%data_id_in)) then
#ifdef DEBUG
                            call l4f_log (L4F_DEBUG,"id: "//t2c(&
                             qccli%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)))
#endif
                            qccli%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)=&
                             qccli%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)
                          end if
                        end if
                      end if
!!$                end where
                    end if
                  end do
                end if
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
!> OBSOLETA
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


SUBROUTINE qc_compute_percentile(this, that, perc_vals,cyclicdt,presentperc, presentnumb)
 
TYPE(qcclitype),INTENT(inout) :: this !< volume providing data to be computed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the computed data
!TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
!TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
!TYPE(datetime),INTENT(in),OPTIONAL :: stopp  !< end of statistical processing interval
real,intent(in) :: perc_vals(:) !< percentile values to use in compute, between 0. and 100.
TYPE(cyclicdatetime),INTENT(in) :: cyclicdt !< cyclic date and time
real, optional :: presentperc !< percentual of data present for compute (default='0.3)
integer, optional :: presentnumb !< number of data present for compute (default='100)
!!$logical, optional :: height2level   !< use conventional level starting from station height

integer :: indana,indtime,indvar,indnetwork,indlevel ,indtimerange ,inddativarr,i,j,k,iana,narea

REAL, DIMENSION(:),allocatable ::  perc
TYPE(vol7d_var) ::  var
character(len=vol7d_ana_lenident) :: ident
character(len=1)            :: type
integer :: areav(size(this%v7d%ana)),iclv(size(this%v7d%ana))
real :: height
logical,allocatable :: mask(:,:,:),maskplus(:,:,:)
integer,allocatable :: area(:)
real :: lpresentperc
integer :: lpresentnumb

lpresentperc=.3
lpresentnumb=imiss

if (present(presentnumb)) then
  if (c_e(presentnumb)) then
    lpresentnumb=presentnumb
  end if
end if


if (present(presentperc)) then
  if (c_e(presentperc)) then
    lpresentperc=presentperc
  end if
end if

allocate (perc(size(perc_vals)))
CALL init(that, time_definition=this%v7d%time_definition)

call init(var, btable="B01192")    ! MeteoDB station ID that here is the number of area

type=cmiss
indvar = index(this%v7d%anavar, var, type=type)
indnetwork=min(1,size(this%v7d%network))

if( indvar > 0 .and. indnetwork > 0 ) then
  select case (type)
  case("d")
    areav=integerdat(this%v7d%volanad(:,indvar,indnetwork),this%v7d%anavar%d(indvar))
  case("r")
    areav=integerdat(this%v7d%volanar(:,indvar,indnetwork),this%v7d%anavar%r(indvar))
  case("i")
    areav=integerdat(this%v7d%volanai(:,indvar,indnetwork),this%v7d%anavar%i(indvar))
  case("b")
    areav=integerdat(this%v7d%volanab(:,indvar,indnetwork),this%v7d%anavar%b(indvar))
  case("c")
    areav=integerdat(this%v7d%volanac(:,indvar,indnetwork),this%v7d%anavar%c(indvar))
  case default
    areav=imiss
  end select
else
  areav=imiss
end if

narea=count_distinct(areav)
allocate(area(narea))
area=pack_distinct(areav,narea)
if (this%height2level) then
  call vol7d_alloc(that,nana=narea*size(perc_vals)*cli_nlevel)
else
  call vol7d_alloc(that,nana=narea*size(perc_vals))
endif

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG, 'displaying this')
CALL display(this%v7d)
#endif

if (this%height2level) then

  call init(var, btable="B07030")    ! height
  
  type=cmiss
  indvar = index(this%v7d%anavar, var, type=type)
  indnetwork=min(1,size(this%v7d%network))

  
!!$#ifdef DEBUG
!!$  CALL l4f_log(L4F_DEBUG, 'SIZE this anavar r '//t2c(SIZE(this%v7d%anavar%r)))
!!$  if (ASSOCIATED(this%anavar%r)) then
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar r '//t2c(SIZE(this%anavar%r)))
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar r btable '//t2c(this%anavar%r(SIZE(this%anavar%r))%btable))
!!$  endif
!!$  CALL l4f_log(L4F_DEBUG, 'SIZE this anavar i '//t2c(SIZE(this%anavar%i)))
!!$  if (ASSOCIATED(this%anavar%i)) then
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar i '//t2c(SIZE(this%anavar%i)))
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar i btable '//t2c(this%anavar%i(SIZE(this%anavar%i))%btable))
!!$  endif
!!$  CALL l4f_log(L4F_DEBUG, 'SIZE this anavar d '//t2c(SIZE(this%anavar%d)))
!!$  if (ASSOCIATED(this%anavar%d)) then
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar d '//t2c(SIZE(this%anavar%d)))
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar d btable '//t2c(this%anavar%d(SIZE(this%anavar%d))%btable))
!!$  endif
!!$  CALL l4f_log(L4F_DEBUG, 'SIZE this anavar b '//t2c(SIZE(this%anavar%b)))
!!$  if (ASSOCIATED(this%anavar%b)) then
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar b '//t2c(SIZE(this%anavar%b)))
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar b btable '//t2c(this%anavar%d(SIZE(this%anavar%b))%btable))
!!$  endif
!!$  CALL l4f_log(L4F_DEBUG, 'SIZE this anavar c '//t2c(SIZE(this%anavar%c)))
!!$  if (ASSOCIATED(this%anavar%c)) then
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar c '//t2c(SIZE(this%anavar%c)))
!!$    CALL l4f_log(L4F_DEBUG, 'SIZE this anavar c btable '//t2c(this%anavar%c(SIZE(this%anavar%c))%btable))
!!$  endif
!!$  CALL l4f_log(L4F_DEBUG, 'indvar has value '//t2c(indvar))
!!$  CALL l4f_log(L4F_DEBUG, 'indnetwork has value '//t2c(indnetwork))
!!$#endif

  do k=1,size(this%v7d%ana)
    
    if( indvar > 0 .and. indnetwork > 0 ) then
      select case (type)
      case("d")
        height=integerdat(this%v7d%volanad(k,indvar,indnetwork),this%v7d%anavar%d(indvar))
      case("r")
        height=integerdat(this%v7d%volanar(k,indvar,indnetwork),this%v7d%anavar%r(indvar))
      case ("i")
        height=integerdat(this%v7d%volanai(k,indvar,indnetwork),this%v7d%anavar%i(indvar))
      case("b")
        height=integerdat(this%v7d%volanab(k,indvar,indnetwork),this%v7d%anavar%b(indvar))
      case("c")
        height=integerdat(this%v7d%volanac(k,indvar,indnetwork),this%v7d%anavar%c(indvar))
      case default
        height=imiss
      end select
    else
      height=imiss
    end if

    if (c_e(height)) then
      iclv(k)=firsttrue(cli_level1 <= height .and. height <= cli_level2 )
    else
      iclv(k)=imiss
    endif

#ifdef DEBUG
    CALL l4f_log(L4F_DEBUG, 'height has value '//t2c(height))
    CALL l4f_log(L4F_DEBUG, 'for k having number '//t2c(k)//&
       ' iclv has value '//t2c(iclv(k)))
#endif
  end do


endif

do i=1,narea
  do j=1,size(perc_vals)
    if (this%height2level) then
      do k=1,cli_nlevel
        write(ident,'("#",i2.2,2i3.3)')k,area(i),nint(perc_vals(j))
        call init(that%ana((k-1)*size(perc_vals)*narea + (j-1)*narea + i),ident=ident,lat=0d0,lon=0d0)
      enddo
    else
      k=0
      write(ident,'("#",i2.2,2i3.3)')k,area(i),nint(perc_vals(j))
      call init(that%ana((j-1)*narea+i),ident=ident,lat=0d0,lon=0d0)
    endif
  end do
end do

!!$do i=1,size(that%ana)
!!$  call display(that%ana(i))
!!$end do

#ifdef DEBUG
CALL l4f_category_log(this%category, L4F_DEBUG, 'nana has value '//t2c(SIZE(this%v7d%ana)))
CALL l4f_category_log(this%category, L4F_DEBUG, 'lpresentperc has value '//t2c(lpresentperc))
CALL l4f_category_log(this%category, L4F_DEBUG, 'lpresentnumb has value '//t2c(lpresentnumb))
#endif


call vol7d_alloc(that,nlevel=size(this%v7d%level), ntimerange=size(this%v7d%timerange), &
 ndativarr=size(this%v7d%dativar%r), nnetwork=1,ntime=1)

that%level=this%v7d%level
that%timerange=this%v7d%timerange
that%dativar%r=this%v7d%dativar%r
that%time(1)=cyclicdatetime_to_conventional(cyclicdt)
call l4f_category_log(this%category, L4F_INFO,"vol7d_compute_percentile conventional datetime "//to_char(that%time(1)))
call init(that%network(1),name="qcclima-perc")

call vol7d_alloc_vol(that,inivol=.true.)

allocate (mask(size(this%v7d%ana),size(this%v7d%time),size(this%v7d%network)))

indtime=1
indnetwork=1
do inddativarr=1,size(this%v7d%dativar%r)
  do indtimerange=1,size(this%v7d%timerange)
    do indlevel=1,size(this%v7d%level)            ! all stations, all times, all networks
      do i=1,narea

        !this%v7d%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)

        !create mask only with valid time
        mask = spread(spread((this%v7d%time == cyclicdt ),1,size(this%v7d%ana)),3,size(this%v7d%network))

#ifdef DEBUG
        CALL l4f_category_log(this%category, L4F_DEBUG, 'count has value '//t2c(count & 
         (mask .and. c_e(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:)))))
#endif

        !delete in mask different area
        do j=1, size(mask,1)
          if (areav(j) /= area(i)) mask(j,:,:) =.false.
        end do

        if (this%height2level) then
          allocate (maskplus(size(this%v7d%ana),size(this%v7d%time),size(this%v7d%network)))
          do k=1,cli_nlevel
#ifdef DEBUG
            CALL l4f_category_log(this%category, L4F_DEBUG, 'k has value '//t2c(k))
#endif

            do iana=1,size(mask,1)
              if (iclv(iana)  /= k) maskplus(iana,:,:) =.false.
              if (iclv(iana)  == k) maskplus(iana,:,:) = mask(iana,:,:)
            enddo

            call sub_perc(maskplus)

          enddo
          deallocate(maskplus)
        else
            
          k=1
          call sub_perc(mask)

        endif
      end do
    end do
  end do
end do

deallocate (perc,mask,area)

contains

subroutine sub_perc(mymask)

logical :: mymask(:,:,:)

                                ! we want more than 30% data present and a number of data bigger than 100 (default)
if &
 ( c_e(lpresentperc) .and. ((float(count & 
 (mymask .and. c_e(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:)))&
 ) / &
 float(count (mymask))) < lpresentperc)) &
 return

if &
 ( c_e(lpresentnumb) .and. (count & 
 (mymask .and. c_e(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:))) < lpresentnumb)&
 ) &
 return


perc= stat_percentile (&
 pack(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:), &
 mask=mymask), &
 perc_vals)


do j=1,size(perc_vals)              
  indana=(k-1)*size(perc_vals)*narea + (j-1)*narea + i
  that%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)=&
   perc(j)
enddo


end subroutine sub_perc


end SUBROUTINE qc_compute_percentile


SUBROUTINE qc_compute_NormalizedDensityIndex(this, that, perc_vals,cyclicdt,presentperc, presentnumb)
 
TYPE(qcclitype),INTENT(inout) :: this !< volume providing data to be computed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the computed data
!TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
!TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
!TYPE(datetime),INTENT(in),OPTIONAL :: stopp  !< end of statistical processing interval
real,intent(in) :: perc_vals(:) !< percentile values to use in compute, between 0. and 100.
TYPE(cyclicdatetime),INTENT(in) :: cyclicdt !< cyclic date and time
real,optional :: presentperc !< rate of data present for compute on expected values (default=0.3)
integer,optional :: presentnumb  !< number of data present for compute (default=100)

integer :: indana,indtime,indvar,indnetwork,indlevel ,indtimerange ,inddativarr, indattr
integer :: i,j,narea
TYPE(vol7d_var) ::  var
character(len=vol7d_ana_lenident) :: ident
character(len=1)            :: type
integer :: areav(size(this%v7d%ana))
logical,allocatable :: mask(:,:,:)
integer,allocatable :: area(:)
REAL, DIMENSION(:),allocatable ::  ndi,limbins
real ::  lpresentperc
integer ::  lpresentnumb


lpresentperc=.3
lpresentnumb=imiss

if (present(presentnumb)) then
  if (c_e(presentnumb)) then
    lpresentnumb=presentnumb
  end if
end if


if (present(presentperc)) then
  if (c_e(presentperc)) then
    lpresentperc=presentperc
  end if
end if

allocate (ndi(size(perc_vals)-1),limbins(size(perc_vals)))
CALL init(that, time_definition=this%v7d%time_definition)
call init(var, btable="B01192")    ! MeteoDB station ID that here is the number of area

type=cmiss
indvar = index(this%v7d%anavar, var, type=type)
indnetwork=1

!if( ind /= 0 ) then
  select case (type)
  case("d")
    areav=integerdat(this%v7d%volanad(:,indvar,indnetwork),this%v7d%anavar%d(indvar))
  case("r")
    areav=integerdat(this%v7d%volanar(:,indvar,indnetwork),this%v7d%anavar%r(indvar))
  case("i")
    areav=integerdat(this%v7d%volanai(:,indvar,indnetwork),this%v7d%anavar%i(indvar))
  case("b")
    areav=integerdat(this%v7d%volanab(:,indvar,indnetwork),this%v7d%anavar%b(indvar))
  case("c")
    areav=integerdat(this%v7d%volanac(:,indvar,indnetwork),this%v7d%anavar%c(indvar))
  case default
    areav=imiss
  end select
!end if

narea=count_distinct(areav)
allocate(area(narea))
area=pack_distinct(areav,narea)
call vol7d_alloc(that,nana=narea*(size(perc_vals)-1))

do i=1,narea
  do j=1,size(perc_vals)-1
    write(ident,'("#",i2.2,2i3.3)')0,area(i),nint(perc_vals(j))
    call init(that%ana((j-1)*narea+i),ident=ident,lat=0d0,lon=0d0)
    !area((j-1)*narea+i)=area(i)
    !percentile((j-1)*narea+i)=perc_vals(j)
  end do
end do

!!$do i=1,size(that%ana)
!!$  call display(that%ana(i))
!!$end do

call vol7d_alloc(that,nlevel=size(this%v7d%level), ntimerange=size(this%v7d%timerange), &
 ndativarr=size(this%v7d%dativar%r), nnetwork=1,ntime=1,ndativarattrr=size(this%v7d%dativar%r),ndatiattrr=1)

that%level=this%v7d%level
that%timerange=this%v7d%timerange
that%dativar%r=this%v7d%dativar%r
that%dativarattr%r=that%dativar%r
call init(that%datiattr%r(1), btable="*B33209")    ! NDI order number
that%time(1)=cyclicdatetime_to_conventional(cyclicdt)

call l4f_category_log(this%category,L4F_INFO,"vol7d_compute_ndi conventional datetime "//to_char(that%time(1)))
call init(that%network(1),name="qcclima-ndi")

call vol7d_alloc_vol(that,inivol=.true.)

allocate (mask(size(this%v7d%ana),size(this%v7d%time),size(this%v7d%network)))

indtime=1
indnetwork=1
indattr=1
do inddativarr=1,size(this%v7d%dativar%r)
  do indtimerange=1,size(this%v7d%timerange)
    do indlevel=1,size(this%v7d%level)            ! all stations, all times, all networks
      do i=1,narea

                                !this%v7d%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)

        !create mask only with valid time
        mask = spread(spread((this%v7d%time == cyclicdt ),1,size(this%v7d%ana)),3,size(this%v7d%network))
        !delete in mask different area
        do j=1, size(mask,1)
          if (areav(j) /= area(i)) mask(j,:,:) =.false.
        end do

        ! we want more than 30% data present

!!$        print*,"-------------------------------------------------------------"
!!$        print*,"Dati presenti:", count (mask .and. c_e(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:)))
!!$        print*,"Dati attesi:", count (mask)

        if ( &
        ((float(count (mask .and. c_e(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:)))) / &
            float(count (mask))) < lpresentperc) &
          .OR. &
        (count (mask .and. c_e(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:))) < lpresentnumb) ) &
         cycle
!!$        print*,"compute"
!!$        print*,"-------------------------------------------------------------"

        call NormalizedDensityIndex (&
         pack(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:), &
         mask=mask), &
         perc_vals, ndi, limbins)

        print *,"------- ndi limbins -----------"
        print *,"min: ",minval(pack(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:),&
         mask=mask.and.c_e(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:))))
        print *,"max: ",maxval(pack(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:),&
         mask=mask.and.c_e(this%v7d%voldatir(:,:, indlevel, indtimerange, inddativarr,:))))
        call display( this%v7d%timerange(indtimerange))
        call display( this%v7d%level(indlevel))
        call display( this%v7d%dativar%r(inddativarr))
        print *, ndi
        print *, limbins

        do j=1,size(perc_vals)-1
          indana=((j-1)*narea+i)
          that%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)=&
           limbins(j)

          ! this is a special case where inddativarr = inddativarr becouse we have only real variables and attributes
!!$          print*," "
!!$          print *,"indici",indana, indtime, indlevel, indtimerange, inddativarr, indnetwork,indattr
!!$          print *, ndi(j) *  100.
          that%voldatiattrr(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork,indattr)=&
           ndi(j) *  100.

        end do
      end do
    end do
  end do
end do

deallocate (ndi,limbins,mask,area)

end SUBROUTINE qc_compute_NormalizedDensityIndex

end module modqccli


!> \example v7d_qccli.f90
!! Sample program for module qccli

