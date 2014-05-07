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

!> \brief  classe per import ed export di volumi da e in DB-All.e 
!!
!!Questo modulo definisce gli oggetti e i metodi per gestire
!!l'importazione e l'esportazione di volumi dal database per dati sparsi
!!DB-All.e
!!
!!Il tutto funziona intorno all'oggetto vol7d_dballe che aggiunge ad un
!!oggetto vol7d ulteriori informazioni.
!!
!!Con la chiamata init vengono definiti i parametri di accesso alla DSN
!!(database) di DB-All.e. oppure da file bufr/crex
!!
!!Con import è possibile acquisire nel prorio programma i dati presenti
!!nel DSN o su file; l'allocazione di memoria è automatica. Import è in grado di
!!importare dati senza nessuna ulteriore specificazione: vengono
!!acquisite tutte le stazioni senza limiti di tempo o di spazio, per
!!tutti i dati, compresi quelli di anagrafica; solo gli attributi
!!vengono tralasciati se non specificati. Se non specificato i dati sono
!!in forma character che permette di essere conservativi su tutti i tipi
!!di dato archiviabile in DB-all.e. Aumentando i parametri
!!all'intefaccia import è possibile specificare sottoinsiemi temporali e
!!spaziali, elenchi di variabili e attributi. E' possibile anche
!!specificare il tipo per ogni variabile o attributo richiesto. E'
!!fornito un set di routine per avere la possibilità di estrarre un
!!vettore di variabili e un vettore di reti. E' possibile anche specificare
!!una singola stazione. vol7d_dballe%%data_id
!!contiene un vettore di servizio con gli id interni del database che
!!indirizzano direttamente i dati. Una particolare opzione permette di
!!attivare l'opzione query di tipo best in DB-All.e per avere in una
!!unica rete i dati migliori presenti nel DB.
!!
!!Con export si riscrivono i dati nel DSN di DB-All.e potendo attivare
!!una serie di filtri.
!!
!!Con delete si elimina definitvamente l'oggetto vol7d_dballe.
!!
!!Mantenendo lo stesso oggetto nella sequenza init, import, export,
!!delete si sovrascrive lo stesso DSN di DB-All.e.  Il vettore
!!vol7d_dballe%%data_id in export permette di sovrascrivere solo gli
!!attributi; eventualmente possono essere sovrascritti i soli attributi
!!relativi agli elementi data_id non mancanti.
!!
!! E' da notare che se si attiva l'opzione "anaonly" solo ma tutte le stazioni e 
!! i dati di anagrafica vengono importati secondo i parametri di query selezionati.
!! Se l'opzione "anaonly" è disattivata solo le stazioni con i dati richiesti presenti 
!! saranno caricati nella sezione anagrafica
!!
!!Utilizzando due differenti oggetti uno per import e uno per export è
!!possibile tramite l'associazione del puntatore in essi contenuto
!!relativo al volume vol7d ricopiare contenuti in altri DSN senza
!!sprechi di memoria.
!!
!!Programma esempio
!!\include esempio_v7ddballe.f90
!!
!!\ingroup vol7d

MODULE vol7d_dballenew_class

USE dballe_class
USE char_utilities
USE vol7d_class
USE array_utilities
use log4fortran
USE geo_coord_class

IMPLICIT NONE

character (len=255),parameter:: subcategory="vol7d_dballe_class"

!>\brief Oggetto per import ed export da DB-All.e
!!
!!L'oggetto è costituito da un oggetto vol7d attorniato dalle 
!!informazioni necessarie per l'accesso al DSN di DB-All.e
!! e da una matrice necessaria per l'ottimizzazione della scrittura dei 
!!degli attributi dei dati in export

TYPE vol7d_dballe

  TYPE(vol7d) :: vol7d !< volume vol7d
  type(dbaconnection) :: idbhandle !< handle delle sessioni connesse al DSN DB-All.e
  type(dbasession) :: handle !< handle delle sessioni connesse al DSN DB-All.e
  !> memorizza gli id interni al database DB-All.e per 
  !!ottimizzare le riscritture degli attributi ai dati
  integer ,pointer :: data_id(:,:,:,:,:)
  integer :: category !< log4fortran
  
END TYPE vol7d_dballe

INTEGER, PARAMETER, PRIVATE :: nftype = 2
CHARACTER(len=16), PARAMETER, PRIVATE :: &
 pathlist(2,nftype) = RESHAPE((/ &
 '/usr/share      ', '/usr/local/share', &
 '/etc            ', '/usr/local/etc  ' /), &
 (/2,nftype/))


CHARACTER(len=20),PRIVATE :: dballe_name='wreport', dballe_name_env='DBA_TABLES'


!>\brief inizializza
INTERFACE init
  MODULE PROCEDURE vol7d_dballe_init
END INTERFACE

!>\brief cancella
INTERFACE delete
  MODULE PROCEDURE vol7d_dballe_delete
END INTERFACE


!>\brief importa
INTERFACE import
  MODULE PROCEDURE  vol7d_dballe_importvvnv,vol7d_dballe_import
END INTERFACE

#ifdef NONE
!>\brief exporta
INTERFACE export
  MODULE PROCEDURE vol7d_dballe_export
END INTERFACE
#endif

type record

  !! prime 5 dimensioni
  integer :: data_id
  TYPE(vol7d_ana) :: ana
  TYPE(datetime) :: time
  TYPE(vol7d_level) :: level
  TYPE(vol7d_timerange) :: timerange
  TYPE(vol7d_network) :: network
  !TYPE(vol7d_var) ::  dativar
  CHARACTER(len=10) :: btable
  !! Volumi di valori e attributi per  dati
  REAL :: dator
  REAL(kind=fp_d) :: datod
  INTEGER :: datoi
  INTEGER(kind=int_b) :: datob 
  CHARACTER(len=vol7d_cdatalen) :: datoc 

!  INTEGER(kind=int_b)   :: datiattrb(3)
!  REAL(kind=fp_d),POINTER :: voldatiattrd(:,:,:,:,:,:,:)
!  INTEGER,POINTER :: voldatiattri(:,:,:,:,:,:,:)
!  INTEGER(kind=int_b),POINTER :: voldatiattrb(:,:,:,:,:,:,:)
!  CHARACTER(len=vol7d_cdatalen),POINTER :: voldatiattrc(:,:,:,:,:,:,:)

END TYPE record

PRIVATE
PUBLIC vol7d_dballe, init, delete, import, export

CONTAINS


!>\brief  inizializza l'oggetto
SUBROUTINE vol7d_dballe_init(this,dsn,user,password,write,wipe,repinfo,&
 filename,format,file,categoryappend,time_definition,idbhandle)


TYPE(vol7d_dballe),INTENT(out) :: this !< l'oggetto da inizializzare
character(len=*), INTENT(in),OPTIONAL :: dsn !< per l'accesso al DSN ( default="test" )
character(len=*), INTENT(in),OPTIONAL :: user !< per l'accesso al DSN ( default="test" )
character(len=*), INTENT(in),OPTIONAL :: password !< per l'accesso al DSN ( default="" )
logical,INTENT(in),OPTIONAL :: write !< abilita la scrittura sul DSN/file ( default=.false. )
logical,INTENT(in),OPTIONAL :: wipe !<  svuota il DSN/file e/o lo prepara per una scrittura ( default=.false. )
character(len=*), INTENT(in),OPTIONAL :: repinfo !< eventuale file repinfo.csv usato con wipe ( default="" )
character(len=*),intent(inout),optional :: filename !< nome del file su cui scrivere; se passato ="" ritorna il valore rielaborato
character(len=*),intent(in),optional :: format !< the file format. It can be "BUFR" or "CREX". (default="BUFR")
logical,INTENT(in),OPTIONAL :: file !< switch to use file or data base ( default=.false )
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
integer,INTENT(in),OPTIONAL :: time_definition !< 0=time is reference time ; 1=time is validity time (default=1) 
integer,INTENT(in),OPTIONAL :: idbhandle !< dsn connection; if present it will be used

character(len=1):: mode="r" ! the open mode ("r" for read, "w" for write or create, "a" append) (comandato da "write", default="r" )

character(len=50) :: quidsn,quiuser,quipassword
character(len=255) :: quirepinfo
logical :: quiwrite,quiwipe,quifile

character(len=512) :: a_name
character(len=254) :: arg,lfilename,lformat
logical :: exist
integer :: ier
logical :: read_next


quiwrite=.false.
if (present(write))then
  quiwrite=write
endif

quiwipe=.false.
quirepinfo=""
if (present(wipe))then
  quiwipe=wipe
  if (present(repinfo))then
    quirepinfo=repinfo
  endif
endif


!! TODO wipe and repinfo

!this%idbhandle=dbaconnection()
!this%handle=dbasession()

if (present(categoryappend))then
  call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
  call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
this%category=l4f_category_get(a_name)

nullify(this%data_id)

!TODO: quando scrivo bisogna gestire questo che non è da fare ?
CALL init(this%vol7d,time_definition=time_definition)

if (optio_log(file)) then

  lformat="BUFR"
  if (present(format))then
    lformat=format
  end if
  

  lfilename=trim(arg)//"."//trim(lformat)
  if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

  if (present(filename))then
    if (filename /= "")then
      lfilename=filename
    end if
  end if


  if (.not. quiwrite) then
    print *,"import in mem:"

    this%idbhandle=dbaconnection(dsn="mem:",user="",password="")
    this%handle=dbasession(this%idbhandle,"write","write","write")
    call this%handle%messages_open_input(lfilename, mode, lformat, simplified=.true.)
    read_next = this%handle%messages_read_next()
    do while (read_next) 
      read_next = this%handle%messages_read_next()
    end do

    print *,"end import in mem:"
  end if

else


    quidsn = "test"
    quiuser = "test"
    quipassword = ""
    IF (PRESENT(dsn)) THEN
      IF (c_e(dsn)) quidsn = dsn
    ENDIF
    IF (PRESENT(user)) THEN
      IF (c_e(user)) quiuser = user
    ENDIF
    IF (PRESENT(password)) THEN
      IF (c_e(password)) quipassword = password
    ENDIF

    this%idbhandle=dbaconnection(quidsn,quiuser,quipassword,idbhandle=idbhandle)

  if(quiwrite)then
    this%handle=dbasession(this%idbhandle,"write","write","write")
  else
    this%handle=dbasession(this%idbhandle,"read","read","read")
  end if
  
endif

END SUBROUTINE vol7d_dballe_init



!>\brief Identica a vol7d_dballe_importvsns con var e network vettore.
!!
!!import da DB-all.e

SUBROUTINE vol7d_dballe_importvvnv(this, var, network, coordmin,coordmax, timei, timef, level,timerange,set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)
TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in) :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(vol7d_ana),INTENT(inout),optional :: ana
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_network),INTENT(in) :: network(:)
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)
logical,intent(in),optional :: anaonly

INTEGER :: i

if (size(network) == 0 )then
  CALL import(this,var, coordmin=coordmin, coordmax=coordmax, timei=timei, timef=timef, level=level,&
   timerange=timerange,set_network=set_network, attr=attr,anavar=anavar,anaattr=anaattr,&
   varkind=varkind,attrkind=attrkind,anavarkind=anavarkind,anaattrkind=anaattrkind,anaonly=anaonly,ana=ana)
else
  DO i = 1, SIZE(network)
    CALL import(this, var, network(i), coordmin, coordmax, timei, timef, level,timerange,set_network,&
     attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)
  ENDDO
end if

END SUBROUTINE vol7d_dballe_importvvnv

!!import da DB-all.e
SUBROUTINE vol7d_dballe_import(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(vol7d_ana),INTENT(inout),optional :: ana
TYPE(datetime),INTENT(in),OPTIONAL :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network,set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)
logical,intent(in),optional :: anaonly

TYPE(vol7d_network) :: lnetwork
TYPE(vol7d_level) :: llevel
TYPE(vol7d_timerange) :: ltimerange

INTEGER,PARAMETER :: maxvarlist=100
!TYPE(vol7d) :: v7d
! da non fare (con gfortran?)!!!!!
!CHARACTER(len=SIZE(var)*7) :: varlist
!CHARACTER(len=SIZE(attr)*8) :: starvarlist
CHARACTER(len=maxvarlist*7) :: varlist
CHARACTER(len=maxvarlist*8) :: starvarlist
CHARACTER(len=6) :: btable
CHARACTER(len=7) ::starbtable

LOGICAL ::  ldegnet, lattr, lanaattr
integer :: year,month,day,hour,minute,sec,msec
integer :: rlevel1, rl1,rlevel2, rl2
integer :: rtimerange, p1, p2
character(len=network_name_len) :: rep_memo
integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork


integer :: nana,ntime,ntimerange,nlevel,nnetwork
TYPE(vol7d_var) :: var_tmp

INTEGER :: i,ii, iii,n,n_ana,nn,nvarattr,istat,indattr
integer :: nvar ,inddatiattr,inddativarattr
integer :: nanavar ,indanavar,indanaattr,indanavarattr,nanavarattr

INTEGER(kind=int_l) :: ilat,ilon
CHARACTER(len=vol7d_ana_lenident) :: ident
!CHARACTER(len=10),allocatable :: lvar(:), lanavar(:)
class(dbadcv),allocatable :: vars,starvars,anavars,anastarvars
type(dbafilter)    :: filter
type(dbacoord)     :: mydbacoordmin, mydbacoordmax
type(dbaana)       :: mydbaana
type(dbadatetime)  :: mydatetimemin, mydatetimemax
type(dbatimerange) :: mydbatimerange
type(dbalevel)     :: mydbalevel
type(dbanetwork)   :: mydbanetwork


!INTEGER(kind=int_b)::attrdatib

integer :: ndativarr,     ndativari,     ndativarb,     ndativard,     ndativarc
integer :: ndatiattrr,    ndatiattri,    ndatiattrb,    ndatiattrd,    ndatiattrc 
integer :: ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc

integer :: nanavarr,     nanavari,     nanavarb,     nanavard,     nanavarc
integer :: nanaattrr,    nanaattri,    nanaattrb,    nanaattrd,    nanaattrc 
integer :: nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc

integer :: ir,ib,id,ic,ier,nanaattr,nattr

TYPE(vol7d) :: vol7dtmp

type(record),ALLOCATABLE :: buffer(:),bufferana(:)
character(len=40) :: query
type(dbametaanddata),allocatable :: metaanddatav(:)
logical :: dataonly

!!!  CALL print_info('Estratte dall''archivio '//TRIM(to_char(nobs)) // ' osservazioni')

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'inizio')
#endif        

IF (PRESENT(set_network)) THEN
  if (c_e(set_network)) then
    ldegnet = .TRUE.
  else
    ldegnet = .FALSE.
  end if
ELSE
  ldegnet = .FALSE.
ENDIF

if(ldegnet) then
  query = "best"
else
  query=cmiss
end if


nvar=0
if (present(var)) then
  nvar=count(c_e(var))
  if (nvar > 0) then
    allocate (vars%dcv(nvar))
    do i=1,size(var)
      if (c_e(var(i)))then
        if (present(varkind))then
          select case (varkind(i))
          case("r")
            allocate (vars%dcv(i)%dat,source=dbadatar(var(i)))
          case("i")
            allocate (vars%dcv(i)%dat,source=dbadatai(var(i)))
          case("b")
            allocate (vars%dcv(i)%dat,source=dbadatab(var(i)))
          case("d")
            allocate (vars%dcv(i)%dat,source=dbadatad(var(i)))
          case("c")
            allocate (vars%dcv(i)%dat,source=dbadatac(var(i)))
          case default
            call l4f_category_log(this%category,L4F_ERROR,"var and varkind mismach")
            CALL raise_fatal_error()
          end select
        end if
      else 
        allocate (vars%dcv(i)%dat,source=dbadatac(var(i))) !char is default
      end if
    end do
  end if
end if



if (present(anavar)) then
  nanavar=count(c_e(anavar))
  if (nanavar > 0) then
    allocate (anavars%dcv(nanavar))
    do i=1,size(anavar)
      if (c_e(anavar(i)))then
        if (present(anavarkind))then
          select case (anavarkind(i))
          case("r")
            allocate (anavars%dcv(i)%dat,source=dbadatar(anavar(i)))
          case("i")
            allocate (anavars%dcv(i)%dat,source=dbadatai(anavar(i)))
          case("b")
            allocate (anavars%dcv(i)%dat,source=dbadatab(anavar(i)))
          case("d")
            allocate (anavars%dcv(i)%dat,source=dbadatad(anavar(i)))
          case("c")
            allocate (anavars%dcv(i)%dat,source=dbadatac(anavar(i)))
          case default
            call l4f_category_log(this%category,L4F_ERROR,"anavar and anavarkind mismach")
            CALL raise_fatal_error()
          end select
        end if
      else 
        allocate (anavars%dcv(i)%dat,source=dbadatac(anavar(i))) !char is default
      end if
    end do
  end if
end if


lattr = .false.
if (present(attr)) then
  nattr=count(c_e(attr))
  if (nattr > 0) then
    lattr = .true.
    allocate (starvars%dcv(nattr))
    do i=1,size(attr)
      if (c_e(attr(i)))then
        if (present(attrkind))then
          select case (attrkind(i))
          case("r")
            allocate (starvars%dcv(i)%dat,source=dbadatar(attr(i)))
          case("i")
            allocate (starvars%dcv(i)%dat,source=dbadatai(attr(i)))
          case("b")
            allocate (starvars%dcv(i)%dat,source=dbadatab(attr(i)))
          case("d")
            allocate (starvars%dcv(i)%dat,source=dbadatad(attr(i)))
          case("c")
            allocate (starvars%dcv(i)%dat,source=dbadatac(attr(i)))
          case default
            call l4f_category_log(this%category,L4F_ERROR,"attr and attrkind mismach")
            CALL raise_fatal_error()
          end select
        end if
      else 
        allocate (starvars%dcv(i)%dat,source=dbadatac(attr(i))) !char is default
      end if
    end do
  end if
end if

lanaattr = .false.
if (present(anaattr)) then
  nanaattr=count(c_e(anaattr))
  if (nanaattr > 0) then
    lanaattr = .true.
    allocate (anastarvars%dcv(nanaattr))
    do i=1,size(anaattr)
      if (c_e(anaattr(i)))then
        if (present(anaattrkind))then
          select case (anaattrkind(i))
          case("r")
            allocate (anastarvars%dcv(i)%dat,source=dbadatar(anaattr(i)))
          case("i")
            allocate (anastarvars%dcv(i)%dat,source=dbadatai(anaattr(i)))
          case("b")
            allocate (anastarvars%dcv(i)%dat,source=dbadatab(anaattr(i)))
          case("d")
            allocate (anastarvars%dcv(i)%dat,source=dbadatad(anaattr(i)))
          case("c")
            allocate (anastarvars%dcv(i)%dat,source=dbadatac(anaattr(i)))
          case default
            call l4f_category_log(this%category,L4F_ERROR,"attr and attrkind mismach")
            CALL raise_fatal_error()
          end select
        end if
      else 
        allocate (anastarvars%dcv(i)%dat,source=dbadatac(anaattr(i))) !char is default
      end if
    end do
  end if
end if


N=0
if ( .not. optio_log(anaonly)) then

  ! like a cast
  mydbacoordmin%geo_coord=coordmin
  mydbacoordmax%geo_coord=coordmax
  mydbaana%vol7d_ana=ana
  mydatetimemin%datetime=timei
  mydatetimemax%datetime=timef
  mydbatimerange%vol7d_timerange=timerange
  mydbalevel%vol7d_level=level
  mydbanetwork%vol7d_network=network

  filter=dbafilter(coordmin=mydbacoordmin,coordmax=mydbacoordmax,ana=mydbaana, &
   datetimemin=mydatetimemin,datetimemax=mydatetimemax, &
   timerange=mydbatimerange,level=mydbalevel,network=mydbanetwork,query=query,&
   vars=vars,starvars=starvars)


!!$  filter=dbafilter(coordmin=coordmin,coordmax=coordmax,ana=ana,datetimemin=timei,datetimemax=timef,&
!!$   timerange=timerange,level=level,network=network,query=query,&
!!$   vars=vars,starvars=starvars)
  
                                ! take in account time_definition
  if (this%vol7d%time_definition == 0) buffer(i)%time = buffer(i)%time - &
   timedelta_new(msec=buffer(i)%timerange%p1*1000)


print *, "filter:"
call filter%display()
call this%handle%set(filter=filter)
if (allocated(starvars%dcv)) then
  dataonly=.false.
else
  dataonly=.true.
end if

call this%handle%extrude(metaanddatav,filter=filter,dataonly=dataonly)

end if


! ---------------->   anagrafica

!ora legge tutti i dati di anagrafica e li mette in bufferana

filter=dbafilter(filter=filter,contextana=.true.)
deallocate (metaanddatav)
call this%handle%extrude(metaanddatav,filter=filter,dataonly=dataonly)

!  ier=idba_enqdate (this%handle_staz,year,month,day,hour,minute,sec)
!  IF (.NOT.c_e(sec)) sec = 0
!  ier=idba_enq (this%handle_staz,"context_id",bufferana(i)%data_id)


! ---------------->   anagrafica fine

#ifdef NONE

if (.not. any(c_e(lvar)))then
  nvar = count_distinct(buffer%btable, back=.TRUE.)
end if

if (optio_log(anaonly)) then
  nana = count_distinct(bufferana%ana, back=.TRUE.)
else
  nana = count_distinct(buffer%ana, back=.TRUE.)
end if

if(ldegnet) then
  nnetwork=1
else
  if (optio_log(anaonly)) then
    nnetwork = count_distinct(bufferana%network, back=.TRUE.)
  else
    nnetwork = count_distinct(buffer%network, back=.TRUE.)
  end if
end if


ntime = count_distinct(buffer%time, back=.TRUE.)
ntimerange = count_distinct(buffer%timerange, back=.TRUE.)
nlevel = count_distinct(buffer%level, back=.TRUE.)


if (present(varkind))then
  ndativarr= count(varkind == "r")
  ndativari= count(varkind == "i")
  ndativarb= count(varkind == "b")
  ndativard= count(varkind == "d")
  ndativarc= count(varkind == "c")
  
else
  ndativarr= 0
  ndativari= 0
  ndativarb= 0
  ndativard= 0
  ndativarc= nvar
end if

!print *, "nana=",nana," ntime=",ntime," ntimerange=",ntimerange, &
!" nlevel=",nlevel," nnetwork=",nnetwork," ndativarr=",ndativarr

if (lattr)then

  if (present(attrkind))then
    ndatiattrr= count(attrkind == "r")
    ndatiattri= count(attrkind == "i")
    ndatiattrb= count(attrkind == "b")
    ndatiattrd= count(attrkind == "d")
    ndatiattrc= count(attrkind == "c")
    
  else
    ndatiattrr= 0
    ndatiattri= 0
    ndatiattrb= 0
    ndatiattrd= 0
    ndatiattrc= size(attr)
  end if
  
else
  ndatiattrr=0
  ndatiattri=0
  ndatiattrb=0
  ndatiattrd=0
  ndatiattrc=0
end if

ndativarattrr=0
ndativarattri=0
ndativarattrb=0
ndativarattrd=0
ndativarattrc=0

if (ndatiattrr > 0 ) ndativarattrr=ndativarr+ndativari+ndativarb+ndativard+ndativarc
if (ndatiattri > 0 ) ndativarattri=ndativarr+ndativari+ndativarb+ndativard+ndativarc
if (ndatiattrb > 0 ) ndativarattrb=ndativarr+ndativari+ndativarb+ndativard+ndativarc
if (ndatiattrd > 0 ) ndativarattrd=ndativarr+ndativari+ndativarb+ndativard+ndativarc
if (ndatiattrc > 0 ) ndativarattrc=ndativarr+ndativari+ndativarb+ndativard+ndativarc


! ---------------->   anagrafica

if ( size(lanavar) == 0 )then
  nanavar = count_distinct(bufferana%btable, back=.TRUE.,mask=(bufferana%btable /= DBA_MVC))
end if

if (present(anavarkind))then
  nanavarr= count(anavarkind == "r")
  nanavari= count(anavarkind == "i")
  nanavarb= count(anavarkind == "b")
  nanavard= count(anavarkind == "d")
  nanavarc= count(anavarkind == "c")
  
else
  nanavarr= 0
  nanavari= 0
  nanavarb= 0
  nanavard= 0
  nanavarc= nanavar
end if

!print *, "nana=",nana," ntime=",ntime," ntimerange=",ntimerange, &
!" nlevel=",nlevel," nnetwork=",nnetwork," ndativarr=",ndativarr

if (lanaattr)then

  if (present(anaattrkind))then
    nanaattrr= count(anaattrkind == "r")
    nanaattri= count(anaattrkind == "i")
    nanaattrb= count(anaattrkind == "b")
    nanaattrd= count(anaattrkind == "d")
    nanaattrc= count(anaattrkind == "c")
    
  else
    nanaattrr= 0
    nanaattri= 0
    nanaattrb= 0
    nanaattrd= 0
    nanaattrc= size(anaattr)
  end if
  
else
  nanaattrr=0
  nanaattri=0
  nanaattrb=0
  nanaattrd=0
  nanaattrc=0
end if

nanavarattrr=0
nanavarattri=0
nanavarattrb=0
nanavarattrd=0
nanavarattrc=0

if (nanaattrr > 0 ) nanavarattrr=nanavarr+nanavari+nanavarb+nanavard+nanavarc
if (nanaattri > 0 ) nanavarattri=nanavarr+nanavari+nanavarb+nanavard+nanavarc
if (nanaattrb > 0 ) nanavarattrb=nanavarr+nanavari+nanavarb+nanavard+nanavarc
if (nanaattrd > 0 ) nanavarattrd=nanavarr+nanavari+nanavarb+nanavard+nanavarc
if (nanaattrc > 0 ) nanavarattrc=nanavarr+nanavari+nanavarb+nanavard+nanavarc

! ---------------->   anagrafica fine


CALL init(vol7dtmp,time_definition=this%vol7d%time_definition)

!print*,"ho fatto init"

call vol7d_alloc (vol7dtmp, &
 nana=nana, ntime=ntime, ntimerange=ntimerange, &
 nlevel=nlevel, nnetwork=nnetwork, &
 ndativarr=ndativarr, ndativari=ndativari, ndativarb=ndativarb, ndativard=ndativard, ndativarc=ndativarc,&
 ndatiattrr=ndatiattrr, ndatiattri=ndatiattri, ndatiattrb=ndatiattrb, ndatiattrd=ndatiattrd, ndatiattrc=ndatiattrc,&
 ndativarattrr=ndativarattrr, &
 ndativarattri=ndativarattri, &
 ndativarattrb=ndativarattrb, &
 ndativarattrd=ndativarattrd, &
 ndativarattrc=ndativarattrc,&
 nanavarr=nanavarr, nanavari=nanavari, nanavarb=nanavarb, nanavard=nanavard, nanavarc=nanavarc,&
 nanaattrr=nanaattrr, nanaattri=nanaattri, nanaattrb=nanaattrb, nanaattrd=nanaattrd, nanaattrc=nanaattrc,&
 nanavarattrr=nanavarattrr, &
 nanavarattri=nanavarattri, &
 nanavarattrb=nanavarattrb, &
 nanavarattrd=nanavarattrd, &
 nanavarattrc=nanavarattrc)

!!$print *, "nana=",nana, "ntime=",ntime, "ntimerange=",ntimerange, &
!!$ "nlevel=",nlevel, "nnetwork=",nnetwork, &
!!$ "ndativarr=",ndativarr, "ndativari=",ndativari, &
!!$ "ndativarb=",ndativarb, "ndativard=",ndativard, "ndativarc=",ndativarc,&
!!$ "ndatiattrr=",ndatiattrr, "ndatiattri=",ndatiattri, "ndatiattrb=",ndatiattrb,&
!!$ "ndatiattrd=",ndatiattrd, "ndatiattrc=",ndatiattrc,&
!!$ "ndativarattrr=",ndativarattrr, "ndativarattri=",ndativarattri, "ndativarattrb=",ndativarattrb,&
!!$ "ndativarattrd=",ndativarattrd, "ndativarattrc=",ndativarattrc
!!$print*,"ho fatto alloc"

if (optio_log(anaonly)) then
  vol7dtmp%ana=pack_distinct(bufferana%ana, nana, back=.TRUE.)
else
  vol7dtmp%ana=pack_distinct(buffer%ana, nana, back=.TRUE.)  
endif

vol7dtmp%time=pack_distinct(buffer%time, ntime, back=.TRUE.)
call sort(vol7dtmp%time)

vol7dtmp%timerange=pack_distinct(buffer%timerange, ntimerange, back=.TRUE.)
call sort(vol7dtmp%timerange)

vol7dtmp%level=pack_distinct(buffer%level, nlevel, back=.TRUE.)
call sort(vol7dtmp%level)

if(ldegnet)then
  vol7dtmp%network(1)=set_network
else
  if (optio_log(anaonly)) then
    vol7dtmp%network=pack_distinct(bufferana%network, nnetwork, back=.TRUE.)
  else
    vol7dtmp%network=pack_distinct(buffer%network, nnetwork, back=.TRUE.)
  end if
end if

!print*,"reti presenti", vol7dtmp%network%name,buffer%network%name

if (any(c_e(lvar)).and. present(varkind))then

  ir=0
  ii=0
  ib=0
  id=0
  ic=0
  
  do i=1,size(varkind)
    if (varkind(i) == "r") then
      ir=ir+1
      call init (vol7dtmp%dativar%r(ir), btable=var(i))
    end if
    if (varkind(i) == "i") then
      ii=ii+1
      call init (vol7dtmp%dativar%i(ii), btable=var(i))
    end if
    if (varkind(i) == "b") then
      ib=ib+1
      call init (vol7dtmp%dativar%b(ib), btable=var(i))
    end if
    if (varkind(i) == "d") then
      id=id+1
      call init (vol7dtmp%dativar%d(id), btable=var(i))
    end if
    if (varkind(i) == "c") then
      ic=ic+1
      call init (vol7dtmp%dativar%c(ic), btable=var(i))  
    end if
  end do
else if (any(c_e(lvar)))then
    do i=1, nvar
      call init (vol7dtmp%dativar%c(i), btable=var(i))
    end do
else

  do i=1,ndativarc
    call init(vol7dtmp%dativar%c(i))
  end do

  if (ndativarc > 0) then 
    call pack_distinct_c(buffer%btable, vol7dtmp%dativar%c%btable, back=.TRUE.,mask=(buffer%btable /= DBA_MVC))
  end if

end if



if ( present(attrkind).and. present(attr).and. any(c_e(lvar)))then

    ir=0
    ii=0
    ib=0
    id=0
    ic=0

  do i=1,size(lvar)
  
    if ( ndativarattrr > 0 )then
      ir=ir+1
      call init (vol7dtmp%dativarattr%r(ir), btable=lvar(i))
    end if

    if ( ndativarattri > 0 )then
      ii=ii+1
      call init (vol7dtmp%dativarattr%i(ii), btable=lvar(i))
    end if

    if ( ndativarattrb > 0 )then
      ib=ib+1
      call init (vol7dtmp%dativarattr%b(ib), btable=lvar(i))
    end if

    if ( ndativarattrd > 0 )then
      id=id+1
      call init (vol7dtmp%dativarattr%d(id), btable=lvar(i))
    end if

    if ( ndativarattrc > 0 )then
      ic=ic+1
      call init (vol7dtmp%dativarattr%c(ic), btable=lvar(i))
    end if

  end do

else  if (present(attr).and. any(c_e(lvar)))then

  do i=1,size(lvar)
    if ( ndativarattrc > 0 )call init (vol7dtmp%dativarattr%c(i), btable=lvar(i))
  end do

else if (associated(vol7dtmp%dativarattr%c).and. associated(vol7dtmp%dativar%c)) then

      vol7dtmp%dativarattr%c=vol7dtmp%dativar%c

end if


if (present(attrkind).and. lattr)then

  ir=0
  ii=0
  ib=0
  id=0
  ic=0

  do i=1,size(attrkind)

    if (attrkind(i) == "r") then
      ir=ir+1
      call init (vol7dtmp%datiattr%r(ir), btable=attr(i))
    end if
    if (attrkind(i) == "i") then
      ii=ii+1
      call init (vol7dtmp%datiattr%i(ii), btable=attr(i))
    end if
    if (attrkind(i) == "b") then
      ib=ib+1
      call init (vol7dtmp%datiattr%b(ib), btable=attr(i))
    end if
    if (attrkind(i) == "d") then
      id=id+1
      call init (vol7dtmp%datiattr%d(id), btable=attr(i))
    end if
    if (attrkind(i) == "c") then
      ic=ic+1
      call init (vol7dtmp%datiattr%c(ic), btable=attr(i))  
    end if
  end do
else  if (present(attr))then

  do i=1, size(attr)
    call init (vol7dtmp%datiattr%c(i), btable=attr(i))
  end do

end if

!-----------------------> anagrafica

if ( size(lanavar) > 0 .and. present(anavarkind))then

  ir=0
  ii=0
  ib=0
  id=0
  ic=0
  
  do i=1,size(anavarkind)
    if (anavarkind(i) == "r") then
      ir=ir+1
      call init (vol7dtmp%anavar%r(ir), btable=anavar(i))
    end if
    if (anavarkind(i) == "i") then
      ii=ii+1
      call init (vol7dtmp%anavar%i(ii), btable=anavar(i))
    end if
    if (anavarkind(i) == "b") then
      ib=ib+1
      call init (vol7dtmp%anavar%b(ib), btable=anavar(i))
    end if
    if (anavarkind(i) == "d") then
      id=id+1
      call init (vol7dtmp%anavar%d(id), btable=anavar(i))
    end if
    if (anavarkind(i) == "c") then
      ic=ic+1
      call init (vol7dtmp%anavar%c(ic), btable=anavar(i))  
    end if
  end do
else if ( size(lanavar) > 0 )then

  do i=1, nanavar
    call init (vol7dtmp%anavar%c(i), btable=anavar(i))
  end do

else

  do i=1,nanavarc
    call init(vol7dtmp%anavar%c(i))
  end do
  if (nanavarc > 0) then
    call pack_distinct_c(bufferana%btable, vol7dtmp%anavar%c%btable, back=.TRUE.,mask=(bufferana%btable /= DBA_MVC))
  end if
end if



if ( present(anaattrkind) .and. present(anaattr) .and. size(anavar) > 0 )then

    ir=0
    ii=0
    ib=0
    id=0
    ic=0

  do i=1,size(anavar)
  
    if ( nanavarattrr > 0 )then
      ir=ir+1
      call init (vol7dtmp%anavarattr%r(ir), btable=anavar(i))
    end if

    if ( nanavarattri > 0 )then
      ii=ii+1
      call init (vol7dtmp%anavarattr%i(ii), btable=anavar(i))
    end if

    if ( nanavarattrb > 0 )then
      ib=ib+1
      call init (vol7dtmp%anavarattr%b(ib), btable=anavar(i))
    end if

    if ( nanavarattrd > 0 )then
      id=id+1
      call init (vol7dtmp%anavarattr%d(id), btable=anavar(i))
    end if

    if ( nanavarattrc > 0 )then
      ic=ic+1
      call init (vol7dtmp%anavarattr%c(ic), btable=anavar(i))
    end if

  end do

else  if (present(anaattr) .and. size(anavar) > 0 )then

  do i=1,size(anavar)
    if ( nanavarattrc > 0 )call init(vol7dtmp%anavarattr%c(i), btable=anavar(i))
  end do

else if (associated(vol7dtmp%anavarattr%c) .and. associated(vol7dtmp%anavar%c)) then

    vol7dtmp%anavarattr%c=vol7dtmp%anavar%c

end if


if (present(anaattrkind).and. present(anaattr))then

  ir=0
  ii=0
  ib=0
  id=0
  ic=0

  do i=1,size(anaattrkind)

    if (anaattrkind(i) == "r") then
      ir=ir+1
      call init (vol7dtmp%anaattr%r(ir), btable=anaattr(i))
    end if
    if (anaattrkind(i) == "i") then
      ii=ii+1
      call init (vol7dtmp%anaattr%i(ii), btable=anaattr(i))
    end if
    if (anaattrkind(i) == "b") then
      ib=ib+1
      call init (vol7dtmp%anaattr%b(ib), btable=anaattr(i))
    end if
    if (anaattrkind(i) == "d") then
      id=id+1
      call init (vol7dtmp%anaattr%d(id), btable=anaattr(i))
    end if
    if (anaattrkind(i) == "c") then
      ic=ic+1
      call init (vol7dtmp%anaattr%c(ic), btable=anaattr(i))  
    end if
  end do
else if (present(anaattr))then

  do i=1, size(anaattr)
    call init (vol7dtmp%anaattr%c(i), btable=anaattr(i))
  end do

end if


!print*,"numero variabili anagrafica",size(vol7dtmp%anavar%r)
!do i=1,size(vol7dtmp%anavar%r)
!  print*,"elenco variabili anagrafica>",vol7dtmp%anavar%r(i)%btable,"<fine"
!end do

!-----------------------> anagrafica fine

call vol7d_alloc_vol (vol7dtmp)

if (lattr) then

  allocate  (this%data_id( nana, ntime, nlevel, ntimerange, nnetwork),stat=istat)
  if (istat/= 0) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,'cannot allocate ' &
     //TRIM(to_char(nana*ntime*nlevel*ntimerange*nnetwork))//' data_id elements')
    CALL raise_fatal_error()
    
  ENDIF

  this%data_id=DBA_MVI

else

  nullify(this%data_id)

end if


if (lattr)then

  IF (SIZE(attr) > maxvarlist) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"too many attributes requested: "//t2c(SIZE(attr)))
    call raise_fatal_error()
  ENDIF

                                ! creo la stringa con l'elenco delle variabili di attributo
  starvarlist = ''
  nvarattr=0
  DO ii = 1, SIZE(attr)
    nvarattr = nvarattr + 1
    IF (nvarattr > 1) starvarlist(LEN_TRIM(starvarlist)+1:) = ',' 
    starvarlist(LEN_TRIM(starvarlist)+1:) = TRIM(attr(ii))
  ENDDO
     !print *,"starvarlist",starvarlist

end if

do i =1, N

   indana = firsttrue(buffer(i)%ana == vol7dtmp%ana)
   indtime = firsttrue(buffer(i)%time == vol7dtmp%time)
   indtimerange = firsttrue(buffer(i)%timerange == vol7dtmp%timerange)
   indlevel = firsttrue(buffer(i)%level == vol7dtmp%level)
   if (ldegnet)then
     indnetwork=1
   else
     indnetwork = firsttrue(buffer(i)%network == vol7dtmp%network)
   endif
   !print *, indana,indtime,indlevel,indtimerange,indnetwork

   if(c_e(buffer(i)%dator))then
     inddativar = firsttrue(buffer(i)%btable == vol7dtmp%dativar%r%btable)
     vol7dtmp%voldatir( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%dator
   end if

   if(c_e(buffer(i)%datoi)) then
     inddativar = firsttrue(buffer(i)%btable == vol7dtmp%dativar%i%btable)
     vol7dtmp%voldatii( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datoi
   end if

   if(c_e(buffer(i)%datob)) then
     inddativar = firsttrue(buffer(i)%btable == vol7dtmp%dativar%b%btable)
     vol7dtmp%voldatib( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datob
   end if

   if(c_e(buffer(i)%datod)) then
     inddativar = firsttrue(buffer(i)%btable == vol7dtmp%dativar%d%btable)
     vol7dtmp%voldatid( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datod
   end if

   if(c_e(buffer(i)%datoc)) then
     inddativar = firsttrue(buffer(i)%btable == vol7dtmp%dativar%c%btable)
     vol7dtmp%voldatic( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datoc
   end if

   if (lattr)then

                                !memorizzo data_id
#ifdef DEBUG
     !CALL l4f_category_log(this%category,L4F_DEBUG,"data_id: "//trim(to_char(buffer(i)%data_id)))
#endif

     this%data_id(indana,indtime,indlevel,indtimerange,indnetwork)=buffer(i)%data_id

     ier=idba_unsetall (this%handle)
#ifdef DEBUG
     CALL l4f_category_log(this%category,L4F_DEBUG,'unsetall handle')
#endif
     ier=idba_set (this%handle,"*context_id",buffer(i)%data_id)
     ier=idba_set (this%handle,"*var_related",buffer(i)%btable)
     !per ogni dato ora lavoro sugli attributi
     ier=idba_set(this%handle, "*varlist",starvarlist )
     ier=idba_voglioancora (this%handle,nn)
     !print*,buffer(i)%btable," numero attributi",nn
     
     do ii=1,nn ! Se ho piu` di 1 attributo devo forse trovare l'indice (ii)
       ier=idba_ancora (this%handle,starbtable)
         !print *, starbtable
       indattr = firsttrue(attr == starbtable)
       IF (indattr<1) cycle ! non c'e'

       call init (var_tmp, btable=starbtable)

       if (present(attrkind))then
         iii=( firsttrue(attr == starbtable))
         !print *,"ho letto indice attributo ",starbtable,iii 
         if (iii > 0)then

!>\todo sostituire qui sotto con struttura case:
           if(attrkind(iii) == "r") then
             inddativarattr  = firsttrue(buffer(i)%btable == vol7dtmp%dativarattr%r%btable)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%r)
             ier=idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattrr(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
           if(attrkind(iii) == "i") then
             inddativarattr  = firsttrue(buffer(i)%btable == vol7dtmp%dativarattr%i%btable)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%i)
             ier=idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattri(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
           if(attrkind(iii) == "b") then
             inddativarattr  = firsttrue(buffer(i)%btable == vol7dtmp%dativarattr%b%btable)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%b)
             !print *,"indici voldatiattr ",indana,indtime,indlevel,indtimerange,&
              !inddativarattr,indnetwork,inddatiattr
             ier=idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattrb(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
           if(attrkind(iii) == "d") then
             inddativarattr  = firsttrue(buffer(i)%btable == vol7dtmp%dativarattr%d%btable)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%d)
             ier=idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattrd(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
           if(attrkind(iii) == "c") then
             inddativarattr  = firsttrue(buffer(i)%btable == vol7dtmp%dativarattr%c%btable)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%c)
             ier=idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattrc(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
         end if
       else

         inddativarattr  = firsttrue(buffer(i)%btable == vol7dtmp%dativarattr%c%btable)
         inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%c)
         ier=idba_enq (this%handle,starbtable,&
          vol7dtmp%voldatiattrc(indana,indtime,indlevel,indtimerange,&
          inddativarattr,indnetwork,inddatiattr)) !char is default
         !print*,starbtable,vol7dtmp%voldatiattrc(indana,indtime,indlevel,indtimerange,&
         ! inddativarattr,indnetwork,inddatiattr)
       end if

     end do
   end if

!( voldati*(nana,ntime,nlevel,ntimerange,ndativar*,nnetwork)
!  voldatiattr*(nana,ntime,nlevel,ntimerange,ndativarattr*,network,ndatiattr*) )

 end do

!------------------------- anagrafica


if (lanaattr)then
                                ! creo la stringa con l'elenco variabili attributi di anagrafica
  starvarlist = ''
  nanavarattr=0
  DO ii = 1, SIZE(anaattr)
    nanavarattr = nanavarattr + 1
    IF (nanavarattr > 1) starvarlist(LEN_TRIM(starvarlist)+1:) = ',' 
    starvarlist(LEN_TRIM(starvarlist)+1:) = TRIM(anaattr(ii))
  ENDDO
                                !print *,"starvarlist",starvarlist
end if


do i =1, N_ana

   indana = firsttrue(bufferana(i)%ana == vol7dtmp%ana)

   if (ldegnet)then
     indnetwork=1
   else
     indnetwork = firsttrue(bufferana(i)%network == vol7dtmp%network)
   endif

   if (indana < 1 .or. indnetwork < 1 )cycle

   !print *, indana,indtime,indlevel,indtimerange,indnetwork

   if(c_e(bufferana(i)%dator))then
     indanavar = firsttrue(bufferana(i)%btable == vol7dtmp%anavar%r%btable)
     vol7dtmp%volanar( indana,indanavar,indnetwork ) = bufferana(i)%dator
   end if
   if(c_e(bufferana(i)%datoi))then
     indanavar = firsttrue(bufferana(i)%btable == vol7dtmp%anavar%i%btable)
     vol7dtmp%volanai( indana,indanavar,indnetwork ) = bufferana(i)%datoi
   end if
   if(c_e(bufferana(i)%datob))then
     indanavar = firsttrue(bufferana(i)%btable == vol7dtmp%anavar%b%btable)
     vol7dtmp%volanab( indana,indanavar,indnetwork ) = bufferana(i)%datob
   end if
   if(c_e(bufferana(i)%datod))then
     indanavar = firsttrue(bufferana(i)%btable == vol7dtmp%anavar%d%btable)
     vol7dtmp%volanad( indana,indanavar,indnetwork ) = bufferana(i)%datod
   end if
   if(c_e(bufferana(i)%datoc))then
     indanavar = firsttrue(bufferana(i)%btable == vol7dtmp%anavar%c%btable)
     vol7dtmp%volanac( indana,indanavar,indnetwork ) = bufferana(i)%datoc
   end if


   if (lanaattr)then

#ifdef DEBUG
     CALL l4f_category_log(this%category,L4F_DEBUG,'unsetall handle_staz')
#endif
     ier=idba_unsetall (this%handle_staz)
     ier=idba_set (this%handle_staz,"*context_id",bufferana(i)%data_id)
     ier=idba_set (this%handle_staz,"*var_related",bufferana(i)%btable)

     !per ogni dato ora lavoro sugli attributi
     ier=idba_set(this%handle_staz, "*varlist",starvarlist )
     ier=idba_voglioancora (this%handle_staz,nn)
     !print*,buffer(i)%dativar%btable," numero attributi",nn
     
     do ii=1,nn ! Se ho piu` di 1 attributo devo forse trovare l'indice (ii)
       ier=idba_ancora (this%handle_staz,starbtable)
         !print *, starbtable
       indattr = firsttrue(anaattr == starbtable)
       IF (indattr<1) cycle ! non c'e'


       call init (var_tmp, btable=starbtable)


       if (present(anaattrkind))then
         iii=( firsttrue(anaattr == starbtable))
         !print *,"ho letto indice attributo ",starbtable,iii 
         if (iii > 0)then

           if(anaattrkind(iii) == "r") then
             indanavarattr  = firsttrue(bufferana(i)%btable == vol7dtmp%anavarattr%r%btable)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%r)
             ier=idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattrr(indana,indanavarattr,indnetwork,indanaattr))
           end if
           if(anaattrkind(iii) == "i") then
             indanavarattr  = firsttrue(bufferana(i)%btable == vol7dtmp%anavarattr%i%btable)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%i)
             ier=idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattri(indana,indanavarattr,indnetwork,indanaattr))
           end if
           if(anaattrkind(iii) == "b") then
             indanavarattr  = firsttrue(bufferana(i)%btable == vol7dtmp%anavarattr%b%btable)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%b)
             ier=idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattrb(indana,indanavarattr,indnetwork,indanaattr))
           end if
           if(anaattrkind(iii) == "d") then
             indanavarattr  = firsttrue(bufferana(i)%btable == vol7dtmp%anavarattr%d%btable)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%d)
             ier=idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattrd(indana,indanavarattr,indnetwork,indanaattr))
           end if
           if(anaattrkind(iii) == "c") then
             indanavarattr  = firsttrue(bufferana(i)%btable == vol7dtmp%anavarattr%c%btable)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%c)
             ier=idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattrc(indana,indanavarattr,indnetwork,indanaattr))
           end if

         end if
       else         
         indanavarattr  = firsttrue(bufferana(i)%btable == vol7dtmp%anavarattr%c%btable)
         indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%c)
         ier=idba_enq (this%handle,starbtable,&
          vol7dtmp%volanaattrc(indana,indanavarattr,indnetwork,indanaattr)) !char is default
       end if

     end do
   end if

 end do

!------------------------- anagrafica fine

deallocate (buffer)
deallocate (bufferana)

! Smart merge
CALL vol7d_merge(this%vol7d, vol7dtmp, sort=.TRUE.)
! should we sort separately in case no merge is done?
!CALL vol7d_smart_sort(this%vol7d, ltime=.TRUE., ltimerange=.TRUE., llevel=.TRUE,)

call vol7d_set_attr_ind(this%vol7d)

call vol7d_dballe_set_var_du(this%vol7d)

!print *,"R-R",this%vol7d%dativar%r(:)%r 
!print *,"R-I",this%vol7d%dativar%r(:)%i 
!print *,"R-B",this%vol7d%dativar%r(:)%b 
!print *,"R-D",this%vol7d%dativar%r(:)%d 
!print *,"R-C",this%vol7d%dativar%r(:)%c 

!print *,"I-R",this%vol7d%dativar%i(:)%r 
!print *,"I-I",this%vol7d%dativar%i(:)%i 
!print *,"I-B",this%vol7d%dativar%i(:)%b 
!print *,"I-D",this%vol7d%dativar%i(:)%d 
!print *,"I-C",this%vol7d%dativar%i(:)%c 

deallocate(lvar,lanavar)

#endif

END SUBROUTINE vol7d_dballe_import


!>\brief Cancella l'oggetto

SUBROUTINE vol7d_dballe_delete(this, preserveidbhandle)
TYPE(vol7d_dballe) :: this !< oggetto da cancellare
logical,intent(in), optional :: preserveidbhandle !< do not close connection to dsn
integer :: ier

call this%handle%delete()
if (.not. optio_log(preserveidbhandle)) call this%idbhandle%delete()

!!$if (associated(this%data_id)) then
!!$  deallocate (this%data_id)
!!$  nullify(this%data_id)
!!$end if

CALL delete(this%vol7d)

!chiudo il logger
call l4f_category_delete(this%category)
!ier=l4f_fini()

END SUBROUTINE vol7d_dballe_delete


end MODULE vol7d_dballenew_class

!>\example esempio_v7ddballe.f90
!!/brief Programma esempio semplice per l'uso di vol7d con DB-All.e
!!

!>\example esempio_v7ddballe_multi.f90
!!/brief Programma esempio per l'uso di vol7d con DB-All.e
!!
!!Vengono estratte più reti

!>\example esempio_v7ddballe_import_export.f90
!!\brief Esempio di utilizzo della classe vol7d_dballe_class
!!
!! Vengono estratti i dati e riscritti in un nuovo DSN


