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
use list_character
use list_linkchar

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
    !print *,"import in mem:"

    this%idbhandle=dbaconnection(dsn="mem:",user="",password="")
    this%handle=dbasession(this%idbhandle,"write","write","write")
    call this%handle%messages_open_input(lfilename, mode, lformat, simplified=.true.)
    read_next = this%handle%messages_read_next()
    do while (read_next) 
      read_next = this%handle%messages_read_next()
    end do

    !print *,"end import in mem:"
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
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,dataonly,ana)
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
logical,intent(in),optional :: dataonly

INTEGER :: i

if (size(network) == 0 )then
  CALL import(this,var, coordmin=coordmin, coordmax=coordmax, timei=timei, timef=timef, level=level,&
   timerange=timerange,set_network=set_network, attr=attr,anavar=anavar,anaattr=anaattr,&
   varkind=varkind,attrkind=attrkind,anavarkind=anavarkind,anaattrkind=anaattrkind,anaonly=anaonly,dataonly=dataonly,ana=ana)
else
  DO i = 1, SIZE(network)
    CALL import(this, var, network(i), coordmin, coordmax, timei, timef, level,timerange,set_network,&
     attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,dataonly,ana)
  ENDDO
end if

END SUBROUTINE vol7d_dballe_importvvnv

!!import da DB-all.e
SUBROUTINE vol7d_dballe_import(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,dataonly,ana)

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
logical,intent(in),optional :: dataonly

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
integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork,indattrvar

integer :: nana,ntime,ntimerange,nlevel,nnetwork
TYPE(vol7d_var) :: var_tmp

INTEGER :: i, j, k, ii, iii,n,n_ana,nn,nvarattr,istat,indattr
integer :: nvar ,inddatiattr,inddativarattr
integer :: nanavar ,indanavar,indanaattr,indanavarattr,nanavarattr

INTEGER(kind=int_l) :: ilat,ilon
CHARACTER(len=vol7d_ana_lenident) :: ident
!CHARACTER(len=10),allocatable :: lvar(:), lanavar(:)
type(dbadcv)       :: vars,starvars,anavars,anastarvars
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
type(characterlist) :: dativarl,dativarattrl,anavarl,anavarattrl
character(len=listcharmaxlen),allocatable :: dativara(:),dativarattra(:),anavara(:),anavarattra(:)
integer :: ndativar,ndativarattr


type(record),ALLOCATABLE :: buffer(:),bufferana(:)
character(len=40) :: query
type(dbametaanddata),allocatable :: metaanddatav(:)
logical :: status


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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ------------- prepare filter options

!!
!!           translate import option for dballe2003 api
!!

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
        else 
          allocate (vars%dcv(i)%dat,source=dbadatac(var(i))) !char is default
        end if
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
        else 
          allocate (anavars%dcv(i)%dat,source=dbadatac(anavar(i))) !char is default
        end if
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
        else 
          allocate (starvars%dcv(i)%dat,source=dbadatac(attr(i))) !char is default
        end if
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
        else 
          allocate (anastarvars%dcv(i)%dat,source=dbadatac(anaattr(i))) !char is default
        end if
      end if
    end do
  end if
end if


if ( .not. optio_log(anaonly)) then

  ! like a cast
  mydbacoordmin=dbacoord()
  if (present(coordmin)) mydbacoordmin%geo_coord=coordmin
  mydbacoordmax=dbacoord()
  if (present(coordmax)) mydbacoordmax%geo_coord=coordmax
  mydbaana=dbaana()
  if (present(ana)) mydbaana%vol7d_ana=ana
  mydatetimemin=dbadatetime()
  if (present(timei)) mydatetimemin%datetime=timei
  mydatetimemax=dbadatetime()
  if (present(timef)) mydatetimemax%datetime=timef
  mydbatimerange=dbatimerange()
  if (present(timerange)) mydbatimerange%vol7d_timerange=timerange
  mydbalevel=dbalevel()
  if (present(level)) mydbalevel%vol7d_level=level
  mydbanetwork=dbanetwork()
  if (present(network)) mydbanetwork%vol7d_network=network

!!
!!         here we have options ready for filter
!!

!
! ---------------->   working on data
!

                                ! set filter

  filter=dbafilter(coordmin=mydbacoordmin,coordmax=mydbacoordmax,ana=mydbaana, &
   datetimemin=mydatetimemin,datetimemax=mydatetimemax, &
   timerange=mydbatimerange,level=mydbalevel,network=mydbanetwork,query=query,&
   vars=vars,starvars=starvars)

!!$  print *, "filter:"
!!$  call filter%display()

  call this%handle%set(filter=filter)


!     uncommenti this if you want compatibility API with old import

!!$  if (allocated(starvars%dcv)) then
!!$    ldataonly=.false.
!!$  else
!!$    ldataonly=.true.
!!$  end if


                                ! estrude the data

  call this%handle%extrude(metaanddatav,filter=filter,dataonly=dataonly)
!!$  do i =1, size(metaanddatav)
!!$    call metaanddatav(i)%display()
!!$  end do

                                ! take in account time_definition
  if (this%vol7d%time_definition == 0) then
    do i =i,size(metaanddatav)
      metaanddatav(i)%metadata%datetime%datetime = &
       metaanddatav(i)%metadata%datetime%datetime - &
       timedelta_new(msec=metaanddatav(i)%metadata%timerange%vol7d_timerange%p1*1000)
    end do
  end if


!!--------------------------------------------------------------------------
!! if not presetted find vars, starvars, anavars, anastarvars
!!

                                ! data
  if (.not. allocated(vars%dcv)) then
    do i =1, size(metaanddatav)
      do j=1, size(metaanddatav(i)%dataattrv%dataattr)
        if (c_e(metaanddatav(i)%metadata%datetime%datetime)) then
          !print *,"dativarl: ", metaanddatav(i)%dataattrv%dataattr(j)%dat%btable
          call dativarl%append(metaanddatav(i)%dataattrv%dataattr(j)%dat%btable)
        else
          !print *,"anavarl: ", metaanddatav(i)%dataattrv%dataattr(j)%dat%btable
          call anavarl%append(metaanddatav(i)%dataattrv%dataattr(j)%dat%btable)
        end if
      end do
    end do
    
    ndativar     = count_distinct (toarray_charl(dativarl)    , back=.TRUE.)
    allocate(dativara(ndativar))
    call pack_distinct_c (toarray_charl(dativarl)    , dativara    , back=.TRUE.)
    status = dativarl%delete()
    allocate (vars%dcv(ndativar))
    do i=1,ndativar
      allocate(vars%dcv(i)%dat,source=dbadatac(dativara(i)))
    end do

    nanavar      = count_distinct (toarray_charl(anavarl)     , back=.TRUE.)
    allocate(anavara(nanavar))
    call pack_distinct_c (toarray_charl(anavarl)     , anavara     , back=.TRUE.)
    status = anavarl%delete()
    allocate (anavars%dcv(nanavar))
    do i=1,nanavar
      allocate(anavars%dcv(i)%dat,source=dbadatac(anavara(i)))
    end do

  end if


                                ! attributes
  if (.not. allocated(starvars%dcv)) then
    do i =1, size(metaanddatav)
      do j=1, size(metaanddatav(i)%dataattrv%dataattr)
        do k=1, size(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
          if (c_e(metaanddatav(i)%metadata%datetime%datetime)) then
            !print *,"dativarattrl: ", metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable
            call dativarattrl%append(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable)
          else
            !print *,"anavarattrl: ", metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable
            call anavarattrl%append(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable)
          end if
        end do
      end do
    end do
    

    ndativarattr = count_distinct (toarray_charl(dativarattrl), back=.TRUE.)
    allocate(dativarattra(ndativarattr))
    call pack_distinct_c (toarray_charl(dativarattrl), dativarattra, back=.TRUE.)
    status = dativarattrl%delete()
    allocate(starvars%dcv(ndativarattr))
    do i=1,ndativarattr
      allocate(starvars%dcv(i)%dat,source=dbadatac(dativarattra(i)))
    end do
    
    nanavarattr  = count_distinct (toarray_charl(anavarattrl) , back=.TRUE.)
    allocate(anavarattra(nanavarattr))
    call pack_distinct_c (toarray_charl(anavarattrl) , anavarattra , back=.TRUE.)
    status = anavarattrl%delete()
    allocate(anastarvars%dcv(nanavarattr))
    do i=1,nanavarattr
      allocate(anastarvars%dcv(i)%dat,source=dbadatac(anavarattra(i)))
    end do

  end if

!!--------------------------------------------------------------------------


!!
!!             count all
!!

  if(ldegnet) then
    nnetwork=1
  else
    nnetwork = count_distinct(metaanddatav(:)%metadata%network%vol7d_network, back=.TRUE.)
  end if
  
  ntime      = count_distinct(metaanddatav(:)%metadata%datetime%datetime, back=.TRUE.)
  ntimerange = count_distinct(metaanddatav(:)%metadata%timerange%vol7d_timerange, back=.TRUE.)
  nlevel     = count_distinct(metaanddatav(:)%metadata%level%vol7d_level, back=.TRUE.)
  nana       = count_distinct(metaanddatav(:)%metadata%ana%vol7d_ana, back=.TRUE.)
  
! var

  ndativarr = 0
  ndativari = 0
  ndativarb = 0
  ndativard = 0
  ndativarc = 0
  
  do i =1 ,size(vars%dcv)
    associate (dato => vars%dcv(i)%dat)
      select type (dato)
      type is (dbadatar)
        ndativarr = ndativarr + 1
      type is (dbadatai)
        ndativari = ndativari + 1
      type is (dbadatab)
        ndativarb = ndativarb + 1
      type is (dbadatad)
        ndativard = ndativard + 1
      type is (dbadatac)
        ndativarc = ndativarc + 1
      end select
    end associate
  end do
  
  
!attr
  
  ndatiattrr = 0
  ndatiattri = 0
  ndatiattrb = 0
  ndatiattrd = 0
  ndatiattrc = 0
  
  do i =1 ,size(starvars%dcv)
    associate (dato => starvars%dcv(i)%dat)
      select type (dato)
      type is (dbadatar)
        ndatiattrr = ndatiattrr + 1
      type is (dbadatai)
        ndatiattri = ndatiattri + 1
      type is (dbadatab)
        ndatiattrb = ndatiattrb + 1
      type is (dbadatad)
        ndatiattrd = ndatiattrd + 1
      type is (dbadatac)
        ndatiattrc = ndatiattrc + 1
      end select
    end associate
  end do
  
  
! ana var

  nanavarr = 0
  nanavari = 0
  nanavarb = 0
  nanavard = 0
  nanavarc = 0
  
  do i =1 ,size(anavars%dcv)
    associate (dato => anavars%dcv(i)%dat)
      select type (dato)
      type is (dbadatar)
        nanavarr = nanavarr + 1
      type is (dbadatai)
        nanavari = nanavari + 1
      type is (dbadatab)
        nanavarb = nanavarb + 1
      type is (dbadatad)
        nanavard = nanavard + 1
      type is (dbadatac)
        nanavarc = nanavarc + 1
      end select
    end associate
  end do
  

! ana attr

  nanaattrr = 0
  nanaattri = 0
  nanaattrb = 0
  nanaattrd = 0
  nanaattrc = 0
  
  do i =1 ,size(anastarvars%dcv)
    associate (dato => anastarvars%dcv(i)%dat)
      select type (dato)
      type is (dbadatar)
        nanaattrr = nanaattrr + 1
      type is (dbadatai)
        nanaattri = nanaattri + 1
      type is (dbadatab)
        nanaattrb = nanaattrb + 1
      type is (dbadatad)
        nanaattrd = nanaattrd + 1
      type is (dbadatac)
        nanaattrc = nanaattrc + 1
      end select
    end associate
  end do
  

!refine

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
  
  
  CALL init(vol7dtmp,time_definition=this%vol7d%time_definition)
  
!!$  print *, "nana=",nana, "ntime=",ntime, "ntimerange=",ntimerange, &
!!$   "nlevel=",nlevel, "nnetwork=",nnetwork, &
!!$   "ndativarr=",ndativarr, "ndativari=",ndativari, &
!!$   "ndativarb=",ndativarb, "ndativard=",ndativard, "ndativarc=",ndativarc,&
!!$   "ndatiattrr=",ndatiattrr, "ndatiattri=",ndatiattri, "ndatiattrb=",ndatiattrb,&
!!$   "ndatiattrd=",ndatiattrd, "ndatiattrc=",ndatiattrc,&
!!$   "ndativarattrr=",ndativarattrr, "ndativarattri=",ndativarattri, "ndativarattrb=",ndativarattrb,&
!!$   "ndativarattrd=",ndativarattrd, "ndativarattrc=",ndativarattrc

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
  
  vol7dtmp%ana=pack_distinct(metaanddatav(:)%metadata%ana%vol7d_ana, nana, back=.TRUE.)
  
  vol7dtmp%time=pack_distinct(metaanddatav(:)%metadata%datetime%datetime, ntime, back=.TRUE.)
  call sort(vol7dtmp%time)

  vol7dtmp%timerange=pack_distinct(metaanddatav(:)%metadata%timerange%vol7d_timerange, ntimerange, back=.TRUE.)
  call sort(vol7dtmp%timerange)
  
  vol7dtmp%level=pack_distinct(metaanddatav(:)%metadata%level%vol7d_level, nlevel, back=.TRUE.)
  call sort(vol7dtmp%level)
  
  if(ldegnet)then
    vol7dtmp%network(1)=set_network
  else
    vol7dtmp%network=pack_distinct(metaanddatav(:)%metadata%network%vol7d_network, nnetwork, back=.TRUE.)
  end if


! var

  ndativarr = 0
  ndativari = 0
  ndativarb = 0
  ndativard = 0
  ndativarc = 0
  
  do i =1 ,size(vars%dcv)
    associate (dato => vars%dcv(i)%dat)
      select type (dato)
      type is (dbadatar)
        ndativarr = ndativarr + 1
        call init (vol7dtmp%dativar%r(ndativarr), btable=dato%btable)
      type is (dbadatai)
        ndativari = ndativari + 1
        call init (vol7dtmp%dativar%i(ndativari), btable=dato%btable)
      type is (dbadatab)
        ndativarb = ndativarb + 1
        call init (vol7dtmp%dativar%b(ndativarb), btable=dato%btable)
      type is (dbadatad)
        ndativard = ndativard + 1
        call init (vol7dtmp%dativar%d(ndativard), btable=dato%btable)
      type is (dbadatac)
        ndativarc = ndativarc + 1
        call init (vol7dtmp%dativar%c(ndativarc), btable=dato%btable)
      end select
    end associate
  end do
  

!attr

  ndatiattrr = 0
  ndatiattri = 0
  ndatiattrb = 0
  ndatiattrd = 0
  ndatiattrc = 0
  
  do i =1 ,size(starvars%dcv)
    associate (dato => starvars%dcv(i)%dat)
      select type (dato)
      type is (dbadatar)
        ndatiattrr = ndatiattrr + 1
        call init (vol7dtmp%datiattr%r(ndatiattrr), btable=dato%btable)
      type is (dbadatai)
        ndatiattri = ndatiattri + 1
        call init (vol7dtmp%datiattr%i(ndatiattri), btable=dato%btable)
      type is (dbadatab)
        ndatiattrb = ndatiattrb + 1
        call init (vol7dtmp%datiattr%b(ndatiattrb), btable=dato%btable)
      type is (dbadatad)
        ndatiattrd = ndatiattrd + 1
        call init (vol7dtmp%datiattr%d(ndatiattrd), btable=dato%btable)
      type is (dbadatac)
        ndatiattrc = ndatiattrc + 1
        call init (vol7dtmp%datiattr%c(ndatiattrc), btable=dato%btable)
      end select
    end associate
  end do
  
    
! ana var
  
  nanavarr = 0
  nanavari = 0
  nanavarb = 0
  nanavard = 0
  nanavarc = 0
  
  do i =1 ,size(anavars%dcv)
    associate (dato => anavars%dcv(i)%dat)
      select type (dato)
      type is (dbadatar)
        nanavarr = nanavarr + 1
        call init (vol7dtmp%anavar%r(nanavarr), btable=dato%btable)
      type is (dbadatai)
        nanavari = nanavari + 1
        call init (vol7dtmp%anavar%i(nanavari), btable=dato%btable)
      type is (dbadatab)
        nanavarb = nanavarb + 1
        call init (vol7dtmp%anavar%b(nanavarb), btable=dato%btable)
      type is (dbadatad)
        nanavard = nanavard + 1
        call init (vol7dtmp%anavar%d(nanavard), btable=dato%btable)
      type is (dbadatac)
        nanavarc = nanavarc + 1
        call init (vol7dtmp%anavar%c(nanavarc), btable=dato%btable)
      end select
    end associate
  end do
  
  
! ana attr
  
  nanaattrr = 0
  nanaattri = 0
  nanaattrb = 0
  nanaattrd = 0
  nanaattrc = 0
  
  do i =1 ,size(anastarvars%dcv)
    associate (dato => anastarvars%dcv(i)%dat)
      select type (dato)
      type is (dbadatar)
        nanaattrr = nanaattrr + 1
        call init (vol7dtmp%anaattr%r(nanaattrr), btable=dato%btable)
      type is (dbadatai)
        nanaattri = nanaattri + 1
        call init (vol7dtmp%anaattr%i(nanaattri), btable=dato%btable)
      type is (dbadatab)
        nanaattrb = nanaattrb + 1
        call init (vol7dtmp%anaattr%b(nanaattrb), btable=dato%btable)
      type is (dbadatad)
        nanaattrd = nanaattrd + 1
        call init (vol7dtmp%anaattr%d(nanaattrd), btable=dato%btable)
      type is (dbadatac)
        nanaattrc = nanaattrc + 1
        call init (vol7dtmp%anaattr%c(nanaattrc), btable=dato%btable)
      end select
    end associate
  end do


! here we colcolate the link from attributes and vars
  do i =1, size(vars%dcv)
    associate (dato => vars%dcv(i)%dat)
      if ( ndativarattri > 0 ) call init(vol7dtmp%dativarattr%i(i),btable=dato%btable)
      if ( ndativarattrr > 0 ) call init(vol7dtmp%dativarattr%r(i),btable=dato%btable)
      if ( ndativarattrd > 0 ) call init(vol7dtmp%dativarattr%d(i),btable=dato%btable)
      if ( ndativarattrb > 0 ) call init(vol7dtmp%dativarattr%b(i),btable=dato%btable)
      if ( ndativarattrc > 0 ) call init(vol7dtmp%dativarattr%c(i),btable=dato%btable)
    end associate
  end do

  do i =1, size(anavars%dcv)
    associate (dato => anavars%dcv(i)%dat)
      if ( nanavarattri > 0 ) call init(vol7dtmp%anavarattr%i(i),btable=dato%btable)
      if ( nanavarattrr > 0 ) call init(vol7dtmp%anavarattr%r(i),btable=dato%btable)
      if ( nanavarattrd > 0 ) call init(vol7dtmp%anavarattr%d(i),btable=dato%btable)
      if ( nanavarattrb > 0 ) call init(vol7dtmp%anavarattr%b(i),btable=dato%btable)
      if ( nanavarattrc > 0 ) call init(vol7dtmp%anavarattr%c(i),btable=dato%btable)
    end associate
  end do

! set index in dativaratt*
  call vol7d_set_attr_ind(this%vol7d)


  call vol7d_alloc_vol (vol7dtmp)

!!$if (lattr) then
!!$
!!$  allocate  (this%data_id( nana, ntime, nlevel, ntimerange, nnetwork),stat=istat)
!!$  if (istat/= 0) THEN
!!$    CALL l4f_category_log(this%category,L4F_ERROR,'cannot allocate ' &
!!$     //TRIM(to_char(nana*ntime*nlevel*ntimerange*nnetwork))//' data_id elements')
!!$    CALL raise_fatal_error()
!!$    
!!$  ENDIF
!!$
!!$  this%data_id=DBA_MVI
!!$
!!$else

  nullify(this%data_id)

!!$end if


! Ora qui bisogna metterci dentro idati


  do i =1, size(metaanddatav)

    indana       = firsttrue(metaanddatav(i)%metadata%ana%vol7d_ana             == vol7dtmp%ana)
    indtime      = firsttrue(metaanddatav(i)%metadata%datetime%datetime         == vol7dtmp%time)
    indtimerange = firsttrue(metaanddatav(i)%metadata%timerange%vol7d_timerange == vol7dtmp%timerange)
    indlevel     = firsttrue(metaanddatav(i)%metadata%level%vol7d_level         == vol7dtmp%level)
    if (ldegnet)then
      indnetwork=1
    else
      indnetwork = firsttrue(metaanddatav(i)%metadata%network%vol7d_network     == vol7dtmp%network)
    endif


    if (c_e(metaanddatav(i)%metadata%datetime%datetime)) then      ! dati
      
      do j=1, size(metaanddatav(i)%dataattrv%dataattr)
        
        associate (dato => metaanddatav(i)%dataattrv%dataattr(j)%dat)
          select type (dato)
          type is (dbadatai)
            inddativar = firsttrue(dato%btable == vol7dtmp%dativar%i%btable)
            vol7dtmp%voldatii( &
             indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
             ) = dato%value
            
          type is (dbadatar)
            inddativar = firsttrue(dato%btable == vol7dtmp%dativar%r%btable)
            vol7dtmp%voldatir( &
             indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
             ) = dato%value
            
          type is (dbadatad)
            inddativar = firsttrue(dato%btable == vol7dtmp%dativar%d%btable)
            vol7dtmp%voldatid( &
             indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
             ) = dato%value
            
          type is (dbadatab)
            inddativar = firsttrue(dato%btable == vol7dtmp%dativar%b%btable)
            vol7dtmp%voldatib( &
             indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
             ) = dato%value
            
          type is (dbadatac)
            inddativar = firsttrue(dato%btable == vol7dtmp%dativar%c%btable)
            vol7dtmp%voldatic( &
             indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
             ) = dato%value
            
          end select
          

                                ! attributes
          do k=1, size(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
            associate (attr => metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat)
              select type (attr)
                
              type is (dbadatai)
                inddativarattr  = firsttrue(dato%btable == vol7dtmp%dativarattr%i%btable)
                indattrvar = firsttrue(attr%btable == vol7dtmp%datiattr%i%btable)
                vol7dtmp%voldatiattri( &
                 indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
                 ) = attr%value
              type is (dbadatar)
                inddativarattr  = firsttrue(dato%btable == vol7dtmp%dativarattr%r%btable)
                indattrvar = firsttrue(attr%btable == vol7dtmp%datiattr%r%btable)
                vol7dtmp%voldatiattrr( &
                 indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
                 ) = attr%value
              type is (dbadatad)
                inddativarattr  = firsttrue(dato%btable == vol7dtmp%dativarattr%d%btable)
                indattrvar = firsttrue(attr%btable == vol7dtmp%datiattr%d%btable)
                vol7dtmp%voldatiattrd( &
                 indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
                 ) = attr%value
              type is (dbadatab)
                inddativarattr  = firsttrue(dato%btable == vol7dtmp%dativarattr%b%btable)
                indattrvar = firsttrue(attr%btable == vol7dtmp%datiattr%b%btable)
                vol7dtmp%voldatiattrb( &
                 indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
                 ) = attr%value
              type is (dbadatac)
                inddativarattr  = firsttrue(dato%btable == vol7dtmp%dativarattr%c%btable)
                indattrvar = firsttrue(attr%btable == vol7dtmp%datiattr%c%btable)
                vol7dtmp%voldatiattrc( &
                 indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
                 ) = attr%value
                
              end select
            end associate
          end do
        end associate
      end do


!!$    else
!!$
!!$      do j=1, size(metaanddatav(i)%dataattrv%dataattr)
!!$        indanavar = firsttrue(metaanddatav(i)%dataattrv%dataattr(j)%dat%btable == vol7dtmp%anavar%c%btable)
!!$        print *,"ana",indanavar
!!$                                ! attributes
!!$        do k=1, size(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
!!$          indanaattr = firsttrue(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable == vol7dtmp%anaattr%c%btable)
!!$          print *,"ana",indanavar,indanaattr
!!$
!!$
!!$
!!$        end do
!!$      end do

    end if

  end do

end if

! ---------------->   constant station data

! filter=dbafilter(filter=filter,contextana=.true.)

filter=dbafilter(coordmin=mydbacoordmin,coordmax=mydbacoordmax,ana=mydbaana, &
 datetimemin=mydatetimemin,datetimemax=mydatetimemax, &
 timerange=mydbatimerange,level=mydbalevel,network=mydbanetwork,query=query,&
 vars=anavars,starvars=anastarvars,contextana=.true.)


deallocate (metaanddatav)
call this%handle%extrude(metaanddatav,filter=filter,dataonly=dataonly)
!!$do i =1, size(metaanddatav)
!!$  call metaanddatav(i)%display()
!!$end do

!  ier=idba_enqdate (this%handle_staz,year,month,day,hour,minute,sec)
!  IF (.NOT.c_e(sec)) sec = 0
!  ier=idba_enq (this%handle_staz,"context_id",bufferana(i)%data_id)


! ---------------->   constant station data end

#ifdef NONE

                                !memorizzo data_id
#ifdef DEBUG
     !CALL l4f_category_log(this%category,L4F_DEBUG,"data_id: "//trim(to_char(buffer(i)%data_id)))
#endif

     this%data_id(indana,indtime,indlevel,indtimerange,indnetwork)=buffer(i)%data_id


     ier=idba_set (this%handle,"*context_id",buffer(i)%data_id)
     ier=idba_set (this%handle,"*var_related",buffer(i)%btable)
     !per ogni dato ora lavoro sugli attributi
     ier=idba_set(this%handle, "*varlist",starvarlist )
     ier=idba_voglioancora (this%handle,nn)
     !print*,buffer(i)%btable," numero attributi",nn
     
#endif

! Smart merge
CALL vol7d_merge(this%vol7d, vol7dtmp, sort=.TRUE.)
! should we sort separately in case no merge is done?
!CALL vol7d_smart_sort(this%vol7d, ltime=.TRUE., ltimerange=.TRUE., llevel=.TRUE,)

!call vol7d_dballe_set_var_du(this%vol7d)


contains

!> return an array of btable
function toarray_charl(this)
character(len=listcharmaxlen),allocatable :: toarray_charl(:) !< array
type(characterlist) :: this

integer :: i

allocate (toarray_charl(this%countelements()))

call this%rewind()
i=0
do while(this%element())
  i=i+1
  toarray_charl(i) =this%current()
  call this%next()
end do
end function toarray_charl


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



























