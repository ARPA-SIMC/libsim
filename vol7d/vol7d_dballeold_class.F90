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

MODULE vol7d_dballeold_class

USE char_utilities
USE vol7d_class
USE array_utilities
use log4fortran
USE geo_coord_class

IMPLICIT NONE

include "dballeff.h"

!external v7d_dballe_error_handler

character (len=255),parameter:: subcategory="vol7d_dballe_class"

!>\brief Oggetto per import ed export da DB-All.e
!!
!!L'oggetto è costituito da un oggetto vol7d attorniato dalle 
!!informazioni necessarie per l'accesso al DSN di DB-All.e
!! e da una matrice necessaria per l'ottimizzazione della scrittura dei 
!!degli attributi dei dati in export

TYPE vol7d_dballe

  TYPE(vol7d) :: vol7d !< volume vol7d
  integer :: idbhandle !< handle delle sessioni connesse al DSN DB-All.e
  integer :: handle,handle_staz !< handle delle sessioni connesse al DSN DB-All.e
  integer :: handle_err !< handle delle sessioni connesse al DSN DB-All.e
  !> memorizza gli id interni al database DB-All.e per 
  !!ottimizzare le riscritture degli attributi ai dati
  integer ,pointer :: data_id(:,:,:,:,:)
  logical :: file=.false. !< true when operation refer to a file
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
  MODULE PROCEDURE vol7d_dballe_importvsns, vol7d_dballe_importvvns, &
   vol7d_dballe_importvsnv, vol7d_dballe_importvvnv
END INTERFACE

!>\brief exporta
INTERFACE export
  MODULE PROCEDURE vol7d_dballe_export
END INTERFACE


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

type(vol7d_var),allocatable,private :: blocal(:) ! cache of dballe.txt

PRIVATE
PUBLIC vol7d_dballe, init, delete, import, export, vol7d_dballe_set_var_du, &
 vol7d_dballe_import_dballevar

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

character(len=1):: mode ! the open mode ("r" for read, "w" for write or create, "a" append) (comandato da "write", default="r" )

character(len=50) :: quidsn,quiuser,quipassword
character(len=255) :: quirepinfo
logical :: quiwrite,quiwipe,quifile

character(len=512) :: a_name
character(len=254) :: arg,lfilename,lformat
logical :: exist
integer :: ier
#ifndef DBALLELT67
logical :: read_next
#endif

this%idbhandle=imiss
this%handle=imiss
this%handle_err=imiss
this%handle_staz=imiss

if (present(categoryappend))then
  call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
  call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
this%category=l4f_category_get(a_name)

nullify(this%data_id)

!TODO: quando scrivo bisogna gestire questo che non è da fare ?
CALL init(this%vol7d,time_definition=time_definition)

                                ! impostiamo la gestione dell'errore
ier=idba_error_set_callback(0,v7d_dballe_error_handler, &
 this%category,this%handle_err)

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

quifile=.false.
if (present(file))then
  quifile=file
endif


if (quifile) then

  call getarg(0,arg)

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

  inquire(file=lfilename,EXIST=exist)

  mode="r"
  if (quiwrite)then
    if (quiwipe.or..not.exist) then
      mode="w"
    else
      mode="a"
      call l4f_category_log(this%category,L4F_INFO,"file exists; appending data to file: "//trim(lfilename))
    end if
  else
    if (.not.exist) then
      call l4f_category_log(this%category,L4F_ERROR,"file does not exist; cannot open file for read: "//trim(lfilename))
      CALL raise_fatal_error()
    end if
  end if

#ifndef DBALLELT67

  if(quiwrite)then
    ier=idba_messaggi(this%handle,lfilename,mode,lformat)
    this%file=.true.
  else

    ier=idba_presentati(this%idbhandle,dsn="mem:",user="",password="")
    ier=idba_preparati (this%idbhandle,this%handle,"write","write","write")
    ier=idba_preparati (this%idbhandle,this%handle_staz,"write","write","write")
    ier = idba_messages_open_input(this%handle, lfilename, mode, lformat, simplified=.true.)
    ier=idba_messages_read_next(this%handle, read_next)
    do while (read_next) 
      ier=idba_messages_read_next(this%handle, read_next)
    end do
    this%file=.false.
  end if
#else

  ier=idba_messaggi(this%handle,lfilename,mode,lformat)
  this%file=.true.

#endif


#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,"handle from idba_messaggi: "//t2c(this%handle))
  call l4f_category_log(this%category,L4F_DEBUG,"filename: "//trim(lfilename))
  call l4f_category_log(this%category,L4F_DEBUG,"mode: "//trim(mode))
  call l4f_category_log(this%category,L4F_DEBUG,"format: "//trim(lformat))
#endif

else


  if (.not. c_e(optio_i(idbhandle))) then

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
    
                                !print*,"write=",quiwrite,"wipe=",quiwipe,"dsn=",quidsn
    ier=idba_presentati(this%idbhandle,quidsn,quiuser,quipassword)
  else
    this%idbhandle=optio_i(idbhandle)
  end if

  if(quiwrite)then
    ier=idba_preparati (this%idbhandle,this%handle,"write","write","write")
    ier=idba_preparati (this%idbhandle,this%handle_staz,"write","write","write")
  else
    ier=idba_preparati (this%idbhandle,this%handle,"read","read","read")
    ier=idba_preparati (this%idbhandle,this%handle_staz,"read","read","read")
  end if
  
  if (quiwipe)ier=idba_scopa (this%handle,quirepinfo)
  
  this%file=.false.
  
endif

END SUBROUTINE vol7d_dballe_init


!>\brief Importa un volume dati da un DSN DB-all.e
!!
!! import da DB-all.e:
!! var e network sono scalari.

SUBROUTINE vol7d_dballe_importvsns(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)
TYPE(vol7d_dballe),INTENT(inout) :: this  !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in) :: var  !< variabile da importare secondo la tabella B locale o relativi alias
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'importazione
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax
!> station coordinate for selected extraction
TYPE(vol7d_ana),INTENT(inout),optional :: ana
!>estremi temporali (inizio e fine) dell'estrazione per l'importazione
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_network),INTENT(in),optional :: network !< network da importare
!>                               estrae i dati migliori disponibili "mergiandoli" in un'unica rete 
!!                               definita da questo parametro:
!!                               ANCORA DA TESTARE !!!!
TYPE(vol7d_network),INTENT(in),OPTIONAL ::set_network
TYPE(vol7d_level),INTENT(in),optional :: level !< livello selezionato per l'estrazione
TYPE(vol7d_timerange),INTENT(in),optional :: timerange !< timerange selezionato per l'importazione
!> variabili da importare secondo la tabella B locale o relativi alias relative ad attributi
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:)
!> variabili da importare secondo la tabella B locale o relativi alias relative ad anagrafica
CHARACTER(len=*),INTENT(in),OPTIONAL :: anavar(:)
!> variabili da importare secondo la tabella B locale o relativi alias relative a attributi dell'anagrafica
CHARACTER(len=*),INTENT(in),OPTIONAL :: anaattr(:)
!> tipi per le variabili da importare relative a dati:
!! - "r" = real
!! - "i" = integer
!! - "b" = byte
!! - "d" = double precision
!! - "c" = character 
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:)
!> tipi per le variabili da importare relative a attributi:
!! - "r" = real
!! - "i" = integer
!! - "b" = byte
!! - "d" = double precision
!! - "c" = character 
CHARACTER(len=*),INTENT(in),OPTIONAL :: attrkind(:)
!> tipi per le variabili da importare relative a anagrafica:
!! - "r" = real
!! - "i" = integer
!! - "b" = byte
!! - "d" = double precision
!! - "c" = character 
CHARACTER(len=*),INTENT(in),OPTIONAL :: anavarkind(:)
!> tipi per le variabili da importare relative a  attributi dell'anagrafica: 
!! - "r" = real
!! - "i" = integer
!! - "b" = byte
!! - "d" = double precision
!! - "c" = character 
CHARACTER(len=*),INTENT(in),OPTIONAL :: anaattrkind(:)
logical,intent(in),optional :: anaonly !< extract all ana data but only that

CALL import(this, (/var/), network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)

END SUBROUTINE vol7d_dballe_importvsns

!>\brief Identica a vol7d_dballe_importvsns con network vettore.
!!
!!import da DB-all.e

SUBROUTINE vol7d_dballe_importvsnv(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)
TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in) :: var
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

if (size(network) == 0) then

  CALL import(this, (/var/), coordmin=coordmin, coordmax=coordmax, timei=timei, timef=timef, level=level,&
   timerange=timerange,set_network=set_network, attr=attr,anavar=anavar,anaattr=anaattr,&
   varkind=varkind,attrkind=attrkind,anavarkind=anavarkind,anaattrkind=anaattrkind,anaonly=anaonly,ana=ana)

else

  DO i = 1, SIZE(network)
    CALL import(this, (/var/), network(i), coordmin, coordmax, timei, timef, level,timerange,set_network,&
     attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)
  ENDDO
end if



END SUBROUTINE vol7d_dballe_importvsnv

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

!>\brief Identica a vol7d_dballe_importvsns con var vettore.
!!
!!import da DB-all.e oppure da BUFR/CREX formato generico

SUBROUTINE vol7d_dballe_importvvns(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in),optional :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax
TYPE(vol7d_ana),INTENT(inout),optional :: ana
TYPE(datetime),INTENT(in),OPTIONAL :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network,set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)
logical,intent(in),optional :: anaonly

if (this%file) then

  call vol7d_dballe_importvvns_file(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
   attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)

else
!!$  if (optio_log(anaonly)) then
!!$    CALL l4f_category_log(this%category,L4F_ERROR,"anaonly=.true. not supported accessing to dba")
!!$    CALL raise_fatal_error()
!!$  end if

  call vol7d_dballe_importvvns_dba(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
   attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,ana)
  
end if

end SUBROUTINE vol7d_dballe_importvvns



!>\brief Identica a vol7d_dballe_importvsns con var vettore.
!!
!!import da DB-all.e
SUBROUTINE vol7d_dballe_importvvns_dba(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
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
CHARACTER(len=10),allocatable :: lvar(:), lanavar(:)
!INTEGER(kind=int_b)::attrdatib

integer :: ndativarr,     ndativari,     ndativarb,     ndativard,     ndativarc
integer :: ndatiattrr,    ndatiattri,    ndatiattrb,    ndatiattrd,    ndatiattrc 
integer :: ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc

integer :: nanavarr,     nanavari,     nanavarb,     nanavard,     nanavarc
integer :: nanaattrr,    nanaattri,    nanaattrb,    nanaattrd,    nanaattrc 
integer :: nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc

integer :: ir,ib,id,ic,ier


!TYPE(datetime) :: odatetime
! nobs, ntime, nana, nvout, nvin, nvbt, &
! datai(3), orai(2), dataf(3), oraf(2),ist
!CHARACTER(len=12),ALLOCATABLE :: tmtmp(:)
!INTEGER,ALLOCATABLE :: anatmp(:), vartmp(:), mapdatao(:)
!LOGICAL :: found, non_valid, varbt_req(SIZE(vartable))

TYPE(vol7d) :: vol7dtmp

type(record),ALLOCATABLE :: buffer(:),bufferana(:)

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

IF (PRESENT(attr)) THEN
  if (any(c_e(attr)).and. size(attr) > 0)then
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,'lattr true')
#endif
    lattr = .TRUE.
  else
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,'lattr false')
#endif
   lattr = .FALSE.
 end if
ELSE
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,'lattr false')
#endif
   lattr = .FALSE.
ENDIF

IF (PRESENT(anaattr)) THEN
  if (size(anaattr) > 0) then
    lanaattr = .TRUE.
  else
   lanaattr = .FALSE.
 end if
ELSE
  lanaattr = .FALSE.
ENDIF

IF (PRESENT(var)) THEN
   allocate(lvar(size(var)))
   lvar=var
ELSE
   allocate(lvar(0))
ENDIF

IF (PRESENT(anavar)) THEN
   allocate(lanavar(size(anavar)))
   lanavar=anavar
ELSE
   allocate(lanavar(0))
ENDIF

if (present(network))  then
  lnetwork=network
else
  call init(lnetwork)
end if

if (present(level))  then
  llevel=level
else
  call init(llevel)
end if

if (present(timerange))  then
  ltimerange=timerange
else
  call init(ltimerange)
end if


ier=idba_unsetall(this%handle)

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'unsetall handle')
#endif

if(c_e(lnetwork))ier=idba_set (this%handle,"rep_memo",lnetwork%name)

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'query rep_memo:'//t2c(lnetwork%name,miss="missing"))
#endif

if(ldegnet)ier=idba_set (this%handle,"query","best")
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'query best:'//t2c(ldegnet))
#endif

if (present(coordmin)) then
!  CALL geo_coord_to_geo(coordmin)
  CALL getval(coordmin, ilat=ilat,ilon=ilon)

#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,'query coordmin:'//t2c(ilon,miss="missing")//"/"//t2c(ilat,miss="missing"))
#endif
  ier=idba_set(this%handle,"lonmin",ilon)
  ier=idba_set(this%handle,"latmin",ilat)
end if

if (present(coordmax)) then
!  CALL geo_coord_to_geo(coordmax)
  CALL getval(coordmax, ilat=ilat,ilon=ilon)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,'query coordmax:'//t2c(ilon,miss="missing")//"/"//t2c(ilat,miss="missing"))
#endif
  ier=idba_set(this%handle,"lonmax",ilon)
  ier=idba_set(this%handle,"latmax",ilat)
end if

if (present(ana)) then
  CALL getval(ana%coord, ilat=ilat,ilon=ilon)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,'query coord:'//t2c(ilon,miss="missing")//"/"//t2c(ilat,miss="missing"))
  CALL l4f_category_log(this%category,L4F_DEBUG,'query ident:'//t2c(ana%ident,miss="missing"))
#endif
  ier=idba_set(this%handle,"lon",ilon)
  ier=idba_set(this%handle,"lat",ilat)
  if (c_e(ana%ident)) then
    ier=idba_set(this%handle,"ident",ana%ident)
    ! mobile ignored
    ier=idba_set(this%handle,"mobile",1)
  else
    ier=idba_set(this%handle,"mobile",0)
  end if
end if

if (present(timei)) then
  if (c_e(timei)) then
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,'query timei:'//to_char(timei))
#endif
    CALL getval(timei, year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)
    sec=nint(float(msec)/1000.)
    ier=idba_setdatemin(this%handle,year,month,day,hour,minute,sec)
                                !print *,"datemin",year,month,day,hour,minute,sec
  end if
end if

if (present(timef)) then
  if (c_e(timef)) then
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,'query timef:'//to_char(timef))
#endif
    CALL getval(timef, year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)
    sec=nint(float(msec)/1000.)
    ier=idba_setdatemax(this%handle,year,month,day,hour,minute,sec)
                                !print *,"datemax",year,month,day,hour,minute,sec
  end if
end if


nvar=0

!if (any(c_e(lvar)) .and. .not. optio_log(anaonly)) then
if (any(c_e(lvar)) .and. .not. optio_log(anaonly)) then
  !usefull for anaonly starting from dballe 6.6

  IF (SIZE(lvar) > maxvarlist) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"too many variables requested: "//t2c(SIZE(lvar)))
    call raise_fatal_error()
  ENDIF

                                ! creo la stringa con l'elenco
  varlist = ''
  DO i = 1, SIZE(lvar)
    nvar = nvar + 1
    IF (nvar > 1) varlist(LEN_TRIM(varlist)+1:) = ',' 
    varlist(LEN_TRIM(varlist)+1:) = TRIM(lvar(i))
  ENDDO
                                !print *,"varlist",varlist

#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,'query varlist:'//t2c(SIZE(lvar))//":"//varlist)
#endif  
  if (varlist /= '' ) ier=idba_set(this%handle, "varlist",varlist )

end if

if (c_e(ltimerange))then
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,'query timerange:'//to_char(timerange))
#endif
  ier=idba_settimerange(this%handle, timerange%timerange, timerange%p1, timerange%p2)
end if

if (c_e(llevel))then
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,'query level:'//to_char(level))
#endif
  ier=idba_setlevel(this%handle, level%level1, level%l1,level%level2, level%l2)
end if

ier=idba_voglioquesto (this%handle,N)
!print*,"numero di dati ",N
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'numero di dati:'//t2c(n))
#endif

if (optio_log(anaonly)) N=0

!ora che so quanti dati ho alloco la memoria per buffer
allocate(buffer(N),stat=istat)
IF (istat/= 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,'cannot allocate ' &
   //TRIM(to_char(n))//' buffer elements')
  CALL raise_fatal_error()
ENDIF


! dammi tutti i dati
do i=1,N

  ier=idba_dammelo (this%handle,btable)
  
  ier=idba_enqdate (this%handle,year,month,day,hour,minute,sec)
  IF (.NOT.c_e(sec)) sec = 0
  ier=idba_enqlevel(this%handle, rlevel1, rl1, rlevel2,rl2)
  ier=idba_enqtimerange(this%handle, rtimerange, p1, p2)
  ier=idba_enq(this%handle, "rep_memo",rep_memo)
                                !print *,"trovato network",rep_memo
  
                                !nbtable=btable_numerico(btable)
                                ! ind = firsttrue(qccli%v7d%dativar%r(:)%btable == nbtable)
                                ! IF (ind<1) cycle ! non c'e'
  
  buffer(i)%dator=DBA_MVR
  buffer(i)%datoi=DBA_MVI
  buffer(i)%datob=DBA_MVB
  buffer(i)%datod=DBA_MVD
  buffer(i)%datoc=DBA_MVC
  
  if (any(c_e(lvar)).and. present(varkind))then
    ii= index_c(lvar, btable)
    if (ii > 0)then
                                !print*, "indici",ii, btable,(varkind(ii))
      if(varkind(ii) == "r") ier=idba_enq (this%handle,btable,buffer(i)%dator)
      if(varkind(ii) == "i") ier=idba_enq (this%handle,btable,buffer(i)%datoi)
      if(varkind(ii) == "b") ier=idba_enq (this%handle,btable,buffer(i)%datob)
      if(varkind(ii) == "d") ier=idba_enq (this%handle,btable,buffer(i)%datod)
      if(varkind(ii) == "c") ier=idba_enq (this%handle,btable,buffer(i)%datoc)
    end if
  else
    ier=idba_enq (this%handle,btable,buffer(i)%datoc) !char is default
  end if
  
                                !metto in memoria l'identificatore numerico dei dati
                                !print*,buffer(i)%data_id
  ier=idba_enq (this%handle,"context_id",buffer(i)%data_id)

                                !recupero i dati di anagrafica
  ier=idba_enq (this%handle,"lat",   ilat)
  ier=idba_enq (this%handle,"lon",   ilon)
  ier=idba_enq (this%handle,"ident",ident)

!!$  print*,"ident",ident
!!$  do ier=1,len(ident)
!!$    print *,iachar(ident(ier:ier))
!!$  end do

                                !bufferizzo il contesto
                                !print *,"lat,lon,ident",lat,lon,ident
                                !print*,year,month,day,hour,minute,sec
                                !print*,btable,dato,buffer(i)%datiattrb
  
  call init(buffer(i)%ana,ilat=ilat,ilon=ilon,ident=ident)
  call init(buffer(i)%time, year=year, month=month, day=day, hour=hour, minute=minute,msec=sec*1000)
  call init(buffer(i)%level, rlevel1,rl1,rlevel2,rl2)
  call init(buffer(i)%timerange, rtimerange, p1, p2)
  call init(buffer(i)%network, rep_memo)
  buffer(i)%btable = btable

                                ! take in account time_definition
  if (this%vol7d%time_definition == 0) buffer(i)%time = buffer(i)%time - &
   timedelta_new(sec=buffer(i)%timerange%p1)

end do

! ---------------->   anagrafica

!ora legge tutti i dati di anagrafica e li mette in bufferana

ier=idba_unsetall(this%handle_staz)
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'unsetall handle_staz')
#endif

if(c_e(lnetwork))ier=idba_set (this%handle_staz,"rep_memo",lnetwork%name)
if(ldegnet)ier=idba_set (this%handle_staz,"query","best")

if (present(coordmin)) then
!  CALL geo_coord_to_geo(coordmin)
  CALL getval(coordmin, ilat=ilat,ilon=ilon)
  ier=idba_set(this%handle_staz,"lonmin",ilon)
  ier=idba_set(this%handle_staz,"latmin",ilat)
end if

if (present(coordmax)) then
!  CALL geo_coord_to_geo(coordmax)
  CALL getval(coordmax, ilat=ilat,ilon=ilon)
  ier=idba_set(this%handle_staz,"lonmax",ilon)
  ier=idba_set(this%handle_staz,"latmax",ilat)
end if

if (present(ana)) then
  CALL getval(ana%coord, ilat=ilat,ilon=ilon)
  ier=idba_set(this%handle_staz,"lon",ilon)
  ier=idba_set(this%handle_staz,"lat",ilat)
  if (c_e(ana%ident)) then
    ier=idba_set(this%handle_staz,"ident",ana%ident)
! mobile ignored
    ier=idba_set(this%handle_staz,"mobile",1)
  else
    ier=idba_set(this%handle_staz,"mobile",0)
  end if
end if

nanavar=0

if (size (lanavar) > 0 ) then
                                ! creo la stringa con l'elenco
  varlist = ''
  DO i = 1, SIZE(lanavar)
    nanavar = nanavar + 1
    IF (nanavar > 1) varlist(LEN_TRIM(varlist)+1:) = ',' 
    varlist(LEN_TRIM(varlist)+1:) = TRIM(lanavar(i))
  ENDDO
!!$  print *,"varlist :",trim(varlist)
!!$  ier=idba_set(this%handle_staz, "varlist",trim(varlist))

end if


ier=idba_setcontextana(this%handle_staz)
ier=idba_voglioquesto (this%handle_staz,N_ana)
!!$print*,"numero di dati ",N_ana

!ora che so quanti dati ho alloco la memoria per bufferana
allocate(bufferana(N_ana),stat=istat)
if (istat/= 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,'cannot allocate ' &
   //TRIM(to_char(n_ana))//' bufferana elements')
  CALL raise_fatal_error()
ENDIF


! dammi tutti i dati di anagrafica
do i=1,N_ana
  call init(bufferana(i)%ana)
  call init(bufferana(i)%network)

  bufferana(i)%dator=DBA_MVR
  bufferana(i)%datoi=DBA_MVI
  bufferana(i)%datob=DBA_MVB
  bufferana(i)%datod=DBA_MVD
  bufferana(i)%datoc=DBA_MVC
  bufferana(i)%btable = DBA_MVC

  ier=idba_dammelo (this%handle_staz,btable)

  
  ier=idba_enqdate (this%handle_staz,year,month,day,hour,minute,sec)
  IF (.NOT.c_e(sec)) sec = 0
  ier=idba_enqlevel(this%handle_staz, rlevel1, rl1, rlevel2,rl2)
  ier=idba_enqtimerange(this%handle_staz, rtimerange, p1, p2)
  ier=idba_enq(this%handle_staz, "rep_memo",rep_memo)
                                !print *,"trovato network",rep_memo
                                !nbtable=btable_numerico(btable)
                                ! ind = firsttrue(qccli%v7d%dativar%r(:)%btable == nbtable)
                                ! IF (ind<1) cycle ! non c'e'
  

                                !metto in memoria l'identificatore numerico dei dati
                                !print*,bufferana(i)%data_id
  ier=idba_enq (this%handle_staz,"context_id",bufferana(i)%data_id)

                                !recupero i dati di anagrafica
  ier=idba_enq (this%handle_staz,"lat",   ilat)
  ier=idba_enq (this%handle_staz,"lon",   ilon)
  ier=idba_enq (this%handle_staz,"ident",ident)


                                !bufferizzo il contesto
  
  call init(bufferana(i)%ana,ilat=ilat,ilon=ilon,ident=ident)
  call init(bufferana(i)%network, rep_memo)

                                !salto lat lon e ident e network
  if (btable == "B05001" .or. btable == "B06001" .or. btable == "B01011" .or. btable == "B01194" ) cycle

  if ( size(lanavar) > 0 .and. present(anavarkind))then
    ii= index_c(lanavar, btable)
    if (ii > 0)then
                                !print*, "indici",ii, btable,(varkind(ii))
      if(anavarkind(ii) == "r") ier=idba_enq (this%handle_staz,btable,bufferana(i)%dator)
      if(anavarkind(ii) == "i") ier=idba_enq (this%handle_staz,btable,bufferana(i)%datoi)
      if(anavarkind(ii) == "b") ier=idba_enq (this%handle_staz,btable,bufferana(i)%datob)
      if(anavarkind(ii) == "d") ier=idba_enq (this%handle_staz,btable,bufferana(i)%datod)
      if(anavarkind(ii) == "c") ier=idba_enq (this%handle_staz,btable,bufferana(i)%datoc)
    end if
  else
    ier=idba_enq (this%handle_staz,btable,bufferana(i)%datoc) !char is default
    !print*,"dato anagrafica",btable," ",bufferana(i)%dator
  end if
  
  call init(bufferana(i)%time, year=year, month=month, day=day, hour=hour, minute=minute,msec=sec*1000)
  call init(bufferana(i)%level, rlevel1,rl1,rlevel2,rl2)
  call init(bufferana(i)%timerange, rtimerange, p1, p2)
  bufferana(i)%btable = btable

end do

! ---------------->   anagrafica fine

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

!vol7dtmp%voldatir=DBA_MVR
!vol7dtmp%voldatii=DBA_MVI
!vol7dtmp%voldatib=DBA_MVB
!vol7dtmp%voldatid=DBA_MVD
!vol7dtmp%voldatic=DBA_MVC
!vol7dtmp%voldatiattrr=DBA_MVR
!vol7dtmp%voldatiattri=DBA_MVI
!vol7dtmp%voldatiattrb=DBA_MVB
!vol7dtmp%voldatiattrd=DBA_MVD
!vol7dtmp%voldatiattrc=DBA_MVC

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


END SUBROUTINE vol7d_dballe_importvvns_dba


!> \brief Exporta un volume dati a un DSN DB-all.e
!!
!! Riscrive i dati nel DSN di DB-All.e con la possibilità di attivare
!! una serie di filtri.
!! Try to make the better work:
!! if write on file and template is generic write ana data and attribute in separate bufr befor data
!! if write on file and template is not generic write ana and data in the same bufr
!! if write on db write ana and use ana_id to insert data
SUBROUTINE vol7d_dballe_export(this, network, coordmin, coordmax,&
 timei, timef,level,timerange,var,attr,anavar,anaattr,attr_only,template,ana)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto contenente il volume e altre info per l'accesso al DSN
character(len=network_name_len),INTENT(in),optional :: network !< network da exportare
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'esportazione
TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
TYPE(vol7d_ana),INTENT(inout),optional :: ana  !< identificativo della stazione da exportare
!>estremi temporali dei dati da esportare
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_level),INTENT(in),optional :: level !< livello selezionato per l'esportazione
TYPE(vol7d_timerange),INTENT(in),optional :: timerange !< timerange selezionato per l'esportazione
!> variabili da exportare secondo la tabella B locale o alias relative a dati, attributi,
!! anagrafica e attributi dell'anagrafica
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:),attr(:),anavar(:),anaattr(:)
!> permette di riscrivere su un DSN letto precedentemente, modificando solo gli attributi ai dati,
!! ottimizzando enormente le prestazioni: gli attributi riscritti saranno quelli con this%data_id definito
!! (solitamente ricopiato dall'oggetto letto)
logical,intent(in),optional :: attr_only 
!> specificando category.subcategory.localcategory oppure un alias ("synop", "metar","temp","generic") forza l'exportazione ad uno specifico template BUFR/CREX"
!!  the special value "generic-frag is used to generate bufr on file where ana data is reported only once at beginning and data in other bufr after
character(len=*),intent(in),optional :: template 

!!$ Conversazioni con spanezz@jabber.linux.it su gio 27 mag 2010 09:38:52 CEST:
!!$ (09:39:00) pat1@jabber.linux.it/Home: 
!!$ ho una domanda
!!$ scrivo dei bufr generici
!!$ quando setto query a "message" viene chiuso un bufr
!!$ posso scrive migliaia di cose senza mai mettere query a message
!!$ il bufr viene piu' piccolo
!!$ quindi nel generico mi conviene scrivere dopo un sensato uso di memoria ?
!!$ (09:41:54) spanezz@jabber.linux.it: 
!!$ dipende da cosa vuoi mettere nel messaggio
!!$ (09:42:21) spanezz@jabber.linux.it: 
!!$ puoi salvare tutto in un unico genericone se vuoi
!!$ poi se lo archivi quando queri queri sempre tutto
!!$ (09:42:40) pat1@jabber.linux.it/Home: 
!!$ nel caso sto scrivendo un volume v7d di dati 
!!$ posso farne solo un bufr oppure migliaia
!!$ (09:48:14) spanezz@jabber.linux.it: 
!!$ se non scrivi generici lui mette nel messaggio solo quello che ci sta nel template, ovviamente
!!$ quindi ci sono solo un certo numero di dati che puoi settare e finiscono nell'output
!!$ (09:49:38) pat1@jabber.linux.it/Home: 
!!$ quindi ad esempio se scrivo generici e cambio stazione me ne POSSO fregare e mettere un solo "query=message" 
!!$ alla fine di tutto
!!$ ma se scrivo synop e faccio lo stesso scrivo solo l'ultima stazione ?
!!$ (09:50:04) spanezz@jabber.linux.it: 
!!$ ni
!!$ (09:51:41) spanezz@jabber.linux.it: 
!!$ la roba in cui scrivi temporaneamente i dati non è una versione in memoria di dballe (del DB di dballe intendo)
!!$ in particolare, una un unico livello di anagrafica in cui ci sta una stazione e un'orario solo
!!$ in particolare, ha un unico livello di anagrafica in cui ci sta una stazione e un'orario solo
!!$ quindi se metti due stazioni, sovrascrivi la seconda
!!$ è indicizzato per (livello, scadenza, codice variabile)
!!$ se fai due prendilo con gli stessi (livello, scadenza, codice variabile), la seconda sovrascrive la prima
!!$ e data ora stazione report vanno nel (livello, scadenza) di "anagrafica" (257,0,  0,0,  0,0,0)
!!$ (09:56:43) pat1@jabber.linux.it/Home: 
!!$ quindi per scrivere N^N roba
!!$ devo ciclare su (livello, scadenza, codice variabile) e ogni volta fare una prendilo
!!$ poi all'esterno devo ciclare su tutto il resto e fare una prendilo con query="message"

!REAL(kind=fp_geo) :: latmin,latmax,lonmin,lonmax
logical, allocatable :: lnetwork(:),llevel(:),ltimerange(:)
integer,allocatable :: ana_id(:,:)
logical :: write,writeattr,lattr_only, generic_frag
character(len=80) :: ltemplate 

!CHARACTER(len=6) :: btable
!CHARACTER(len=7) ::starbtable

integer :: year,month,day,hour,minute,sec,msec
integer :: nstaz,ntime,ntimerange,nlevel,nnetwork


INTEGER :: i,ii,iii,iiii,iiiii,iiiiii,a,ind,inddatiattr,indanaattr,ier

INTEGER(kind=int_l) :: ilat,ilon 
!INTEGER(kind=int_b)::attrdatib


integer :: ndativarr,ndatiattrr
integer :: ndativari,ndatiattri
integer :: ndativarb,ndatiattrb
integer :: ndativard,ndatiattrd
integer :: ndativarc,ndatiattrc

integer :: nanavarr,nanaattrr
integer :: nanavari,nanaattri
integer :: nanavarb,nanaattrb
integer :: nanavard,nanaattrd
integer :: nanavarc,nanaattrc

logical, allocatable :: lvarr(:),lattrr(:)
logical, allocatable :: lvari(:),lattri(:)
logical, allocatable :: lvarb(:),lattrb(:)
logical, allocatable :: lvard(:),lattrd(:)
logical, allocatable :: lvarc(:),lattrc(:)

logical, allocatable :: lanavarr(:),lanaattrr(:)
logical, allocatable :: lanavari(:),lanaattri(:)
logical, allocatable :: lanavarb(:),lanaattrb(:)
logical, allocatable :: lanavard(:),lanaattrd(:)
logical, allocatable :: lanavarc(:),lanaattrc(:)


ndativarr=0
ndatiattrr=0
ndativari=0
ndatiattri=0
ndativarb=0
ndatiattrb=0
ndativard=0
ndatiattrd=0
ndativarc=0
ndatiattrc=0

nanavarr=0
nanaattrr=0
nanavari=0
nanaattri=0
nanavarb=0
nanaattrb=0
nanavard=0
nanaattrd=0
nanavarc=0
nanaattrc=0

call vol7d_alloc_vol(this%vol7d) ! be safe
nstaz=size(this%vol7d%ana(:))

ntimerange=size(this%vol7d%timerange(:))
allocate (ltimerange(ntimerange))
ltimerange=.false.

if (present(timerange))then
  where (timerange == this%vol7d%timerange(:))
    ltimerange(:)=.true.
  end where
else
  ltimerange=.true.
end if

nlevel=size(this%vol7d%level(:))
allocate (llevel(nlevel))
llevel=.false.

if (present(level))then
  where (level == this%vol7d%level(:))
    llevel(:)=.true.
  end where
else
  llevel=.true.
end if

if (present(attr_only))then
  lattr_only=attr_only
else
  lattr_only=.false.
end if

if ( .not. associated(this%data_id))then
  lattr_only=.false.
end if


nnetwork=size(this%vol7d%network(:))
ntime=size(this%vol7d%time(:))

allocate (lnetwork(nnetwork))
lnetwork=.false.
allocate (ana_id(nstaz,nnetwork))
ana_id=DBA_MVI


if (present(network))then
      where (network == this%vol7d%network(:)%name)
         lnetwork(:)=.true.
      end where
else
   lnetwork=.true.
end if

ltemplate=optio_c(template,len(ltemplate))
if (template == "generic-frag") then
  ltemplate="generic"
  generic_frag=.true.
else
  ltemplate=template
  generic_frag=.false.
end if



!!!!!  anagrafica

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo r")
#endif
#include "vol7d_dballe_class_nana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo i")
#endif
#include "vol7d_dballe_class_nana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo b")
#endif
#include "vol7d_dballe_class_nana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo d")
#endif
#include "vol7d_dballe_class_nana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo c")
#endif
#include "vol7d_dballe_class_nana.F90"
#undef VOL7D_POLY_TYPES_V


!!!!!!!   dati

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo r")
#endif
#include "vol7d_dballe_class_ndati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo i")
#endif
#include "vol7d_dballe_class_ndati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo b")
#endif
#include "vol7d_dballe_class_ndati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo d")
#endif
#include "vol7d_dballe_class_ndati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo c")
#endif
#include "vol7d_dballe_class_ndati.F90"
#undef VOL7D_POLY_TYPES_V


! vital statistics data

!print *,"nstaz,ntime,nlevel,ntimerange,nnetwork",nstaz,ntime,nlevel,ntimerange,nnetwork

do iiiiii=1, nnetwork
  if (.not.lnetwork(iiiiii))cycle

! l'anagrafica su file la scrivo solo per i generici_frag or for ana_only datasets
  if (this%file .and. .not. generic_frag .and. ntime > 0 ) cycle

  do i=1, nstaz

    if (present(coordmin).and.present(coordmax))then
      if (.not. inside(this%vol7d%ana(i)%coord,coordmin,coordmax)) cycle
    end if

    CALL getval(this%vol7d%ana(i)%coord, ilat=ilat,ilon=ilon)
    ier=idba_unsetall (this%handle)
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,'unsetall handle')
#endif
    ier=idba_setcontextana (this%handle)

    ier=idba_set (this%handle,"lat",ilat)
    ier=idba_set (this%handle,"lon",ilon)

    if (present(ana))then
      if (c_e(ana%ident) .and. ana%ident /= this%vol7d%ana(i)%ident ) cycle
      if (c_e(ana%coord) .and. ana%coord /= this%vol7d%ana(i)%coord ) cycle
    end if

!      this%vol7d%ana(i)%ident=cmiss

!!$      print*,"ident",this%vol7d%ana(i)%ident
!!$      do ier=1,len(this%vol7d%ana(i)%ident)
!!$        print *,iachar(this%vol7d%ana(i)%ident(ier:ier))
!!$      end do

    if ( c_e(this%vol7d%ana(i)%ident)) then
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,"I have found a mobile station! ident: "//&
       this%vol7d%ana(i)%ident)
#endif
      ier=idba_set (this%handle,"ident",this%vol7d%ana(i)%ident)
      ier=idba_set (this%handle,"mobile",1)
    else
      ier=idba_set (this%handle,"mobile",0)
    end if

    ier=idba_set(this%handle,"rep_memo",this%vol7d%network(iiiiii)%name)

    write=.false.

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
!print*,"ana macro tipo r"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
!print*,"ana macro tipo i"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
!print*,"ana macro tipo b"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
!print*,"ana macro tipo d"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
!print*,"ana macro tipo c"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V


    if (this%file)then
      if (write .or. generic_frag) then

        if (c_e(ltemplate)) then
          ier=idba_set (this%handle,"query","message "//trim(ltemplate))
        else
          ier=idba_set (this%handle,"query","message")
        end if

#ifdef DEBUG
        call l4f_category_log(this%category,L4F_DEBUG,"eseguo una main prendilo di anagrafica")
#endif
        ier=idba_prendilo (this%handle)
      end if

    else

                                !se NON ho dati di anagrafica (ma solo lat e long ..) devo fare comunque una prendilo
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,"eseguo una main prendilo di anagrafica")
#endif
      ier=idba_prendilo (this%handle)
      ier=idba_enq (this%handle,"*ana_id",ana_id(i,iiiiii))

    end if

    do ii=1,nanavarr
      if (c_e(this%vol7d%anavar%r(ii)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%r(ii)%btable )
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%r(ii)%btable)
#endif
    end do
    do ii=1,nanavari
      if (c_e(this%vol7d%anavar%i(ii)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%i(ii)%btable )
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%i(ii)%btable)
#endif
    end do
    do ii=1,nanavarb
      if (c_e(this%vol7d%anavar%b(ii)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%b(ii)%btable )
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%b(ii)%btable)
#endif
    end do
    do ii=1,nanavard
      if (c_e(this%vol7d%anavar%d(ii)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%d(ii)%btable )
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%d(ii)%btable)
#endif
    end do
    do ii=1,nanavarc
      if (c_e(this%vol7d%anavar%c(ii)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%c(ii)%btable )
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%c(ii)%btable)
#endif
    end do

  end do
end do


! data
!print *,"nstaz,ntime,nlevel,ntimerange,nnetwork",nstaz,ntime,nlevel,ntimerange,nnetwork


do iiiiii=1, nnetwork
  if (.not.lnetwork(iiiiii))cycle

  do i=1, nstaz

    if ( (.not. this%file) .and. (.not. c_e(ana_id(i,iiiiii))) ) cycle
    if (present(coordmin).and.present(coordmax))then
      if (.not. inside(this%vol7d%ana(i)%coord,coordmin,coordmax)) cycle
    end if

    do ii=1,ntime
      if (present(timei) )then
        if ( this%vol7d%time(ii) < timei ) cycle
      endif
      if (present(timef) )then
        if ( this%vol7d%time(ii) > timef ) cycle
      endif
                                !>\todo optimize setting and unsetting in the right place

      ier=idba_unsetall (this%handle)
#ifdef DEBUG
      CALL l4f_category_log(this%category,L4F_DEBUG,'unsetall handle')
#endif        

      ier=idba_set (this%handle,"rep_memo",this%vol7d%network(iiiiii)%name)
#ifdef DEBUG
      CALL l4f_category_log(this%category,L4F_DEBUG,'set rep_memo:'//this%vol7d%network(iiiiii)%name)
#endif        
      
      if (this%file)then
                                ! writing on file cannot use ana_id
        call getval(this%vol7d%ana(i)%coord, ilat=ilat,ilon=ilon)
        ier=idba_set (this%handle,"lat",ilat)
        ier=idba_set (this%handle,"lon",ilon)
#ifdef DEBUG
        call l4f_category_log(this%category,L4F_DEBUG,"dati riferiti a lat: "//to_char(ilat)//" lon: "//to_char(ilon))
#endif        

        if (present(ana))then
          if (c_e(ana%ident) .and. ana%ident /= this%vol7d%ana(i)%ident ) cycle
          if (c_e(ana%coord) .and. ana%coord /= this%vol7d%ana(i)%coord ) cycle
        end if
          
        if ( c_e(this%vol7d%ana(i)%ident)) then
          ier=idba_set (this%handle,"ident",this%vol7d%ana(i)%ident)
          ier=idba_set (this%handle,"mobile",1)
#ifdef DEBUG
          call l4f_category_log(this%category,L4F_DEBUG,"there is a mobile station! identity: "&
           //this%vol7d%ana(i)%ident)
#endif
        else
          ier=idba_set (this%handle,"mobile",0)
        end if
        

! l'anagrafica su file la scrivo solo per i non generici_frag
        if (.not. generic_frag) then

#ifdef DEBUG
          call l4f_category_log(this%category,L4F_DEBUG,"setcontextana")
#endif
          ier=idba_setcontextana (this%handle)

          write=.false.
            
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
!print*,"ana macro tipo r"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
!print*,"ana macro tipo i"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
!print*,"ana macro tipo b"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
!print*,"ana macro tipo d"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
!print*,"ana macro tipo c"
#include "vol7d_dballe_class_ana.F90"
#undef VOL7D_POLY_TYPES_V


          if (write) then
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"eseguo una main prendilo di anagrafica")
#endif
            ier=idba_prendilo (this%handle)
          end if

        end if
      else
#ifdef DEBUG
        call l4f_category_log(this%category,L4F_DEBUG,"specify ana_id: "&
           //to_char(ana_id(i,iiiiii)))
#endif
        ier=idba_set (this%handle,"ana_id",ana_id(i,iiiiii))
      end if

      CALL getval(this%vol7d%time(ii), year=year, month=month, day=day, hour=hour, minute=minute,msec=msec)
      sec=nint(float(msec)/1000.)
#ifdef DEBUG
        call l4f_category_log(this%category,L4F_DEBUG,"setdate: "&
         //t2c(year)//t2c(month)//t2c(day)//t2c(hour)//t2c(minute)//t2c(sec))
#endif
      ier=idba_setdate (this%handle,year,month,day,hour,minute,sec)

      do iii=1,nlevel
        if (.not.llevel(iii))cycle
        
        do iiii=1,ntimerange
          if (.not.ltimerange(iiii))cycle
   
          if (.not. lattr_only) then
                                  

            ier=idba_setlevel(this%handle, this%vol7d%level(iii)%level1, this%vol7d%level(iii)%l1,&
             this%vol7d%level(iii)%level2, this%vol7d%level(iii)%l2)
            
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"level1: "//to_char(this%vol7d%level(iii)%level1))
            call l4f_category_log(this%category,L4F_DEBUG,"l1: "//to_char(this%vol7d%level(iii)%l1))
            call l4f_category_log(this%category,L4F_DEBUG,"level2: "//to_char(this%vol7d%level(iii)%level2))
            call l4f_category_log(this%category,L4F_DEBUG,"l2: "//to_char(this%vol7d%level(iii)%l2))
#endif              
                 
            ier=idba_settimerange(this%handle, this%vol7d%timerange(iiii)%timerange, &
             this%vol7d%timerange(iiii)%p1, this%vol7d%timerange(iiii)%p2)
              
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"timerange: "//to_char(this%vol7d%timerange(iiii)%timerange))
            call l4f_category_log(this%category,L4F_DEBUG,"T1: "//to_char(this%vol7d%timerange(iiii)%p1))
            call l4f_category_log(this%category,L4F_DEBUG,"T2: "//to_char(this%vol7d%timerange(iiii)%p2))
#endif            
                 
          end if
               
                                !print *, ">>>>> ",ana_id(i,iiiiii),this%vol7d%network(iiiiii)%name
                                !print *, year,month,day,hour,minute
                                !print *, this%vol7d%level(iii)%level1, this%vol7d%level(iii)%l1, this%vol7d%level(iii)%l2
                                !print *, this%vol7d%timerange(iiii)%timerange,this%vol7d%timerange(iiii)%p1, this%vol7d%timerange(iiii)%p2
               

          write=.false.

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#ifdef DEBUG
          call l4f_category_log(this%category,L4F_DEBUG,"macro tipo r")
#endif
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#ifdef DEBUG
          call l4f_category_log(this%category,L4F_DEBUG,"macro tipo i")
#endif
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#ifdef DEBUG
          call l4f_category_log(this%category,L4F_DEBUG,"macro tipo b")
#endif
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#ifdef DEBUG
          call l4f_category_log(this%category,L4F_DEBUG,"macro tipo d")
#endif
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#ifdef DEBUG
          call l4f_category_log(this%category,L4F_DEBUG,"macro tipo c")
#endif
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V


          if (write) then

!            if (.not. this%file)then
!
! !!!!!!!!!!! workaround to dballe fortran api bug
! ! TODO remove this duplicated set of ana_id
!#ifdef DEBUG
!              call l4f_category_log(this%category,L4F_DEBUG,"rispecify ana_id: "&
!               //to_char(ana_id(i,iiiiii)))
!#endif
!              ier=idba_set (this%handle,"ana_id",ana_id(i,iiiiii))
!            end if

                                !print*,"eseguo una main prendilo"
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"eseguo una main prendilo sui dati")
#endif
            ier=idba_prendilo (this%handle)
                 
          end if


!ana

          if (this%file .and. .not. generic_frag) then

            do a=1,nanavarr
              if (c_e(this%vol7d%anavar%r(a)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%r(a)%btable )
#ifdef DEBUG
              call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%r(a)%btable)
#endif
            end do
            do a=1,nanavari
              if (c_e(this%vol7d%anavar%i(a)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%i(a)%btable )
#ifdef DEBUG
              call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%i(a)%btable)
#endif
            end do
            do a=1,nanavarb
              if (c_e(this%vol7d%anavar%b(a)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%b(a)%btable )
#ifdef DEBUG
              call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%b(a)%btable)
#endif
            end do
            do a=1,nanavard
              if (c_e(this%vol7d%anavar%d(a)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%d(a)%btable )
#ifdef DEBUG
              call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%d(a)%btable)
#endif
            end do
            do a=1,nanavarc
              if (c_e(this%vol7d%anavar%c(a)%btable))ier=idba_unset (this%handle,this%vol7d%anavar%c(a)%btable )
#ifdef DEBUG
              call l4f_category_log(this%category,L4F_DEBUG,"unset ana: "//this%vol7d%anavar%c(a)%btable)
#endif
            end do
            
          end if

! data
          
          do iiiii=1,ndativarr
            if(c_e(this%vol7d%dativar%r(iiiii)%btable))ier=idba_unset (this%handle,this%vol7d%dativar%r(iiiii)%btable )
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"unset dati: "//this%vol7d%dativar%r(iiiii)%btable)
#endif
          end do
          do iiiii=1,ndativari
            if(c_e(this%vol7d%dativar%i(iiiii)%btable))ier=idba_unset (this%handle,this%vol7d%dativar%i(iiiii)%btable )
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"unset dati: "//this%vol7d%dativar%i(iiiii)%btable)
#endif
          end do
          do iiiii=1,ndativarb
            if(c_e(this%vol7d%dativar%b(iiiii)%btable))ier=idba_unset (this%handle,this%vol7d%dativar%b(iiiii)%btable )
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"unset dati: "//this%vol7d%dativar%b(iiiii)%btable)
#endif
          end do
          do iiiii=1,ndativard
            if(c_e(this%vol7d%dativar%d(iiiii)%btable))ier=idba_unset (this%handle,this%vol7d%dativar%d(iiiii)%btable )
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"unset dati: "//this%vol7d%dativar%d(iiiii)%btable)
#endif
          end do
          do iiiii=1,ndativarc
            if(c_e(this%vol7d%dativar%c(iiiii)%btable))ier=idba_unset (this%handle,this%vol7d%dativar%c(iiiii)%btable )
#ifdef DEBUG
            call l4f_category_log(this%category,L4F_DEBUG,"unset dati: "//this%vol7d%dativar%c(iiiii)%btable)
#endif
          end do
          
          
        end do
      end do

      if (this%file)then
        if (c_e(ltemplate)) then
          ier=idba_set (this%handle,"query","message "//trim(ltemplate))
        else
          ier=idba_set (this%handle,"query","message")
        end if
#ifdef DEBUG
        call l4f_category_log(this%category,L4F_DEBUG,"close message ")

                                !print*,"eseguo una main prendilo"
        call l4f_category_log(this%category,L4F_DEBUG,"eseguo una main prendilo sui dati")
#endif
        ier=idba_prendilo (this%handle)
        
      end if
    end do
  end do
end do

END SUBROUTINE vol7d_dballe_export


!>\brief Cancella l'oggetto

SUBROUTINE vol7d_dballe_delete(this, preserveidbhandle)
TYPE(vol7d_dballe) :: this !< oggetto da cancellare
logical,intent(in), optional :: preserveidbhandle !< do not close connection to dsn
integer :: ier

if (this%file)then

  ier=idba_fatto(this%handle)
  
else

  ier=idba_fatto(this%handle)
  ier=idba_fatto(this%handle_staz)
  if (.not. optio_log(preserveidbhandle)) ier=idba_arrivederci(this%idbhandle)

end if

ier=idba_error_remove_callback(this%handle_err)

!this%dsn=cmiss
!this%user=cmiss
!this%password=cmiss
this%idbhandle=imiss
this%handle=imiss
this%handle_err=imiss
this%handle_staz=imiss

if (associated(this%data_id)) then
  deallocate (this%data_id)
  nullify(this%data_id)
end if
CALL delete(this%vol7d)

!chiudo il logger
call l4f_category_delete(this%category)
!ier=l4f_fini()

END SUBROUTINE vol7d_dballe_delete



subroutine vol7d_dballe_import_dballevar(this)

type(vol7d_var),pointer :: this(:)
INTEGER :: i,un,n

IF (associated(this)) return
IF (allocated(blocal)) then
  ALLOCATE(this(size(blocal)))
  this=blocal
  return
end if

un = open_dballe_file('dballe.txt', filetype_data)
IF (un < 0) then

  call l4f_log(L4F_ERROR,"error open_dballe_file: dballe.txt")
  CALL raise_error("error open_dballe_file: dballe.txt")
  return
end if

n = 0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  n = n + 1
ENDDO
100 CONTINUE

IF (n > 0) THEN
  ALLOCATE(this(n))
  ALLOCATE(blocal(n))
  REWIND(un)
  readline: do i = 1 ,n
    READ(un,'(1x,A6,1x,a65,a24,i4)')blocal(i)%btable,blocal(i)%description,blocal(i)%unit,&
     blocal(i)%scalefactor
    blocal(i)%btable(:1)="B"
    !print*,"B=",blocal(i)%btable
    !print*," D=",blocal(i)%description
    !PRINT*," U=",blocal(i)%unit
    !PRINT*," D=",blocal(i)%scalefactor
  ENDDO readline

  CALL l4f_log(L4F_INFO,'Found '//TRIM(to_char(i-1))//' variables in dballe master table')

  this=blocal

ENDIF
CLOSE(un)

END SUBROUTINE vol7d_dballe_import_dballevar



!> \brief Integra il vettore delle variabili in vol7d con le descrizioni e le unità di misura
!!eventualmente mancanti.

subroutine vol7d_dballe_set_var_du(this)

TYPE(vol7d) :: this !< oggetto vol7d con le variabili da completare
integer :: i,j
type(vol7d_var),pointer :: dballevar(:)


call vol7d_dballe_import_dballevar(dballevar)

#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME dativar


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V

#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME anavar


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V


#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME datiattr


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V


#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME anaattr


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V


deallocate(dballevar)

return

end subroutine vol7d_dballe_set_var_du



FUNCTION get_dballe_filepath(filename, filetype) RESULT(path)
CHARACTER(len=*), INTENT(in) :: filename
INTEGER, INTENT(in) :: filetype

INTEGER ::  j
CHARACTER(len=512) :: path
LOGICAL :: exist

IF (dballe_name == ' ') THEN
  CALL getarg(0, dballe_name)
  ! dballe_name_env
ENDIF

IF (filetype < 1 .OR. filetype > nftype) THEN
  path = ""
  CALL l4f_log(L4F_ERROR, 'dballe file type '//TRIM(to_char(filetype))// &
   ' not valid')
  CALL raise_error()
  RETURN
ENDIF

! try with environment variable
CALL getenv(TRIM(dballe_name_env), path)
IF (path /= ' ') THEN

  path=TRIM(path)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL l4f_log(L4F_INFO, 'dballe file '//TRIM(path)//' found')
    RETURN
  ENDIF
ENDIF
! try with pathlist
DO j = 1, SIZE(pathlist,1)
  IF (pathlist(j,filetype) == ' ') EXIT
  path=TRIM(pathlist(j,filetype))//'/'//TRIM(dballe_name)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL l4f_log(L4F_INFO, 'dballe file '//TRIM(path)//' found')
    RETURN
  ENDIF
ENDDO
CALL l4f_log(L4F_ERROR, 'dballe file '//TRIM(filename)//' not found')
CALL raise_error()
path = ""

END FUNCTION get_dballe_filepath


FUNCTION open_dballe_file(filename, filetype) RESULT(unit)
CHARACTER(len=*), INTENT(in) :: filename
INTEGER, INTENT(in) :: filetype
INTEGER :: unit,i

CHARACTER(len=512) :: path

unit = -1
path=get_dballe_filepath(filename, filetype)
IF (path == '') RETURN

unit = getunit()
IF (unit == -1) RETURN

OPEN(unit, file=path, status='old', iostat = i)
IF (i == 0) THEN
  CALL l4f_log(L4F_INFO, 'dballe file '//TRIM(path)//' opened')
  RETURN
ENDIF

CALL l4f_log(L4F_ERROR, 'dballe file '//TRIM(filename)//' not found')
CALL raise_error()
unit = -1

END FUNCTION open_dballe_file



FUNCTION v7d_dballe_error_handler(category)
INTEGER :: category, code, l4f_level
INTEGER :: v7d_dballe_error_handler

CHARACTER(len=1000) :: message, buf

code = idba_error_code()

! check if "Value outside acceptable domain"
if (code == 13 ) then
  l4f_level=L4F_WARN
else
  l4f_level=L4F_ERROR
end if

call idba_error_message(message)
call l4f_category_log(category,l4f_level,message)

call idba_error_context(buf)

call l4f_category_log(category,l4f_level,trim(buf))

call idba_error_details(buf)
call l4f_category_log(category,L4F_INFO,trim(buf))


! if "Value outside acceptable domain" do not raise error
if (l4f_level == L4F_ERROR ) CALL raise_fatal_error("dballe: "//message)

v7d_dballe_error_handler = 0
return

END FUNCTION v7d_dballe_error_handler



!>\brief It works like vol7d_dballe_importvvns reading from file.
!!
!! It works like vol7d_dballe_importvvns reading from file but with some restrictions.
!! File can user BUFR or CREX format.

#ifndef F2003_EXTENDED_FEATURES
!! Attributes will not be imported at all.
#endif

SUBROUTINE vol7d_dballe_importvvns_file(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
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

!TYPE(vol7d) :: v7d
!CHARACTER(len=SIZE(var)*7) :: varlist
!CHARACTER(len=SIZE(attr)*8) :: starvarlist
CHARACTER(len=6) :: btable

LOGICAL ::  ldegnet, lanaonly
integer :: year,month,day,hour,minute,sec
integer :: rlevel1, rl1,rlevel2, rl2
integer :: rtimerange, p1, p2
character(len=network_name_len) ::rep_memo
integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork


integer :: nana,ntime,ntimerange,nlevel,nnetwork
TYPE(vol7d_network),ALLOCATABLE :: networktmp(:)

INTEGER :: i,ii, n, na, nd
integer :: nvar, nanavar ,indanavar

INTEGER(kind=int_l) :: ilat,ilon,latmin,latmax,lonmin,lonmax,ilata,ilona
CHARACTER(len=vol7d_ana_lenident) :: ident
!INTEGER(kind=int_b)::attrdatib

integer :: ndativarr,     ndativari,     ndativarb,     ndativard,     ndativarc
integer :: ndatiattrr,    ndatiattri,    ndatiattrb,    ndatiattrd,    ndatiattrc 
integer :: ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc

integer :: nanavarr,     nanavari,     nanavarb,     nanavard,     nanavarc
integer :: nanaattrr,    nanaattri,    nanaattrb,    nanaattrd,    nanaattrc 
integer :: nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc

integer :: ir,ib,id,ic,ier

TYPE(datetime) :: timee
TYPE(vol7d_level) :: levele
TYPE(vol7d_timerange) :: timerangee

TYPE(vol7d_network) :: lnetwork
TYPE(vol7d_level) :: llevel
TYPE(vol7d_timerange) :: ltimerange
logical :: lattr

!TYPE(datetime) :: odatetime
! nobs, ntime, nana, nvout, nvin, nvbt, &
! datai(3), orai(2), dataf(3), oraf(2),ist
!CHARACTER(len=12),ALLOCATABLE :: tmtmp(:)
!INTEGER,ALLOCATABLE :: anatmp(:), vartmp(:), mapdatao(:)
!LOGICAL :: found, non_valid, varbt_req(SIZE(vartable))


TYPE(vol7d) :: vol7dtmp

type(record),pointer :: buffer(:),bufferana(:)

!!!  CALL print_info('Estratte dall''archivio '//TRIM(to_char(nobs)) // ' osservazioni')

call optio(anaonly,lanaonly)


IF (PRESENT(set_network)) THEN
  if (c_e(set_network)) then
    ldegnet = .TRUE.
    call l4f_category_log(this%category,L4F_INFO,&
     "set_network is not fully implemented in BUFR/CREX import: priority will be ignored")
  else
    ldegnet = .FALSE.
  end if
ELSE
  ldegnet = .FALSE.
ENDIF

if (present(attr))then
  if (size(attr) > 0 )then
    lattr=.true.
  else
    lattr=.false.
  end if
else
    lattr=.false.
end if

if ( lattr .or. present(anaattr) .or. present(attrkind) .or. present(anaattrkind))then
  call l4f_category_log(this%category,L4F_ERROR,"attributes not managed in BUFR/CREX import: try --disable-qc when is possible")
  CALL raise_error()
end if


if (present(network))  then
  lnetwork=network
else
  call init(lnetwork)
end if

if (present(level))  then
  llevel=level
else
  call init(llevel)
end if

if (present(timerange))  then
  ltimerange=timerange
else
  call init(ltimerange)
end if


ier=idba_unsetall(this%handle)
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'unsetall handle')
#endif        

N=1
nd=0
na=0

call mem_acquire( buffer,nd,1000,this%category )
call mem_acquire( bufferana,na,100,this%category )

ier=idba_setcontextana (this%handle)
do while ( .true. )

  ier=idba_voglioquesto (this%handle,N)
  if (ier /= 0) then
    call l4f_category_log(this%category,L4F_ERROR,"voglioquesto return error status")
    N=1 ! I do not want terminate while loop
    cycle
  end if

  call l4f_category_log(this%category,L4F_debug,"numero dati voglioquesto:"//to_char(n))

  if (.not. c_e(N)) exit

#ifdef DBALLELT67
  if (N == 0) exit                                  ! use only with dballe svn <= 4266
#endif

  ! dammi tutti i dati
  do i=1,N

    ier=idba_dammelo (this%handle,btable)
  
    ier=idba_enqdate (this%handle,year,month,day,hour,minute,sec)
    IF (.NOT.c_e(sec)) sec = 0
    ier=idba_enqlevel(this%handle, rlevel1, rl1, rlevel2,rl2)
    ier=idba_enqtimerange(this%handle, rtimerange, p1, p2)
    ier=idba_enq(this%handle, "rep_memo",rep_memo)
                                !print *,"trovato network",rep_memo
  
                                !nbtable=btable_numerico(btable)
                                ! ind = firsttrue(qccli%v7d%dativar%r(:)%btable == nbtable)
                                ! IF (ind<1) cycle ! non c'e'

                                !recupero i dati di anagrafica
    ier=idba_enq (this%handle,"lat",   ilat)
    ier=idba_enq (this%handle,"lon",   ilon)
    ier=idba_enq (this%handle,"ident",ident)

!!$    print*,"ident",ident
!!$    do ier=1,len(ident)
!!$      print *,iachar(ident(ier:ier))
!!$    end do

    ! inizio la serie dei test con i parametri richiesti 

    if(c_e(lnetwork)) then
      if (rep_memo /= lnetwork%name) cycle
    end if

! in alternativa si trattano insieme
!!$    call init(ana,lat=lat,lon=lon,ident=ident)
!!$
!!$    if (present(coordmin).and.present(coordmax))then
!!$
!!$      if (.not. inside(this%vol7d%ana(i)%coord,coordmin,coordmax)) cycle
!!$                                !print * ,"sei dentro, OK"
!!$    end if


    if (present(coordmin)) then
!      CALL geo_coord_to_geo(coordmin)
      if (c_e(coordmin)) then
        CALL getval(coordmin, ilat=latmin,ilon=lonmin)
        if (lonmin > ilon) cycle
        if (latmin > ilat) cycle
      end if
    end if

    if (present(coordmax)) then
!      CALL geo_coord_to_geo(coordmax)
      if (c_e(coordmax)) then
        CALL getval(coordmax, ilat=latmax,ilon=lonmax)
        if (lonmax < ilon) cycle
        if (latmax < ilat) cycle
      end if
    end if


    if (present(ana)) then
      if (c_e(ana%coord)) then
        CALL getval(ana%coord, ilat=ilata,ilon=ilona)
        if (ilona /= ilon) cycle
        if (ilata /= ilat) cycle
      end if
      if (c_e(ana%ident)) then
        if (ana%ident /= ident) cycle
      end if
    end if

    call init(timee, year=year, month=month, day=day, hour=hour, minute=minute,msec=sec*1000)

    if (present(timei)) then
      if (c_e(timei) .and. timee < timei) cycle
    end if

    if (present(timef)) then
      if (c_e(timef) .and. timee > timef) cycle
    end if

    if (c_e(ltimerange))then
      call init(timerangee, timerange%timerange, timerange%p1, timerange%p2)
      if (timerangee /= ltimerange) cycle
    end if

    if (c_e(llevel))then
      call  init (levele, rlevel1, rl1,rlevel2, rl2)
      if (levele /= llevel) cycle
    end if

    if (rlevel1 /= 257)then
      ! dati

      if (present (var)) then
!        nvar=count(c_e(var))
        if (any(c_e(var)) .and. (all(btable /= var))) cycle
      end if

      ! fine test


      nd =nd+1
#ifdef DEBUG
      call l4f_category_log(this%category,L4F_DEBUG,"numero dati dati:"//to_char(nd)//btable)
#endif
      call mem_acquire( buffer,nd,0,this%category )
  
      buffer(nd)%dator=DBA_MVR
      buffer(nd)%datoi=DBA_MVI
      buffer(nd)%datob=DBA_MVB
      buffer(nd)%datod=DBA_MVD
      buffer(nd)%datoc=DBA_MVC
  
      if (present(var).and. present(varkind))then
        ii=( firsttrue(var == btable))
        if (ii > 0)then
                                !print*, "indici",ii, btable,(varkind(ii))
          if(varkind(ii) == "r") ier=idba_enq (this%handle,btable,buffer(nd)%dator)
          if(varkind(ii) == "i") ier=idba_enq (this%handle,btable,buffer(nd)%datoi)
          if(varkind(ii) == "b") ier=idba_enq (this%handle,btable,buffer(nd)%datob)
          if(varkind(ii) == "d") ier=idba_enq (this%handle,btable,buffer(nd)%datod)
          if(varkind(ii) == "c") ier=idba_enq (this%handle,btable,buffer(nd)%datoc)
        end if
      else
        ier=idba_enq (this%handle,btable,buffer(nd)%datoc) !char is default
      end if
  
                                !bufferizzo il contesto
                                !print *,"lat,lon,ident",lat,lon,ident
                                !print*,year,month,day,hour,minute,sec
                                !print*,btable,dato,buffer(nd)%datiattrb
  

      call init(buffer(nd)%ana,ilat=ilat,ilon=ilon,ident=ident)
      call init(buffer(nd)%time, year=year, month=month, day=day, hour=hour, minute=minute,msec=sec*1000)
      call init(buffer(nd)%level, rlevel1,rl1,rlevel2,rl2)
      call init(buffer(nd)%timerange, rtimerange, p1, p2)
      call init(buffer(nd)%network, rep_memo)
      buffer(nd)%btable = btable

      ! take in account time_definition
      IF (this%vol7d%time_definition == 0) buffer(nd)%time = buffer(nd)%time - &
       timedelta_new(sec=buffer(nd)%timerange%p1)

                                ! put ana in bufferana becouse we can have no station data but we need ana
                                !todo ; we have to do the same for network but I am tired ....      
      if ( index(bufferana%ana,buffer(nd)%ana) <= 0) then
        na=na+1
        call mem_acquire( bufferana,na,0,this%category )

        call init(bufferana(na)%ana,ilat=ilat,ilon=ilon,ident=ident)
        call init(bufferana(na)%time, year=year, month=month, day=day, hour=hour, minute=minute,msec=sec*1000)
        call init(bufferana(na)%level, rlevel1,rl1,rlevel2,rl2)
        call init(bufferana(na)%timerange, rtimerange, p1, p2)
        call init(bufferana(na)%network, rep_memo)

        bufferana(na)%dator=DBA_MVR
        bufferana(na)%datoi=DBA_MVI
        bufferana(na)%datob=DBA_MVB
        bufferana(na)%datod=DBA_MVD
        bufferana(na)%datoc=DBA_MVC
        bufferana(na)%btable = DBA_MVC

      end if


    else

      ! ---------------->   anagrafica


      !ora legge tutti i dati di anagrafica e li mette in bufferana


                                !anno mese giorno
      if (btable == "B04001" .or. btable == "B04002" .or. btable == "B04003") cycle
                                !ora minuti secondi
      if (btable == "B04004" .or. btable == "B04005" .or. btable == "B04006") cycle
                                ! network
      if (btable == "B01193" .or. btable == "B01194") cycle


      if (present (anavar)) then
        if (any(c_e(anavar)) .and. (all(btable /= anavar)))  btable=DBA_MVC
      end if


      if (.not. lanaonly)then
                                !salto lat lon e ident
        if (btable == "B05001" .or. btable == "B06001" .or. btable == "B01011" .or. btable == "B01194") btable=DBA_MVC

      end if

      na=na+1
      call l4f_category_log(this%category,L4F_debug,"numero dati ana:"//to_char(na)//btable)

      call mem_acquire( bufferana,na,0,this%category )

      bufferana(na)%dator=DBA_MVR
      bufferana(na)%datoi=DBA_MVI
      bufferana(na)%datob=DBA_MVB
      bufferana(na)%datod=DBA_MVD
      bufferana(na)%datoc=DBA_MVC
      bufferana(na)%btable = DBA_MVC


      if (c_e(btable)) then

        if (present(anavar).and. present(anavarkind))then
          ii=( firsttrue(anavar == btable))
          if (ii > 0)then
                                !print*, "indici",ii, btable,(varkind(ii))
            if(anavarkind(ii) == "r") ier=idba_enq (this%handle,btable,bufferana(na)%dator)
            if(anavarkind(ii) == "i") ier=idba_enq (this%handle,btable,bufferana(na)%datoi)
            if(anavarkind(ii) == "b") ier=idba_enq (this%handle,btable,bufferana(na)%datob)
            if(anavarkind(ii) == "d") ier=idba_enq (this%handle,btable,bufferana(na)%datod)
            if(anavarkind(ii) == "c") ier=idba_enq (this%handle,btable,bufferana(na)%datoc)
          end if
        else
          ier=idba_enq (this%handle,btable,bufferana(na)%datoc) !char is default
                                !print*,"dato anagrafica",btable," ",bufferana(na)%dator
        end if
      end if
                                !bufferizzo il contesto
                                !print *,"lat,lon",lat,lon
                                !print*,year,month,day,hour,minute,sec
                                !print*,btable,na
  
      call init(bufferana(na)%ana,ilat=ilat,ilon=ilon,ident=ident)
      call init(bufferana(na)%time, year=year, month=month, day=day, hour=hour, minute=minute,msec=sec*1000)
      call init(bufferana(na)%level, rlevel1,rl1,rlevel2,rl2)
      call init(bufferana(na)%timerange, rtimerange, p1, p2)
      call init(bufferana(na)%network, rep_memo)
      bufferana(na)%btable = btable

    end if
  end do
end do

! ---------------->   anagrafica fine

if (.not. present(var))then
  nvar = count_distinct(buffer(:nd)%btable, back=.TRUE.)
else 
  if ( all(.not. c_e(var))) then
    nvar = count_distinct(buffer(:nd)%btable, back=.TRUE.)
  else
    nvar=count(c_e(var))
  end if
end if

nana = count_distinct(bufferana(:na)%ana, back=.TRUE.)
!nana = count_distinct(buffer(:nd)%ana, back=.TRUE.)
ntime = count_distinct(buffer(:nd)%time, back=.TRUE.)
ntimerange = count_distinct(buffer(:nd)%timerange, back=.TRUE.)
nlevel = count_distinct(buffer(:nd)%level, back=.TRUE.)
if (ldegnet) then
  nnetwork=1
else
  ALLOCATE(networktmp(na+nd))
  networktmp(1:nd) = buffer(1:nd)%network
  networktmp(nd+1:na+nd) = bufferana(1:na)%network
  nnetwork = count_distinct(networktmp, back=.TRUE.)
endif


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

!!$print *, "nana=",nana," ntime=",ntime," ntimerange=",ntimerange, &
!!$ " nlevel=",nlevel," nnetwork=",nnetwork," ndativarr=",ndativarr

ndatiattrr=0
ndatiattri=0
ndatiattrb=0
ndatiattrd=0
ndatiattrc=0

ndativarattrr=0
ndativarattri=0
ndativarattrb=0
ndativarattrd=0
ndativarattrc=0

! ---------------->   anagrafica

if (.not. present(anavar))then
  nanavar = count_distinct(bufferana(:na)%btable, back=.TRUE.,mask=(bufferana(:na)%btable /= DBA_MVC))
else
  if (all(.not. c_e(anavar))) then
    nanavar = count_distinct(bufferana(:na)%btable, back=.TRUE.,mask=(bufferana(:na)%btable /= DBA_MVC))
  else
    nanavar = count(c_e(anavar))
  end if
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


nanaattrr=0
nanaattri=0
nanaattrb=0
nanaattrd=0
nanaattrc=0

nanavarattrr=0
nanavarattri=0
nanavarattrb=0
nanavarattrd=0
nanavarattrc=0


! ---------------->   anagrafica fine


CALL init(vol7dtmp,time_definition=this%vol7d%time_definition)

if (lanaonly)then

  ! qui faccio le operazioni minime per avere solo l'anagrafica utile per certe operazioni

  CALL vol7d_alloc (vol7dtmp, nana=nana, nnetwork=nnetwork)
  call vol7d_alloc_vol(vol7dtmp)
  vol7dtmp%ana=pack_distinct(bufferana(:na)%ana, nana, back=.TRUE.)

  ! Release memory
  deallocate (buffer)
  deallocate (bufferana)

  if(ldegnet)then
    vol7dtmp%network(1)=set_network
  else
    vol7dtmp%network=pack_distinct(networktmp, nnetwork, back=.TRUE.)
    DEALLOCATE(networktmp)
  end if

  ! Smart merge
  CALL vol7d_merge(this%vol7d, vol7dtmp)

  return

end if


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

vol7dtmp%ana=pack_distinct(bufferana(:na)%ana, nana, back=.TRUE.)
!vol7dtmp%ana=pack_distinct(buffer(:nd)%ana, nana, back=.TRUE.)
vol7dtmp%time=pack_distinct(buffer(:nd)%time, ntime, back=.TRUE.)
call sort(vol7dtmp%time)
vol7dtmp%timerange=pack_distinct(buffer(:nd)%timerange, ntimerange, back=.TRUE.)
call sort(vol7dtmp%timerange)
vol7dtmp%level=pack_distinct(buffer(:nd)%level, nlevel, back=.TRUE.)
call sort(vol7dtmp%level)

if(ldegnet)then
  vol7dtmp%network(1)=set_network
else
  vol7dtmp%network=pack_distinct(networktmp, nnetwork, back=.TRUE.)
  DEALLOCATE(networktmp)
end if

!print*,"reti presenti", vol7dtmp%network%name,buffer%network%name

if (present(var).and. present(varkind))then

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
else if (present(var))then
  if (any(c_e(var))) then
    do i=1, nvar
      call init (vol7dtmp%dativar%c(i), btable=var(i))
    end do

  else

    do i=1,ndativarc
      call init(vol7dtmp%dativar%c(i))
    end do
    if (ndativarc > 0) then
      call pack_distinct_c(buffer(:nd)%btable,vol7dtmp%dativar%c%btable, back=.TRUE.)
    end if
  end if

else
  do i=1,ndativarc
    call init(vol7dtmp%dativar%c(i))
  end do
  if (ndativarc > 0) then
    call pack_distinct_c(buffer(:nd)%btable,vol7dtmp%dativar%c%btable, back=.TRUE.)
  end if
end if


!-----------------------> anagrafica

if (present(anavar).and. present(anavarkind))then

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
else if (present(anavar))then

  IF (ANY(c_e(anavar))) THEN
    DO i=1, nanavar
      CALL init (vol7dtmp%anavar%c(i), btable=anavar(i))
    END DO
  ELSE

    do i=1,nanavarc
      call init(vol7dtmp%anavar%c(i))
    end do

    if (nanavarc > 0) then ! we can have only lat lon and ident and not btables at all (so here we can get a strange segfault)
      call pack_distinct_c(bufferana(:na)%btable,vol7dtmp%anavar%c%btable, back=.TRUE.,&
       mask=(bufferana(:na)%btable /= DBA_MVC))
    end if

  ENDIF

else

  do i=1,nanavarc
    call init(vol7dtmp%anavar%c(i))
  end do

  if (nanavarc > 0) then ! we can have only lat lon and ident and not btables at all (so here we can get a strange segfault)
    call pack_distinct_c(bufferana(:na)%btable,vol7dtmp%anavar%c%btable, back=.TRUE.,&
     mask=(bufferana(:na)%btable /= DBA_MVC))
  end if
end if

!-----------------------> anagrafica fine

call vol7d_alloc_vol (vol7dtmp)

do i =1, nd

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
  
end do

!------------------------- anagrafica


do i =1, Na

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
   if (nanavarc > 0) then 
     if(c_e(bufferana(i)%datoc))then
       indanavar = firsttrue(bufferana(i)%btable == vol7dtmp%anavar%c%btable)
       vol7dtmp%volanac( indana,indanavar,indnetwork ) = bufferana(i)%datoc
     end if
   end if

 end do

!------------------------- anagrafica fine

!
! Release memory
!

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


END SUBROUTINE vol7d_dballe_importvvns_file



subroutine mem_acquire( buffer,n,npool,category )

INTEGER      :: n,mem,npool,category,istat
type(record),pointer :: buffer(:)
type(record),pointer :: buffertmp(:)


if ( n == 0 ) then

  allocate (buffer(npool))
  return

end if

mem=size(buffer)

!call l4f_category_log(category,L4F_DEBUG,"mem_acquire dimension of buffer: "//to_char(mem)//" "//to_char(n))

if (n > mem) then

  ALLOCATE (buffertmp(max(mem*2,n)),stat=istat)
  IF (istat /= 0) THEN
    CALL l4f_category_log(category,L4F_ERROR,'mem_acquire, cannot allocate ' &
     //TRIM(to_char(mem*2))//' buffer elements')
    CALL raise_fatal_error()
  endif

  buffertmp(:mem)=buffer(:)

  deallocate (buffer)

  buffer=>buffertmp

end if

end subroutine mem_acquire


end MODULE vol7d_dballeold_class

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


