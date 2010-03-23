#include "config.h"

!>\brief  classe per import ed export di volumi da e in DB-All.e 
!!
!!Questo modulo definisce gli oggetti e i metodi per gestire
!!l'importazione e l'esportazione di volumi dal database per dati sparsi
!!DB-All.e
!!
!!Il tutto funziona intorno all'oggetto vol7d_dballe che aggiunge ad un
!!oggetto vol7d ulteriori informazioni.
!!
!!Con la chiamata init vengono definiti i parametri di accesso alla DSN
!!(database) di DB-All.e.
!!
!!Con import è possibile acquisire nel prorio programma i dati presenti
!!nel DSN; l'allocazione di memoria è automatica. Import è in grado di
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
!!vettore di variabili e un vettore di reti. vol7d_dballe%%data_id
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
!!Utilizzando due differenti oggetti uno per import e uno per export è
!!possibile tramite l'associazione del puntatore in essi contenuto
!!relativo al volume vol7d ricopiare contenuti in altri DSN senza
!!sprechi di memoria.
!!
!!Programma esempio
!!\include esempio_v7ddballe.f90
!!
!!\ingroup vol7d
MODULE vol7d_dballe_class

USE char_utilities
USE vol7d_class
USE vol7d_utilities
use log4fortran

IMPLICIT NONE
PRIVATE
PUBLIC vol7d_dballe, init, delete, import, export, vol7d_dballe_set_var_du

include "dballef.h"

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


CHARACTER(len=20),PRIVATE :: dballe_name='dballe', dballe_name_env='DBA_TABLES'


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
  TYPE(vol7d_var) ::  dativar

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



CONTAINS


!>\brief  inizializza l'oggetto
SUBROUTINE vol7d_dballe_init(this,dsn,user,password,write,wipe,repinfo,&
 filename,format,file,categoryappend)


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

character(len=1):: mode ! the open mode ("r" for read, "w" for write or create, "a" append) (comandato da "write", default="r" )

character(len=50) :: quidsn,quiuser,quipassword
character(len=255) :: quirepinfo
logical :: quiwrite,quiwipe,quifile

character(len=512) :: a_name
character(len=254) :: arg,lfilename,lformat
logical :: exist

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
!init di log4fortran
!ier=l4f_init()
this%category=l4f_category_get(a_name)

nullify(this%data_id)

!TODO: quando scrivo bisogna gestire questo che non è da fare ?
CALL init(this%vol7d)

                                ! impostiamo la gestione dell'errore
call idba_error_set_callback(0,v7d_dballe_error_handler, &
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
    if (filename == "")then
      filename=lfilename
    else
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

  call idba_messaggi(this%handle,lfilename,mode,lformat)

  this%file=.true.
  call l4f_category_log(this%category,L4F_DEBUG,"handle from idba_messaggi: "//to_char(this%handle))
  call l4f_category_log(this%category,L4F_DEBUG,"filename: "//to_char(lfilename))
  call l4f_category_log(this%category,L4F_DEBUG,"mode: "//to_char(mode))
  call l4f_category_log(this%category,L4F_DEBUG,"format: "//to_char(lformat))

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
    
                                !print*,"write=",quiwrite,"wipe=",quiwipe,"dsn=",quidsn
  
  if(quiwrite)then
    call idba_presentati(this%idbhandle,quidsn,quiuser,quipassword)
    call idba_preparati (this%idbhandle,this%handle,"write","write","write")
    call idba_preparati (this%idbhandle,this%handle_staz,"write","write","write")
  else
    call idba_presentati(this%idbhandle,quidsn,quiuser,quipassword)
    call idba_preparati (this%idbhandle,this%handle,"read","read","read")
    call idba_preparati (this%idbhandle,this%handle_staz,"read","read","read")
  end if
  
  if (quiwipe)call idba_scopa (this%handle,quirepinfo)
  
  this%file=.false.
  
endif

END SUBROUTINE vol7d_dballe_init


!>\brief Importa un volume dati da un DSN DB-all.e
!!
!! import da DB-all.e:
!! var e network sono scalari.

SUBROUTINE vol7d_dballe_importvsns(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly)
TYPE(vol7d_dballe),INTENT(inout) :: this  !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in) :: var  !< variabile da importare secondo la tabella B locale o relativi alias
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'importazione
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
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
logical,intent(in),optional :: anaonly !< imposta importazione della sola anagrafica 

CALL import(this, (/var/), network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)

END SUBROUTINE vol7d_dballe_importvsns

!>\brief Identica a vol7d_dballe_importvsns con network vettore.
!!
!!import da DB-all.e

SUBROUTINE vol7d_dballe_importvsnv(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in) :: var
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_network),INTENT(in) :: network(:)
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)


INTEGER :: i

DO i = 1, SIZE(network)
  CALL import(this, (/var/), network(i), coordmin, coordmax, timei, timef, level,timerange,set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
ENDDO

END SUBROUTINE vol7d_dballe_importvsnv

!>\brief Identica a vol7d_dballe_importvsns con var e network vettore.
!!
!!import da DB-all.e

SUBROUTINE vol7d_dballe_importvvnv(this, var, network, coordmin,coordmax, timei, timef, level,timerange,set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in) :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_network),INTENT(in) :: network(:)
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)

INTEGER :: i

DO i = 1, SIZE(network)
  CALL import(this, var, network(i), coordmin, coordmax, timei, timef, level,timerange,set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
ENDDO

END SUBROUTINE vol7d_dballe_importvvnv

!>\brief Identica a vol7d_dballe_importvsns con var vettore.
!!
!!import da DB-all.e oppure da BUFR/CREX formato generico

SUBROUTINE vol7d_dballe_importvvns(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in),optional :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),OPTIONAL :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network,set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)
logical,intent(in),optional :: anaonly

if (this%file) then

  call vol7d_dballe_importvvns_file(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
   attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly)

else
  if (optio_log(anaonly)) then
    CALL l4f_category_log(this%category,L4F_ERROR,"anaonly=.true. not supported accessing to dba")
    CALL raise_fatal_error()
  end if

  call vol7d_dballe_importvvns_dba(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
   attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
  
end if

end SUBROUTINE vol7d_dballe_importvvns



!>\brief Identica a vol7d_dballe_importvsns con var vettore.
!!
!!import da DB-all.e
SUBROUTINE vol7d_dballe_importvvns_dba(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),OPTIONAL :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network,set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)


!TYPE(vol7d) :: v7d
CHARACTER(len=SIZE(var)*7) :: varlist
CHARACTER(len=SIZE(attr)*8) :: starvarlist
CHARACTER(len=6) :: btable
CHARACTER(len=7) ::starbtable

LOGICAL ::  ldegnet, lattr, lanaattr
integer :: year,month,day,hour,minute,sec
integer :: rlevel1, rl1,rlevel2, rl2
integer :: rtimerange, p1, p2
character(len=network_name_len) :: rep_memo
integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork


integer :: nana,ntime,ntimerange,nlevel,nnetwork
TYPE(vol7d_var) :: var_tmp

INTEGER :: i,ii, iii,n,n_ana,nn,nvarattr,istat,indattr
integer :: nvar ,inddatiattr,inddativarattr
integer :: nanavar ,indanavar,indanaattr,indanavarattr,nanavarattr

REAL(kind=fp_geo) :: lat,lon
CHARACTER(len=vol7d_ana_lenident) :: ident
!INTEGER(kind=int_b)::attrdatib

integer :: ndativarr,     ndativari,     ndativarb,     ndativard,     ndativarc
integer :: ndatiattrr,    ndatiattri,    ndatiattrb,    ndatiattrd,    ndatiattrc 
integer :: ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc

integer :: nanavarr,     nanavari,     nanavarb,     nanavard,     nanavarc
integer :: nanaattrr,    nanaattri,    nanaattrb,    nanaattrd,    nanaattrc 
integer :: nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc

integer :: ir,ib,id,ic


!TYPE(datetime) :: odatetime
! nobs, ntime, nana, nvout, nvin, nvbt, &
! datai(3), orai(2), dataf(3), oraf(2),ist
!CHARACTER(len=12),ALLOCATABLE :: tmtmp(:)
!INTEGER,ALLOCATABLE :: anatmp(:), vartmp(:), mapdatao(:)
!LOGICAL :: found, non_valid, varbt_req(SIZE(vartable))

TYPE(vol7d) :: vol7dtmp

type(record),ALLOCATABLE :: buffer(:),bufferana(:)

!!!  CALL print_info('Estratte dall''archivio '//TRIM(to_char(nobs)) // ' osservazioni')


IF (PRESENT(set_network)) THEN
   ldegnet = .TRUE.
ELSE
   ldegnet = .FALSE.
ENDIF

IF (PRESENT(attr)) THEN
   lattr = .TRUE.
ELSE
   lattr = .FALSE.
ENDIF

IF (PRESENT(anaattr)) THEN
   lanaattr = .TRUE.
ELSE
   lanaattr = .FALSE.
ENDIF

call idba_unsetall(this%handle)

if(present(network))call idba_set (this%handle,"rep_memo",network%name)
call idba_set (this%handle,"mobile",0)
!print*,"network,mobile",network%name,0

if(ldegnet)call idba_set (this%handle,"query","best")

if (present(coordmin)) then
  CALL geo_coord_to_geo(coordmin)
  CALL getval(coordmin, lat=lat,lon=lon)
  call idba_set(this%handle,"lonmin",lon)
  call idba_set(this%handle,"latmin",lat)
end if

if (present(coordmax)) then
  CALL geo_coord_to_geo(coordmax)
  CALL getval(coordmax, lat=lat,lon=lon)
  call idba_set(this%handle,"lonmax",lon)
  call idba_set(this%handle,"latmax",lat)
end if

if (present(timei)) then
  CALL getval(timei, year=year, month=month, day=day, hour=hour, minute=minute)
  call idba_setdatemin(this%handle,year,month,day,hour,minute,0)
                                !print *,"datemin",year,month,day,hour,minute,0
end if

if (present(timef)) then
  CALL getval(timef, year=year, month=month, day=day, hour=hour, minute=minute)
  call idba_setdatemax(this%handle,year,month,day,hour,minute,0)
                                !print *,"datemax",year,month,day,hour,minute,0
end if


nvar=0

if (present (var)) then
                                ! creo la stringa con l'elenco
  varlist = ''
  DO i = 1, SIZE(var)
    nvar = nvar + 1
    IF (nvar > 1) varlist(LEN_TRIM(varlist)+1:) = ',' 
    varlist(LEN_TRIM(varlist)+1:) = TRIM(var(i))
  ENDDO
                                !print *,"varlist",varlist
  
  call idba_set(this%handle, "varlist",varlist )

end if

if (present(timerange))then
   call idba_settimerange(this%handle, timerange%timerange, timerange%p1, timerange%p2)
end if

if (present(level))then
   call idba_setlevel(this%handle, level%level1, level%l1,level%level2, level%l2)
end if

call idba_voglioquesto (this%handle,N)
!print*,"numero di dati ",N

!ora che so quanti dati ho alloco la memoria per buffer
allocate(buffer(N),stat=istat)
IF (istat/= 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,'cannot allocate ' &
   //TRIM(to_char(n))//' buffer elements')
  CALL raise_fatal_error()
ENDIF


! dammi tutti i dati
do i=1,N

  call idba_dammelo (this%handle,btable)
  
  call idba_enqdate (this%handle,year,month,day,hour,minute,sec)
  call idba_enqlevel(this%handle, rlevel1, rl1, rlevel2,rl2)
  call idba_enqtimerange(this%handle, rtimerange, p1, p2)
  call idba_enq(this%handle, "rep_memo",rep_memo)
                                !print *,"trovato network",rep_memo
  
                                !nbtable=btable_numerico(btable)
                                ! ind = firsttrue(qccli%v7d%dativar%r(:)%btable == nbtable)
                                ! IF (ind<1) cycle ! non c'e'
  
  buffer(i)%dator=DBA_MVR
  buffer(i)%datoi=DBA_MVI
  buffer(i)%datob=DBA_MVB
  buffer(i)%datod=DBA_MVD
  buffer(i)%datoc=DBA_MVC
  
  if (present(var).and. present(varkind))then
    ii=( firsttrue(var == btable))
    if (ii > 0)then
                                !print*, "indici",ii, btable,(varkind(ii))
      if(varkind(ii) == "r") call idba_enq (this%handle,btable,buffer(i)%dator)
      if(varkind(ii) == "i") call idba_enq (this%handle,btable,buffer(i)%datoi)
      if(varkind(ii) == "b") call idba_enq (this%handle,btable,buffer(i)%datob)
      if(varkind(ii) == "d") call idba_enq (this%handle,btable,buffer(i)%datod)
      if(varkind(ii) == "c") call idba_enq (this%handle,btable,buffer(i)%datoc)
    end if
  else
    call idba_enq (this%handle,btable,buffer(i)%datoc) !char is default
  end if
  
                                !metto in memoria l'identificatore numerico dei dati
                                !print*,buffer(i)%data_id
  call idba_enq (this%handle,"context_id",buffer(i)%data_id)

                                !recupero i dati di anagrafica
  call idba_enq (this%handle,"lat",   lat)
  call idba_enq (this%handle,"lon",   lon)
  call idba_enq (this%handle,"ident",ident)
   
                                !bufferizzo il contesto
                                !print *,"lat,lon,ident",lat,lon,ident
                                !print*,year,month,day,hour,minute,sec
                                !print*,btable,dato,buffer(i)%datiattrb
  
  call init(buffer(i)%ana,lat=lat,lon=lon,ident=ident)
  call init(buffer(i)%time, year=year, month=month, day=day, hour=hour, minute=minute)
  call init(buffer(i)%level, rlevel1,rl1,rlevel2,rl2)
  call init(buffer(i)%timerange, rtimerange, p1, p2)
  call init(buffer(i)%network, rep_memo)
  call init(buffer(i)%dativar, btable)

end do

! ---------------->   anagrafica

!ora legge tutti i dati di anagrafica e li mette in bufferana

call idba_unsetall(this%handle_staz)

if(present(network))call idba_set (this%handle_staz,"rep_memo",network%name)
call idba_set (this%handle_staz,"mobile",0)
!print*,"network,mobile",network%name,0

if(ldegnet)call idba_set (this%handle_staz,"query","best")

if (present(coordmin)) then
  CALL geo_coord_to_geo(coordmin)
  CALL getval(coordmin, lat=lat,lon=lon)
  call idba_set(this%handle_staz,"lonmin",lon)
  call idba_set(this%handle_staz,"latmin",lat)
end if

if (present(coordmax)) then
  CALL geo_coord_to_geo(coordmax)
  CALL getval(coordmax, lat=lat,lon=lon)
  call idba_set(this%handle_staz,"lonmax",lon)
  call idba_set(this%handle_staz,"latmax",lat)
end if

nanavar=0

if (present (anavar)) then
                                ! creo la stringa con l'elenco
  varlist = ''
  DO i = 1, SIZE(anavar)
    nanavar = nanavar + 1
    IF (nanavar > 1) varlist(LEN_TRIM(varlist)+1:) = ',' 
    varlist(LEN_TRIM(varlist)+1:) = TRIM(anavar(i))
  ENDDO
                                !print *,"varlist",varlist
  call idba_set(this%handle_staz, "varlist",varlist )

end if


call idba_setcontextana(this%handle_staz)
call idba_voglioquesto (this%handle_staz,N_ana)
!print*,"numero di dati ",N_ana

!ora che so quanti dati ho alloco la memoria per bufferana
allocate(bufferana(N_ana),stat=istat)
if (istat/= 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,'cannot allocate ' &
   //TRIM(to_char(n_ana))//' bufferana elements')
  CALL raise_fatal_error()
ENDIF


! dammi tutti i dati di anagrafica
do i=1,N_ana

  bufferana(i)%dator=DBA_MVR
  bufferana(i)%datoi=DBA_MVI
  bufferana(i)%datob=DBA_MVB
  bufferana(i)%datod=DBA_MVD
  bufferana(i)%datoc=DBA_MVC
  call init(bufferana(i)%dativar, DBA_MVC)

  call idba_dammelo (this%handle_staz,btable)

  
                                !salto lat lon e ident
  if (btable == "B05001" .or. btable == "B06001" .or. btable == "B01011") cycle

  call idba_enqdate (this%handle_staz,year,month,day,hour,minute,sec)
  call idba_enqlevel(this%handle_staz, rlevel1, rl1, rlevel2,rl2)
  call idba_enqtimerange(this%handle_staz, rtimerange, p1, p2)
  call idba_enq(this%handle_staz, "rep_memo",rep_memo)
                                !print *,"trovato network",rep_memo
                                !nbtable=btable_numerico(btable)
                                ! ind = firsttrue(qccli%v7d%dativar%r(:)%btable == nbtable)
                                ! IF (ind<1) cycle ! non c'e'
  

  if (present(anavar).and. present(anavarkind))then
    ii=( firsttrue(anavar == btable))
    if (ii > 0)then
                                !print*, "indici",ii, btable,(varkind(ii))
      if(anavarkind(ii) == "r") call idba_enq (this%handle_staz,btable,bufferana(i)%dator)
      if(anavarkind(ii) == "i") call idba_enq (this%handle_staz,btable,bufferana(i)%datoi)
      if(anavarkind(ii) == "b") call idba_enq (this%handle_staz,btable,bufferana(i)%datob)
      if(anavarkind(ii) == "d") call idba_enq (this%handle_staz,btable,bufferana(i)%datod)
      if(anavarkind(ii) == "c") call idba_enq (this%handle_staz,btable,bufferana(i)%datoc)
    end if
  else
    call idba_enq (this%handle_staz,btable,bufferana(i)%datoc) !char is default
    !print*,"dato anagrafica",btable," ",bufferana(i)%dator
  end if
  
                                !metto in memoria l'identificatore numerico dei dati
                                !print*,buffer(i)%data_id
  call idba_enq (this%handle_staz,"context_id",bufferana(i)%data_id)

                                !recupero i dati di anagrafica
  call idba_enq (this%handle_staz,"lat",   lat)
  call idba_enq (this%handle_staz,"lon",   lon)
  call idba_enq (this%handle_staz,"ident",ident)
   
                                !bufferizzo il contesto
                                !print *,"lat,lon",lat,lon
                                !print*,year,month,day,hour,minute,sec
                                !print*,btable,dato,buffer(i)%datiattrb
  
  call init(bufferana(i)%ana,lat=lat,lon=lon,ident=ident)
  call init(bufferana(i)%time, year=year, month=month, day=day, hour=hour, minute=minute)
  call init(bufferana(i)%level, rlevel1,rl1,rlevel2,rl2)
  call init(bufferana(i)%timerange, rtimerange, p1, p2)
  call init(bufferana(i)%network, rep_memo)
  call init(bufferana(i)%dativar, btable)

end do

! ---------------->   anagrafica fine

if (.not. present(var))then
  nvar = count_distinct(buffer%dativar, back=.TRUE.)
end if

nana = count_distinct(buffer%ana, back=.TRUE.)
ntime = count_distinct(buffer%time, back=.TRUE.)
ntimerange = count_distinct(buffer%timerange, back=.TRUE.)
nlevel = count_distinct(buffer%level, back=.TRUE.)
nnetwork = count_distinct(buffer%network, back=.TRUE.)
if(ldegnet)nnetwork=1

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

if (.not. present(anavar))then
  nanavar = count_distinct(bufferana%dativar, back=.TRUE.,mask=(bufferana%dativar%btable /= DBA_MVC))
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


CALL init(vol7dtmp)

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

! print *, "nana=",nana, "ntime=",ntime, "ntimerange=",ntimerange, &
! "nlevel=",nlevel, "nnetwork=",nnetwork, &
! "ndativarr=",ndativarr, "ndativari=",ndativari, &
! "ndativarb=",ndativarb, "ndativard=",ndativard, "ndativarc=",ndativarc,&
! "ndatiattrr=",ndatiattrr, "ndatiattri=",ndatiattri, "ndatiattrb=",ndatiattrb,&
! "ndatiattrd=",ndatiattrd, "ndatiattrc=",ndatiattrc,&
! "ndativarattrr=",ndativarattrr, "ndativarattri=",ndativarattri, "ndativarattrb=",ndativarattrb,&
! "ndativarattrd=",ndativarattrd, "ndativarattrc=",ndativarattrc
! print*,"ho fatto alloc"


vol7dtmp%ana=pack_distinct(buffer%ana, nana, back=.TRUE.)
vol7dtmp%time=pack_distinct(buffer%time, ntime, back=.TRUE.)
vol7dtmp%timerange=pack_distinct(buffer%timerange, ntimerange, back=.TRUE.)
vol7dtmp%level=pack_distinct(buffer%level, nlevel, back=.TRUE.)

if(ldegnet)then
  vol7dtmp%network(1)=set_network
else
  vol7dtmp%network=pack_distinct(buffer%network, nnetwork, back=.TRUE.)
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

  do i=1, nvar
    call init (vol7dtmp%dativar%c(i), btable=var(i))
  end do

else

  vol7dtmp%dativar%c=pack_distinct(buffer%dativar, ndativarc, back=.TRUE.)

end if



if ( present(attrkind).and. present(attr).and. present(var))then

    ir=0
    ii=0
    ib=0
    id=0
    ic=0

  do i=1,size(var)
  
    if ( ndativarattrr > 0 )then
      ir=ir+1
      call init (vol7dtmp%dativarattr%r(ir), btable=var(i))
    end if

    if ( ndativarattri > 0 )then
      ii=ii+1
      call init (vol7dtmp%dativarattr%i(ii), btable=var(i))
    end if

    if ( ndativarattrb > 0 )then
      ib=ib+1
      call init (vol7dtmp%dativarattr%b(ib), btable=var(i))
    end if

    if ( ndativarattrd > 0 )then
      id=id+1
      call init (vol7dtmp%dativarattr%d(id), btable=var(i))
    end if

    if ( ndativarattrc > 0 )then
      ic=ic+1
      call init (vol7dtmp%dativarattr%c(ic), btable=var(i))
    end if

  end do

else  if (present(attr).and.present(var))then

  do i=1,size(var)
    if ( ndativarattrc > 0 )call init (vol7dtmp%dativarattr%c(i), btable=var(i))
  end do

else if (associated(vol7dtmp%dativarattr%c).and. associated(vol7dtmp%dativar%c)) then

      vol7dtmp%dativarattr%c(1)=vol7dtmp%dativar%c(1)

end if


if (present(attrkind).and. present(attr))then

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

  do i=1, nanavar
    call init (vol7dtmp%anavar%c(i), btable=anavar(i))
  end do

else

  vol7dtmp%anavar%c=pack_distinct(bufferana%dativar, nanavarc, back=.TRUE.,mask=(bufferana%dativar%btable /= DBA_MVC))

end if



if ( present(anaattrkind) .and. present(anaattr) .and. present(anavar))then

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

else  if (present(anaattr) .and. present (anavar))then

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

!print*,"prima di alloc"

call vol7d_alloc_vol (vol7dtmp)

if (lattr) then

  allocate  (this%data_id( nana, ntime, nlevel, ntimerange, nnetwork),stat=istat)
  if (istat/= 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,'cannot allocate ' &
   //TRIM(to_char(nana*ntime*nlevel*ntimerange*nnetwork))//' data_id elements')
  CALL raise_fatal_error()
ENDIF

this%data_id=DBA_MVI

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

!print*,"ho fatto un volume vuoto"

if (lattr)then
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
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%r)
     vol7dtmp%voldatir( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%dator
   end if

   if(c_e(buffer(i)%datoi)) then
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%i)
     vol7dtmp%voldatii( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datoi
   end if

   if(c_e(buffer(i)%datob)) then
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%b)
     vol7dtmp%voldatib( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datob
   end if

   if(c_e(buffer(i)%datod)) then
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%d)
     vol7dtmp%voldatid( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datod
   end if

   if(c_e(buffer(i)%datoc)) then
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%c)
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

     call idba_unsetall (this%handle)
     call idba_set (this%handle,"*context_id",buffer(i)%data_id)
     call idba_set (this%handle,"*var_related",buffer(i)%dativar%btable)
     !per ogni dato ora lavoro sugli attributi
     call idba_set(this%handle, "*varlist",starvarlist )
     call idba_voglioancora (this%handle,nn)
     !print*,buffer(i)%dativar%btable," numero attributi",nn
     
     do ii=1,nn ! Se ho piu` di 1 attributo devo forse trovare l'indice (ii)
       call idba_ancora (this%handle,starbtable)
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
             inddativarattr  = firsttrue(buffer(i)%dativar == vol7dtmp%dativarattr%r)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%r)
             call idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattrr(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
           if(attrkind(iii) == "i") then
             inddativarattr  = firsttrue(buffer(i)%dativar == vol7dtmp%dativarattr%i)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%i)
             call idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattri(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
           if(attrkind(iii) == "b") then
             inddativarattr  = firsttrue(buffer(i)%dativar == vol7dtmp%dativarattr%b)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%b)
             !print *,"indici voldatiattr ",indana,indtime,indlevel,indtimerange,&
              !inddativarattr,indnetwork,inddatiattr
             call idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattrb(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
           if(attrkind(iii) == "d") then
             inddativarattr  = firsttrue(buffer(i)%dativar == vol7dtmp%dativarattr%d)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%d)
             call idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattrd(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
           if(attrkind(iii) == "c") then
             inddativarattr  = firsttrue(buffer(i)%dativar == vol7dtmp%dativarattr%c)
             inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%c)
             call idba_enq (this%handle,starbtable,&
              vol7dtmp%voldatiattrc(indana,indtime,indlevel,indtimerange,&
              inddativarattr,indnetwork,inddatiattr))
           end if
         end if
       else         
         inddativarattr  = firsttrue(buffer(i)%dativar == vol7dtmp%dativarattr%c)
         inddatiattr = firsttrue(var_tmp == vol7dtmp%datiattr%c)
         call idba_enq (this%handle,starbtable,&
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
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%r)
     vol7dtmp%volanar( indana,indanavar,indnetwork ) = bufferana(i)%dator
   end if
   if(c_e(bufferana(i)%datoi))then
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%i)
     vol7dtmp%volanai( indana,indanavar,indnetwork ) = bufferana(i)%datoi
   end if
   if(c_e(bufferana(i)%datob))then
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%b)
     vol7dtmp%volanab( indana,indanavar,indnetwork ) = bufferana(i)%datob
   end if
   if(c_e(bufferana(i)%datod))then
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%d)
     vol7dtmp%volanad( indana,indanavar,indnetwork ) = bufferana(i)%datod
   end if
   if(c_e(bufferana(i)%datoc))then
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%c)
     vol7dtmp%volanac( indana,indanavar,indnetwork ) = bufferana(i)%datoc
   end if


   if (lanaattr)then

     call idba_unsetall (this%handle_staz)
     call idba_set (this%handle_staz,"*context_id",bufferana(i)%data_id)
     call idba_set (this%handle_staz,"*var_related",bufferana(i)%dativar%btable)

     !per ogni dato ora lavoro sugli attributi
     call idba_set(this%handle_staz, "*varlist",starvarlist )
     call idba_voglioancora (this%handle_staz,nn)
     !print*,buffer(i)%dativar%btable," numero attributi",nn
     
     do ii=1,nn ! Se ho piu` di 1 attributo devo forse trovare l'indice (ii)
       call idba_ancora (this%handle_staz,starbtable)
         !print *, starbtable
       indattr = firsttrue(anaattr == starbtable)
       IF (indattr<1) cycle ! non c'e'


       call init (var_tmp, btable=starbtable)


       if (present(anaattrkind))then
         iii=( firsttrue(anaattr == starbtable))
         !print *,"ho letto indice attributo ",starbtable,iii 
         if (iii > 0)then

           if(anaattrkind(iii) == "r") then
             indanavarattr  = firsttrue(bufferana(i)%dativar == vol7dtmp%anavarattr%r)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%r)
             call idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattrr(indana,indanavarattr,indnetwork,indanaattr))
           end if
           if(anaattrkind(iii) == "i") then
             indanavarattr  = firsttrue(bufferana(i)%dativar == vol7dtmp%anavarattr%i)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%i)
             call idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattri(indana,indanavarattr,indnetwork,indanaattr))
           end if
           if(anaattrkind(iii) == "b") then
             indanavarattr  = firsttrue(bufferana(i)%dativar == vol7dtmp%anavarattr%b)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%b)
             call idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattrb(indana,indanavarattr,indnetwork,indanaattr))
           end if
           if(anaattrkind(iii) == "d") then
             indanavarattr  = firsttrue(bufferana(i)%dativar == vol7dtmp%anavarattr%d)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%d)
             call idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattrd(indana,indanavarattr,indnetwork,indanaattr))
           end if
           if(anaattrkind(iii) == "c") then
             indanavarattr  = firsttrue(bufferana(i)%dativar == vol7dtmp%anavarattr%c)
             indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%c)
             call idba_enq (this%handle_staz,starbtable,&
              vol7dtmp%volanaattrc(indana,indanavarattr,indnetwork,indanaattr))
           end if

         end if
       else         
         indanavarattr  = firsttrue(bufferana(i)%dativar == vol7dtmp%anavarattr%c)
         indanaattr = firsttrue(var_tmp == vol7dtmp%anaattr%c)
         call idba_enq (this%handle,starbtable,&
          vol7dtmp%volanaattrc(indana,indanavarattr,indnetwork,indanaattr)) !char is default
       end if

     end do
   end if

 end do

!------------------------- anagrafica fine

deallocate (buffer)
deallocate (bufferana)

! Se l'oggetto ha gia` un volume allocato lo fondo con quello estratto
!>\todo manca test su associated dei vol*
IF (ASSOCIATED(this%vol7d%ana) .AND. ASSOCIATED(this%vol7d%time)) THEN
  CALL vol7d_merge(this%vol7d, vol7dtmp, sort=.TRUE.)
ELSE ! altrimenti lo assegno
  this%vol7d = vol7dtmp
ENDIF

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


END SUBROUTINE vol7d_dballe_importvvns_dba


!> \brief Exporta un volume dati a un DSN DB-all.e
!!
!! Riscrive i dati nel DSN di DB-All.e con la possibilità di attivare
!! una serie di filtri.

SUBROUTINE vol7d_dballe_export(this, network, coordmin, coordmax, ident,&
 timei, timef,level,timerange,var,attr,anavar,anaattr,attr_only,template)

!> \todo gestire il filtro staz_id la qual cosa vuol dire aggiungere un id nel type ana

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto contenente il volume e altre info per l'accesso al DSN
character(len=network_name_len),INTENT(in),optional :: network !< network da exportare
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'esportazione
TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
CHARACTER(len=vol7d_ana_lenident),optional :: ident !< identificativo della stazione da exportare
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
character(len=*),intent(in),optional :: template !< specificando category.subcategory.localcategory oppure un alias ("synop", "metar","temp","generic") forza l'exportazione ad uno specifico template BUFR/CREX"  


!REAL(kind=fp_geo) :: latmin,latmax,lonmin,lonmax
logical, allocatable :: lnetwork(:),llevel(:),ltimerange(:)
integer,allocatable :: ana_id(:,:)
logical :: write,writeattr,lattr_only

!CHARACTER(len=6) :: btable
!CHARACTER(len=7) ::starbtable

integer :: year,month,day,hour,minute
integer :: nstaz,ntime,ntimerange,nlevel,nnetwork


INTEGER :: i,ii,iii,iiii,iiiii,iiiiii,ind,inddatiattr,indanaattr

REAL(kind=fp_geo) :: lat,lon 
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


!!!!!  anagrafica

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo r")
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo i")
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo b")
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo d")
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
call l4f_category_log(this%category,L4F_DEBUG,"macro nana tipo c")
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V


!!!!!!!   dati

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo r")
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo i")
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo b")
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo d")
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
call l4f_category_log(this%category,L4F_DEBUG,"macro ndati tipo c")
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V


! vital statistics data

!print *,"nstaz,ntime,nlevel,ntimerange,nnetwork",nstaz,ntime,nlevel,ntimerange,nnetwork

do iii=1, nnetwork
   if (.not.lnetwork(iii))cycle

! l'anagrafica su file la scrivo solo per i generici
   if (this%file .and. present(template)) then
     if (template /= "generic") cycle
   end if

   do i=1, nstaz

      if (present(coordmin).and.present(coordmax))then

        if (.not. inside(this%vol7d%ana(i)%coord,coordmin,coordmax)) cycle
                                !print * ,"sei dentro, OK"
      end if


! in alternativa si trattano separatamente
!!$    if (present(coordmin)) then
!!$      CALL geo_coord_to_geo(coordmin)
!!$      CALL getval(coordmin, lat=latmin,lon=lonmin)
!!$      if (lonmin > lon) cycle
!!$      if (latmin > lat) cycle
!!$    end if
!!$
!!$    if (present(coordmax)) then
!!$      CALL geo_coord_to_geo(coordmax)
!!$      CALL getval(coordmax, lat=latmax,lon=lonmax)
!!$      if (lonmax < lon) cycle
!!$      if (latmax < lat) cycle
!!$    end if


      CALL geo_coord_to_geo(this%vol7d%ana(i)%coord)
      CALL getval(this%vol7d%ana(i)%coord, lat=lat,lon=lon)

      call idba_unsetall (this%handle)

      call idba_setcontextana (this%handle)
      if (this%file)then
        if (present(template)) then
          call idba_set (this%handle,"query","message "//trim(template))
        else
          call idba_set (this%handle,"query","message")
        end if
      end if

      call idba_set (this%handle,"lat",lat)
      call idba_set (this%handle,"lon",lon)

      if (present(ident))then
         if (c_e(ident) .and. ident /= this%vol7d%ana(i)%ident ) cycle
      end if

      if ( c_e(this%vol7d%ana(i)%ident)) then
         call l4f_category_log(this%category,L4F_DEBUG,"I have found a mobile station! ident: "//&
          to_char(this%vol7d%ana(i)%ident))
         call idba_set (this%handle,"ident",this%vol7d%ana(i)%ident)
         call idba_set (this%handle,"mobile",1)
      else
         call idba_set (this%handle,"mobile",0)
      end if

      call idba_set(this%handle,"rep_memo",this%vol7d%network(iii)%name)

      write=.true.

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

      !se NON ho dati di anagrafica (ma solo lat e long ..) devo fare comunque una prendilo
      ! in quanto write indica DATI in sospeso da scrivere

      if ( write ) then

        call l4f_category_log(this%category,L4F_DEBUG,"eseguo una main prendilo di anagrafica")
        call idba_prendilo (this%handle)

        if (.not. this%file ) then
          call idba_enq (this%handle,"ana_id",ana_id(i,iii))
        end if
      end if


      do ii=1,nanavarr
        if (c_e(this%vol7d%anavar%r(ii)%btable))call idba_unset (this%handle,this%vol7d%anavar%r(ii)%btable )
      end do
      do ii=1,nanavari
        if (c_e(this%vol7d%anavar%i(ii)%btable))call idba_unset (this%handle,this%vol7d%anavar%i(ii)%btable )
      end do
      do ii=1,nanavarb
        if (c_e(this%vol7d%anavar%b(ii)%btable))call idba_unset (this%handle,this%vol7d%anavar%b(ii)%btable )
      end do
      do ii=1,nanavard
        if (c_e(this%vol7d%anavar%d(ii)%btable))call idba_unset (this%handle,this%vol7d%anavar%d(ii)%btable )
      end do
      do ii=1,nanavarc
        if (c_e(this%vol7d%anavar%c(ii)%btable))call idba_unset (this%handle,this%vol7d%anavar%c(ii)%btable )
      end do

      write=.false.

   end do
end do


! data
!print *,"nstaz,ntime,nlevel,ntimerange,nnetwork",nstaz,ntime,nlevel,ntimerange,nnetwork


do i=1, nstaz

   do ii=1,ntime
      if (present(timei) )then
         if ( this%vol7d%time(ii) < timei ) cycle
      endif
      if (present(timef) )then
         if ( this%vol7d%time(ii) > timef ) cycle
      endif
   
      do iiiiii=1, nnetwork
        if (.not.lnetwork(iiiiii))cycle

        if ( (.not. this%file) .and. (.not. c_e(ana_id(i,iiiiii))) ) cycle

                                !>\todo ottimizzare settando e unsettando le cose giuste al posto giusto

        call idba_unsetall (this%handle)
        
        if (this%file)then
          if (present(template)) then
            call idba_set (this%handle,"query","message "//trim(template))
          else
            call idba_set (this%handle,"query","message")
          end if
          call l4f_category_log(this%category,L4F_DEBUG,"chiuso messaggio ")
        end if


        CALL getval(this%vol7d%time(ii), year=year, month=month, day=day, hour=hour, minute=minute)
        call idba_setdate (this%handle,year,month,day,hour,minute,0)

        if (this%file)then
                                ! scrivo su file non posso usare ana_id
          call getval(this%vol7d%ana(i)%coord, lat=lat,lon=lon)
          call idba_set (this%handle,"lat",lat)
          call idba_set (this%handle,"lon",lon)
          call l4f_category_log(this%category,L4F_DEBUG,"dati riferiti a lat: "//to_char(lat)//" lon: "//to_char(lon))

          if (present(ident))then
            if (c_e(ident) .and. ident /= this%vol7d%ana(i)%ident ) cycle
          end if
          
          if ( c_e(this%vol7d%ana(i)%ident)) then
            call idba_set (this%handle,"ident",this%vol7d%ana(i)%ident)
            call idba_set (this%handle,"mobile",1)
            call l4f_category_log(this%category,L4F_DEBUG,"hai una stazione che va a spasso! identificativo: "&
             //to_char(this%vol7d%ana(i)%ident))
          else
            call idba_set (this%handle,"mobile",0)
          end if
        else
          call idba_set (this%handle,"ana_id",ana_id(i,iiiiii))
        end if


        call idba_set (this%handle,"rep_memo",this%vol7d%network(iiiiii)%name)
                 

        do iii=1,nlevel
          if (.not.llevel(iii))cycle

         do iiii=1,ntimerange
            if (.not.ltimerange(iiii))cycle
   
               if (.not. lattr_only) then
                                  

                 call idba_setlevel(this%handle, this%vol7d%level(iii)%level1, this%vol7d%level(iii)%l1,&
                  this%vol7d%level(iii)%level2, this%vol7d%level(iii)%l2)

                 call l4f_category_log(this%category,L4F_DEBUG,"livello1: "//to_char(this%vol7d%level(iii)%level1))
                 call l4f_category_log(this%category,L4F_DEBUG,"l1: "//to_char(this%vol7d%level(iii)%l1))
                 call l4f_category_log(this%category,L4F_DEBUG,"livello2: "//to_char(this%vol7d%level(iii)%level2))
                 call l4f_category_log(this%category,L4F_DEBUG,"l2: "//to_char(this%vol7d%level(iii)%l2))


                 call idba_settimerange(this%handle, this%vol7d%timerange(iiii)%timerange, &
                  this%vol7d%timerange(iiii)%p1, this%vol7d%timerange(iiii)%p2)
                 
                 call l4f_category_log(this%category,L4F_DEBUG,"timerange: "//to_char(this%vol7d%timerange(iiii)%timerange))
                 call l4f_category_log(this%category,L4F_DEBUG,"T1: "//to_char(this%vol7d%timerange(iiii)%p1))
                 call l4f_category_log(this%category,L4F_DEBUG,"T2: "//to_char(this%vol7d%timerange(iiii)%p2))
                 
                 
               end if
               
                                !print *, ">>>>> ",ana_id(i,iiiiii),this%vol7d%network(iiiiii)%name
                                !print *, year,month,day,hour,minute
                                !print *, this%vol7d%level(iii)%level1, this%vol7d%level(iii)%l1, this%vol7d%level(iii)%l2
                                !print *, this%vol7d%timerange(iiii)%timerange,this%vol7d%timerange(iiii)%p1, this%vol7d%timerange(iiii)%p2
               

               write=.false.

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
call l4f_category_log(this%category,L4F_DEBUG,"macro tipo r")
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
call l4f_category_log(this%category,L4F_DEBUG,"macro tipo i")
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
call l4f_category_log(this%category,L4F_DEBUG,"macro tipo b")
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
call l4f_category_log(this%category,L4F_DEBUG,"macro tipo d")
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
call l4f_category_log(this%category,L4F_DEBUG,"macro tipo c")
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
               
               if (write) then
                                !print*,"eseguo una main prendilo"
                 call l4f_category_log(this%category,L4F_DEBUG,"eseguo una main prendilo sui dati")
                 call idba_prendilo (this%handle)

                 do iiiii=1,ndativarr
                   if(c_e(this%vol7d%dativar%r(iiiii)%btable))call idba_unset (this%handle,this%vol7d%dativar%r(iiiii)%btable )
                 end do
                 do iiiii=1,ndativari
                   if(c_e(this%vol7d%dativar%i(iiiii)%btable))call idba_unset (this%handle,this%vol7d%dativar%i(iiiii)%btable )
                 end do
                 do iiiii=1,ndativarb
                   if(c_e(this%vol7d%dativar%b(iiiii)%btable))call idba_unset (this%handle,this%vol7d%dativar%b(iiiii)%btable )
                 end do
                 do iiiii=1,ndativard
                   if(c_e(this%vol7d%dativar%d(iiiii)%btable))call idba_unset (this%handle,this%vol7d%dativar%d(iiiii)%btable )
                 end do
                 do iiiii=1,ndativarc
                   if(c_e(this%vol7d%dativar%c(iiiii)%btable))call idba_unset (this%handle,this%vol7d%dativar%c(iiiii)%btable )
                 end do

                 write=.false.
               end if
                 
            end do
         end do
      end do
   end do
end do

END SUBROUTINE vol7d_dballe_export


!>\brief Cancella l'oggetto

SUBROUTINE vol7d_dballe_delete(this)
TYPE(vol7d_dballe) :: this !< oggetto da cancellare

if (this%file)then

  call idba_fatto(this%handle)
  
else

  call idba_fatto(this%handle)
  call idba_fatto(this%handle_staz)
  call idba_arrivederci(this%idbhandle)

end if

call idba_error_remove_callback(this%handle_err)


!this%dsn=cmiss
!this%user=cmiss
!this%password=cmiss
this%idbhandle=imiss
this%handle=imiss
this%handle_err=imiss
this%handle_staz=imiss

if (associated(this%data_id)) deallocate (this%data_id)

CALL delete(this%vol7d)


!chiudo il logger
call l4f_category_delete(this%category)
!ier=l4f_fini()



END SUBROUTINE vol7d_dballe_delete



subroutine vol7d_dballe_import_dballevar(this)

type(vol7d_var),pointer :: this(:)
type(vol7d_var),allocatable,save :: blocal(:)
INTEGER :: i,un,n

IF (associated(this)) return
IF (allocated(blocal)) then
  ALLOCATE(this(size(blocal)))
  this=blocal
  return
end if

un = open_dballe_file('dballe.txt', filetype_data)
IF (un < 0) then
  print *,"errore open_dballe_file"
                                !>\todo gestione corretta dell'errore
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

  CALL l4f_log(L4F_INFO,'Ho letto '//TRIM(to_char(i-1))//' variabili dalla tabella')

  this=blocal

ENDIF
CLOSE(un)

END SUBROUTINE vol7d_dballe_import_dballevar



!>\brief Integra il vettore delle variabili in vol7d con le descrizioni e le unità di misura
!eventualmente mancanti.

subroutine vol7d_dballe_set_var_du(this)

TYPE(vol7d) :: this !< oggetto vol7d con le variabili da completare
integer :: i,j
type(vol7d_var),pointer :: dballevar(:)


call vol7d_dballe_import_dballevar(dballevar)

#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME dativar


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V

#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME anavar


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V


#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME datiattr


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V


#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME anaattr


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.f90"
#undef VOL7D_POLY_TYPES_V

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



subroutine v7d_dballe_error_handler(category)

integer :: category
character(len=1000) :: message,buf

call idba_error_message(message)
call l4f_category_log(category,L4F_ERROR,message)

call idba_error_context(buf)
call l4f_category_log(category,L4F_DEBUG,trim(buf))

call idba_error_details(buf)
call l4f_category_log(category,L4F_DEBUG,trim(buf))

CALL raise_fatal_error("dballe: "//message)


return

end subroutine v7d_dballe_error_handler



!>\brief Identica a vol7d_dballe_importvvns con lettura da file.
!!
!!import da DB-all.e
SUBROUTINE vol7d_dballe_importvvns_file(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),OPTIONAL :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network,set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)
logical,intent(in),optional :: anaonly

!TYPE(vol7d) :: v7d
CHARACTER(len=SIZE(var)*7) :: varlist
CHARACTER(len=SIZE(attr)*8) :: starvarlist
CHARACTER(len=6) :: btable
CHARACTER(len=7) ::starbtable

LOGICAL ::  ldegnet, lattr, lanaattr,lanaonly
integer :: year,month,day,hour,minute,sec
integer :: rlevel1, rl1,rlevel2, rl2
integer :: rtimerange, p1, p2
character(len=network_name_len) ::rep_memo
integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork


integer :: nana,ntime,ntimerange,nlevel,nnetwork
TYPE(vol7d_var) :: var_tmp

INTEGER :: i,ii, iii,n,n_ana,nn,nvarattr,istat,indattr,na,nd
integer :: nvar ,inddatiattr,inddativarattr
integer :: nanavar ,indanavar,indanaattr,indanavarattr,nanavarattr

REAL(kind=fp_geo) :: lat,lon,latmin,latmax,lonmin,lonmax
CHARACTER(len=vol7d_ana_lenident) :: ident
!INTEGER(kind=int_b)::attrdatib

integer :: ndativarr,     ndativari,     ndativarb,     ndativard,     ndativarc
integer :: ndatiattrr,    ndatiattri,    ndatiattrb,    ndatiattrd,    ndatiattrc 
integer :: ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc

integer :: nanavarr,     nanavari,     nanavarb,     nanavard,     nanavarc
integer :: nanaattrr,    nanaattri,    nanaattrb,    nanaattrd,    nanaattrc 
integer :: nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc

integer :: ir,ib,id,ic

logical :: found
TYPE(datetime) :: timee
TYPE(vol7d_level) :: levele
TYPE(vol7d_timerange) :: timerangee

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
   ldegnet = .TRUE.
   call l4f_category_log(this%category,L4F_INFO,&
    "set_network is not fully implemented in BUFR/CREX import: priority will be ignored")
ELSE
   ldegnet = .FALSE.
ENDIF


if (present(attr) .or. present(anaattr) .or. present(attrkind) .or. present(anaattrkind))then
  call l4f_category_log(this%category,L4F_ERROR,"attributes not managed in BUFR/CREX import")
  CALL raise_error()
end if

call idba_unsetall(this%handle)

N=1
nd=0
na=0

call mem_acquire( buffer,nd,1000,this%category )
call mem_acquire( bufferana,na,100,this%category )


do while ( N > 0 )

  call idba_voglioquesto (this%handle,N)

  call l4f_category_log(this%category,L4F_debug,"numero dati voglioquesto:"//to_char(n))

  ! dammi tutti i dati
  do i=1,N

    call idba_dammelo (this%handle,btable)
  
    call idba_enqdate (this%handle,year,month,day,hour,minute,sec)
    call idba_enqlevel(this%handle, rlevel1, rl1, rlevel2,rl2)
    !TODO
    !dballe BUG: missing viene scritto come 0
    !qui faccio un altibug ma sarà meglio eliminarlo in futuro
    if  (rlevel1 == 0 .or. rlevel1 == 255 )then
      rlevel1=imiss
      rl1=imiss 
    end if
    if  (rlevel2 == 0 .or. rlevel2 == 255 )then
      rlevel2=imiss
      rl2=imiss 
    end if

    call idba_enqtimerange(this%handle, rtimerange, p1, p2)
    call idba_enq(this%handle, "rep_memo",rep_memo)
                                !print *,"trovato network",rep_memo
  
                                !nbtable=btable_numerico(btable)
                                ! ind = firsttrue(qccli%v7d%dativar%r(:)%btable == nbtable)
                                ! IF (ind<1) cycle ! non c'e'

                                !recupero i dati di anagrafica
    call idba_enq (this%handle,"lat",   lat)
    call idba_enq (this%handle,"lon",   lon)
    call idba_enq (this%handle,"ident",ident)
   
    ! inizio la serie dei test con i parametri richiesti 

    if(present(network)) then
      if (rep_memo /= network%name) cycle
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
      CALL geo_coord_to_geo(coordmin)
      CALL getval(coordmin, lat=latmin,lon=lonmin)
      if (lonmin > lon) cycle
      if (latmin > lat) cycle
    end if

    if (present(coordmax)) then
      CALL geo_coord_to_geo(coordmax)
      CALL getval(coordmax, lat=latmax,lon=lonmax)
      if (lonmax < lon) cycle
      if (latmax < lat) cycle
    end if

    call init(timee, year=year, month=month, day=day, hour=hour, minute=minute)

    if (present(timei)) then
      if (timee < timei) cycle
    end if

    if (present(timef)) then
      if (timee > timef) cycle
    end if

    if (present(timerange))then
      call init(timerangee, timerange%timerange, timerange%p1, timerange%p2)
      if (timerangee /= timerange) cycle
    end if

    if (present(level))then
      call  init (levele, rlevel1, rl1,rlevel2, rl2)
      if (levele /= level) cycle
    end if


    if (rlevel1 /= 257)then
      ! dati


      !! TODO attenzione attenzione
      ! qui non si capisce cosa succede
      ! pare che se var viene omessa, pur essendo tutto optional
      ! il test present sia sempre true !!!!!!

      if (present (var)) then
        nvar=size(var)
        found=.false.
        DO ii = 1, nvar
                                !call l4f_category_log(this%category,L4F_DEBUG,"VARIABILI:"//btable//to_char(var(ii)))
          if (btable == var(ii)) found =.true.
        end do
        if (.not. found) cycle
      end if
      
      ! fine test


      nd =nd+1
      call l4f_category_log(this%category,L4F_DEBUG,"numero dati dati:"//to_char(nd)//btable)

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
          if(varkind(ii) == "r") call idba_enq (this%handle,btable,buffer(nd)%dator)
          if(varkind(ii) == "i") call idba_enq (this%handle,btable,buffer(nd)%datoi)
          if(varkind(ii) == "b") call idba_enq (this%handle,btable,buffer(nd)%datob)
          if(varkind(ii) == "d") call idba_enq (this%handle,btable,buffer(nd)%datod)
          if(varkind(ii) == "c") call idba_enq (this%handle,btable,buffer(nd)%datoc)
        end if
      else
        call idba_enq (this%handle,btable,buffer(nd)%datoc) !char is default
      end if
  
                                !bufferizzo il contesto
                                !print *,"lat,lon,ident",lat,lon,ident
                                !print*,year,month,day,hour,minute,sec
                                !print*,btable,dato,buffer(nd)%datiattrb
  
      call init(buffer(nd)%ana,lat=lat,lon=lon,ident=ident)
      call init(buffer(nd)%time, year=year, month=month, day=day, hour=hour, minute=minute)
      call init(buffer(nd)%level, rlevel1,rl1,rlevel2,rl2)
      call init(buffer(nd)%timerange, rtimerange, p1, p2)
      call init(buffer(nd)%network, rep_memo)
      call init(buffer(nd)%dativar, btable)
    
    else

      ! ---------------->   anagrafica

      if (present (anavar)) then
        nanavar=size(anavar)
        found=.false.
        DO ii = 1, nanavar
                                !call l4f_category_log(this%category,L4F_DEBUG,"VARIABILI:"//btable//to_char(var(ii)))
          if (btable == anavar(ii)) found =.true.
        end do
        if (.not. found) cycle
      end if


      !ora legge tutti i dati di anagrafica e li mette in bufferana


      if (.not. lanaonly)then
                                !salto lat lon e ident
        if (btable == "B05001" .or. btable == "B06001" .or. btable == "B01011") cycle

      end if
                                !anno mese giorno
      if (btable == "B04001" .or. btable == "B04002" .or. btable == "B04003") cycle
                                !ora minuti secondi
      if (btable == "B04004" .or. btable == "B04005" .or. btable == "B04006") cycle
                                ! network
      if (btable == "B01193") cycle


      na=na+1
      call l4f_category_log(this%category,L4F_debug,"numero dati ana:"//to_char(na)//btable)

      call mem_acquire( bufferana,na,0,this%category )

      bufferana(na)%dator=DBA_MVR
      bufferana(na)%datoi=DBA_MVI
      bufferana(na)%datob=DBA_MVB
      bufferana(na)%datod=DBA_MVD
      bufferana(na)%datoc=DBA_MVC
      call init(bufferana(na)%dativar, DBA_MVC)

 
      if (present(anavar).and. present(anavarkind))then
        ii=( firsttrue(anavar == btable))
        if (ii > 0)then
                                !print*, "indici",ii, btable,(varkind(ii))
          if(anavarkind(ii) == "r") call idba_enq (this%handle,btable,bufferana(na)%dator)
          if(anavarkind(ii) == "i") call idba_enq (this%handle,btable,bufferana(na)%datoi)
          if(anavarkind(ii) == "b") call idba_enq (this%handle,btable,bufferana(na)%datob)
          if(anavarkind(ii) == "d") call idba_enq (this%handle,btable,bufferana(na)%datod)
          if(anavarkind(ii) == "c") call idba_enq (this%handle,btable,bufferana(na)%datoc)
        end if
      else
        call idba_enq (this%handle,btable,bufferana(na)%datoc) !char is default
                                !print*,"dato anagrafica",btable," ",bufferana(na)%dator
      end if
  
                                !recupero i dati di anagrafica
      call idba_enq (this%handle,"lat",   lat)
      call idba_enq (this%handle,"lon",   lon)
      call idba_enq (this%handle,"ident",ident)
   
                                !bufferizzo il contesto
                                !print *,"lat,lon",lat,lon
                                !print*,year,month,day,hour,minute,sec
                                !print*,btable,na
  
      call init(bufferana(na)%ana,lat=lat,lon=lon,ident=ident)
      call init(bufferana(na)%time, year=year, month=month, day=day, hour=hour, minute=minute)
      call init(bufferana(na)%level, rlevel1,rl1,rlevel2,rl2)
      call init(bufferana(na)%timerange, rtimerange, p1, p2)
      call init(bufferana(na)%network, rep_memo)
      call init(bufferana(na)%dativar, btable)

    end if
  end do
end do

! ---------------->   anagrafica fine

if (.not. present(var))then
  nvar = count_distinct(buffer(:nd)%dativar, back=.TRUE.)
end if

nana = count_distinct(bufferana(:na)%ana, back=.TRUE.)
ntime = count_distinct(buffer(:nd)%time, back=.TRUE.)
ntimerange = count_distinct(buffer(:nd)%timerange, back=.TRUE.)
nlevel = count_distinct(buffer(:nd)%level, back=.TRUE.)
nnetwork = count_distinct(buffer(:nd)%network, back=.TRUE.)
if(ldegnet)nnetwork=1

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
  nanavar = count_distinct(bufferana(:na)%dativar, back=.TRUE.,mask=(bufferana(:na)%dativar%btable /= DBA_MVC))
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


CALL init(vol7dtmp)

!print*,"ho fatto init"


if (lanaonly)then

  ! qui faccio le operazioni minime per avere solo l'anagrafica utile per certe operazioni

  call vol7d_alloc (vol7dtmp, nana=nana)
  vol7dtmp%ana=pack_distinct(bufferana(:na)%ana, nana, back=.TRUE.)

  ! Release memory
  deallocate (buffer)
  deallocate (bufferana)

  ! Se l'oggetto ha gia` un volume allocato lo fondo con quello estratto
  !>\todo manca test su associated dei vol*
  IF (ASSOCIATED(this%vol7d%ana) .AND. ASSOCIATED(this%vol7d%time)) THEN
    CALL vol7d_merge(this%vol7d, vol7dtmp, sort=.TRUE.)
  ELSE ! altrimenti lo assegno
    this%vol7d = vol7dtmp
  ENDIF

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

! print *, "nana=",nana, "ntime=",ntime, "ntimerange=",ntimerange, &
! "nlevel=",nlevel, "nnetwork=",nnetwork, &
! "ndativarr=",ndativarr, "ndativari=",ndativari, &
! "ndativarb=",ndativarb, "ndativard=",ndativard, "ndativarc=",ndativarc,&
! "ndatiattrr=",ndatiattrr, "ndatiattri=",ndatiattri, "ndatiattrb=",ndatiattrb,&
! "ndatiattrd=",ndatiattrd, "ndatiattrc=",ndatiattrc,&
! "ndativarattrr=",ndativarattrr, "ndativarattri=",ndativarattri, "ndativarattrb=",ndativarattrb,&
! "ndativarattrd=",ndativarattrd, "ndativarattrc=",ndativarattrc
! print*,"ho fatto alloc"


vol7dtmp%ana=pack_distinct(bufferana(:na)%ana, nana, back=.TRUE.)
vol7dtmp%time=pack_distinct(buffer(:nd)%time, ntime, back=.TRUE.)
vol7dtmp%timerange=pack_distinct(buffer(:nd)%timerange, ntimerange, back=.TRUE.)
vol7dtmp%level=pack_distinct(buffer(:nd)%level, nlevel, back=.TRUE.)

if(ldegnet)then
  vol7dtmp%network(1)=set_network
else
  vol7dtmp%network=pack_distinct(buffer(:nd)%network, nnetwork, back=.TRUE.)
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

  do i=1, nvar
    call init (vol7dtmp%dativar%c(i), btable=var(i))
  end do

else

  vol7dtmp%dativar%c=pack_distinct(buffer(:nd)%dativar, ndativarc, back=.TRUE.)

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

  do i=1, nanavar
    call init (vol7dtmp%anavar%c(i), btable=anavar(i))
  end do

else

  vol7dtmp%anavar%c=pack_distinct(bufferana(:na)%dativar, nanavarc, back=.TRUE.,&
   mask=(bufferana(:na)%dativar%btable /= DBA_MVC))

end if

!-----------------------> anagrafica fine

!print*,"prima di alloc"

call vol7d_alloc_vol (vol7dtmp)

!print*,"ho fatto un volume vuoto"


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
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%r)
     vol7dtmp%voldatir( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%dator
   end if

   if(c_e(buffer(i)%datoi)) then
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%i)
     vol7dtmp%voldatii( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datoi
   end if

   if(c_e(buffer(i)%datob)) then
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%b)
     vol7dtmp%voldatib( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datob
   end if

   if(c_e(buffer(i)%datod)) then
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%d)
     vol7dtmp%voldatid( &
      indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
      ) = buffer(i)%datod
   end if

   if(c_e(buffer(i)%datoc)) then
     inddativar = firsttrue(buffer(i)%dativar == vol7dtmp%dativar%c)
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
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%r)
     vol7dtmp%volanar( indana,indanavar,indnetwork ) = bufferana(i)%dator
   end if
   if(c_e(bufferana(i)%datoi))then
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%i)
     vol7dtmp%volanai( indana,indanavar,indnetwork ) = bufferana(i)%datoi
   end if
   if(c_e(bufferana(i)%datob))then
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%b)
     vol7dtmp%volanab( indana,indanavar,indnetwork ) = bufferana(i)%datob
   end if
   if(c_e(bufferana(i)%datod))then
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%d)
     vol7dtmp%volanad( indana,indanavar,indnetwork ) = bufferana(i)%datod
   end if
   if(c_e(bufferana(i)%datoc))then
     indanavar = firsttrue(bufferana(i)%dativar == vol7dtmp%anavar%c)
     vol7dtmp%volanac( indana,indanavar,indnetwork ) = bufferana(i)%datoc
   end if

 end do

!------------------------- anagrafica fine

!
! Release memory
!

deallocate (buffer)
deallocate (bufferana)

! Se l'oggetto ha gia` un volume allocato lo fondo con quello estratto
!>\todo manca test su associated dei vol*
IF (ASSOCIATED(this%vol7d%ana) .AND. ASSOCIATED(this%vol7d%time)) THEN
  CALL vol7d_merge(this%vol7d, vol7dtmp, sort=.TRUE.)
ELSE ! altrimenti lo assegno
  this%vol7d = vol7dtmp
ENDIF

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

  ALLOCATE (buffertmp(mem*2),stat=istat)
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


end MODULE vol7d_dballe_class

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


