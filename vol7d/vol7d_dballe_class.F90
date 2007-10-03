MODULE vol7d_dballe_class

!omstart vol7d_dballe_class
!idx classe per l'import ed export di volumi da e in DB-All.e 
!Questo modulo definisce gli oggetti e i metodi per gestire
!l'importazione e l'esportazione di volumi dal database per dati sparsi
!DB-All.e
!

!L'oggetto principale definito dalla classe è:
!
!vol7d_dballe
!l'oggetto è costituito da un oggetto vol7d attorniato dalle 
!informazioni necessarie per l'accesso al DSN di DB-All.e
! e da una matrice necessaria per l'ottimizzazione della scrittura dei 
!dati in export
!omend


USE char_utilities
USE vol7d_class
USE vol7d_utilities

IMPLICIT NONE

include "dballef.h"

TYPE vol7d_dballe
  TYPE(vol7d) :: vol7d
  !character(len=19) :: dsn,user,password
  integer :: idbhandle,handle,handle_staz,handle_err
  integer :: debug = 1
  integer ,pointer :: data_id(:,:,:,:,:)

END TYPE vol7d_dballe



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


PRIVATE
PUBLIC vol7d_dballe, init, delete, import, export

INTERFACE init
  MODULE PROCEDURE vol7d_dballe_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_dballe_delete
END INTERFACE

INTERFACE import
  MODULE PROCEDURE vol7d_dballe_importvsns, vol7d_dballe_importvvns, &
   vol7d_dballe_importvsnv, vol7d_dballe_importvvnv
END INTERFACE

INTERFACE export
  MODULE PROCEDURE vol7d_dballe_export
END INTERFACE

CONTAINS


SUBROUTINE vol7d_dballe_init(this,dsn,user,password,debug,write,wipe)

!omstart init(this,dsn,user,password,debug,write,wipe)
!idx classe per l'import ed export di volumi da e in DB-All.e 
!
!TYPE(vol7d_dballe),INTENT(out) :: this
!                                  l'oggetto da inizializzare
!character(len=*), INTENT(in),OPTIONAL :: dsn,user,password
!                                  parametri per l'accesso al DSN di DB-All.e
!integer,INTENT(in),OPTIONAL :: debug
!                                  attiva alcune opzioni di debug
!                                  default=1
!logical,INTENT(in),OPTIONAL :: write
!                                  abilita la scrittura sul DSN
!                                  default=.false.
!logical,INTENT(in),OPTIONAL :: wipe
!                                  svuota il DSN e lo prepara per una scrittura
!                                  default=.false.
!omend

TYPE(vol7d_dballe),INTENT(out) :: this
character(len=*), INTENT(in),OPTIONAL :: dsn,user,password
integer,INTENT(in),OPTIONAL :: debug
integer :: ldebug
character(len=50) :: quidsn,quiuser,quipassword
logical,INTENT(in),OPTIONAL :: wipe,write
logical :: quiwrite,quiwipe


IF (PRESENT(debug)) THEN
  ldebug = debug
ELSE
  ldebug = 1
ENDIF

quidsn = "test"
quiuser = "test"
quipassword = ""

IF (PRESENT(dsn))quidsn = dsn
IF (PRESENT(user))quiuser = user
IF (PRESENT(password))quipassword = password

! utilizziamo la routine di default per la gestione dell'errore
call idba_error_set_callback(0,idba_default_error_handler, &
     ldebug,this%handle_err)

!TODO: quando scrivo bisogna gestire questo che non è da fare ?
CALL init(this%vol7d)

quiwrite=.false.
if (present(write))then
   quiwrite=write
endif

quiwipe=.false.
if (present(wipe))then
   quiwipe=wipe
endif

!print*,"write=",quiwrite,"wipe=",wipe,"dsn=",quidsn

if(quiwrite)then
   call idba_presentati(this%idbhandle,quidsn,quiuser,quipassword)
   call idba_preparati (this%idbhandle,this%handle,"write","write","write")
   call idba_preparati (this%idbhandle,this%handle_staz,"write","write","write")
else
   call idba_presentati(this%idbhandle,quidsn,quiuser,quipassword)
   call idba_preparati (this%idbhandle,this%handle,"read","read","read")
   call idba_preparati (this%idbhandle,this%handle_staz,"read","read","read")
end if

if (quiwipe)call idba_scopa (this%handle,"")

END SUBROUTINE vol7d_dballe_init

!omstart import(this, var, network, coordmin,coordmax, timei, timef, level,timerange,set_network, attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
!
!idx importa un volume dati da un DSN DB_all.e
!
!TYPE(vol7d_dballe),INTENT(out) :: this
!                               oggetto vol7d_dballe
!CHARACTER(len=*),INTENT(in) :: var
!                               variabili da importare secondo la 
!                               tabella B locale o relativi alias
!INTEGER,INTENT(in) :: network
!                               codici dei network da importare
!TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
!                               coordinate minime e massime che definiscono il 
!                               rettangolo di estrazione per l'importazione
!TYPE(datetime),INTENT(in) :: timei, timef
!                               estremi temporali dell'estrazione per l'importazione
!TYPE(vol7d_level),INTENT(in),optional :: level
!                               livello selezionato per l'estrazione
!TYPE(vol7d_timerange),INTENT(in),optional :: timerange
!                               timerange selezionato per l'importazione
!TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
!                               estrae i dati migliori disponibili "mergiandoli" in un'unica rete 
!                               definita da questo parametro
!                               ANCORA DA TESTARE !!!!
!CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
!CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)

!omend

SUBROUTINE vol7d_dballe_importvsns(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
TYPE(vol7d_dballe),INTENT(out) :: this
CHARACTER(len=*),INTENT(in),optional :: var
TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network,set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)

CALL import(this, (/var/), network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)

END SUBROUTINE vol7d_dballe_importvsns


SUBROUTINE vol7d_dballe_importvsnv(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
TYPE(vol7d_dballe),INTENT(out) :: this
CHARACTER(len=*),INTENT(in),optional :: var
TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network(:),set_network
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


SUBROUTINE vol7d_dballe_importvvnv(this, var, network, coordmin,coordmax, timei, timef, level,timerange,set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)
TYPE(vol7d_dballe),INTENT(out) :: this
CHARACTER(len=*),INTENT(in),optional :: var(:)
TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network(:),set_network
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


SUBROUTINE vol7d_dballe_importvvns(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind)

TYPE(vol7d_dballe),INTENT(inout) :: this
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:)
TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
TYPE(datetime),INTENT(in),OPTIONAL :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network,set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)

TYPE(vol7d) :: v7d
CHARACTER(len=SIZE(var)*7) :: varlist
CHARACTER(len=SIZE(attr)*8) :: starvarlist
CHARACTER(len=6) :: btable
CHARACTER(len=7) ::starbtable

LOGICAL ::  ldegnet, lattr, lanaattr
integer :: year,month,day,hour,minute,sec
integer :: rlevel, rl1, rl2
integer :: rtimerange, p1, p2,rep_cod
integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork


integer :: nana,ntime,ntimerange,nlevel,nnetwork
TYPE(vol7d_var) :: var_tmp

INTEGER :: i,ii, iii,j, k,n,n_ana,nn,nvarattr,istat,ana_id,ana_id_staz,nstaz,ist,indattr
integer :: nvar ,inddatiattr,inddativarattr
integer :: nanavar ,indanavar,indanaattr,indanavarattr,nanavarattr

REAL(kind=fp_geo) :: lat,lon
CHARACTER(len=vol7d_ana_lenident) :: ident
INTEGER(kind=int_b)::attrdatib

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

if(present(network))call idba_set (this%handle,"rep_cod",network%id)
call idba_set (this%handle,"mobile",0)
!print*,"network,mobile",network%id,0

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
   call idba_setlevel(this%handle, level%level, level%l1, level%l2)
end if

call idba_voglioquesto (this%handle,N)
!print*,"numero di dati ",N

!ora che so quanti dati ho alloco la memoria per buffer
allocate(buffer(N),stat=istat)
if (istat/= 0) CALL raise_error('errore allocazione memoria')


! dammi tutti i dati
do i=1,N

  call idba_dammelo (this%handle,btable)
  
  call idba_enqdate (this%handle,year,month,day,hour,minute,sec)
  call idba_enqlevel(this%handle, rlevel, rl1, rl2)
  call idba_enqtimerange(this%handle, rtimerange, p1, p2)
  call idba_enq(this%handle, "rep_cod",rep_cod)
                                !print *,"trovato network",rep_cod
  
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
  call init(buffer(i)%level, rlevel,rl1,rl2)
  call init(buffer(i)%timerange, rtimerange, p1, p2)
  call init(buffer(i)%network, rep_cod)
  call init(buffer(i)%dativar, btable)

end do

! ---------------->   anagrafica

!ora legge tutti i dati di anagrafica e li mette in bufferana

call idba_unsetall(this%handle_staz)

if(present(network))call idba_set (this%handle_staz,"rep_cod",network%id)
call idba_set (this%handle_staz,"mobile",0)
!print*,"network,mobile",network%id,0

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
if (istat/= 0) CALL raise_error('errore allocazione memoria anagrafica')

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
  call idba_enqlevel(this%handle_staz, rlevel, rl1, rl2)
  call idba_enqtimerange(this%handle_staz, rtimerange, p1, p2)
  call idba_enq(this%handle_staz, "rep_cod",rep_cod)
                                !print *,"trovato network",rep_cod
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
  call init(bufferana(i)%level, rlevel,rl1,rl2)
  call init(bufferana(i)%timerange, rtimerange, p1, p2)
  call init(bufferana(i)%network, rep_cod)
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
 ndativarattrr=ndativarattrr, ndativarattri=ndativarattri, ndativarattrb=ndativarattrb, ndativarattrd=ndativarattrd, ndativarattrc=ndativarattrc,&
 nanavarr=nanavarr, nanavari=nanavari, nanavarb=nanavarb, nanavard=nanavard, nanavarc=nanavarc,&
 nanaattrr=nanaattrr, nanaattri=nanaattri, nanaattrb=nanaattrb, nanaattrd=nanaattrd, nanaattrc=nanaattrc,&
 nanavarattrr=nanavarattrr, nanavarattri=nanavarattri, nanavarattrb=nanavarattrb, nanavarattrd=nanavarattrd, nanavarattrc=nanavarattrc)

 !print *, "nana=",nana, "ntime=",ntime, "ntimerange=",ntimerange, &
 !"nlevel=",nlevel, "nnetwork=",nnetwork, &
 !"ndativarr=",ndativarr, "ndativari=",ndativari, "ndativarb=",ndativarb, "ndativard=",ndativard, "ndativarc=",ndativarc,&
 !"ndatiattrr=",ndatiattrr, "ndatiattri=",ndatiattri, "ndatiattrb=",ndatiattrb, "ndatiattrd=",ndatiattrd, "ndatiattrc=",ndatiattrc,&
 !"ndativarattrr=",ndativarattrr, "ndativarattri=",ndativarattri, "ndativarattrb=",ndativarattrb, "ndativarattrd=",ndativarattrd, "ndativarattrc=",ndativarattrc
 !print*,"ho fatto alloc"


vol7dtmp%ana=pack_distinct(buffer%ana, back=.TRUE.)
vol7dtmp%time=pack_distinct(buffer%time, back=.TRUE.)
vol7dtmp%timerange=pack_distinct(buffer%timerange, back=.TRUE.)
vol7dtmp%level=pack_distinct(buffer%level, back=.TRUE.)

if(ldegnet)then
  vol7dtmp%network(1)=set_network
else
  vol7dtmp%network=pack_distinct(buffer%network, back=.TRUE.)
end if

!print*,"reti presenti", vol7dtmp%network%id,buffer%network%id

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

  vol7dtmp%dativar%c=pack_distinct(buffer%dativar, back=.TRUE.)

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

else

      vol7dtmp%dativarattr%c=vol7dtmp%dativar%c

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

  vol7dtmp%anavar%c=pack_distinct(bufferana%dativar, back=.TRUE.,mask=(bufferana%dativar%btable /= DBA_MVC))

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

else

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
  if (istat/= 0) CALL raise_error('errore allocazione memoria')
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

!TODO sostituire qui sotto con struttura case:
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
!TODO manca test su associated dei vol*
IF (ASSOCIATED(this%vol7d%ana) .AND. ASSOCIATED(this%vol7d%time)) THEN
  CALL vol7d_merge(this%vol7d, vol7dtmp, sort=.TRUE.)
ELSE ! altrimenti lo assegno
  this%vol7d = vol7dtmp
ENDIF

call vol7d_set_attr_ind(this%vol7d)

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


END SUBROUTINE vol7d_dballe_importvvns


SUBROUTINE vol7d_dballe_export(this, network, coordmin, coordmax, staz_id, ident,&
 timei, timef,level,timerange,var,attr,anavar,anaattr,attr_only)

! TODO: gestire staz_id la qual cosa vuol dire aggiungere un id nel type ana

TYPE(vol7d_dballe),INTENT(in) :: this
INTEGER,INTENT(in),optional :: network,staz_id
CHARACTER(len=vol7d_ana_lenident),optional :: ident
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
REAL(kind=fp_geo) :: latmin,latmax,lonmin,lonmax
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:),attr(:),anavar(:),anaattr(:)
logical,intent(in),optional :: attr_only
logical, allocatable :: lnetwork(:),llevel(:),ltimerange(:)
integer,allocatable :: ana_id(:,:)
logical :: write,writeattr,lattr_only

!CHARACTER(len=6) :: btable
!CHARACTER(len=7) ::starbtable

integer :: year,month,day,hour,minute,sec
integer :: nstaz,ntime,ntimerange,nlevel,nnetwork


INTEGER :: i,ii,iii,iiii,iiiii,iiiiii,j,ind,inddatiattr,indanaattr

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


nstaz=size(this%vol7d%ana(:))

ntimerange=size(this%vol7d%timerange(:))
allocate (ltimerange(ntimerange))

if (present(timerange))then
      where (timerange == this%vol7d%timerange(:))
         ltimerange(:)=.true.
      end where
else
   ltimerange(:)=.true.
end if

nlevel=size(this%vol7d%level(:))
allocate (llevel(nlevel))

if (present(level))then
      where (level == this%vol7d%level(:))
         llevel(:)=.true.
      end where
else
   llevel(:)=.true.
end if

if (present(attr_only))then
  lattr_only=attr_only
else
  lattr_only=.false.
end if

if ( .not. allocated(this%data_id))then
  lattr_only=.false.
end if


nnetwork=size(this%vol7d%network(:))
ntime=size(this%vol7d%time(:))

allocate (lnetwork(nnetwork))
allocate (ana_id(nstaz,nnetwork))

if (present(network))then
      where (network == this%vol7d%network(:)%id)
         lnetwork(:)=.true.
      end where
else
   lnetwork(:)=.true.
end if



!!!!!  anagrafica


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_nana.f90"
#undef VOL7D_POLY_TYPES_V


!!!!!!!   dati

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_ndati.f90"
#undef VOL7D_POLY_TYPES_V


call idba_unsetall (this%handle)
     
! vital statistics data
call idba_setcontextana (this%handle)

do iii=1, nnetwork
   if (.not.lnetwork(iii))cycle

   do i=1, nstaz

      ana_id(i,iii)=DBA_MVI

      if (present(coordmin).and.present(coordmax))then

        if (.not. inside(this%vol7d%ana(i)%coord,coordmin,coordmax)) cycle
                                !print * ,"sei dentro, OK"
      end if

      CALL geo_coord_to_geo(this%vol7d%ana(i)%coord)
      CALL getval(this%vol7d%ana(i)%coord, lat=lat,lon=lon)
      !print *,lat,lon

      call idba_set (this%handle,"lat",lat)
      call idba_set (this%handle,"lon",lon)

      if (present(ident))then
         if (c_e(ident) .and. ident /= this%vol7d%ana(i)%ident ) cycle
      end if

      if ( c_e(this%vol7d%ana(i)%ident)) then
         call idba_set (this%handle,"ident",ident)
         call idba_set (this%handle,"mobile",1)
         print *, "ti piace andare a spasso he ...",ident
      else
         call idba_set (this%handle,"mobile",0)
      end if


      call idba_set(this%handle,"rep_cod",this%vol7d%network(iii)%id)
                                !print *,"network",this%vol7d%network(iii)%id


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
                                !print*,"eseguo una main prendilo"
        call idba_prendilo (this%handle)
        call idba_enq (this%handle,"ana_id",ana_id(i,iii))
        write=.false.
      end if

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
      do iii=1,nlevel
         if (.not.llevel(iii))cycle
         do iiii=1,ntimerange
            if (.not.ltimerange(iiii))cycle
            do iiiiii=1, nnetwork
               if (.not.lnetwork(iiiiii))cycle

               if (.not. c_e(ana_id(i,iiiiii))) cycle

               call idba_unsetall (this%handle)

               if (.not. lattr_only) then
                                !TODO: ottimizzare settando e unsettando le cose giuste al posto giusto
                 
                 call idba_set (this%handle,"ana_id",ana_id(i,iiiiii))
                 call idba_set (this%handle,"rep_cod",this%vol7d%network(iiiiii)%id)
                 call idba_setlevel(this%handle, this%vol7d%level(iii)%level, this%vol7d%level(iii)%l1, this%vol7d%level(iii)%l2)
                 call idba_settimerange(this%handle, this%vol7d%timerange(iiii)%timerange, &
                  this%vol7d%timerange(iiii)%p1, this%vol7d%timerange(iiii)%p2)
                 
                 CALL getval(this%vol7d%time(ii), year=year, month=month, day=day, hour=hour, minute=minute)
                 call idba_setdate (this%handle,year,month,day,hour,minute,0)
                 
               end if
               
                                !print *, ">>>>> ",ana_id(i,iiiiii),this%vol7d%network(iiiiii)%id
                                !print *, year,month,day,hour,minute
                                !print *, this%vol7d%level(iii)%level, this%vol7d%level(iii)%l1, this%vol7d%level(iii)%l2
                                !print *, this%vol7d%timerange(iiii)%timerange,this%vol7d%timerange(iiii)%p1, this%vol7d%timerange(iiii)%p2
               

               write=.false.

#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
!print*,"macro tipo r"
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
!print*,"macro tipo i"
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
!print*,"macro tipo b"
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
!print*,"macro tipo d"
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
!print*,"macro tipo c"
#include "vol7d_dballe_class_dati.F90"
#undef VOL7D_POLY_TYPES_V
               
               if (write) then
                                !print*,"eseguo una main prendilo"
                 call idba_prendilo (this%handle)
                 write=.false.
               end if
                 
            end do
         end do
      end do
   end do
end do

END SUBROUTINE vol7d_dballe_export


SUBROUTINE vol7d_dballe_delete(this)
TYPE(vol7d_dballe) :: this

call idba_fatto(this%handle)
call idba_fatto(this%handle_staz)
call idba_arrivederci(this%idbhandle)

!this%dsn=cmiss
!this%user=cmiss
!this%password=cmiss
this%idbhandle=imiss
this%handle=imiss
this%handle_err=imiss
this%handle_staz=imiss

deallocate (this%data_id)

CALL delete(this%vol7d)

END SUBROUTINE vol7d_dballe_delete


END MODULE 
