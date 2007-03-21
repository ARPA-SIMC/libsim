MODULE vol7d_oraclesim_class
USE vol7d_class
USE vol7d_utilities
IMPLICIT NONE

TYPE vol7d_oraclesim
  TYPE(vol7d) :: vol7d
  INTEGER :: ounit
END TYPE vol7d_oraclesim

TYPE ora_var_conv
  INTEGER :: varora
  CHARACTER(len=10) :: varbt
  CHARACTER(len=20) :: unit
  INTEGER :: level(3), timerange(3)
  CHARACTER(len=20) :: description
  REAL :: afact, bfact
  INTEGER :: network
END TYPE ora_var_conv

TYPE ora_ana
  REAL :: lon, lat
  INTEGER :: alt
  INTEGER :: ora_cod
END TYPE ora_ana

TYPE ora_network_conv
  TYPE(ora_ana),POINTER :: ana(:)
END TYPE ora_network_conv

INTEGER,EXTERNAL :: n_getgsta ! da sostituire con include/interface ?!

INTEGER,ALLOCATABLE ::stazo(:), varo(:), valid(:)
REAL,ALLOCATABLE :: valore1(:), valore2(:)
CHARACTER(len=1),ALLOCATABLE :: valore3(:)
CHARACTER(len=12),ALLOCATABLE ::cdatao(:)
INTEGER :: nmax=0, nact=0
INTEGER,PARAMETER :: nmaxmin=100000, nmaxmax=5000000 ,netmax=41
TYPE(ora_var_conv),ALLOCATABLE :: vartable(:)

! Tabella scritta nella roccia, vedi:
!http://spino.metarpa/~patruno/accesso_db_meteo/accesso_db_per_programmatori/node55.html
! aggiunta 12-FIDUTO
TYPE(ora_network_conv) :: networktable(netmax)
!!$TYPE(ora_network_conv) :: networktable(33) = (/ &
!!$ ora_network_conv(01, 'SYNOP', NULL()), ora_network_conv(02, 'TEMPA', NULL()), &
!!$ ora_network_conv(03, 'TEMPB', NULL()), ora_network_conv(04, 'TEMPC', NULL()), &
!!$ ora_network_conv(05, 'TEMPD', NULL()), ora_network_conv(10, 'METAR', NULL()), &
!!$ ora_network_conv(11, 'LOCALI', NULL()), ora_network_conv(12, 'FIDUTO', NULL()), &
!!$ ora_network_conv(13, 'AGRMET', NULL()), &
!!$ ora_network_conv(14, 'POLLINI', NULL()), ora_network_conv(15, 'URBANE', NULL()), &
!!$ ora_network_conv(17, 'FIDUMA', NULL()), ora_network_conv(18, 'FIDUPO', NULL()), &
!!$ ora_network_conv(19, 'ICIRFE', NULL()), ora_network_conv(20, 'SIMNBO', NULL()), &
!!$ ora_network_conv(21, 'SIMNPR', NULL()), ora_network_conv(22, 'SPDSRA', NULL()), &
!!$ ora_network_conv(23, 'EMLOKM', NULL()), ora_network_conv(24, 'LILOKM', NULL()), &
!!$ ora_network_conv(25, 'PILOKM', NULL()), ora_network_conv(26, 'TRLOKM', NULL()), &
!!$ ora_network_conv(27, 'VELOKM', NULL()), ora_network_conv(28, 'SALOKM', NULL()), &
!!$ ora_network_conv(29, 'LOLOKM', NULL()), ora_network_conv(30, 'MALOKM', NULL()), &
!!$ ora_network_conv(31, 'FRLOKM', NULL()), ora_network_conv(35, 'CLIMAT', NULL()), &
!!$ ora_network_conv(36, 'GIAS', NULL()), ora_network_conv(37, 'IDROST', NULL()), &
!!$ ora_network_conv(38, 'VMSTAT', NULL()), ora_network_conv(39, 'IDRMEC', NULL()), &
!!$ ora_network_conv(40, 'CLINUR', NULL()), ora_network_conv(41, 'IDRSTA', NULL()) /)

PRIVATE
PUBLIC vol7d_oraclesim, vol7d_oraclesim_init, vol7d_oraclesim_delete, &
 vol7d_oraclesim_import

INTERFACE init
  MODULE PROCEDURE vol7d_oraclesim_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_oraclesim_delete
END INTERFACE

INTERFACE import
  MODULE PROCEDURE vol7d_oraclesim_import
END INTERFACE


CONTAINS


SUBROUTINE vol7d_oraclesim_init(this)
TYPE(vol7d_oraclesim),INTENT(out) :: this

CALL vol7d_oraclesim_alloc(nmaxmin)
IF (.NOT. ALLOCATED(vartable)) CALL vol7d_oraclesim_setup_conv()
nact = nact + 1 ! Tengo il conto delle istanze attive
this%ounit = -1

END SUBROUTINE vol7d_oraclesim_init


SUBROUTINE vol7d_oraclesim_delete(this)
TYPE(vol7d_oraclesim) :: this

IF (this%ounit >= 0) CALL n_close(this%ounit)
nact = MAX(nact - 1, 0) ! Tengo il conto delle istanze attive
IF (nact == 0) THEN
  CALL vol7d_oraclesim_dealloc()
  DEALLOCATE(vartable) ! eliminare?
ENDIF

END SUBROUTINE vol7d_oraclesim_delete


SUBROUTINE vol7d_oraclesim_import(this, var, network, timei, timef)
TYPE(vol7d_oraclesim),INTENT(out) :: this
CHARACTER(len=*),INTENT(in) :: var
INTEGER,INTENT(in) :: network
TYPE(vol7d_time),INTENT(in) :: timei, timef

INTEGER :: i, j, nvar, nstaz, ntime, nana, nnet, nvout, &
 datai(3), orai(2), dataf(3), oraf(2)
CHARACTER(len=20) :: cnetwork, cvar
INTEGER,ALLOCATABLE :: remapa(:), remapt(:)
TYPE(vol7d_time),ALLOCATABLE :: tmptime(:)

CALL getval(timei, year=datai(3), month=datai(2), day=datai(1), &
 hour=orai(1), minute=orai(2))
CALL getval(timef, year=dataf(3), month=dataf(2), day=dataf(1), &
 hour=oraf(1), minute=oraf(2))
!!$CALL vol7d_datetime_get(timei, year=datai(3), month=datai(2), day=datai(1), &
!!$ hour=orai(1), minute=orai(2))
!!$CALL vol7d_datetime_get(timef, year=dataf(3), month=dataf(2), day=dataf(1), &
!!$ hour=oraf(1), minute=oraf(2))

WRITE(cnetwork,'(I8)')network

! Cerco la rete nella tabella
IF (network <= 0 .OR. network >= netmax ) THEN
  CALL raise_error('rete '//TRIM(cnetwork)//' non valida')
  STOP
ENDIF
! Cerco la variabile per la rete nella tabella
nvar = 0
DO i = 1, SIZE(vartable)
  IF (vartable(i)%varbt == var .AND. vartable(i)%network == network) THEN
    nvar = i
    EXIT
  ENDIF
ENDDO
IF (nvar == 0) THEN
  CALL raise_error('variabile '//TRIM(var)//' non valida per la rete '// &
   TRIM(cnetwork))
  STOP
ENDIF

WRITE(cvar,'(I8)')vartable(nvar)%varora

nvout = 1 ! n. di valori in uscita
CALL n_set_select_mode(-1) ! <0 prolisso, >0 sintetico
nstaz = n_getgsta(this%ounit, TRIM(cnetwork), TRIM(cvar), datai, orai, &
 dataf, oraf, nvout, &
 nmax, cdatao, stazo, varo, valore1, valore2, valore3, valid)
DO WHILE(nstaz > 0 .AND. nstaz == nmax .AND. nmax < nmaxmax)
  CALL vol7d_oraclesim_alloc(MIN(nmax*2, nmaxmax))
  nstaz = n_getgsta(this%ounit, TRIM(cnetwork), TRIM(cvar), datai, orai, &
   dataf, oraf, nvout, &
   nmax, cdatao, stazo, varo, valore1, valore2, valore3, valid)
ENDDO

IF (nstaz < 0) THEN
  CALL raise_error('in estrazione oracle')
  STOP
ENDIF

IF (.NOT. ASSOCIATED(networktable(network)%ana)) THEN
  CALL vol7d_oraclesim_ora_ana(network)
ENDIF

! Alloco lo spazio
CALL init(this%vol7d)
! Per level, timerange, network e var e` facile
CALL vol7d_alloc(this%vol7d, nlevel=1, ntimerange=1, nnetwork=1, ndativarr=1)

CALL init(this%vol7d%level(1), vartable(nvar)%level(1), vartable(nvar)%level(2), &
 vartable(nvar)%level(3))
CALL init(this%vol7d%timerange(1), vartable(nvar)%timerange(1), &
 vartable(nvar)%timerange(2), vartable(nvar)%timerange(3))
CALL init(this%vol7d%network(1), network)
CALL init(this%vol7d%dativar%r(1), var)

! Per ana e time devo contare, ordinare e mappare
ALLOCATE(remapa(nstaz), remapt(nstaz))

ntime = 0
time_ora: DO i = 1, nstaz
  DO j = 1, i-1
    IF (cdatao(i) == cdatao(j)) THEN
      remapt(i) = j ! occhio
      CYCLE time_ora
    ENDIF
  ENDDO
  ntime = ntime + 1
  remapt(i) = ntime
ENDDO time_ora
ALLOCATE(tmptime(ntime))
CALL vol7d_alloc(this%vol7d, ntime=ntime)
DO i = 1, ntime
  CALL init(tmptime(i), oraclesimdate=cdatao(remapt(i)))
ENDDO
DO i = 1, ntime
  remapt(i) = COUNT(tmptime(i) > tmptime(:)) + 1
ENDDO
this%vol7d%time(:) = tmptime(remapt(1:ntime))
DEALLOCATE(tmptime)

! = orig =
!!$ntime = 0
!!$time_orao: DO i = 1, nstaz
!!$  DO j = 1, i-1
!!$    IF (cdatao(i) == cdatao(j)) CYCLE time_orao
!!$  ENDDO
!!$  ntime = ntime + 1
!!$  remapt(ntime) = i
!!$ENDDO time_orao
!!$ALLOCATE(tmptime(ntime))
!!$CALL vol7d_alloc(this%vol7d, ntime=ntime)
!!$DO i = 1, ntime
!!$  CALL init(tmptime(i), time_oraclesim=cdatao(remapt(i)))
!!$ENDDO
!!$DO i = 1, ntime
!!$  remapt(i) = COUNT(tmptime(i) > tmptime(:)) + 1
!!$ENDDO
!!$this%time(:) = tmptime(remapt(1:ntime))
!!$DEALLOCATE(tmptime)
! = orig =

nana = 0
ana_ora: DO i = 1, nstaz
  DO j = 1, i-1
    IF (stazo(i) == stazo(j)) CYCLE ana_ora
  ENDDO
  IF (.NOT. ANY(stazo(i) == networktable(nnet)%ana(:)%ora_cod)) CYCLE ana_ora
  nana = nana + 1
  remapt(nana) = i
ENDDO ana_ora
CALL vol7d_alloc(this%vol7d, nana=nana)
DO i = 1, nana
  j = firsttrue(stazo(i) == networktable(nnet)%ana(:)%ora_cod) ! ottimizzare
  CALL init(this%vol7d%ana(i), lon=REAL(networktable(nnet)%ana(j)%lon,geoprec), &
   lat=REAL(networktable(nnet)%ana(j)%lat,geoprec))
ENDDO

CALL vol7d_alloc_vol(this%vol7d)
DO i = 1, nstaz
  this%vol7d%voldatir(remapa(i),remapt(i),1,1,1,1) = &
   valore1(i)*vartable(nvar)%afact+vartable(nvar)%bfact 
ENDDO
!!$DO j = 1, ntime
!!$  DO i = 1, nana
!!$    this%vol7d%voldatir(i,j,1,1,1,1) = valore !*afact+bfact 
!!$  ENDDO
!!$ENDDO

END SUBROUTINE vol7d_oraclesim_import

!=================
! Routine private
!=================

! Alloca o rialloca i vettori di lavoro per le routine di accesso a oracle
SUBROUTINE vol7d_oraclesim_alloc(n)
INTEGER,INTENT(in) :: n

IF (nmax >= n) RETURN ! c'e' gia' posto sufficiente
CALL vol7d_oraclesim_dealloc
ALLOCATE(stazo(n), varo(n), valid(n), valore1(n), valore2(n), valore3(n), cdatao(n))
nmax = n

END SUBROUTINE vol7d_oraclesim_alloc


! Delloca i vettori di lavoro per le routine di accesso a oracle
! e le anagrafiche eventualemnte lette
SUBROUTINE vol7d_oraclesim_dealloc()

INTEGER :: i

DO i = 1, SIZE(networktable)
  IF (ASSOCIATED(networktable(1)%ana)) DEALLOCATE(networktable(1)%ana)
ENDDO
IF (ALLOCATED(stazo)) DEALLOCATE(stazo, varo, valid, valore1, valore2, valore3, &
 cdatao)
nmax = 0

END SUBROUTINE vol7d_oraclesim_dealloc


! Legge la tabella di conversione per le variabili
SUBROUTINE vol7d_oraclesim_setup_conv()
INTEGER,PARAMETER :: nf=15 ! formato file
INTEGER :: i, sep(nf), n1, n2, un
CHARACTER(len=512) :: line
CHARACTER(len=40) :: net
TYPE(ora_ana),POINTER :: dummy => null() ! temporaneo

un = open_package_file('varmap.csv', filetype_data)
IF (un < 0) STOP

i = 0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  i = i + 1
ENDDO
100 CONTINUE

IF (i > 0) THEN
  IF (ALLOCATED(vartable)) DEALLOCATE(vartable)
  ALLOCATE(vartable(i))
  REWIND(un)
  i = 0
  readline: DO WHILE(.TRUE.)
    READ(un,'(A)',END=120)line
    i = i + 1
    IF (delim_csv(line, sep) < 0) CYCLE readline
    READ(line(sep(1)+1:sep(2)-1),'(I8)')vartable(i)%varora
    READ(line(sep(2)+1:sep(3)-1),'(A)')vartable(i)%varbt
    READ(line(sep(3)+1:sep(4)-1),'(A)')vartable(i)%unit
    READ(line(sep(4)+1:sep(5)-1),'(I8)')vartable(i)%level(1)
    READ(line(sep(5)+1:sep(6)-1),'(I8)')vartable(i)%level(2)
    READ(line(sep(6)+1:sep(7)-1),'(I8)')vartable(i)%level(3)
    READ(line(sep(7)+1:sep(8)-1),'(I8)')vartable(i)%timerange(1)
    READ(line(sep(8)+1:sep(9)-1),'(I8)')vartable(i)%timerange(2)
    READ(line(sep(9)+1:sep(10)-1),'(I8)')vartable(i)%timerange(3)
    READ(line(sep(11)+1:sep(12)-1),'(A)')vartable(i)%description
    READ(line(sep(12)+1:sep(13)-1),'(F10.0)')vartable(i)%afact
    READ(line(sep(13)+1:sep(14)-1),'(F10.0)')vartable(i)%bfact
    READ(line(sep(14)+1:sep(15)-1),'(I8)')vartable(i)%network
  ENDDO readline
120 CONTINUE
ENDIF


END SUBROUTINE vol7d_oraclesim_setup_conv


! Legge l'anagrafica per la rete specificata
SUBROUTINE vol7d_oraclesim_ora_ana(network)
INTEGER,INTENT(in) :: network

INTEGER :: i, j, un
CHARACTER(len=2) :: cnet

WRITE (cnet,"(i2.2)") network
un = open_package_file(cnet//'.ana', filetype_data)
IF (un < 0) STOP

i = 0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  i = i + 1
ENDDO
100 CONTINUE

ALLOCATE(networktable(network)%ana(i))
DO j = 1, SIZE(networktable(network)%ana)
  READ(un,*)networktable(network)%ana(j)%ora_cod, networktable(network)%ana(j)%lat,&
       networktable(network)%ana(j)%lon, networktable(network)%ana(j)%alt
ENDDO
CLOSE(un)

END SUBROUTINE vol7d_oraclesim_ora_ana


END MODULE vol7d_oraclesim_class
