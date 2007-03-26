MODULE vol7d_oraclesim_class
USE char_utilities
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
CALL init(this%vol7d)

END SUBROUTINE vol7d_oraclesim_init


SUBROUTINE vol7d_oraclesim_delete(this)
TYPE(vol7d_oraclesim) :: this

IF (this%ounit >= 0) CALL n_close(this%ounit)
nact = MAX(nact - 1, 0) ! Tengo il conto delle istanze attive
IF (nact == 0) THEN
  CALL vol7d_oraclesim_dealloc()
  DEALLOCATE(vartable)
ENDIF

END SUBROUTINE vol7d_oraclesim_delete


SUBROUTINE vol7d_oraclesim_import(this, var, network, timei, timef, degnet)
TYPE(vol7d_oraclesim),INTENT(out) :: this
CHARACTER(len=*),INTENT(in) :: var
INTEGER,INTENT(in) :: network
TYPE(vol7d_time),INTENT(in) :: timei, timef
LOGICAL,INTENT(in),OPTIONAL :: degnet

TYPE(vol7d) :: v7dtmp
INTEGER :: i, j, nvar, nobs, ntime, nana, nvout, &
 datai(3), orai(2), dataf(3), oraf(2)
CHARACTER(len=8) :: cnetwork, cvar
INTEGER,ALLOCATABLE :: remapa(:), remapai(:), remapt(:), remapti(:)
LOGICAL :: verbose, trovato, ldegnet

IF (PRESENT(degnet)) THEN
  ldegnet = degnet
ELSE
  ldegnet = .FALSE.
ENDIF
CALL getval(timei, year=datai(3), month=datai(2), day=datai(1), &
 hour=orai(1), minute=orai(2))
CALL getval(timef, year=dataf(3), month=dataf(2), day=dataf(1), &
 hour=oraf(1), minute=oraf(2))
CALL getval(verbose=verbose)
IF (verbose) THEN ! <0 prolisso, >0 sintetico
  CALL n_set_select_mode(-1)
ELSE
  CALL n_set_select_mode(1)
ENDIF
cnetwork = TRIM(to_char(network))

! Cerco la rete nella tabella
IF (network <= 0 .OR. network >= netmax ) THEN
  CALL raise_error('rete '//TRIM(cnetwork)//' non valida')
  STOP
ENDIF
! Leggo l'anagrafica per la rete se necessario
IF (.NOT. ASSOCIATED(networktable(network)%ana)) THEN
  CALL vol7d_oraclesim_ora_ana(network)
ENDIF
trovato = .FALSE.
! Cerco la variabile per la rete nella tabella
nvar: DO nvar = 1, SIZE(vartable)
  IF (vartable(nvar)%varbt == var .AND. vartable(nvar)%network == network) THEN
    cvar = TRIM(to_char(vartable(nvar)%varora))
    trovato = .TRUE.

    nvout = 1 ! n. di valori in uscita, raffinare
! Ripeto l'estrazione oracle fino ad essere sicuro di avere
! estratto tutto (nobs < nmax)
    DO WHILE(.TRUE.)
      nobs = n_getgsta(this%ounit, TRIM(cnetwork), TRIM(cvar), datai, orai, &
       dataf, oraf, nvout, &
       nmax, cdatao, stazo, varo, valore1, valore2, valore3, valid)
      IF (verbose) PRINT* ! Termina la riga
      IF (nobs < nmax .OR. nmax >= nmaxmax) EXIT
      CALL print_info('Troppe osservazioni, rialloco ' &
       //TRIM(to_char(MIN(nmax*2, nmaxmax)))//' elementi')
      CALL vol7d_oraclesim_alloc(MIN(nmax*2, nmaxmax))
    ENDDO
    IF (nobs < 0) THEN
      CALL raise_error('in estrazione oracle', nobs)
      STOP
    ELSE
      CALL print_info('Estratte dall''archivio '//TRIM(to_char(nobs)) &
       //' osservazioni')
    ENDIF
    IF (nobs >= nmax) THEN
      CALL raise_warning('troppi dati richiesti, estrazione incompleta')
    ENDIF

! Alloco lo spazio: per level, timerange, network e var e` facile
    CALL init(v7dtmp)
    CALL vol7d_alloc(v7dtmp, nlevel=1, ntimerange=1, nnetwork=1, ndativarr=1)
! inizializzo i descrittori
    CALL init(v7dtmp%level(1), vartable(nvar)%level(1), &
     vartable(nvar)%level(2), vartable(nvar)%level(3))
    CALL init(v7dtmp%timerange(1), vartable(nvar)%timerange(1), &
     vartable(nvar)%timerange(2), vartable(nvar)%timerange(3))
    IF (ldegnet) THEN
      CALL init(v7dtmp%network(1), 0)
    ELSE
      CALL init(v7dtmp%network(1), network)
    ENDIF
    CALL init(v7dtmp%dativar%r(1), var)

! per ana e time devo contare, ordinare e mappare
    ALLOCATE(remapa(nobs), remapai(nobs), remapt(nobs), remapti(nobs))

    nana = 0
    ntime = 0
    remap: DO i = 1, nobs ! ciclo sulle osservazioni ottenute
      DO j = i-1, 1, -1 ! ottimizzo scorrendo all'indietro
        IF (stazo(i) == stazo(j)) THEN ! ho gia` incontrato la stazione?
          remapa(i) = remapa(j) ! si`
          GOTO 20
        ENDIF
      ENDDO
      IF (.NOT. ANY(stazo(i) == networktable(network)%ana(:)%ora_cod)) THEN
        CALL raise_warning('stazione '//TRIM(to_char(stazo(i)))// &
         ' non trovata nell''anagrafica della rete '//TRIM(cnetwork)//', la salto')
        remapa(i) = -1
        remapt(i) = -1
        CYCLE remap
      ENDIF
      nana = nana + 1 ! no
      remapa(i) = nana ! mappatura diretta
      remapai(nana) = i ! mappatura inversa

20    CONTINUE
      DO j = i-1, 1, -1 ! ottimizzo scorrendo all'indietro
        IF (cdatao(i) == cdatao(j)) THEN ! ho gia` incontrato la data?
          remapt(i) = remapt(j) ! si`
          CYCLE remap
        ENDIF
      ENDDO
      ntime = ntime + 1 ! no
      remapt(i) = ntime ! mappatura diretta
      remapti(ntime) = i ! mappatura inversa
    ENDDO remap

! Alloco e riempio il descrittore dell'anagrafica
    CALL vol7d_alloc(v7dtmp, nana=nana)
    DO i = 1, nana
      j = firsttrue(stazo(remapai(i)) == networktable(network)%ana(:)%ora_cod) ! ottimizzare
      IF (j > 0) THEN
        CALL init(v7dtmp%ana(i), &
         lon=REAL(networktable(network)%ana(j)%lon,geoprec), &
         lat=REAL(networktable(network)%ana(j)%lat,geoprec))
      ELSE ! Non dovrebbe mai succedere, eliminare?
        CALL raise_error('errore interno in vol7d_oraclesiom_class')
        STOP
      ENDIF
    ENDDO
! Alloco e riempio riordinando il descrittore del tempo
! Oracle ordina gia', per ora mi risparmio di farlo io
    CALL vol7d_alloc(v7dtmp, ntime=ntime)
    DO i = 1, ntime
      CALL init(v7dtmp%time(i), oraclesimdate=cdatao(remapti(i)))
    ENDDO
! Alloco e riempio il volume di dati
    CALL vol7d_alloc_vol(v7dtmp)
    v7dtmp%voldatir(:,:,:,:,:,:) = rmiss
    DO i = 1, nobs
      IF (remapa(i) == -1) CYCLE
      v7dtmp%voldatir(remapa(i),remapt(i),1,1,1,1) = &
       valore1(i)*vartable(nvar)%afact+vartable(nvar)%bfact 
    ENDDO

    DEALLOCATE (remapa, remapai, remapt, remapti)
! Se l'oggetto ha gia` un volume allocato lo fondo con quello estratto
    IF (ASSOCIATED(this%vol7d%ana) .AND. ASSOCIATED(this%vol7d%time) .AND. &
     ASSOCIATED(this%vol7d%voldatir)) THEN
      CALL vol7d_merge(this%vol7d, v7dtmp, sort=.TRUE.)
    ELSE
      this%vol7d = v7dtmp
    ENDIF

  ENDIF
ENDDO nvar
IF (.NOT.trovato) THEN
  CALL raise_error('variabile '//TRIM(var)//' non valida per la rete '// &
   TRIM(cnetwork))
  STOP
ENDIF

END SUBROUTINE vol7d_oraclesim_import

!=================
! Routine private
!=================

! Alloca o rialloca i vettori di lavoro per le routine di accesso a oracle
SUBROUTINE vol7d_oraclesim_alloc(n)
INTEGER,INTENT(in) :: n

IF (nmax >= n) RETURN ! c'e' gia' posto sufficiente
CALL vol7d_oraclesim_dealloc
ALLOCATE(stazo(n), varo(n), valid(n), valore1(n), valore2(n), valore3(n), &
 cdatao(n))

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

  CALL print_info('Ho letto '//TRIM(to_char(i))//' variabili dalla tabella')
ENDIF

END SUBROUTINE vol7d_oraclesim_setup_conv


! Legge l'anagrafica per la rete specificata
SUBROUTINE vol7d_oraclesim_ora_ana(network)
INTEGER,INTENT(in) :: network

INTEGER :: i, j, un
CHARACTER(len=2) :: cnet

cnet = to_char(network,'(I2.2)')
un = open_package_file(cnet//'.ana', filetype_data)
IF (un < 0) STOP

i = 0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  i = i + 1
ENDDO
100 CONTINUE
REWIND(un)
ALLOCATE(networktable(network)%ana(i))

DO j = 1, i
  READ(un,*)networktable(network)%ana(j)%ora_cod, &
   networktable(network)%ana(j)%lat, networktable(network)%ana(j)%lon, &
   networktable(network)%ana(j)%alt
ENDDO
CLOSE(un)

CALL print_info('Ho letto l''anagrafica di '//TRIM(to_char(i))// &
 ' stazioni per la rete '//cnet)

END SUBROUTINE vol7d_oraclesim_ora_ana


END MODULE vol7d_oraclesim_class
