!> \brief Estensione di vol7d_class per importare volumi di dati
!! dall'archivio Oracle del SIM.
!!
!! Questo modulo definisce gli oggetti e i metodi per importare
!! un volume di dati dall'archivio Oracle del SIM in un oggetto di tipo
!! vol7d_class::vol7d.
!!
!! \todo terminare la procedura per creare i file di anagrafica direttamente
!! dall'archivio Oracle (Andrea Selvini) e offrire la possibilità di importare
!! (nella ::import) anche variabili di anagrafica come la quota della stazione.
!!
!! \ingroup vol7d
MODULE vol7d_oraclesim_class
USE char_utilities
USE vol7d_class
USE vol7d_utilities
IMPLICIT NONE

!> Oggetto usato per interfacciarsi all'archivio.
!! Estende vol7d_class::vol7d aggiungendo le informazioni necessarie
!! all'estrazione. L'utente, pur potendo accedere a tutti i componenti
!! dell'oggetto, dovrà preoccuparsi del solo componente vol7d.
TYPE vol7d_oraclesim
  TYPE(vol7d) :: vol7d !< oggetto di tipo vol7d che conterrà i dati estratti
  INTEGER :: ounit !< informazione di servizio
END TYPE vol7d_oraclesim

TYPE ora_var_conv
  INTEGER :: varora
  CHARACTER(len=10) :: varbt
  CHARACTER(len=20) :: unit
  TYPE(vol7d_level) :: level
  TYPE(vol7d_timerange) :: timerange
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
!http://spino.metarpa/~patruno/accesso_db_meteo/accesso_db_per_programmatori/node55.html

INTEGER,ALLOCATABLE ::stazo(:), varo(:), valid(:)
REAL,ALLOCATABLE :: valore1(:), valore2(:)
CHARACTER(len=1),ALLOCATABLE :: valore3(:)
CHARACTER(len=12),ALLOCATABLE ::cdatao(:)
INTEGER :: nmax=0, nact=0
INTEGER,PARAMETER :: nmaxmin=100000, nmaxmax=5000000 ,netmax=41
! tabella di conversione variabili da btable a oraclesim
TYPE(ora_var_conv),ALLOCATABLE :: vartable(:)
! tabella reti e anagrafica
TYPE(ora_network_conv) :: networktable(netmax)

PRIVATE
PUBLIC vol7d_oraclesim, vol7d_oraclesim_init, vol7d_oraclesim_delete, &
 import

!> Costruttore per la classe vol7d_oraclesim.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE vol7d_oraclesim_init
END INTERFACE

!> Distruttore per la classe vol7d_oraclesim.
INTERFACE delete
  MODULE PROCEDURE vol7d_oraclesim_delete
END INTERFACE

!> Metodi di importazione dati dall'archivio Oracle.
INTERFACE import
  MODULE PROCEDURE vol7d_oraclesim_importvsns, vol7d_oraclesim_importvvns, &
   vol7d_oraclesim_importvsnv, vol7d_oraclesim_importvvnv
END INTERFACE

CONTAINS


!> Inizializza un oggetto di tipo vol7doraclesim.
!! Trattandosi di un'estensione di vol7d, provvede ad inizializzare
!! anche l'oggetto vol7d contenuto.
!! Riceve parametri opzionali per pura compatibilità
!! con il corrispondente metodo di vol7d_dballe_class, ma essi sono ininfluenti.
!! Alla prima chiamata in un programma, provvede anche ad importare
!! le tabelle di conversione variabili dal file vartab.csv.
SUBROUTINE vol7d_oraclesim_init(this, dsn, user, password, write, wipe)
TYPE(vol7d_oraclesim),INTENT(out) :: this
CHARACTER(len=*), INTENT(in),OPTIONAL :: dsn, user, password
LOGICAL,INTENT(in),OPTIONAL :: wipe, WRITE

CALL vol7d_oraclesim_alloc(nmaxmin)
IF (.NOT. ALLOCATED(vartable)) CALL vol7d_oraclesim_setup_conv()
nact = nact + 1 ! Tengo il conto delle istanze attive
this%ounit = -1
CALL init(this%vol7d)

END SUBROUTINE vol7d_oraclesim_init


!> Distrugge l'oggetto in maniera pulita.
!! Trattandosi di un'estensione di vol7d, provvede a distruggere
!! anche l'oggetto vol7d contenuto.
SUBROUTINE vol7d_oraclesim_delete(this)
TYPE(vol7d_oraclesim) :: this

IF (this%ounit >= 0) CALL n_close(this%ounit)
nact = MAX(nact - 1, 0) ! Tengo il conto delle istanze attive
IF (nact == 0) THEN
  CALL vol7d_oraclesim_dealloc()
  DEALLOCATE(vartable)
ENDIF

END SUBROUTINE vol7d_oraclesim_delete


SUBROUTINE vol7d_oraclesim_importvsns(this, var, network, timei, timef, level, &
 timerange, set_network)
TYPE(vol7d_oraclesim),INTENT(out) :: this
CHARACTER(len=*),INTENT(in) :: var
INTEGER,INTENT(in) :: network
TYPE(datetime),INTENT(in) :: timei, timef
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network

CALL import(this, (/var/), network, timei, timef, level, timerange, set_network)

END SUBROUTINE vol7d_oraclesim_importvsns


SUBROUTINE vol7d_oraclesim_importvsnv(this, var, network, timei, timef, level, &
 timerange, set_network)
TYPE(vol7d_oraclesim),INTENT(out) :: this
CHARACTER(len=*),INTENT(in) :: var
INTEGER,INTENT(in) :: network(:)
TYPE(datetime),INTENT(in) :: timei, timef
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network

INTEGER :: i

DO i = 1, SIZE(network)
  CALL import(this, (/var/), network(i), timei, timef, level, timerange, &
   set_network)
ENDDO

END SUBROUTINE vol7d_oraclesim_importvsnv


!> Importa un volume vol7d dall'archivio Oracle SIM.
!! Attualmente l'importazione crea solo un volume di dati reali
!! vol7d_class::vol7d::voldatir. Tutti i descrittori vengono assegnati
!! correttamente, compresa l'anagrafica delle stazioni.
SUBROUTINE vol7d_oraclesim_importvvnv(this, var, network, timei, timef, level, &
 timerange, set_network)
TYPE(vol7d_oraclesim),INTENT(out) :: this !< oggetto in cui importare i dati
CHARACTER(len=*),INTENT(in) :: var(:) !< lista delle variabili da importare (codice alfanumerico della tabella B del WMO)
INTEGER,INTENT(in) :: network(:) !< indicativo numerico della rete nell'archivio SIM
TYPE(datetime),INTENT(in) :: timei !< istante iniziale delle osservazionida estrarre (estremo incluso)
TYPE(datetime),INTENT(in) :: timef !< istante finale delle osservazionida estrarre (estremo incluso)
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level !< estrae solo il livello verticale fornito, default=tutti
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange !< estrae solo i dati con intervallo temporale (es. istantaneo, cumulato, ecc.) analogo al timerange fornito, default=tutti
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network !< se fornito, collassa tutte le reti nell'unica rete indicata, eliminando le stazioni comuni a reti diverse

INTEGER :: i

DO i = 1, SIZE(network)
  CALL import(this, var, network(i), timei, timef, level, timerange, set_network)
ENDDO

END SUBROUTINE vol7d_oraclesim_importvvnv


SUBROUTINE vol7d_oraclesim_importvvns(this, var, network, timei, timef, level, &
 timerange, set_network)
TYPE(vol7d_oraclesim),INTENT(out) :: this
CHARACTER(len=*),INTENT(in) :: var(:)
INTEGER,INTENT(in) :: network
TYPE(datetime),INTENT(in) :: timei, timef
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network

TYPE(vol7d) :: v7dtmp, v7dtmp2
TYPE(datetime) :: odatetime
INTEGER :: i, j, k, nvar, nobs, ntime, nana, nvout, nvin, nvbt, &
 datai(3), orai(2), dataf(3), oraf(2), verbose
CHARACTER(len=8) :: cnetwork
CHARACTER(len=SIZE(var)*16) :: cvar
CHARACTER(len=12),ALLOCATABLE :: tmtmp(:)
INTEGER,ALLOCATABLE :: anatmp(:), vartmp(:), mapdatao(:)
LOGICAL :: found, non_valid, varbt_req(SIZE(vartable))

CALL getval(timei, year=datai(3), month=datai(2), day=datai(1), &
 hour=orai(1), minute=orai(2))
CALL getval(timef, year=dataf(3), month=dataf(2), day=dataf(1), &
 hour=oraf(1), minute=oraf(2))
CALL eh_getval(verbose=verbose)
IF (verbose >= eh_verbose_info) THEN ! <0 prolisso, >0 sintetico
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
! Conto le variabili da estrarre
nvar = 0
varbt_req(:) = .FALSE.
DO nvin = 1, SIZE(var)
  found = .FALSE.
  DO nvbt = 1, SIZE(vartable)
    IF (vartable(nvbt)%varbt == var(nvin) .AND. &
     vartable(nvbt)%network == network) THEN

      IF (PRESENT(level))THEN
        IF (vartable(nvbt)%level /= level) CYCLE
      END IF

      IF (PRESENT(timerange))THEN
        IF (vartable(nvbt)%timerange /= timerange) CYCLE
      END IF

      found = .TRUE.
      nvar = nvar + 1
      varbt_req(nvbt) = .TRUE.
    ENDIF
  ENDDO
  IF (.NOT.found) CALL raise_warning('variabile '//TRIM(var(nvin))// &
   ' non valida per la rete '//TRIM(cnetwork))
ENDDO
IF (nvar == 0) THEN
  CALL raise_error('nessuna delle variabili '//TRIM(var(1))// &
   ' e` valida per la rete '//TRIM(cnetwork))
  RETURN
ENDIF
! Mappo le variabili da btable a oraclesim e creo la stringa con l'elenco
nvar = 0
cvar = ''
DO nvin = 1, SIZE(var)
  DO nvbt = 1, SIZE(vartable)
    IF (vartable(nvbt)%varbt == var(nvin) .AND. &
     vartable(nvbt)%network == network) THEN
      nvar = nvar + 1

      IF (PRESENT(level))THEN
        IF (vartable(nvbt)%level /= level) CYCLE
      END IF

      IF (PRESENT(timerange))THEN
        IF (vartable(nvbt)%timerange /= timerange) CYCLE
      END IF

! Controllare di non eccedere cvar????
      IF (nvar > 1) cvar(LEN_TRIM(cvar)+1:) = ',' ! Finezza per la ','
      cvar(LEN_TRIM(cvar)+1:) = TRIM(to_char(vartable(nvbt)%varora))
    ENDIF
  ENDDO
ENDDO

nvout = 1 ! n. di valori in uscita, raffinare
! Ripeto l'estrazione oracle fino ad essere sicuro di avere
! estratto tutto (nobs < nmax)
DO WHILE(.TRUE.)
  nobs = n_getgsta(this%ounit, cnetwork, cvar, datai, orai, &
   dataf, oraf, nvout, &
   nmax, cdatao, stazo, varo, valore1, valore2, valore3, valid)
!  IF (verbose) 
  PRINT* ! Termina la riga per estetica, manca un \n
  IF (nobs < nmax .OR. nmax >= nmaxmax) EXIT ! tutto estratto o errore
  CALL print_info('Troppe osservazioni, rialloco ' &
   //TRIM(to_char(MIN(nmax*2, nmaxmax)))//' elementi')
  CALL vol7d_oraclesim_alloc(MIN(nmax*2, nmaxmax))
ENDDO
IF (nobs < 0) THEN
  CALL raise_error('in estrazione oracle', nobs)
  RETURN
ELSE
  CALL print_info('Estratte dall''archivio '//TRIM(to_char(nobs)) &
   //' osservazioni')
ENDIF
IF (nobs >= nmax) THEN ! tertium datur
  CALL raise_warning('troppi dati richiesti, estrazione incompleta')
ENDIF

! Controllo la validita` dei descrittori ana, time e var ottenuti da oracle
non_valid = .FALSE. ! ottimizzazione per la maggior parte dei casi
nana = count_distinct(stazo(1:nobs), back=.TRUE.)
ntime = count_distinct(cdatao(1:nobs), back=.TRUE.)
nvar = count_distinct(varo(1:nobs), back=.TRUE.)
ALLOCATE(anatmp(nana), tmtmp(ntime), vartmp(nvar))
anatmp(:) = pack_distinct(stazo(1:nobs), back=.TRUE.)
CALL pack_distinct_c(cdatao(1:nobs), tmtmp, back=.TRUE.)
vartmp(:) = pack_distinct(varo(1:nobs), back=.TRUE.)

DO i = 1, nana
  IF (.NOT. ANY(anatmp(i) == networktable(network)%ana(:)%ora_cod)) THEN
    non_valid = .TRUE.
    CALL raise_warning('stazione oraclesim '//TRIM(to_char(anatmp(i)))// &
     ' non trovata nell''anagrafica della rete '//TRIM(cnetwork)// &
     ', la ignoro')
    WHERE(stazo(1:nobs) == anatmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
ENDDO

DO i = 1, ntime
  CALL init(odatetime, oraclesimdate=tmtmp(i))
  IF (odatetime < timei .OR. odatetime > timef) THEN
    non_valid = .TRUE.
    CALL raise_warning('data oraclesim '//tmtmp(i)//' inattesa, la ignoro')
    WHERE(cdatao(1:nobs) == tmtmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
ENDDO

DO i = 1, nvar
  IF (.NOT.ANY((vartmp(i) == vartable(:)%varora) .AND. varbt_req(:))) THEN
    non_valid = .TRUE.
    CALL raise_warning('variabile oraclesim '//TRIM(to_char(vartmp(i)))// &
     ' inattesa, la ignoro')
    WHERE(varo(1:nobs) == vartmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
ENDDO
! ricreo gli elenchi solo se ci sono dati rigettati
IF (non_valid) THEN
  DEALLOCATE(anatmp, tmtmp, vartmp)
  WHERE (stazo(1:nobs) == 0) ! mal comune, mezzo gaudio
    cdatao(1:nobs) = ''
    varo(1:nobs) = 0
  END WHERE
  nana = count_distinct(stazo(1:nobs), back=.TRUE., mask=(stazo(1:nobs) /= 0))
  ntime = count_distinct(cdatao(1:nobs), back=.TRUE., mask=(cdatao(1:nobs) /= ''))
  nvar = count_distinct(varo(1:nobs), back=.TRUE., mask=(varo(1:nobs) /= 0))
  ALLOCATE(anatmp(nana), tmtmp(ntime), vartmp(nvar))
  anatmp(:) = pack_distinct(stazo(1:nobs), back=.TRUE., mask=(stazo(1:nobs) /= 0))
  CALL pack_distinct_c(cdatao(1:nobs), tmtmp, back=.TRUE., mask=(cdatao(1:nobs) /= ''))
  vartmp(:) = pack_distinct(varo(1:nobs), back=.TRUE., mask=(varo(1:nobs) /= 0))
ENDIF
! creo la mappatura, riciclo stazo che e` intero, con cdatao non posso
ALLOCATE(mapdatao(nobs))
DO i = 1, nana
  WHERE(stazo(1:nobs) == anatmp(i))
    stazo(1:nobs) = i
  END WHERE
ENDDO
DO i = 1, ntime
  WHERE(cdatao(1:nobs) == tmtmp(i))
    mapdatao(1:nobs) = i
  END WHERE
ENDDO
! ciclo sulle variabili per riempire vol7d
DO i = 1, nvar
  CALL init(v7dtmp)
  CALL vol7d_alloc(v7dtmp, ntime=ntime, nana=nana, &
   nlevel=1, ntimerange=1, nnetwork=1, ndativarr=1)

  IF (i == 1) THEN ! la prima volta inizializzo i descrittori fissi
    DO j = 1, ntime
      CALL init(v7dtmp%time(j), oraclesimdate=tmtmp(j))
    ENDDO
    DO j = 1, nana
      k = firsttrue(anatmp(j) == networktable(network)%ana(:)%ora_cod) ! ottimizzar
      CALL init(v7dtmp%ana(j), &
       lon=REAL(networktable(network)%ana(k)%lon,fp_geo), &
       lat=REAL(networktable(network)%ana(k)%lat,fp_geo))
    ENDDO
    IF (PRESENT(set_network)) THEN
      v7dtmp%network(1) = set_network ! dummy network
    ELSE
      CALL init(v7dtmp%network(1), network)
    ENDIF
  ELSE ! successivamente li copio da quelli precedenti
    v7dtmp%time = v7dtmp2%time
    v7dtmp%ana = v7dtmp2%ana
    v7dtmp%network = v7dtmp2%network
  ENDIF
  nvbt = firsttrue((vartmp(i) == vartable(:)%varora) .AND. varbt_req(:))
  CALL init(v7dtmp%dativar%r(1), vartable(nvbt)%varbt)
  v7dtmp%level(1) = vartable(nvbt)%level
  v7dtmp%timerange(1) = vartable(nvbt)%timerange

! Alloco e riempio il volume di dati
  CALL vol7d_alloc_vol(v7dtmp)
  v7dtmp%voldatir(:,:,:,:,:,:) = rmiss
  DO j = 1, nobs
    IF (varo(j) /= vartmp(i)) CYCLE ! solo la variabile corrente
    v7dtmp%voldatir(stazo(j),mapdatao(j),1,1,1,1) = &
     valore1(j)*vartable()%afact+vartable()%bfact 
  ENDDO

  IF (i == 1) THEN ! la prima volta assegno a v7dtmp2
    v7dtmp2 = v7dtmp
  ELSE ! successivamente fondo con il volume precedente
    CALL vol7d_merge(v7dtmp2, v7dtmp, sort=.FALSE.)
  ENDIF
ENDDO

! Se l'oggetto ha gia` un volume allocato lo fondo con quello estratto
IF (ASSOCIATED(this%vol7d%ana) .AND. ASSOCIATED(this%vol7d%time) .AND. &
 ASSOCIATED(this%vol7d%voldatir)) THEN
  CALL vol7d_merge(this%vol7d, v7dtmp2, sort=.TRUE.)
ELSE ! altrimenti lo assegno
  this%vol7d = v7dtmp2
ENDIF

DEALLOCATE(anatmp, tmtmp, vartmp, mapdatao)

END SUBROUTINE vol7d_oraclesim_importvvns

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
INTEGER :: i, sep(nf), n1, n2, un, i1, i2, i3
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
    READ(line(sep(4)+1:sep(5)-1),'(I8)')i1
    READ(line(sep(5)+1:sep(6)-1),'(I8)')i2
    READ(line(sep(6)+1:sep(7)-1),'(I8)')i3
    CALL init(vartable(i)%level, i1, i2, i3)
    READ(line(sep(7)+1:sep(8)-1),'(I8)')i1
    READ(line(sep(8)+1:sep(9)-1),'(I8)')i2
    READ(line(sep(9)+1:sep(10)-1),'(I8)')i3
    CALL init(vartable(i)%timerange, i1, i2, i3)
    READ(line(sep(11)+1:sep(12)-1),'(A)')vartable(i)%description
    READ(line(sep(12)+1:sep(13)-1),'(F10.0)')vartable(i)%afact
    READ(line(sep(13)+1:sep(14)-1),'(F10.0)')vartable(i)%bfact
    READ(line(sep(14)+1:sep(15)-1),'(I8)')vartable(i)%network
  ENDDO readline
120 CONTINUE

  CALL print_info('Ho letto '//TRIM(to_char(i))//' variabili dalla tabella')
ENDIF
CLOSE(un)

END SUBROUTINE vol7d_oraclesim_setup_conv


! Legge l'anagrafica per la rete specificata
SUBROUTINE vol7d_oraclesim_ora_ana(network)
INTEGER,INTENT(in) :: network

INTEGER :: i, j, un
CHARACTER(len=3) :: cnet

cnet = to_char(network,'(I3.3)')
un = open_package_file('simana_'//cnet//'.txt', filetype_data)
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
