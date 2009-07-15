!> \brief Estensione di vol7d_class per importare volumi di dati
!! dall'archivio Oracle del SIM.
!!
!! Questo modulo definisce gli oggetti e i metodi per importare
!! un volume di dati dall'archivio Oracle del SIM in un oggetto di tipo
!! vol7d_class::vol7d.
!!
!! \todo terminare la procedura per creare i file di anagrafica direttamente
!! dall'archivio Oracle (Andrea Selvini).
!!
!! \ingroup vol7d
MODULE vol7d_oraclesim_class
USE kinds
USE char_utilities
USE vol7d_class
USE file_utilities
IMPLICIT NONE

!> Definisce un'istanza di estrazione dall'archivio Oracle SIM.
!! Estende vol7d_class::vol7d aggiungendo le informazioni necessarie
!! all'estrazione. L'utente, pur potendo accedere a tutti i componenti
!! dell'oggetto, dovrà preoccuparsi del solo componente vol7d.
TYPE vol7d_oraclesim
  TYPE(vol7d) :: vol7d !< oggetto di tipo vol7d che conterrà i dati estratti
  INTEGER :: ounit !< informazione di servizio
  INTEGER(kind=ptr_c) :: connid
END TYPE vol7d_oraclesim

TYPE ora_var_conv
  INTEGER :: varora
  CHARACTER(len=10) :: varbt
  CHARACTER(len=20) :: unit
  TYPE(vol7d_level) :: level
  TYPE(vol7d_timerange) :: timerange
  CHARACTER(len=20) :: description
  REAL :: afact, bfact
  INTEGER :: networkid
END TYPE ora_var_conv

INTEGER,EXTERNAL :: oraextra_gethead, oraextra_getdata ! da sostituire con include/interface ?!
INTEGER(kind=ptr_c),EXTERNAL :: oraextra_init

INTEGER,ALLOCATABLE ::stazo(:), varo(:), valid(:)
REAL,ALLOCATABLE :: valore1(:), valore2(:)
INTEGER(kind=int_b),ALLOCATABLE :: cdatao(:,:), cflag(:,:)
!CHARACTER(len=1),ALLOCATABLE :: valore3(:)
CHARACTER(len=12),ALLOCATABLE ::fdatao(:)
INTEGER :: nmax=0, nact=0
INTEGER,PARAMETER :: nmaxmin=100000, nmaxmax=5000000, oraclesim_netmax=45, &
 datelen=13, flaglen=10
 
! tabella di conversione variabili da btable a oraclesim
TYPE(ora_var_conv),ALLOCATABLE :: vartable(:)
! tabella reti e anagrafica
TYPE(vol7d) :: netana(oraclesim_netmax)
LOGICAL :: networktable(oraclesim_netmax) = .FALSE.
INTEGER, PARAMETER :: netana_nvarr=2, netana_nvari=1, netana_nvarc=1

PRIVATE
PUBLIC vol7d_oraclesim, init, delete, import!, oraclesim_netmax

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
  MODULE PROCEDURE vol7d_oraclesim_import
END INTERFACE

CONTAINS

!> Inizializza un oggetto di tipo vol7doraclesim.
!! Trattandosi di un'estensione di vol7d, provvede ad inizializzare
!! anche l'oggetto vol7d contenuto.
!! Alla prima chiamata in un programma, provvede anche ad importare
!! le tabelle di conversione variabili dal file varmap.csv.
SUBROUTINE vol7d_oraclesim_init(this, dsn, user, password, write, wipe)
TYPE(vol7d_oraclesim),INTENT(out) :: this !< Oggetto da inizializzare
CHARACTER(len=*), INTENT(in),OPTIONAL :: dsn !< Nome del database, se non fornito usa il nome standard per l'archivio Oracle del SIM
CHARACTER(len=*), INTENT(in),OPTIONAL :: user !< Nome utente per il server Oracle, se non fornito usa il nome standard per l'archivio Oracle del SIM
CHARACTER(len=*), INTENT(in),OPTIONAL :: password !< Password per il server Oracle, se non fornito usa la password standard per l'archivio Oracle del SIM
LOGICAL,INTENT(in),OPTIONAL :: WRITE !< Non utilizzato, presente per compatibilità
LOGICAL,INTENT(in),OPTIONAL :: wipe !< Non utilizzato, presente per compatibilità

CHARACTER(len=32) :: ldsn, luser, lpassword

INTEGER :: err
INTEGER(kind=int_b) :: msg(256)
IF (PRESENT(dsn)) THEN
  ldsn = dsn
ELSE
  ldsn = 'metw'
ENDIF
IF (PRESENT(user)) THEN
  luser = user
ELSE
  luser = 'leggo'
ENDIF
IF (PRESENT(password)) THEN
  lpassword = password
ELSE
  lpassword = 'meteo'
ENDIF

this%connid = oraextra_init(fchar_to_cstr(TRIM(luser)), &
 fchar_to_cstr(TRIM(lpassword)), fchar_to_cstr(TRIM(ldsn)), err)
IF (err /= 0) THEN
  CALL oraextra_geterr(this%connid, msg)
  CALL oraextra_delete(this%connid)
  CALL l4f_log(L4F_FATAL, TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error(TRIM(cstr_to_fchar(msg)))
ENDIF
CALL vol7d_oraclesim_alloc(nmaxmin)
IF (.NOT. ALLOCATED(vartable)) CALL vol7d_oraclesim_setup_conv()
nact = nact + 1 ! Tengo il conto delle istanze attive
CALL init(this%vol7d)

END SUBROUTINE vol7d_oraclesim_init


!> Distrugge l'oggetto in maniera pulita.
!! Trattandosi di un'estensione di vol7d, provvede a distruggere
!! anche l'oggetto vol7d contenuto.
SUBROUTINE vol7d_oraclesim_delete(this)
TYPE(vol7d_oraclesim) :: this

CALL oraextra_delete(this%connid)
nact = MAX(nact - 1, 0) ! Tengo il conto delle istanze attive
IF (nact == 0) THEN
  CALL vol7d_oraclesim_dealloc()
  DEALLOCATE(vartable)
ENDIF

END SUBROUTINE vol7d_oraclesim_delete


!> Importa un volume vol7d dall'archivio Oracle SIM.
!! Tutti i descrittori vengono assegnati correttamente,
!! compresa l'anagrafica delle stazioni.
!! Attualmente l'importazione crea un volume di dati reali
!! vol7d_class::vol7d::voldatir con le osservazioni richieste
!! ed un eventuale volume di variabili di anagrafica
!! intere, reali e/o carattere vol7d_class::vol7d::volanai,
!! vol7d_class::vol7d::volanar, vol7d_class::vol7d::volanac
!! se il parametro \a anavar viene fornito; le variabili di anagrafica
!! attualmente disponibili sono:
!!  - 'B07001' station height (reale)
!!  - 'B07031' barometer height (reale)
!!  - 'B01192' Oracle station id (intero)
!!  - 'B01019' station name (carattere)
!!
!! Gestisce le flag di qualità SIM 'fase 0', cioè:
!!  - '1' dato invalidato manualmente -> restituisce valore mancante
!!  - '2' dato sostituito manualmente -> restituisce il dato sostituito
!!
SUBROUTINE vol7d_oraclesim_import(this, var, network, timei, timef, level, &
 timerange, anavar, set_network)
TYPE(vol7d_oraclesim),INTENT(out) :: this !< oggetto in cui importare i dati
CHARACTER(len=*),INTENT(in) :: var(:) !< lista delle variabili da importare, codice alfanumerico della tabella B locale
TYPE(vol7d_network),INTENT(in) :: network(:) !< lista di reti da estrarre, inizializzata con l'indicativo numerico che ha nell'archivio SIM
TYPE(datetime),INTENT(in) :: timei !< istante iniziale delle osservazioni da estrarre (estremo incluso)
TYPE(datetime),INTENT(in) :: timef !< istante finale delle osservazioni da estrarre (estremo incluso)
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level !< estrae solo il livello verticale fornito, default=tutti
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange !< estrae solo i dati con intervallo temporale (es. istantaneo, cumulato, ecc.) analogo al timerange fornito, default=tutti
!> variabili da importare secondo la tabella B locale o relativi alias relative ad anagrafica
CHARACTER(len=*),INTENT(in),OPTIONAL :: anavar(:) !< lista delle variabili di anagrafica da importare, codice alfanumerico della tabella B locale, se non fornito non ne importa nessuna
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network !< se fornito, collassa tutte le reti nell'unica rete indicata, eliminando le stazioni comuni a reti diverse

INTEGER :: i

DO i = 1, SIZE(network)
  CALL vol7d_oraclesim_importvvns(this, var, network(i), timei, timef, &
   level, timerange, anavar, set_network)
ENDDO

END SUBROUTINE vol7d_oraclesim_import


! Routine interna che fa la vera importazione, una rete alla volta
SUBROUTINE vol7d_oraclesim_importvvns(this, var, network, timei, timef, level, &
 timerange, anavar, set_network)
TYPE(vol7d_oraclesim),INTENT(out) :: this !< oggetto in cui importare i dati
CHARACTER(len=*),INTENT(in) :: var(:) !< lista delle variabili da importare (codice alfanumerico della tabella B del WMO)
TYPE(vol7d_network),INTENT(in) :: network !< rete da estrarre, inizializzata con l'indicativo numerico che ha nell'archivio SIM
TYPE(datetime),INTENT(in) :: timei !< istante iniziale delle osservazioni da estrarre (estremo incluso)
TYPE(datetime),INTENT(in) :: timef !< istante finale delle osservazioni da estrarre (estremo incluso)
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level !< estrae solo il livello verticale fornito, default=tutti
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange !< estrae solo i dati con intervallo temporale (es. istantaneo, cumulato, ecc.) analogo al timerange fornito, default=tutti
CHARACTER(len=*),INTENT(in),OPTIONAL :: anavar(:) !< lista delle variabili di anagrafica da importare, codice alfanumerico della tabella B locale, se non fornito non ne importa nessuna
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network !< se fornito, collassa tutte le reti nell'unica rete indicata, eliminando le stazioni comuni a reti diverse

TYPE(vol7d) :: v7dtmp, v7dtmp2, v7dtmpana
TYPE(datetime) :: odatetime
INTEGER :: i, j, k, nvar, nobs, nobso, ntime, nana, nvout, nvin, nvbt
CHARACTER(len=8) :: cnetwork
CHARACTER(len=12),ALLOCATABLE :: tmtmp(:)
CHARACTER(len=12) :: datai, dataf
INTEGER,ALLOCATABLE :: anatmp(:), vartmp(:), mapdatao(:), mapstazo(:), varlist(:)
LOGICAL,ALLOCATABLE :: lana(:)
LOGICAL :: found, non_valid, varbt_req(SIZE(vartable))
INTEGER(kind=int_b) :: msg(256)
LOGICAL :: lanar(netana_nvarr), lanai(netana_nvari), lanac(netana_nvarc)

CALL getval(timei, simpledate=datai)
CALL getval(timef, simpledate=dataf)

cnetwork = TRIM(to_char(network%id))
! Cerco la rete nella tabella
IF (network%id <= 0 .OR. network%id >= oraclesim_netmax ) THEN
  CALL l4f_log(L4F_ERROR, 'in oraclesim rete '//TRIM(cnetwork)//' non valida')
  return
ENDIF
! Leggo l'anagrafica per la rete se necessario
IF (.NOT. networktable(network%id)) THEN
  CALL vol7d_oraclesim_ora_ana(network%id)
ENDIF
! Conto le variabili da estrarre
varbt_req(:) = .FALSE.
DO nvin = 1, SIZE(var)
  found = .FALSE.
  DO nvbt = 1, SIZE(vartable)
    IF (vartable(nvbt)%varbt == var(nvin) .AND. &
     vartable(nvbt)%networkid == network%id) THEN

      IF (PRESENT(level))THEN
        IF (vartable(nvbt)%level /= level) CYCLE
      END IF

      IF (PRESENT(timerange))THEN
        IF (vartable(nvbt)%timerange /= timerange) CYCLE
      END IF

      found = .TRUE.
      varbt_req(nvbt) = .TRUE.
    ENDIF
  ENDDO
  IF (.NOT.found) CALL l4f_log(L4F_WARN, 'variabile '//TRIM(var(nvin))// &
   ' non valida per la rete '//TRIM(cnetwork))
ENDDO

nvar = COUNT(varbt_req)
ALLOCATE(varlist(nvar))
varlist = PACK(vartable(:)%varora, varbt_req)
IF (nvar == 0) THEN
  CALL l4f_log(L4F_WARN, 'nessuna delle variabili '//TRIM(var(1))// &
   ' e` valida per la rete '//TRIM(cnetwork))
  RETURN
ENDIF
CALL l4f_log(L4F_INFO, 'in oraclesim_class nvar='//to_char(nvar))

nobs = oraextra_gethead(this%connid, fchar_to_cstr(datai), &
 fchar_to_cstr(dataf), network%id, varlist, SIZE(varlist))
IF (nobs < 0) THEN
  CALL oraextra_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraextra_gethead: '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error('in oraextra_gethead: '//TRIM(cstr_to_fchar(msg)))
ENDIF
CALL l4f_log(L4F_INFO, 'in oraextra_gethead nobs='//to_char(nobs))

CALL vol7d_oraclesim_alloc(nobs) ! Mi assicuro di avere spazio
i = oraextra_getdata(this%connid, nobs, nobso, cdatao, stazo, varo, valore1, &
 valore2, cflag, rmiss)
IF (i /= 0) THEN
  CALL oraextra_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraextra_getdata: '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error('in oraextra_getdata: '//TRIM(cstr_to_fchar(msg)))
ENDIF

nobs = nobso
DO i = 1, nobs
  fdatao(i) = cstr_to_fchar(cdatao(:,i)) ! Converto la data da char C a CHARACTER
! Gestione flag di qualita` fase 0
  IF (cflag(1,i) == ICHAR('1')) THEN ! dato invalidato manualmente
    valore1(i) = rmiss ! forzo dato mancante
  ELSE IF (cflag(1,i) == ICHAR('2')) THEN ! dato modificato manualmente
! il valore buono e` il secondo a meno che esso non sia mancante
! come nei casi indicati da vpavan@arpa.emr.it e-mail del 14/07/2008:
! ==
! variabile precipitazione o bagnatura fogliare. I dati originali
! (cumulate) saltano una mezzora, o piu` di una, ma il dato successivo al
! periodo mancante e` uguale all'ultimo dato buono. Se ne desume che non
! e` piovuto per tutto il periodo e il valore 0 viene immesso nel primo
! campo.
! ==
! in tal caso e` buono il primo
    IF (valore2(i) /= rmiss) valore1(i) = valore2(i)
  ENDIF
ENDDO
non_valid = .FALSE. ! ottimizzazione per la maggior parte dei casi
nana = count_distinct(stazo(1:nobs), back=.TRUE.)
ntime = count_distinct(fdatao(1:nobs), back=.TRUE.)
nvar = count_distinct(varo(1:nobs), back=.TRUE.)
ALLOCATE(anatmp(nana), tmtmp(ntime), vartmp(nvar))
anatmp(:) = pack_distinct(stazo(1:nobs), nana, back=.TRUE.)
CALL pack_distinct_c(fdatao(1:nobs), tmtmp, back=.TRUE.)
vartmp(:) = pack_distinct(varo(1:nobs), nvar, back=.TRUE.)
CALL l4f_log(L4F_INFO, 'in oraclesim_class onvar='//to_char(nvar))

DO i = 1, nana
  IF (.NOT. ANY(anatmp(i) == netana(network%id)%volanai(:,1,1))) THEN
    non_valid = .TRUE.
    CALL l4f_log(L4F_WARN, 'stazione oraclesim '//TRIM(to_char(anatmp(i)))// &
     ' non trovata nell''anagrafica della rete '//TRIM(cnetwork)// &
     ', la ignoro')
    WHERE(stazo(1:nobs) == anatmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
ENDDO

! Praticamente inutile se mi fido della query
DO i = 1, ntime
 odatetime = datetime_new(simpledate=tmtmp(i))
  IF (odatetime < timei .OR. odatetime > timef) THEN
    non_valid = .TRUE.
    CALL l4f_log(L4F_WARN, 'data oraclesim '//tmtmp(i)//' inattesa, la ignoro')
    WHERE(fdatao(1:nobs) == tmtmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
ENDDO

! Praticamente inutile se mi fido della query
DO i = 1, nvar
  IF (.NOT.ANY((vartmp(i) == vartable(:)%varora) .AND. varbt_req(:))) THEN
    non_valid = .TRUE.
    CALL l4f_log(L4F_WARN, 'variabile oraclesim '//TRIM(to_char(vartmp(i)))// &
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
    fdatao(1:nobs) = ''
    varo(1:nobs) = 0
  END WHERE
  nana = count_distinct(stazo(1:nobs), back=.TRUE., mask=(stazo(1:nobs) /= 0))
  ntime = count_distinct(fdatao(1:nobs), back=.TRUE., mask=(fdatao(1:nobs) /= ''))
  nvar = count_distinct(varo(1:nobs), back=.TRUE., mask=(varo(1:nobs) /= 0))
  ALLOCATE(anatmp(nana), tmtmp(ntime), vartmp(nvar))
  anatmp(:) = pack_distinct(stazo(1:nobs), nana, back=.TRUE., mask=(stazo(1:nobs) /= 0))
  CALL pack_distinct_c(fdatao(1:nobs), tmtmp, back=.TRUE., mask=(fdatao(1:nobs) /= ''))
  vartmp(:) = pack_distinct(varo(1:nobs), nvar, back=.TRUE., mask=(varo(1:nobs) /= 0))
ENDIF

! creo la mappatura
ALLOCATE(mapdatao(nobs), mapstazo(nobs))
DO i = 1, nana
  WHERE(stazo(1:nobs) == anatmp(i))
    mapstazo(1:nobs) = i
  END WHERE
ENDDO
DO i = 1, ntime
  WHERE(fdatao(1:nobs) == tmtmp(i))
    mapdatao(1:nobs) = i
  END WHERE
ENDDO
! ciclo sulle variabili per riempire vol7d
CALL init(v7dtmp2) ! nel caso di nvar/nobs = 0
DO i = 1, nvar
  CALL init(v7dtmp)
  CALL vol7d_alloc(v7dtmp, ntime=ntime, nana=nana, &
   nlevel=1, ntimerange=1, nnetwork=1, ndativarr=1)

  IF (i == 1) THEN ! la prima volta inizializzo i descrittori fissi
    IF (PRESENT(set_network)) THEN
      v7dtmp%network(1) = set_network ! dummy network
    ELSE
      v7dtmp%network(1) = network
    ENDIF
    ALLOCATE(lana(SIZE(netana(network%id)%ana)))
    lana = .FALSE.
    DO j = 1, nana
      k = INDEX(netana(network%id)%volanai(:,1,1), anatmp(j))
      v7dtmp%ana(j) = netana(network%id)%ana(k) ! attenzione ai puntatori
      lana(k) = .TRUE.
    ENDDO
! se sono richieste delle variabili di anagrafica
! copio il sottoinsieme di anagrafica che mi interessa in tmpana
! e lo fondo col volume appena creato
    IF (PRESENT(anavar)) THEN
      DO j = 1, SIZE(netana(network%id)%anavar%r)
        lanar(j) = ANY(netana(network%id)%anavar%r(j)%btable == anavar)
      ENDDO
      DO j = 1, SIZE(netana(network%id)%anavar%i)
        lanai(j) = ANY(netana(network%id)%anavar%i(j)%btable == anavar)
      ENDDO
      DO j = 1, SIZE(netana(network%id)%anavar%c)
        lanac(j) = ANY(netana(network%id)%anavar%c(j)%btable == anavar)
      ENDDO
      CALL vol7d_copy(netana(network%id), v7dtmpana, lana=lana, &
       lanavarr=lanar, lanavari=lanai, lanavarc=lanac)
      v7dtmpana%network(1) = v7dtmp%network(1) ! faccio coincidere la rete
! fondo v7dtmpana appena creato con v7dtmp
! qui faccio affidamenteo sul fatto che vol7d_merge conserva l'ordine
! del primo argomento (v7dtmp e v7dtmpana hanno la stessa anagrafica
! ma con un ordinamento in generale diverso)
      CALL vol7d_merge(v7dtmp, v7dtmpana)
    ENDIF
    DEALLOCATE(lana)

    DO j = 1, ntime
      v7dtmp%time(j) = datetime_new(simpledate=tmtmp(j))
    ENDDO
  ELSE ! successivamente li copio da quelli precedenti
    v7dtmp%time = v7dtmp2%time
    v7dtmp%ana = v7dtmp2%ana
    v7dtmp%network = v7dtmp2%network
  ENDIF
  nvbt = INDEX(vartable(:)%varora, vartmp(i),  mask=varbt_req(:))
  CALL init(v7dtmp%dativar%r(1), vartable(nvbt)%varbt)
  v7dtmp%level(1) = vartable(nvbt)%level
  v7dtmp%timerange(1) = vartable(nvbt)%timerange

! Alloco e riempio il volume di dati
  CALL vol7d_alloc_vol(v7dtmp)
  v7dtmp%voldatir(:,:,:,:,:,:) = rmiss
  DO j = 1, nobs
! Solo la variabile corrente e, implicitamente, dato non scartato
    IF (varo(j) /= vartmp(i)) CYCLE
    v7dtmp%voldatir(mapstazo(j),mapdatao(j),1,1,1,1) = &
     valore1(j)*vartable(nvbt)%afact+vartable(nvbt)%bfact 
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

DEALLOCATE(anatmp, tmtmp, vartmp, mapdatao, mapstazo)

END SUBROUTINE vol7d_oraclesim_importvvns

!=================
! Routine private
!=================

! Alloca o rialloca i vettori di lavoro per le routine di accesso a oracle
SUBROUTINE vol7d_oraclesim_alloc(n)
INTEGER,INTENT(in) :: n

IF (nmax >= n) RETURN ! c'e' gia' posto sufficiente
IF (ALLOCATED(stazo)) DEALLOCATE(stazo, varo, valid, valore1, valore2, &
 cdatao, fdatao, cflag)
ALLOCATE(stazo(n), varo(n), valid(n), valore1(n), valore2(n), &
 cdatao(datelen, n), fdatao(n), cflag(flaglen,n))

nmax = n

END SUBROUTINE vol7d_oraclesim_alloc


! Delloca i vettori di lavoro per le routine di accesso a oracle
! e le anagrafiche eventualemnte lette
SUBROUTINE vol7d_oraclesim_dealloc()

INTEGER :: i

DO i = 1, oraclesim_netmax
  IF (networktable(i)) THEN
    CALL delete(netana(i))
    networktable(i) = .FALSE.
  ENDIF
ENDDO
IF (ALLOCATED(stazo)) DEALLOCATE(stazo, varo, valid, valore1, valore2, &
 cdatao, fdatao, cflag)
nmax = 0

END SUBROUTINE vol7d_oraclesim_dealloc


! Legge la tabella di conversione per le variabili
SUBROUTINE vol7d_oraclesim_setup_conv()
INTEGER,PARAMETER :: nf=16 ! formato file
INTEGER :: i, sep(nf), n1, n2, un, i1, i2, i3, i4
CHARACTER(len=512) :: line
CHARACTER(len=64) :: buf
TYPE(csv_record) :: csv

un = open_package_file('varmap.csv', filetype_data)
IF (un < 0) then
  CALL l4f_log(L4F_FATAL, 'in oraclesim non trovo il file delle variabili')
  CALL raise_fatal_error('in oraclesim non trovo il file delle variabili')
END IF
i = 0
DO WHILE(.TRUE.)
  READ(un,'(A)',END=100)line
!  IF (delim_csv(line, sep) < 0) CYCLE
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
    CALL init(csv, line)
    i = i + 1
    CALL csv_record_getfield(csv, vartable(i)%varora)
    CALL csv_record_getfield(csv, vartable(i)%varbt)
    CALL csv_record_getfield(csv, buf)
    vartable(i)%unit = buf ! uso buf per evitare un warning stringa troppo corta
    CALL csv_record_getfield(csv, i1)
    CALL csv_record_getfield(csv, i2)
    CALL csv_record_getfield(csv, i3)
    CALL csv_record_getfield(csv, i4)
    CALL init(vartable(i)%level, i1, i2, i3, i4)
    CALL csv_record_getfield(csv, i1)
    CALL csv_record_getfield(csv, i2)
    CALL csv_record_getfield(csv, i3)
    CALL init(vartable(i)%timerange, i1, i2, i3)
    CALL csv_record_getfield(csv)
    CALL csv_record_getfield(csv, buf)
    vartable(i)%description = buf ! uso buf per evitare un warning stringa troppo corta
    CALL csv_record_getfield(csv, vartable(i)%afact)
    CALL csv_record_getfield(csv, vartable(i)%bfact)
    CALL csv_record_getfield(csv, vartable(i)%networkid)
    CALL delete(csv)
  ENDDO readline
120 CONTINUE

  CALL l4f_log(L4F_INFO, 'Ho letto '//TRIM(to_char(i))//' variabili dalla tabella')
ENDIF
CLOSE(un)

END SUBROUTINE vol7d_oraclesim_setup_conv


! Legge l'anagrafica per la rete specificata
SUBROUTINE vol7d_oraclesim_ora_ana(netid)
INTEGER,INTENT(in) :: netid

INTEGER :: i, j, un
CHARACTER(len=3) :: cnet
CHARACTER(len=1) :: macroa
REAL(kind=fp_geo) :: lon, lat

networktable(netid) = .TRUE.
CALL init(netana(netid))
cnet = to_char(netid,'(I3.3)')
un = open_package_file('net_'//cnet//'.simana', filetype_data)
IF (un < 0) then
  CALL l4f_log(L4F_FATAL, 'non trovo il file di anagrafica per la rete '//cnet)
  CALL raise_fatal_error('non trovo il file di anagrafica per la rete '//cnet)
END IF
i = 0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  i = i + 1
ENDDO
100 CONTINUE
REWIND(un)

CALL vol7d_alloc(netana(netid), nnetwork=1, nana=i, &
 nanavarr=netana_nvarr, nanavari=netana_nvari, nanavarc=netana_nvarc)
CALL vol7d_alloc_vol(netana(netid))
CALL init(netana(netid)%network(1), id=netid)
CALL init(netana(netid)%anavar%r(1), btable='B07001') ! station height
CALL init(netana(netid)%anavar%r(2), btable='B07031') ! barometer height
CALL init(netana(netid)%anavar%i(1), btable='B01192') ! Oracle station id
CALL init(netana(netid)%anavar%c(1), btable='B01019') ! station name
DO j = 1, i
  READ(un,*)netana(netid)%volanai(j,1,1), lat, lon, netana(netid)%volanar(j,2,1), &
   netana(netid)%volanar(j,1,1), macroa, netana(netid)%volanac(j,1,1)
  IF (netana(netid)%volanar(j,1,1) < -9998.) netana(netid)%volanar(j,1,1) = rmiss
  IF (netana(netid)%volanar(j,2,1) < -9998.) netana(netid)%volanar(j,2,1) = rmiss
  IF (lon < -99.8 .AND. lat < -99.8) THEN
    CALL init(netana(netid)%ana(j))
  ELSE
    CALL init(netana(netid)%ana(j), lon=lon, lat=lat)
  ENDIF
ENDDO
CLOSE(un)

CALL l4f_log(L4F_INFO, 'ho letto l''anagrafica di '//TRIM(to_char(i))// &
 ' stazioni per la rete '//cnet)

END SUBROUTINE vol7d_oraclesim_ora_ana


END MODULE vol7d_oraclesim_class

