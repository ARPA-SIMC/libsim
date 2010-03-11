PROGRAM v7doracle_pollini
! Programma di estrazione operativa dei dati di pollini
USE datetime_class
USE file_utilities
USE vol7d_oraclesim_class
USE vol7d_class
USE vol7d_class_compute
USE vol7d_utilities
USE log4fortran
USE getopt_m
IMPLICIT NONE

! Namelist pollini e affini
CHARACTER(len=10) :: variabili(100)!, v7variabili(100)
CHARACTER(len=30) :: famiglie(100), file_stazioni(100)
INTEGER :: stazioni(100)
CHARACTER(len=16) :: data_inizio, data_fine, data_inizio_l, data_fine_l
CHARACTER(len=1024) :: file_rapporto, file_rapporto_l, file_naml
NAMELIST /pollini/ variabili, famiglie, stazioni, file_stazioni, &
 data_inizio, data_fine, file_rapporto
INTEGER :: nvar, nfam, nstaz, nstazf

! Estrazione oracle
TYPE(vol7d_oraclesim) :: db_v7d
TYPE(vol7d) :: v7d_fill
TYPE(vol7d_network) :: network
TYPE(datetime) :: ti, tf
TYPE(timedelta) :: dtfill
INTEGER, ALLOCATABLE :: famptr(:), v7dfamptr(:)
INTEGER :: i, j, k, n
REAL, POINTER :: vol3d(:,:,:)
INTEGER, POINTER :: stazid(:)

! Output csv
CHARACTER(len=1024) :: intestaz
TYPE(timedelta) :: gm1
TYPE(csv_record) :: csv_write
INTEGER :: dataa, datam, datag
INTEGER, ALLOCATABLE :: ndstaz(:)
REAL :: conc

! Log4fortran
INTEGER :: category, ier
CHARACTER(len=512):: a_name

!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name)
!init di log4fortran
ier = l4f_init()
category = l4f_category_get(a_name)

! Imposto i default
variabili = ''
famiglie = ''
stazioni = 0
file_stazioni = ''
data_inizio = ''
data_fine = ''
data_inizio_l = ''
data_fine_l = ''
file_rapporto = ''
file_rapporto_l = ''
file_naml = 'pollini.naml'
! Controllo le opzioni a linea di comando
DO
  SELECT CASE( getopt( 'i:f:r:n:'))
  CASE( CHAR(0)) ! end of options
    EXIT
  CASE( 'i' )
    data_inizio_l = optarg
  CASE( 'f' )
    data_fine_l = optarg
  CASE( 'r' )
    file_rapporto_l = optarg
  CASE( 'n' )
    file_naml = optarg
  END SELECT
END DO

OPEN(10, file=file_naml)
READ(10, NML=pollini)
CLOSE(10)

! Ricopro i valori di namelist dalla linea di comando
IF (data_inizio_l /= '') data_inizio = data_inizio_l
IF (data_fine_l /= '') data_fine = data_fine_l
IF (file_rapporto_l /= '') file_rapporto = file_rapporto_l

! conto le variabili, le famiglie e le stazioni richieste
nvar = COUNT(variabili /= '')
nfam = COUNT(famiglie /= '')
nstaz = COUNT(stazioni /= 0)
nstazf = COUNT(file_stazioni /= '')
! controllo se mi hanno imbrogliato
IF (nfam /= nvar) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Numero famiglie e numero variabili non coincidono: '// &
   TRIM(to_char(nfam))//' '//TRIM(to_char(nvar))//'.')
ENDIF
IF (ANY(variabili(1:nvar) == '')) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Esistono nomi di variabile non validi.')
ENDIF
IF (count_distinct(variabili(1:nvar), back=.TRUE.) /= nvar) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Una o piu` variabili sono ripetute, questo non e` consentito per ora.')
ENDIF
IF (ANY(famiglie(1:nfam) == '')) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Esistono nomi di famiglia non validi.')
ENDIF
IF (nstaz /= nstazf) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Numero stazioni e numero file stazione non coincidono: '// &
   TRIM(to_char(nstaz))//' '//TRIM(to_char(nstazf))//'.')
ENDIF
IF (ANY(stazioni(1:nstaz) == 0)) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Esistono identificativi stazione non validi.')
ENDIF
IF (ANY(file_stazioni(1:nstazf) == '')) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Esistono nomi stazione non validi.')
ENDIF

! conto le famiglie diverse
nfam = count_distinct(famiglie(1:nvar), back=.TRUE.)
CALL l4f_category_log(category,L4F_INFO, &
 'Periodo richiesto: '//to_char(data_inizio)//' - '//to_char(data_fine))
CALL l4f_category_log(category,L4F_INFO, &
 'Richieste '//TRIM(to_char(nvar))//' variabili per '// &
 TRIM(to_char(nfam))//' famiglie diverse.')
ALLOCATE(famptr(nvar))
famptr(:) = map_distinct(famiglie(1:nvar), back=.TRUE.)

! Definisco le reti da cui voglio estrarre
CALL init(network, 'POLLINI')

! Chiamo il costruttore della classe vol7d_oraclesim per il mio oggetto
CALL init(db_v7d)
! estraggo i dati
CALL import(db_v7d, variabili(1:nvar), (/network/), &
 datetime_new(isodate=data_inizio), datetime_new(isodate=data_fine), &
 anavar=(/'B01192'/), timerange=vol7d_timerange_new(0,0,86400))

dtfill = timedelta_new(day=1)
CALL vol7d_fill_time(db_v7d%vol7d, v7d_fill, dtfill, datetime_new(isodate=data_inizio), datetime_new(isodate=data_fine))
CALL delete(db_v7d%vol7d)
db_v7d%vol7d = v7d_fill
! devo rimappare le variabili perche' l'estrazione oracle non garantisce l'ordine
ALLOCATE(v7dfamptr(SIZE(db_v7d%vol7d%dativar%r)))
v7dfamptr(:) = 0
DO k = 1, nfam
  DO j = 1, nvar
    IF (famptr(j) == k) THEN
      WHERE(db_v7d%vol7d%dativar%r(:)%btable == variabili(j))
        v7dfamptr(:) = k
      END WHERE
    ENDIF
  ENDDO
ENDDO

! Creo una vista su un array tridimensionale che scorre le dimensioni
! dell'anagrafica, del tempo e delle variabili (vol7d_ana_d, vol7d_time_d)
CALL vol7d_get_voldatir(db_v7d%vol7d, (/vol7d_ana_d,vol7d_time_d,vol7d_var_d/), vol3dp=vol3d)
CALL vol7d_get_volanai(db_v7d%vol7d, (/vol7d_ana_d/), vol1dp=stazid)

! Comincio a scrivere i risultati
CALL init(csv_write)
CALL csv_record_addfield(csv_write, 'Anno')
CALL csv_record_addfield(csv_write, 'Mese')
CALL csv_record_addfield(csv_write, 'Giorno')
DO k = 1, nfam ! famiglia
  CALL csv_record_addfield(csv_write, TRIM(famiglie(firsttrue(famptr(:) == k))))
ENDDO
! ricordo l'intestazione (da ripetere per ogni file)
intestaz = csv_record_getrecord(csv_write)
CALL delete(csv_write)

ALLOCATE(ndstaz(SIZE(db_v7d%vol7d%ana)))
ndstaz(:) = 0
gm1 = timedelta_new(day=0)
! Stampo su file la tabella in uscita
DO j = 1, SIZE(db_v7d%vol7d%ana) ! stazione
  n = firsttrue(stazioni == stazid(j))
  IF (n <= 0) CYCLE ! stazione non richiesta
  OPEN(10, file=file_stazioni(n))
  WRITE(10,'(a)') TRIM(intestaz)
  DO i = 1, SIZE(db_v7d%vol7d%time) ! data
    CALL getval(db_v7d%vol7d%time(i)+gm1, year=dataa, month=datam, day=datag, hour=k)
    CALL init(csv_write)
    CALL csv_record_addfield(csv_write, dataa)
    CALL csv_record_addfield(csv_write, datam)
    CALL csv_record_addfield(csv_write, datag)
    DO k = 1, nfam ! famiglia
      n = COUNT(vol3d(j,i,:) /= rmiss .AND. v7dfamptr(:) == k)
      IF (n > 0) THEN
        conc = SUM(vol3d(j,i,:), mask=(vol3d(j,i,:) /= rmiss .AND. v7dfamptr(:) == k))
        CALL csv_record_addfield(csv_write, conc, form='(F0.1)')
        ndstaz(j) = ndstaz(j) + 1
      ELSE
        conc = rmiss
        CALL csv_record_addfield(csv_write, 'NA')
      ENDIF
    ENDDO
    WRITE(10,'(A)') TRIM(csv_record_getrecord(csv_write))
    CALL delete(csv_write)
  ENDDO
  CLOSE(10)
ENDDO

IF (file_rapporto /= '') THEN ! richiesto il rapporto
  OPEN(10, file=file_rapporto)
  DO j = 1, SIZE(db_v7d%vol7d%ana)
    n = firsttrue(stazioni == stazid(j))
    IF (n <= 0) CYCLE ! stazione non richiesta
! scrivo nome file stazione e percentuale di dati validi
    WRITE(10,'(A,1X,I4)') file_stazioni(n), &
     NINT(100.*ndstaz(j)/REAL(SIZE(db_v7d%vol7d%time)*nfam))
  ENDDO
ENDIF

CALL l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM v7doracle_pollini
