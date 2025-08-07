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
PROGRAM v7doracle_pollini
! Programma di estrazione operativa dei dati di pollini
USE datetime_class
USE file_utilities
USE vol7d_class
USE vol7d_dballe_class
USE vol7d_class_compute
USE array_utilities
USE log4fortran
USE optionparser_class
IMPLICIT NONE

! Opzioni a linea di comando
TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
LOGICAL :: version
! Namelist pollini e affini
INTEGER,PARAMETER :: safedim=100
CHARACTER(len=10) :: variabili(safedim), lonlist(safedim), latlist(safedim)
CHARACTER(len=30) :: famiglie(safedim), file_stazioni(safedim)
INTEGER :: stazioni(safedim)
CHARACTER(len=19) :: data_inizio, data_fine, data_inizio_l, data_fine_l
CHARACTER(len=1024) :: input_file, file_rapporto, file_rapporto_l, file_naml, form_in
NAMELIST /pollini/ variabili, famiglie, stazioni, lonlist, latlist, file_stazioni, &
 data_inizio, data_fine, form_in, file_rapporto
INTEGER :: nvar, nfam, nstaz, nstazf, lonvar, latvar

! Lettura bufr
TYPE(vol7d_dballe) :: db_v7d
TYPE(vol7d) :: v7d_fill
TYPE(vol7d_network) :: network
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
INTEGER :: ier
TYPE(l4f_handle) :: category
CHARACTER(len=512):: a_name

!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name)
!init di log4fortran
ier = l4f_init()
category = l4f_category_get_handle(a_name)

! Imposto i default
variabili = ''
famiglie = ''
stazioni = 0
file_stazioni = ''
data_inizio = ''
data_fine = ''
form_in = 'BUFR'
file_rapporto = ''
! Controllo le opzioni a linea di comando

! define the option parser
opt = optionparser_new(description_msg= &
 'Program for preprocessing pollen data from ARPA-SIM database; &
 &pollen data are read in bufr format', &
 usage_msg='v7d_pollini [options] input_file')

CALL optionparser_add(opt, 'i', 'start-date', data_inizio_l, '', help= &
 'initial date for retrieving data, if provided, it overrides the date &
 &specified in the namelist file')
CALL optionparser_add(opt, 'f', 'end-date', data_fine_l, '', help= &
 'final date for retrieving data, if provided, it overrides the date &
 &specified in the namelist file')
CALL optionparser_add(opt, 'r', 'report', file_rapporto_l, '', help= &
 'name of output report file, if provided, it overrides the name &
 &specified in the namelist file')
CALL optionparser_add(opt, 'n', 'naml', file_naml, 'pollini.naml', help= &
 'name of namelist file where configuration will be read')

! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

CALL optionparser_parse(opt, optind, optstatus)
IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line parameters')
  CALL raise_fatal_error()
ENDIF

! check input/output files
i = iargc() - optind
IF (i < 0) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'input file missing')
  CALL raise_fatal_error()
ENDIF
CALL getarg(iargc(), input_file)

! Leggo namelist file
OPEN(10, file=file_naml)
READ(10, NML=pollini)
CLOSE(10)
! Ricopro i valori di namelist dalla linea di comando
IF (data_inizio_l /= '') data_inizio = data_inizio_l
IF (data_fine_l /= '') data_fine = data_fine_l
IF (file_rapporto_l /= '') file_rapporto = file_rapporto_l

CALL delete(opt)

! conto le variabili, le famiglie e le stazioni richieste
nvar = COUNT(variabili /= '')
nfam = COUNT(famiglie /= '')
nstaz = COUNT(stazioni /= 0)
nstazf = COUNT(file_stazioni /= '')
! controllo se mi hanno imbrogliato
IF (nfam /= nvar) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Numero famiglie e numero variabili non coincidono: '// &
   t2c(nfam)//' '//t2c(nvar)//'.')
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
   t2c(nstaz)//' '//t2c(nstazf)//'.')
ENDIF
!IF (ANY(stazioni(1:nstaz) == 0)) THEN
!  CALL l4f_category_log(category, L4F_FATAL, &
!   'Esistono identificativi stazione non validi.')
!ENDIF
IF (ANY(file_stazioni(1:nstazf) == '')) THEN
  CALL l4f_category_log(category, L4F_FATAL, &
   'Esistono nomi stazione non validi.')
ENDIF

! conto le famiglie diverse
nfam = count_distinct(famiglie(1:nvar), back=.TRUE.)
CALL l4f_category_log(category,L4F_INFO, &
 'Periodo richiesto: '//TRIM(data_inizio)//' - '//TRIM(data_fine))
CALL l4f_category_log(category,L4F_INFO, &
 'Richieste '//t2c(nvar)//' variabili per '// &
 t2c(nfam)//' famiglie diverse.')
ALLOCATE(famptr(nvar))
famptr(:) = map_distinct(famiglie(1:nvar), back=.TRUE.)

! Definisco le reti da cui voglio estrarre
CALL init(network, 'POLLINI')

CALL init(db_v7d, filename=input_file, FORMAT=TRIM(form_in), file=.TRUE., &
 time_definition=1)
! estraggo i dati, seleziono solo i dati osservati mediati su 1 giorno
CALL import(db_v7d, timerange=vol7d_timerange_new(0,0,86400), &
 timei=datetime_new(isodate=data_inizio), timef=datetime_new(isodate=data_fine))

dtfill = timedelta_new(day=1)
CALL vol7d_fill_time(db_v7d%vol7d, v7d_fill, dtfill, datetime_new(isodate=data_inizio), datetime_new(isodate=data_fine))
CALL delete(db_v7d%vol7d)
!db_v7d%vol7d = v7d_fill
CALL vol7d_convr(v7d_fill, db_v7d%vol7d, anaconv=.FALSE.)
!CALL delete(db_v7d%vol7d)
!db_v7d%vol7d=v7dtmp
CALL init(v7d_fill) ! detach it

! devo rimappare le variabili non conosco l'ordine
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

CALL display(db_v7d%vol7d)
PRINT*,'famptr',famptr
PRINT*,'v7dptr',v7dfamptr
! Creo una vista su un array tridimensionale che scorre le dimensioni
! dell'anagrafica, del tempo e delle variabili (vol7d_ana_d, vol7d_time_d)
CALL vol7d_get_voldatir(db_v7d%vol7d, (/vol7d_ana_d,vol7d_time_d,vol7d_var_d/), vol3dp=vol3d)
CALL vol7d_get_volanai(db_v7d%vol7d, (/vol7d_ana_d/), vol1dp=stazid)

lonvar = firsttrue(db_v7d%vol7d%anavar%c%btable == "B06001")
latvar = firsttrue(db_v7d%vol7d%anavar%c%btable == "B05001")

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
gm1 = timedelta_new(day=-1)
! Stampo su file la tabella in uscita
DO j = 1, SIZE(db_v7d%vol7d%ana) ! stazione
  n = ana_match(db_v7d%vol7d%volanac(j,lonvar,1),db_v7d%vol7d%volanac(j,latvar,1))
  PRINT*,'staz',n
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
    n = ana_match(db_v7d%vol7d%volanac(j,lonvar,1),db_v7d%vol7d%volanac(j,latvar,1))
!    n = firsttrue(stazioni == stazid(j))
    IF (n <= 0) THEN
      CALL l4f_category_log(category,L4F_WARN, &
       'stazione non riconosciuta, coordinate (10^5deg):' &
       //TRIM(db_v7d%vol7d%volanac(j,lonvar,1))//',' &
       //TRIM(db_v7d%vol7d%volanac(j,latvar,1)))
      CYCLE ! stazione non richiesta
    ENDIF
! scrivo nome file stazione e percentuale di dati validi
    WRITE(10,'(A,1X,I4)') file_stazioni(n), &
     NINT(100.*ndstaz(j)/REAL(SIZE(db_v7d%vol7d%time)*nfam))
  ENDDO
ENDIF

CALL delete(db_v7d)
CALL l4f_category_delete(category)
ier=l4f_fini()

CONTAINS

FUNCTION ana_match(clon, clat)
CHARACTER(len=*),INTENT(in) :: clon
CHARACTER(len=*),INTENT(in) :: clat
INTEGER :: ana_match

INTEGER :: i

DO i = 1, SIZE(lonlist)
  IF (clon == lonlist(i)) THEN
    IF (clat == latlist(i)) THEN
      ana_match = i
      RETURN
    ENDIF
  ENDIF
ENDDO
ana_match = -1 ! imiss?

END FUNCTION ana_match

END PROGRAM v7doracle_pollini
