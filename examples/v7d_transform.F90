PROGRAM v7d_transform
#include "config.h"
USE log4fortran
USE char_utilities
USE getopt_m
USE io_units
USE vol7d_class
USE vol7d_class_compute
USE datetime_class
USE vol7d_oraclesim_class
USE vol7d_dballe_class
USE geo_coord_class
!USE ISO_FORTRAN_ENV
IMPLICIT NONE

TYPE(op_option) :: options(40) ! remember to update dimension when adding options
TYPE(optionparser) :: opt
CHARACTER(len=8) :: input_format, output_format
CHARACTER(len=512) :: input_file, output_file, network_list, variable_list
character(len=80) :: output_template
TYPE(vol7d_network), ALLOCATABLE :: nl(:)
CHARACTER(len=10), ALLOCATABLE :: vl(:)
CHARACTER(len=23) :: start_date, end_date
TYPE(datetime) :: s_d, e_d
INTEGER :: iun, ier, i, j, n, nc
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d) :: v7d, v7d_comp1, v7d_comp2
TYPE(vol7d_dballe) :: v7d_dba, v7d_dba_out
TYPE(vol7d_oraclesim) :: v7d_osim
LOGICAL :: ldisplay
CHARACTER(len=512):: a_name
INTEGER :: category

! for computing
LOGICAL :: comp_regularize, comp_average, comp_cumulate
CHARACTER(len=23) :: comp_interval, comp_start
TYPE(timedelta) :: c_i
TYPE(datetime) :: c_s

! for csv output
CHARACTER(len=8) :: csv_volume
CHARACTER(len=512) :: csv_column, csv_variable
LOGICAL :: csv_skip_miss, csv_rescale
INTEGER :: csv_header, icsv_column(7)


!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name,a_name_force="v7d_transform")
!init di log4fortran
ier=l4f_init()
!imposta a_name
category=l4f_category_get(a_name//".main")

! define command-line options
CALL op_option_nullify(options)

! options for defining input
options(1) = op_option_new(' ', 'input-format', input_format, 'native', help= &
 'format of input, ''native'' for vol7d native binary file&
#ifdef HAVE_DBALLE
 &, ''BUFR'' for BUFR file with generic template, ''CREX'' for CREX file&
 &, ''dba'' for dballe database&
#endif
#ifdef HAVE_ORSIM
 &, ''orsim'' for SIM Oracle database&
#endif
 &')
!options(2) = op_option_new('i', 'input-file', input_file, '-', help= &
! 'if input-format is of file type, input file name, ''-'' for stdin; &
! &if input-format is of database type, database access info in the form &
! &user/password@dsn, if empty or ''-'', a suitable default is used.')

! input database options
options(4) = op_option_new('s', 'start-date', start_date, '1900-01-01 00:00:00', help= &
 'if input-format is of database type, initial date for extracting data')
options(5) = op_option_new('e', 'end-date', end_date, '2021-01-01 00:00:00', help= &
 'if input-format is of database type, final date for extracting data')
options(6) = op_option_new('n', 'network-list', network_list, '', help= &
 'if input-format is of database type, list of station networks to be extracted &
 &in the form of a comma-separated list of alphanumeric network identifiers')
options(7) = op_option_new('v', 'variable-list', variable_list, '', help= &
 'if input-format is of database type, list of variables to be extracted &
 & in the form of a comma-separated list of B-table alphanumeric codes, e.g. &
 &''B10004,B12001''')

! option for displaying/processing
options(10) = op_option_new('d', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
ldisplay = .FALSE.
options(11) = op_option_new(' ', 'comp-regularize', comp_regularize, help= &
 'regularize the time series keeping only the data at regular time intervals')
comp_regularize = .FALSE.
options(12) = op_option_new(' ', 'comp-average', comp_average, help= &
 'recompute average of average fields on a different time interval')
comp_average = .FALSE.
options(13) = op_option_new(' ', 'comp-cumulate', comp_cumulate, help= &
 'recompute cumulation of accumulated fields on a different time interval')
comp_cumulate = .FALSE.
options(14) = op_option_new(' ', 'comp-interval', comp_interval, '0000000001 00:00:00.000', help= &
 'length of regularization, average or cumulation interval in the format &
 &''DDDDDDDDDD hh:mm:ss.msc''')
options(15) = op_option_new(' ', 'comp-start', comp_start, '', help= &
 'start of regularization, average or cumulation period, an empty value means &
 &take the initial period of the available data; the format is the same as for &
 &--start-date parameter')



! options for defining output
!options(20) = op_option_new('o', 'output-file', output_file, '-', help= &
! 'output file, ''-'' for stdout')
options(21) = op_option_new(' ', 'output-format', output_format, 'native', help= &
 'format of output file, ''native'' for vol7d native binary format&
#ifdef HAVE_DBALLE
 &, ''BUFR'' for BUFR with generic template, ''CREX'' for CREX format&
#endif
 &, csv for formatted csv output')
#ifdef HAVE_DBALLE
options(22) = op_option_new('t', 'output-template', output_template, 'generic', help= &
 'output TEMPLATE for BUFR/CREX, in the form ''category.subcategory.localcategory'', or &
& an alias like ''synop'', ''metar'',''temp'',''generic''')
#endif

! options for configuring csv output
options(30) = op_option_new(' ', 'csv-volume', csv_volume, 'all', help= &
 'vol7d volumes to be output to csv: ''all'' for all volumes, &
 &''ana'' for station volumes only or ''data'' for data volumes only')
options(31) = op_option_new(' ', 'csv-column', csv_column, 'time,timerange,ana,level', help= &
 'list of columns (excluding variables) that have to appear in csv output: &
 &a comma-separated combination of ''time,timerange,level,ana,network'' &
 &in the desired order')
options(32) = op_option_new(' ', 'csv-variable', csv_variable, 'all', help= &
 'list of variables that have to appear in the data columns of csv output: &
 &''all'' or a comma-separated list of B-table alphanumeric codes, e.g. &
 &''B10004,B12001'' in the desired order')
options(33) = op_option_new(' ', 'csv-header', csv_header, 2, help= &
 'write 0 to 2 header lines at the beginning of csv output')
options(34) = op_option_new(' ', 'csv-skip-miss', csv_skip_miss, help= &
 'skip records containing only missing values in csv output')
csv_skip_miss = .FALSE.
options(35) = op_option_new(' ', 'csv-rescale', csv_rescale, help= &
 'rescale integer variables according to its scale factor in output')
csv_rescale = .FALSE.

! help option
options(40) = op_option_help_new('h', 'help', help= &
 'show an help message')

! define the option parser
opt = optionparser_new(options, description_msg= &
 'Vol7d transformation application, it imports a vol7d volume from a &
 &native vol7d file&
#ifdef HAVE_DBALLE
 &, from a dbAll.e database, from a BUFR/CREX file&
#endif
#ifdef HAVE_ORSIM
 &, from SIM Oracle database&
#endif
 & and exports it into a native v7d file&
#ifdef HAVE_DBALLE
 &, or into a BUFR/CREX file&
#endif
 &, or into a configurable formatted csv file. &
 &If input-format is of file type, inputfile ''-'' indicates stdin, &
 &if input-format is of database type, inputfile specifies &
 &database access info in the form user/password@dsn, &
 &if empty or ''-'', a suitable default is used. &
 &If output-format is of file type, outputfile ''-'' indicates stdout.', &
 usage_msg='v7d_transform [options] inputfile outputfile')

! parse options and check for errors
optind = optionparser_parseoptions(opt)
IF (optind <= 0) THEN
  CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters')
  CALL EXIT(1)
ENDIF

IF ( optind <= iargc()) THEN
  CALL getarg(optind, input_file)
  optind=optind+1
ELSE
  CALL l4f_category_log(category,L4F_ERROR,'input file missing')
  CALL optionparser_printhelp(opt)
  CALL EXIT(1)
ENDIF

IF (optind <= iargc()) THEN
  CALL getarg(optind, output_file)
  optind=optind+1
ELSE
  CALL l4f_category_log(category,L4F_ERROR,'output file missing')
  CALL optionparser_printhelp(opt)
  CALL EXIT(1)
ENDIF

! generate network
IF (LEN_TRIM(network_list) > 0) THEN
  n = word_split(network_list, w_s, w_e, ',')
  ALLOCATE(nl(n))
  DO i = 1, n
    CALL init(nl(i), name=network_list(w_s(i):w_e(i)))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF
! generate variable
IF (LEN_TRIM(variable_list) > 0) THEN
  n = word_split(variable_list, w_s, w_e, ',')
  ALLOCATE(vl(n))
  DO i = 1, n
    vl(i) = variable_list(w_s(i):w_e(i))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF
CALL init(s_d, isodate=start_date)
CALL init(e_d, isodate=end_date)
c_i = timedelta_new(isodate=comp_interval)
IF (comp_start /= '') THEN
  c_s = datetime_new(isodate=comp_start)
ELSE
  c_s = datetime_miss
ENDIF

! check csv-column
nc = word_split(csv_column, w_s, w_e, ',')
j = 0
icsv_column(:) = -1
DO i = 1, MIN(nc, SIZE(icsv_column))
  SELECT CASE(csv_column(w_s(i):w_e(i)))
  CASE('time')
    j = j + 1
    icsv_column(j) = vol7d_time_d
  CASE('timerange')
    j = j + 1
    icsv_column(j) = vol7d_timerange_d
  CASE('level')
    j = j + 1
    icsv_column(j) = vol7d_level_d
  CASE('ana')
    j = j + 1
    icsv_column(j) = vol7d_ana_d
  CASE('network')
    j = j + 1
    icsv_column(j) = vol7d_network_d
  CASE default
    CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters, column '// &
     csv_column(w_s(i):w_e(i))//' in --csv-column not valid.')
    CALL EXIT(1)
  END SELECT
END DO
nc = j
DEALLOCATE(w_s, w_e)

! import data from source
IF (input_format == 'native') THEN
  IF (input_file == '-') THEN
    iun = stdin_unit
  ELSE
    iun = getunit()
    OPEN(iun, file=input_file, form='UNFORMATTED', access='SEQUENTIAL')
  ENDIF
  CALL init(v7d, time_definition=0)
  CALL import(v7d, unit=iun)
  IF (input_file /= '-') THEN
    CLOSE(iun)
  ENDIF

#ifdef HAVE_DBALLE
ELSE IF (input_format == 'BUFR' .OR. input_format == 'CREX') THEN
  IF (input_file == '-') THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, stdin not supported for'// &
     TRIM(input_format)//' input format.')
    CALL EXIT(1)
  ELSE
    CALL init(v7d_dba, filename=input_file, FORMAT=input_format, file=.TRUE.)
    CALL import(v7d_dba)
    v7d = v7d_dba%vol7d
  ENDIF

ELSE IF (input_format == 'dba') THEN
  IF (.NOT.ALLOCATED(nl) .OR. .NOT.ALLOCATED(vl)) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, it is necessary to provide --network-list &
     &and --variable-list with dbAll.e source.')
    CALL EXIT(1)
  ENDIF
  CALL init(v7d_dba, file=.FALSE.) ! dsn, user, password? repinfo??
  CALL import(v7d_dba, vl, nl, timei=s_d, timef=e_d)
  v7d = v7d_dba%vol7d
#endif

#ifdef HAVE_ORSIM
ELSE IF (input_format == 'orsim') THEN
  IF (.NOT.ALLOCATED(nl) .OR. .NOT.ALLOCATED(vl)) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, it is necessary to provide --network-list &
     &and --variable-list with SIM Oracle source.')
    CALL EXIT(1)
  ENDIF
  CALL init(v7d_osim, time_definition=0) ! dsn, user, password? repinfo??
  CALL import(v7d_osim, vl, nl, timei=s_d, timef=e_d)
  v7d = v7d_osim%vol7d
#endif

ELSE
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, format '// &
   TRIM(input_format)//' in --input-format not valid or not supported.')
  CALL EXIT(1)
ENDIF

! displaying/processing
#ifdef HAVE_DBALLE
CALL vol7d_dballe_set_var_du(v7d)
#endif

IF (ldisplay) CALL display(v7d)

IF (comp_regularize) THEN
  CALL init(v7d_comp1)
  CALL vol7d_regularize_time(v7d, v7d_comp1, c_i, c_s)
  CALL delete(v7d)
  v7d = v7d_comp1
ENDIF

IF (comp_average .OR. comp_cumulate) THEN
  CALL init(v7d_comp1, time_definition=v7d%time_definition)
  CALL init(v7d_comp2, time_definition=v7d%time_definition)
  IF (comp_average) THEN
    CALL vol7d_average(v7d, v7d_comp1, c_i, c_s)
  ENDIF
  IF (comp_cumulate) THEN
    CALL vol7d_cumulate(v7d, v7d_comp2, c_i, c_s, full_steps=.TRUE.)
  ENDIF
! merge the tho computed fields and throw away the rest
! to be improved in vol7d_compute
  CALL delete(v7d)
  CALL vol7d_append(v7d_comp1, v7d_comp2, sort=.TRUE.)
  v7d = v7d_comp1
  CALL delete(v7d_comp2)
ENDIF

! output
IF (output_format == 'native') THEN
  IF (output_file == '-') THEN
    iun = stdout_unit
  ELSE
    iun = getunit()
    OPEN(iun, file=output_file, form='UNFORMATTED', access='SEQUENTIAL')
  ENDIF
  CALL export(v7d, unit=iun)
  IF (output_file /= '-') CLOSE(iun)
  CALL delete(v7d)

ELSE IF (output_format == 'csv') THEN
  IF (output_file == '-') THEN
    iun = stdout_unit
  ELSE
    iun = getunit()
    OPEN(iun, file=output_file, form='FORMATTED', access='SEQUENTIAL')
  ENDIF
  CALL csv_export(v7d, csv_volume, csv_variable, csv_header, csv_skip_miss, &
   csv_rescale, icsv_column, iun, nc)
  IF (output_file /= '-') CLOSE(iun)
  CALL delete(v7d)

#ifdef HAVE_DBALLE
ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX') THEN
  CALL init(v7d_dba_out, filename=output_file, FORMAT=output_format, file=.TRUE., &
   WRITE=.TRUE., wipe=.TRUE.)
  v7d_dba_out%vol7d = v7d
  CALL export(v7d_dba_out, template=output_template)
  CALL delete(v7d_dba_out)
#endif

ELSE IF (output_format /= '') THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, format '// &
   TRIM(output_format)//' in --output-format not valid or not supported.')
  CALL EXIT(1)
ENDIF

! cleanly close the databases
IF (input_format == 'native') THEN
  CALL delete(v7d)
#ifdef HAVE_DBALLE
ELSE IF (input_format == 'BUFR' .OR. input_format == 'CREX' &
 .OR. input_format == 'dba') THEN
  CALL delete(v7d_dba)
#endif
#ifdef HAVE_ORSIM
ELSE IF (input_format == 'orsim') THEN
  CALL delete(v7d_osim)
#endif
ENDIF

END PROGRAM v7d_transform


SUBROUTINE csv_export(v7d, csv_volume, csv_variable, csv_header, csv_skip_miss, &
 csv_rescale, icsv_column, iun, nc)
USE vol7d_class
USE file_utilities
IMPLICIT NONE
TYPE(vol7d),INTENT(inout) :: v7d
CHARACTER(len=8),INTENT(in) :: csv_volume
CHARACTER(len=512),INTENT(in) :: csv_variable
INTEGER,INTENT(in) :: csv_header 
LOGICAL,INTENT(in) :: csv_skip_miss, csv_rescale
INTEGER,INTENT(in) :: icsv_column(7)
INTEGER,INTENT(in) :: iun, nc

LOGICAL :: no_miss
CHARACTER(len=50) :: deshead(7)=(/'COORD    ','REFTIME  ','VERTLEV  ','TIMERANGE', &
 'VARIABLE ','NETWORK  ','ATTRIBUTE'/), desdata(7)
CHARACTER(len=128) :: charbuffer
TYPE(csv_record) :: csvline, csv_desdata(7)
INTEGER :: i, i1, i2, i3, i4, i5, i6, i7, nv
REAL(kind=fp_geo) :: l1, l2
INTEGER,POINTER :: w_s(:), w_e(:)


IF (csv_variable /= 'all') THEN
  nv = word_split(csv_variable, w_s, w_e, ',')
  CALL checkvar(v7d%dativar%r)
  CALL checkvar(v7d%dativar%d)
  CALL checkvar(v7d%dativar%i)
  CALL checkvar(v7d%dativar%b)
  CALL checkvar(v7d%dativar%c)
  CALL vol7d_reform(v7d, miss=.TRUE.) ! sort?
  DEALLOCATE(w_s, w_e)
ENDIF

IF (csv_header > 1) THEN
  CALL init(csvline)
  CALL csv_record_addfield(csvline, 'written by v7d_transform')
  WRITE(iun,'(A)')csv_record_getrecord(csvline)
  CALL delete(csvline)
ENDIF

IF (csv_header > 0) THEN
  CALL init(csvline)
  CALL csv_record_addfield(csvline, 'Date')
  CALL csv_record_addfield(csvline, 'Time range')
  CALL csv_record_addfield(csvline, 'P1')
  CALL csv_record_addfield(csvline, 'P2')
  CALL csv_record_addfield(csvline, 'Longitude')
  CALL csv_record_addfield(csvline, 'Latitude')
  CALL csv_record_addfield(csvline, 'Level1')
  CALL csv_record_addfield(csvline, 'L1')
  CALL csv_record_addfield(csvline, 'Level2')
  CALL csv_record_addfield(csvline, 'L2')
  CALL csv_record_addfield(csvline, 'Report')
!  DO i = 1, nc ! add header for the dimensions descriptors
!    CALL csv_record_addfield(csvline,TRIM(deshead(icsv_column(i))))
!  ENDDO
  IF (ASSOCIATED(v7d%dativar%r)) THEN
    DO i5 = 1, SIZE(v7d%dativar%r)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%r(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%dativar%d)) THEN
    DO i5 = 1, SIZE(v7d%dativar%d)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%d(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%dativar%i)) THEN
    DO i5 = 1, SIZE(v7d%dativar%i)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%i(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%dativar%b)) THEN
    DO i5 = 1, SIZE(v7d%dativar%b)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%b(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%dativar%c)) THEN
    DO i5 = 1, SIZE(v7d%dativar%c)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%c(i5)%btable))
    ENDDO
  ENDIF

  WRITE(iun,'(A)')csv_record_getrecord(csvline)
  CALL delete(csvline)
ENDIF

DO i = 1, SIZE(csv_desdata)
  CALL init(csv_desdata(i))
ENDDO

DO i2 = 1, size(v7d%time)
  CALL csv_record_rewind(csv_desdata(2))
  CALL getval(v7d%time(i2), isodate=charbuffer(1:19))
  CALL csv_record_addfield(csv_desdata(2), charbuffer(1:19))
  DO i4 = 1, SIZE(v7d%timerange)
    CALL csv_record_rewind(csv_desdata(4))
    CALL csv_record_addfield(csv_desdata(4), v7d%timerange(i4)%timerange)
    CALL csv_record_addfield(csv_desdata(4), v7d%timerange(i4)%p1)
    CALL csv_record_addfield(csv_desdata(4), v7d%timerange(i4)%p2)
!    desdata(4) = TRIM(to_char(v7d%timerange(i4)%timerange))//','// &
!     TRIM(to_char(v7d%timerange(i4)%p1))//','//TRIM(to_char(v7d%timerange(i4)%p2))
    DO i3 = 1, SIZE(v7d%level)
      CALL csv_record_rewind(csv_desdata(3))
      CALL csv_record_addfield(csv_desdata(3), v7d%level(i3)%level1)
      CALL csv_record_addfield(csv_desdata(3), v7d%level(i3)%l1)
      CALL csv_record_addfield(csv_desdata(3), v7d%level(i3)%level2)
      CALL csv_record_addfield(csv_desdata(3), v7d%level(i3)%l2)
!      desdata(3) = TRIM(to_char(v7d%level(i3)%level1))// &
!       ','//TRIM(to_char(v7d%level(i3)%l1))// &
!       ','//TRIM(to_char(v7d%level(i3)%level2))// &
!       ','//TRIM(to_char(v7d%level(i3)%l2))
      DO i6 = 1, SIZE(v7d%network)
        CALL csv_record_rewind(csv_desdata(6))
        CALL csv_record_addfield(csv_desdata(6), v7d%network(i6)%name) ! trim?
!        desdata(6) = v7d%network(i6)%name
        DO i1 = 1, SIZE(v7d%ana)
          CALL csv_record_rewind(csv_desdata(1))
          CALL getval(v7d%ana(i1)%coord, lon=l1, lat=l2)
          CALL csv_record_addfield(csv_desdata(1), l1)
          CALL csv_record_addfield(csv_desdata(1), l2)
!          desdata(1) = TRIM(to_char(l1))//','//TRIM(to_char(l2))
          no_miss = .FALSE.
          CALL init(csvline)
          DO i = 1, nc ! add data for the dimensions descriptors
            CALL csv_record_addfield(csvline,csv_desdata(icsv_column(i)))
          ENDDO
          IF (ASSOCIATED(v7d%voldatir)) THEN
            DO i5 = 1, SIZE(v7d%voldatir(i1,i2,i3,i4,:,i6))
              IF (c_e(v7d%voldatir(i1,i2,i3,i4,i5,i6))) THEN
                CALL csv_record_addfield(csvline,v7d%voldatir(i1,i2,i3,i4,i5,i6))
                no_miss = .TRUE.
              ELSE
                CALL csv_record_addfield(csvline,'')
              ENDIF
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatid)) THEN
            DO i5 = 1, SIZE(v7d%voldatid(i1,i2,i3,i4,:,i6))
              IF (c_e(v7d%voldatid(i1,i2,i3,i4,i5,i6))) THEN
                CALL csv_record_addfield(csvline,v7d%voldatid(i1,i2,i3,i4,i5,i6))
                no_miss = .TRUE.
              ELSE
                CALL csv_record_addfield(csvline,'')
              ENDIF
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatii)) THEN
            DO i5 = 1, SIZE(v7d%voldatii(i1,i2,i3,i4,:,i6))
              IF (c_e(v7d%voldatii(i1,i2,i3,i4,i5,i6))) THEN
                IF (csv_rescale .AND. c_e(v7d%dativar%i(i5)%scalefactor)) THEN
                  CALL csv_record_addfield(csvline, &
                   10.**(-v7d%dativar%i(i5)%scalefactor)* &
                   REAL(v7d%voldatii(i1,i2,i3,i4,i5,i6)))
                ELSE
                  CALL csv_record_addfield(csvline,v7d%voldatii(i1,i2,i3,i4,i5,i6))
                ENDIF
                no_miss = .TRUE.
              ELSE
                CALL csv_record_addfield(csvline,'')
              ENDIF
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatib)) THEN
            DO i5 = 1, SIZE(v7d%voldatib(i1,i2,i3,i4,:,i6))
              IF (c_e(v7d%voldatib(i1,i2,i3,i4,i5,i6))) THEN
                IF (csv_rescale .AND. c_e(v7d%dativar%b(i5)%scalefactor)) THEN
                  CALL csv_record_addfield(csvline, &
                   10.**(-v7d%dativar%b(i5)%scalefactor)* &
                   REAL(v7d%voldatib(i1,i2,i3,i4,i5,i6)))
                ELSE
                  CALL csv_record_addfield(csvline,INT(v7d%voldatib(i1,i2,i3,i4,i5,i6)))
                ENDIF
                no_miss = .TRUE.
              ELSE
                CALL csv_record_addfield(csvline,'')
              ENDIF
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatic)) THEN
            DO i5 = 1, SIZE(v7d%voldatic(i1,i2,i3,i4,:,i6))
              IF (c_e(v7d%voldatic(i1,i2,i3,i4,i5,i6))) THEN
                CALL csv_record_addfield(csvline,v7d%voldatic(i1,i2,i3,i4,i5,i6))
                no_miss = .TRUE.
              ELSE
                CALL csv_record_addfield(csvline,'')
              ENDIF
            ENDDO
          ENDIF
          IF (.NOT.csv_skip_miss .OR. no_miss) THEN
            WRITE(iun,'(A)')csv_record_getrecord(csvline)
          ENDIF
          CALL delete(csvline)
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

CONTAINS

SUBROUTINE checkvar(var)
TYPE(vol7d_var), POINTER :: var(:)

INTEGER :: i, j

IF (.NOT.ASSOCIATED(var)) RETURN

v7dvarloop: DO i = 1, SIZE(var)
  csvvarloop:  DO j = 1, nv
    IF (var(i)%btable == csv_variable(w_s(j):w_e(j))) THEN
      CYCLE v7dvarloop
    ENDIF
  ENDDO csvvarloop
  var(i) = vol7d_var_miss ! var not found, nullify
ENDDO v7dvarloop

END SUBROUTINE checkvar

END SUBROUTINE csv_export
