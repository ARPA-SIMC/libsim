!> Utilities for managing files. This module is a collection of generic utilities
!! for managing files. A group of utilities is dedicated to locating
!! and opening configuration files in standard directories or in
!! directories specified by environmental variables. The module also
!! contains the class \a csv_record for creating and interpreting the
!! records of a csv file.
!! \ingroup base
MODULE file_utilities
USE kinds
USE char_utilities
USE missing_values
USE optional_values
USE log4fortran
USE err_handling
IMPLICIT NONE

INTEGER, PARAMETER, PRIVATE :: nftype = 2
CHARACTER(len=16), PARAMETER, PRIVATE :: &
 pathlist(2,nftype) = RESHAPE((/ &
 '/usr/share      ', '/usr/local/share', &
 '/etc            ', '/usr/local/etc  ' /), &
 (/2,nftype/))
CHARACTER(len=6), PARAMETER, PRIVATE :: &
 filetypename(nftype) = (/ 'DATA  ', 'CONFIG' /)
INTEGER, PARAMETER :: filetype_data = 1 !< Data file requested
INTEGER, PARAMETER :: filetype_config = 2 !< Configuration file requested

CHARACTER(len=20) :: program_name='libsim', program_name_env='LIBSIM'

!> Class for interpreting the records of a csv file.
!! See http://en.wikipedia.org/wiki/Comma-separated_values for a
!! detailed description of the csv format.
TYPE csv_record
  PRIVATE
  INTEGER :: cursor, action, nfield !, ntotal
  INTEGER(KIND=int_b) :: csep, cquote
  INTEGER(KIND=int_b), POINTER :: record(:)
END TYPE csv_record

INTEGER, PARAMETER, PRIVATE :: csv_basereclen=1024, &
 csv_action_read=0, csv_action_write=1

!> Constructor for the class \a csv_record. It has to be called for every
!! record (line) csv to be created or interpreted.
INTERFACE init
  MODULE PROCEDURE csv_record_init
END INTERFACE

!> Destructor for the class \a csv_record. It is important to call
!! it before reusing the object for the following record, in order to
!! avoid memory leaks.
INTERFACE delete
  MODULE PROCEDURE csv_record_delete
END INTERFACE

!> Methods for successively obtaining the fields of a \a csv_record object.
!! The generic name \c csv_record_getfield with parameters of the
!! desired type should be used instead of the specific names, the
!! compiler will select the proper subroutine.
INTERFACE csv_record_getfield
  MODULE PROCEDURE csv_record_getfield_char, csv_record_getfield_int, &
   csv_record_getfield_real, csv_record_getfield_double
END INTERFACE

!> Methods for successively adding fields to a \a csv_record object.
!! The generic name \c csv_record_addfield with parameters of the
!! desired type should be used instead of the specific names, the
!! compiler will select the proper subroutine. Missing values are
!! literally inserted in the output without special treatment.
INTERFACE csv_record_addfield
  MODULE PROCEDURE csv_record_addfield_char, csv_record_addfield_int, &
   csv_record_addfield_real, csv_record_addfield_double, &
   csv_record_addfield_csv_record
END INTERFACE

!> Methods for successively adding fields to a \a csv_record object.
!! The generic name \c csv_record_addfield with parameters of the
!! desired type should be used instead of the specific names, the
!! compiler will select the proper subroutine. Missing values are
!! inserted as empty fields.
INTERFACE csv_record_addfield_miss
  MODULE PROCEDURE csv_record_addfield_char_miss, csv_record_addfield_int_miss, &
   csv_record_addfield_real_miss, csv_record_addfield_double_miss
END INTERFACE


PRIVATE csv_record_init, csv_record_delete, csv_record_getfield_char, &
 csv_record_getfield_int, csv_record_getfield_real, csv_record_getfield_double, &
 csv_record_addfield_char, csv_record_addfield_int, csv_record_addfield_real, &
 csv_record_addfield_double, csv_record_addfield_csv_record, &
 csv_record_addfield_char_miss, csv_record_addfield_int_miss, &
 csv_record_addfield_real_miss, csv_record_addfield_double_miss, &
 checkrealloc, add_byte

CONTAINS

!> Returns the number of a Fortran input/output unit currently unused.
!! It returns -1 in case of error. Example of use:
!! \code
!! USE file_utilities
!! ...
!! INTEGER :: n
!! ...
!! n=getunit()
!! IF (n /= -1) THEN
!!   OPEN(n, FILE='ostregheta.txt')
!!   ...
!! \endcode
FUNCTION getunit() RESULT(unit)
INTEGER :: unit

LOGICAL :: op

DO unit = 100, 32767
  INQUIRE(unit, opened=op)
  IF (.NOT. op) RETURN
ENDDO

CALL l4f_log(L4F_ERROR, 'Too many open files')
CALL raise_error()
unit = -1

END FUNCTION getunit

!> Looks for a specific file for the libsim package.
!! It searches in different directories in the following order:
!!  - directory specified by the environmental variabile \c LIBSIM_DATA for data files or \c LIBSIM_CONFIG for configuration files, if defined
!!  - directory \c /usr/share/libsim for data files or \c /etc/libsim for configuration files
!!  - directory \c /usr/local/share/libsim for data files or \c /usr/local/etc/libsim for configuration files.
!! It returns the full path to the existing file or an empty string if not found.
FUNCTION get_package_filepath(filename, filetype) RESULT(path)
CHARACTER(len=*), INTENT(in) :: filename !< name of the file to be searched, it must be a relative path name
INTEGER, INTENT(in) :: filetype !< type of file, the constants \a ::filetype_data or \a ::filetype_config have to be used

INTEGER :: i, j
CHARACTER(len=512) :: path
LOGICAL :: exist

IF (program_name == ' ') THEN
  CALL getarg(0, program_name)
  ! program_name_env?
ENDIF

IF (filetype < 1 .OR. filetype > nftype) THEN
  path = ''
  CALL l4f_log(L4F_ERROR, 'package file type '//TRIM(to_char(filetype))// &
   ' not valid')
  CALL raise_error()
  RETURN
ENDIF

! try with environment variable
CALL getenv(TRIM(program_name_env)//'_'//TRIM(filetypename(filetype)), path)
IF (path /= ' ') THEN

  path=TRIM(path)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL l4f_log(L4F_INFO, 'package file '//TRIM(path)//' found')
    RETURN
  ENDIF
ENDIF
! try with pathlist
DO j = 1, SIZE(pathlist,1)
  IF (pathlist(j,filetype) == ' ') EXIT
  path=TRIM(pathlist(j,filetype))//'/'//TRIM(program_name)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL l4f_log(L4F_INFO, 'package file '//TRIM(path)//' found')
    RETURN
  ENDIF
ENDDO
CALL l4f_log(L4F_ERROR, 'package file '//TRIM(filename)//' not found')
CALL raise_error()
path = ''

END FUNCTION get_package_filepath


!> Opens a specific file for the libsim package.
!! It searches in different directories in the following order:
!!  - directory specified by the environmental variabile \c LIBSIM_DATA for data files or \c LIBSIM_CONFIG for configuration files, if defined
!!  - directory \c /usr/share/libsim for data files or \c /etc/libsim for configuration files
!!  - directory \c /usr/local/share/libsim for data files or \c /usr/local/etc/libsim for configuration files
!! It returns the unit number associated to the file found and successfully opened,
!! or -1 if the file does not exist or an error occurred while opening it.
FUNCTION open_package_file(filename, filetype) RESULT(unit)
CHARACTER(len=*), INTENT(in) :: filename !< name of the file to be opened, it must be a relative path name
INTEGER, INTENT(in) :: filetype !< type of file, the constants \a ::filetype_data or \a ::filetype_config have to be used
INTEGER :: unit, i

CHARACTER(len=512) :: path

unit = -1
path=get_package_filepath(filename, filetype)
IF (path == '') RETURN

unit = getunit()
IF (unit == -1) RETURN

OPEN(unit, file=path, status='old', iostat = i)
IF (i == 0) THEN
  CALL l4f_log(L4F_INFO, 'package file '//TRIM(path)//' opened')
  RETURN
ENDIF

CALL l4f_log(L4F_ERROR, 'package file '//TRIM(filename)//' not found')
CALL raise_error()
unit = -1

END FUNCTION open_package_file


!> Interpreta una riga di un file csv.
!! Resa obsoleta dalla classe ::csv_record.
!! Restituisce in vdelim gli indici dei separatori (cdelim, default ',')
!! trovati nella stringa line (tipicamente la riga di un file csv)
!! per definizione: vdelim(1)=0, vdelim(size(vdelim))=LEN_TRIM(line)+1 .
!! Ignora le righe che iniziano con il carattere \c # .
!! Il valore restituito dalla funzione è:
!!  - 0 se tutto è andato bene
!!  - -1 se si tratta di una riga di commento
!!  - -2 se la riga contiene un numero di separatori maggiore della dimensione di
!!    \a vdelim, in tal caso sono assegnati solo quelli che ci stanno in \a vdelim.
!!
!! Esempio di utilizzo:
!! \code
!! INTEGER,PARAMETER :: nf=4 ! formato file
!! INTEGER :: i, sep(nf), n1, n2, un, i1, i2, i3
!! CHARACTER(len=512) :: line
!!
!! un = open_package_file('varmap.csv', filetype_data)
!! IF (un < 0) CALL raise_fatal_error('non trovo il file delle variabili')
!!
!! ...
!! i = 0
!! readline: DO WHILE(.TRUE.)
!!   READ(un,'(A)',END=120)line
!!   IF (delim_csv(line, sep) < 0) CYCLE readline
!!   i = i + 1
!!   READ(line(sep(1)+1:sep(2)-1),'(I8)')vartable(i)%varora
!!   READ(line(sep(2)+1:sep(3)-1),'(A)')vartable(i)%varbt
!!   READ(line(sep(3)+1:sep(4)-1),'(A)')vartable(i)%unit
!!  ENDDO readline
!! 120 CONTINUE
!! CALL print_info('Ho letto '//TRIM(to_char(i))//' variabili dalla tabella')
!! CLOSE(un)
!! \endcode
!! Per leggere un file tipo:
!! \code
!! 160,B13011,mm->KG/M**2
!! 220,B13011,mm/10->KG/M**2
!! 159,B13011,mm->KG/M**2
!! 159,B13011,mm->KG/M**2
!! \endcode
FUNCTION delim_csv(line, vdelim, cdelim) RESULT(status)
CHARACTER(len=*),INTENT(in) :: line !< riga in formato csv da interpretare
INTEGER,INTENT(out) :: vdelim(:) !< vettore degli indici dei separatori trovati in \a line, per definizione vdelim(1)=0, vdelim(size(vdelim))=LEN_TRIM(line)+1, deve essere dimansionato al numero di campi previsti in \a line +1
CHARACTER(len=1),INTENT(in),OPTIONAL :: cdelim !< carattere da interpretare come delimitatore, default \c ','
INTEGER :: status

CHARACTER (len=1) :: cldelim
INTEGER :: j

IF (line(1:1) == '#') THEN ! Commento
  status = -1
  RETURN
ENDIF
IF (PRESENT(cdelim)) THEN
  cldelim = cdelim
ELSE
  cldelim = ','
ENDIF

vdelim(1) = 0
DO j = 2, SIZE(vdelim)-1
  vdelim(j) = INDEX(line(vdelim(j-1)+1:), cldelim) + vdelim(j-1)
  IF (vdelim(j) == vdelim(j-1)) THEN ! N. di record insufficiente
    vdelim(j+1:) = vdelim(j)
    status = -2
    RETURN
  ENDIF
ENDDO
status = 0
vdelim(j) = LEN_TRIM(line) + 1

END FUNCTION delim_csv

!> Initialise a \a csv_record object.
!! If the record is provided in input, the object is used for decoding a
!! record read from a file (\a csv_record_getfield methods),
!! if record is not provided, then the object will be used for
!! coding a csv record (\a csv_record_addfield methods), for the
!! successive write on file.
!! It is possible to specify nonstandard characters for delimiting
!! and grouping fields, default comma (,) and double quote (").
!! In case of decoding, it is possible to obtain in output the number of fields
!! in the record, but this will take extra computing time. As an alternative,
!! the ::csv_record_end method can be used when extracting each field.
!! Warning: the \a csv_record class does not handle csv records that extend
!! on different lines.
SUBROUTINE csv_record_init(this, record, csep, cquote, nfield)
TYPE(csv_record),INTENT(INOUT) :: this !< object to be initialised
CHARACTER(len=*),INTENT(IN), OPTIONAL :: record !< csv record to be interpreted, if not provided, it means we want to code a csv record for output
CHARACTER(len=1),INTENT(IN),OPTIONAL :: csep !< field separator character, default \c , (comma)
CHARACTER(len=1),INTENT(IN),OPTIONAL :: cquote !< field grouping character, default \c " (double quote); it is usually used when a field contains comma or blanks
INTEGER,INTENT(OUT),OPTIONAL :: nfield !< number of fields in the record

INTEGER :: l

IF (PRESENT(csep)) THEN
  this%csep = TRANSFER(csep, this%csep)
ELSE
  this%csep = TRANSFER(',', this%csep)
ENDIF
IF (PRESENT(cquote)) THEN
  this%cquote = TRANSFER(cquote, this%cquote)
ELSE
  this%cquote = TRANSFER('"', this%cquote)
ENDIF

this%cursor = 1
this%nfield = 0
IF (PRESENT(record)) THEN
  l = LEN_TRIM(record)
  ALLOCATE(this%record(l))
  this%record(:) = TRANSFER(record, this%record, l) ! ice in pgf90 with TRIM(record)

  IF (PRESENT(nfield)) THEN
    nfield = 0
    DO WHILE(.NOT.csv_record_end(this)) ! faccio un giro a vuoto sul record
      nfield = nfield + 1
      CALL csv_record_getfield(this)
    ENDDO
    this%cursor = 1 ! riazzero il cursore
  ENDIF
ELSE
  ALLOCATE(this%record(csv_basereclen))
ENDIF

END SUBROUTINE csv_record_init


!> Destroy the \a csv_record object, freeing allocated memory.
SUBROUTINE csv_record_delete(this)
TYPE(csv_record), INTENT(INOUT) :: this !< object to be destroyed

DEALLOCATE(this%record)

END SUBROUTINE csv_record_delete


!> Rewind the pointer in order to allow rescan or rewrite of the same record.
SUBROUTINE csv_record_rewind(this)
TYPE(csv_record),INTENT(INOUT) :: this !< object to be rewound

this%cursor = 1
this%nfield = 0

END SUBROUTINE csv_record_rewind


!> Add a field from a \c CHARACTER variable to the csv record \a this.
!! The field will be quoted if necessary.
!! \todo Improve the trailing blank quoting.
SUBROUTINE csv_record_addfield_char(this, field, form, force_quote)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add field
CHARACTER(len=*),INTENT(IN) :: field !< field to be added
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format, ignored by now
LOGICAL, INTENT(in), OPTIONAL :: force_quote !< if provided and \c .TRUE. , the field will be quoted even if not necessary

INTEGER :: i
LOGICAL :: lquote

lquote = optio_log(force_quote)
IF (LEN(field) == 0) THEN ! Particular case to be handled separately
  CALL checkrealloc(this, 1)
  IF (this%nfield > 0) THEN
    CALL add_byte(this, this%csep) ! add separator if necessary
  ELSE
    CALL add_byte(this, this%cquote) ! if first record is empty it should be quoted
    CALL add_byte(this, this%cquote) ! in case it is the only one
  ENDIF
ELSE IF (INDEX(field, TRANSFER(this%csep,field(1:1))) == 0 &
 .AND. INDEX(field, TRANSFER(this%cquote,field(1:1))) == 0 &
 .AND. .NOT.is_space_c(field(1:1)) &
 .AND. .NOT.is_space_c(field(LEN(field):LEN(field))) &
 .AND. .NOT.lquote) THEN ! quote not required
  CALL checkrealloc(this, LEN(field)+1)
  IF (this%nfield > 0) CALL add_byte(this, this%csep) ! add separator if necessary
  this%record(this%cursor:this%cursor+LEN(field)-1) = TRANSFER(field, this%record)
  this%cursor = this%cursor + LEN(field)
ELSE ! quote required
  CALL checkrealloc(this, 2*LEN(field)+3) ! worst case """""""""
  IF (this%nfield > 0) CALL add_byte(this, this%csep) ! add separator if necessary
  CALL add_byte(this, this%cquote) ! add quote
  DO i = 1, LEN(field)
    CALL add_char(field(i:i))
  ENDDO
  CALL add_byte(this, this%cquote) ! add quote
ENDIF

this%nfield = this%nfield + 1

CONTAINS

! add a character, doubling it if it's a quote
SUBROUTINE add_char(char)
CHARACTER(len=1) :: char

this%record(this%cursor) = TRANSFER(char, this%record(1))
this%cursor = this%cursor+1
IF (this%record(this%cursor-1) == this%cquote) THEN ! double the quote
  this%record(this%cursor) = this%cquote
  this%cursor = this%cursor+1
ENDIF

END SUBROUTINE add_char

END SUBROUTINE csv_record_addfield_char


! Reallocate record if necessary
SUBROUTINE checkrealloc(this, enlarge)
TYPE(csv_record),INTENT(INOUT) :: this
INTEGER, INTENT(in) :: enlarge

INTEGER(KIND=int_b), POINTER :: tmpptr(:)

IF (this%cursor+enlarge > SIZE(this%record)) THEN
  ALLOCATE(tmpptr(SIZE(this%record)+MAX(csv_basereclen, enlarge)))
  tmpptr(1:SIZE(this%record)) = this%record(:)
  DEALLOCATE(this%record)
  this%record => tmpptr
ENDIF

END SUBROUTINE checkrealloc


! add a byte
SUBROUTINE add_byte(this, char)
TYPE(csv_record),INTENT(INOUT) :: this
INTEGER(kind=int_b) :: char

this%record(this%cursor) = char
this%cursor = this%cursor+1

END SUBROUTINE add_byte


!> Add a field from a \c CHARACTER variable to the csv record \a this.
!! The field will be quoted if necessary. A missing value is inserted
!! as an empty field.
SUBROUTINE csv_record_addfield_char_miss(this, field, form, force_quote)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add field
CHARACTER(len=*),INTENT(IN) :: field !< field to be added
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format, ignored by now
LOGICAL, INTENT(in), OPTIONAL :: force_quote !< if provided and \c .TRUE. , the field will be quoted even if not necessary

IF (c_e(field)) THEN
  CALL csv_record_addfield(this, field, form, force_quote=force_quote)
ELSE
  CALL csv_record_addfield(this, '')
ENDIF

END SUBROUTINE csv_record_addfield_char_miss


!> Add a field from an \c INTEGER variable to the csv record \a this.
!! The field will be quoted if necessary.
SUBROUTINE csv_record_addfield_int(this, field, form, force_quote)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add field
INTEGER,INTENT(IN) :: field !< field to be added
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format
LOGICAL, INTENT(in), OPTIONAL :: force_quote !< if provided and \c .TRUE. , the field will be quoted even if not necessary

CALL csv_record_addfield(this, TRIM(to_char(field, form)), force_quote=force_quote)

END SUBROUTINE csv_record_addfield_int


!> Add a field from an \c INTEGER variable to the csv record \a this.
!! The field will be quoted if necessary. A missing value is inserted
!! as an empty field.
SUBROUTINE csv_record_addfield_int_miss(this, field, form, force_quote)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add field
INTEGER,INTENT(IN) :: field !< field to be added
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format
LOGICAL, INTENT(in), OPTIONAL :: force_quote !< if provided and \c .TRUE. , the field will be quoted even if not necessary

IF (c_e(field)) THEN
  CALL csv_record_addfield(this, TRIM(to_char(field, form)), force_quote=force_quote)
ELSE
  CALL csv_record_addfield(this, '')
ENDIF

END SUBROUTINE csv_record_addfield_int_miss


!> Add a field from a \c REAL variable to the csv record \a this.
!! The field will be quoted if necessary.
SUBROUTINE csv_record_addfield_real(this, field, form, force_quote)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add field
REAL,INTENT(IN) :: field !< field to be added
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format
LOGICAL, INTENT(in), OPTIONAL :: force_quote !< if provided and \c .TRUE. , the field will be quoted even if not necessary

CALL csv_record_addfield(this, TRIM(to_char(field, form)), force_quote=force_quote)

END SUBROUTINE csv_record_addfield_real


!> Add a field from a \c REAL variable to the csv record \a this.
!! The field will be quoted if necessary. A missing value is inserted
!! as an empty field.
SUBROUTINE csv_record_addfield_real_miss(this, field, form, force_quote)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add field
REAL,INTENT(IN) :: field !< field to be added
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format
LOGICAL, INTENT(in), OPTIONAL :: force_quote !< if provided and \c .TRUE. , the field will be quoted even if not necessary

IF (c_e(field)) THEN
  CALL csv_record_addfield(this, TRIM(to_char(field, form)), force_quote=force_quote)
ELSE
  CALL csv_record_addfield(this, '')
ENDIF

END SUBROUTINE csv_record_addfield_real_miss


!> Add a field from a \c DOUBLE PRECISION variable to the csv record \a this.
!! The field will be quoted if necessary.
SUBROUTINE csv_record_addfield_double(this, field, form, force_quote)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add field
DOUBLE PRECISION,INTENT(IN) :: field !< field to be added
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format
LOGICAL, INTENT(in), OPTIONAL :: force_quote !< if provided and \c .TRUE. , the field will be quoted even if not necessary

CALL csv_record_addfield(this, TRIM(to_char(field, form)), force_quote=force_quote)

END SUBROUTINE csv_record_addfield_double


!> Add a field from a \c DOUBLE PRECISION variable to the csv record \a this.
!! The field will be quoted if necessary. A missing value is inserted
!! as an empty field.
SUBROUTINE csv_record_addfield_double_miss(this, field, form, force_quote)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add field
DOUBLE PRECISION,INTENT(IN) :: field !< field to be added
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format
LOGICAL, INTENT(in), OPTIONAL :: force_quote !< if provided and \c .TRUE. , the field will be quoted even if not necessary

IF (c_e(field)) THEN
  CALL csv_record_addfield(this, TRIM(to_char(field, form)), force_quote=force_quote)
ELSE
  CALL csv_record_addfield(this, '')
ENDIF

END SUBROUTINE csv_record_addfield_double_miss


!> Add a full \a csv_record object to the csv record \a this.
!! The object to be added must have been generated through \a
!! csv_record_addfield calls (csv encoding mode). Both \a csv_record
!! objects \a this and \a record must use the same delimiter and
!! quoting characters, otherwise the operation will silently fail.
SUBROUTINE csv_record_addfield_csv_record(this, record)
TYPE(csv_record),INTENT(INOUT) :: this !< object where to add record
TYPE(csv_record),INTENT(IN) :: record !< record to be added

IF (this%csep /= record%csep .OR. this%cquote /= record%cquote) RETURN ! error
CALL checkrealloc(this, record%cursor)
IF (this%nfield > 0) CALL add_byte(this, this%csep)
this%record(this%cursor:this%cursor+record%cursor-2) = &
 record%record(1:record%cursor-1)
this%cursor = this%cursor + record%cursor-1
this%nfield = this%nfield + record%nfield

END SUBROUTINE csv_record_addfield_csv_record


!> Return current csv-coded record as a \a CHARACTER variable, ready to be written
!! to a file. It is not necessary to trim the result for trailing blanks.
FUNCTION csv_record_getrecord(this, nfield)
TYPE(csv_record),INTENT(IN) :: this !< object to be coded, the object is not modified, so that other fields can still be added after the call to ::csv_record_getrecord
INTEGER, INTENT(out), OPTIONAL :: nfield !< number of fields contained in the record

CHARACTER(len=this%cursor-1) :: csv_record_getrecord

csv_record_getrecord = TRANSFER(this%record(1:this%cursor-1), csv_record_getrecord)
IF (present(nfield)) nfield = this%nfield

END FUNCTION csv_record_getrecord


!> Returns next field from the record \a this as a \c CHARACTER variable.
!! The field pointer is advanced to the next field.
!! If all the fields have already been interpreted it returns an empty string
!! anyway; in order to verify the end-of-record condition the \a ier parameter
!! must be used.
SUBROUTINE csv_record_getfield_char(this, field, flen, ier)
TYPE(csv_record),INTENT(INOUT) :: this !< object to be decoded
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: field !< contents of the field, if not provided, the field pointer is increased only; if the variable is not long enough, a warning is printed and the part that fits is returned;
!< the variable is space-terminated anyway, so the \a flen parameter has to be used in order to evaluate possible significant trailing spaces
INTEGER,INTENT(OUT),OPTIONAL :: flen !< actual length of the field including trailing blaks, it is correctly computed also when \a field is not provided or too short
INTEGER,INTENT(OUT),OPTIONAL :: ier!< error code, 0 = OK, 1 = \a field too short, 2 = end of record

LOGICAL :: inquote, inpre, inpost, firstquote
INTEGER :: i, ocursor, ofcursor, lier

IF (PRESENT(field)) field = ''
IF (PRESENT(ier)) ier = 0
IF (csv_record_end(this)) THEN
  IF (PRESENT(ier)) ier = 2
  CALL l4f_log(L4F_ERROR, &
   'in csv_record_getfield, attempt to read past end of record')
  CALL raise_error()
  RETURN
ENDIF
lier = 0
ocursor = 0
ofcursor = 0
inquote = .FALSE.
inpre = .TRUE.
inpost = .FALSE.
firstquote = .FALSE.

DO i = this%cursor, SIZE(this%record)
  IF (inpre) THEN ! sono nel preludio, butto via gli spazi
    IF (is_space_b(this%record(i))) THEN
      CYCLE
    ELSE
      inpre = .FALSE.
    ENDIF
  ENDIF

  IF (.NOT.inquote) THEN ! fuori da " "
    IF (this%record(i) == this%cquote) THEN ! ": inizia " "
      inquote = .TRUE.
      CYCLE
    ELSE IF (this%record(i) == this%csep) THEN ! ,: fine campo
      EXIT
    ELSE ! carattere normale, elimina "trailing blanks"
      CALL add_char(this%record(i), .TRUE., field, ier)
      CYCLE
    ENDIF
  ELSE ! dentro " "
    IF (.NOT.firstquote) THEN ! il precedente non e` "
      IF (this%record(i) == this%cquote) THEN ! ": fine " " oppure ""
        firstquote = .TRUE.
        CYCLE
      ELSE ! carattere normale
        CALL add_char(this%record(i), .FALSE., field, ier)
        CYCLE
      ENDIF
    ELSE ! il precedente e` "
      firstquote = .FALSE.
      IF (this%record(i) == this%cquote) THEN ! ": sequenza ""
        CALL add_char(this%cquote, .FALSE., field, ier)
        CYCLE
      ELSE ! carattere normale: e` terminata " "
        inquote = .FALSE.
        IF (this%record(i) == this%csep) THEN ! , fine campo
          EXIT
        ELSE ! carattere normale, elimina "trailing blanks"
          CALL add_char(this%record(i), .TRUE., field, ier)
          CYCLE
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDDO

this%cursor = MIN(i + 1, SIZE(this%record) + 1)
IF (PRESENT(flen)) flen = ofcursor ! restituisco la lunghezza
IF (PRESENT(field)) THEN ! controllo overflow di field
  IF (ofcursor > LEN(field)) THEN
    IF (PRESENT(ier)) ier = 1
    CALL l4f_log(L4F_WARN, &
     'in csv_record_getfield, CHARACTER variable too short for field: '// &
     TRIM(to_char(LEN(field)))//'/'//TRIM(to_char(ocursor)))
  ENDIF
ENDIF

CONTAINS

SUBROUTINE add_char(char, check_space, field, ier)
INTEGER(kind=int_b) :: char
LOGICAL,INTENT(IN) :: check_space
CHARACTER(len=*),INTENT(OUT),OPTIONAL :: field
INTEGER,INTENT(OUT),OPTIONAL :: ier

ocursor = ocursor + 1
IF (PRESENT(field)) THEN
  IF (ocursor <= LEN(field)) THEN
    field(ocursor:ocursor) = TRANSFER(char, field)
  ENDIF
ENDIF
IF (check_space) THEN
  IF (.NOT.is_space_b(char)) ofcursor = ocursor
ELSE
  ofcursor = ocursor
ENDIF

END SUBROUTINE add_char

END SUBROUTINE csv_record_getfield_char


!> Returns next field from the record \a this as an \c INTEGER variable.
!! The field pointer is advanced to the next field.
!! If all the fields have already been interpreted or the field cannot be
!! interpreted as an integer, or if it is longer than 32 characters,
!! it returns a missing value.
SUBROUTINE csv_record_getfield_int(this, field, ier)
TYPE(csv_record),INTENT(INOUT) :: this !< object to be decoded
INTEGER,INTENT(OUT) :: field !< value of the field, = \a imiss if conversion fails
INTEGER,INTENT(OUT),OPTIONAL :: ier !< error code, 0 = OK, 1 = cannot convert to integer, 2 = end of record

CHARACTER(len=32) :: cfield
INTEGER :: lier

CALL csv_record_getfield(this, field=cfield, ier=lier)
IF (lier == 0 .AND. LEN_TRIM(cfield) /= 0) THEN
  READ(cfield, '(I32)', iostat=lier) field
  IF (lier /= 0) THEN
    lier = 1 ! standardize
    field = imiss
    CALL l4f_log(L4F_ERROR, &
     'in csv_record_getfield, invalid integer field: '//TRIM(cfield))
    CALL raise_error()
  ENDIF
ELSE
  field = imiss
ENDIF
IF (PRESENT(ier)) ier = lier

END SUBROUTINE csv_record_getfield_int


!> Returns next field from the record \a this as a \c REAL variable.
!! The field pointer is advanced to the next field.
!! If all the fields have already been interpreted or the field cannot be
!! interpreted as an integer, or if it is longer than 32 characters,
!! it returns a missing value.
SUBROUTINE csv_record_getfield_real(this, field, ier)
TYPE(csv_record),INTENT(INOUT) :: this !< object to be decoded
REAL,INTENT(OUT) :: field !< value of the field, = \a rmiss if conversion fails
INTEGER,INTENT(OUT),OPTIONAL :: ier!< error code, 0 = OK, 1 = cannot convert to integer, 2 = end of record

CHARACTER(len=32) :: cfield
INTEGER :: lier

CALL csv_record_getfield(this, field=cfield, ier=lier)
IF (lier == 0 .AND. LEN_TRIM(cfield) /= 0) THEN
  READ(cfield, '(F32.0)', iostat=lier) field
  IF (lier /= 0) THEN
    lier = 1 ! standardize
    field = rmiss
    CALL l4f_log(L4F_ERROR, &
     'in csv_record_getfield, invalid real field: '//TRIM(cfield))
    CALL raise_error()
  ENDIF
ELSE
  field = rmiss
ENDIF
IF (PRESENT(ier)) ier = lier

END SUBROUTINE csv_record_getfield_real


!> Returns next field from the record \a this as a \c DOUBLE PRECISION variable.
!! The field pointer is advanced to the next field.
!! If all the fields have already been interpreted or the field cannot be
!! interpreted as an integer, or if it is longer than 32 characters,
!! it returns a missing value.
SUBROUTINE csv_record_getfield_double(this, field, ier)
TYPE(csv_record),INTENT(INOUT) :: this !< object to be decoded
DOUBLE PRECISION,INTENT(OUT) :: field !< value of the field, = \a dmiss if conversion fails
INTEGER,INTENT(OUT),OPTIONAL :: ier!< error code, 0 = OK, 1 = cannot convert to integer, 2 = end of record

CHARACTER(len=32) :: cfield
INTEGER :: lier

CALL csv_record_getfield(this, field=cfield, ier=lier)
IF (lier == 0 .AND. LEN_TRIM(cfield) /= 0) THEN
  READ(cfield, '(F32.0)', iostat=lier) field
  IF (lier /= 0) THEN
    lier = 1 ! standardize
    field = dmiss
    CALL l4f_log(L4F_ERROR, &
     'in csv_record_getfield, invalid double precision field: '//TRIM(cfield))
    CALL raise_error()
  ENDIF
ELSE
  field = dmiss
ENDIF
IF (PRESENT(ier)) ier = lier

END SUBROUTINE csv_record_getfield_double


!> Tells whether end of record was reached (\c .TRUE.)
!! or there are still some fields left (\c .FALSE.).
FUNCTION csv_record_end(this)
TYPE(csv_record), INTENT(IN) :: this !< object to be checked for end of record
LOGICAL :: csv_record_end

csv_record_end = this%cursor > SIZE(this%record)

END FUNCTION csv_record_end


FUNCTION is_space_c(char) RESULT(is_space)
CHARACTER(len=1) :: char
LOGICAL :: is_space

is_space = (ICHAR(char) == 32 .OR. ICHAR(char) == 9) ! improve

END FUNCTION is_space_c


FUNCTION is_space_b(char) RESULT(is_space)
INTEGER(kind=int_b) :: char
LOGICAL :: is_space

is_space = (char == 32 .OR. char == 9) ! improve

END FUNCTION is_space_b


END MODULE file_utilities
