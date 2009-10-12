!> \brief Utilities for CHARACTER variables.
!!
!! This module is a collection of all-purpose utilities connected to
!! the use of CHARACTER variables, and text handling in general.
!!
!! Example of use:
!! \code
!! USE char_utilities
!! INTEGER :: j
!! ...
!! CALL raise_error('The value provided, '//TRIM(to_char(j))//', is too large')
!! ...
!! \endcode
!! \ingroup base
MODULE char_utilities
USE kinds
USE missing_values
IMPLICIT NONE

CHARACTER( * ), PRIVATE, PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
CHARACTER( * ), PRIVATE, PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

PRIVATE int_to_char, byte_to_char, char_to_char, &
   real_to_char, double_to_char, logical_to_char

!> Set of functions that return the input variable converted into
!! a string, using a default format suitable to provide a reasonable
!! representation of the input variable, or using a user-defined
!! format provided with the optional variable \a form.  The return
!! value may be quite long, in order to take into account all possible
!! cases, so it is suggested to trim the result with the intrinsic
!! function \a TRIM() before using it. Be warned that no check is
!! performed on the optional format \a form, so a runtime error may
!! occur if it is syntactically wrong or not suitable to the type of
!! data associated.
INTERFACE to_char
  MODULE PROCEDURE int_to_char, byte_to_char, char_to_char, &
   real_to_char, double_to_char, logical_to_char
END INTERFACE

!> Class that allows splitting a long line into shorter lines of equal
!! length at the occurrence of a specific character (typically a blank
!! space). All the members of the class are \a PRIVATE, all the
!! operations are performed through its methods.
TYPE line_split
  PRIVATE
  INTEGER :: align_type, ncols, nlines
  INTEGER, POINTER :: word_start(:), word_end(:)
  CHARACTER(len=1), POINTER :: paragraph(:,:)
END TYPE line_split

!> Destructor for the \a line_split class.
INTERFACE delete
  MODULE PROCEDURE line_split_delete
END INTERFACE

PRIVATE line_split_delete


CONTAINS

!> Version with integer argument, please use the generic \a to_char
!! rather than this function directly.
ELEMENTAL FUNCTION int_to_char(in, form) RESULT(char)
INTEGER,INTENT(in) :: in !< value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format

CHARACTER(len=11) :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(char,'(I0)') in
ENDIF

END FUNCTION int_to_char


!> Version with 1-byte integer argument, please use the generic \a to_char
!! rather than this function directly.
ELEMENTAL FUNCTION byte_to_char(in, form) RESULT(char)
INTEGER(kind=int_b),INTENT(in) :: in !< value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format

CHARACTER(len=4) :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(char,'(I0)') in
ENDIF

END FUNCTION byte_to_char


!> Version with character argument, please use the generic \a to_char
!! rather than this function directly. It is almost useless, just
!! provided for completeness.
FUNCTION char_to_char(in, form) result(char)
CHARACTER(len=*),INTENT(in) :: in !< value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format

CHARACTER(len=LEN_TRIM(in)) :: char
!fortran 2003
!CHARACTER(len=:),allocatable :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  char = TRIM(in)
ENDIF

END FUNCTION char_to_char


!> Version with single precision real argument, please use the generic
!! \a to_char rather than this function directly.
ELEMENTAL FUNCTION real_to_char(in, form) RESULT(char)
REAL,INTENT(in) :: in !< value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format

CHARACTER(len=15) :: char, tmpchar
INTEGER :: i

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(tmpchar,'(G15.9)') in
  DO i = 1, LEN(tmpchar)
    IF (tmpchar(i:i) /= ' ') EXIT
  ENDDO
  char = tmpchar(i:)
ENDIF

END FUNCTION real_to_char


!> Version with double precision real argument, please use the generic
!! \a to_char rather than this function directly.
ELEMENTAL FUNCTION double_to_char(in, form) RESULT(char)
DOUBLE PRECISION,INTENT(in) :: in !< value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format

CHARACTER(len=24) :: char, tmpchar
INTEGER :: i


IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(tmpchar,'(G24.17)') in
  DO i = 1, LEN(tmpchar)
    IF (tmpchar(i:i) /= ' ') EXIT
  ENDDO
  char = tmpchar(i:)
ENDIF

END FUNCTION double_to_char


!> Version with logical argument, please use the generic \a to_char
!! rather than this function directly.
ELEMENTAL FUNCTION logical_to_char(in, form) RESULT(char)
LOGICAL,INTENT(in) :: in !< value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< optional format

CHARACTER(len=1) :: char

!IF (PRESENT(form)) THEN
!  WRITE(char,form) in
!ELSE
WRITE(char,'(L1)') in
!ENDIF

END FUNCTION logical_to_char


!> Converts a \a CHARACTER variable into a string which can be
!! directly passed to a C function requiring a null-terminated \a
!! char* argument. If the result is going to be stored into an array,
!! it has to be dimensioned with a suitable size (\a LEN(fchar) \a +
!! \a 1 ).
FUNCTION fchar_to_cstr(fchar) RESULT(cstr)
CHARACTER(len=*), INTENT(in) :: fchar !< variable to be converted
!INTEGER(kind=int_b), POINTER, OPTIONAL :: pcstr(:) !< puntatore opzionale che può essere allocato per contenere il risultato, dovrà essere deallocato a carico del programma chiamante
INTEGER(kind=int_b) :: cstr(LEN(fchar)+1)

cstr(1:LEN(fchar)) = TRANSFER(fchar, cstr, LEN(fchar))
cstr(LEN(fchar)+1) = 0 ! Termino con zero
!IF (PRESENT(pcstr)) THEN
!  ALLOCATE(pcstr(LEN(fchar)+1))
!  pcstr = cstr
!ENDIF

END FUNCTION fchar_to_cstr


!> Converts a \a CHARACTER variable into a string which can be
!! directly passed to a C function requiring a null-terminated \a
!! char* argument. The result is stored int \a pcstr which is
!! allocated within the subroutine and has to be deallocated by the
!! calling procedure.
SUBROUTINE fchar_to_cstr_alloc(fchar, pcstr)
CHARACTER(len=*), INTENT(in) :: fchar !< variable to be converted
INTEGER(kind=int_b), POINTER :: pcstr(:) !< pointer to a 1-d byte array which will be allocated and, on output, will contain the null-terminated string

ALLOCATE(pcstr(LEN(fchar)+1))
pcstr(1:LEN(fchar)) = TRANSFER(fchar, pcstr, LEN(fchar))
pcstr(LEN(fchar)+1) = 0 ! Termino con zero

END SUBROUTINE fchar_to_cstr_alloc


!> Converts a null-terminated C-style string into a Fortran \a CHARACTER
!! variable of the same length, the null termination character is
!! removed.
FUNCTION cstr_to_fchar(cstr) RESULT(fchar)
INTEGER(kind=int_b), INTENT(in) :: cstr(:) !< variable to be converted
CHARACTER(len=SIZE(cstr)-1) :: fchar

INTEGER :: i

!l = MIN(LEN(char), SIZE(cstr)-1)
fchar = TRANSFER(cstr(1:SIZE(cstr)-1), fchar)
DO i = 1, SIZE(cstr)-1
  IF (fchar(i:i) == CHAR(0)) THEN ! truncate if the null terminator is found before
    fchar(i:) = ' '
    EXIT
  ENDIF
ENDDO

END FUNCTION cstr_to_fchar


!> Convert string to uppercase
FUNCTION UpperCase ( Input_String ) RESULT ( Output_String )
CHARACTER( * ), INTENT( IN ) :: Input_String !< strig to convert
CHARACTER( LEN( Input_String ) ) :: Output_String
                                ! -- Local variables
INTEGER :: i, n

                                ! -- Copy input string
Output_String = Input_String
                                ! -- Loop over string elements
DO i = 1, LEN( Output_String )
                                ! -- Find location of letter in lower case constant string
  n = INDEX( LOWER_CASE, Output_String( i:i ) )
                                ! -- If current substring is a lower case letter, make it upper case
  IF ( n /= 0 ) Output_String( i:i ) = UPPER_CASE( n:n )
END DO
END FUNCTION UpperCase

!> Convert string to lowercase
FUNCTION LowerCase ( Input_String ) RESULT ( Output_String )
                                ! -- Argument and result
CHARACTER( * ), INTENT( IN ) :: Input_String !< strig to convert
CHARACTER( LEN( Input_String ) ) :: Output_String
                                ! -- Local variables
INTEGER :: i, n

                                ! -- Copy input string
Output_String = Input_String
                                ! -- Loop over string elements
DO i = 1, LEN( Output_String )
                                ! -- Find location of letter in upper case constant string
  n = INDEX( UPPER_CASE, Output_String( i:i ) )
                                ! -- If current substring is an upper case letter, make it lower case
  IF ( n /= 0 ) Output_String( i:i ) = LOWER_CASE( n:n )
END DO
END FUNCTION LowerCase


!> Returns \a input_string aligned to the left (i.e. without leading blanks).
!! The needed number of trailing blanks is added at the end in order
!! to keep the length of the resulting string equal to the input
!! length.
FUNCTION align_left(input_string) RESULT(aligned)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be aligned

CHARACTER(len=LEN(input_string)) :: aligned

aligned = input_string(f_nblnk(input_string):)

END FUNCTION align_left


!> Returns \a input_string aligned to the right (i.e. without trailing blanks).
!! The needed number of leading blanks is added at the beginning in
!! order to keep the length of the resulting string equal to the input
!! length.
FUNCTION align_right(input_string) RESULT(aligned)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be aligned

CHARACTER(len=LEN(input_string)) :: aligned

aligned = ''
aligned(LEN(input_string)-l_nblnk(input_string)+1:) = input_string

END FUNCTION align_right


!> Returns \a input_string centered, i.e. with an equal number of 
!! leading and trailing blanks (±1 if they are odd).  The needed
!! number of leading blanks is added or removed at the beginning and
!! at the end in order to keep the length of the resulting string
!! equal to the input length.
FUNCTION align_center(input_string) RESULT(aligned)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be aligned

CHARACTER(len=LEN(input_string)) :: aligned

INTEGER :: n1, n2

n1 = f_nblnk(input_string)
n2 = LEN(input_string)-l_nblnk(input_string)+1

aligned = ''
aligned((n1+n2)/2:) = input_string(n1:)

END FUNCTION align_center


!> Return the index of last character in \a input_string which is not
!! a blank space. If the string is zero-length or contains only blank
!! spaces, zero is returned. It is named l_nblnk and not lnblnk in
!! order to avoid conflict with a nondefault intrinsic Fortran
!! function with the same name, available on some compilers.
FUNCTION l_nblnk(input_string, blnk) RESULT(nblnk)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be scanned
CHARACTER(len=1), OPTIONAL :: blnk !< optional blank character, if not provided, a blank space is assumed

CHARACTER(len=1) :: lblnk
INTEGER :: nblnk

IF (PRESENT(blnk)) THEN
  lblnk = blnk
ELSE
  lblnk = ' '
ENDIF

DO nblnk = LEN(input_string), 1, -1
  IF (input_string(nblnk:nblnk) /= lblnk) RETURN
ENDDO

END FUNCTION l_nblnk


!> Return the index of first character in \a input_string which is not
!! a blank space. If the string is zero-length or contains only blank
!! spaces, \a LEN(input_string)+1 is returned.
FUNCTION f_nblnk(input_string, blnk) RESULT(nblnk)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be scanned
CHARACTER(len=1), OPTIONAL :: blnk !< optional blank character, if not provided, a blank space is assumed

CHARACTER(len=1) :: lblnk
INTEGER :: nblnk

IF (PRESENT(blnk)) THEN
  lblnk = blnk
ELSE
  lblnk = ' '
ENDIF

DO nblnk = 1, LEN(input_string)
  IF (input_string(nblnk:nblnk) /= lblnk) RETURN
ENDDO

END FUNCTION f_nblnk


!> Split a line into words at a predefined character (default blank).
!! Returns the number of words in \a input_string. If pointers \a
!! word_start and \a word_end are provided, they are allocated with \a
!! nword elements and set to the indices of initial and final
!! character of every word in \a input_string. Groups of contiguous
!! separation characters are treated as single separator characters.
FUNCTION word_split(input_string, word_start, word_end, sep) RESULT(nword)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be scanned
INTEGER, POINTER, OPTIONAL :: word_start(:) !< indices of first character of each word in \a input_string, allocated here, must be deallocated by the user
INTEGER, POINTER, OPTIONAL :: word_end(:) !< indices of last character of each word in \a input_string, allocated here, must be deallocated by the user
CHARACTER(len=1), OPTIONAL :: sep !< optional word separator character, if not provided, a blank space is assumed

INTEGER :: nword

INTEGER :: ls, le
INTEGER, POINTER :: lsv(:), lev(:)
CHARACTER(len=1) :: lsep

IF (PRESENT(sep)) THEN
  lsep = sep
ELSE
  lsep = ' '
ENDIF

nword = 0
le = 0
DO WHILE(.TRUE.)
  ls = f_nblnk(input_string(le+1:), lsep) + le ! search next nonblank
  IF (ls > LEN(input_string)) EXIT ! end of words
  le = INDEX(input_string(ls:), lsep)
  IF (le == 0) THEN
    le = LEN(input_string)
  ELSE
    le = le + ls - 2
  ENDIF
  nword = nword + 1
ENDDO

IF (.NOT.PRESENT(word_start) .AND. .NOT.PRESENT(word_end)) RETURN

ALLOCATE(lsv(nword), lev(nword))
nword = 0
le = 0
DO WHILE(.TRUE.)
  ls = f_nblnk(input_string(le+1:), lsep) + le ! search next nonblank
  IF (ls > LEN(input_string)) EXIT ! end of words
  le = INDEX(input_string(ls:), lsep)
  IF (le == 0) THEN
    le = LEN(input_string)
  ELSE
    le = le + ls - 2
  ENDIF
  nword = nword + 1
  lsv(nword) = ls
  lev(nword) = le
ENDDO

IF (PRESENT(word_start)) THEN
  word_start => lsv
ELSE
  DEALLOCATE(lsv)
ENDIF
IF (PRESENT(word_end)) THEN
  word_end => lev
ELSE
  DEALLOCATE(lev)
ENDIF

END FUNCTION word_split


!> Constructor for the \a line_split class. It creates a new object
!! allowing to split a line of text into multiple lines of predefined
!! length at blank spaces. If a line can't be splitted because a word
!! is longer than the line, it is truncated.
FUNCTION line_split_new(line, ncols) RESULT(this)
CHARACTER(len=*), INTENT(in) :: line !< line to be splitted
INTEGER, INTENT(in), OPTIONAL :: ncols !< maximum number of columns on every line, if not provided a suitable default is used

TYPE(line_split) :: this

INTEGER :: nw, nwords, nlines, columns_in_line, words_in_line, ncols_next_word

IF (PRESENT(ncols)) THEN
  this%ncols = ncols
ELSE
  this%ncols = default_columns()
ENDIF
! split the input line
nwords = word_split(line, this%word_start, this%word_end)
! count the lines required to accomodate the input line in a paragraph
nlines = 0
nw = 0
DO WHILE(nw < nwords)
  columns_in_line = 0
  words_in_line = 0
  DO WHILE(nw < nwords)
    nw = nw + 1
    ncols_next_word = this%word_end(nw) - this%word_start(nw) + 1
    IF (words_in_line > 0) ncols_next_word = ncols_next_word + 1 ! previous space
    IF (columns_in_line + ncols_next_word <= this%ncols .OR. &
     words_in_line == 0) THEN ! accept the word
      columns_in_line = columns_in_line + ncols_next_word
      words_in_line = words_in_line + 1
    ELSE ! refuse the word
      nw = nw - 1
      EXIT
    ENDIF
  ENDDO
  nlines = nlines + 1
ENDDO

!IF (nlines == 0)
ALLOCATE(this%paragraph(this%ncols, nlines))
this%paragraph = ' '
! repeat filling the paragraph
nlines = 0
nw = 0
DO WHILE(nw < nwords)
  columns_in_line = 0
  words_in_line = 0
  DO WHILE(nw < nwords)
    nw = nw + 1
    ncols_next_word = this%word_end(nw) - this%word_start(nw) + 1
    IF (words_in_line > 0) ncols_next_word = ncols_next_word + 1 ! previous space
    IF (columns_in_line + ncols_next_word <= this%ncols .OR. &
     words_in_line == 0) THEN ! accept the word
      columns_in_line = columns_in_line + ncols_next_word
! now fill the paragraph
      IF (columns_in_line <= this%ncols) THEN ! non truncated line
        IF (words_in_line > 0) THEN ! previous space
          this%paragraph(columns_in_line-ncols_next_word+1:columns_in_line,nlines+1) = &
           TRANSFER(' '//line(this%word_start(nw):this%word_end(nw)), this%paragraph)
        ELSE ! no previous space
          this%paragraph(columns_in_line-ncols_next_word+1:columns_in_line,nlines+1) = &
           TRANSFER(line(this%word_start(nw):this%word_end(nw)), this%paragraph)
        ENDIF
      ELSE ! truncated line (word longer than line)
        this%paragraph(1:this%ncols,nlines+1) = &
         TRANSFER(line(this%word_start(nw):this%word_start(nw)+this%ncols-1), this%paragraph)
      ENDIF
      words_in_line = words_in_line + 1
    ELSE ! refuse the word
      nw = nw - 1
      EXIT
    ENDIF
  ENDDO
  nlines = nlines + 1
ENDDO

END FUNCTION line_split_new


!> Cleanly destroy a \a line_split object, deallocating all the
!! dynamically allocatd space. Use the generic name \a delete rather
!! than this specfoc subroutine.
SUBROUTINE line_split_delete(this)
TYPE(line_split), INTENT(inout) :: this !< object to be destroyed

IF (ASSOCIATED(this%paragraph)) DEALLOCATE(this%paragraph)
IF (ASSOCIATED(this%word_start)) DEALLOCATE(this%word_start)
IF (ASSOCIATED(this%word_end)) DEALLOCATE(this%word_end)

END SUBROUTINE line_split_delete


!> Return the number of lines over which the input line was splitted.
FUNCTION line_split_get_nlines(this) RESULT(nlines)
TYPE(line_split), INTENT(in) :: this !< object initialised with the line to be splitted

INTEGER :: nlines

IF (ASSOCIATED(this%paragraph)) THEN
  nlines = SIZE(this%paragraph, 2)
ELSE
  nlines = 0
ENDIF

END FUNCTION line_split_get_nlines


!> Return the \a nline -th line obtained after splitting. If \a nline
!! is out of range, a missing value is returned.
FUNCTION line_split_get_line(this, nline) RESULT(line)
TYPE(line_split), INTENT(in) :: this !< object initialised with the line to be splitted
INTEGER, INTENT(in) :: nline !< index of the line to be returned

CHARACTER(len=SIZE(this%paragraph, 1)) :: line
IF (nline > 0 .AND. nline <= SIZE(this%paragraph, 2)) THEN
  line = TRANSFER(this%paragraph(:,nline), line)
ELSE
  line = cmiss
ENDIF

END FUNCTION line_split_get_line


!> Return the suffix of a filename.
FUNCTION suffixname ( Input_String ) RESULT ( Output_String )
! -- Argument and result
CHARACTER( * ), INTENT( IN ) :: Input_String !< string to be interpreted as a filename
CHARACTER( LEN( Input_String ) ) :: Output_String
! -- Local variables
INTEGER :: i

Output_String=""
i = index (input_string,".",back=.True.)
if (i > 0 .and. i < len(Input_String)) Output_String= Input_String(i+1:)

END FUNCTION Suffixname


!> Return the number of columns in the terminal, if available it is
!! taken from the \a COLUMNS environment variable (it may be necessary
!! to execute \c export \c COLUMNS before running the program, in
!! order for this to work), otherwise it is set to 80.  A positive
!! value is returned in any case
FUNCTION default_columns() RESULT(cols)
INTEGER :: cols

INTEGER, PARAMETER :: defaultcols = 80 ! default of the defaults
INTEGER, PARAMETER :: maxcols = 256 ! maximum value
CHARACTER(len=8) :: ccols

cols = defaultcols
CALL getenv('COLUMNS', ccols)
IF (ccols == '') RETURN

READ(ccols, '(I10)', ERR=100) cols
cols = MIN(cols, maxcols)
IF (cols <= 0) cols = defaultcols
RETURN

100 cols = defaultcols ! error in reading the value

END FUNCTION default_columns


END MODULE char_utilities
