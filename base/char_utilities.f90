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
!> \brief Utilities for CHARACTER variables.
!!
!! This module is a collection of all-purpose utilities connected to
!! the use of CHARACTER variables, and text handling in general.
!!
!! \ingroup base
MODULE char_utilities
USE kinds
USE missing_values
IMPLICIT NONE

CHARACTER( * ), PRIVATE, PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
CHARACTER( * ), PRIVATE, PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

PRIVATE int_to_char, byte_to_char, char_to_char, &
   real_to_char, double_to_char, logical_to_char
PRIVATE trim_int_to_char, trim_byte_to_char, trim_char_to_char, &
   trim_real_to_char, trim_double_to_char, trim_logical_to_char

!> Set of functions that return a CHARACTER representation of the
!! input variable. The functions use a default format suitable to
!! reasonably represent the input variable, or a user-defined format
!! provided with the optional variable \a form.  The return value may
!! be quite long, in order to take into account all possible cases, so
!! it is suggested to trim the result with the intrinsic function \a
!! TRIM() before using it. Be warned that no check is performed on the
!! optional format \a form, so a runtime error may occur if it is
!! syntactically wrong or not suitable to the data type provided.  The
!! functions are \a ELEMENTAL, so they can be applied to arrays of any
!! shape. The return value is of type \a CHARACTER with a predefined
!! length depending on the type of the input.
!!
!! \param in (any INTEGER or REAL basic type) value to be represented as CHARACTER
!! \param form CHARACTER(len=*),INTENT(in),OPTIONAL optional format
!!
!! Example of use:
!! \code
!! USE char_utilities
!! INTEGER :: j
!! ...
!! WRITE(*,*)'The value provided, '//TRIM(to_char(j))//', is too large'
!! ...
!! \endcode
INTERFACE to_char
  MODULE PROCEDURE int_to_char, byte_to_char, char_to_char, &
   real_to_char, double_to_char, logical_to_char
END INTERFACE

!> Set of functions that return a trimmed CHARACTER representation of the
!! input variable. The functions are analogous to \a to_char but they
!! return representation of the input in a CHARACTER with a variable
!! length, which needs not to be trimmed before use. The optional
!! format here is not accepted and these functions are not \a
!! ELEMENTAL so they work only on scalar arguments.
!!
!! \param in (any INTEGER or REAL basic type) value to be represented as CHARACTER
!!
!! Example of use:
!! \code
!! USE char_utilities
!! INTEGER :: j
!! ...
!! WRITE(*,*)'The value provided, '//t2c(j)//', is too large'
!! ...
!! \endcode
INTERFACE t2c
  MODULE PROCEDURE trim_int_to_char, trim_byte_to_char, trim_char_to_char, &
   trim_real_to_char, trim_double_to_char, trim_logical_to_char
END INTERFACE

!> Class that allows splitting a long line into shorter lines of equal
!! length at the occurrence of a specific character (typically a blank
!! space). All the members of the class are \a PRIVATE, thus all the
!! operations are performed through its methods.
TYPE line_split
  PRIVATE
  INTEGER :: align_type, ncols, nlines
  INTEGER, POINTER :: word_start(:), word_end(:)
  CHARACTER(len=1), POINTER :: paragraph(:,:)
END TYPE line_split

!> Destructor for the \a line_split class.
!! It cleanly destroys a \a line_split object, deallocating all the
!! dynamically allocated space.
!!
!! \param this (TYPE(line_split)) object to be destroyed
INTERFACE delete
  MODULE PROCEDURE line_split_delete
END INTERFACE

PRIVATE line_split_delete


!> Tries to match the given string with the pattern
!! Result:
!!     .true. if the entire string matches the pattern, .false.
!!     otherwise
!! Note:
!!     Trailing blanks are ignored
!!
!! provides a string matching method known as glob matching: it is used
!! for instance under UNIX, Linux and DOS to select files whose names
!! match a certain pattern - strings like "*.f90" describe all file
!! swhose names end in ".f90".
!!
!! The method implemented in the module is somewhat simplified than the
!! full glob matching possible under UNIX: it does not support
!! character classes.
!!
!! Glob patterns are intended to match the entire string. In this
!! implementation, however, trailing blanks in both the string and the
!! pattern are ignored, so that it is a bit easier to use in Fortran.
!!
!! The module supports both "*" and "?" as wild cards, where "*" means
!! any sequence of characters, including zero and "?" means a single
!! character. If you need to match the characters "*" or "?", then
!! precede them with a backslash ("\"). If you need to match a
!! backslash, you will need to use two:
!!
!! 	
!!    match = string_match( "c:\somedir" "c:\\*" )
!!
!! will return .true., while:
!!
!!    match = string_match( "c:\somedir" "c:\*" )
!!
!! will not match, as the backslash "escapes" the asterisk, which then becomes an ordinary character. 
!!
!! BUGS
!!
!! The matching algorithm is not flawless:
!!
!!    * Patterns like "e* *" may fail, because trailing blanks are
!!      removed. The string "e " ought to match this pattern, but
!!      because only the substring "e" will be considered, the
!!      trailing blank that is necessary for matching between the two
!!      asterisks is removed from the matching process.
!!
!!      The test program contains a case that should fail on this, but it does not, oddly enough.
!!
!!    * Patterns like "b*ba" fail on a string like "babababa" because
!!      the algorithm finds an early match (the substring at 3:4) for
!!      the last literal substring "ba" in the pattern. It should
!!      instead skip over that substring and search for the substring
!!      7:8.
!!
!!      There are two ways to deal with this:
!!
!!          o Insert an extra character at the end, which does not occur anywhere in the pattern.
!!
!!          o If the match fails, continue at a point after the position of the literal substring where matching failed. 
!!
!!      The second is probably the way to go, but it may be a bit slower. 
INTERFACE match
  MODULE PROCEDURE string_match, string_match_v
END INTERFACE

PRIVATE string_match, string_match_v

CONTAINS

! Version with integer argument, please use the generic \a to_char
! rather than this function directly.
ELEMENTAL FUNCTION int_to_char(in, form) RESULT(char)
INTEGER,INTENT(in) :: in ! value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form ! optional format

CHARACTER(len=11) :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(char,'(I0)') in
ENDIF

END FUNCTION int_to_char


FUNCTION trim_int_to_char(in) RESULT(char)
INTEGER,INTENT(in) :: in ! value to be represented as CHARACTER

CHARACTER(len=len_trim(int_to_char(in))) :: char

char=int_to_char(in)

END FUNCTION trim_int_to_char


! Version with 1-byte integer argument, please use the generic \a to_char
! rather than this function directly.
ELEMENTAL FUNCTION byte_to_char(in, form) RESULT(char)
INTEGER(kind=int_b),INTENT(in) :: in ! value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form ! optional format

CHARACTER(len=4) :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(char,'(I0)') in
ENDIF

END FUNCTION byte_to_char


FUNCTION trim_byte_to_char(in) RESULT(char)
INTEGER(kind=int_b),INTENT(in) :: in ! value to be represented as CHARACTER

CHARACTER(len=len_trim(byte_to_char(in))) :: char

char=byte_to_char(in)

END FUNCTION trim_byte_to_char


! Version with character argument, please use the generic \a to_char
! rather than this function directly. It is almost useless, just
! provided for completeness.
FUNCTION char_to_char(in, form) result(char)
CHARACTER(len=*),INTENT(in) :: in ! value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form ! optional format

CHARACTER(len=LEN_TRIM(in)) :: char
!fortran 2003
!CHARACTER(len=:),allocatable :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  char = TRIM(in)
ENDIF

END FUNCTION char_to_char


FUNCTION trim_char_to_char(in) result(char)
CHARACTER(len=*),INTENT(in) :: in ! value to be represented as CHARACTER

CHARACTER(len=LEN_TRIM(in)) :: char
!fortran 2003
!CHARACTER(len=:),allocatable :: char

char = char_to_char(in)

END FUNCTION trim_char_to_char


! Version with single precision real argument, please use the generic
! \a to_char rather than this function directly.
ELEMENTAL FUNCTION real_to_char(in, form) RESULT(char)
REAL,INTENT(in) :: in ! value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form ! optional format

CHARACTER(len=15) :: char, tmpchar

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(tmpchar,'(G15.9)') in
  char = align_left(tmpchar)
ENDIF

END FUNCTION real_to_char


FUNCTION trim_real_to_char(in) RESULT(char)
REAL,INTENT(in) :: in ! value to be represented as CHARACTER

CHARACTER(len=len_trim(real_to_char(in))) :: char

char=real_to_char(in)

END FUNCTION trim_real_to_char


! Version with double precision real argument, please use the generic
! \a to_char rather than this function directly.
ELEMENTAL FUNCTION double_to_char(in, form) RESULT(char)
DOUBLE PRECISION,INTENT(in) :: in ! value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form ! optional format

CHARACTER(len=24) :: char, tmpchar

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(tmpchar,'(G24.17)') in
  char = align_left(tmpchar)
ENDIF

END FUNCTION double_to_char

FUNCTION trim_double_to_char(in) RESULT(char)
DOUBLE PRECISION,INTENT(in) :: in ! value to be represented as CHARACTER
CHARACTER(len=len_trim(double_to_char(in))) :: char

char=double_to_char(in)

END FUNCTION trim_double_to_char


! Version with logical argument, please use the generic \a to_char
! rather than this function directly.
ELEMENTAL FUNCTION logical_to_char(in, form) RESULT(char)
LOGICAL,INTENT(in) :: in ! value to be represented as CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form ! optional format

CHARACTER(len=1) :: char

!IF (PRESENT(form)) THEN
!  WRITE(char,form) in
!ELSE
WRITE(char,'(L1)') in
!ENDIF

END FUNCTION logical_to_char


ELEMENTAL FUNCTION trim_logical_to_char(in) RESULT(char)
LOGICAL,INTENT(in) :: in ! value to be represented as CHARACTER

CHARACTER(len=1) :: char

!IF (PRESENT(form)) THEN
!  WRITE(char,form) in
!ELSE
WRITE(char,'(L1)') in
!ENDIF

END FUNCTION trim_logical_to_char


!> Converts a \a CHARACTER variable into a string which can be
!! directly passed to a C function requiring a null-terminated \a
!! const \a char* (input) argument. If the result is going to be
!! stored into an array, this has to be dimensioned with a suitable
!! size (\a LEN(fchar) \a + \a 1 ).
FUNCTION fchar_to_cstr(fchar) RESULT(cstr)
CHARACTER(len=*), INTENT(in) :: fchar !< variable to be converted
INTEGER(kind=int_b) :: cstr(LEN(fchar)+1)

cstr(1:LEN(fchar)) = TRANSFER(fchar, cstr, LEN(fchar))
cstr(LEN(fchar)+1) = 0 ! zero-terminate

END FUNCTION fchar_to_cstr


!> Converts a \a CHARACTER variable into a string which can be
!! directly passed to a C function requiring a null-terminated \a
!! char* (input/output) argument. The result is stored into \a pcstr
!! which is allocated within the subroutine and has to be deallocated
!! by the calling procedure.
SUBROUTINE fchar_to_cstr_alloc(fchar, pcstr)
CHARACTER(len=*), INTENT(in) :: fchar !< variable to be converted
INTEGER(kind=int_b), POINTER :: pcstr(:) !< pointer to a 1-d byte array which will be allocated and, on output, will contain the null-terminated string

ALLOCATE(pcstr(LEN(fchar)+1))
pcstr(1:LEN(fchar)) = TRANSFER(fchar, pcstr, LEN(fchar))
pcstr(LEN(fchar)+1) = 0 ! zero-terminate

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


!> Convert a \a CHARACTER variable to uppercase.
FUNCTION UpperCase ( Input_String ) RESULT ( Output_String )
CHARACTER( * ), INTENT( IN ) :: Input_String !< variable to be converted
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


!> Convert a \a CHARACTER variable to lowercase.
FUNCTION LowerCase ( Input_String ) RESULT ( Output_String )
                                ! -- Argument and result
CHARACTER( * ), INTENT( IN ) :: Input_String !< variable to be converted
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


!> \brief Returns \a input_string aligned to the left,
!! i.e.\ without leading blanks.  The needed number of trailing
!! blanks is added at the end in order to keep the length of the
!! resulting string equal to the input length.
ELEMENTAL FUNCTION align_left(input_string) RESULT(aligned)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be aligned

CHARACTER(len=LEN(input_string)) :: aligned

aligned = input_string(f_nblnk(input_string):)

END FUNCTION align_left


!> \brief Returns \a input_string aligned to the right,
!! i.e.\ without trailing blanks. The needed number of leading
!! blanks is added at the beginning in order to keep the length of the
!! resulting string equal to the input length.
ELEMENTAL FUNCTION align_right(input_string) RESULT(aligned)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be aligned

CHARACTER(len=LEN(input_string)) :: aligned

aligned = ''
aligned(LEN(input_string)-l_nblnk(input_string)+1:) = input_string

END FUNCTION align_right


!> Returns \a input_string centered, i.e.\ with an equal number of 
!! leading and trailing blanks (±1 if they are odd). The needed
!! number of leading/trailing blanks is added or removed at the
!! beginning and/or at the end in order to keep the length of the
!! resulting string equal to the input length.
ELEMENTAL FUNCTION align_center(input_string) RESULT(aligned)
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
ELEMENTAL FUNCTION l_nblnk(input_string, blnk) RESULT(nblnk)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be scanned
CHARACTER(len=1), INTENT(in), OPTIONAL :: blnk !< optional blank character, if not provided, a blank space is assumed

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
ELEMENTAL FUNCTION f_nblnk(input_string, blnk) RESULT(nblnk)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be scanned
CHARACTER(len=1), INTENT(in), OPTIONAL :: blnk !< optional blank character, if not provided, a blank space is assumed

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


! Cleanly destroy a \a line_split object, deallocating all the
! dynamically allocated space. Use the generic name \a delete rather
! than this specfoc subroutine.
SUBROUTINE line_split_delete(this)
TYPE(line_split), INTENT(inout) :: this ! object to be destroyed

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
!! is out of range, a missing value is returned. The line is always
!! left-aligned and it is padded with trailing blanks up to the
!! requested line length.
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
CHARACTER(len=10) :: ccols

cols = defaultcols
CALL getenv('COLUMNS', ccols)
IF (ccols == '') RETURN

READ(ccols, '(I10)', ERR=100) cols
cols = MIN(cols, maxcols)
IF (cols <= 0) cols = defaultcols
RETURN

100 cols = defaultcols ! error in reading the value

END FUNCTION default_columns


!> Remove the requested characters from a string.
!! This function returns a string cleaned from unwanted characters,
!! either by removing "bad" characters (argument \a badchar) or by
!! keeping only "good" characters (argument \a goodchar).  If neither
!! \a badchar nor \a goodchar are provided, it keeps only alphabetic
!! ASCII characters.
ELEMENTAL FUNCTION wash_char(in, goodchar,badchar) RESULT(char)
CHARACTER(len=*),INTENT(in) :: in !< string to be cleaned
CHARACTER(len=*),INTENT(in),OPTIONAL :: badchar !< optional set of "bad" characters
CHARACTER(len=*),INTENT(in),OPTIONAL :: goodchar !< optional set of "good" characters
integer,allocatable :: igoodchar(:)
integer,allocatable :: ibadchar(:)

CHARACTER(len=len(in)) :: char,charr,charrr
integer :: i,ia

char=""
charr=""
charrr=""

if (present(goodchar)) then

allocate(igoodchar(len(goodchar)))

  do i =1, len(goodchar)
    igoodchar=iachar(goodchar(i:))
  end do

  do i=1,len(in)
    ia = iachar(in(i:))
    if (any(ia == igoodchar))then
      charrr=trim(charrr)//achar(ia)
    end if
  end do

deallocate(igoodchar)

else

  charrr=in

end if



if (present(badchar)) then

allocate(ibadchar(len(badchar)))

  do i =1, len(badchar)
    ibadchar=iachar(badchar(i:))
  end do

  do i=1,len(charrr)
    ia = iachar(charrr(i:))
    if (.not. any(ia == ibadchar))then
      charr=trim(charr)//achar(ia)
    end if
  end do

deallocate(ibadchar)

else

  charr=charrr

end if


if (.not. present(goodchar) .and. .not. present(badchar)) then

  do i=1,len(charr)
    ia = iachar(charr(i:))
    if ((ia >= 65 .and. ia <= 90) .or. &
        (ia >= 97 .and. ia <= 122))then
      char=trim(char)//achar(ia)
    end if
  end do

end if


END FUNCTION wash_char


! derived by http://sourceforge.net/projects/flibs
!
! globmatch.f90 --
!     Match strings according to (simplified) glob patterns
!
!     The pattern matching is limited to literals, * and ?
!     (character classes are not supported). A backslash escapes
!     any character.
!
!     $Id: globmatch.f90,v 1.5 2006/03/26 19:03:53 arjenmarkus Exp $
!!$Copyright (c) 2008, Arjen Markus
!!$
!!$All rights reserved.
!!$
!!$Redistribution and use in source and binary forms, with or without modification,
!!$are permitted provided that the following conditions are met:
!!$
!!$Redistributions of source code must retain the above copyright notice,
!!$this list of conditions and the following disclaimer.
!!$Redistributions in binary form must reproduce the above copyright notice,
!!$this list of conditions and the following disclaimer in the documentation
!!$and/or other materials provided with the distribution.
!!$Neither the name of the author nor the names of the contributors
!!$may be used to endorse or promote products derived from this software
!!$without specific prior written permission.
!!$THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!!$"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
!!$THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
!!$ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
!!$FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
!!$DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
!!$SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
!!$CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
!!$OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!!$OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!

!> Tries to match the given string with the pattern (vector version)
!! Result:
!!     .true. if the entire string matches the pattern, .false.
!!     otherwise
!! Note:
!!     Trailing blanks are ignored
!!
logical function string_match_v( string, pattern ) result(match)
character(len=*), intent(in) :: string(:) !< String to be examined
character(len=*), intent(in) :: pattern !< Glob pattern to be used for the matching
logical                      :: match(size(string))

integer :: i

do i =1,size(string)
  match(i)=string_match(string(i),pattern)
end do

end function string_match_v

!> Tries to match the given string with the pattern
!! Result:
!!     .true. if the entire string matches the pattern, .false.
!!     otherwise
!! Note:
!!     Trailing blanks are ignored
!!
recursive function string_match( string, pattern ) result(match)
    character(len=*), intent(in) :: string !< String to be examined
    character(len=*), intent(in) :: pattern !< Glob pattern to be used for the matching
    logical                      :: match

    character(len=1), parameter :: backslash = '\\'
    character(len=1), parameter :: star      = '*'
    character(len=1), parameter :: question  = '?'

    character(len=len(pattern))  :: literal
    integer                      :: ptrim
    integer                      :: p
    integer                      :: k
    integer                      :: ll
    integer                      :: method
    integer                      :: start
    integer                      :: strim

    match  = .false.
    method = 0
    ptrim  = len_trim( pattern )
    strim  = len_trim( string )
    p      = 1
    ll     = 0
    start  = 1

    !
    ! Split off a piece of the pattern
    !
    do while ( p <= ptrim )
        select case ( pattern(p:p) )
            case( star )
                if ( ll .ne. 0 ) exit
                method = 1
            case( question )
                if ( ll .ne. 0 ) exit
                method = 2
                start  = start + 1
            case( backslash )
                p  = p + 1
                ll = ll + 1
                literal(ll:ll) = pattern(p:p)
            case default
                ll = ll + 1
                literal(ll:ll) = pattern(p:p)
        end select

        p = p + 1
    enddo

    !
    ! Now look for the literal string (if any!)
    !
    if ( method == 0 ) then
        !
        ! We are at the end of the pattern, and of the string?
        !
        if ( strim == 0 .and. ptrim == 0 ) then
            match = .true.
        else
            !
            ! The string matches a literal part?
            !
            if ( ll > 0 ) then
                if ( string(start:min(strim,start+ll-1)) == literal(1:ll) ) then
                    start = start + ll
                    match = string_match( string(start:), pattern(p:) )
                endif
            endif
        endif
    endif

    if ( method == 1 ) then
        !
        ! Scan the whole of the remaining string ...
        !
        if ( ll == 0 ) then
            match = .true.
        else
            do while ( start <= strim )
                k     = index( string(start:), literal(1:ll) )
                if ( k > 0 ) then
                    start = start + k + ll - 1
                    match = string_match( string(start:), pattern(p:) )
                    if ( match ) then
                        exit
                    endif
                endif

                start = start + 1
            enddo
        endif
    endif

    if ( method == 2 .and. ll > 0 ) then
        !
        ! Scan the whole of the remaining string ...
        !
        if ( string(start:min(strim,start+ll-1)) == literal(1:ll) ) then
            match = string_match( string(start+ll:), pattern(p:) )
        endif
    endif
    return
end function string_match

END MODULE char_utilities
