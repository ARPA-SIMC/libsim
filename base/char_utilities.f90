!> \brief Utilità per le variabili CHARACTER.
!!
!! Questo modulo raccoglie utilità di uso generale legate
!! alla gestione delle variabili CHARACTER.
!! Esempio tipico di utilizzo:
!! \code
!! USE char_utilities
!! INTEGER :: j
!! ...
!! CALL raise_error('Valore inserito '//TRIM(to_char(j))//' troppo grande')
!! ...
!! \endcode
!! \ingroup base
MODULE char_utilities
USE kinds
IMPLICIT NONE

CHARACTER( * ), PRIVATE, PARAMETER :: LOWER_CASE = 'abcdefghijklmnopqrstuvwxyz'
CHARACTER( * ), PRIVATE, PARAMETER :: UPPER_CASE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

PRIVATE int_to_char, byte_to_char, char_to_char, &
   real_to_char, double_to_char, logical_to_char

!> Insieme di funzioni che restituiscono la variabile in input
!! convertita in stringa con un
!! formato di default sufficiente a rappresentarla ragionevolmente bene,
!! oppure con il formato opzionale richiesto \a form.  La variabile
!! CHARACTER restituita ha una lunghezza abbondante, per cui è
!! opportuno che venga ``tosata'' con la funzione intrinseca
!! TRIM() prima di essere stampata.  Attenzione che non viene
!! effettuato alcun controllo sull'eventuale formato fornito in
!! \a form, per cui potrebbero verificarsi errori in fase di
!! esecuzione se il formato non è sintatticamente corretto o non si
!! accorda con il tipo di \a x.
INTERFACE to_char
  MODULE PROCEDURE int_to_char, byte_to_char, char_to_char, &
   real_to_char, double_to_char, logical_to_char
END INTERFACE

CONTAINS

!> Variante con input intero
ELEMENTAL FUNCTION int_to_char(in, form) RESULT(char)
INTEGER,INTENT(in) :: in !< valore da rappresentare in CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< formato opzionale

CHARACTER(len=11) :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(char,'(I0)') in
ENDIF

END FUNCTION int_to_char


!> Variante con input byte
ELEMENTAL FUNCTION byte_to_char(in, form) RESULT(char)
INTEGER(kind=int_b),INTENT(in) :: in !< valore da rappresentare in CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< formato opzionale

CHARACTER(len=4) :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(char,'(I0)') in
ENDIF

END FUNCTION byte_to_char


!> Variante con input character
FUNCTION char_to_char(in, form) result(char)
CHARACTER(len=*),INTENT(in) :: in !< valore da rappresentare in CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< formato opzionale

CHARACTER(len=LEN_TRIM(in)) :: char
!fortran 2003
!CHARACTER(len=:),allocatable :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  char = TRIM(in)
ENDIF

END FUNCTION char_to_char


!> Variante con input reale
ELEMENTAL FUNCTION real_to_char(in, form) RESULT(char)
REAL,INTENT(in) :: in !< valore da rappresentare in CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< formato opzionale

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


!> Variante con input reale a doppia precisione
ELEMENTAL FUNCTION double_to_char(in, form) RESULT(char)
DOUBLE PRECISION,INTENT(in) :: in !< valore da rappresentare in CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< formato opzionale

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


!> Variante con input logico
ELEMENTAL FUNCTION logical_to_char(in, form) RESULT(char)
LOGICAL,INTENT(in) :: in !< valore da rappresentare in CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< formato opzionale

CHARACTER(len=1) :: char

!IF (PRESENT(form)) THEN
!  WRITE(char,form) in
!ELSE
WRITE(char,'(L1)') in
!ENDIF

END FUNCTION logical_to_char


!> Converte una variabile \a CHARACTER in una stringa passabile ad una
!! funzione scritta in linguaggio C come \a char* .
FUNCTION fchar_to_cstr(fchar, pcstr) RESULT(cstr)
CHARACTER(len=*), INTENT(in) :: fchar !< variabile da convertire
INTEGER(kind=int_b), POINTER, OPTIONAL :: pcstr(:) !< puntatore opzionale che può essere allocato per contenere il risultato, dovrà essere deallocato a carico del programma chiamante
INTEGER(kind=int_b) :: cstr(LEN(fchar)+1)

cstr(1:LEN(fchar)) = TRANSFER(fchar, cstr, LEN(fchar))
cstr(LEN(fchar)+1) = 0 ! Termino con zero
IF (PRESENT(pcstr)) THEN
  ALLOCATE(pcstr(LEN(fchar)+1))
  pcstr = cstr
ENDIF

END FUNCTION fchar_to_cstr


!> Converte una stringa passabile ad una funzione scritta in linguaggio C
!! come \a char* in una variabile \a CHARACTER
FUNCTION cstr_to_fchar(cstr) RESULT(fchar)
INTEGER(kind=int_b), INTENT(in) :: cstr(:) !< variabile da convertire
!CHARACTER(len=SIZE(cstr)-1) :: char
CHARACTER(len=SIZE(cstr)-1) :: fchar

INTEGER :: i

!l = MIN(LEN(char), SIZE(cstr)-1)
fchar = TRANSFER(cstr(1:SIZE(cstr)-1), fchar)
DO i = 1, SIZE(cstr)-1
  IF (fchar(i:i) == CHAR(0)) THEN ! se e` 'null-terminated' riempio con spazi
    fchar(i:) = ' '
    EXIT
  ENDIF
ENDDO

END FUNCTION cstr_to_fchar

!> convert lowercase to uppercase
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

!> convert uppercase to lowercase
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

aligned = input_string(fnblnk(input_string):)

END FUNCTION align_left


!> Returns \a input_string aligned to the right (i.e. without trailing blanks).
!! The needed number of leading blanks is added at the beginning in
!! order to keep the length of the resulting string equal to the input
!! length.
FUNCTION align_right(input_string) RESULT(aligned)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be aligned

CHARACTER(len=LEN(input_string)) :: aligned

aligned = ''
aligned(LEN(input_string)-lnblnk(input_string)+1:) = input_string

END FUNCTION align_right


!> Returns \a input_string centered, i.e. with an equal number of 
!! leading and trailing blanks (±1 if they are odd).  The needed
!! number of leading blanks is added or removed at the beginning in
!! order to keep the length of the resulting string equal to the input
!! length.
FUNCTION align_center(input_string) RESULT(aligned)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be aligned

CHARACTER(len=LEN(input_string)) :: aligned

INTEGER :: n1, n2

n1 = fnblnk(input_string)
n2 = LEN(input_string)-lnblnk(input_string)+1

aligned = ''
aligned((n1+n2)/2:) = input_string(n1:)

END FUNCTION align_center


!> Return the index of last character in \a input_string which is not
!! a blank space. If the strings is zero-length or contains only blank
!! spaces, returns zero.
FUNCTION lnblnk(input_string) RESULT(nblnk)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be scanned

INTEGER :: nblnk

DO nblnk = LEN(input_string), 1, -1
  IF (input_string(nblnk:nblnk) /= ' ') RETURN
ENDDO

END FUNCTION lnblnk


!> Return the index of first character in \a input_string which is not
!! a blank space. If the strings is zero-length or contains only blank
!! spaces, returns \a LEN(input_string)+1.
FUNCTION fnblnk(input_string) RESULT(nblnk)
CHARACTER(len=*), INTENT(in) :: input_string !< string to be scanned

INTEGER :: nblnk

DO nblnk = 1, LEN(input_string)
  IF (input_string(nblnk:nblnk) /= ' ') RETURN
ENDDO

END FUNCTION fnblnk

END MODULE char_utilities
