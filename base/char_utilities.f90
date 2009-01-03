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

CHARACTER(len=15) :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(char,'(G15.9)') in
ENDIF

END FUNCTION real_to_char


!> Variante con input reale a doppia precisione
ELEMENTAL FUNCTION double_to_char(in, form) RESULT(char)
DOUBLE PRECISION,INTENT(in) :: in !< valore da rappresentare in CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< formato opzionale

CHARACTER(len=24) :: char

IF (PRESENT(form)) THEN
  WRITE(char,form) in
ELSE
  WRITE(char,'(G24.17)') in
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


END MODULE char_utilities
