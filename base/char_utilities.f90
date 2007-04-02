MODULE char_utilities
IMPLICIT NONE

!omstart char_utilities
!idx Modulo di utiit&agrave; per la gestione di variabili carattere
!vedi err_handling
!Questo modulo definisce le seguenti routine/funzioni:
!
!FUNCTION int_to_char(i, form)
!INTEGER,INTENT(in) :: i
!CHARACTER(len=*),INTENT(in),OPTIONAL :: form
!
!Restituisce la variabile intera i convertita in stringa
!con un formato di default ('I0' la pi&ugrave; breve rappresentazione
!in base 10) oppure con il formato opzionale richiesto form.
!
!Esempio di utilizzo:
!INTEGER :: j
!...
!CALL raise_error('Valore inserito '//TRIM(to_char(j))//' troppo grande')
!
!omend

PRIVATE int_to_char

INTERFACE to_char
  MODULE PROCEDURE int_to_char
END INTERFACE

CONTAINS

FUNCTION int_to_char(i, form)
INTEGER,INTENT(in) :: i
CHARACTER(len=*),INTENT(in),OPTIONAL :: form

CHARACTER(len=10) :: int_to_char

IF (PRESENT(form)) THEN
  WRITE(int_to_char,form) i
ELSE
  WRITE(int_to_char,'(I0)') i
ENDIF

END FUNCTION int_to_char

END MODULE char_utilities
