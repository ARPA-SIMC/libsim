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
IMPLICIT NONE

PRIVATE int_to_char

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
  MODULE PROCEDURE int_to_char
END INTERFACE

CONTAINS

!> Variante con input intero
elemental FUNCTION int_to_char(i, form)
INTEGER,INTENT(in) :: i !< valore da rappresentare in CHARACTER
CHARACTER(len=*),INTENT(in),OPTIONAL :: form !< formato opzionale

CHARACTER(len=10) :: int_to_char

IF (PRESENT(form)) THEN
  WRITE(int_to_char,form) i
ELSE
  WRITE(int_to_char,'(I0)') i
ENDIF

END FUNCTION int_to_char

END MODULE char_utilities
