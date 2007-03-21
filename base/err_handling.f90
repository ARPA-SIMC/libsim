MODULE err_handling
USE io_units
IMPLICIT NONE

!omstart err_handling
!idx Modulo per la gestione degli errori in fortran
!Questo modulo definisce le seguenti routine per la gestione di errori in fortran:
!
!SUBROUTINE raise_error(msg, ierval, ier)
!CHARACTER (len=*), INTENT(in) :: msg
!INTEGER, OPTIONAL, INTENT(in) :: ierval
!INTEGER, OPTIONAL, INTENT(out) :: ier
!
!Stampa il messaggio di errore msg (e il suo eventuale numero ierval),
!se ier &egrave; presente gli viene assegnato il valore ierval.
!Se richiesto l'esecuzione del programma si interrompe.
!
!SUBROUTINE raise_warning(msg, ierval, ier)
!CHARACTER (len=*), INTENT(in) :: msg
!INTEGER, OPTIONAL, INTENT(in) :: ierval
!INTEGER, OPTIONAL, INTENT(out) :: ier
!
!Stampa il messaggio di avviso msg (e il suo eventuale numero ierval),
!se ier &egrave; presente gli viene assegnato il valore ierval.
!L'esecuzione del programma non si interrompe.
!
!SUBROUTINE errhandling_set(fatal, to_stderr, to_stdout, to_unit)
!LOGICAL, OPTIONAL, INTENT(in) :: fatal, to_stderr, to_stdout
!INTEGER, OPTIONAL, INTENT(in) :: to_unit
!
!Configura il comportamento delle routine raise_error e raise_warning:
!fatal=.TRUE./.FALSE. => il programma si arresta/prosegue dopo raise_error
!to_stderr=.TRUE. => raise_error e raise_warning scrivono su stderr
!to_stdout=.TRUE. => raise_error e raise_warning scrivono su stdout
!to_unit => raise_error e raise_warning scrivono sull'unit&agrave; to_unit
!
!Esempio di utilizzo:
!INTEGER :: ier
!CALL errhandling_set(fatal=.TRUE., to_stderr=.TRUE.)
!CALL con_errori_gestiti(...,ier)
!...
!SUBROUTINE con_errori_gestiti(...,ier)
!USE err_handling
!INTEGER, INTENT(out) :: ier
!...
!IF (condizione_di_errore) THEN
!  CALL raise_error('File non trovato',8,ier)
!  RETURN
!ENDIF
!
!omend

LOGICAL :: eh_fatal = .TRUE., eh_to_stderr = .TRUE.
INTEGER :: eh_unit = stderr_unit

PRIVATE
PUBLIC raise_error, raise_warning, errhandling_set

CONTAINS

SUBROUTINE raise_error(msg, ierval, ier)
CHARACTER (len=*), INTENT(in) :: msg
INTEGER, OPTIONAL, INTENT(in) :: ierval
INTEGER, OPTIONAL, INTENT(out) :: ier

CALL output_message('Error', msg, ierval)
IF (eh_fatal) THEN
  IF (PRESENT(ierval)) CALL EXIT(ABS(ierval))
  STOP 1
ENDIF
IF (PRESENT(ier) .AND. PRESENT(ierval)) ier = ierval

END SUBROUTINE raise_error


SUBROUTINE raise_warning(msg, ierval, ier)
CHARACTER (len=*), INTENT(in) :: msg
INTEGER, OPTIONAL, INTENT(in) :: ierval
INTEGER, OPTIONAL, INTENT(out) :: ier

CALL output_message('Warning', msg, ierval)
IF (PRESENT(ier) .AND. PRESENT(ierval)) ier = ierval

END SUBROUTINE raise_warning


SUBROUTINE errhandling_set(fatal, to_stderr, to_stdout, to_unit)
LOGICAL, OPTIONAL, INTENT(in) :: fatal, to_stderr, to_stdout
INTEGER, OPTIONAL, INTENT(in) :: to_unit

IF (PRESENT(fatal)) eh_fatal = fatal
IF (PRESENT(to_stderr)) THEN
  IF (to_stderr) THEN
    eh_unit = stderr_unit
  ELSE
    eh_unit = stdout_unit
  ENDIF
ENDIF
IF (PRESENT(to_stdout)) THEN
  IF (to_stdout) THEN
    eh_unit = stdout_unit
  ELSE
    eh_unit = stderr_unit
  ENDIF
ENDIF
IF (PRESENT(to_unit)) eh_unit = to_unit

END SUBROUTINE errhandling_set


SUBROUTINE output_message(head, msg, ierval)
CHARACTER (len=*), INTENT(in) :: head, msg
INTEGER, OPTIONAL, INTENT(in) :: ierval

WRITE(eh_unit, '(3A)') head, ': ', TRIM(msg)
IF (PRESENT(ierval)) WRITE(eh_unit, '(2A,I6)') head,' code: ',ierval

END SUBROUTINE output_message


END MODULE err_handling
