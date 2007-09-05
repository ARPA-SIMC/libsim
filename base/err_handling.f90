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

INTEGER, PARAMETER :: eh_verbose_err=1, eh_verbose_warn=2, eh_verbose_info=3
LOGICAL :: eh_fatal = .TRUE., eh_to_stderr = .TRUE.
INTEGER :: eh_unit = stderr_unit, eh_verbose = eh_verbose_info

PRIVATE
PUBLIC eh_verbose_err, eh_verbose_warn, eh_verbose_info, &
 raise_fatal_error, raise_error, raise_warning, print_info, eh_setval, eh_getval

CONTAINS

SUBROUTINE raise_fatal_error(msg, ierval)
CHARACTER (len=*), INTENT(in) :: msg
INTEGER, OPTIONAL, INTENT(in) :: ierval

CALL output_message('Fatal error: ', msg, -1, ierval)
IF (PRESENT(ierval)) CALL EXIT(ABS(ierval))
STOP 1

END SUBROUTINE raise_fatal_error


SUBROUTINE raise_error(msg, ierval, ier)
CHARACTER (len=*), INTENT(in) :: msg
INTEGER, OPTIONAL, INTENT(in) :: ierval
INTEGER, OPTIONAL, INTENT(out) :: ier

CALL output_message('Error: ', msg, eh_verbose_err, ierval)
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

CALL output_message('Warning: ', msg, eh_verbose_warn, ierval)
IF (PRESENT(ier) .AND. PRESENT(ierval)) ier = ierval

END SUBROUTINE raise_warning


SUBROUTINE print_info(msg, verblev)
CHARACTER (len=*), INTENT(in) :: msg
INTEGER, OPTIONAL, INTENT(in) :: verblev

INTEGER :: lverblev

IF (PRESENT(verblev)) THEN
  lverblev = verblev
ELSE
  lverblev = eh_verbose_info
ENDIF

CALL output_message('Info: ', msg, lverblev)

END SUBROUTINE print_info


SUBROUTINE eh_setval(fatal, verbose, to_stderr, to_stdout, to_unit)
LOGICAL, OPTIONAL, INTENT(in) :: fatal, to_stderr, to_stdout
INTEGER, OPTIONAL, INTENT(in) :: verbose, to_unit

IF (PRESENT(fatal)) eh_fatal = fatal
IF (PRESENT(verbose)) eh_verbose = MAX(verbose,0)
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

END SUBROUTINE eh_setval


SUBROUTINE eh_getval(fatal, verbose, to_unit)
LOGICAL, OPTIONAL, INTENT(out) :: fatal
INTEGER, OPTIONAL, INTENT(out) :: verbose, to_unit

IF (PRESENT(fatal)) fatal = eh_fatal
IF (PRESENT(verbose)) verbose = eh_verbose
IF (PRESENT(to_unit)) to_unit = eh_unit

END SUBROUTINE eh_getval


SUBROUTINE output_message(head, msg, verblev, ierval)
CHARACTER (len=*), INTENT(in) :: head, msg
INTEGER, INTENT(in) :: verblev
INTEGER, OPTIONAL, INTENT(in) :: ierval

IF (eh_verbose >= verblev) THEN
  WRITE(eh_unit, '(2A)') head, TRIM(msg)
  IF (PRESENT(ierval)) WRITE(eh_unit, '(2A,I6)') head,' code: ',ierval
ENDIF

END SUBROUTINE output_message


END MODULE err_handling
