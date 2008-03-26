!> Gestione degli errori
!!
!! Questo modulo raccoglie procedure utili per la gestione
!! delle condizioni di errore e la stampa dei relativi messaggi
!! in un programma Fortran. Sono previste anche routine per la stampa di
!! messaggi informativi. I messaggi contengono già un'intestazione, per cui
!! non è necessario specificare in essi 'errore' o 'warning'.
!! L'utente della libreria può decidere, al momento dell'esecuzione,
!! quanto vuole essere "disturbato" dai messaggi, specificando il
!! livello di "disturbo" tollerato; il default all'inizio del programma è
!! di permettere di stampare i messaggi di errore, di warning e informativi
!! standard ma non stampare i messaggi informativi con livello di disturbo
!! superiore a \a eh_verbose_info. Questo fatto può essere sfruttato per
!! inserire nel programma dei messaggi informativi prolissi di debugging
!! che vengono visualizzati solo quando l'utente aumenta esplicitamente
!! il livello di disturbo tollerato.
!! 
!! \code
!! PROGRAM auto_mobile
!! USE err_handling
!! INTEGER :: ier
!! ...
!! IF (debug) THEN
!!   CALL errhandling_set(verbose=eh_verbose_info+1) ! stampa tutto
!! ELSE
!!   CALL errhandling_set(verbose=eh_verbose_info) ! default, inutile
!! ENDIF
!! CALL errhandling_set(fatal=.FALSE.) ! continua in caso di errore
!! ...
!! CALL metti_in_moto(...,ier)
!! IF (ier == 8) CALL metti_in_moto_a_spinta(...)
!! ...
!! IF (benz/benz_tot < 0.1) CALL raise_warning('Abbiamo poca benzina')
!! CALL print_info('Stiamo andando a '//TRIM(to_char(speed*3.6))//' all''ora')
!! CALL print_info('La sai l'ultima?...', eh_verbose_info+1)
!! ...
!! END PROGRAM auto_mobile
!! 
!! SUBROUTINE metti_in_moto(...,ier)
!! USE err_handling
!! INTEGER, INTENT(out) :: ier
!! ...
!! IF (volt < 12) THEN
!!   CALL raise_error('Batteria scarica',8,ier)
!!   RETURN
!! ENDIF
!! END SUBROUTINE metti_in_moto
!! \endcode
!! \ingroup base
MODULE err_handling
USE io_units
IMPLICIT NONE

INTEGER, PARAMETER :: eh_verbose_err=1 !< definisce il livello di disturbo prodotto dai messaggi di errore
INTEGER, PARAMETER :: eh_verbose_warn=2 !< definisce il livello di disturbo prodotto dai messaggi di avviso
INTEGER, PARAMETER :: eh_verbose_info=3 !< definisce il livello di disturbo prodottodai messaggi informativi
LOGICAL :: eh_fatal = .TRUE., eh_to_stderr = .TRUE.
INTEGER :: eh_unit = stderr_unit, eh_verbose = eh_verbose_info

PRIVATE
PUBLIC eh_verbose_err, eh_verbose_warn, eh_verbose_info, &
 raise_fatal_error, raise_error, raise_warning, print_info, eh_setval, eh_getval

CONTAINS

!> Stampa il messaggio di errore fornito (e il suo eventuale numero) ed esce.
!! Il messaggio veine stampato incondizionatamente (cioè indipendentemente 
!! dalle impostazioni del livello massimo di disturbo tollerabile)
!! e l'esecuzione del programma si interrompe incondizionatamente.
SUBROUTINE raise_fatal_error(msg, ierval)
CHARACTER (len=*), INTENT(in) :: msg !< messaggio di errore
INTEGER, OPTIONAL, INTENT(in) :: ierval !< codice di errore opzionale

CALL output_message('Fatal error: ', msg, -1, ierval)
IF (PRESENT(ierval)) CALL EXIT(ABS(ierval))
call exit(1)

END SUBROUTINE raise_fatal_error


!> Stampa il messaggio di errore fornito (e il suo eventuale numero).
!! Se specificato in configurazione (vedi eh_setval), il programma
!! si interrompe ed esce con codice di
!! errore di sistema \a ierval (se fornito).
SUBROUTINE raise_error(msg, ierval, ier)
CHARACTER (len=*), INTENT(in) :: msg !< messaggio di errore
INTEGER, OPTIONAL, INTENT(in) :: ierval !< codice di errore opzionale
INTEGER, OPTIONAL, INTENT(out) :: ier !< codice di errore opzionale restituito in uscita

CALL output_message('Error: ', msg, eh_verbose_err, ierval)
IF (eh_fatal) THEN
  IF (PRESENT(ierval)) CALL EXIT(ABS(ierval))
  call exit(1)
ENDIF
IF (PRESENT(ier) .AND. PRESENT(ierval)) ier = ierval

END SUBROUTINE raise_error


!> Stampa il messaggio di avviso fornito (e il suo eventuale numero).
SUBROUTINE raise_warning(msg, ierval, ier)
CHARACTER (len=*), INTENT(in) :: msg !< messaggio di avviso
INTEGER, OPTIONAL, INTENT(in) :: ierval !< codice di errore opzionale
INTEGER, OPTIONAL, INTENT(out) :: ier !< codice di errore opzionale restituito in uscita

CALL output_message('Warning: ', msg, eh_verbose_warn, ierval)
IF (PRESENT(ier) .AND. PRESENT(ierval)) ier = ierval

END SUBROUTINE raise_warning


!> Stampa il messaggio informativo fornito (e il suo eventuale numero).
SUBROUTINE print_info(msg, verblev)
CHARACTER (len=*), INTENT(in) :: msg !< messaggio di avviso
INTEGER, OPTIONAL, INTENT(in) :: verblev !< livello di "disturbo" associato al messagio, più è alto e maggiore è la possibilità che l'utente non lo legga

INTEGER :: lverblev

IF (PRESENT(verblev)) THEN
  lverblev = verblev
ELSE
  lverblev = eh_verbose_info
ENDIF

CALL output_message('Info: ', msg, lverblev)

END SUBROUTINE print_info


SUBROUTINE eh_setval(fatal, verbose, to_stderr, to_stdout, to_unit)
LOGICAL, OPTIONAL, INTENT(in) :: fatal !< specifica se gli errori devono interrompere il programma (\a .TRUE.) o meno (\a .FALSE.)
LOGICAL, OPTIONAL, INTENT(in) :: to_stderr
LOGICAL, OPTIONAL, INTENT(in) :: to_stdout
INTEGER, OPTIONAL, INTENT(in) :: verbose !< specifica il livello di "disturbo" tollerato dall'utente, i messaggi con un livello di disturbo superiore non saranno stampati
INTEGER, OPTIONAL, INTENT(in) :: to_unit !< specifica l'unità su cui stampare i messaggi di errore, il default è standard error, è consigliato usare i valori definiti dal modulo io_units

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
