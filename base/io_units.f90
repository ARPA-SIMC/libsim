!> \brief Definizione di costanti utili per le unità di I/O
!!
!! Questo modulo definisce delle costanti che associano
!! le unità di Input/Output standard UNIX alle unità di
!! Input/Output del Fortran; si utilizzano tipicamente nei comandi
!! READ, WRITE, INQUIRE. 
!! Esempio tipico di utilizzo:
!! \code
!! USE io_units
!!
!! WRITE(stout_unit,*)'Dimmi qualcosa di carino'
!! READ(stdin_unit,*)mesg
!! IF (mesg == 'scemo') THEN
!!   WRITE(stderr_unit,*)'Mascalzone!'
!!   STOP
!! ENDIF
!! ...
!! \endcode
!! \ingroup base
MODULE io_units
IMPLICIT NONE

! Da condizionare con #ifdef ?!
INTEGER, PARAMETER :: stderr_unit = 0 !< standard error
INTEGER, PARAMETER :: stdin_unit = 5 !< standard input
INTEGER, PARAMETER :: stdout_unit = 6 !< standard output

END MODULE io_units
