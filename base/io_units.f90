MODULE io_units
IMPLICIT NONE

!omstart io_units
!idx Costanti fortran per le unit&agrave; di I/O
!Questo modulo definisce le seguenti costanti che associano
!le unit&agrave; di I/O standard unix alle unit&agrave; I/O del fortrtan:
!
!stdin_unit  => standard input
!stdout_unit => standard output
!stderr_unit => standard error
!
!Esempio di utilizzo:
!USE io_units
!...
!WRITE(0,*)'Errore fatale'
!STOP
!
!Esso fornisce inoltre le seguenti routine/funzioni:
!
!FUNCTION getunit() RESULT(unit)
!INTEGER :: unit
!
!restituisce il numero di un'unit&agrave; I/O fortran
!attualmente non utilizzata
!
!Esempio di utilizzo:
!USE io_units
!INTEGER :: unit
!unit=getunit
!OPEN(unit, file='input.txt')
!
!omend

! Da condizionare con #ifdef ?!
INTEGER, PARAMETER :: &
 stderr_unit = 0, &
 stdin_unit = 5, &
 stdout_unit = 6

END MODULE io_units
