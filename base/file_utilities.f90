MODULE file_utilities
USE err_handling
IMPLICIT NONE

!omstart file_utilities
!idx Modulo di utiit&agrave; per la gestione dei file
!Questo modulo definisce le seguenti routine/funzioni:
!
!FUNCTION getunit()
!INTEGER :: unit
!
!Restituisce un numero intero associato ad un'unit&agrave; di file libera,
!utilizzabilie in una OPEN() fortran; restituisce -1 in caso di errore
!
!Esempio di utilizzo:
!INTEGER :: n
!...
!n=getunit()
!OPEN(n, FILE='ostregheta.txt')
!
!omend


CONTAINS

FUNCTION getunit() RESULT(unit)
INTEGER :: unit

LOGICAL :: op

DO unit = 100, 32767
  INQUIRE(unit, opened=op)
  IF (.NOT. op) RETURN
ENDDO

CALL raise_error('Too many open files')
unit = -1

END FUNCTION getunit

END MODULE file_utilities
