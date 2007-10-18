!> \brief Utilità per i file.
!!
!! Questo modulo raccoglie utilità di uso generale legate alla gestione dei file.
!! \ingroup base
MODULE file_utilities
USE err_handling
IMPLICIT NONE

CONTAINS

!> Restituisce il numero di un'unità I/O Fortran attualmente non
!! utilizzata. Restituisce -1 in caso di errore. Da inserire in un'istruzione
!! fortran \c OPEN. Esempio di utilizzo:
!! \code
!! USE file_utilities
!! ...
!! INTEGER :: n
!! ...
!! n=getunit()
!! OPEN(n, FILE='ostregheta.txt')
!! \endcode
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
