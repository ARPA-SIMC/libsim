MODULE file_utilities
USE err_handling
IMPLICIT NONE

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
