MODULE char_utilities
IMPLICIT NONE

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
