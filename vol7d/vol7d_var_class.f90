MODULE vol7d_var_class
USE kinds
USE missing_values
IMPLICIT NONE

TYPE vol7d_var
  CHARACTER(len=10) :: btable
  CHARACTER(len=20) :: description, unit
  INTEGER :: r, d, i, b, c
END TYPE  vol7d_var

INTERFACE init
  MODULE PROCEDURE vol7d_var_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_var_delete
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_var_eq, vol7d_var_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_var_ne, vol7d_var_nesv
END INTERFACE

CONTAINS

SUBROUTINE vol7d_var_init(this, btable, description, unit)
TYPE(vol7d_var),INTENT(INOUT) :: this
!INTEGER,INTENT(in),OPTIONAL :: btable
CHARACTER(len=10),INTENT(in),OPTIONAL :: btable
CHARACTER(len=20),INTENT(in),OPTIONAL :: description, unit

IF (PRESENT(btable)) THEN
  this%btable = btable
ELSE
  this%btable = cmiss
  this%description = cmiss
  this%unit = cmiss
  RETURN
ENDIF
IF (PRESENT(description)) THEN
  this%description = description
ELSE
  this%description = cmiss
ENDIF
IF (PRESENT(unit)) THEN
  this%unit = unit
ELSE
  this%unit = cmiss
ENDIF

END SUBROUTINE vol7d_var_init


SUBROUTINE vol7d_var_delete(this)
TYPE(vol7d_var),INTENT(INOUT) :: this

this%btable = cmiss
this%description = cmiss
this%unit = cmiss

END SUBROUTINE vol7d_var_delete


elemental FUNCTION vol7d_var_eq(this, that) RESULT(res)
TYPE(vol7d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = this%btable == that%btable

END FUNCTION vol7d_var_eq


FUNCTION vol7d_var_eqsv(this, that) RESULT(res)
TYPE(vol7d_var),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION vol7d_var_eqsv


elemental FUNCTION vol7d_var_ne(this, that) RESULT(res)
TYPE(vol7d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_var_ne


FUNCTION vol7d_var_nesv(this, that) RESULT(res)
TYPE(vol7d_var),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_var_nesv


END MODULE vol7d_var_class
