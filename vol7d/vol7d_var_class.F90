MODULE vol7d_var_class
USE kinds
USE missing_values
IMPLICIT NONE

TYPE vol7d_var
  CHARACTER(len=10) :: btable
  CHARACTER(len=20) :: description, unit
  INTEGER :: r, d, i, b, c
END TYPE  vol7d_var

TYPE(vol7d_var),PARAMETER :: vol7d_var_miss= &
 vol7d_var(cmiss,cmiss,cmiss,imiss,imiss,imiss,imiss,imiss)

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

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_var
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_var
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_var
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_var
END INTERFACE

CONTAINS

SUBROUTINE vol7d_var_init(this, btable, description, unit)
TYPE(vol7d_var),INTENT(INOUT) :: this
!INTEGER,INTENT(in),OPTIONAL :: btable
CHARACTER(len=*),INTENT(in),OPTIONAL :: btable
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

this%r = -1
this%d = -1
this%i = -1
this%b = -1
this%c = -1

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


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(vol7d_var)
#define VOL7D_POLY_TYPES _var
#include "vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_var_class
