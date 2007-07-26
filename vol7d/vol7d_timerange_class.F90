MODULE vol7d_timerange_class
USE kinds
USE missing_values
IMPLICIT NONE

TYPE vol7d_timerange
  INTEGER :: timerange,p1,p2
END TYPE vol7d_timerange

TYPE(vol7d_timerange),PARAMETER :: vol7d_timerange_miss= &
 vol7d_timerange(imiss,imiss,imiss)

INTERFACE init
  MODULE PROCEDURE vol7d_timerange_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_timerange_delete
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_timerange_eq, vol7d_timerange_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_timerange_ne, vol7d_timerange_nesv
END INTERFACE

INTERFACE OPERATOR (>)
  MODULE PROCEDURE vol7d_timerange_gt, vol7d_timerange_gtsv
END INTERFACE

INTERFACE OPERATOR (<)
  MODULE PROCEDURE vol7d_timerange_lt, vol7d_timerange_ltsv
END INTERFACE

INTERFACE OPERATOR (>=)
  MODULE PROCEDURE vol7d_timerange_ge, vol7d_timerange_gesv
END INTERFACE

INTERFACE OPERATOR (<=)
  MODULE PROCEDURE vol7d_timerange_le, vol7d_timerange_lesv
END INTERFACE

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_timerange
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_timerange
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_timerange
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_timerange
END INTERFACE

CONTAINS

SUBROUTINE vol7d_timerange_init(this, timerange, p1, p2)
TYPE(vol7d_timerange),INTENT(INOUT) :: this
INTEGER,INTENT(IN),OPTIONAL :: timerange, p1, p2

IF (PRESENT(timerange)) THEN
  this%timerange = timerange
ELSE
  this%timerange = imiss
  this%p1 = imiss
  this%p2 = imiss
  RETURN
ENDIF
IF (timerange == 1) THEN ! p1 sempre 0
  this%p1 = 0
  this%p2 = imiss
ELSE IF (timerange == 0 .OR. timerange == 10) THEN ! solo p1
  IF (PRESENT(p1)) THEN
    this%p1 = p1
  ELSE
    this%p1 = 0
  ENDIF
  this%p2 = imiss
ELSE ! tutti gli altri
  IF (PRESENT(p1)) THEN
    this%p1 = p1
  ELSE
    this%p1 = imiss
  ENDIF
  IF (PRESENT(p2)) THEN
    this%p2 = p2
  ELSE
    this%p2 = imiss
  ENDIF
END IF

END SUBROUTINE vol7d_timerange_init


SUBROUTINE vol7d_timerange_delete(this)
TYPE(vol7d_timerange),INTENT(INOUT) :: this

this%timerange = imiss
this%p1 = imiss
this%p2 = imiss

END SUBROUTINE vol7d_timerange_delete


elemental FUNCTION vol7d_timerange_eq(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%timerange == that%timerange .AND. &
 this%p1 == that%p1 .AND. this%p2 == that%p2) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_timerange_eq


FUNCTION vol7d_timerange_eqsv(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION vol7d_timerange_eqsv


elemental FUNCTION vol7d_timerange_ne(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_timerange_ne


FUNCTION vol7d_timerange_nesv(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_timerange_nesv


elemental FUNCTION vol7d_timerange_gt(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%timerange > that%timerange .OR. &
 (this%timerange == that%timerange .AND. this%p1 > that%p1) .OR. &
 (this%timerange == that%timerange .AND. this%p1 == that%p1 .AND. &
 this%p2 > that%p2)) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_timerange_gt


FUNCTION vol7d_timerange_gtsv(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this > that(i)
ENDDO

END FUNCTION vol7d_timerange_gtsv


elemental FUNCTION vol7d_timerange_lt(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%timerange < that%timerange .OR. &
 (this%timerange == that%timerange .AND. this%p1 < that%p1) .OR. &
 (this%timerange == that%timerange .AND. this%p1 == that%p1 .AND. &
 this%p2 < that%p2)) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_timerange_lt


FUNCTION vol7d_timerange_ltsv(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this < that(i)
ENDDO

END FUNCTION vol7d_timerange_ltsv


elemental FUNCTION vol7d_timerange_ge(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this > that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_timerange_ge


FUNCTION vol7d_timerange_gesv(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this >= that(i)
ENDDO

END FUNCTION vol7d_timerange_gesv


elemental FUNCTION vol7d_timerange_le(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this < that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_timerange_le


FUNCTION vol7d_timerange_lesv(this, that) RESULT(res)
TYPE(vol7d_timerange),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this <= that(i)
ENDDO

END FUNCTION vol7d_timerange_lesv


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(vol7d_timerange)
#define VOL7D_POLY_TYPES _timerange
#include "vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_timerange_class
