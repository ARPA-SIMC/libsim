MODULE vol7d_level_class
USE kinds
USE missing_values
IMPLICIT NONE

TYPE vol7d_level
  INTEGER :: level,l1,l2
END TYPE  vol7d_level

TYPE(vol7d_level),PARAMETER :: vol7d_level_miss=vol7d_level(imiss,imiss,imiss)

INTERFACE init
  MODULE PROCEDURE vol7d_level_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_level_delete
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_level_eq, vol7d_level_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_level_ne, vol7d_level_nesv
END INTERFACE

INTERFACE OPERATOR (>)
  MODULE PROCEDURE vol7d_level_gt, vol7d_level_gtsv
END INTERFACE

INTERFACE OPERATOR (<)
  MODULE PROCEDURE vol7d_level_lt, vol7d_level_ltsv
END INTERFACE

INTERFACE OPERATOR (>=)
  MODULE PROCEDURE vol7d_level_ge, vol7d_level_gesv
END INTERFACE

INTERFACE OPERATOR (<=)
  MODULE PROCEDURE vol7d_level_le, vol7d_level_lesv
END INTERFACE

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_level
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_level
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_level
END INTERFACE

CONTAINS

SUBROUTINE vol7d_level_init(this, level, l1, l2)
TYPE(vol7d_level),INTENT(INOUT) :: this
INTEGER,INTENT(IN),OPTIONAL :: level, l1, l2

IF (PRESENT(level)) THEN
  this%level = level
ELSE
  this%level = imiss
  this%l1 = imiss
  this%l2 = imiss
  RETURN
ENDIF

IF (level > 0 .AND. level < 10) THEN ! nessun l1/l2
  this%l1 = imiss
  this%l2 = imiss
ELSE IF (level == 20 .OR. level == 100 .OR. level == 103 .OR. level == 105 &
 .OR. level == 107 .OR. level == 109 .OR. level == 111 .OR. level == 113 &
 .OR. level == 115 .OR. level == 119 .OR. level == 125 .OR. level == 160) THEN ! solo l1
  IF (PRESENT(l1)) THEN
    this%l1 = l1
  ELSE
    this%l1 = 0
  ENDIF
  this%l2 = imiss
ELSE ! Tutti gli altri
  IF (PRESENT(l1)) THEN
    this%l1 = l1
  ELSE
    this%l1 = imiss
  ENDIF
  IF (PRESENT(l2)) THEN
    this%l2 = l2
  ELSE
    this%l2 = imiss
  ENDIF
END IF

END SUBROUTINE vol7d_level_init


SUBROUTINE vol7d_level_delete(this)
TYPE(vol7d_level),INTENT(INOUT) :: this

this%level = imiss
this%l1 = imiss
this%l2 = imiss

END SUBROUTINE vol7d_level_delete


elemental FUNCTION vol7d_level_eq(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%level == that%level .AND. &
 this%l1 == that%l1 .AND. this%l2 == that%l2) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_eq


FUNCTION vol7d_level_eqsv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION vol7d_level_eqsv


elemental FUNCTION vol7d_level_ne(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_level_ne


FUNCTION vol7d_level_nesv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_level_nesv


elemental FUNCTION vol7d_level_gt(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%level > that%level .OR. &
 (this%level == that%level .AND. this%l1 > that%l1) .OR. &
 (this%level == that%level .AND. this%l1 == that%l1 .AND. &
 this%l2 > that%l2)) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_gt


FUNCTION vol7d_level_gtsv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this > that(i)
ENDDO

END FUNCTION vol7d_level_gtsv


elemental FUNCTION vol7d_level_lt(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%level < that%level .OR. &
 (this%level == that%level .AND. this%l1 < that%l1) .OR. &
 (this%level == that%level .AND. this%l1 == that%l1 .AND. &
 this%l2 < that%l2)) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_lt


FUNCTION vol7d_level_ltsv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this < that(i)
ENDDO

END FUNCTION vol7d_level_ltsv


elemental FUNCTION vol7d_level_ge(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this > that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_ge


FUNCTION vol7d_level_gesv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this >= that(i)
ENDDO

END FUNCTION vol7d_level_gesv


elemental FUNCTION vol7d_level_le(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that
LOGICAL :: res

IF (this == that) THEN
  res = .TRUE.
ELSE IF (this < that) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_level_le


FUNCTION vol7d_level_lesv(this, that) RESULT(res)
TYPE(vol7d_level),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this <= that(i)
ENDDO

END FUNCTION vol7d_level_lesv


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(vol7d_level)
#define VOL7D_POLY_TYPES _level
#include "vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_level_class
