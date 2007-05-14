MODULE vol7d_ana_class
USE kinds
USE missing_values
USE geo_coord_class
IMPLICIT NONE

INTEGER,PARAMETER :: vol7d_ana_lenident=20

TYPE vol7d_ana
  TYPE(geo_coord) :: coord
  CHARACTER(len=vol7d_ana_lenident) :: ident
END TYPE  vol7d_ana

! Deve essere dichiarata PARAMETER, non e` cosi` per un bug del pgi
TYPE(vol7d_ana) :: vol7d_ana_miss=vol7d_ana(geo_coord_miss,cmiss)

INTERFACE init
  MODULE PROCEDURE vol7d_ana_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_ana_delete
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_ana_eq, vol7d_ana_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_ana_ne, vol7d_ana_nesv
END INTERFACE

CONTAINS

SUBROUTINE vol7d_ana_init(this, lon, lat, ident)
TYPE(vol7d_ana),INTENT(INOUT) :: this
REAL(kind=fp_geo),INTENT(in),OPTIONAL :: lon, lat
CHARACTER(len=vol7d_ana_lenident),INTENT(in),OPTIONAL :: ident

CALL init(this%coord, lon=lon, lat=lat)
IF (PRESENT(ident)) THEN
  this%ident = ident
ELSE
  this%ident = cmiss
ENDIF

END SUBROUTINE vol7d_ana_init


SUBROUTINE vol7d_ana_delete(this)
TYPE(vol7d_ana),INTENT(INOUT) :: this

CALL delete(this%coord)
this%ident = cmiss

END SUBROUTINE vol7d_ana_delete


elemental FUNCTION vol7d_ana_eq(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%coord == that%coord .AND. this%ident == that%ident) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_ana_eq


FUNCTION vol7d_ana_eqsv(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION vol7d_ana_eqsv


elemental FUNCTION vol7d_ana_ne(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_ana_ne


FUNCTION vol7d_ana_nesv(this, that) RESULT(res)
TYPE(vol7d_ana),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_ana_nesv


END MODULE vol7d_ana_class
