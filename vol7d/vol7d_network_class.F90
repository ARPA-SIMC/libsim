MODULE vol7d_network_class
USE kinds
USE missing_values
IMPLICIT NONE

TYPE vol7d_network
  INTEGER :: id
END TYPE vol7d_network

TYPE(vol7d_network),PARAMETER :: vol7d_network_miss=vol7d_network(imiss)

INTERFACE init
  MODULE PROCEDURE vol7d_network_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_network_delete
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE vol7d_network_eq, vol7d_network_eqsv
END INTERFACE

INTERFACE OPERATOR (/=)
  MODULE PROCEDURE vol7d_network_ne, vol7d_network_nesv
END INTERFACE

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_network
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_network
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_network
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_network
END INTERFACE

CONTAINS

SUBROUTINE vol7d_network_init(this, id)
TYPE(vol7d_network),INTENT(INOUT) :: this
INTEGER,INTENT(in),optional :: id

IF (PRESENT(id)) THEN
  this%id = id
ELSE
  this%id = imiss
END IF

END SUBROUTINE vol7d_network_init


SUBROUTINE vol7d_network_delete(this)
TYPE(vol7d_network),INTENT(INOUT) :: this

this%id = imiss

END SUBROUTINE vol7d_network_delete


elemental FUNCTION vol7d_network_eq(this, that) RESULT(res)
TYPE(vol7d_network),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%id == that%id) THEN
  res = .TRUE.
ELSE
  res = .FALSE.
ENDIF

END FUNCTION vol7d_network_eq


FUNCTION vol7d_network_eqsv(this, that) RESULT(res)
TYPE(vol7d_network),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = this == that(i)
ENDDO

END FUNCTION vol7d_network_eqsv


elemental FUNCTION vol7d_network_ne(this, that) RESULT(res)
TYPE(vol7d_network),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION vol7d_network_ne


FUNCTION vol7d_network_nesv(this, that) RESULT(res)
TYPE(vol7d_network),INTENT(IN) :: this, that(:)
LOGICAL :: res(SIZE(that))

INTEGER :: i

DO i = 1, SIZE(that)
  res(i) = .NOT.(this == that(i))
ENDDO

END FUNCTION vol7d_network_nesv


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(vol7d_network)
#define VOL7D_POLY_TYPES _network
#include "vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


END MODULE vol7d_network_class
