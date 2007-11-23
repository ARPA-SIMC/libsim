MODULE vol7d_utilities
USE file_utilities
USE err_handling
USE datetime_class
USE kinds

IMPLICIT NONE

! la routine per i char non puo' essere sviluppata in macro perche` si deve scrivere diversa
!cosi' esiste la function count_distinctc (senza _ ) e la subroutine pack_distinctc qui ivi scritte

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_i, count_distinct_r, count_distinct_d, &
   count_distinct_datetime, count_distinct_c
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_i, pack_distinct_r, pack_distinct_d, &
   pack_distinct_datetime !, pack_distinct_c
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_i, map_distinct_r, map_distinct_d, &
   map_distinct_datetime, map_distinct_c
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_i, map_inv_distinct_r, map_inv_distinct_d, &
   map_inv_distinct_datetime, map_inv_distinct_c
END INTERFACE

INTERFACE index
  MODULE PROCEDURE index_i, index_r, index_d, &
   index_datetime !, index_c
END INTERFACE

CONTAINS


! Ritorna l'indice del primo elemento vero del vettore logico v
FUNCTION firsttrue(v) RESULT(i)
LOGICAL,INTENT(in) :: v(:)
INTEGER :: i

DO i = 1, SIZE(v)
  IF (v(i)) RETURN
ENDDO
i = 0

END FUNCTION firsttrue


! Definisce le funzioni count_distinct e pack_distinct
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE INTEGER
#define VOL7D_POLY_TYPES _i
#include "vol7d_distinct.F90"

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE REAL
#define VOL7D_POLY_TYPES _r
#include "vol7d_distinct.F90"

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE REAL(kind=fp_d)
#define VOL7D_POLY_TYPES _d
#include "vol7d_distinct.F90"

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE TYPE(datetime)
#define VOL7D_POLY_TYPES _datetime
#include "vol7d_distinct.F90"

#define VOL7D_NO_PACK
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE CHARACTER(len=*)
#define VOL7D_POLY_TYPES _c
#include "vol7d_distinct.F90"


SUBROUTINE pack_distinct_c(vect, pack_distinct, mask, back) !RESULT(pack_distinct)
CHARACTER(len=*),INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
CHARACTER(len=LEN(vect)) :: pack_distinct(SIZE(vect))

INTEGER :: count_distinct
INTEGER :: i, j
LOGICAL :: lback

IF (PRESENT(back)) THEN
  lback = back
ELSE
  lback = .FALSE.
ENDIF
count_distinct = 0

IF (PRESENT (mask)) THEN
  IF (lback) THEN
    vectm1: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm1
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) CYCLE vectm1
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
      DO j = 1, i-1
        IF (vect(j) == vect(i)) CYCLE vectm2
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = 1, SIZE(vect)
      DO j = i-1, 1, -1
        IF (vect(j) == vect(i)) CYCLE vect1
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
      DO j = 1, i-1
        IF (vect(j) == vect(i)) CYCLE vect2
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vect2
  ENDIF
ENDIF

END SUBROUTINE pack_distinct_c


END MODULE vol7d_utilities
