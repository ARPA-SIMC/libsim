! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.



!> This module defines usefull general purpose function and subroutine
!!\ingroup base
#include "config.h"
MODULE array_utilities

IMPLICIT NONE

! la routine per i char non puo' essere sviluppata in macro perche` si deve scrivere diversa
!cosi' esiste la function count_distinctc (senza _ ) e la subroutine pack_distinctc qui ivi scritte

#undef VOL7D_POLY_TYPE_AUTO

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE INTEGER
#define VOL7D_POLY_TYPES _i
#define ENABLE_SORT
#include "array_utilities_pre.F90"
#undef ENABLE_SORT

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE REAL
#define VOL7D_POLY_TYPES _r
#define ENABLE_SORT
#include "array_utilities_pre.F90"
#undef ENABLE_SORT

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE DOUBLEPRECISION
#define VOL7D_POLY_TYPES _d
#define ENABLE_SORT
#include "array_utilities_pre.F90"
#undef ENABLE_SORT

#define VOL7D_NO_PACK
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE CHARACTER(len=*)
#define VOL7D_POLY_TYPE_AUTO(var) CHARACTER(len=LEN(var))
#define VOL7D_POLY_TYPES _c
#define ENABLE_SORT
#include "array_utilities_pre.F90"
#undef VOL7D_POLY_TYPE_AUTO
#undef ENABLE_SORT


#define ARRAYOF_ORIGEQ 1

#define ARRAYOF_ORIGTYPE INTEGER
#define ARRAYOF_TYPE arrayof_integer
#include "arrayof_pre.F90"

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE REAL
#define ARRAYOF_TYPE arrayof_real
#include "arrayof_pre.F90"

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE DOUBLEPRECISION
#define ARRAYOF_TYPE arrayof_doubleprecision
#include "arrayof_pre.F90"

#undef ARRAYOF_ORIGEQ

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE LOGICAL
#define ARRAYOF_TYPE arrayof_logical
#include "arrayof_pre.F90"

PRIVATE
! from arrayof
PUBLIC insert, append, remove, delete, packarray
PUBLIC insert_unique, append_unique

PUBLIC sort, index, index_c, &
 count_distinct, pack_distinct, count_and_pack_distinct, &
 map_distinct, map_inv_distinct, &
 firsttrue, lasttrue, pack_distinct_c, map

CONTAINS


!> Return the index ot the first true element of the input logical array \a v.
!! If no \c .TRUE. elements are found, it returns 0.
FUNCTION firsttrue(v) RESULT(i)
LOGICAL,INTENT(in) :: v(:) !< logical array to test
INTEGER :: i

DO i = 1, SIZE(v)
  IF (v(i)) RETURN
ENDDO
i = 0

END FUNCTION firsttrue


!> Return the index ot the last true element of the input logical array \a v.
!! If no \c .TRUE. elements are found, it returns 0.
FUNCTION lasttrue(v) RESULT(i)
LOGICAL,INTENT(in) :: v(:) !< logical array to test
INTEGER :: i

DO i = SIZE(v), 1, -1
  IF (v(i)) RETURN
ENDDO

END FUNCTION lasttrue


! Definisce le funzioni count_distinct e pack_distinct
#undef VOL7D_POLY_TYPE_AUTO
#undef VOL7D_NO_PACK

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE INTEGER
#define VOL7D_POLY_TYPES _i
#define ENABLE_SORT
#include "array_utilities_inc.F90"
#undef ENABLE_SORT

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE REAL
#define VOL7D_POLY_TYPES _r
#define ENABLE_SORT
#include "array_utilities_inc.F90"
#undef ENABLE_SORT

#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE DOUBLEPRECISION
#define VOL7D_POLY_TYPES _d
#define ENABLE_SORT
#include "array_utilities_inc.F90"
#undef ENABLE_SORT

#define VOL7D_NO_PACK
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES
#define VOL7D_POLY_TYPE CHARACTER(len=*)
#define VOL7D_POLY_TYPE_AUTO(var) CHARACTER(len=LEN(var))
#define VOL7D_POLY_TYPES _c
#define ENABLE_SORT
#include "array_utilities_inc.F90"
#undef VOL7D_POLY_TYPE_AUTO
#undef ENABLE_SORT

SUBROUTINE pack_distinct_c(vect, pack_distinct, mask, back) !RESULT(pack_distinct)
CHARACTER(len=*),INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
CHARACTER(len=LEN(vect)) :: pack_distinct(:)

INTEGER :: count_distinct
INTEGER :: i, j, dim
LOGICAL :: lback

dim = SIZE(pack_distinct)
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
!      DO j = i-1, 1, -1
!        IF (vect(j) == vect(i)) CYCLE vectm1
      DO j = count_distinct, 1, -1
        IF (pack_distinct(j) == vect(i)) CYCLE vectm1
      ENDDO
      count_distinct = count_distinct + 1
      IF (count_distinct > dim) EXIT
      pack_distinct(count_distinct) = vect(i)
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
!      DO j = 1, i-1
!        IF (vect(j) == vect(i)) CYCLE vectm2
      DO j = 1, count_distinct
        IF (pack_distinct(j) == vect(i)) CYCLE vectm2
      ENDDO
      count_distinct = count_distinct + 1
      IF (count_distinct > dim) EXIT
      pack_distinct(count_distinct) = vect(i)
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = 1, SIZE(vect)
!      DO j = i-1, 1, -1
!        IF (vect(j) == vect(i)) CYCLE vect1
      DO j = count_distinct, 1, -1
        IF (pack_distinct(j) == vect(i)) CYCLE vect1
      ENDDO
      count_distinct = count_distinct + 1
      IF (count_distinct > dim) EXIT
      pack_distinct(count_distinct) = vect(i)
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
!      DO j = 1, i-1
!        IF (vect(j) == vect(i)) CYCLE vect2
      DO j = 1, count_distinct
        IF (pack_distinct(j) == vect(i)) CYCLE vect2
      ENDDO
      count_distinct = count_distinct + 1
      IF (count_distinct > dim) EXIT
      pack_distinct(count_distinct) = vect(i)
    ENDDO vect2
  ENDIF
ENDIF

END SUBROUTINE pack_distinct_c

!> Return the index of the array only where the mask is true  
FUNCTION map(mask)  RESULT(mapidx)
LOGICAL,INTENT(in) :: mask(:)
INTEGER :: mapidx(count(mask))

INTEGER :: i,j

j = 0
DO i=1, SIZE(mask)
  j = j + 1
  IF (mask(i)) mapidx(j)=i
ENDDO

END FUNCTION map

#define ARRAYOF_ORIGEQ 1

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE INTEGER
#define ARRAYOF_TYPE arrayof_integer
#include "arrayof_post.F90"

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE REAL
#define ARRAYOF_TYPE arrayof_real
#include "arrayof_post.F90"

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE DOUBLEPRECISION
#define ARRAYOF_TYPE arrayof_doubleprecision
#include "arrayof_post.F90"

#undef ARRAYOF_ORIGEQ

#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE LOGICAL
#define ARRAYOF_TYPE arrayof_logical
#include "arrayof_post.F90"

END MODULE array_utilities
