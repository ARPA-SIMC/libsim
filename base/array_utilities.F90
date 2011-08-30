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

!> to document
INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_i, count_distinct_r, count_distinct_d, &
   count_distinct_c
END INTERFACE

!> to document
INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_i, pack_distinct_r, pack_distinct_d
END INTERFACE

!> to document
INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_i, map_distinct_r, map_distinct_d, &
   map_distinct_c
END INTERFACE

!> to document
INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_i, map_inv_distinct_r, map_inv_distinct_d, &
   map_inv_distinct_c
END INTERFACE

!> Find the firsth or last index of an element in a vector equal to the values provided
INTERFACE index
  MODULE PROCEDURE index_i, index_r, index_d
END INTERFACE

!>\brief Sorts inline into ascending order.
!!  Quicksort chooses a "pivot" in the set, and explores the
!!  array from both ends, looking for a value > pivot with the
!!  increasing index, for a value <= pivot with the decreasing
!!  index, and swapping them when it has found one of each.
!!  The array is then subdivided in 2 ([3]) subsets:
!!  { values <= pivot} {pivot} {values > pivot}
!!  One then call recursively the program to sort each subset.
!!  When the size of the subarray is small enough, one uses an
!!  insertion sort that is faster for very small sets.
INTERFACE sort
  MODULE PROCEDURE sort_i, sort_r, sort_d, sort_c
END INTERFACE

private
public sort, index ,index_c
public count_distinct, pack_distinct, map_distinct, map_inv_distinct, firsttrue
public pack_distinct_c, map

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
#undef VOL7D_POLY_TYPE_AUTO

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

j=0

do i=1, size(mask)

  J=j+1
  if (mask(i)) mapidx(j)=i

end do
END FUNCTION map

END MODULE array_utilities
