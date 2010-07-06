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
!> conta gli elementi distinti in vect
FUNCTION count_distinct/**/VOL7D_POLY_TYPES(vect, mask, back) RESULT(count_distinct)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
INTEGER :: count_distinct

#ifdef VOL7D_POLY_TYPE_AUTO
VOL7D_POLY_TYPE_AUTO(vect) :: pack_distinct(SIZE(vect))
#else
VOL7D_POLY_TYPE :: pack_distinct(SIZE(vect))
#endif
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
!      DO j = i-1, 1, -1
!        IF (.NOT.mask(j)) CYCLE
!        IF (vect(j) == vect(i)) CYCLE vectm1
      DO j = count_distinct, 1, -1
        IF (pack_distinct(j) == vect(i)) CYCLE vectm1
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
!      DO j = 1, i-1
!        IF (.NOT.mask(j)) CYCLE
!        IF (vect(j) == vect(i)) CYCLE vectm2
      DO j = 1, count_distinct
        IF (pack_distinct(j) == vect(i)) CYCLE vectm2
      ENDDO
      count_distinct = count_distinct + 1
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
      pack_distinct(count_distinct) = vect(i)
    ENDDO vect2
  ENDIF
ENDIF

END FUNCTION count_distinct/**/VOL7D_POLY_TYPES


#ifndef VOL7D_NO_PACK
!> compatta gli elementi distinti di vect in un array
FUNCTION pack_distinct/**/VOL7D_POLY_TYPES(vect, dim, mask, back) &
 RESULT(pack_distinct)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:)
INTEGER,INTENT(in) :: dim
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
VOL7D_POLY_TYPE :: pack_distinct(dim)

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
!      DO j = i-1, 1, -1
!        IF (.NOT.mask(j)) CYCLE
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
!        IF (.NOT.mask(j)) CYCLE
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

END FUNCTION pack_distinct/**/VOL7D_POLY_TYPES
#endif

!> map distinct
FUNCTION map_distinct/**/VOL7D_POLY_TYPES(vect, mask, back) RESULT(map_distinct)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
INTEGER :: map_distinct(SIZE(vect))

INTEGER :: count_distinct
#ifdef VOL7D_POLY_TYPE_AUTO
VOL7D_POLY_TYPE_AUTO(vect) :: pack_distinct(SIZE(vect))
#else
VOL7D_POLY_TYPE :: pack_distinct(SIZE(vect))
#endif
INTEGER :: i, j
LOGICAL :: lback

IF (PRESENT(back)) THEN
  lback = back
ELSE
  lback = .FALSE.
ENDIF
count_distinct = 0
map_distinct(:) = 0

IF (PRESENT (mask)) THEN
  IF (lback) THEN
    vectm1: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm1
!      DO j = i-1, 1, -1
!        IF (.NOT.mask(j)) CYCLE
!        IF (vect(j) == vect(i)) THEN
!          map_distinct(i) = map_distinct(j)
      DO j = count_distinct, 1, -1
        IF (pack_distinct(j) == vect(i)) THEN
          map_distinct(i) = j
          CYCLE vectm1
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
      map_distinct(i) = count_distinct
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
!      DO j = 1, i-1
!        IF (.NOT.mask(j)) CYCLE
!        IF (vect(j) == vect(i)) THEN
!          map_distinct(i) = map_distinct(j)
      DO j = 1, count_distinct
        IF (pack_distinct(j) == vect(i)) THEN
          map_distinct(i) = j
          CYCLE vectm2
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
      map_distinct(i) = count_distinct
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = 1, SIZE(vect)
!      DO j = i-1, 1, -1
!        IF (vect(j) == vect(i)) THEN
!          map_distinct(i) = map_distinct(j)
      DO j = count_distinct, 1, -1
        IF (pack_distinct(j) == vect(i)) THEN
          map_distinct(i) = j
          CYCLE vect1
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
      map_distinct(i) = count_distinct
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
!      DO j = 1, i-1
!        IF (vect(j) == vect(i)) THEN
!          map_distinct(i) = map_distinct(j)
      DO j = 1, count_distinct
        IF (pack_distinct(j) == vect(i)) THEN
          map_distinct(i) = j
          CYCLE vect2
        ENDIF
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
      map_distinct(i) = count_distinct
    ENDDO vect2
  ENDIF
ENDIF

END FUNCTION map_distinct/**/VOL7D_POLY_TYPES


!> map inv distinct
FUNCTION map_inv_distinct/**/VOL7D_POLY_TYPES(vect, dim, mask, back) &
 RESULT(map_inv_distinct)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:)
INTEGER,INTENT(in) :: dim
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
INTEGER :: map_inv_distinct(dim)

INTEGER :: count_distinct
#ifdef VOL7D_POLY_TYPE_AUTO
VOL7D_POLY_TYPE_AUTO(vect) :: pack_distinct(SIZE(vect))
#else
VOL7D_POLY_TYPE :: pack_distinct(SIZE(vect))
#endif
INTEGER :: i, j
LOGICAL :: lback

IF (PRESENT(back)) THEN
  lback = back
ELSE
  lback = .FALSE.
ENDIF
count_distinct = 0
map_inv_distinct(:) = 0

IF (PRESENT (mask)) THEN
  IF (lback) THEN
    vectm1: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm1
!      DO j = i-1, 1, -1
!        IF (.NOT.mask(j)) CYCLE
!        IF (vect(j) == vect(i)) CYCLE vectm1
      DO j = count_distinct, 1, -1
        IF (pack_distinct(j) == vect(i)) CYCLE vectm1
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
      IF (count_distinct > dim) EXIT
      map_inv_distinct(count_distinct) = i
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
!      DO j = 1, i-1
!        IF (.NOT.mask(j)) CYCLE
!        IF (vect(j) == vect(i)) CYCLE vectm2
      DO j = 1, count_distinct
        IF (pack_distinct(j) == vect(i)) CYCLE vectm2
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
      IF (count_distinct > dim) EXIT
      map_inv_distinct(count_distinct) = i
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
      pack_distinct(count_distinct) = vect(i)
      IF (count_distinct > dim) EXIT
      map_inv_distinct(count_distinct) = i
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
!      DO j = 1, i-1
!        IF (vect(j) == vect(i)) CYCLE vect2
      DO j = 1, count_distinct
        IF (pack_distinct(j) == vect(i)) CYCLE vect2
      ENDDO
      count_distinct = count_distinct + 1
      pack_distinct(count_distinct) = vect(i)
      IF (count_distinct > dim) EXIT
      map_inv_distinct(count_distinct) = i
    ENDDO vect2
  ENDIF
ENDIF

END FUNCTION map_inv_distinct/**/VOL7D_POLY_TYPES


#ifndef VOL7D_NO_PACK
!> Cerca l'indice del primo o ultimo elemento di vect uguale a search
FUNCTION index/**/VOL7D_POLY_TYPES(vect, search, mask, back) &
 RESULT(index_)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:), search
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
INTEGER :: index_

INTEGER :: i
LOGICAL :: lback

IF (PRESENT(back)) THEN
  lback = back
ELSE
  lback = .FALSE.
ENDIF
index_ = 0

IF (PRESENT (mask)) THEN
  IF (lback) THEN
    vectm1: DO i = SIZE(vect), 1, -1
      IF (.NOT.mask(i)) CYCLE vectm1
      IF (vect(i) == search) THEN
        index_ = i
        RETURN
      ENDIF
    ENDDO vectm1
  ELSE
    vectm2: DO i = 1, SIZE(vect)
      IF (.NOT.mask(i)) CYCLE vectm2
      IF (vect(i) == search) THEN
        index_ = i
        RETURN
      ENDIF
    ENDDO vectm2
  ENDIF
ELSE
  IF (lback) THEN
    vect1: DO i = SIZE(vect), 1, -1
      IF (vect(i) == search) THEN
        index_ = i
        RETURN
      ENDIF
    ENDDO vect1
  ELSE
    vect2: DO i = 1, SIZE(vect)
      IF (vect(i) == search) THEN
        index_ = i
        RETURN
      ENDIF
    ENDDO vect2
  ENDIF
ENDIF

END FUNCTION index/**/VOL7D_POLY_TYPES
#endif
