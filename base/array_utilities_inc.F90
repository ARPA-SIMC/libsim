! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! sort from public domain utilities http://www.fortran-2000.com :
! Michel Olagnon - Apr. 2000

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



#ifdef ENABLE_SORT

!>\brief Sorts inline into ascending order - Quicksort
!!  Quicksort chooses a "pivot" in the set, and explores the
!!  array from both ends, looking for a value > pivot with the
!!  increasing index, for a value <= pivot with the decreasing
!!  index, and swapping them when it has found one of each.
!!  The array is then subdivided in 2 ([3]) subsets:
!!  { values <= pivot} {pivot} {values > pivot}
!!  One then call recursively the program to sort each subset.
!!  When the size of the subarray is small enough, one uses an
!!  insertion sort that is faster for very small sets.

Subroutine sort/**/VOL7D_POLY_TYPES (XDONT)

!  Sorts XDONT into ascending order - Quicksort
!  Michel Olagnon - Apr. 2000
! _________________________________________________________

VOL7D_POLY_TYPE, Dimension (:), Intent (InOut) :: XDONT !> vector to sort inline
! __________________________________________________________
!
!
      Call subsor/**/VOL7D_POLY_TYPES (XDONT, 1, Size (XDONT))
      Call inssor/**/VOL7D_POLY_TYPES (XDONT)
      Return
End Subroutine sort/**/VOL7D_POLY_TYPES
Recursive Subroutine subsor/**/VOL7D_POLY_TYPES (XDONT, IDEB1, IFIN1)
!  Sorts XDONT from IDEB1 to IFIN1
! __________________________________________________________
      VOL7D_POLY_TYPE, dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL

#ifdef VOL7D_POLY_TYPE_AUTO
      VOL7D_POLY_TYPE_AUTO(XDONT) :: XPIV, XWRK
#else
      VOL7D_POLY_TYPE ::   XPIV, XWRK
#endif

!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (XDONT(IMIL) < XDONT(IDEB)) Then
            XWRK = XDONT (IDEB)
            XDONT (IDEB) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
         End If
         If (XDONT(IMIL) > XDONT(IFIN)) Then
            XWRK = XDONT (IFIN)
            XDONT (IFIN) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
            If (XDONT(IMIL) < XDONT(IDEB)) Then
               XWRK = XDONT (IDEB)
               XDONT (IDEB) = XDONT (IMIL)
               XDONT (IMIL) = XWRK
            End If
         End If
         XPIV = XDONT (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if XDONT (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (XDONT(ICRS) > XPIV) Exit
            End Do
            Do
               If (XDONT(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = XDONT (IDCR)
            XDONT (IDCR) = XDONT (ICRS)
            XDONT (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call subsor/**/VOL7D_POLY_TYPES (XDONT, IDEB1, ICRS-1)
         Call subsor/**/VOL7D_POLY_TYPES (XDONT, IDCR, IFIN1)
      End If
      Return
      End Subroutine Subsor/**/VOL7D_POLY_TYPES
   Subroutine inssor/**/VOL7D_POLY_TYPES  (XDONT)
!  Sorts XDONT into increasing order (Insertion sort)
! __________________________________________________________
      VOL7D_POLY_TYPE, dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
      Integer :: ICRS, IDCR

#ifdef VOL7D_POLY_TYPE_AUTO
      VOL7D_POLY_TYPE_AUTO(XDONT) :: XWRK
#else
      VOL7D_POLY_TYPE :: XWRK
#endif

!
      Do ICRS = 2, Size (XDONT)
         XWRK = XDONT (ICRS)
         If (XWRK >= XDONT(ICRS-1)) Cycle
         XDONT (ICRS) = XDONT (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
      End Do
!
      Return
!
      End Subroutine inssor/**/VOL7D_POLY_TYPES
!

#endif