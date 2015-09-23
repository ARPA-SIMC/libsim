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

#ifdef ENABLE_SORT
!> conta gli elementi distinti in un sorted array
FUNCTION count_distinct_sorted/**/VOL7D_POLY_TYPES(vect, mask) RESULT(count_distinct_sorted)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:)
LOGICAL,INTENT(in),OPTIONAL :: mask(:)
INTEGER :: count_distinct_sorted

INTEGER :: i, j

count_distinct_sorted = 0

j=1
i = 1
do while (i <= size(vect))
  if (present(mask)) then
    do while (.not. mask(i))
      i=i+1
      if ( i > size(vect)) return
    end do
  end if
                                ! count the first
  if (i==j)  count_distinct_sorted = count_distinct_sorted + 1

  if (vect(j) /= vect(i)) then
    count_distinct_sorted = count_distinct_sorted + 1
    j = i
  end if

  i = i+1

end do

END FUNCTION count_distinct_sorted/**/VOL7D_POLY_TYPES
#endif

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

#ifdef ENABLE_SORT
!> compatta gli elementi distinti di vect in un sorted array
FUNCTION pack_distinct_sorted/**/VOL7D_POLY_TYPES(vect, dim, mask) &
 RESULT(pack_distinct_sorted)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:)
INTEGER,INTENT(in) :: dim
LOGICAL,INTENT(in),OPTIONAL :: mask(:)
VOL7D_POLY_TYPE :: pack_distinct_sorted(dim)

INTEGER :: i,count_distinct

if (dim < 1) return

count_distinct = 0

DO i = 1, SIZE(vect)
  IF (PRESENT (mask)) THEN
    IF (.NOT.mask(i)) CYCLE
  end IF

  if (count_distinct == 0) then
    count_distinct = count_distinct + 1
    pack_distinct_sorted(count_distinct)=vect(i)
  end if
  if (pack_distinct_sorted(count_distinct) == vect(i)) CYCLE
  count_distinct = count_distinct + 1
  if (count_distinct > dim) return
  pack_distinct_sorted(count_distinct)=vect(i)
  
ENDDO

END FUNCTION pack_distinct_sorted/**/VOL7D_POLY_TYPES
#endif

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


FUNCTION count_and_pack_distinct/**/VOL7D_POLY_TYPES(vect, pack_distinct, mask, back) RESULT(count_distinct)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:)
#ifdef VOL7D_POLY_TYPE_AUTO
VOL7D_POLY_TYPE_AUTO(vect),INTENT(out) :: pack_distinct(:)
#else
VOL7D_POLY_TYPE,INTENT(out) :: pack_distinct(:)
#endif
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
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

END FUNCTION count_and_pack_distinct/**/VOL7D_POLY_TYPES
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
FUNCTION index/**/VOL7D_POLY_TYPES(vect, search, mask, back, cache) &
 RESULT(index_)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:), search
LOGICAL,INTENT(in),OPTIONAL :: mask(:)
LOGICAL,INTENT(in),OPTIONAL :: back
INTEGER,INTENT(in),OPTIONAL :: cache
INTEGER :: index_

INTEGER :: i, lcache
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
  IF (PRESENT(cache)) THEN
    lcache = MAX(MIN(SIZE(vect),cache),1)
    DO i = lcache, SIZE(vect)
      IF (vect(i) == search) THEN
        index_ = i
        RETURN
      ENDIF
    ENDDO
    DO i = 1, lcache-1
      IF (vect(i) == search) THEN
        index_ = i
        RETURN
      ENDIF
    ENDDO
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
ENDIF

END FUNCTION index/**/VOL7D_POLY_TYPES


#ifdef ENABLE_SORT


!> Cerca l'indice del primo o ultimo elemento di vect uguale a search
recursive FUNCTION index_sorted/**/VOL7D_POLY_TYPES(vect, search) &
 RESULT(index_)
VOL7D_POLY_TYPE,INTENT(in) :: vect(:), search
INTEGER :: index_

integer ::  mid
 
    mid = size(vect)/2 + 1

!!$    if (size(vect) == 0) then
!!$      index_ = 0        ! not found
!!$
!!$    else if (size(vect) == 1) then
!!$      if (vect(1) == search) then
!!$        index_ = 1
!!$      else
!!$        index_ = 0        ! not found
!!$      end if
!!$ else if .....

    if (size(vect) < 10) then
      !print *,"call index with size: ",size(vect)
      index_=index(vect, search)     ! sequential search for few number
      !print *,"returned: ",index_
    else if (vect(mid) > search) then
      !print *,"call index_sorted  -->",mid-1
      index_= index_sorted/**/VOL7D_POLY_TYPES(vect(:mid-1), search)
    else if (vect(mid) < search) then
      !print *,"call index_sorted",mid+1,"<--"
      index_ = index_sorted/**/VOL7D_POLY_TYPES(vect(mid+1:), search)
      if (index_ /= 0) then
        index_ = mid + index_
      end if
    else
      index_ = mid      ! SUCCESS!!
    end if
    
END FUNCTION index_sorted/**/VOL7D_POLY_TYPES


!!$Da Wikipedia, l'enciclopedia libera.
!!$Il merge sort   un algoritmo di ordinamento abbastanza rapido che utilizza un processo di risoluzione ricorsivo.
!!$Raffigurazione grafica delle versioni iterativa e ricorsiva dell'algoritmo merge sort.
!!$
!!$L'idea alla base del merge sort   il procedimento Divide et Impera, che consiste nella suddivisione del problema in sottoproblemi via via pi  piccoli.
!!$
!!$Il merge sort opera quindi dividendo l'insieme da ordinare in due met  e procedendo all'ordinamento delle medesime ricorsivamente. Quando si sono divise tutte le met  si procede alla loro fusione (merge appunto) costruendo un insieme ordinato.
!!$
!!$L'algoritmo fu inventato da John von Neumann nel 1945.
!!$
!!$ Pseudocodice [modifica]
!!$
!!$ merge (a[], left, center, right)  
!!$    i   left
!!$    j   center + 1
!!$    k   0
!!$ 
!!$    while ((i <= center) && (j <= right)) do
!!$       if (a[i] <= a[j])
!!$        then
!!$          b[k]   a[i]
!!$          i   i + 1
!!$        else
!!$           b[k]   a[j]
!!$           j   j + 1  
!!$       k   k + 1
!!$    end while
!!$ 
!!$    while (i <= center) do
!!$          b[k]   a[i]
!!$          i   i + 1
!!$          k   k + 1
!!$    end while
!!$ 
!!$    while (j <= right) do
!!$          b[k]   a[j] 
!!$          j   j + 1
!!$         k   k + 1
!!$    end while
!!$ 
!!$    for k   left to right do
!!$        a[k]   b[k - left]
!!$ 
!!$ mergesort (a[], left, right)
!!$    if (left < right) then
!!$        center   (left + right) / 2
!!$        mergesort(a, left, center)
!!$        mergesort(a, center+1, right)
!!$        merge(a, left, center, right)
!!$

!!$Bottom-up merge sort
!!$
!!$ Bottom-up merge sort is a non-recursive variant of the merge sort, 
!!$ in which the array is sorted by a sequence of passes. During each pass,
!!$ the array is divided into blocks of size m\,. (Initially, m=1\,).
!!$ Every two adjacent blocks are merged (as in normal merge sort), and the next pass is made with a twice larger value of m\,.
!!$
!!$In pseudo-code:
!!$
!!$Input: array a[] indexed from 0 to n-1.
!!$
!!$m = 1
!!$while m < n do
!!$    i = 0
!!$    while i < n-m do
!!$        merge subarrays a[i..i+m-1] and a[i+m .. min(i+2*m-1,n-1)] in-place.
!!$        i = i + 2 * m
!!$    m = m * 2
!!$

!>\brief Sorts inline into ascending order - Quicksort
!!  Quicksort chooses a "pivot" in the set, and explores the
!!  array from both ends, looking for a value > pivot with the
!!  increasing index, for a value <= pivot with the decreasing
!!  index, and swapping them when it has found one of each.
!!  The array is then subdivided in 2 ([3]) subsets:
!!  { values <= pivot} {pivot} {values > pivot}
!!  One then call recursively the program to sort each subset.
!!  When the size of the subarray is small enough or the maximum
!!  level of recursion is gained, one uses an
!!  insertion sort that is faster for very small sets.
Subroutine sort/**/VOL7D_POLY_TYPES (XDONT)

!  Sorts XDONT into ascending order - Quicksort
!  Michel Olagnon - Apr. 2000
! _________________________________________________________

VOL7D_POLY_TYPE, Dimension (:), Intent (InOut) :: XDONT !< vector to sort inline
integer :: recursion
! __________________________________________________________
!
!
      recursion=0
      Call subsor/**/VOL7D_POLY_TYPES (XDONT, 1, Size (XDONT), recursion)
      Call inssor/**/VOL7D_POLY_TYPES (XDONT)
      Return
End Subroutine sort/**/VOL7D_POLY_TYPES
Recursive Subroutine subsor/**/VOL7D_POLY_TYPES (XDONT, IDEB1, IFIN1, recursion)
!  Sorts XDONT from IDEB1 to IFIN1
! __________________________________________________________
      VOL7D_POLY_TYPE, dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (In) :: IDEB1, IFIN1
      Integer, Intent (InOut) :: recursion
! __________________________________________________________
      Integer, Parameter :: NINS = 16 , maxrec=5000 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL

#ifdef VOL7D_POLY_TYPE_AUTO
      VOL7D_POLY_TYPE_AUTO(XDONT) :: XPIV, XWRK
#else
      VOL7D_POLY_TYPE ::   XPIV, XWRK
#endif

      print *,"recursion:",recursion
!
      recursion=recursion+1
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS .and. recursion <= maxrec*2 ) Then
        print *,"subsor:",ifin-ideb

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
        Call subsor/**/VOL7D_POLY_TYPES (XDONT, IDEB1, ICRS-1, recursion)
        Call subsor/**/VOL7D_POLY_TYPES (XDONT, IDCR, IFIN1, recursion)

!!$      else
!!$        Call inssor/**/VOL7D_POLY_TYPES (XDONT(IDEB:IFIN))

      End If
      Return
      End Subroutine Subsor/**/VOL7D_POLY_TYPES


!> \brief Sorts into increasing order (Insertion sort)
!!  Sorts XDONT into increasing order (Insertion sort)
!!  This subroutine uses insertion sort. It does not use any
!!  work array and is faster when XDONT is of very small size
!!  (< 20), or already almost sorted, so it is used in a final
!!  pass when the partial quicksorting has left a sequence
!!  of small subsets and that sorting is only necessary within
!!  each subset to complete the process.
!!  Michel Olagnon - Apr. 2000
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

      print *,"inssor:",size(xdont)

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



!!$Heapsort is an in-place sorting algorithm with worst case and average
!!$complexity of O(n logn).
!!$
!!$The basic idea is to turn the array into a binary heap structure,
!!$which has the property that it allows efficient retrieval and removal
!!$of the maximal element. We repeatedly "remove" the maximal element
!!$from the heap, thus building the sorted list from back to
!!$front. Heapsort requires random access, so can only be used on an
!!$array-like data structure.

subroutine heapsort/**/VOL7D_POLY_TYPES(a)

VOL7D_POLY_TYPE, intent(in out) :: a(0:)
 
#ifdef VOL7D_POLY_TYPE_AUTO
      VOL7D_POLY_TYPE_AUTO(a) :: temp
#else
      VOL7D_POLY_TYPE :: temp
#endif

integer :: start, n, bottom
 
n = size(a)
do start = (n - 2) / 2, 0, -1
  call siftdown(a, start, n);
end do

do bottom = n - 1, 1, -1
  temp = a(0)
  a(0) = a(bottom)
  a(bottom) = temp;
  call siftdown(a, 0, bottom)
end do
   
contains 
subroutine siftdown(a, start, bottom)
 
VOL7D_POLY_TYPE, intent(in out) :: a(0:)

#ifdef VOL7D_POLY_TYPE_AUTO
VOL7D_POLY_TYPE_AUTO(a) :: temp
#else
VOL7D_POLY_TYPE :: temp
#endif

integer, intent(in) :: start, bottom
integer :: child, root

root = start
do while(root*2 + 1 < bottom)
  child = root * 2 + 1
  
  if (child + 1 < bottom) then
    if (a(child) < a(child+1)) child = child + 1
  end if
 
  if (a(root) < a(child)) then
    temp = a(child)
    a(child) = a (root)
    a(root) = temp
    root = child
  else
    return
  end if
end do
 
end subroutine siftdown

end subroutine heapsort/**/VOL7D_POLY_TYPES


! oppure



!*****************************************************
!*  Sorts an array RA of length N in ascending order *
!*                by the Heapsort method             *
!* ------------------------------------------------- *
!* INPUTS:                                           *
!*	    N	  size of table RA                   *
!*          RA	  table to be sorted                 *
!* OUTPUT:                                           *
!*	    RA    table sorted in ascending order    *
!*                                                   *
!* NOTE: The Heapsort method is a N Log2 N routine,  *
!*       and can be used for very large arrays.      *
!*****************************************************         
SUBROUTINE HPSORT/**/VOL7D_POLY_TYPES(RA)

VOL7D_POLY_TYPE,intent(INOUT) ::  RA(:)

#ifdef VOL7D_POLY_TYPE_AUTO
VOL7D_POLY_TYPE_AUTO(RA) :: RRA
#else
VOL7D_POLY_TYPE RRA
#endif

integer :: i,j,l,ir

IR=size(ra)
L=ir/2+1

                                !The index L will be decremented from its initial value during the
                                !"hiring" (heap creation) phase. Once it reaches 1, the index IR 
                                !will be decremented from its initial value down to 1 during the
                                !"retirement-and-promotion" (heap selection) phase.
do while(.true.)
  if(L > 1)then
    L=L-1
    RRA=RA(L)
  else
    RRA=RA(IR)
    RA(IR)=RA(1)
    IR=IR-1
    if(IR.eq.1)then
      RA(1)=RRA
      return
    end if
  end if
  I=L
  J=L+L
do while(J.le.IR)
  if(J < IR)then
    if(RA(J) < RA(J+1))  J=J+1
  end if
  if(RRA < RA(J))then
    RA(I)=RA(J)
    I=J; J=J+J
  else
    J=IR+1
  end if
end do

RA(I)=RRA

end do

END SUBROUTINE HPSORT/**/VOL7D_POLY_TYPES



!!$Selection sort
!!$
!!$L'ordinamento per selezione (selection sort) è un algoritmo di
!!$ordinamento che opera in place ed in modo simile all'ordinamento per
!!$inserzione. L'algoritmo è di tipo non adattivo, ossia il suo tempo di
!!$esecuzione non dipende dall'input ma dalla dimensione dell'array.
!!$
!!$Descrizione dell'algoritmo
!!$
!!$L'algoritmo seleziona di volta in volta il numero minore nella
!!$sequenza di partenza e lo sposta nella sequenza ordinata; di fatto la
!!$sequenza viene suddivisa in due parti: la sottosequenza ordinata, che
!!$occupa le prime posizioni dell'array, e la sottosequenza da ordinare,
!!$che costituisce la parte restante dell'array.
!!$
!!$Dovendo ordinare un array A di lunghezza n, si fa scorrere l'indice i
!!$da 1 a n-1 ripetendo i seguenti passi:
!!$
!!$    si cerca il più piccolo elemento della sottosequenza A[i..n];
!!$    si scambia questo elemento con l'elemento i-esimo.
!!$

!!$! --------------------------------------------------------------------
!!$! INTEGER FUNCTION  FindMinimum():
!!$!    This function returns the location of the minimum in the section
!!$! between Start and End.
!!$! --------------------------------------------------------------------
!!$
!!$   INTEGER FUNCTION  FindMinimum(x, Start, End)
!!$      IMPLICIT  NONE
!!$      INTEGER, DIMENSION(1:), INTENT(IN) :: x
!!$      INTEGER, INTENT(IN)                :: Start, End
!!$      INTEGER                            :: Minimum
!!$      INTEGER                            :: Location
!!$      INTEGER                            :: i
!!$
!!$      Minimum  = x(Start)		! assume the first is the min
!!$      Location = Start			! record its position
!!$      DO i = Start+1, End		! start with next elements
!!$         IF (x(i) < Minimum) THEN	!   if x(i) less than the min?
!!$            Minimum  = x(i)		!      Yes, a new minimum found
!!$            Location = i                !      record its position
!!$         END IF
!!$      END DO
!!$      FindMinimum = Location        	! return the position
!!$   END FUNCTION  FindMinimum
!!$
!!$! --------------------------------------------------------------------
!!$! SUBROUTINE  Swap():
!!$!    This subroutine swaps the values of its two formal arguments.
!!$! --------------------------------------------------------------------
!!$
!!$   SUBROUTINE  Swap(a, b)
!!$      IMPLICIT  NONE
!!$      INTEGER, INTENT(INOUT) :: a, b
!!$      INTEGER                :: Temp
!!$
!!$      Temp = a
!!$      a    = b
!!$      b    = Temp
!!$   END SUBROUTINE  Swap
!!$
!!$! --------------------------------------------------------------------
!!$! SUBROUTINE  Sort():
!!$!    This subroutine receives an array x() and sorts it into ascending
!!$! order.
!!$! --------------------------------------------------------------------
!!$
!!$   SUBROUTINE  Sort(x, Size)
!!$      IMPLICIT  NONE
!!$      INTEGER, DIMENSION(1:), INTENT(INOUT) :: x
!!$      INTEGER, INTENT(IN)                   :: Size
!!$      INTEGER                               :: i
!!$      INTEGER                               :: Location
!!$
!!$      DO i = 1, Size-1			! except for the last
!!$         Location = FindMinimum(x, i, Size)	! find min from this to last
!!$         CALL  Swap(x(i), x(Location))	! swap this and the minimum
!!$      END DO
!!$   END SUBROUTINE  Sort
!!$


!!$il Bubble sort o bubblesort (letteralmente: ordinamento a bolle) è un
!!$semplice algoritmo di ordinamento di dati. Il suo funzionamento è
!!$semplice: ogni coppia di elementi adiacenti della lista viene
!!$comparata e se sono nell'ordine sbagliato vengono invertiti di
!!$posizione. L'algoritmo continua poi a scorrere tutta la lista finché
!!$non vengono più eseguiti scambi, situazione che indica che la lista è
!!$ordinata.
!!$
!!$Il Bubble sort non è un algoritmo efficiente
!!$
!!$SUBROUTINE Bubble_Sort(a)
!!$  REAL, INTENT(in out), DIMENSION(:) :: a
!!$  REAL :: temp
!!$  INTEGER :: i, j
!!$  LOGICAL :: swapped
!!$ 
!!$  DO j = SIZE(a)-1, 1, -1
!!$    swapped = .FALSE.
!!$    DO i = 1, j
!!$      IF (a(i) > a(i+1)) THEN
!!$        temp = a(i)
!!$        a(i) = a(i+1)
!!$        a(i+1) = temp
!!$        swapped = .TRUE.
!!$      END IF
!!$    END DO
!!$    IF (.NOT. swapped) EXIT
!!$  END DO
!!$END SUBROUTINE Bubble_Sort


!!$ lo Shaker sort, noto anche come Bubble sort bidirezionale, Cocktail
!!$ sort, Cocktail shaker sort, Ripple sort, Happy hour sort o Shuttle
!!$ sort è un algoritmo di ordinamento dei dati sviluppato dalla Sun
!!$ Microsystems. Lo shaker sort è sostanzialmente una variante del
!!$ bubble sort: si differenzia da quest'ultimo per l'indice del ciclo
!!$ più interno che, anziché scorrere dall'inizio alla fine, inverte la
!!$ sua direzione ad ogni ciclo. Pur mantenendo la stessa complessità,
!!$ ovvero O(n²), lo shaker sort riduce la probabilità che l'ordinamento
!!$ abbia un costo corrispondente al caso peggiore.
!!$
!!$ 
!!$  SUBROUTINE Cocktail_sort(a)
!!$    INTEGER, INTENT(IN OUT) :: a(:)
!!$    INTEGER :: i, bottom, top, temp 
!!$    LOGICAL :: swapped
!!$ 
!!$    bottom = 1
!!$    top = SIZE(a) - 1
!!$    DO WHILE (bottom < top )
!!$       swapped = .FALSE.
!!$       DO i = bottom, top
!!$          IF (array(i) > array(i+1)) THEN
!!$              temp = array(i)
!!$              array(i) = array(i+1)
!!$              array(i+1) = temp
!!$              swapped = .TRUE.
!!$          END IF
!!$       END DO
!!$       IF (.NOT. swapped) EXIT
!!$       DO i = top, bottom + 1, -1
!!$          IF (array(i) < array(i-1)) THEN
!!$              temp = array(i)
!!$              array(i) = array(i-1)
!!$              array(i-1) = temp
!!$              swapped = .TRUE.
!!$          END IF
!!$       END DO
!!$       IF (.NOT. swapped) EXIT
!!$       bottom = bottom + 1
!!$       top = top - 1
!!$    END DO
!!$  END SUBROUTINE Cocktail_sort

#endif
