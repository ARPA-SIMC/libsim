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
SUBROUTINE vol7d_remap2_/**/VOL7D_POLY_TYPE(varin1, varin2, varout, sort, remap1, remap2)
TYPE(/**/VOL7D_POLY_TYPE),POINTER :: varin1(:), varin2(:), varout(:)
LOGICAL,INTENT(in) :: sort
INTEGER,POINTER :: remap1(:), remap2(:)

INTEGER :: i, n
LOGICAL av1, av2

av1 = .FALSE.; av2 = .FALSE.
IF (.NOT.ASSOCIATED(varin1) .AND. .NOT.ASSOCIATED(varin2)) THEN
  NULLIFY(remap1, remap2, varout)
  RETURN
ENDIF
! Complete allocations
IF (.NOT.ASSOCIATED(varin1)) THEN
  ALLOCATE(varin1(0))
  av1 = .TRUE.
ENDIF
IF (.NOT.ASSOCIATED(varin2)) THEN
  ALLOCATE(varin2(0))
  av2 = .TRUE.
ENDIF
ALLOCATE(remap1(SIZE(varin1)), remap2(SIZE(varin2)))

! Count different elements
n = SIZE(varin1)
DO i = 1, SIZE(varin2)
  IF (ALL(varin1 /= varin2(i))) THEN ! ALL(zero-sized) = .TRUE.
    n = n + 1
  ENDIF
ENDDO
#ifdef VOL7D_NO_ZERO_ALLOC
IF (n == 0) THEN ! in case of variables do not allocate zero-length arrays
  DEALLOCATE(remap1, remap2)
  NULLIFY(varout)
  IF (av1) DEALLOCATE(varin1)
  IF (av2) DEALLOCATE(varin2)
  RETURN
ENDIF
#endif
! Allocate new array
ALLOCATE(varout(n))
! Fill it
n = SIZE(varin1)
varout(1:n) = varin1(:)
DO i = 1, SIZE(varin2)
  IF (ALL(varin1 /= varin2(i))) THEN
    n = n + 1
    varout(n) = varin2(i)
  ENDIF
ENDDO

#ifdef VOL7D_SORT
IF (sort) THEN ! sort
  DO i = 1, SIZE(varin1)
    remap1(i) = COUNT(varin1(i) > varout(:)) + 1
  ENDDO
  DO i = 1, SIZE(varin2)
    remap2(i) = COUNT(varin2(i) > varout(:)) + 1
  ENDDO
  IF (SIZE(varin1) > 0) varout(remap1) = varin1(:)
  IF (SIZE(varin2) > 0) varout(remap2) = varin2(:)
ELSE ! compute simple remapping
#endif
  IF (SIZE(varin1) > 0) remap1(:) = (/(i,i=1,SIZE(varin1))/)
  DO i = 1, SIZE(varin2)
    remap2(i) = firsttrue(varin2(i) == varout(:))
  ENDDO
#ifdef VOL7D_SORT
ENDIF
#endif

END SUBROUTINE vol7d_remap2_/**/VOL7D_POLY_TYPE


SUBROUTINE vol7d_remap1_/**/VOL7D_POLY_TYPE(varin, varout, &
 sort, unique, miss, remap, misslist)
TYPE(/**/VOL7D_POLY_TYPE),POINTER :: varin(:), varout(:), varoutsort(:)
LOGICAL,INTENT(in) :: sort, unique, miss
INTEGER,POINTER :: remap(:)
LOGICAL,INTENT(in),OPTIONAL :: misslist(:)

INTEGER :: i, j, n, r
INTEGER,POINTER :: remaptmp(:)
TYPE(/**/VOL7D_POLY_TYPE) :: v
LOGICAL,ALLOCATABLE :: lmask(:) ! local mask of values to be remapped

NULLIFY(remap, varout)
IF (.NOT.ASSOCIATED(varin)) RETURN

IF (PRESENT(misslist) .OR. miss) THEN
! prepare local mask
  ALLOCATE (lmask(SIZE(varin)))
  lmask = .TRUE.
  IF (PRESENT(misslist)) THEN
    IF(SIZE(misslist) >= SIZE(varin)) THEN ! Use the given mask element by element
      lmask = lmask .AND. misslist(1:SIZE(varin))
    ELSE IF (SIZE(misslist) == 1) THEN ! Use the given mask element for all the elements
      lmask = lmask .AND. misslist(1)
    ENDIF
  ENDIF
  IF (miss) lmask = lmask .AND. (varin /= VOL7D_POLY_TYPE/**/_miss)

  IF (unique) THEN
    n = count_distinct(varin, back=.TRUE., mask=lmask)
  ELSE
    n = COUNT(lmask)
  ENDIF
ELSE
  IF (unique) THEN
    n = count_distinct(varin, back=.TRUE.)
  ELSE
    n = SIZE(varin)
  ENDIF
ENDIF
#ifdef VOL7D_NO_ZERO_ALLOC
IF (n == 0) THEN
  IF (ALLOCATED(lmask)) DEALLOCATE(lmask)
  RETURN ! in case of variables do not allocate zero-length arrays
ENDIF
#endif
! Complete allocations
ALLOCATE(remap(n), varout(n))
IF (ALLOCATED(lmask)) THEN
  IF (unique) THEN
    remap = map_inv_distinct(varin, n, back=.TRUE., mask=lmask)
  ELSE
    remap = PACK((/(i, i=1,SIZE(varin))/), mask=lmask)
  ENDIF
  DEALLOCATE(lmask) ! not used anymore (deallocation useless in f95)
ELSE
  IF (unique) THEN
    remap = map_inv_distinct(varin, n, back=.TRUE.)
  ELSE
    remap = (/(i, i=1,n)/)
  ENDIF
ENDIF

varout(:) = varin(remap)

#ifdef VOL7D_SORT
IF (sort) THEN ! sort with the simplest algorithm both varout and remap
  DO j = 2, n
    v = varout(j)
    r = remap(j)
    DO i = j-1, 1, -1
      IF (v >= varout(i)) EXIT
      varout(i+1) = varout(i)
      remap(i+1) = remap(i)
    ENDDO
    varout(i+1) = v
    remap(i+1) = r
  ENDDO
ENDIF
#endif

END SUBROUTINE vol7d_remap1_/**/VOL7D_POLY_TYPE
