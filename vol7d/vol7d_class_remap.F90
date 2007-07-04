SUBROUTINE vol7d_remap_/**/VOL7D_POLY_TYPE(varin1, varin2, varout, sort, remap1, remap2)
TYPE(/**/VOL7D_POLY_TYPE),POINTER :: varin1(:), varin2(:), varout(:)
LOGICAL,INTENT(in) :: sort
INTEGER,POINTER :: remap1(:), remap2(:)

INTEGER :: i, n

IF (.NOT.ASSOCIATED(varin1) .AND. .NOT.ASSOCIATED(varin2)) THEN
  NULLIFY(remap1, remap2, varout)
  RETURN
ENDIF
! Complete allocations
IF (.NOT.ASSOCIATED(varin1)) ALLOCATE(varin1(0))
IF (.NOT.ASSOCIATED(varin2)) ALLOCATE(varin2(0))
ALLOCATE(remap1(SIZE(varin1)), remap2(SIZE(varin2)))

! Count different elements
n = SIZE(varin1)
DO i = 1, SIZE(varin2)
  IF (ALL(varin1 /= varin2(i))) THEN ! ALL(zero-sized) = .TRUE.
    n = n + 1
  ENDIF
ENDDO
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

END SUBROUTINE vol7d_remap_/**/VOL7D_POLY_TYPE
