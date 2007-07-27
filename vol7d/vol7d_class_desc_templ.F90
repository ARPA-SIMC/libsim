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


SUBROUTINE vol7d_remap1_/**/VOL7D_POLY_TYPE(varin, varout, sort, miss, remap)
TYPE(/**/VOL7D_POLY_TYPE),POINTER :: varin(:), varout(:)
LOGICAL,INTENT(in) :: sort, miss
INTEGER,POINTER :: remap(:)

INTEGER :: n

NULLIFY(remap, varout)
IF (.NOT.ASSOCIATED(varin)) RETURN

IF (miss) THEN
  n = count_distinct(varin, back=.TRUE., mask=(varin /= VOL7D_POLY_TYPE/**/_miss))
ELSE
  n = count_distinct(varin, back=.TRUE.)
ENDIF
#ifdef VOL7D_NO_ZERO_ALLOC
IF (n == 0) RETURN ! in case of variables do not allocate zero-length arrays
#endif
! Complete allocations
ALLOCATE(remap(n), varout(n))
IF (miss) THEN
  remap = map_inv_distinct(varin, back=.TRUE., mask=(varin /= VOL7D_POLY_TYPE/**/_miss))
  varout = pack_distinct(varin, back=.TRUE., mask=(varin /= VOL7D_POLY_TYPE/**/_miss))
ELSE
  remap = map_distinct(varin, back=.TRUE.)
  varout = pack_distinct(varin, back=.TRUE.)
ENDIF

END SUBROUTINE vol7d_remap1_/**/VOL7D_POLY_TYPE
