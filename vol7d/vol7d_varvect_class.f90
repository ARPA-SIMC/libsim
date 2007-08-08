MODULE vol7d_varvect_class
USE kinds
USE missing_values
USE vol7d_var_class
IMPLICIT NONE

TYPE vol7d_varvect
  TYPE(vol7d_var),POINTER :: r(:), d(:), i(:), b(:), c(:)
END TYPE  vol7d_varvect

INTERFACE init
  MODULE PROCEDURE vol7d_varvect_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE vol7d_varvect_delete
END INTERFACE

CONTAINS

SUBROUTINE vol7d_varvect_init(this)
TYPE(vol7d_varvect),INTENT(INOUT) :: this

NULLIFY(this%r, this%d, this%i, this%b, this%c)

END SUBROUTINE vol7d_varvect_init


SUBROUTINE vol7d_varvect_delete(this)
TYPE(vol7d_varvect),INTENT(INOUT) :: this

IF (ASSOCIATED(this%r)) DEALLOCATE(this%r)
IF (ASSOCIATED(this%d)) DEALLOCATE(this%d)
IF (ASSOCIATED(this%i)) DEALLOCATE(this%i)
IF (ASSOCIATED(this%b)) DEALLOCATE(this%b)
IF (ASSOCIATED(this%c)) DEALLOCATE(this%c)

END SUBROUTINE vol7d_varvect_delete


SUBROUTINE vol7d_varvect_alloc(this, nvarr, nvard, nvari, nvarb, nvarc, ini)
TYPE(vol7d_varvect),INTENT(INOUT) :: this
INTEGER,INTENT(in),OPTIONAL :: nvarr, nvard, nvari, nvarb, nvarc
LOGICAL,INTENT(in),OPTIONAL :: ini

INTEGER :: i
LOGICAL :: linit

IF (PRESENT(ini)) THEN
  linit = ini
ELSE
  linit = .FALSE.
ENDIF

IF (PRESENT(nvarr)) THEN
  IF (nvarr > 0) THEN
    IF (ASSOCIATED(this%r)) DEALLOCATE(this%r)
    ALLOCATE(this%r(nvarr))
    IF (linit) THEN
      DO i = 1, nvarr
        CALL init(this%r(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvard)) THEN
  IF (nvard > 0) THEN
    IF (ASSOCIATED(this%d)) DEALLOCATE(this%d)
    ALLOCATE(this%d(nvard))
    IF (linit) THEN
      DO i = 1, nvard
        CALL init(this%d(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvari)) THEN
  IF (nvari > 0) THEN
    IF (ASSOCIATED(this%i)) DEALLOCATE(this%i)
    ALLOCATE(this%i(nvari))
    IF (linit) THEN
      DO i = 1, nvari
        CALL init(this%i(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvarb)) THEN
  IF (nvarb > 0) THEN
    IF (ASSOCIATED(this%b)) DEALLOCATE(this%b)
    ALLOCATE(this%b(nvarb))
    IF (linit) THEN
      DO i = 1, nvarb
        CALL init(this%b(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvarc)) THEN
  IF (nvarc > 0) THEN
    IF (ASSOCIATED(this%c)) DEALLOCATE(this%c)
    ALLOCATE(this%c(nvarc))
    IF (linit) THEN
      DO i = 1, nvarc
        CALL init(this%c(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF

END SUBROUTINE vol7d_varvect_alloc

END MODULE vol7d_varvect_class
