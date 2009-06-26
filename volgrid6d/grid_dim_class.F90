#include "config.h"
MODULE grid_dim_class
USE missing_values
USE char_utilities
USE optional_values
USE err_handling
IMPLICIT NONE

!> Dimensioni del grigliato lat lon con eventuali vettori di coordinate
TYPE grid_dim
  INTEGER :: nx, ny
  DOUBLE PRECISION, POINTER :: lat(:,:), lon(:,:)
END TYPE grid_dim

INTERFACE delete
  MODULE PROCEDURE grid_dim_delete
END INTERFACE

INTERFACE copy
  MODULE PROCEDURE grid_dim_copy
END INTERFACE

INTERFACE alloc
  MODULE PROCEDURE grid_dim_alloc
END INTERFACE

INTERFACE dealloc
  MODULE PROCEDURE grid_dim_dealloc
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE grid_dim_eq
END INTERFACE

INTERFACE write_unit
  MODULE PROCEDURE grid_dim_write_unit
END INTERFACE

INTERFACE read_unit
  MODULE PROCEDURE grid_dim_read_unit
END INTERFACE

INTERFACE display
  MODULE PROCEDURE grid_dim_display
END INTERFACE

PRIVATE grid_dim_delete, grid_dim_copy, grid_dim_alloc, grid_dim_dealloc, &
 grid_dim_eq, grid_dim_read_unit, grid_dim_write_unit, grid_dim_display

CONTAINS

FUNCTION grid_dim_init(nx, ny) RESULT(this)
INTEGER, INTENT(in), OPTIONAL :: nx, ny

TYPE(grid_dim) :: this

this%nx = optio_l(nx)
this%ny = optio_l(ny)
NULLIFY(this%lat, this%lon)

END FUNCTION grid_dim_init


SUBROUTINE grid_dim_delete(this)
TYPE(grid_dim), INTENT(inout) :: this

CALL dealloc(this)
this%nx = imiss
this%ny = imiss

END SUBROUTINE grid_dim_delete


SUBROUTINE grid_dim_alloc(this)
TYPE(grid_dim),INTENT(inout) :: this

IF (ASSOCIATED(this%lon) .AND. ASSOCIATED(this%lat)) THEN
  IF (SIZE(this%lon, 1) == this%nx .AND. SIZE(this%lon, 2) == this%ny .AND. &
   SIZE(this%lat, 1) == this%nx .AND. SIZE(this%lat, 2) == this%ny) RETURN
ENDIF
CALL dealloc(this)
IF (c_e(this%nx) .AND. c_e(this%ny)) THEN
  ALLOCATE(this%lon(this%nx, this%ny), this%lat(this%nx, this%ny))
ENDIF

END SUBROUTINE grid_dim_alloc


SUBROUTINE grid_dim_dealloc(this)
TYPE(grid_dim),INTENT(inout) :: this

IF (ASSOCIATED(this%lon)) DEALLOCATE(this%lon)
IF (ASSOCIATED(this%lat)) DEALLOCATE(this%lat)

END SUBROUTINE grid_dim_dealloc


SUBROUTINE grid_dim_copy(this, that)
TYPE(grid_dim),INTENT(in) :: this
TYPE(grid_dim),INTENT(out) :: that

that = grid_dim_init(this%nx, this%ny)

IF (ASSOCIATED(this%lon) .AND. ASSOCIATED(this%lat))THEN
  CALL alloc(that)

#ifdef DEBUG
  IF (SIZE(this%lon,1) /= this%nx .OR. SIZE(this%lon,2) /= this%ny) THEN
    CALL raise_error('grid_dim_copy, dimensioni non valide: '// &
     TRIM(to_char(SIZE(this%lon,1)))//' '//TRIM(to_char(this%nx))// &
     TRIM(to_char(SIZE(this%lon,2)))//' '//TRIM(to_char(this%ny)))
  ENDIF
  IF (SIZE(this%lat,1) /= this%nx .OR. SIZE(this%lat,2) /= this%ny) THEN
    CALL raise_error('grid_dim_copy, dimensioni non valide: '// &
     TRIM(to_char(SIZE(this%lat,1)))//' '//TRIM(to_char(this%nx))// &
     TRIM(to_char(SIZE(this%lat,2)))//' '//TRIM(to_char(this%ny)))
  ENDIF
#endif

  that%lon(:,:) = this%lon(:,:)
  that%lat(:,:) = this%lat(:,:)
ENDIF

END SUBROUTINE grid_dim_copy


ELEMENTAL FUNCTION grid_dim_eq(this, that) RESULT(res)
TYPE(grid_dim),INTENT(IN) :: this, that
LOGICAL :: res

res = this%nx == that%nx .and. &
 this%ny == that%ny

END FUNCTION grid_dim_eq


SUBROUTINE grid_dim_read_unit(this, unit) 
TYPE(grid_dim),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form
LOGICAL :: is_all

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this%nx,this%ny
  READ(unit,*)is_all
  IF (is_all) THEN
    CALL alloc(this)
    READ(unit,*)this%lon,this%lat
  ELSE
    READ(unit,*)
  ENDIF
ELSE
  READ(unit)this%nx,this%ny
  READ(unit)is_all
  IF (is_all) THEN
    CALL alloc(this)
    READ(unit)this%lon,this%lat
  ELSE
    READ(unit)
  ENDIF
ENDIF

END SUBROUTINE grid_dim_read_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE grid_dim_write_unit(this, unit)
TYPE(grid_dim),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form
LOGICAL :: is_all

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this%nx,this%ny
  is_all = (ASSOCIATED(this%lon) .AND. ASSOCIATED(this%lat))
  WRITE(unit,*)is_all
  IF (is_all) THEN
    WRITE(unit,*)this%lon,this%lat
  ELSE
    WRITE(unit,*)
  ENDIF
ELSE
  WRITE(unit)this%nx,this%ny
  is_all = (ASSOCIATED(this%lon) .AND. ASSOCIATED(this%lat))
  WRITE(unit)is_all
  IF (is_all) THEN
    WRITE(unit)this%lon,this%lat
  ELSE
    WRITE(unit)
  ENDIF
ENDIF

END SUBROUTINE grid_dim_write_unit


!> Display on the screen a brief content of griddim object.
SUBROUTINE grid_dim_display(this) 
TYPE(grid_dim),INTENT(in) :: this !< grid_dim object to display

PRINT*,'Number of points along x direction',this%nx
PRINT*,'Number of points along y direction',this%ny

END SUBROUTINE grid_dim_display

END MODULE grid_dim_class
