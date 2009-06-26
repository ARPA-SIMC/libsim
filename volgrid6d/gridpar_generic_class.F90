#include "config.h"
MODULE gridpar_generic_class
USE missing_values
IMPLICIT NONE

!> Tipo derivato che descrive una griglia generica rettangolare.
TYPE gridpar_generic
!> coordinate dell'angolo in basso a sinistra della griglia, in proiezione (gradi o m)
  DOUBLE PRECISION :: x1, y1
!> coordinate dell'angolo in alto a destra della griglia, in proiezione (gradi o m)
  DOUBLE PRECISION :: x2, y2
!> passo di griglia nelle 2 direzioni in proiezione (gradi o m)
  DOUBLE PRECISION :: dx, dy
!> Resolution and Component Flags
!! -  bit 1	
!!            -  0	i direction increments not given
!!            -  1	i direction increments given
!! -  bit 2	
!!            -  0	j direction increments not given
!!            -  1	j direction increments given
!! -  bit 3	
!!            -  0 	Resolved u- and v- components of vector quantities relative to easterly and northerly directions
!!            -  1 	Resolved u- and v- components of vector quantities relative to the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
  INTEGER :: component_flag
END TYPE gridpar_generic

INTERFACE delete
  MODULE PROCEDURE gridpar_generic_delete
END INTERFACE

INTERFACE get_val
  MODULE PROCEDURE gridpar_generic_get_val
END INTERFACE

INTERFACE set_val
  MODULE PROCEDURE gridpar_generic_set_val
END INTERFACE

INTERFACE copy
  MODULE PROCEDURE gridpar_generic_copy
END INTERFACE

INTERFACE OPERATOR(==)
  MODULE PROCEDURE gridpar_generic_eq
END INTERFACE

INTERFACE write_unit
  MODULE PROCEDURE gridpar_generic_write_unit
END INTERFACE

INTERFACE read_unit
  MODULE PROCEDURE gridpar_generic_read_unit
END INTERFACE

INTERFACE display
  MODULE PROCEDURE gridpar_generic_display
END INTERFACE

PRIVATE gridpar_generic_delete, gridpar_generic_get_val, gridpar_generic_set_val, &
 gridpar_generic_copy, gridpar_generic_eq, &
 gridpar_generic_read_unit, gridpar_generic_write_unit, gridpar_generic_display

CONTAINS

FUNCTION gridpar_generic_init(x1, x2, y1, y2, &
 dx, dy, component_flag) RESULT(this)
!> longitudini e latitudini minime e massime
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: x1, x2, y1, y2
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: dx !< passo di griglia in x
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: dy !< passo di griglia in y
!> Resolution and Component Flags
!! -  bit 1	
!!            -  0	i direction increments not given
!!            -  1	i direction increments given
!! -  bit 2	
!!            -  0	j direction increments not given
!!            -  1	j direction increments given
!! -  bit 3	
!!            -  0 	Resolved u- and v- components of vector quantities relative to easterly and northerly directions
!!            -  1 	Resolved u- and v- components of vector quantities relative to the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,OPTIONAL,INTENT(in) :: component_flag

TYPE(gridpar_generic) :: this


IF (PRESENT(x1)) THEN
  this%x1 = x1
ELSE
  this%x1 = dmiss
ENDIF

IF (PRESENT(y1)) THEN
  this%y1 = y1
ELSE
  this%y1 = dmiss
ENDIF

IF (PRESENT(x2)) THEN
  this%x2 = x2
ELSE
  this%x2 = dmiss
ENDIF

IF (PRESENT(y2)) THEN
  this%y2 = y2
ELSE
  this%y2 = dmiss
ENDIF

IF (PRESENT(dx)) THEN
  this%dx = dx
ELSE
  this%dx = dmiss
ENDIF

IF (PRESENT(dy)) THEN
  this%dy = dy
ELSE
  this%dy = dmiss
ENDIF

IF (PRESENT(component_flag)) THEN
  this%component_flag = component_flag
ELSE
  this%component_flag = imiss
ENDIF

END FUNCTION gridpar_generic_init


SUBROUTINE gridpar_generic_delete(this)
TYPE(gridpar_generic), INTENT(inout) :: this


this%x1 = dmiss
this%y1 = dmiss
this%x2 = dmiss
this%y2 = dmiss
this%dx = dmiss
this%dy = dmiss
this%component_flag = imiss

END SUBROUTINE gridpar_generic_delete


SUBROUTINE gridpar_generic_get_val(this, x1, x2, y1, y2, &
 dx, dy, component_flag)
TYPE(gridpar_generic), INTENT(in) :: this
!> longitudini e latitudini minime e massime
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: x1, x2, y1, y2
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: dx !< passo di griglia in x
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: dy !< passo di griglia in y
!> Resolution and Component Flags
!! -  bit 1	
!!            -  0	i direction increments not given
!!            -  1	i direction increments given
!! -  bit 2	
!!            -  0	j direction increments not given
!!            -  1	j direction increments given
!! -  bit 3	
!!            -  0 	Resolved u- and v- components of vector quantities relative to easterly and northerly directions
!!            -  1 	Resolved u- and v- components of vector quantities relative to the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,OPTIONAL,INTENT(out) :: component_flag


IF (PRESENT(x1)) THEN
  x1 = this%x1
ENDIF
IF (PRESENT(y1)) THEN
  y1 = this%y1
ENDIF
IF (PRESENT(x2)) THEN
  x2 = this%x2
ENDIF
IF (PRESENT(y2)) THEN
  y2 = this%y2
ENDIF
IF (PRESENT(dx)) THEN
  dx = this%dx
ENDIF
IF (PRESENT(dy)) THEN
  dy = this%dy
ENDIF
IF (PRESENT(component_flag)) THEN
  component_flag = this%component_flag
ENDIF

END SUBROUTINE gridpar_generic_get_val


SUBROUTINE gridpar_generic_set_val(this, x1, x2, y1, y2, &
 dx, dy, component_flag)
TYPE(gridpar_generic), INTENT(inout) :: this
!> longitudini e latitudini minime e massime
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: x1, x2, y1, y2
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: dx !< passo di griglia in x
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: dy !< passo di griglia in y
!> Resolution and Component Flags
!! -  bit 1	
!!            -  0	i direction increments not given
!!            -  1	i direction increments given
!! -  bit 2	
!!            -  0	j direction increments not given
!!            -  1	j direction increments given
!! -  bit 3	
!!            -  0 	Resolved u- and v- components of vector quantities relative to easterly and northerly directions
!!            -  1 	Resolved u- and v- components of vector quantities relative to the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,OPTIONAL,INTENT(in) :: component_flag


IF (PRESENT(x1)) THEN
  this%x1 = x1
ENDIF
IF (PRESENT(y1)) THEN
  this%y1 = y1
ENDIF
IF (PRESENT(x2)) THEN
  this%x2 = x2
ENDIF
IF (PRESENT(y2)) THEN
  this%y2 = y2
ENDIF
IF (PRESENT(dx)) THEN
  this%dx = dx
ENDIF
IF (PRESENT(dy)) THEN
  this%dy = dy
ENDIF
IF (PRESENT(component_flag)) THEN
  this%component_flag = component_flag
ENDIF

END SUBROUTINE gridpar_generic_set_val


SUBROUTINE gridpar_generic_copy(this, that)
TYPE(gridpar_generic), INTENT(in) :: this
TYPE(gridpar_generic), INTENT(out) :: that

that = this

END SUBROUTINE gridpar_generic_copy


ELEMENTAL FUNCTION gridpar_generic_eq(this, that) RESULT(res)
TYPE(gridpar_generic), INTENT(in) :: this
TYPE(gridpar_generic), INTENT(in) :: that

LOGICAL :: res


res = (this%x1 == that%x1 .AND. this%x2 == that%x2 .AND. &
 this%y1 == that%y1 .AND. this%y2 == that%y2 .AND. &
 this%dx == that%dx .AND. this%dy == that%dy .AND. &
 this%component_flag == that%component_flag)

END FUNCTION gridpar_generic_eq


SUBROUTINE gridpar_generic_read_unit(this, unit) 
TYPE(gridpar_generic),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this%x1,this%y1,this%x2,this%y2,this%dx,this%dy,this%component_flag
ELSE
  READ(unit)this%x1,this%y1,this%x2,this%y2,this%dx,this%dy,this%component_flag
ENDIF

END SUBROUTINE gridpar_generic_read_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE gridpar_generic_write_unit(this, unit)
TYPE(gridpar_generic),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this%x1,this%y1,this%x2,this%y2,this%dx,this%dy,this%component_flag
ELSE
  WRITE(unit)this%x1,this%y1,this%x2,this%y2,this%dx,this%dy,this%component_flag
ENDIF

END SUBROUTINE gridpar_generic_write_unit


!> Display on the screen a brief content of griddim object.
SUBROUTINE gridpar_generic_display(this) 
TYPE(gridpar_generic),INTENT(in) :: this !< gridpar_generic object to display

PRINT*,"xFirst",this%x1
PRINT*,"xLast ",this%x2
PRINT*,"yFirst",this%y1
PRINT*,"yLast ",this%y2
PRINT*,"dx, dy",this%dx,this%dy

END SUBROUTINE gridpar_generic_display


!> Generates coordinates of every point of a generic grid from the
!! grid description. The number of grid points along both direction is
!! guessed from the shape of x and y arrays, which must be conformal.
SUBROUTINE gridpar_coordinates(this, x, y)
TYPE(gridpar_generic),INTENT(in) :: this !< generic grid descriptor
DOUBLE PRECISION,INTENT(out) :: x(:,:) !< x coordinate of every point, linearly computed between grid extremes
DOUBLE PRECISION,INTENT(out) :: y(:,:) !< y coordinate of every point, linearly computed between grid extremes, it should have the same shape as x(:,:)

DOUBLE PRECISION :: dx, dy
INTEGER :: nx, ny, i, j

nx = SIZE(x,1)
ny = SIZE(x,2)

#ifdef DEBUG
IF (SIZE(y,1) /= nx .OR. SIZE(y,2) /= ny) THEN
  x(:,:) = dmiss
  y(:,:) = dmiss
  RETURN
ENDIF
#endif

CALL gridpar_steps(this, nx, ny, dx, dy)

x(:,:) = RESHAPE((/ ((this%x1+(dx*DBLE(i)), i=0,nx-1), j=0,ny-1) /),&
 (/nx,ny/))
y(:,:) = RESHAPE((/ ((this%y1+(dy*DBLE(j)), i=0,nx-1), j=0,ny-1) /),&
 (/nx,ny/))

END SUBROUTINE gridpar_coordinates


!> Compute grid steps
SUBROUTINE gridpar_steps(this, nx, ny, dx, dy)
TYPE(gridpar_generic), INTENT(in) :: this !< generic grid descriptor
INTEGER,INTENT(in) :: nx !< number of points along x direction
INTEGER,INTENT(in) :: ny !< number of points along y direction
DOUBLE PRECISION,INTENT(out) :: dx !< grid step along x direction
DOUBLE PRECISION,INTENT(out) :: dy !< grid step along y direction


dx = (this%x2 - this%x1)/DBLE(nx - 1)
dy = (this%y2 - this%y1)/DBLE(ny - 1)

END SUBROUTINE gridpar_steps


!> Compute and set grid steps
SUBROUTINE gridpar_setsteps(this, nx, ny)
TYPE(gridpar_generic), INTENT(inout) :: this !< generic grid descriptor
INTEGER,INTENT(in) :: nx !< number of points along x direction
INTEGER,INTENT(in) :: ny !< number of points along y direction

CALL gridpar_steps(this, nx, ny, this%dx, this%dy)

END SUBROUTINE gridpar_setsteps

END MODULE gridpar_generic_class

