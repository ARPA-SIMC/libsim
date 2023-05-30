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
#include "config.h"
MODULE grid_rect_class
USE missing_values
USE optional_values
IMPLICIT NONE

TYPE grid_rect
  DOUBLE PRECISION :: xmin
  DOUBLE PRECISION :: xmax
  DOUBLE PRECISION :: ymin
  DOUBLE PRECISION :: ymax
  DOUBLE PRECISION :: dx
  DOUBLE PRECISION :: dy
  INTEGER :: component_flag
END TYPE grid_rect

INTERFACE delete
  MODULE PROCEDURE grid_rect_delete
END INTERFACE

INTERFACE get_val
  MODULE PROCEDURE grid_rect_get_val
END INTERFACE

INTERFACE set_val
  MODULE PROCEDURE grid_rect_set_val
END INTERFACE

INTERFACE copy
  MODULE PROCEDURE grid_rect_copy
END INTERFACE

INTERFACE OPERATOR(==)
  MODULE PROCEDURE grid_rect_eq
END INTERFACE

INTERFACE write_unit
  MODULE PROCEDURE grid_rect_write_unit
END INTERFACE

INTERFACE read_unit
  MODULE PROCEDURE grid_rect_read_unit
END INTERFACE

INTERFACE display
  MODULE PROCEDURE grid_rect_display
END INTERFACE


PRIVATE grid_rect_delete, grid_rect_get_val, &
 grid_rect_set_val, grid_rect_copy, grid_rect_eq, &
 grid_rect_read_unit, grid_rect_write_unit, grid_rect_display

CONTAINS

FUNCTION grid_rect_new(xmin, xmax, ymin, ymax, dx, dy, component_flag) RESULT(this)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xmin, xmax, ymin, ymax !< grid extremes in projection units (degrees or meters depending on the projection type)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: dx, dy !< grid steps in x and y directions
!> Resolved u- and v- components of vector quantities relative to 0=the easterly and northerly directions
!! 1=the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,INTENT(in),OPTIONAL :: component_flag

TYPE(grid_rect) :: this

this%xmin = optio_d(xmin)
this%ymin = optio_d(ymin)
this%xmax = optio_d(xmax)
this%ymax = optio_d(ymax)
this%dx = optio_d(dx)
this%dy = optio_d(dy)
this%component_flag = optio_l(component_flag)

END FUNCTION grid_rect_new


SUBROUTINE grid_rect_delete(this)
TYPE(grid_rect),INTENT(inout) :: this

this%xmin = dmiss
this%ymin = dmiss
this%xmax = dmiss
this%ymax = dmiss
this%dx = dmiss
this%dy = dmiss
this%component_flag = imiss

END SUBROUTINE grid_rect_delete


SUBROUTINE grid_rect_get_val(this, xmin, xmax, ymin, ymax, dx, dy, component_flag)
TYPE(grid_rect), INTENT(in) :: this !< object to be queried
DOUBLE PRECISION,INTENT(out),OPTIONAL :: xmin, xmax, ymin, ymax !< grid extremes in projection units (degrees or meters depending on the projection type)
DOUBLE PRECISION,INTENT(out),OPTIONAL :: dx, dy !< grid steps in x and y directions
!> Resolved u- and v- components of vector quantities relative to 0=the easterly and northerly directions
!! 1=the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,INTENT(out),OPTIONAL :: component_flag

IF (PRESENT(xmin)) THEN
  xmin = this%xmin
ENDIF
IF (PRESENT(ymin)) THEN
  ymin = this%ymin
ENDIF
IF (PRESENT(xmax)) THEN
  xmax = this%xmax
ENDIF
IF (PRESENT(ymax)) THEN
  ymax = this%ymax
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

END SUBROUTINE grid_rect_get_val


SUBROUTINE grid_rect_set_val(this, xmin, xmax, ymin, ymax, &
 dx, dy, component_flag)
TYPE(grid_rect), INTENT(inout) :: this !< object to be modified
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xmin, xmax, ymin, ymax !< grid extremes in projection units (degrees or meters depending on the projection type)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: dx, dy !< grid steps in x and y directions
!> Resolved u- and v- components of vector quantities relative to 0=the easterly and northerly directions
!! 1=the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,INTENT(in),OPTIONAL :: component_flag


IF (PRESENT(xmin)) THEN
  this%xmin = xmin
ENDIF
IF (PRESENT(ymin)) THEN
  this%ymin = ymin
ENDIF
IF (PRESENT(xmax)) THEN
  this%xmax = xmax
ENDIF
IF (PRESENT(ymax)) THEN
  this%ymax = ymax
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

END SUBROUTINE grid_rect_set_val


SUBROUTINE grid_rect_copy(this, that)
TYPE(grid_rect), INTENT(in) :: this
TYPE(grid_rect), INTENT(out) :: that

that = this

END SUBROUTINE grid_rect_copy


ELEMENTAL FUNCTION grid_rect_eq(this, that) RESULT(res)
TYPE(grid_rect), INTENT(in) :: this
TYPE(grid_rect), INTENT(in) :: that

LOGICAL :: res


res = (this%xmin == that%xmin .AND. this%xmax == that%xmax .AND. &
 this%ymin == that%ymin .AND. this%ymax == that%ymax .AND. &
 this%dx == that%dx .AND. this%dy == that%dy .AND. &
 this%component_flag == that%component_flag)

END FUNCTION grid_rect_eq


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE grid_rect_read_unit(this, unit) 
TYPE(grid_rect),INTENT(out) :: this !< oobject to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this%xmin,this%ymin,this%xmax,this%ymax,this%dx,this%dy,this%component_flag
ELSE
  READ(unit)this%xmin,this%ymin,this%xmax,this%ymax,this%dx,this%dy,this%component_flag
ENDIF

END SUBROUTINE grid_rect_read_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE grid_rect_write_unit(this, unit)
TYPE(grid_rect),INTENT(in) :: this !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this%xmin,this%ymin,this%xmax,this%ymax,this%dx,this%dy,this%component_flag
ELSE
  WRITE(unit)this%xmin,this%ymin,this%xmax,this%ymax,this%dx,this%dy,this%component_flag
ENDIF

END SUBROUTINE grid_rect_write_unit


!> Display on the screen a brief content of the object.
SUBROUTINE grid_rect_display(this) 
TYPE(grid_rect),INTENT(in) :: this !< grid_rect object to display

PRINT*,"xFirst",this%xmin
PRINT*,"xLast ",this%xmax
PRINT*,"yFirst",this%ymin
PRINT*,"yLast ",this%ymax
PRINT*,"dx, dy",this%dx,this%dy
PRINT*,"componentFlag",this%component_flag

END SUBROUTINE grid_rect_display


!> Generates coordinates of every point of a generic grid from the
!! grid description. The number of grid points along both direction is
!! guessed from the shape of x and y arrays, which must be conformal.
SUBROUTINE grid_rect_coordinates(this, x, y)
TYPE(grid_rect),INTENT(in) :: this !< rectangular grid descriptor
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

CALL grid_rect_steps(this, nx, ny, dx, dy)
IF (c_e(dx) .AND. c_e(dy)) THEN
  x(:,:) = RESHAPE((/ ((this%xmin+(dx*DBLE(i)), i=0,nx-1), j=0,ny-1) /),&
   (/nx,ny/))
  y(:,:) = RESHAPE((/ ((this%ymin+(dy*DBLE(j)), i=0,nx-1), j=0,ny-1) /),&
   (/nx,ny/))
ELSE
  x(:,:) = dmiss
  y(:,:) = dmiss
ENDIF

END SUBROUTINE grid_rect_coordinates


!> Compute and return grid steps.
SUBROUTINE grid_rect_steps(this, nx, ny, dx, dy)
TYPE(grid_rect), INTENT(in) :: this !< rectangular grid descriptor
INTEGER,INTENT(in) :: nx !< number of points along x direction
INTEGER,INTENT(in) :: ny !< number of points along y direction
DOUBLE PRECISION,INTENT(out) :: dx !< grid step along x direction
DOUBLE PRECISION,INTENT(out) :: dy !< grid step along y direction

IF (c_e(nx) .AND. c_e(this%xmax) .AND. c_e(this%xmin) .AND. &
 c_e(nx) .AND. nx > 1) THEN
  dx = (this%xmax - this%xmin)/DBLE(nx - 1)
ELSE
  dx = dmiss
ENDIF
IF (c_e(ny) .AND. c_e(this%ymax) .AND. c_e(this%ymin) .AND. &
 c_e(ny) .AND. ny > 1) THEN
  dy = (this%ymax - this%ymin)/DBLE(ny - 1)
ELSE
  dy = dmiss
ENDIF

END SUBROUTINE grid_rect_steps


!> Compute and set grid steps.
SUBROUTINE grid_rect_setsteps(this, nx, ny)
TYPE(grid_rect), INTENT(inout) :: this !< generic grid descriptor
INTEGER,INTENT(in) :: nx !< number of points along x direction
INTEGER,INTENT(in) :: ny !< number of points along y direction

CALL grid_rect_steps(this, nx, ny, this%dx, this%dy)

END SUBROUTINE grid_rect_setsteps

END MODULE grid_rect_class

