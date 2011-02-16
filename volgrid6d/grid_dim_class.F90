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
!> Module for defining the extension and coordinates of a rectangular
!! georeferenced grid.
!!
!!\ingroup volgrid6d
MODULE grid_dim_class
USE missing_values
USE char_utilities
USE optional_values
USE err_handling
IMPLICIT NONE

!> Derived type describing the extension of a grid and the geographical
!! coordinates of each point. It is not used alone but rather as a
!! subtype of a \a griddim_def type.
TYPE grid_dim
  INTEGER :: nx !< number of points along x dimension
  INTEGER :: ny !< number of points along y dimension
  DOUBLE PRECISION,POINTER :: lat(:,:) !< array of geographical latitudes
  DOUBLE PRECISION,POINTER :: lon(:,:) !< array of geographical longitudes
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

FUNCTION grid_dim_new(nx, ny) RESULT(this)
INTEGER, INTENT(in), OPTIONAL :: nx, ny

TYPE(grid_dim) :: this

this%nx = optio_l(nx)
this%ny = optio_l(ny)
NULLIFY(this%lat, this%lon)

END FUNCTION grid_dim_new


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

that = grid_dim_new(this%nx, this%ny)

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


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE grid_dim_read_unit(this, unit) 
TYPE(grid_dim),INTENT(out) :: this !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit

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


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE grid_dim_write_unit(this, unit)
TYPE(grid_dim),INTENT(in) :: this !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit

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


!> Display on the screen a brief content of the object.
SUBROUTINE grid_dim_display(this) 
TYPE(grid_dim),INTENT(in) :: this !< object to display

PRINT*,'Number of points along x direction',this%nx
PRINT*,'Number of points along y direction',this%ny

END SUBROUTINE grid_dim_display

END MODULE grid_dim_class
