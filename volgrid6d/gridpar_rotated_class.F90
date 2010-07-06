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
MODULE gridpar_rotated_class
USE missing_values
IMPLICIT NONE

!> Descrive i parametri per la rotazione di un sistema di coordinate sulla sfera
TYPE gridpar_rotated
  DOUBLE PRECISION :: longitude_south_pole, latitude_south_pole, angle_rotation
END TYPE gridpar_rotated

INTERFACE delete
  MODULE PROCEDURE gridpar_rotated_delete
END INTERFACE

INTERFACE get_val
  MODULE PROCEDURE gridpar_rotated_get_val
END INTERFACE

INTERFACE set_val
  MODULE PROCEDURE gridpar_rotated_set_val
END INTERFACE

INTERFACE copy
  MODULE PROCEDURE gridpar_rotated_copy
END INTERFACE

INTERFACE OPERATOR(==)
  MODULE PROCEDURE gridpar_rotated_eq
END INTERFACE

INTERFACE write_unit
  MODULE PROCEDURE gridpar_rotated_write_unit
END INTERFACE

INTERFACE read_unit
  MODULE PROCEDURE gridpar_rotated_read_unit
END INTERFACE

INTERFACE display
  MODULE PROCEDURE gridpar_rotated_display
END INTERFACE

PRIVATE gridpar_rotated_delete, gridpar_rotated_get_val, gridpar_rotated_set_val, &
 gridpar_rotated_copy, gridpar_rotated_eq, &
 gridpar_rotated_read_unit, gridpar_rotated_write_unit, gridpar_rotated_display

CONTAINS

FUNCTION gridpar_rotated_new(longitude_south_pole, latitude_south_pole, &
 angle_rotation) RESULT(this)
TYPE(gridpar_rotated) :: this
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_south_pole
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_south_pole
DOUBLE PRECISION,INTENT(in),OPTIONAL :: angle_rotation


IF (PRESENT(longitude_south_pole)) THEN
  this%longitude_south_pole = longitude_south_pole
ELSE
  this%longitude_south_pole = dmiss
ENDIF

IF (PRESENT(latitude_south_pole)) THEN
  this%latitude_south_pole = latitude_south_pole
ELSE
  this%latitude_south_pole = dmiss
ENDIF

IF (PRESENT(angle_rotation)) THEN
  this%angle_rotation = angle_rotation
ELSE
  this%angle_rotation = dmiss
ENDIF

END FUNCTION gridpar_rotated_new


SUBROUTINE gridpar_rotated_delete(this)
TYPE(gridpar_rotated), INTENT(inout) :: this


this%longitude_south_pole = dmiss
this%latitude_south_pole = dmiss
this%angle_rotation = dmiss

END SUBROUTINE gridpar_rotated_delete


SUBROUTINE gridpar_rotated_get_val(this, longitude_south_pole, latitude_south_pole, &
 angle_rotation)
TYPE(gridpar_rotated), INTENT(in) :: this
DOUBLE PRECISION,INTENT(out),OPTIONAL :: longitude_south_pole
DOUBLE PRECISION,INTENT(out),OPTIONAL :: latitude_south_pole
DOUBLE PRECISION,INTENT(out),OPTIONAL :: angle_rotation


IF (PRESENT(longitude_south_pole)) THEN
  longitude_south_pole = this%longitude_south_pole
ENDIF
IF (PRESENT(latitude_south_pole)) THEN
  latitude_south_pole = this%latitude_south_pole
ENDIF
IF (PRESENT(angle_rotation)) THEN
  angle_rotation = this%angle_rotation
ENDIF

END SUBROUTINE gridpar_rotated_get_val


SUBROUTINE gridpar_rotated_set_val(this, longitude_south_pole, latitude_south_pole, &
 angle_rotation)
TYPE(gridpar_rotated), INTENT(inout) :: this
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_south_pole
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_south_pole
DOUBLE PRECISION,INTENT(in),OPTIONAL :: angle_rotation


IF (PRESENT(longitude_south_pole)) THEN
  this%longitude_south_pole = longitude_south_pole
ENDIF
IF (PRESENT(latitude_south_pole)) THEN
  this%latitude_south_pole = latitude_south_pole
ENDIF
IF (PRESENT(angle_rotation)) THEN
  this%angle_rotation = angle_rotation
ENDIF

END SUBROUTINE gridpar_rotated_set_val


SUBROUTINE gridpar_rotated_copy(this, that)
TYPE(gridpar_rotated), INTENT(in) :: this
TYPE(gridpar_rotated), INTENT(out) :: that

that = this

END SUBROUTINE gridpar_rotated_copy


ELEMENTAL FUNCTION gridpar_rotated_eq(this, that) RESULT(res)
TYPE(gridpar_rotated), INTENT(in) :: this
TYPE(gridpar_rotated), INTENT(in) :: that

LOGICAL :: res


res = (this%longitude_south_pole == that%longitude_south_pole .AND. &
 this%latitude_south_pole == that%latitude_south_pole .AND. &
 this%angle_rotation == that%angle_rotation)

END FUNCTION gridpar_rotated_eq


SUBROUTINE gridpar_rotated_read_unit(this, unit)
TYPE(gridpar_rotated),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this%longitude_south_pole,this%latitude_south_pole,&
   this%angle_rotation
ELSE
  READ(unit)this%longitude_south_pole,this%latitude_south_pole,&
   this%angle_rotation
ENDIF

END SUBROUTINE gridpar_rotated_read_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE gridpar_rotated_write_unit(this, unit)
TYPE(gridpar_rotated),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this%longitude_south_pole,this%latitude_south_pole,&
   this%angle_rotation
ELSE
  WRITE(unit)this%longitude_south_pole,this%latitude_south_pole,&
   this%angle_rotation
ENDIF

END SUBROUTINE gridpar_rotated_write_unit


!> Display on the screen a brief content of griddim object.
SUBROUTINE gridpar_rotated_display(this) 
TYPE(gridpar_rotated),INTENT(in) :: this !< gridpar_rotated object to display

IF (c_e(this%longitude_south_pole) .OR. c_e(this%latitude_south_pole)) THEN
  PRINT*,"Rotated projection:"
  PRINT*,"lonSouthPole",this%longitude_south_pole
  PRINT*,"latSouthPole",this%latitude_south_pole
ENDIF

END SUBROUTINE gridpar_rotated_display

END MODULE gridpar_rotated_class

