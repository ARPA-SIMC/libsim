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
MODULE gridpar_stretched_class
USE missing_values
IMPLICIT NONE

!> Descrive i parametri per la rotazione di un sistema di coordinate sulla sfera
TYPE gridpar_stretched
  DOUBLE PRECISION :: latitude_stretch_pole, longitude_stretch_pole, stretch_factor
END TYPE gridpar_stretched

INTERFACE delete
  MODULE PROCEDURE gridpar_stretched_delete
END INTERFACE

INTERFACE get_val
  MODULE PROCEDURE gridpar_stretched_get_val
END INTERFACE

INTERFACE set_val
  MODULE PROCEDURE gridpar_stretched_set_val
END INTERFACE

INTERFACE copy
  MODULE PROCEDURE gridpar_stretched_copy
END INTERFACE

INTERFACE OPERATOR(==)
  MODULE PROCEDURE gridpar_stretched_eq
END INTERFACE

INTERFACE write_unit
  MODULE PROCEDURE gridpar_stretched_write_unit
END INTERFACE

INTERFACE read_unit
  MODULE PROCEDURE gridpar_stretched_read_unit
END INTERFACE

INTERFACE display
  MODULE PROCEDURE gridpar_stretched_display
END INTERFACE

PRIVATE gridpar_stretched_delete, gridpar_stretched_get_val, gridpar_stretched_set_val, &
 gridpar_stretched_copy, gridpar_stretched_eq, &
 gridpar_stretched_read_unit, gridpar_stretched_write_unit, gridpar_stretched_display

CONTAINS

FUNCTION gridpar_stretched_new(longitude_stretch_pole, latitude_stretch_pole, &
 stretch_factor) RESULT(this)
TYPE(gridpar_stretched) :: this
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_stretch_pole
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_stretch_pole
DOUBLE PRECISION,INTENT(in),OPTIONAL :: stretch_factor


IF (PRESENT(longitude_stretch_pole)) THEN
  this%longitude_stretch_pole = longitude_stretch_pole
ELSE
  this%longitude_stretch_pole = dmiss
ENDIF

IF (PRESENT(latitude_stretch_pole)) THEN
  this%latitude_stretch_pole = latitude_stretch_pole
ELSE
  this%latitude_stretch_pole = dmiss
ENDIF

IF (PRESENT(stretch_factor)) THEN
  this%stretch_factor = stretch_factor
ELSE
  this%stretch_factor = dmiss
ENDIF

END FUNCTION gridpar_stretched_new


SUBROUTINE gridpar_stretched_delete(this)
TYPE(gridpar_stretched), INTENT(inout) :: this


this%longitude_stretch_pole = dmiss
this%latitude_stretch_pole = dmiss
this%stretch_factor = dmiss

END SUBROUTINE gridpar_stretched_delete


SUBROUTINE gridpar_stretched_get_val(this, longitude_stretch_pole, latitude_stretch_pole, &
 stretch_factor)
TYPE(gridpar_stretched), INTENT(in) :: this
DOUBLE PRECISION,INTENT(out),OPTIONAL :: longitude_stretch_pole
DOUBLE PRECISION,INTENT(out),OPTIONAL :: latitude_stretch_pole
DOUBLE PRECISION,INTENT(out),OPTIONAL :: stretch_factor


IF (PRESENT(longitude_stretch_pole)) THEN
  longitude_stretch_pole = this%longitude_stretch_pole
ENDIF
IF (PRESENT(latitude_stretch_pole)) THEN
  latitude_stretch_pole = this%latitude_stretch_pole
ENDIF
IF (PRESENT(stretch_factor)) THEN
  stretch_factor = this%stretch_factor
ENDIF

END SUBROUTINE gridpar_stretched_get_val


SUBROUTINE gridpar_stretched_set_val(this, longitude_stretch_pole, latitude_stretch_pole, &
 stretch_factor)
TYPE(gridpar_stretched), INTENT(inout) :: this
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_stretch_pole
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_stretch_pole
DOUBLE PRECISION,INTENT(in),OPTIONAL :: stretch_factor


IF (PRESENT(longitude_stretch_pole)) THEN
  this%longitude_stretch_pole = longitude_stretch_pole
ENDIF
IF (PRESENT(latitude_stretch_pole)) THEN
  this%latitude_stretch_pole = latitude_stretch_pole
ENDIF
IF (PRESENT(stretch_factor)) THEN
  this%stretch_factor = stretch_factor
ENDIF

END SUBROUTINE gridpar_stretched_set_val


SUBROUTINE gridpar_stretched_copy(this, that)
TYPE(gridpar_stretched), INTENT(in) :: this
TYPE(gridpar_stretched), INTENT(out) :: that

that = this

END SUBROUTINE gridpar_stretched_copy


ELEMENTAL FUNCTION gridpar_stretched_eq(this, that) RESULT(res)
TYPE(gridpar_stretched), INTENT(in) :: this
TYPE(gridpar_stretched), INTENT(in) :: that

LOGICAL :: res


res = (this%longitude_stretch_pole == that%longitude_stretch_pole .AND. &
 this%latitude_stretch_pole == that%latitude_stretch_pole .AND. &
 this%stretch_factor == that%stretch_factor)

END FUNCTION gridpar_stretched_eq


SUBROUTINE gridpar_stretched_read_unit(this, unit)
TYPE(gridpar_stretched),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this%longitude_stretch_pole,this%latitude_stretch_pole, &
   this%stretch_factor
ELSE
  READ(unit)this%longitude_stretch_pole,this%latitude_stretch_pole, &
   this%stretch_factor
ENDIF

END SUBROUTINE gridpar_stretched_read_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE gridpar_stretched_write_unit(this, unit)
TYPE(gridpar_stretched),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this%longitude_stretch_pole,this%latitude_stretch_pole, &
   this%stretch_factor
ELSE
  WRITE(unit)this%longitude_stretch_pole,this%latitude_stretch_pole, &
   this%stretch_factor
ENDIF

END SUBROUTINE gridpar_stretched_write_unit


!> Display on the screen a brief content of griddim object.
SUBROUTINE gridpar_stretched_display(this) 
TYPE(gridpar_stretched),INTENT(in) :: this !< gridpar_stretched object to display

IF (c_e(this%longitude_stretch_pole) .OR. c_e(this%latitude_stretch_pole) .OR. &
 c_e(this%stretch_factor)) THEN
  PRINT*,"Stretched projection:"
  PRINT*,"lonStretchPole",this%longitude_stretch_pole
  PRINT*,"latStretchPole",this%latitude_stretch_pole
  PRINT*,"stretchFactor ",this%stretch_factor
ENDIF

END SUBROUTINE gridpar_stretched_display

END MODULE gridpar_stretched_class

