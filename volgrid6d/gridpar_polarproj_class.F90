#include "config.h"
MODULE gridpar_polarproj_class
USE missing_values
USE optional_values
IMPLICIT NONE

!> Tipo derivato che descrive i parametri aggiuntivi per una proiezione polare.
TYPE gridpar_polarproj
!> latitudini a cui il piano di proiezione è secante alla sfera
  DOUBLE PRECISION :: latin1, latin2
  DOUBLE PRECISION :: lov !> line of view, ovvero meridiano parallelo all'asse y del piano di proiezione
  DOUBLE PRECISION :: lad !> latitudine a cui dx e dy (in m) sono specificati
!> conservate per ricordo nel caso grib
  DOUBLE PRECISION :: lon1, lat1
  INTEGER :: projection_center_flag !> 0 = polo sud, 128 = polo nord
END TYPE gridpar_polarproj

INTERFACE delete
  MODULE PROCEDURE gridpar_polarproj_delete
END INTERFACE

INTERFACE get_val
  MODULE PROCEDURE gridpar_polarproj_get_val
END INTERFACE

INTERFACE set_val
  MODULE PROCEDURE gridpar_polarproj_set_val
END INTERFACE

INTERFACE copy
  MODULE PROCEDURE gridpar_polarproj_copy
END INTERFACE

INTERFACE OPERATOR(==)
  MODULE PROCEDURE gridpar_polarproj_eq
END INTERFACE

INTERFACE write_unit
  MODULE PROCEDURE gridpar_polarproj_write_unit
END INTERFACE

INTERFACE read_unit
  MODULE PROCEDURE gridpar_polarproj_read_unit
END INTERFACE

INTERFACE display
  MODULE PROCEDURE gridpar_polarproj_display
END INTERFACE

PRIVATE gridpar_polarproj_delete, gridpar_polarproj_get_val, gridpar_polarproj_set_val, &
 gridpar_polarproj_copy, gridpar_polarproj_eq, &
 gridpar_polarproj_read_unit, gridpar_polarproj_write_unit, gridpar_polarproj_display

CONTAINS

FUNCTION gridpar_polarproj_init(latin1, latin2, lov, lad, lon1, lat1, &
 projection_center_flag) RESULT(this)
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: latin1, latin2
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: lov, lad
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: lon1, lat1
INTEGER,OPTIONAL,INTENT(in) :: projection_center_flag

TYPE(gridpar_polarproj) :: this

  this%latin1 = optio_d(latin1)
  this%latin2 = optio_d(latin2)
  this%lov = optio_d(lov)
  this%lad = optio_d(lad)
  this%lon1 = optio_d(lon1)
  this%lat1 = optio_d(lat1)
  this%projection_center_flag = optio_l(projection_center_flag)

END FUNCTION gridpar_polarproj_init


SUBROUTINE gridpar_polarproj_delete(this)
TYPE(gridpar_polarproj), INTENT(inout) :: this


this%latin1 = dmiss
this%latin2 = dmiss
this%lov = dmiss
this%lad = dmiss
this%lon1 = dmiss
this%lat1 = dmiss
this%projection_center_flag = imiss

END SUBROUTINE gridpar_polarproj_delete


SUBROUTINE gridpar_polarproj_get_val(this, latin1, latin2, lov, lad, lon1, lat1, &
 projection_center_flag)
TYPE(gridpar_polarproj), INTENT(in) :: this
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: latin1, latin2
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: lov, lad
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: lon1, lat1
INTEGER,OPTIONAL,INTENT(out) :: projection_center_flag


IF (PRESENT(latin1)) THEN
  latin1 = this%latin1
ENDIF
IF (PRESENT(latin2)) THEN
  latin2 = this%latin2
ENDIF
IF (PRESENT(lov)) THEN
  lov = this%lov
ENDIF
IF (PRESENT(lad)) THEN
  lad = this%lad
ENDIF
IF (PRESENT(lon1)) THEN
  lon1 = this%lon1
ENDIF
IF (PRESENT(lat1)) THEN
  lat1 = this%lat1
ENDIF
IF (PRESENT(projection_center_flag)) THEN
  projection_center_flag = this%projection_center_flag
ENDIF

END SUBROUTINE gridpar_polarproj_get_val


SUBROUTINE gridpar_polarproj_set_val(this, latin1, latin2, lov, lad, lon1, lat1, &
 projection_center_flag)
TYPE(gridpar_polarproj), INTENT(inout) :: this
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: latin1, latin2
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: lov, lad
DOUBLE PRECISION,OPTIONAL,INTENT(in) :: lon1, lat1
INTEGER,OPTIONAL,INTENT(in) :: projection_center_flag


IF (PRESENT(latin1)) THEN
  this%latin1 = latin1
ENDIF
IF (PRESENT(latin2)) THEN
  this%latin2 = latin2
ENDIF
IF (PRESENT(lov)) THEN
  this%lov = lov
ENDIF
IF (PRESENT(lad)) THEN
  this%lad = lad
ENDIF
IF (PRESENT(lon1)) THEN
  this%lon1 = lon1
ENDIF
IF (PRESENT(lat1)) THEN
  this%lat1 = lat1
ENDIF
IF (PRESENT(projection_center_flag)) THEN
  this%projection_center_flag = projection_center_flag
ENDIF

END SUBROUTINE gridpar_polarproj_set_val


SUBROUTINE gridpar_polarproj_copy(this, that)
TYPE(gridpar_polarproj), INTENT(in) :: this
TYPE(gridpar_polarproj), INTENT(out) :: that

that = this

END SUBROUTINE gridpar_polarproj_copy


ELEMENTAL FUNCTION gridpar_polarproj_eq(this, that) RESULT(res)
TYPE(gridpar_polarproj), INTENT(in) :: this
TYPE(gridpar_polarproj), INTENT(in) :: that

LOGICAL :: res


! lon1 e lat1 volutamente omessi
res = (this%latin1 == that%latin1 .AND. this%latin2 == that%latin2 .AND. &
 this%lov == that%lov .AND. this%lad == that%lad .AND. &
 this%projection_center_flag == that%projection_center_flag)

END FUNCTION gridpar_polarproj_eq


SUBROUTINE gridpar_polarproj_read_unit(this, unit)
TYPE(gridpar_polarproj),INTENT(out) :: this !< oggetto da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this%latin1,this%latin2,this%lov,this%lad,this%lon1,this%lat1, &
   this%projection_center_flag
ELSE
  READ(unit)this%latin1,this%latin2,this%lov,this%lad,this%lon1,this%lat1, &
   this%projection_center_flag
ENDIF

END SUBROUTINE gridpar_polarproj_read_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE gridpar_polarproj_write_unit(this, unit)
TYPE(gridpar_polarproj),INTENT(in) :: this !< oggetto da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form

INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this%latin1,this%latin2,this%lov,this%lad,this%lon1,this%lat1, &
   this%projection_center_flag
ELSE
  WRITE(unit)this%latin1,this%latin2,this%lov,this%lad,this%lon1,this%lat1, &
   this%projection_center_flag
ENDIF

END SUBROUTINE gridpar_polarproj_write_unit


!> Display on the screen a brief content of griddim object.
SUBROUTINE gridpar_polarproj_display(this) 
TYPE(gridpar_polarproj),INTENT(in) :: this !< gridpar_polarproj object to display


IF (c_e(this%latin1) .OR. c_e(this%latin2) .OR. c_e(this%lov)) THEN
  PRINT*,"Polar projection:"
  PRINT*,"latIntersections",this%latin1,this%latin2
  PRINT*,"centralMeridian",this%lov
  IF (IAND(this%projection_center_flag, 128) == 0) THEN
    PRINT*,"North Pole"
  ELSE
    PRINT*,"South Pole"
  ENDIF
ENDIF

END SUBROUTINE gridpar_polarproj_display

END MODULE gridpar_polarproj_class

