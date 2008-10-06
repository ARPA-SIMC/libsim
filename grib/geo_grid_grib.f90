MODULE geo_grid_grib
USE geo_grid_class
USE grib_io_class
IMPLICIT NONE

INTERFACE import
  MODULE PROCEDURE geo_grid_import_from_grib
END INTERFACE

INTERFACE export
  MODULE PROCEDURE geo_grid_export_to_grib
END INTERFACE

CONTAINS

SUBROUTINE geo_grid_import_from_grib(this, grib, ier)
!
! Import data from relevant gribex section arrays,
! after call to a gribex I or D operation, into a geo_grid object
!
TYPE(geo_grid), INTENT(OUT) :: this
TYPE(grib_io), INTENT(IN) :: grib
INTEGER, INTENT(OUT) :: ier

INTEGER :: proj, x1, x2, xs, y1, y2, ys, ord(2)
REAL :: s2_4, s2_5, s2_7, s2_8, s2_9, s2_10
REAL(kind=fp_gg), POINTER :: field(:,:)

SELECT CASE(grib%isec2(1))
CASE(0)
  proj = gg_proj_geo
CASE(10)
  proj = gg_proj_georot
!CASE(??)
!  proj = gg_proj_utm
CASE default
  proj = gg_proj_gen
  CALL raise_warning('proiezione (drt) grib '//to_char(grib%isec4(1)) &
   //' non supportata, procedo lo stesso')
END SELECT
CALL setval(this, proj=proj)

SELECT CASE(grib%isec2(1))
CASE(0, 10, 20, 30) ! Lat/lon
  IF (IAND(grib%isec2(11), 128) == 0) THEN
    s2_5 = grib%isec2(5)/1000.
    s2_8 = grib%isec2(8)/1000.
  ELSE
    s2_8 = grib%isec2(5)/1000.
    s2_5 = grib%isec2(8)/1000.
  ENDIF
  IF (IAND(grib%isec2(11), 64) == 0) THEN
    s2_7 = grib%isec2(4)/1000.
    s2_4 = grib%isec2(7)/1000.
  ELSE
    s2_4 = grib%isec2(4)/1000.
    s2_7 = grib%isec2(7)/1000.
  ENDIF
  s2_9 = grib%isec2(9)/1000.
  s2_10 = grib%isec2(10)/1000.

  CALL setval(this, nx=grib%isec2(2), ny=grib%isec2(3), x1=s2_5, &
   y1=s2_4, x2=s2_8, y2=s2_7)

  IF (grib%isec2(6) /= 0) THEN ! Increments given
    CALL setval(this, dx=grib%isec2(9)/1000., dy=grib%isec2(10)/1000.)
  ENDIF

CASE default ! Add other projections!!
  CALL raise_warning('proiezione (drt) grib '//to_char(grib%isec2(1)) &
   //' non supportata, procedo lo stesso')
  CALL setval(this, nx=grib%isec4(1), ny=1)
END SELECT

SELECT CASE(grib%isec2(1))
CASE(10, 14, 30, 34) ! Rotated lat/lon or Gaussian
!!$  s2_13 = grib%isec2(13)/1000.
!!$  s2_14 = grib%isec2(14)/1000.
  CALL setval(this, xrot=grib%isec2(14)/1000., &
   yrot=raddeg*ACOS(-SIN(degrad*grib%isec2(13)/1000.)), rot=grib%zsec2(1))
CASE default
  CALL setval(this, xrot=0., yrot=-90., rot=0.)
END SELECT

!!$IF (grib%isec2(12) > 0) THEN ! Vertical coordinate parameters present
!!$  IF (ASSOCIATED(this%vcp)) THEN
!!$    IF (SIZE(this%vcp) /= grib%isec2(12)) THEN ! == is often the case
!!$      DEALLOCATE(this%vcp)
!!$      ALLOCATE(this%vcp(grib%isec2(12)))
!!$    ENDIF
!!$  ELSE
!!$    ALLOCATE(this%vcp(grib%isec2(12)))
!!$  ENDIF
!!$  this%vcp(1:grib%isec2(12)) = grib%zsec2(11:11+grib%isec2(12)-1)
!!$ELSE
!!$  IF (ASSOCIATED(this%vcp)) THEN
!!$    DEALLOCATE(this%vcp)
!!$  ENDIF
!!$ENDIF

! Allocate space if necessary
CALL alloc(this, ier)
IF (ier /= 0) RETURN
NULLIFY(field)
CALL getval(this, field2d=field)
IF (.NOT. ASSOCIATED(field)) THEN
  CALL raise_error('griglia non allocata')
  RETURN
ENDIF
! Transfer data field changing scanning mode to 64
IF (IAND(grib%isec2(11), 128) == 0) THEN
  x1 = 1
  x2 = grib%isec2(2)
  xs = 1
ELSE
  x1 = grib%isec2(2)
  x2 = 1
  xs = -1
ENDIF
IF (IAND(grib%isec2(11), 64) == 0) THEN
  y1 = grib%isec2(3)
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = grib%isec2(3)
  ys = 1
ENDIF
IF (IAND(grib%isec2(11), 32) == 0) THEN
  ord = (/1,2/)
ELSE
  ord = (/2,1/)
ENDIF
field(x1:x2:xs,y1:y2:ys) = &
 RESHAPE(grib%zsec4(1:grib%isec2(2)*grib%isec2(3)), &
 (/grib%isec2(2), grib%isec2(3)/), ORDER=ord)

END SUBROUTINE geo_grid_import_from_grib


SUBROUTINE geo_grid_export_to_grib(this, grib, ier)
!
! Export a geo_grid object to a grib_io object
!
TYPE(geo_grid), INTENT(IN) :: this
TYPE(grib_io), INTENT(OUT) :: grib
INTEGER, INTENT(OUT) :: ier

INTEGER :: proj, x1, x2, xs, y1, y2, ys
REAL :: s2_4, s2_5, s2_7, s2_8, s2_9, s2_10, s2_13, s2_14
REAL(kind=fp_gg), POINTER :: field(:,:)

CALL getval(this, proj=proj)
SELECT CASE(proj)
CASE(gg_proj_geo)
  grib%isec2(1) = 0
CASE(gg_proj_georot)
  grib%isec2(1) = 10
!CASE(gg_proj_utm)
!  grib%isec2(1) = ??
CASE default
  CALL raise_error('invalid projection code '//to_char(proj) &
   //' in geo_grid object', 1, ier)
  RETURN
END SELECT

SELECT CASE(grib%isec2(1))
CASE(0, 10, 20, 30) ! Lat/lon
  CALL getval(this, nx=grib%isec2(2), ny=grib%isec2(3), x1=s2_5, &
   y1=s2_4, x2=s2_8, y2=s2_7, dx=s2_9, dy=s2_10)

  IF (IAND(grib%isec2(11), 128) == 0) THEN
    grib%isec2(5) = s2_5*1000.
    grib%isec2(8) = s2_8*1000.
  ELSE
    grib%isec2(5) = s2_8*1000.
    grib%isec2(8) = s2_5*1000.
  ENDIF
  IF (IAND(grib%isec2(11), 64) == 0) THEN
    grib%isec2(4) = s2_7*1000.
    grib%isec2(7) = s2_4*1000.
  ELSE
    grib%isec2(4) = s2_4*1000.
    grib%isec2(7) = s2_7*1000.
  ENDIF
  grib%isec2(9) = s2_9*1000.
  grib%isec2(10) = s2_10*1000.
  IF (ABS(REAL(grib%isec2(9) - s2_9*1000.)) > 1E-3 .OR. &
   ABS(REAL(grib%isec2(10) - s2_10*1000.)) > 1E-3) THEN ! Increments not accurate
    grib%isec2(6) = 0
    grib%isec2(9) = 0
    grib%isec2(10) = 0
  ELSE ! Increments accurate, can be given
    grib%isec2(6) = 128
  ENDIF
  grib%isec2(17:18) = 0
  grib%isec2(20:22) = 0
CASE DEFAULT ! Add other projections!!
  CALL raise_error('projection '//to_char(grib%isec2(1))//' not supported', 1, ier)
  RETURN
END SELECT

SELECT CASE(grib%isec2(1))
CASE(10, 14, 30, 34) ! Rotated lat/lon or Gaussian
  CALL getval(this, xrot=s2_14, yrot=s2_13, rot=grib%zsec2(1))
  grib%isec2(14) = s2_14*1000.
  grib%isec2(13) = raddeg*ASIN(-COS(degrad*s2_13))*1000.
END SELECT

!!$IF (grib%isec2(1) == 20 .OR. grib%isec2(1) == 24 .OR. &
!!$ grib%isec2(1) == 30 .OR. grib%isec2(1) == 34) THEN ! Stretched lat/lon or Gaussian
!!$  grib%isec2(16) = getrval(this, gg_lonstr)*1000.
!!$  grib%isec2(15) = getrval(this, gg_latstr)*1000.
!!$  grib%zsec2(2) = getrval(this, gg_str)
!!$ELSE
!!$  grib%isec2(16) = 0
!!$  grib%isec2(15) = -90000
!!$  grib%zsec2(2) = 1.
!!$ENDIF
grib%zsec2(3:10) = 0.

grib%isec3(1) = 0 ! Explicit missing data
grib%isec3(2) = grio_imiss
grib%zsec3(1) = 0.
grib%zsec3(2) = grio_rmiss
grib%isec4(1) = grib%isec2(2)*grib%isec2(3)
grib%isec4(2) = MIN(MAX(grib%isec4(2),1),24) ! Use input safely
grib%isec4(3:33) = 0

IF (.NOT. ASSOCIATED(grib%zsec4)) THEN
  ALLOCATE(grib%zsec4(grib%isec4(1)), STAT=ier)
ELSE IF (SIZE(grib%zsec4) < grib%isec4(1)) THEN
  DEALLOCATE(grib%zsec4)
  ALLOCATE(grib%zsec4(grib%isec4(1)), STAT=ier)
ENDIF
IF (ier /= 0) RETURN

CALL getval(this, field2d=field)
! Transfer data field changing scanning mode from 64
IF (IAND(grib%isec2(11), 128) == 0) THEN
  x1 = 1
  x2 = grib%isec2(2)
  xs = 1
ELSE
  x1 = grib%isec2(2)
  x2 = 1
  xs = -1
ENDIF
IF (IAND(grib%isec2(11), 64) == 0) THEN
  y1 = grib%isec2(3)
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = grib%isec2(3)
  ys = 1
ENDIF
IF (IAND(grib%isec2(11), 32) == 0) THEN
  grib%zsec4(1:grib%isec4(1)) = PACK(field(x1:x2:xs,y1:y2:ys), .TRUE.)
ELSE
  grib%zsec4(1:grib%isec4(1)) = PACK(TRANSPOSE(field(x1:x2:xs,y1:y2:ys)), .TRUE.)
ENDIF

END SUBROUTINE geo_grid_export_to_grib

END MODULE geo_grid_grib
