MODULE geo_grid_/**/GRIBLIB
USE geo_grid_class
USE GRIBLIB/**/_io_class
IMPLICIT NONE

#ifdef WITH_EMOS
INTEGER, PARAMETER :: &
 idrt=1, inx=2, iny=3, ila0=4, ilo0=5, irf=6, ila1=7, ilo1=8, idx=9, idy=10, &
 iscm=11, ilasp=13, ilosp=14

#elif defined WITH_DWDGRIB1
INTEGER, PARAMETER :: &
 idrt=4, inx=5, iny=6, ila0=7, ilo0=8, ila1=10, ilo1=11, idx=12, idy=13, iscm=14, &
 ilasp=20, ilosp=21
#endif

INTERFACE import
  MODULE PROCEDURE geo_grid_import_from_/**/GRIBLIB
END INTERFACE

INTERFACE export
  MODULE PROCEDURE geo_grid_export_to_/**/GRIBLIB
END INTERFACE

CONTAINS

SUBROUTINE geo_grid_import_from_/**/GRIBLIB(this, grib, ier)
!
! Import data from relevant gribex section arrays,
! after call to a gribex I or D operation, into a geo_grid object
!
TYPE(geo_grid), INTENT(OUT) :: this
TYPE(GRIBLIB/**/_io), INTENT(IN) :: grib
INTEGER, INTENT(OUT) :: ier

INTEGER :: proj, x1, x2, xs, y1, y2, ys, ord(2)
REAL :: s2_4, s2_5, s2_7, s2_8, rot
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
  CALL raise_warning('proiezione (drt) grib '//to_char(grib%isec2(1)) &
   //' non supportata, procedo lo stesso')
END SELECT
CALL setval(this, proj=proj)

SELECT CASE(grib%isec2(idrt))
CASE(0, 10, 20, 30) ! Lat/lon
  IF (IAND(grib%isec2(iscm), 128) == 0) THEN
    s2_5 = grib%isec2(ilo0)/1000.
    s2_8 = grib%isec2(ilo1)/1000.
  ELSE
    s2_8 = grib%isec2(ilo0)/1000.
    s2_5 = grib%isec2(ilo1)/1000.
  ENDIF
  IF (IAND(grib%isec2(iscm), 64) == 0) THEN
    s2_7 = grib%isec2(ila0)/1000.
    s2_4 = grib%isec2(ila1)/1000.
  ELSE
    s2_4 = grib%isec2(ila0)/1000.
    s2_7 = grib%isec2(ila1)/1000.
  ENDIF
!  s2_9 = grib%isec2(idx)/1000.
!  s2_10 = grib%isec2(idy)/1000.

  CALL setval(this, nx=grib%isec2(inx), ny=grib%isec2(iny), x1=s2_5, &
   y1=s2_4, x2=s2_8, y2=s2_7)

#ifdef WITH_EMOS
  IF (grib%isec2(irf) /= 0) THEN ! Increments given
    CALL setval(this, dx=grib%isec2(idx)/1000., dy=grib%isec2(idy)/1000.)
  ENDIF
#elif defined WITH_DWDGRIB1
  IF (grib%isec2(idx) > 0 .AND. grib%isec2(idy) > 0) THEN
    CALL setval(this, dx=grib%isec2(idx)/1000., dy=grib%isec2(idy)/1000.)
  ENDIF
#endif

CASE default ! Add other projections!!
  CALL raise_warning('proiezione (drt) grib '//to_char(grib%isec2(idrt)) &
   //' non supportata, procedo lo stesso')
  CALL setval(this, nx=grib%isec2(inx)*grib%isec2(iny), ny=1) ! grib%isec4(1)
END SELECT

SELECT CASE(grib%isec2(idrt))
CASE(10, 14, 30, 34) ! Rotated lat/lon or Gaussian
#ifdef WITH_EMOS
  CALL setval(this, xrot=grib%isec2(ilosp)/1000., &
   yrot=raddeg*ACOS(-SIN(degrad*grib%isec2(ilasp)/1000.)), rot=grib%zsec2(1))
#elif defined WITH_DWDGRIB1
  rot = grib%isec2(22)/1000.
  CALL setval(this, xrot=grib%isec2(ilosp)/1000., &
   yrot=raddeg*ACOS(-SIN(degrad*grib%isec2(ilasp)/1000.)), rot=rot)
#endif

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
IF (IAND(grib%isec2(iscm), 128) == 0) THEN
  x1 = 1
  x2 = grib%isec2(inx)
  xs = 1
ELSE
  x1 = grib%isec2(inx)
  x2 = 1
  xs = -1
ENDIF
IF (IAND(grib%isec2(iscm), 64) == 0) THEN
  y1 = grib%isec2(iny)
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = grib%isec2(iny)
  ys = 1
ENDIF
IF (IAND(grib%isec2(11), 32) == 0) THEN
  ord = (/1,2/)
ELSE
  ord = (/2,1/)
ENDIF
field(x1:x2:xs,y1:y2:ys) = &
 RESHAPE(grib%zsec4(1:grib%isec2(inx)*grib%isec2(iny)), &
 (/grib%isec2(inx), grib%isec2(iny)/), ORDER=ord)

END SUBROUTINE geo_grid_import_from_/**/GRIBLIB


SUBROUTINE geo_grid_export_to_/**/GRIBLIB(this, grib, ier)
!
! Export a geo_grid object to a grib_io object
!
TYPE(geo_grid), INTENT(IN) :: this
TYPE(GRIBLIB/**/_io), INTENT(OUT) :: grib
INTEGER, INTENT(OUT) :: ier

INTEGER :: proj, x1, x2, xs, y1, y2, ys
REAL :: s2_4, s2_5, s2_7, s2_8, s2_9, s2_10, s2_13, s2_14, rot
REAL(kind=fp_gg), POINTER :: field(:,:)

CALL getval(this, proj=proj)
SELECT CASE(proj)
CASE(gg_proj_geo)
  grib%isec2(idrt) = 0
CASE(gg_proj_georot)
  grib%isec2(idrt) = 10
!CASE(gg_proj_utm)
!  grib%isec2(1) = ??
CASE default
  CALL raise_error('invalid projection code '//to_char(proj) &
   //' in geo_grid object', 1, ier)
  RETURN
END SELECT

SELECT CASE(grib%isec2(idrt))
CASE(0, 10, 20, 30) ! Lat/lon
  CALL getval(this, nx=grib%isec2(inx), ny=grib%isec2(iny), x1=s2_5, &
   y1=s2_4, x2=s2_8, y2=s2_7, dx=s2_9, dy=s2_10)

  IF (IAND(grib%isec2(iscm), 128) == 0) THEN
    grib%isec2(ilo0) = s2_5*1000.
    grib%isec2(ilo1) = s2_8*1000.
  ELSE
    grib%isec2(ilo0) = s2_8*1000.
    grib%isec2(ilo1) = s2_5*1000.
  ENDIF
  IF (IAND(grib%isec2(iscm), 64) == 0) THEN
    grib%isec2(ila0) = s2_7*1000.
    grib%isec2(ila1) = s2_4*1000.
  ELSE
    grib%isec2(ila0) = s2_4*1000.
    grib%isec2(ila1) = s2_7*1000.
  ENDIF
  grib%isec2(idx) = s2_9*1000.
  grib%isec2(idy) = s2_10*1000.
  IF (ABS(REAL(grib%isec2(idx) - s2_9*1000.)) > 1E-3 .OR. &
   ABS(REAL(grib%isec2(idy) - s2_10*1000.)) > 1E-3) THEN ! Increments not accurate
#ifdef WITH_EMOS
    grib%isec2(irf) = 0
#endif
    grib%isec2(idx) = 0 ! should be -65536?
    grib%isec2(idy) = 0
  ELSE ! Increments accurate, can be given
#ifdef WITH_EMOS
    grib%isec2(irf) = 128
#endif
  ENDIF
#ifdef WITH_EMOS
  grib%isec2(17:18) = 0
  grib%isec2(20:22) = 0
#endif

CASE DEFAULT ! Add other projections!!
  CALL raise_error('projection '//to_char(grib%isec2(1))//' not supported', 1, ier)
  RETURN
END SELECT

SELECT CASE(grib%isec2(idrt))
CASE(10, 14, 30, 34) ! Rotated lat/lon or Gaussian
#ifdef WITH_EMOS
  CALL getval(this, xrot=s2_14, yrot=s2_13, rot=grib%zsec2(1))
#elif defined WITH_DWDGRIB1
  CALL getval(this, xrot=s2_14, yrot=s2_13, rot=rot)
  grib%isec2(22) = rot/1000.
#endif
  grib%isec2(ilosp) = s2_14*1000.
  grib%isec2(ilasp) = raddeg*ASIN(-COS(degrad*s2_13))*1000.
END SELECT

#ifdef WITH_EMOS
grib%zsec2(3:10) = 0.
grib%isec3(1) = 0 ! Explicit missing data
grib%isec3(2) = grio_imiss
grib%zsec3(1) = 0.
grib%zsec3(2) = grio_rmiss
grib%isec4(1) = grib%isec2(2)*grib%isec2(3)
grib%isec4(2) = MIN(MAX(grib%isec4(2),1),24) ! Use input safely
grib%isec4(3:33) = 0
#elif defined WITH_DWDGRIB1
grib%idims(17) = grib%isec2(inx)*grib%isec2(iny) ! ANZAHL DER DS-DATENWERTE
#endif

IF (.NOT. ASSOCIATED(grib%zsec4)) THEN
  ALLOCATE(grib%zsec4(grib%isec2(inx)*grib%isec2(iny)), STAT=ier)
ELSE IF (SIZE(grib%zsec4) < grib%isec2(inx)*grib%isec2(iny)) THEN
  DEALLOCATE(grib%zsec4)
  ALLOCATE(grib%zsec4(grib%isec2(inx)*grib%isec2(iny)), STAT=ier)
ENDIF
IF (ier /= 0) RETURN

CALL getval(this, field2d=field)
! Transfer data field changing scanning mode from 64
IF (IAND(grib%isec2(iscm), 128) == 0) THEN
  x1 = 1
  x2 = grib%isec2(inx)
  xs = 1
ELSE
  x1 = grib%isec2(inx)
  x2 = 1
  xs = -1
ENDIF
IF (IAND(grib%isec2(11), 64) == 0) THEN
  y1 = grib%isec2(iny)
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = grib%isec2(iny)
  ys = 1
ENDIF
IF (IAND(grib%isec2(iscm), 32) == 0) THEN
  grib%zsec4(1:grib%isec2(inx)*grib%isec2(iny)) = &
   PACK(field(x1:x2:xs,y1:y2:ys), .TRUE.)
ELSE
  grib%zsec4(1:grib%isec2(inx)*grib%isec2(iny)) = &
   PACK(TRANSPOSE(field(x1:x2:xs,y1:y2:ys)), .TRUE.)
ENDIF

END SUBROUTINE geo_grid_export_to_/**/GRIBLIB

END MODULE geo_grid_/**/GRIBLIB
