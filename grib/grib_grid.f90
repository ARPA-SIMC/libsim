MODULE grib_grid_class

USE geo_grid_class
USE grib_io_class
IMPLICIT NONE

TYPE grib_grid
  TYPE(geo_grid) :: grid
  TYPE(grib_io) :: grib
END TYPE grib_grid

PRIVATE :: map_2d_to_1d

INTERFACE init
  MODULE PROCEDURE grgr_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE grgr_delete
END INTERFACE

!!$INTERFACE alloc
!!$  MODULE PROCEDURE grgr_alloc
!!$END INTERFACE

INTERFACE setsize
  MODULE PROCEDURE grgr_setsize
END INTERFACE

INTERFACE setelement
  MODULE PROCEDURE grgr_setelement
END INTERFACE

INTERFACE getival
  MODULE PROCEDURE grgr_getival
END INTERFACE

INTERFACE getrval
  MODULE PROCEDURE grgr_getrval
END INTERFACE

INTERFACE setval
  MODULE PROCEDURE grgr_setival, grgr_setrval
END INTERFACE

INTERFACE filter5p
  MODULE PROCEDURE grgr_filter5p
END INTERFACE

INTERFACE rescale
  MODULE PROCEDURE grgr_rescale
END INTERFACE

INTERFACE cut
  MODULE PROCEDURE grgr_cut
END INTERFACE

INTERFACE regrid
  MODULE PROCEDURE grgr_regrid
END INTERFACE

INTERFACE findgribinfo
  MODULE PROCEDURE grgr_findgribinfo
END INTERFACE

INTERFACE findgribdata
  MODULE PROCEDURE grgr_findgribdata
END INTERFACE

INTERFACE getgribinfo
  MODULE PROCEDURE grgr_getgribinfo
END INTERFACE

INTERFACE getgribdata
  MODULE PROCEDURE grgr_getgribdata
END INTERFACE

INTERFACE putgribdata
  MODULE PROCEDURE grgr_putgribdata
END INTERFACE

CONTAINS

SUBROUTINE grgr_init(this)
TYPE(grib_grid) :: this

CALL init(this%grid)
CALL init(this%grib)

END SUBROUTINE grgr_init


SUBROUTINE grgr_delete(this)
TYPE (grib_grid) :: this

CALL delete(this%grib)
CALL delete(this%grid)

END SUBROUTINE grgr_delete


!!$SUBROUTINE grgr_alloc(this, ier)
!!$TYPE (grib_grid) :: this
!!$INTEGER, INTENT(OUT) :: ier
!!$
!!$CALL alloc(this%grid, ier)
!!$
!!$END SUBROUTINE grgr_alloc


SUBROUTINE grgr_setsize(this, nlev, nvar, ntim, ier)
TYPE (grib_grid) :: this
INTEGER, INTENT(IN), OPTIONAL :: nlev, nvar, ntim
INTEGER, INTENT(OUT), OPTIONAL :: ier

CALL setsize(this%grid, nlev, nvar, ntim, ier)

END SUBROUTINE grgr_setsize


SUBROUTINE grgr_setelement(this, nlev, nvar, ntim, ier)
TYPE (grib_grid) :: this
INTEGER, INTENT(IN), OPTIONAL :: nlev, nvar, ntim
INTEGER, INTENT(OUT), OPTIONAL :: ier

CALL setelement(this%grid, nlev, nvar, ntim, ier)

END SUBROUTINE grgr_setelement


INTEGER FUNCTION grgr_getival(this, what) RESULT(grgr_getval)
TYPE(grib_grid), INTENT(IN) :: this
INTEGER, INTENT(IN) :: what

grgr_getval = getival(this%grid, what)

END FUNCTION grgr_getival


REAL FUNCTION grgr_getrval(this, what) RESULT(grgr_getval)
TYPE(grib_grid), INTENT(IN) :: this
INTEGER, INTENT(IN) :: what

grgr_getval = getival(this%grid, what)

END FUNCTION grgr_getrval


SUBROUTINE grgr_setival(this, what, val)
TYPE(grib_grid), INTENT(OUT) :: this
INTEGER, INTENT(IN) :: what
INTEGER :: val

CALL setval(this%grid, what, val)

END SUBROUTINE grgr_setival


SUBROUTINE grgr_setrval(this, what, val)
TYPE(grib_grid), INTENT(OUT) :: this
INTEGER, INTENT(IN) :: what
REAL :: val

CALL setval(this%grid, what, val)

END SUBROUTINE grgr_setrval


SUBROUTINE grgr_filter5p(this, times)
TYPE(grib_grid), INTENT(INOUT) :: this
INTEGER, INTENT(in), OPTIONAL :: times

CALL filter5p(this%grid, times)

END SUBROUTINE grgr_filter5p


SUBROUTINE grgr_rescale(this, convm, convs)
TYPE(grib_grid), INTENT(INOUT) :: this
REAL, INTENT(in) :: convm, convs

CALL rescale(this%grid, convm, convs)

END SUBROUTINE grgr_rescale


SUBROUTINE grgr_cut(this, i1, i2, j1, j2, ier)
TYPE(grib_grid), INTENT(INOUT) :: this
INTEGER, INTENT(IN) :: i1, i2 , j1, j2
INTEGER, INTENT(OUT) :: ier

CALL cut(this%grid, i1, i2, j1, j2, ier)

END SUBROUTINE grgr_cut


SUBROUTINE grgr_regrid(this, ngx, ngy, ier)
TYPE(grib_grid), INTENT(INOUT) :: this
INTEGER, INTENT(IN) :: ngx, ngy
INTEGER, INTENT(OUT) :: ier

CALL regrid(this%grid, ngx, ngy, ier)

END SUBROUTINE grgr_regrid


FUNCTION grgr_findgribinfo(this, unit, ier)
! Find a grib matching this%mg key and store its info in this
TYPE (grib_grid) :: this
INTEGER :: unit, ier
LOGICAL :: grgr_findgribinfo

grgr_findgribinfo = findgribinfo(this%grib, unit, ier)

END FUNCTION grgr_findgribinfo


FUNCTION grgr_findgribdata(this, unit, ier)
! Find a grib matching this%mg key and store its info and data in this
TYPE (grib_grid) :: this
INTEGER :: unit, ier
LOGICAL :: grgr_findgribdata

!!$IF (ASSOCIATED(this%grid%field2d)) &
!!$ CALL map_2d_to_1d(this%grid%field2d, SHAPE(this%grid%field2d), this%grib%zsec4)
grgr_findgribdata = findgribdata(this%grib, unit, ier)
IF (grgr_findgribdata) CALL gribexd_to_grid(this%grib, this%grid, ier)

END FUNCTION grgr_findgribdata


SUBROUTINE grgr_getgribinfo(this, unit, ier)
! Read next grib in unit and store its info in this
TYPE (grib_grid) :: this
INTEGER :: unit, ier
CALL getgribinfo(this%grib, unit, ier)
END SUBROUTINE grgr_getgribinfo


SUBROUTINE grgr_getgribdata(this, unit, ier)
! Read next grib in unit and store its info and data in this
TYPE (grib_grid) :: this
INTEGER :: unit, ier
!!$IF (ASSOCIATED(this%grid%field2d)) &
!!$ CALL map_2d_to_1d(this%grid%field2d, SHAPE(this%grid%field2d), this%grib%zsec4)
CALL getgribdata(this%grib, unit, ier)
CALL gribexd_to_grid(this%grib, this%grid, ier)
END SUBROUTINE grgr_getgribdata


SUBROUTINE grgr_putgribdata(this, unit, ier)
! Write grib in this to unit
TYPE (grib_grid) :: this
INTEGER :: unit, ier

IF (ASSOCIATED(this%grid%field2d)) THEN
 CALL map_2d_to_1d(this%grid%field2d, SHAPE(this%grid%field2d), this%grib%zsec4)
ELSE
  ier = 1
  RETURN
ENDIF
CALL grid_to_gribexc(this%grid, this%grib, ier)
CALL putgribdata(this%grib, unit, ier)
END SUBROUTINE grgr_putgribdata

SUBROUTINE gribexd_to_grid(grib, grid, ier)
!
! Fill geo_grid object with data from relevant gribex section arrays
! after call to a gribex I or D operation
!
TYPE(grib_io), INTENT(IN) :: grib
TYPE(geo_grid), INTENT(OUT) :: grid
INTEGER, INTENT(OUT) :: ier

INTEGER :: x1, x2, xs, y1, y2, ys, ord(2)
REAL, POINTER :: field(:,:)

CALL setval(grid, gg_drt, grib%isec2(1))
IF (grib%isec2(1) == 0 .OR. grib%isec2(1) == 10 .OR. grib%isec2(1) == 20 &
 .OR. grib%isec2(1) == 30) THEN ! Lat/lon
  CALL setval(grid, gg_nx, grib%isec2(2))
  CALL setval(grid, gg_ny, grib%isec2(3))
  CALL setval(grid, gg_firstlon, grib%isec2(5)/1000.)
  CALL setval(grid, gg_firstlat, grib%isec2(4)/1000.)
  CALL setval(grid, gg_lastlon, grib%isec2(8)/1000.)
  CALL setval(grid, gg_lastlat, grib%isec2(7)/1000.)
  CALL setval(grid, gg_ija, grib%isec2(11))
  IF (grib%isec2(6) == 0) THEN ! Compute increments
    CALL setval(grid, gg_loninc, &
     ABS(getrval(grid, gg_lastlon)-getrval(grid, gg_firstlon))/(grib%isec2(2)-1))
    CALL setval(grid, gg_latinc, &
     ABS(getrval(grid, gg_lastlat)-getrval(grid, gg_firstlat))/(grib%isec2(3)-1))
  ELSE ! Increments given
    CALL setval(grid, gg_loninc, grib%isec2(9)/1000.)
    CALL setval(grid, gg_latinc, grib%isec2(10)/1000.)
  ENDIF
ELSE IF (grib%isec2(1) == 4 .OR. grib%isec2(1) == 14 .OR. grib%isec2(1) == 24 &
 .OR. grib%isec2(1) == 34) THEN ! Gaussian
  CALL setval(grid, gg_nx, grib%isec2(2))
  CALL setval(grid, gg_ny, grib%isec2(3))
  CALL setval(grid, gg_firstlon, grib%isec2(5)/1000.)
  CALL setval(grid, gg_firstlat, grib%isec2(4)/1000.)
  CALL setval(grid, gg_lastlon, grib%isec2(8)/1000.)
  CALL setval(grid, gg_lastlat, grib%isec2(7)/1000.)
  CALL setval(grid, gg_npar, grib%isec2(10))
  CALL setval(grid, gg_ija, grib%isec2(11))
  IF (grib%isec2(6) == 0) THEN ! Compute increments
    CALL setval(grid, gg_loninc, &
     ABS(getrval(grid, gg_lastlon)-getrval(grid, gg_firstlon))/(grib%isec2(2)-1))
  ELSE ! Increments given
    CALL setval(grid, gg_loninc, grib%isec2(9)/1000.)
  ENDIF
ELSE ! Add other projections!!
  CALL setval(grid, gg_nx, grib%isec4(1))
  CALL setval(grid, gg_ny, 1)
ENDIF

IF (grib%isec2(1) == 10 .OR. grib%isec2(1) == 14 .OR. &
 grib%isec2(1) == 30 .OR. grib%isec2(1) == 34) THEN ! Rotated lat/lon or Gaussian
  CALL setval(grid, gg_lonrot, grib%isec2(14)/1000.)
  CALL setval(grid, gg_latrot, grib%isec2(13)/1000.)
  CALL setval(grid, gg_rot, grib%zsec2(1))
!!$  CALL setval(grid, gg_lonrot_hibu, grid%lonrot)
!!$  CALL setval(grid, gg_latrot_hibu, ACOSD(-SIND(grid%latrot)))
ELSE
  CALL setval(grid, gg_lonrot, 0.)
  CALL setval(grid, gg_latrot, -90.)
  CALL setval(grid, gg_rot, 0.)
!!$  CALL setval(grid, gg_lonrot_hibu, 0.)
!!$  CALL setval(grid, gg_latrot_hibu, 0.)
ENDIF
IF (grib%isec2(1) == 20 .OR. grib%isec2(1) == 24 .OR. &
 grib%isec2(1) == 30 .OR. grib%isec2(1) == 34) THEN ! Stretched lat/lon or Gaussian
  CALL setval(grid, gg_lonstr, grib%isec2(16)/1000.)
  CALL setval(grid, gg_latstr, grib%isec2(15)/1000.)
  CALL setval(grid, gg_str, grib%zsec2(2))
ELSE
  CALL setval(grid, gg_lonstr, 0.)
  CALL setval(grid, gg_latstr, -90.)
  CALL setval(grid, gg_str, 1.)
ENDIF

!!$IF (grib%isec2(12) > 0) THEN ! Vertical coordinate parameters present
!!$  IF (ASSOCIATED(grid%vcp)) THEN
!!$    IF (SIZE(grid%vcp) /= grib%isec2(12)) THEN ! == is often the case
!!$      DEALLOCATE(grid%vcp)
!!$      ALLOCATE(grid%vcp(grib%isec2(12)))
!!$    ENDIF
!!$  ELSE
!!$    ALLOCATE(grid%vcp(grib%isec2(12)))
!!$  ENDIF
!!$  grid%vcp(1:grib%isec2(12)) = grib%zsec2(11:11+grib%isec2(12)-1)
!!$ELSE
!!$  IF (ASSOCIATED(grid%vcp)) THEN
!!$    DEALLOCATE(grid%vcp)
!!$  ENDIF
!!$ENDIF

! Allocate space if necessary
CALL alloc(grid, ier)
IF (ier /= 0) RETURN
field => getfield2d(grid)
! Transfer data field changing scanning mode to 64
IF (IAND(grib%isec2(11), 128) == 0) THEN
  x1 = 1
  x2 = getival(grid, gg_nx)
  xs = 1
ELSE
  x1 = getival(grid, gg_nx)
  x2 = 1
  xs = -1
ENDIF
IF (IAND(grib%isec2(11), 64) == 0) THEN
  y1 = getival(grid, gg_ny)
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = getival(grid, gg_ny)
  ys = 1
ENDIF
IF (IAND(grib%isec2(11), 32) == 0) THEN
  ord = (/1,2/)
ELSE
  ord = (/2,1/)
ENDIF
field(x1:x2:xs,y1:y2:ys) = &
 RESHAPE(grib%zsec4(1:getival(grid, gg_nx)*getival(grid, gg_ny)), &
 (/getival(grid, gg_nx), getival(grid, gg_ny)/), ORDER=ord)

END SUBROUTINE gribexd_to_grid


SUBROUTINE grid_to_gribexc(grid, grib, ier)
!
! Fill geo_grid object with data from relevant gribex section arrays
! after call to a gribex I or D operation
!
TYPE(geo_grid), INTENT(IN) :: grid
TYPE(grib_io), INTENT(OUT) :: grib
INTEGER, INTENT(OUT) :: ier

INTEGER :: x1, x2, xs, y1, y2, ys
REAL, POINTER :: field(:,:)

grib%isec2(1) = getival(grid, gg_drt)
IF (grib%isec2(1) == 0 .OR. grib%isec2(1) == 10 .OR. grib%isec2(1) == 20 &
 .OR. grib%isec2(1) == 30) THEN ! Lat/lon
  grib%isec2(2) = getival(grid, gg_nx)
  grib%isec2(3) = getival(grid, gg_ny)
  grib%isec2(5) = getrval(grid, gg_firstlon)*1000.
  grib%isec2(4) = getrval(grid, gg_firstlat)*1000.
  grib%isec2(8) = getrval(grid, gg_lastlon)*1000.
  grib%isec2(7) = getrval(grid, gg_lastlat)*1000.
  grib%isec2(11) = getival(grid, gg_ija)
  grib%isec2(9) = getrval(grid, gg_loninc)*1000.
  grib%isec2(10) = getrval(grid, gg_latinc)*1000.
  IF (ABS(REAL(grib%isec2(9) - getrval(grid, gg_loninc)*1000.)) > 0.5 .OR. &
   ABS(REAL(grib%isec2(10) - getrval(grid, gg_latinc)*1000)) > 0.5) THEN ! Increments not accurate
    grib%isec2(6) = 0
    grib%isec2(9) = 0
    grib%isec2(10) = 0
  ELSE ! Increments accurate, can be given
    grib%isec2(6) = 128
  ENDIF
  grib%isec2(18:22) = 0
ELSE IF (grib%isec2(1) == 4 .OR. grib%isec2(1) == 14 .OR. grib%isec2(1) == 24 &
 .OR. grib%isec2(1) == 34) THEN ! Gaussian
  grib%isec2(2) = getival(grid, gg_nx)
  grib%isec2(3) = getival(grid, gg_ny)
  grib%isec2(5) = getrval(grid, gg_firstlon)*1000.
  grib%isec2(4) = getrval(grid, gg_firstlat)*1000.
  grib%isec2(8) = getrval(grid, gg_lastlon)*1000.
  grib%isec2(7) = getrval(grid, gg_lastlat)*1000.
  grib%isec2(10) = getival(grid, gg_npar)
  grib%isec2(11) = getival(grid, gg_ija)
  grib%isec2(9) = getrval(grid, gg_loninc)*1000.
  IF (ABS(REAL(grib%isec2(9) - getrval(grid, gg_loninc)*1000.)) > 0.5) THEN ! Increments not accurate
    grib%isec2(6) = 0
    grib%isec2(9) = 0
  ELSE ! Increments accurate, can be given
    grib%isec2(6) = 128
  ENDIF
  grib%isec2(18:22) = 0
ELSE ! Add other projections!!
  PRINT'(A,I3,A)','Error, projection ',grib%isec2(1),' not supported'
  ier = 1
  RETURN
ENDIF

IF (grib%isec2(1) == 10 .OR. grib%isec2(1) == 14 .OR. &
 grib%isec2(1) == 30 .OR. grib%isec2(1) == 34) THEN ! Rotated lat/lon or Gaussian
  grib%isec2(14) = getrval(grid, gg_lonrot)*1000.
  grib%isec2(13) = getrval(grid, gg_latrot)*1000.
  grib%zsec2(1) = getrval(grid, gg_rot)
ELSE
  grib%isec2(14) = 0
  grib%isec2(13) = -90000
  grib%zsec2(1) = 0.
ENDIF
IF (grib%isec2(1) == 20 .OR. grib%isec2(1) == 24 .OR. &
 grib%isec2(1) == 30 .OR. grib%isec2(1) == 34) THEN ! Stretched lat/lon or Gaussian
  grib%isec2(16) = getrval(grid, gg_lonstr)*1000.
  grib%isec2(15) = getrval(grid, gg_latstr)*1000.
  grib%zsec2(2) = getrval(grid, gg_str)
ELSE
  grib%isec2(16) = 0
  grib%isec2(15) = -90000
  grib%zsec2(2) = 1.
ENDIF
grib%zsec2(3:10) = 0.

!!$IF (grib%isec2(12) > 0) THEN ! Vertical coordinate parameters present
!!$  IF (ASSOCIATED(grid%vcp)) THEN
!!$    IF (SIZE(grid%vcp) /= grib%isec2(12)) THEN ! == is often the case
!!$      DEALLOCATE(grid%vcp)
!!$      ALLOCATE(grid%vcp(grib%isec2(12)))
!!$    ENDIF
!!$  ELSE
!!$    ALLOCATE(grid%vcp(grib%isec2(12)))
!!$  ENDIF
!!$  grid%vcp(1:grib%isec2(12)) = grib%zsec2(11:11+grib%isec2(12)-1)
!!$ELSE
!!$  IF (ASSOCIATED(grid%vcp)) THEN
!!$    DEALLOCATE(grid%vcp)
!!$  ENDIF
!!$ENDIF
grib%isec3(1) = 0 ! Explicit missing data
grib%isec3(2) = grio_imd
grib%zsec3(1) = 0.
grib%zsec3(2) = gg_rmd
grib%isec4(1) = grib%isec2(2)*grib%isec2(3)
grib%isec4(2) = MIN(MAX(grib%isec4(1),1),24) ! Use input safely
grib%isec4(3:) = 0

IF (.NOT. ASSOCIATED(grib%zsec4)) THEN
  ALLOCATE(grib%zsec4(grib%isec4(1)), STAT=ier)
ELSE IF (SIZE(grib%zsec4) < grib%isec4(1)) THEN
  DEALLOCATE(grib%zsec4)
  ALLOCATE(grib%zsec4(grib%isec4(1)), STAT=ier)
ENDIF
IF (ier /= 0) RETURN

field => getfield2d(grid)
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

END SUBROUTINE grid_to_gribexc


SUBROUTINE map_2d_to_1d(var2d, shape2d, var1d)
INTEGER, INTENT(IN) :: shape2d(2)
REAL, TARGET :: var2d(shape2d(1)*shape2d(2))
REAL, POINTER :: var1d(:)
var1d => var2d
END SUBROUTINE map_2d_to_1d


END MODULE grib_grid_class
