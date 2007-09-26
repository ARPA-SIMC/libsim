MODULE geo_grid_class
USE kinds
USE missing_values
USE char_utilities
USE err_handling
USE geo_coord_class

IMPLICIT NONE

INTEGER, PARAMETER :: fp_gg = fp_s
REAL(kind=fp_gg), PARAMETER :: gg_miss = rsmiss
INTEGER, PARAMETER :: gg_proj_gen=-1, gg_proj_geo=0, gg_proj_utm=1, &
 gg_proj_georot=2

TYPE geo_grid
  PRIVATE
  INTEGER :: proj, nx, ny
  INTEGER :: fuso, elliss
  REAL :: x1, x2, y1, y2, dx, dy, &
   xrot, yrot, rot
  INTEGER :: nlev, nvar, ntim, curlev, curvar, curtim
  REAL, POINTER :: vcp(:)
  REAL(kind=fp_gg),POINTER :: field2d(:,:), field5d(:,:,:,:,:)
  REAL,POINTER :: coord(:,:,:)
END TYPE geo_grid

INTERFACE init
  MODULE PROCEDURE gg_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE gg_delete
END INTERFACE

INTERFACE alloc
  MODULE PROCEDURE gg_alloc
END INTERFACE

INTERFACE dealloc
  MODULE PROCEDURE gg_dealloc
END INTERFACE

INTERFACE set_2d_slice
  MODULE PROCEDURE gg_set_2d_slice
END INTERFACE

INTERFACE getval
  MODULE PROCEDURE gg_getval
END INTERFACE

INTERFACE setval
  MODULE PROCEDURE gg_setval
END INTERFACE

PRIVATE gg_adjust_coord, rtlld

CONTAINS

SUBROUTINE gg_init(this)
TYPE(geo_grid) :: this

this%proj = gg_proj_gen
this%fuso = imiss
this%elliss = imiss
this%nx = 0
this%ny = 0
this%x1 = rmiss
this%x2 = rmiss
this%y1 = rmiss
this%y2 = rmiss
this%dx = 1.0
this%dy = 1.0
this%nlev = 1
this%nvar = 1
this%ntim = 1
this%curlev = 1
this%curvar = 1
this%curtim = 1
NULLIFY(this%vcp, this%field2d, this%field5d, this%coord)

END SUBROUTINE gg_init


SUBROUTINE gg_delete(this)
TYPE(geo_grid),INTENT(INOUT) :: this

CALL gg_dealloc(this)
CALL gg_init(this)

END SUBROUTINE gg_delete


RECURSIVE SUBROUTINE gg_alloc(this, ier)
TYPE (geo_grid) :: this
INTEGER, INTENT(OUT) :: ier

INTEGER :: sh(5)
REAL(kind=fp_gg), POINTER :: tmpptr(:,:,:,:,:)

ier = 0
IF (this%nx <= 0 .OR. this%ny <= 0) THEN
  CALL raise_error('cannot allocate, nx and/or ny not set', 1, ier)
  RETURN
ENDIF
IF (ASSOCIATED(this%field5d)) THEN ! time/var/level extension
  tmpptr => this%field5d
  NULLIFY(this%field5d)
  sh = SHAPE(tmpptr)
  CALL gg_alloc(this, ier)
  IF (ier == 0) THEN
    this%field5d = gg_miss
    IF (sh(1) /= this%nx .OR. sh(2) /= this%ny .OR. sh(3) > this%nlev &
     .OR. sh(4) > this%nvar .OR. sh(5) > this%ntim) THEN
      CALL raise_warning('grid not extendible I discard the old data')
    ELSE
      this%field5d(:,:,1:sh(3),1:sh(4),1:sh(5)) = tmpptr(:,:,:,:,:)
    ENDIF
  ENDIF
  DEALLOCATE(tmpptr)
ELSE
  ALLOCATE(this%field5d(this%nx, this%ny, this%nlev, this%nvar, this%ntim), &
   STAT=ier)
  IF (ier /= 0) THEN
    CALL raise_error('cannot allocate '// &
     to_char(this%nx*this%ny*this%nlev*this%nvar*this%ntim)//' words')
    RETURN
  ENDIF
  CALL gg_adjust_coord(this)
ENDIF
CALL set_2d_slice(this) ! Assign field2d

END SUBROUTINE gg_alloc


SUBROUTINE gg_dealloc(this)
TYPE(geo_grid),INTENT(INOUT) :: this

IF (ASSOCIATED(this%field5d)) DEALLOCATE(this%field5d)
NULLIFY(this%field2d)
IF (ASSOCIATED(this%vcp)) DEALLOCATE(this%vcp)
IF (ASSOCIATED(this%coord)) DEALLOCATE(this%coord)

END SUBROUTINE gg_dealloc


SUBROUTINE gg_setval(this, nx, ny, nlev, nvar, ntim, proj, fuso, elliss, x1, x2, &
 y1, y2, dx, dy, xrot, yrot, rot)
TYPE (geo_grid) :: this
INTEGER, INTENT(IN), OPTIONAL :: nx, ny, nlev, nvar, ntim, proj, fuso, elliss
REAL, INTENT(IN), OPTIONAL :: x1, x2, &
 y1, y2, dx, dy, xrot, yrot, rot

IF (ASSOCIATED(this%field5d)) THEN ! Following members can only be extended
  IF (PRESENT(nlev)) THEN
    this%nlev = MAX(this%nlev, nlev)
  ENDIF
  IF (PRESENT(nvar)) THEN
    this%nvar = MAX(this%nvar, nvar)
  ENDIF
  IF (PRESENT(ntim)) THEN
    this%ntim = MAX(this%ntim, ntim)
  ENDIF
ELSE ! In deallocated state everything can be changed
  IF (PRESENT(nlev)) this%nlev = MAX(nlev, 1)
  IF (PRESENT(nvar)) this%nvar = MAX(nvar, 1)
  IF (PRESENT(ntim)) this%ntim = MAX(ntim, 1)
  IF (PRESENT(nx)) this%nx = MAX(nx, 0)
  IF (PRESENT(ny)) this%ny = MAX(ny, 0)
  IF (PRESENT(proj)) this%proj = proj
  IF (PRESENT(fuso)) this%fuso = fuso
  IF (PRESENT(elliss)) this%elliss = elliss
  IF (PRESENT(x1)) this%x1 = x1
  IF (PRESENT(x2)) this%x2 = x2
  IF (PRESENT(y1)) this%y1 = y1
  IF (PRESENT(y2)) this%y2 = y2
  IF (PRESENT(dx)) this%dx = dx
  IF (PRESENT(dy)) this%dy = dy
  IF (PRESENT(xrot)) this%xrot = xrot
  IF (PRESENT(yrot)) this%yrot = yrot
  IF (PRESENT(rot)) this%rot = rot
ENDIF

END SUBROUTINE gg_setval


SUBROUTINE gg_getval(this, nx, ny, nlev, nvar, ntim, proj, x1, x2, &
 y1, y2, dx, dy, xrot, yrot, rot, field2d, field5d, coord)
TYPE (geo_grid) :: this
INTEGER, INTENT(OUT), OPTIONAL :: nx, ny, nlev, nvar, ntim, proj
REAL, INTENT(OUT), OPTIONAL :: x1, x2, &
 y1, y2, dx, dy, xrot, yrot, rot
REAL(kind=fp_gg), POINTER, OPTIONAL :: field2d(:,:), field5d(:,:,:,:,:)
REAL, POINTER, OPTIONAL :: coord(:,:,:)


IF (PRESENT(nlev)) nlev = this%nlev
IF (PRESENT(nvar)) nvar = this%nvar
IF (PRESENT(ntim)) ntim = this%ntim
IF (PRESENT(nx)) nx = this%nx
IF (PRESENT(ny)) ny = this%ny
IF (PRESENT(proj)) proj = this%proj
IF (PRESENT(x1)) x1 = this%x1
IF (PRESENT(x2)) x2 = this%x2
IF (PRESENT(y1)) y1 = this%y1
IF (PRESENT(y2)) y2 = this%y2
IF (PRESENT(dx)) dx = this%dx
IF (PRESENT(dy)) dy = this%dy
IF (PRESENT(xrot)) xrot = this%xrot
IF (PRESENT(yrot)) yrot = this%yrot
IF (PRESENT(rot)) rot = this%rot

IF (PRESENT(field2d)) field2d => this%field2d
IF (PRESENT(field5d)) field5d => this%field5d
IF (PRESENT(coord)) coord => this%coord

END SUBROUTINE gg_getval


SUBROUTINE gg_set_2d_slice(this, nlev, nvar, ntim)
TYPE (geo_grid) :: this
INTEGER, INTENT(IN), OPTIONAL :: nlev, nvar, ntim

IF (PRESENT(nlev)) this%curlev = MAX(1,MIN(nlev,this%nlev))
IF (PRESENT(nvar)) this%curvar = MAX(1,MIN(nvar,this%nvar))
IF (PRESENT(ntim)) this%curtim = MAX(1,MIN(ntim,this%ntim))
IF (ASSOCIATED(this%field5d)) &
 this%field2d => this%field5d(:,:,this%curlev,this%curvar,this%curtim)

END SUBROUTINE gg_set_2d_slice


SUBROUTINE gg_adjust_coord(this)
TYPE (geo_grid) :: this

IF (this%proj == gg_proj_gen) THEN
  this%x1 = 1.0
  this%x2 = REAL(this%nx)
  this%dx = 1.0
  this%y1 = 1.0
  this%y2 = REAL(this%ny)
  this%dy = 1.0
ELSE
  IF (this%x1 == rmiss .AND. this%x2 == rmiss) THEN
    this%x1 = 1.0
    this%x2 = this%x1 + (this%nx - 1)*this%dx
  ELSE IF (this%x1 /= rmiss .AND. this%x2 /= rmiss .AND. this%nx > 1) THEN
    this%dx = (this%x2 - this%x1)/(this%nx - 1)
  ELSE IF (this%x2 == rmiss .AND. this%x1 /= rmiss) THEN
    this%x2 = this%x1 + (this%nx - 1)*this%dx
  ELSE IF (this%x1 == rmiss .AND. this%x2 /= rmiss) THEN
    this%x1 = this%x2 - (this%nx - 1)*this%dx
  ENDIF
  IF (this%y1 == rmiss .AND. this%y2 == rmiss) THEN
    this%y1 = 1.0
    this%y2 = this%y1 + (this%ny - 1)*this%dy
  ELSE IF (this%y1 /= rmiss .AND. this%y2 /= rmiss .AND. this%ny > 1) THEN
    this%dy = (this%y2 - this%y1)/(this%ny - 1)
  ELSE IF (this%y2 == rmiss .AND. this%y1 /= rmiss) THEN
    this%y2 = this%y1 + (this%ny - 1)*this%dy
  ELSE IF (this%y1 == rmiss .AND. this%y2 /= rmiss) THEN
    this%y1 = this%y2 - (this%ny - 1)*this%dy
  ENDIF
ENDIF

END SUBROUTINE gg_adjust_coord


SUBROUTINE gg_compute_coord(this)
TYPE (geo_grid) :: this

INTEGER :: i, j
REAL :: cosy, siny

IF (.NOT. ASSOCIATED(this%field5d)) CALL gg_adjust_coord(this)
IF (ASSOCIATED(this%coord)) DEALLOCATE(this%coord)
ALLOCATE(this%coord(this%nx,this%ny,2))
IF (this%proj == gg_proj_gen) THEN
  DO i = 1, this%nx
    this%coord(i,:,1) = REAL(i)
  ENDDO
  DO j = 1, this%ny
    this%coord(:,j,2) = REAL(j)
  ENDDO
ELSE
  DO i = 1, this%nx
    this%coord(i,:,1) = this%x1+(i-1)*this%dx ! make sure at this%nx = this%x2 ?
  ENDDO
  DO j = 1, this%ny
    this%coord(:,j,2) = this%y1+(j-1)*this%dy
  ENDDO
  IF (this%proj == gg_proj_georot) THEN
    cosy = COSD(this%yrot)
    siny = SIND(this%yrot)
    DO j = 1, this%ny
      DO i = 1, this%nx
        CALL rtlld(this%coord(i, j, 1), this%coord(i, j, 2), this%xrot, cosy, siny)
      ENDDO
    ENDDO
  ENDIF
ENDIF

END SUBROUTINE gg_compute_coord


SUBROUTINE rtlld(x, y, x0, cy0, sy0)
REAL, INTENT(inout) :: x, y
REAL, INTENT(in) :: x0, cy0, sy0

REAL :: sx, sy, cx, cy

sx = SIND(x)
cx = COSD(x)
sy = SIND(y)
cy = COSD(y)

y = ASIND(sy0*cy*cx+cy0*sy)
x = x0 + ASIND(sx*cy/COSD(y))

END SUBROUTINE rtlld


SUBROUTINE gg_inside(this, poly, spoly, mpoly)
TYPE(geo_grid) :: this
TYPE(geo_coordvect), INTENT(IN) :: poly(:)
INTEGER, POINTER, OPTIONAL :: spoly(:,:)
LOGICAL, POINTER, OPTIONAL :: mpoly(:,:,:)

INTEGER :: i, j, p
TYPE(geo_coord) :: point

IF (PRESENT(spoly)) NULLIFY(spoly)
IF (PRESENT(mpoly)) NULLIFY(mpoly)
CALL gg_compute_coord(this)
IF (.NOT. ASSOCIATED(this%coord)) RETURN
IF (PRESENT(spoly)) THEN
  ALLOCATE(spoly(this%nx, this%ny))
  spoly(:,:) = 0
  DO j = 1, this%ny
    DO i = 1, this%nx
      IF (this%proj == gg_proj_utm) THEN
        CALL init(point, utme=REAL(this%coord(i,j,1),kind=fp_utm), &
         utmn=REAL(this%coord(i,j,2),kind=fp_utm), &
         fuso=this%fuso, elliss=this%elliss)
      ELSE
        CALL init(point, lon=REAL(this%coord(i,j,1),kind=fp_geo), &
         lat=REAL(this%coord(i,j,2),kind=fp_geo))
      ENDIF
      DO p = 1, SIZE(poly)
        IF (geo_coord_inside(point, poly(p))) THEN
          spoly(i,j) = p
          CYCLE
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ELSE IF (PRESENT(mpoly)) THEN
  ALLOCATE(mpoly(this%nx, this%ny, SIZE(poly)))
  mpoly(:,:,:) = .FALSE.
  DO j = 1, this%ny
    DO i = 1, this%nx
      IF (this%proj == gg_proj_utm) THEN
        CALL init(point, utme=REAL(this%coord(i,j,1),kind=fp_utm), &
         utmn=REAL(this%coord(i,j,2),kind=fp_utm), &
         fuso=this%fuso, elliss=this%elliss)
      ELSE
        CALL init(point, lon=REAL(this%coord(i,j,1),kind=fp_geo), &
         lat=REAL(this%coord(i,j,2),kind=fp_geo))
      ENDIF
      DO p = 1, SIZE(poly)
        mpoly(i,j,p) = geo_coord_inside(point, poly(p))
      ENDDO
    ENDDO
  ENDDO
ENDIF

END SUBROUTINE gg_inside


! Transformation methods acting only on one field at a time
! and not on grid description

SUBROUTINE gg_filter5p(this, times)
TYPE(geo_grid), INTENT(INOUT) :: this
INTEGER, INTENT(in), OPTIONAL :: times

INTEGER :: i, j, im1, ip1, jm1, jp1, l, ltimes, sh(2)
REAL(kind=fp_gg), ALLOCATABLE :: tmpbuff(:,:)

IF (PRESENT(times)) THEN
  ltimes = times
ELSE
  ltimes = 1
ENDIF
IF (ASSOCIATED(this%field2d) .AND. ltimes > 0) THEN
  sh = SHAPE(this%field2d)
  ALLOCATE(tmpbuff(sh(1), sh(2)))

  DO l = 1, ltimes
    DO j = 1, sh(2)
      jm1=MAX(j-1,1)
      jp1=MIN(j+1,sh(2))
      DO i = 1, sh(1)
        im1=MAX(i-1,1)
        ip1=MIN(i+1,sh(1))
        tmpbuff(i,j) = 0.2_fp_gg*(this%field2d(i,j) + &
         this%field2d(im1,j) + &
         this%field2d(ip1,j) + &
         this%field2d(i,jm1) + &
         this%field2d(i,jp1))
      ENDDO
    ENDDO
  ENDDO
  this%field2d = tmpbuff

  DEALLOCATE(tmpbuff)
ENDIF

END SUBROUTINE gg_filter5p


SUBROUTINE gg_rescale(this, convm, convs)
TYPE(geo_grid), INTENT(INOUT) :: this
REAL(kind=fp_gg), INTENT(in) :: convm, convs

IF (convm /= 1. .OR. convs /= 0.) THEN ! Convert
  WHERE(this%field2d(:,:) /= gg_miss)
    this%field2d(:,:) = convs + &
     this%field2d(:,:)*convm
  END WHERE
ENDIF

END SUBROUTINE gg_rescale


! Transformation methods acting on the whole grid
! including grid description => involve reallocating field5d

SUBROUTINE gg_cut(this, i1, i2, j1, j2, ier)
TYPE(geo_grid), INTENT(INOUT) :: this
INTEGER, INTENT(IN) :: i1, i2 , j1, j2
INTEGER, INTENT(OUT) :: ier

INTEGER :: sh(5)
REAL(kind=fp_gg), POINTER :: tmp5d(:,:,:,:,:)

IF (i1 < 0 .OR. i2 < 0 .OR. j1 < 0 .OR. j2 < 0) THEN
  CALL raise_Error('invalid cut parameters', 1, ier)
  RETURN
ENDIF
IF (i1+i2 > this%nx .OR. j1+j2 > this%ny) THEN
  CALL raise_error('cut bigger than actual size of area', 1, ier)
  RETURN
ENDIF

sh = SHAPE(this%field5d)
IF (sh(1) /= this%nx .OR. sh(2) /= this%ny) THEN
  CALL raise_error('internal grid error', 3, ier)
  RETURN
ENDIF
ALLOCATE(tmp5d(sh(1)-(i1+i2), sh(2)-(j1+j2), sh(3), sh(4), sh(5)))
tmp5d(:,:,:,:,:) = this%field5d(i1+1:sh(1)-i2, j1+1:sh(2)-j2, :, :, :)

DEALLOCATE(this%field5d)
this%field5d => tmp5d
! Adjust the grid description
this%nx = this%nx-(i1+i2)
this%ny = this%ny-(j1+j2)
this%x1 = this%x1+i1*this%dx
this%x2 = this%x2-i2*this%dx
this%y1 = this%y1+j1*this%dy
this%y2 = this%y2-j2*this%dy
CALL set_2d_slice(this) ! Assign field2d

END SUBROUTINE gg_cut


SUBROUTINE gg_regrid(this, ngx, ngy, ier)
TYPE(geo_grid), INTENT(INOUT) :: this
INTEGER, INTENT(IN) :: ngx, ngy
INTEGER, INTENT(OUT) :: ier

INTEGER :: i, j, ie, je, ii, jj, navg, l3, l4, l5
INTEGER :: sh(5)
REAL(kind=fp_gg), POINTER :: tmp5d(:,:,:,:,:)

! Sanity checks
IF (ngx <= 0 .OR. ngy <= 0 .OR. ngx > this%nx .OR. ngy > this%ny) THEN
  CALL raise_Error('invalid regrid parameters', 1, ier)
  RETURN
ENDIF
IF (ngx == 1 .AND. ngy == 1) RETURN ! Nothing to do

sh = SHAPE(this%field5d)
IF (sh(1) /= this%nx .OR. sh(2) /= this%ny) THEN
  CALL raise_error('internal grid error', 3, ier)
  RETURN
ENDIF
ALLOCATE(tmp5d(sh(1)/ngx, sh(2)/ngy, sh(3), sh(4), sh(5)))

DO l5 = 1, sh(5)
  DO l4 = 1, sh(4)
    DO l3 = 1, sh(3)
      ii = 0
      jj = 0
      DO j = 1, this%ny-ngy+1, ngy
        je = j+ngy-1
        jj = jj+1
        DO i = 1, this%nx-ngx+1, ngx
          ie = i+ngx-1
          ii = ii+1
          navg = COUNT(this%field5d(i:ie,j:je,l3,l4,l5) /= gg_miss)
          IF (navg > 0) THEN
            tmp5d(ii,jj,l3,l4,l5) = SUM(this%field5d(i:ie,j:je,l3,l4,l5), &
             MASK=(this%field5d(i:ie,j:je,l3,l4,l5) /= gg_miss))/navg
          ELSE
            tmp5d(ii,jj,l3,l4,l5) = gg_miss
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

DEALLOCATE(this%field5d)
this%field5d => tmp5d
! Adjust the grid description
this%x1 = this%x1+(ngx-1)*0.5*this%dx
this%y1 = this%y1+(ngy-1)*0.5*this%dy
this%nx = this%nx/ngx
this%ny = this%ny/ngy
this%dx = this%dx*ngx
this%dy = this%dy*ngy
this%x2 = this%x1+(this%nx-1)*this%dx
this%y2 = this%y1+(this%ny-1)*this%dy
CALL set_2d_slice(this) ! Assign field2d

END SUBROUTINE gg_regrid

END MODULE geo_grid_class
