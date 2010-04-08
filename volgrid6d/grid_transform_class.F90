#include "config.h"
!> Module for defining transformations between rectangular
!! georeferenced grids and between grids and sparse points and
!! vice-versa. The module defines two classes: \a transform_def which
!! describes an 'abstract' transformation between grids and/or sparse
!! points, and \a grid_transform which includes a \a transform_def
!! object and describes a transformation between specific grids and/or
!! sets of sparse points, with precomputed coefficients specific to
!! the grids/sparse points involved. A single \a transform_def object
!! can be used for defining different \a grid_transform objects which
!! apply the same transformation to different sets of grids/sparse
!! points pairs. In the same manner, a single \a grid_transform object
!! can be used for interpolating any number of fields lying on the
!! same grids/sparse points pair, thus recycling the interpolation
!! coefficients which are computed only once at the time of defining
!! the \a grid_transform object.
!!
!! Different abstract transformations are supported, defined by the
!! parameter \a trans_type, and its corresponding \a sub_type:
!!
!!  - trans_type='zoom' cuts or extends the input grid on a new one
!!    adding or removing points on the four sides (grid-to-grid only)
!!    - sub_type='coord' the bounds of the zoomed/extended area are
!!      defined by geographical coordinates
!!    - sub_type='index' the bounds of the zoomed/extended area are
!!      defined by grid indices
!!  - trans_type='boxregrid' regrids the input data grid on a new grid
!!    in which the value at every point is a the result of a function
!!    computed over \a npx X \a npy points of the original grid
!!    (grid-to-grid only)
!!    - sub_type='average' the function used is the average
!!  - trans_type='inter' interpolates the input data on a new set of points
!!    - sub_type='near' the interpolated value is that of the nearest
!!      input point (grid-to-grid, grid-to-sparse point)
!!    - sub_type='bilin' the interpolated value is computed as a
!!      bilinear interpolation of the 4 surrounding input points
!!      (grid-to-grid, grid-to-sparse point)
!!    - sub_type='linear' the interpolated value is computed as a
!!      linear interpolation of the 3 surrounding input points
!!      individuated by means of a triangulation procedure (sparse
!!      points-to-grid)
!!  - trans_type='boxinter' computes data on a new grid in which the
!!    value at every point is the result of a function computed over
!!    those input points that lie into the output point grid box
!!    (grid-to-grid and sparse points-to-grid)
!!    - sub_type='average' the function used is the average
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!    - sub_type='percentile' the function used is a requested
!!      percentile of the input points distribution.
!!
!! \ingroup volgrid6d
MODULE grid_transform_class
USE vol7d_class
USE err_handling
USE grid_class
USE optional_values
USE simple_stat
IMPLICIT NONE

CHARACTER(len=255),PARAMETER:: subcategory="grid_transform_class"

! subtype nearest information
type inter_near
  logical :: external ! enable elaboration outside data bounding box
end type inter_near

! subtype bilinear information
type inter_bilin
  logical :: external ! enable elaboration outside data bounding box
end type inter_bilin

! subtype linear information
type inter_linear
  logical :: external ! enable elaboration outside data bounding box
end type inter_linear

! subtype box information
type inter_box
  double precision :: boxdx ! longitudinal/x extension of the box for box interpolation, default the target x grid step
  double precision :: boxdy ! latitudinal/y extension of the box for box interpolation, default the target y grid step
  double precision :: boxpercentile ! percentile [0,100] of the distribution of points in the box to use as interpolated value, if missing, the average is used
  logical :: external ! enable elaboration outside data bounding box
end type inter_box

! interpolation information 
type inter
  CHARACTER(len=80) :: sub_type ! subtype of transformation, can be \c 'near', \c 'bilin', \c 'linear', \c 'box'
  type(inter_near) :: near ! subtype nearest information
  type(inter_bilin) :: bilin ! subtype bilinear information
  type(inter_linear) :: linear ! subtype linear information
  type(inter_box) :: box ! subtype box information
end type inter

! zoom subtype index information
type zoom_ind
  INTEGER :: ix ! index of initial point of new grid on x
  INTEGER :: iy ! index of initial point of new grid on y
  INTEGER :: fx ! index of final point of new grid on x
  INTEGER :: fy ! index of final point of new grid on y
end type zoom_ind

! zoom subtype coord information
type zoom_coo
  DOUBLEPRECISION ilon ! coordinate of initial point of new grid on x
  DOUBLEPRECISION ilat ! coordinate of initial point of new grid on y
  DOUBLEPRECISION flon ! coordinate of final point of new grid on x
  DOUBLEPRECISION flat ! coordinate of final point of new grid on y
end type zoom_coo

! zoom information
type zoom
  CHARACTER(len=80) :: sub_type ! subtype of transformation, can be \c 'index', \c 'coord'
  type(zoom_ind) :: index ! zoom providing index
  type(zoom_coo) :: coord ! zoom providing coordinates
end type zoom

! boxregrid subtype average information
!type boxregrid_average
!
!end type boxregrid_average
! no extra information needed now

! boxregrid  information
type boxregrid
  CHARACTER(len=80) :: sub_type ! subtype of transformation, can be \c 'average'
  INTEGER :: npx ! number of points to average along x direction
  INTEGER :: npy ! number of points to average along y direction
!  type(boxregrid_average) :: average
end type boxregrid

! Vertical interpolation information.
! The input vertical coordinate can be indicated either as the value
! of the vertical level (so that it will be the same on every point
! at a given vertical level), or as the value of a specified variable
! at each point in space (so that it will depend on the horizontal
! position too).
TYPE vertint
  CHARACTER(len=80) :: sub_type ! subtype of transformation, can be \c 'linear'
  TYPE(vol7d_level) :: input_levtype ! type of vertical level of input data (only type of first and second surface are used, level values are ignored)
  TYPE(vol7d_var) :: input_coordvar ! variable that defines the vertical coordinate in the input volume, if missing, the value of the vertical level is used
  TYPE(vol7d_level) :: output_levtype ! type of vertical level of output data (only type of first and second surface are used, level values are ignored)
END TYPE vertint

!> This object defines the type of transformation to be applied.
!! It does not carry specific information about the grid to which it
!! will be applied, so the same instance can be reused for
!! transforming in the same way different grids.
TYPE transform_def
  private

  CHARACTER(len=80) :: trans_type ! type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'inter', \c 'vertint' ...
  type(zoom) :: zoom ! zoom specification
  type(boxregrid) :: boxregrid ! boxregrid specification
  type(inter) :: inter ! interpolation specification
  type(vertint) :: vertint ! vertical interpolation specification
  integer :: time_definition ! time definition for interpolating to sparse points
  integer :: category ! category for log4fortran

END TYPE transform_def


!> This object fully defines a transformation between a couple of
!! particular \a griddim_def or \a vol7d objects (any combination is
!! possible). It carries information about the objects' mutual
!! coordinates in order to speed up the transformation when it has to
!! be repeated on objects having the same coordinates and grid
!! projections.
TYPE grid_transform
  private

  TYPE(transform_def) :: trans ! type of transformation required

  integer :: innx,  inny
  integer :: outnx, outny
  integer :: iniox,inioy,infox,infoy,outinx,outiny,outfnx,outfny
  integer,pointer :: inter_index_x(:,:),inter_index_y(:,:)
  doubleprecision,pointer :: inter_x(:,:),inter_y(:,:)
  doubleprecision,pointer :: inter_xp(:,:),inter_yp(:,:)
!  type(volgrid6d) :: input_vertcoordvol ! volume which provides the input vertical coordinate if separated from the data volume itself (for vertint) cannot be here because of cross-use, should be an argument of compute
!  type(vol7d_level), pointer :: output_vertlevlist(:) ! list of vertical levels of output data (for vertint) can be here or an argument of compute, how to do?
  integer :: category ! category for log4fortran

END TYPE grid_transform


!> Constructors of the corresponding objects.
INTERFACE init
  MODULE PROCEDURE transform_init, grid_transform_init, &
   grid_transform_grid_vol7d_init, grid_transform_vol7d_grid_init
END INTERFACE

!> Destructors of the corresponding objects.
INTERFACE delete
  MODULE PROCEDURE transform_delete, grid_transform_delete
END INTERFACE

!> Method for returning the contents of the object.
INTERFACE get_val
  MODULE PROCEDURE transform_get_val
END INTERFACE

!> Compute the output data array from input data array according to
!! the defined transformation.
INTERFACE compute
  MODULE PROCEDURE grid_transform_compute, grid_transform_v7d_grid_compute
END INTERFACE

PRIVATE
PUBLIC init, delete, get_val, compute
PUBLIC transform_def, grid_transform

CONTAINS


!> Constructor for a \a transform_def object, defining an abstract
!! transformation between gridded and/or sparse point data.  The
!! parameters \a trans_type and \a sub_type define the type of
!! transformation, while all the other following parameters are
!! optional, they have to be passed in keyword mode and those required
!! by the transformation type and subtype chosen have to be present.
SUBROUTINE transform_init(this, trans_type, sub_type, &
 ix, iy, fx, fy, ilon, ilat, flon, flat, &
 npx, npy, boxdx, boxdy, boxpercentile, &
 external, time_definition, &
 input_levtype, input_coordvar, output_levtype, categoryappend)
TYPE(transform_def),INTENT(out) :: this !< transformation object
CHARACTER(len=*) :: trans_type !< type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'interp', \c 'vertint' ...
CHARACTER(len=*) :: sub_type !< sub type of transformation, it depends on \a trans_type
INTEGER,INTENT(in),OPTIONAL :: ix !< index of initial point of new grid on x (for zoom)
INTEGER,INTENT(in),OPTIONAL :: iy !< index of initial point of new grid on y (for zoom)
INTEGER,INTENT(in),OPTIONAL :: fx !< index of final point of new grid on x (for zoom)
INTEGER,INTENT(in),OPTIONAL :: fy !< index of final point of new grid on y (for zoom)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: ilon !< coordinate of initial point of new grid on x (for zoom)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: ilat !< coordinate of initial point of new grid on y (for zoom)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: flon !< coordinate of final point of new grid on x (for zoom)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: flat !< coordinate of final point of new grid on y (for zoom)
INTEGER,INTENT(IN),OPTIONAL :: npx !< number of points to average along x direction (for boxregrid)
INTEGER,INTENT(IN),OPTIONAL :: npy !< number of points to average along y direction (for boxregrid)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: boxdx !< longitudinal/x extension of the box for box interpolation, default the target x grid step (unimplemented !)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: boxdy !< latitudinal/y extension of the box for box interpolation, default the target y grid step (unimplemented !)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: boxpercentile !< percentile [0,1] of the distribution of points in the box to use as interpolated value, if missing, the average is used
LOGICAL,INTENT(IN),OPTIONAL :: external !< activate external area interpolation (for interpolation) (unimplemented !)
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< time definition for output vol7d object 0=time is reference time ; 1=time is validity time
TYPE(vol7d_level),INTENT(IN),OPTIONAL :: input_levtype !< type of vertical level of input data to be vertically interpolated (only type of first and second surface are used, level values are ignored)
TYPE(vol7d_var),INTENT(IN),OPTIONAL :: input_coordvar !< variable that defines the vertical coordinate in the input volume for vertical interpolation, if missing, the value of the vertical level defined with \a input_levtype is used
TYPE(vol7d_level),INTENT(IN),OPTIONAL :: output_levtype !< type of vertical level to which data should be vertically interpolated (only type of first and second surface are used, level values are ignored)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< suffix to append to log4fortran namespace category
character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

call optio(trans_type,this%trans_type)

if (trans_type == "zoom") call optio(sub_type,this%zoom%sub_type)

call optio(ix,this%zoom%index%ix)
call optio(iy,this%zoom%index%iy)
call optio(fx,this%zoom%index%fx)
call optio(fy,this%zoom%index%fy)

call optio(ilon,this%zoom%coord%ilon)
call optio(ilat,this%zoom%coord%ilat)
call optio(flon,this%zoom%coord%flon)
call optio(flat,this%zoom%coord%flat)

call optio(time_definition,this%time_definition)
if (c_e(this%time_definition) .and. &
 (this%time_definition < 0 .OR. this%time_definition > 1))THEN
  call l4f_category_log(this%category,L4F_ERROR,"Error in time_definition: "//to_char(this%time_definition))
  call raise_fatal_error()
end if

if (trans_type == "boxregrid") call optio(sub_type,this%boxregrid%sub_type)

call optio(npx,this%boxregrid%npx)
call optio(npy,this%boxregrid%npy)

if (trans_type == "inter") call optio(sub_type,this%inter%sub_type)

call optio(external,this%inter%near%external)
call optio(external,this%inter%bilin%external)
call optio(external,this%inter%linear%external)


IF (this%trans_type == 'zoom') THEN

  if (this%zoom%sub_type == 'coord')then

    if (c_e(this%zoom%coord%ilon) .and. c_e(this%zoom%coord%ilat) .and. &
        c_e(this%zoom%coord%flon) .and. c_e(this%zoom%coord%flat)) then ! coordinates given
    
!check
      if ( this%zoom%coord%ilon > this%zoom%coord%flon .or. &
       this%zoom%coord%ilat > this%zoom%coord%flat ) then

        call l4f_category_log(this%category,L4F_ERROR, &
         "invalid zoom coordinates: ")
        call l4f_category_log(this%category,L4F_ERROR, &
         TRIM(to_char(this%zoom%coord%ilon))//'/'// &
         TRIM(to_char(this%zoom%coord%flon)))
        call l4f_category_log(this%category,L4F_ERROR, &
         TRIM(to_char(this%zoom%coord%ilat))//'/'// &
         TRIM(to_char(this%zoom%coord%flat)))
        call raise_fatal_error()
      end if

    else

      call l4f_category_log(this%category,L4F_ERROR,"zoom: coord parameters missing")
      call raise_fatal_error()
        
    end if

  else if (this%zoom%sub_type == 'index')then

    if (c_e(this%zoom%index%ix) .and. c_e(this%zoom%index%iy) .or. &
        c_e(this%zoom%index%fx) .or. c_e(this%zoom%index%fy)) then

! check
      if (this%zoom%index%ix > this%zoom%index%fx .OR. &
       this%zoom%index%iy > this%zoom%index%fy) THEN

        CALL l4f_category_log(this%category,L4F_ERROR,'invalid zoom indices: ')
        CALL l4f_category_log(this%category,L4F_ERROR, &
         TRIM(to_char(this%zoom%index%ix))//'/'// &
         TRIM(to_char(this%zoom%index%fx)))
        CALL l4f_category_log(this%category,L4F_ERROR, &
         TRIM(to_char(this%zoom%index%iy))//'/'// &
         TRIM(to_char(this%zoom%index%fy)))

        CALL raise_fatal_error()
      ENDIF

    else

      CALL l4f_category_log(this%category,L4F_ERROR,&
       'zoom: index parameters ix, iy, fx, fy not provided')
      CALL raise_fatal_error()

    ENDIF

  else

    CALL l4f_category_log(this%category,L4F_ERROR,'zoom: sub_type '// &
     TRIM(this%zoom%sub_type)//' is wrong')
    CALL raise_fatal_error()

  end if


ELSE IF (this%trans_type == 'boxregrid') THEN

  IF (c_e(this%boxregrid%npx) .AND. c_e(this%boxregrid%npy)) THEN

    IF (this%boxregrid%npx <= 0 .OR. this%boxregrid%npy <= 0 ) THEN
      CALL l4f_category_log(this%category,L4F_ERROR,'invalid regrid parameters: '//&
       TRIM(to_char(this%boxregrid%npx))//' '//TRIM(to_char(this%boxregrid%npy)))
      CALL raise_fatal_error()
    ENDIF

  ELSE

    CALL l4f_category_log(this%category,L4F_ERROR, &
     'boxregrid parameters npx, npy not provided')
    CALL raise_fatal_error()

  ENDIF

  IF (this%boxregrid%sub_type == 'average')THEN
! nothing to do here
  ELSE
    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid: sub_type '// &
     TRIM(this%boxregrid%sub_type)//' is wrong')
    CALL raise_fatal_error()
  ENDIF

ELSE IF (this%trans_type == 'inter') THEN

  if (this%inter%sub_type == 'near')then
! nothing to do here
  else if (this%inter%sub_type == 'bilin')then
! nothing to do here
  else if (this%inter%sub_type == 'linear')then
! nothing to do here
  else
    CALL l4f_category_log(this%category,L4F_ERROR,'inter: sub_type '// &
     TRIM(this%inter%sub_type)//' is wrong')
    CALL raise_fatal_error()
  endif

ELSE IF (this%inter%sub_type == 'boxinter')THEN

  CALL optio(boxdx,this%inter%box%boxdx) ! unused
  CALL optio(boxdy,this%inter%box%boxdy) ! now

  IF (this%inter%sub_type == 'average') THEN
! nothing to do here
  ELSE IF (this%inter%sub_type == 'max') THEN
! nothing to do here
  ELSE IF (this%inter%sub_type == 'min') THEN
! nothing to do here
  ELSE IF (this%inter%sub_type == 'percentile') THEN
    CALL optio(boxpercentile,this%inter%box%boxpercentile)
  ELSE
    CALL l4f_category_log(this%category,L4F_ERROR,'boxinter: sub_type '// &
     TRIM(this%inter%sub_type)//' is wrong')
    CALL raise_fatal_error()
  ENDIF

ELSE IF (this%trans_type == 'vertint') THEN

  CALL optio(sub_type,this%vertint%sub_type)

  IF (PRESENT(input_levtype)) THEN
    this%vertint%input_levtype = input_levtype
  ELSE
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'vertint parameter input_levtype not provided')
    CALL raise_fatal_error()
  ENDIF

  IF (PRESENT(input_coordvar)) THEN
    this%vertint%input_coordvar = input_coordvar
  ELSE
    this%vertint%input_coordvar = vol7d_var_miss
  ENDIF

  IF (PRESENT(output_levtype)) THEN
    this%vertint%output_levtype = output_levtype
  ELSE
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'vertint parameter output_levtype not provided')
    CALL raise_fatal_error()
  ENDIF

ELSE

  CALL l4f_category_log(this%category,L4F_ERROR,'trans_type '// &
   TRIM(this%trans_type)//' is wrong')
  CALL raise_fatal_error()

ENDIF


END SUBROUTINE transform_init


!> Destructor of \a tranform_def object.
!! It releases any memory and data associated to the \a transform_def
!! object \a this, the logger category will be deleted too.
SUBROUTINE transform_delete(this)
TYPE(transform_def),INTENT(out) :: this !< transformation object

this%trans_type=cmiss

this%zoom%sub_type=cmiss

this%zoom%index%ix=imiss
this%zoom%index%iy=imiss
this%zoom%index%fx=imiss
this%zoom%index%fy=imiss

this%zoom%coord%ilon=dmiss
this%zoom%coord%ilat=dmiss
this%zoom%coord%flon=dmiss
this%zoom%coord%flat=dmiss

this%boxregrid%sub_type=cmiss

this%boxregrid%npx=imiss
this%boxregrid%npy=imiss

this%inter%sub_type=cmiss

this%inter%near%external=.false.
this%inter%bilin%external=.false.
this%inter%linear%external=.false.

!chiudo il logger
call l4f_category_delete(this%category)

END SUBROUTINE transform_delete


!> Method for returning the contents of the object.
SUBROUTINE transform_get_val(this, time_definition)
type(transform_def),intent(in) :: this !< object to examine
INTEGER,INTENT(out),OPTIONAL :: time_definition !< 0=time is reference time, 1=time is validity time

if ( present(time_definition)) time_definition=this%time_definition

END SUBROUTINE transform_get_val


!> Constructor for a \a grid_transform object, defining a particular
!! grid-to-grid transformation.
!! It defines an object describing a transformation from one
!! rectangular grid to another; the abstract type of transformation is
!! described in the transformation object \a trans (type
!! transform_def) which must have been properly initialised. The
!! additional information required here is the description of the
!! input grid \a in (type griddim_def), the description of the output
!! grid \a out (type griddim_def as well). The description of the
!! output grid must be initialized for interpolating type
!! transformations ('inter' and 'boxinter'), while it is generated by
!! this constructor and returned in output for 'zoom' and 'boxregrid'
!! transformations. The generated \a grid_transform object is specific
!! to the input and output grids involved.
SUBROUTINE grid_transform_init(this,trans,in,out,categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(inout) :: in !< griddim object to transform
TYPE(griddim_def),INTENT(inout) :: out !< griddim object defining target grid (input or output depending on type of transformation)
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: nx, ny, i, j
DOUBLE PRECISION :: lon_min, lon_max, lat_min, lat_max, steplon, steplat, &
 lon_min_new, lat_min_new
character(len=512) :: a_name
doubleprecision :: l1, l2

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start init_grid_transform")
#endif

this%trans=trans

nullify (this%inter_index_x)
nullify (this%inter_index_y)

nullify (this%inter_x)
nullify (this%inter_y)

nullify (this%inter_xp)
nullify (this%inter_yp)

IF (this%trans%trans_type == 'zoom') THEN

  if (this%trans%zoom%sub_type == 'coord') THEN

    CALL griddim_zoom_coord(in, &
     this%trans%zoom%coord%ilon, this%trans%zoom%coord%ilat,&
     this%trans%zoom%coord%flon, this%trans%zoom%coord%flat,&
     this%trans%zoom%index%ix, this%trans%zoom%index%iy, &
     this%trans%zoom%index%fx, this%trans%zoom%index%fy)

!    this%trans%zoom%sub_type = 'index'

  ELSE IF (this%trans%zoom%sub_type == 'index') THEN
! nothing particular to do
  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_grid_transform zoom sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    RETURN
    
  end if
! to do in all zoom cases
  
  CALL get_val(in, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
   lat_min=lat_min, lat_max=lat_max, dx=steplon, dy=steplat)

! old indices
  this%iniox = min(max(this%trans%zoom%index%ix,1),nx) ! iox
  this%inioy = min(max(this%trans%zoom%index%iy,1),ny) ! ioy
  this%infox = max(min(this%trans%zoom%index%fx,nx),1) ! fox
  this%infoy = max(min(this%trans%zoom%index%fy,ny),1) ! foy
! new indices
  this%outinx = min(max(2-this%trans%zoom%index%ix,1),nx) ! inx
  this%outiny = min(max(2-this%trans%zoom%index%iy,1),ny) ! iny
  this%outfnx = min(this%trans%zoom%index%fx,nx)-this%trans%zoom%index%ix+1 ! fnx
  this%outfny = min(this%trans%zoom%index%fy,ny)-this%trans%zoom%index%iy+1 ! fny

  lon_min=lon_min+steplon*(this%trans%zoom%index%ix-1)
  lat_min=lat_min+steplat*(this%trans%zoom%index%iy-1)
  lon_max=lon_max+steplon*(this%trans%zoom%index%fx-nx)
  lat_max=lat_max+steplat*(this%trans%zoom%index%fy-ny)

  call copy (in,out)

  out%dim%nx = this%trans%zoom%index%fx - this%trans%zoom%index%ix + 1 ! newx
  out%dim%ny = this%trans%zoom%index%fy - this%trans%zoom%index%iy + 1 ! newy

  this%innx = nx
  this%inny = ny

  this%outnx=out%dim%nx
  this%outny=out%dim%ny

  call set_val (out,&
   lon_min = lon_min,  lon_max = lon_max, &
   lat_min = lat_min,  lat_max = lat_max )

ELSE IF (this%trans%trans_type == 'boxregrid') THEN

  IF (this%trans%boxregrid%sub_type == 'average') THEN

    CALL get_val(in, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
     lat_min=lat_min, lat_max=lat_max, dx=steplon, dy=steplat)

    this%innx = nx
    this%inny = ny

! new grid
    lon_min_new = lon_min + (this%trans%boxregrid%npx - 1)*0.5D0*steplon
    lat_min_new = lat_min + (this%trans%boxregrid%npy - 1)*0.5D0*steplat

    CALL l4f_category_log(this%category,L4F_DEBUG,"copying griddim in out")
    call copy(in, out)
    out%dim%nx = nx/this%trans%boxregrid%npx
    out%dim%ny = ny/this%trans%boxregrid%npy

    this%outnx=out%dim%nx
    this%outny=out%dim%ny
    steplon = steplon*this%trans%boxregrid%npx
    steplat = steplat*this%trans%boxregrid%npy

    CALL set_val(out, lon_min=lon_min_new, lat_min=lat_min_new, &
     lon_max=lon_min_new + DBLE(out%dim%nx-1)*steplon, dx=steplon, &
     lat_max=lat_min_new + DBLE(out%dim%ny-1)*steplat, dy=steplat)

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_grid_transform boxregrid sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    
  ENDIF
  
ELSE IF (this%trans%trans_type == 'inter') THEN

! set increments in new grid in order for all the baraque to work
  CALL griddim_setsteps(out, out%dim%nx, out%dim%ny)
! set output component_flag equal to input
  CALL get_val(in, component_flag=i)
  CALL set_val(out, component_flag=i)

  IF (this%trans%inter%sub_type == 'near' .OR. this%trans%inter%sub_type == 'bilin' ) THEN
    
    CALL get_val(in, nx=this%innx, ny=this%inny, &
     lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max)
    CALL get_val(out, nx=this%outnx, ny=this%outny)
  
    ALLOCATE(this%inter_index_x(this%outnx,this%outny), &
     this%inter_index_y(this%outnx,this%outny))

    CALL find_index(in,this%trans%inter%sub_type,&
     nx=this%innx, ny=this%inny ,&
     lon_min=lon_min, lon_max=lon_max,&
     lat_min=lat_min, lat_max=lat_max,&
     lon=out%dim%lon,lat=out%dim%lat,&
     index_x=this%inter_index_x,index_y=this%inter_index_y)

    IF ( this%trans%inter%sub_type == 'bilin' ) THEN
      ALLOCATE(this%inter_x(this%innx,this%inny), &
       this%inter_y(this%innx,this%inny))
      ALLOCATE(this%inter_xp(this%outnx,this%outny), &
       this%inter_yp(this%outnx,this%outny))

! compute coordinates of input grid
      CALL griddim_gen_coord(in, this%inter_x, this%inter_y)
! TODO chi mi garantisce che e` stata chiamata la unproj(out)?
! compute coordinates of output grid in input system
      CALL proj(in,out%dim%lon,out%dim%lat,this%inter_xp,this%inter_yp)

    ENDIF
  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    
  ENDIF

ELSE IF (this%trans%trans_type == 'boxinter') THEN

  CALL get_val(in, nx=this%innx, ny=this%inny)
  CALL get_val(out, nx=this%outnx, ny=this%outny, &
   lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max)
! TODO now box size is ignored
! if box size not provided, use the actual grid step
  IF (.NOT.c_e(this%trans%inter%box%boxdx)) &
   CALL get_val(out, dx=this%trans%inter%box%boxdx)
  IF (.NOT.c_e(this%trans%inter%box%boxdy)) &
   CALL get_val(out, dx=this%trans%inter%box%boxdy)
! half size is actually needed
  this%trans%inter%box%boxdx = this%trans%inter%box%boxdx*0.5D0
  this%trans%inter%box%boxdy = this%trans%inter%box%boxdy*0.5D0
! unlike before, here index arrays must have the shape of input grid
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))

! compute coordinates of input grid in geo system
  CALL unproj(in) ! TODO costringe a dichiarare in INTENT(inout), si puo` evitare?
! use find_index in the opposite way as before
  CALL find_index(out,'near',&
   nx=this%outnx, ny=this%outny ,&
   lon_min=lon_min, lon_max=lon_max,&
   lat_min=lat_min, lat_max=lat_max,&
   lon=in%dim%lon, lat=in%dim%lat,&
   index_x=this%inter_index_x, index_y=this%inter_index_y)

ELSE

  CALL l4f_category_log(this%category,L4F_WARN, &
   'init_grid_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')

ENDIF

END SUBROUTINE grid_transform_init


!> Constructor for a \a grid_transform object, defining a particular
!! grid-to-sparse points transformation.
!! It defines an object describing a transformation from a rectangular
!! grid to a set of sparse points; the abstract type of transformation
!! is described in the transformation object \a trans (type
!! transform_def) which must have been properly initialised. The
!! additional information required here is the description of the
!! input grid \a in (type griddim_def), and a vol7d object (\a v7d
!! argument) which must have been initialized with the coordinates of
!! sparse points over which the transformation (typically an
!! interpolation) should take place. The generated \a grid_transform
!! object is specific to the grid and sparse point list provided.
SUBROUTINE grid_transform_grid_vol7d_init(this,trans,in,v7d,categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(in) :: in !< griddim object to transform
TYPE(vol7d),INTENT(in) :: v7d !< vol7d object with the coordinates of the sparse point to be used as transformation target
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: nx, ny, i, j
DOUBLE PRECISION :: lon_min, lon_max, lat_min, lat_max, steplon, steplat,lon_min_new, lat_min_new
doubleprecision,allocatable :: lon(:),lat(:)

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start init_grid_v7d_transform" )
#endif

this%trans=trans

nullify (this%inter_index_x)
nullify (this%inter_index_y)

nullify (this%inter_x)
nullify (this%inter_y)

nullify (this%inter_xp)
nullify (this%inter_yp)


IF (this%trans%trans_type == 'inter') THEN

  IF (this%trans%inter%sub_type == 'near' .OR. this%trans%inter%sub_type == 'bilin' ) THEN

    CALL get_val(in, nx=nx, ny=ny)
    this%innx=nx
    this%inny=ny

    this%outnx=SIZE(v7d%ana)
    this%outny=1

    ALLOCATE (this%inter_index_x(this%outnx,this%outny),&
     this%inter_index_y(this%outnx,this%outny))
    ALLOCATE(lon(this%outnx),lat(this%outnx))

    CALL get_val(in, &
     lon_min=lon_min, lon_max=lon_max,&
     lat_min=lat_min, lat_max=lat_max)

    CALL getval(v7d%ana(:)%coord,lon=lon,lat=lat)

    CALL find_index(in,this%trans%inter%sub_type,&
     nx=this%innx, ny=this%inny ,&
     lon_min=lon_min, lon_max=lon_max,&
     lat_min=lat_min, lat_max=lat_max,&
     lon=RESHAPE(lon,(/SIZE(lon),1/)),lat=RESHAPE(lat,(/SIZE(lat),1/)),&
     index_x=this%inter_index_x,index_y=this%inter_index_y)

    IF ( this%trans%inter%sub_type == 'bilin' ) THEN
      ALLOCATE(this%inter_x(this%innx,this%inny),this%inter_y(this%innx,this%inny))
      ALLOCATE(this%inter_xp(this%outnx,this%outny),this%inter_yp(this%outnx,this%outny))

      CALL griddim_gen_coord(in, this%inter_x, this%inter_y)

      CALL proj(in,&
       RESHAPE(lon,(/SIZE(lon),1/)),RESHAPE(lat,(/SIZE(lat),1/)),&
       this%inter_xp,this%inter_yp)

    ENDIF

    DEALLOCATE(lon,lat)

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_grid_v7d_transform inter sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    
  ENDIF

ELSE

  CALL l4f_category_log(this%category,L4F_WARN, &
   'init_grid_v7d_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')

ENDIF

END SUBROUTINE grid_transform_grid_vol7d_init


!> Constructor for a \a grid_transform object, defining a particular
!! sparse points-to-grid transformation.
!! It defines an object describing a transformation from a set of
!! sparse points to a rectangular grid; the abstract type of
!! transformation is described in the transformation object \a trans
!! (type transform_def) which must have been properly initialised. The
!! additional information required here is the list of the input
!! sparse points in the form of a \a vol7d object (parameter \a v7d),
!! which can be the same volume that will be successively used for
!! interpolation or a volume with just the same coordinate data, and
!! the description of the output grid \a griddim (a \a griddim_def
!! object). The generated \a grid_transform object is specific to the
!! sparse point list and grid provided.
SUBROUTINE grid_transform_vol7d_grid_init(this,trans,v7d,griddim,categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(vol7d),INTENT(in) :: v7d !< vol7d object with the coordinates of the sparse point to be used as input (only information about coordinates is used
TYPE(griddim_def),INTENT(in) :: griddim !< griddim object defining target grid
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: nx, ny,i,j
DOUBLE PRECISION :: lon_min, lon_max, lat_min, lat_max, steplon, steplat,lon_min_new, lat_min_new
doubleprecision,allocatable :: lon(:),lat(:)

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

this%trans=trans

nullify (this%inter_index_x)
nullify (this%inter_index_y)

nullify (this%inter_x)
nullify (this%inter_y)

nullify (this%inter_xp)
nullify (this%inter_yp)


IF (this%trans%trans_type == 'inter') THEN

  IF ( this%trans%inter%sub_type == 'linear' ) THEN
    
    CALL get_val(griddim, nx=nx, ny=ny)
    this%outnx=nx
    this%outny=ny
  
    this%innx=SIZE(v7d%ana)
    this%inny=1
  
    ALLOCATE(lon(this%innx),lat(this%innx))
    ALLOCATE(this%inter_xp(this%innx,this%inny),this%inter_yp(this%innx,this%inny))
    ALLOCATE(this%inter_x(this%outnx,this%outny),this%inter_y(this%outnx,this%outny))

    CALL get_val(griddim, &
     lon_min=lon_min, lon_max=lon_max,&
     lat_min=lat_min, lat_max=lat_max)

    CALL getval(v7d%ana(:)%coord,lon=lon,lat=lat)

    CALL proj(griddim,&
     RESHAPE(lon,(/SIZE(lon),1/)),RESHAPE(lat,(/SIZE(lat),1/)),&
     this%inter_xp,this%inter_yp)

    CALL griddim_gen_coord(griddim, this%inter_x, this%inter_y)

    DEALLOCATE(lon,lat)

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_v7d_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')

  ENDIF

ELSE IF (this%trans%trans_type == 'boxinter') THEN

  this%innx=SIZE(v7d%ana)
  this%inny=1
  CALL get_val(griddim, nx=this%outnx, ny=this%outny, &
   lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max)
! TODO now box size is ignored
! if box size not provided, use the actual grid step
  IF (.NOT.c_e(this%trans%inter%box%boxdx)) &
   CALL get_val(griddim, dx=this%trans%inter%box%boxdx)
  IF (.NOT.c_e(this%trans%inter%box%boxdy)) &
   CALL get_val(griddim, dx=this%trans%inter%box%boxdy)
! half size is actually needed
  this%trans%inter%box%boxdx = this%trans%inter%box%boxdx*0.5D0
  this%trans%inter%box%boxdy = this%trans%inter%box%boxdy*0.5D0
! index arrays must have the shape of input grid
  ALLOCATE(lon(this%innx),lat(this%innx))
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))

! get coordinates of input grid in geo system
  CALL getval(v7d%ana(:)%coord,lon=lon,lat=lat)
! use find_index in the opposite way
  CALL find_index(griddim,'near',&
   nx=this%outnx, ny=this%outny ,&
   lon_min=lon_min, lon_max=lon_max,&
   lat_min=lat_min, lat_max=lat_max,&
   lon=lon, lat=lat,&
   index_x=this%inter_index_x(:,1), index_y=this%inter_index_y(:,1))

ELSE

  CALL l4f_category_log(this%category,L4F_WARN, &
   'init_v7d_grid_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')

ENDIF

END SUBROUTINE grid_transform_vol7d_grid_init


!> Destructor of \a grid_tranform object.
!! It releases any memory and data associated to
!! \a grid_transform object \a this, the logger category will be deleted too.
SUBROUTINE grid_transform_delete(this)
TYPE(grid_transform),INTENT(inout) :: this !< grid_transform object

call delete(this%trans)

this%innx=imiss
this%inny=imiss
this%outnx=imiss
this%outny=imiss
this%iniox=imiss
this%inioy=imiss
this%infox=imiss
this%infoy=imiss
this%outinx=imiss
this%outiny=imiss
this%outfnx=imiss
this%outfny=imiss

if (associated(this%inter_index_x)) deallocate (this%inter_index_x)
if (associated(this%inter_index_y)) deallocate (this%inter_index_y)

if (associated(this%inter_x)) deallocate (this%inter_x)
if (associated(this%inter_y)) deallocate (this%inter_y)

if (associated(this%inter_xp)) deallocate (this%inter_xp)
if (associated(this%inter_yp)) deallocate (this%inter_yp)

! close the logger
call l4f_category_delete(this%category)

END SUBROUTINE grid_transform_delete


!> Compute the output data array from input data array according to
!! the defined transformation. The \a grid_transform object \a this
!! must have been properly initialised, so that it contains all the
!! information needed for computing the transformation. This is the
!! grid-to-grid and grid-to-sparse points version.
SUBROUTINE grid_transform_compute(this, field_in, field_out)
TYPE(grid_transform),INTENT(in) :: this !< grid_transformation object
REAL, INTENT(in) :: field_in(:,:) !< input array
REAL, INTENT(out) :: field_out(:,:) !< output aarray

INTEGER :: i, j, ii, jj, ie, je, navg
INTEGER,ALLOCATABLE :: nval(:,:)
real :: z1,z2,z3,z4
doubleprecision  :: x1,x3,y1,y3,xp,yp

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start grid_transform_compute")
#endif

! check size of field_in, field_out

if (any(shape(field_in) /= (/this%innx,this%inny/))) then

  call l4f_category_log(this%category,L4F_ERROR,"inconsistent in shape: "//&
   TRIM(to_char(this%innx))//","//TRIM(to_char(this%inny))//" /= "//&
   TRIM(to_char(SIZE(field_in,1)))//","//TRIM(to_char(SIZE(field_in,2))))
  call raise_fatal_error()
end if

if (any(shape(field_out) /= (/this%outnx,this%outny/))) then

  call l4f_category_log(this%category,L4F_ERROR,"inconsistent out shape: "//&
   TRIM(to_char(this%outnx))//","//TRIM(to_char(this%outny))//" /= "//&
   TRIM(to_char(SIZE(field_out,1)))//","//TRIM(to_char(SIZE(field_out,2))))
  call raise_fatal_error()
end if


field_out(:,:) = rmiss

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG, &
   "start grid_transform_compute "//TRIM(this%trans%trans_type)//':'// &
   TRIM(this%trans%inter%sub_type))
#endif

IF (this%trans%trans_type == 'zoom') THEN

  field_out(this%outinx:this%outfnx, &
   this%outiny:this%outfny) = &
   field_in(this%iniox:this%infox, &
   this%inioy:this%infoy)

ELSE IF (this%trans%trans_type == 'boxregrid') THEN

  jj = 0
  DO j = 1, this%inny - this%trans%boxregrid%npy + 1, this%trans%boxregrid%npy
    je = j+this%trans%boxregrid%npy-1
    jj = jj+1
    ii = 0
    DO i = 1, this%innx - this%trans%boxregrid%npx + 1, this%trans%boxregrid%npx
      ie = i+this%trans%boxregrid%npx-1
      ii = ii+1
      navg = COUNT(field_in(i:ie,j:je) /= rmiss)
      IF (navg > 0) THEN
        field_out(ii,jj) = SUM(field_in(i:ie,j:je)/navg, &
         MASK=(field_in(i:ie,j:je) /= rmiss))
      ENDIF
    ENDDO
  ENDDO

ELSE IF (this%trans%trans_type == 'inter') THEN

  IF (this%trans%inter%sub_type == 'near') THEN

    DO j = 1, this%outny 
      DO i = 1, this%outnx 

        if (c_e(this%inter_index_x(i,j)) .and. c_e(this%inter_index_y(i,j)))&
         field_out(i,j) = field_in(this%inter_index_x(i,j),this%inter_index_y(i,j))

      ENDDO
    ENDDO

  else if (this%trans%inter%sub_type == 'bilin') THEN

    DO j = 1, this%outny 
      DO i = 1, this%outnx 

        IF (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j)))THEN

          z1=field_in(this%inter_index_x(i,j),this%inter_index_y(i,j))
          z2=field_in(this%inter_index_x(i,j)+1,this%inter_index_y(i,j))
          z3=field_in(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1)
          z4=field_in(this%inter_index_x(i,j),this%inter_index_y(i,j)+1)

          IF (c_e(z1) .AND. c_e(z2) .AND. c_e(z3) .AND. c_e(z4)) THEN

            x1=this%inter_x(this%inter_index_x(i,j),this%inter_index_y(i,j))
            y1=this%inter_y(this%inter_index_x(i,j),this%inter_index_y(i,j))
            x3=this%inter_x(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1)
            y3=this%inter_y(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1)

            xp=this%inter_xp(i,j)
            yp=this%inter_yp(i,j)
            
            field_out(i,j) = hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp)

          END IF
        END IF

      ENDDO
    ENDDO

  ENDIF
ELSE IF (this%trans%trans_type == 'boxinter') THEN

  IF (this%trans%inter%sub_type == 'average') THEN
    
    ALLOCATE(nval(this%outnx, this%outny))
    field_out(:,:) = 0.0
    nval(:,:) = 0
    DO j = 1, this%inny
      DO i = 1, this%innx
        IF (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j))) THEN
          field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)) = &
           field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)) + &
           field_in(i,j)
          nval(this%inter_index_x(i,j),this%inter_index_y(i,j)) = &
           nval(this%inter_index_x(i,j),this%inter_index_y(i,j)) + 1
        ENDIF
      ENDDO
    ENDDO
    WHERE (nval(:,:) /= 0)
      field_out(:,:) = field_out(:,:)/nval(:,:)
    ELSEWHERE
      field_out(:,:) = rmiss
    END WHERE
    DEALLOCATE(nval)

  ELSE IF (this%trans%inter%sub_type == 'max') THEN

    field_out(:,:) = rmiss
    DO j = 1, this%inny
      DO i = 1, this%innx
        IF (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j))) THEN
          IF (c_e(field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)))) THEN
            field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)) = &
             MAX(field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)), &
             field_in(i,j))
          ELSE
            field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)) = field_in(i,j)
          ENDIF
        ENDIF
      ENDDO
    ENDDO


  ELSE IF (this%trans%inter%sub_type == 'min') THEN

    field_out(:,:) = rmiss
    DO j = 1, this%inny
      DO i = 1, this%innx
        IF (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j))) THEN
          IF (c_e(field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)))) THEN
            field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)) = &
             MIN(field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)), &
             field_in(i,j))
          ELSE
            field_out(this%inter_index_x(i,j),this%inter_index_y(i,j)) = field_in(i,j)
          ENDIF
        ENDIF
      ENDDO
    ENDDO

  ELSE IF (this%trans%inter%sub_type == 'percentile') THEN
    
    DO j = 1, this%outny
      DO i = 1, this%outnx
        field_out(i:i,j) = stat_percentile( &
         RESHAPE(field_in, (/SIZE(field_in)/)), &
         (/REAL(this%trans%inter%box%boxpercentile)/), &
         mask=RESHAPE((this%inter_index_x == i .AND. &
         this%inter_index_y == j), (/SIZE(field_in)/)))
      ENDDO
    ENDDO

  ENDIF

ENDIF

END SUBROUTINE grid_transform_compute


!> Compute the output data array from input data array according to
!! the defined transformation. The \a grid_transform object \a this
!! must have been properly initialised, so that it contains all the
!! information needed for computing the transformation. This is the
!! sparse points-to-grid version.
SUBROUTINE grid_transform_v7d_grid_compute(this, field_in, field_out)
TYPE(grid_transform),INTENT(in) :: this !< grid_tranform object
REAL, INTENT(in) :: field_in(:) !< input array
REAL, INTENT(out):: field_out(:,:) !< output array

real,allocatable :: field_in_p(:),x_in_p(:),y_in_p(:)
real,allocatable :: x_out(:),y_out(:)
integer :: inn_p,ier

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start v7d_grid_transform_compute")
#endif

! check size of field_in, field_out

IF (SIZE(field_in) /= this%innx .OR. 1 /= this%inny) THEN

  call l4f_category_log(this%category,L4F_ERROR,"inconsistent in shape: "//&
   TRIM(to_char(this%innx))//","//TRIM(to_char(this%inny))//" /= "//&
   TRIM(to_char(SIZE(field_in)))//",1")
  call raise_fatal_error()
end if

if (any(shape(field_out) /= (/this%outnx,this%outny/))) then

  call l4f_category_log(this%category,L4F_ERROR,"inconsistent out shape: "//&
   TRIM(to_char(this%outnx))//","//TRIM(to_char(this%outny))//" /= "//&
   TRIM(to_char(SIZE(field_out,1)))//","//TRIM(to_char(SIZE(field_out,2))))
  call raise_fatal_error()
end if

field_out(:,:) = rmiss

IF (this%trans%trans_type == 'inter') THEN

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,"start v7d_grid_transform_compute inter")
#endif

  if (this%trans%inter%sub_type == 'linear') THEN

    inn_p=count(c_e(field_in))

    call l4f_category_log(this%category,L4F_INFO,"Number of sparse data points= "//to_char(inn_p))

    if (inn_p > 2) then

      allocate(field_in_p(inn_p))
      allocate(x_in_p(inn_p))
      allocate(y_in_p(inn_p))

      field_in_p=pack(field_in,c_e(field_in))
      x_in_p=pack(this%inter_xp(:,1),c_e(field_in))
      y_in_p=pack(this%inter_yp(:,1),c_e(field_in))

#ifdef HAVE_LIBNGMATH

      CALL NATGRIDS(inn_p,x_in_p,y_in_p,field_in_p,&
       this%outnx, this%outny, REAL(this%inter_x(:,1)), &
       REAL(this%inter_y(1,:)), field_out, ier)
#else
      call l4f_category_log(this%category,L4F_ERROR,"libsim compiled without NATGRIDD (ngmath ncarg library)")
      call raise_fatal_error()

#endif

      IF (ier /= 0) THEN
        call l4f_category_log(this%category,L4F_ERROR,"Error return from NATGRIDD = "//to_char(ier))
        call raise_fatal_error()
      ENDIF

      deallocate(field_in_p,x_in_p,y_in_p)

    else

      call l4f_category_log(this%category,L4F_INFO,"Insufficient data in gridded region to triangulate")

    end if

  ELSE IF (this%trans%trans_type == 'boxinter') THEN ! use the grid-to-grid method
      
    CALL compute(this, RESHAPE(field_in, (/SIZE(field_in), 1/)), field_out)

  ELSE

    call l4f_category_log(this%category,L4F_ERROR, &
     "sub_type not right here: "//this%trans%inter%sub_type)
    call raise_fatal_error()

  END IF

else

  call l4f_category_log(this%category,L4F_ERROR, &
   "trans_type not right here: "//this%trans%trans_type)
  call raise_fatal_error()

END IF

END SUBROUTINE grid_transform_v7d_grid_compute


! Bilinear interpolation
! Effettua interpolazione bilineare dati i valori nei punti 1,2,3,4 e
! le coordinate dei punti 1 e 3 oltre a quelle del punto p dove viene
! valutato il campo.
!_____________________________________________________________
!				disposizione punti
!	4	3
!
!	  p
!
!	1	2
! _____________________________________________________________
elemental real function hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp) result (zp)

doubleprecision,intent(in):: x1,y1 ! coordinate of the lower left point
doubleprecision,intent(in):: x3,y3 ! coordinate of the upper right point
doubleprecision,intent(in):: xp,yp ! coordinate of point where interpolate
real,intent(in) :: z1,z2,z3,z4 ! Z values on the four points

doubleprecision :: p1,p2
real :: z5,z6


p2=((yp-y1)/(y3-y1))
p1=((xp-x1)/(x3-x1))

z5=(z4-z1)*p2+z1
z6=(z3-z2)*p2+z2

zp=(z6-z5)*(p1)+z5
      

end function hbilin


! Locate index of requested point
elemental subroutine find_index(this,inter_type,&
 nx,ny, lon_min, lon_max, lat_min,lat_max,&
 lon,lat,index_x,index_y)

type(griddim_def),intent(in) :: this ! griddim object (from grid)
character(len=*),intent(in) :: inter_type ! interpolation type (determine wich point is requested)
! dimension (to grid)
integer,intent(in) :: nx,ny 
! extreme coordinate (to grid)
doubleprecision,intent(in) :: lon_min, lon_max, lat_min, lat_max
! target coordinate
doubleprecision,intent(in) :: lon,lat
! index of point requested
integer,optional,intent(out) :: index_x,index_y 

doubleprecision :: x,y

if (inter_type == "near") then

  call proj(this,lon,lat,x,y)

  if (present(index_x))then
    index_x=nint((x-lon_min)/((lon_max-lon_min)/dble(nx-1)))+1
    if ( index_x < 1 .or. index_x > nx ) index_x=imiss
  end if

  if (present(index_y))then
    index_y=nint((y-lat_min)/((lat_max-lat_min)/dble(ny-1)))+1
    if ( index_y < 1 .or. index_y > ny ) index_y=imiss
  end if

else if (inter_type == "bilin") then

  call proj(this,lon,lat,x,y)

  if (present(index_x))then
    index_x=(x-lon_min)/((lon_max-lon_min)/dble(nx-1))+1
    if ( index_x < 1 .or. index_x+1 > nx ) index_x=imiss
  end if

  if (present(index_y))then
    index_y=(y-lat_min)/((lat_max-lat_min)/dble(ny-1))+1
    if ( index_y < 1 .or. index_y+1 > ny ) index_y=imiss
  end if

else

  if (present(index_x)) index_x=imiss
  if (present(index_y)) index_y=imiss

end if

end subroutine find_index


END MODULE grid_transform_class
