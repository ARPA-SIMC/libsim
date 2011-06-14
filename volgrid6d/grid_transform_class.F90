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
!!  - trans_type='zoom' a new grid is obtained by cutting or extending
!!    (with missing values) the input grid, adding or removing points
!!    on the four sides, without changing the geographical reference
!!    system (grid-to-grid)
!!    - sub_type='coord' the corners of the zoomed/extended area are
!!      defined by geographical coordinates
!!    - sub_type='coordbb' the bounds of the zoomed/extended area are
!!      computed so to contain all the input points which lie in the
!!      provided lon/lat bounding box
!!    - sub_type='index' the bounds of the zoomed/extended area are
!!      defined by grid indices.
!!  - trans_type='boxregrid' regrids the input data grid on a new grid
!!    in which the value at every point is the result of a function
!!    computed over \a npx X \a npy points of the original grid,
!!    without changing the geographical reference system
!!    (grid-to-grid)
!!    - sub_type='average' the function used is the average.
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!  - trans_type='inter' interpolates the input data on a new set of
!!    points
!!    - sub_type='near' the interpolated value is that of the nearest
!!      input point (grid-to-grid, grid-to-sparse point)
!!    - sub_type='bilin' the interpolated value is computed as a
!!      bilinear interpolation of the 4 surrounding input points
!!      (grid-to-grid, grid-to-sparse point)
!!    - sub_type='linear' the interpolated value is computed as a
!!      linear interpolation of the 3 surrounding input points
!!      individuated by means of a triangulation procedure (sparse
!!      points-to-grid, sparse points-to-sparse points).
!!  - trans_type='boxinter' computes data on a new grid in which the
!!    value at every point is the result of a function computed over
!!    those input points that lie inside the output point grid box
!!    (grid-to-grid and sparse points-to-grid)
!!    - sub_type='average' the function used is the average
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!    - sub_type='percentile' the function used is a requested
!!      percentile of the input points distribution.
!!  - trans_type='polyinter' computes data on a new set of points in
!!    which the value at every point is the result of a function
!!    computed over those input points that lie inside an arbitrary
!!    georoferenced polygon; the output point coordinates are defined
!!    as the centroids of the polygons (grid-to-sparse points and
!!    sparse points-to-sparse points)
!!    - sub_type='average' the function used is the average
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!    - sub_type='percentile' the function used is a requested
!!      percentile of the input points distribution.
!!  - trans_type='metamorphosis' the output points and values are the
!!    same as the input ones but either the component flag of the
!!    output grid is changed, keeping the same projection and grid, or
!!    the underlying data structure is changed from grid to sparse
!!    points (grid-to-grid and grid-to-sparse points)
!!    - sub_type='all' all the input points are kept in the output
!!    - sub_type='coordbb' the input points which lie in the provided
!!      lon/lat bounding box are kept in the output (grid-to-sparse
!!      points only).
!!
!! \ingroup volgrid6d
MODULE grid_transform_class
USE vol7d_class
USE err_handling
USE geo_proj_class
USE grid_class
USE grid_dim_class
USE optional_values
USE geo_coord_class
USE simple_stat
IMPLICIT NONE

CHARACTER(len=255),PARAMETER:: subcategory="grid_transform_class"

! information for interpolation aver a rectangular area
TYPE area_info
  double precision :: boxdx ! longitudinal/x extension of the box for box interpolation, default the target x grid step
  double precision :: boxdy ! latitudinal/y extension of the box for box interpolation, default the target y grid step
END TYPE area_info

! information for statistical processing of interpoland data
TYPE stat_info
  DOUBLE PRECISION :: percentile ! percentile [0,100] of the distribution of points in the box to use as interpolated value, if missing, the average is used, if 0.or 100. the MIN() and MAX() are used as a shortcut
END TYPE stat_info

! rectangle index information
type rect_ind
  INTEGER :: ix ! index of initial point of new grid on x
  INTEGER :: iy ! index of initial point of new grid on y
  INTEGER :: fx ! index of final point of new grid on x
  INTEGER :: fy ! index of final point of new grid on y
end type rect_ind

! rectangle coord information
type rect_coo
  DOUBLEPRECISION ilon ! coordinate of initial point of new grid on x
  DOUBLEPRECISION ilat ! coordinate of initial point of new grid on y
  DOUBLEPRECISION flon ! coordinate of final point of new grid on x
  DOUBLEPRECISION flat ! coordinate of final point of new grid on y
end type rect_coo

! box information
type box_info
  INTEGER :: npx ! number of points along x direction
  INTEGER :: npy ! number of points along y direction
end type box_info

! Vertical interpolation information.
! The input vertical coordinate can be indicated either as the value
! of the vertical level (so that it will be the same on every point
! at a given vertical level), or as the value of a specified variable
! at each point in space (so that it will depend on the horizontal
! position too).
TYPE vertint
!  CHARACTER(len=80) :: sub_type ! subtype of transformation, can be \c 'linear'
  TYPE(vol7d_level) :: input_levtype ! type of vertical level of input data (only type of first and second surface are used, level values are ignored)
  TYPE(vol7d_var) :: input_coordvar ! variable that defines the vertical coordinate in the input volume, if missing, the value of the vertical level is used
  TYPE(vol7d_level) :: output_levtype ! type of vertical level of output data (only type of first and second surface are used, level values are ignored)
END TYPE vertint

!> This object defines in an abstract way the type of transformation
!! to be applied.
!! It does not carry specific information about the grid to which it
!! will be applied, so the same instance can be reused for
!! transforming in the same way different grids.
TYPE transform_def
  private

  CHARACTER(len=80) :: trans_type ! type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'inter', \c 'vertint' ...
  CHARACTER(len=80) :: sub_type ! subtype of transformation, can be \c 'linear'
  logical :: extrap ! enable elaboration outside data bounding box

  type(rect_ind) :: rect_ind ! rectangle information by index
  type(rect_coo) :: rect_coo ! rectangle information by coordinates
  type(area_info) :: area_info ! 
  type(stat_info) :: stat_info ! 
  type(box_info) :: box_info ! boxregrid specification
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
  integer,pointer :: inter_index_x(:,:),inter_index_y(:,:),inter_index_z(:)
  doubleprecision,pointer :: inter_x(:,:),inter_y(:,:)
  doubleprecision,pointer :: inter_xp(:,:),inter_yp(:,:),inter_zp(:)
  LOGICAL,POINTER :: point_mask(:,:)
!  type(volgrid6d) :: input_vertcoordvol ! volume which provides the input vertical coordinate if separated from the data volume itself (for vertint) cannot be here because of cross-use, should be an argument of compute
!  type(vol7d_level), pointer :: output_vertlevlist(:) ! list of vertical levels of output data (for vertint) can be here or an argument of compute, how to do?
  integer :: category ! category for log4fortran
  logical :: valid ! the transformation has been successfully initialised

END TYPE grid_transform


!> Constructors of the corresponding objects.
INTERFACE init
  MODULE PROCEDURE transform_init, grid_transform_init, &
   grid_transform_grid_vol7d_init, grid_transform_vol7d_grid_init, &
   grid_transform_vol7d_vol7d_init
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

!> Returns \a .TRUE. if, after \a init , the corresponding \a grid_transform
!! object has been correctly initilised.
INTERFACE c_e
  MODULE PROCEDURE grid_transform_c_e
END INTERFACE

PRIVATE
PUBLIC init, delete, get_val, compute, c_e
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
 npx, npy, boxdx, boxdy, percentile, &
 extrap, time_definition, &
 input_levtype, input_coordvar, output_levtype, categoryappend)
TYPE(transform_def),INTENT(out) :: this !< transformation object
CHARACTER(len=*) :: trans_type !< type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'interp', \c 'vertint' ...
CHARACTER(len=*) :: sub_type !< sub type of transformation, it depends on \a trans_type
INTEGER,INTENT(in),OPTIONAL :: ix !< index of initial point of new grid on x (for zoom)
INTEGER,INTENT(in),OPTIONAL :: iy !< index of initial point of new grid on y (for zoom)
INTEGER,INTENT(in),OPTIONAL :: fx !< index of final point of new grid on x (for zoom)
INTEGER,INTENT(in),OPTIONAL :: fy !< index of final point of new grid on y (for zoom)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: ilon !< coordinate of initial point of new grid or of bounding box on x (for zoom and metamorphosis)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: ilat !< coordinate of initial point of new grid or of bounding box on y (for zoom and metamorphosis)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: flon !< coordinate of final point of new grid or of bounding box on x (for zoom and metamorphosis)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: flat !< coordinate of final point of new grid or of bounding box on y (for zoom and metamorphosis)
INTEGER,INTENT(IN),OPTIONAL :: npx !< number of points to average along x direction (for boxregrid)
INTEGER,INTENT(IN),OPTIONAL :: npy !< number of points to average along y direction (for boxregrid)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: boxdx !< longitudinal/x extension of the box for box interpolation, default the target x grid step (unimplemented !)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: boxdy !< latitudinal/y extension of the box for box interpolation, default the target y grid step (unimplemented !)
DOUBLEPRECISION,INTENT(in),OPTIONAL :: percentile !< percentile [0,1] of the distribution of points in the box to use as interpolated value, if missing, the average is used
LOGICAL,INTENT(IN),OPTIONAL :: extrap !< activate extrapolation outside input domain (use with care!)
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< time definition for output vol7d object 0=time is reference time ; 1=time is validity time
TYPE(vol7d_level),INTENT(IN),OPTIONAL :: input_levtype !< type of vertical level of input data to be vertically interpolated (only type of first and second surface are used, level values are ignored)
TYPE(vol7d_var),INTENT(IN),OPTIONAL :: input_coordvar !< variable that defines the vertical coordinate in the input volume for vertical interpolation, if missing, the value of the vertical level defined with \a input_levtype is used
TYPE(vol7d_level),INTENT(IN),OPTIONAL :: output_levtype !< type of vertical level to which data should be vertically interpolated (only type of first and second surface are used, level values are ignored)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< suffix to append to log4fortran namespace category

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

this%trans_type = trans_type
this%sub_type = sub_type

CALL optio(extrap,this%extrap)

call optio(ix,this%rect_ind%ix)
call optio(iy,this%rect_ind%iy)
call optio(fx,this%rect_ind%fx)
call optio(fy,this%rect_ind%fy)

call optio(ilon,this%rect_coo%ilon)
call optio(ilat,this%rect_coo%ilat)
call optio(flon,this%rect_coo%flon)
call optio(flat,this%rect_coo%flat)

CALL optio(boxdx,this%area_info%boxdx)
CALL optio(boxdy,this%area_info%boxdy)
CALL optio(percentile,this%stat_info%percentile)

CALL optio(npx,this%box_info%npx)
CALL optio(npy,this%box_info%npy)

IF (PRESENT(input_levtype)) THEN
  this%vertint%input_levtype = input_levtype
ELSE
  this%vertint%input_levtype = vol7d_level_miss
ENDIF
IF (PRESENT(input_coordvar)) THEN
  this%vertint%input_coordvar = input_coordvar
ELSE
  this%vertint%input_coordvar = vol7d_var_miss
ENDIF
IF (PRESENT(output_levtype)) THEN
  this%vertint%output_levtype = output_levtype
ELSE
  this%vertint%output_levtype = vol7d_level_miss
ENDIF

call optio(time_definition,this%time_definition)
if (c_e(this%time_definition) .and. &
 (this%time_definition < 0 .OR. this%time_definition > 1))THEN
  call l4f_category_log(this%category,L4F_ERROR,"Error in time_definition: "//to_char(this%time_definition))
  call raise_fatal_error()
end if


IF (this%trans_type == 'zoom') THEN

  if (this%sub_type == 'coord')then

    if (c_e(this%rect_coo%ilon) .and. c_e(this%rect_coo%ilat) .and. &
        c_e(this%rect_coo%flon) .and. c_e(this%rect_coo%flat)) then ! coordinates given
    
!check
      if ( this%rect_coo%ilon > this%rect_coo%flon .or. &
       this%rect_coo%ilat > this%rect_coo%flat ) then

        call l4f_category_log(this%category,L4F_ERROR, &
         "invalid zoom coordinates: ")
        call l4f_category_log(this%category,L4F_ERROR, &
         TRIM(to_char(this%rect_coo%ilon))//'/'// &
         TRIM(to_char(this%rect_coo%flon)))
        call l4f_category_log(this%category,L4F_ERROR, &
         TRIM(to_char(this%rect_coo%ilat))//'/'// &
         TRIM(to_char(this%rect_coo%flat)))
        call raise_fatal_error()
      end if

    else

      call l4f_category_log(this%category,L4F_ERROR,"zoom: coord parameters missing")
      call raise_fatal_error()
        
    end if

  else if (this%sub_type == 'coordbb')then

    if (c_e(this%rect_coo%ilon) .and. c_e(this%rect_coo%ilat) .and. &
        c_e(this%rect_coo%flon) .and. c_e(this%rect_coo%flat)) then ! coordinates given
    else

      call l4f_category_log(this%category,L4F_ERROR,"zoom: coordbb parameters missing")
      call raise_fatal_error()
        
    end if

  else if (this%sub_type == 'index')then

    if (c_e(this%rect_ind%ix) .and. c_e(this%rect_ind%iy) .or. &
        c_e(this%rect_ind%fx) .or. c_e(this%rect_ind%fy)) then

! check
      if (this%rect_ind%ix > this%rect_ind%fx .OR. &
       this%rect_ind%iy > this%rect_ind%fy) THEN

        CALL l4f_category_log(this%category,L4F_ERROR,'invalid zoom indices: ')
        CALL l4f_category_log(this%category,L4F_ERROR, &
         TRIM(to_char(this%rect_ind%ix))//'/'// &
         TRIM(to_char(this%rect_ind%fx)))
        CALL l4f_category_log(this%category,L4F_ERROR, &
         TRIM(to_char(this%rect_ind%iy))//'/'// &
         TRIM(to_char(this%rect_ind%fy)))

        CALL raise_fatal_error()
      ENDIF

    else

      CALL l4f_category_log(this%category,L4F_ERROR,&
       'zoom: index parameters ix, iy, fx, fy not provided')
      CALL raise_fatal_error()

    ENDIF

  else

    CALL l4f_category_log(this%category,L4F_ERROR,'zoom: sub_type '// &
     TRIM(this%sub_type)//' is wrong')
    CALL raise_fatal_error()

  end if

ELSE IF (this%trans_type == 'boxregrid') THEN

  IF (c_e(this%box_info%npx) .AND. c_e(this%box_info%npy)) THEN

    IF (this%box_info%npx <= 0 .OR. this%box_info%npy <= 0 ) THEN
      CALL l4f_category_log(this%category,L4F_ERROR,'invalid regrid parameters: '//&
       TRIM(to_char(this%box_info%npx))//' '//TRIM(to_char(this%box_info%npy)))
      CALL raise_fatal_error()
    ENDIF

  ELSE

    CALL l4f_category_log(this%category,L4F_ERROR, &
     'boxregrid parameters npx, npy not provided')
    CALL raise_fatal_error()

  ENDIF

  IF (this%sub_type == 'average' .OR. &
   this%sub_type == 'max' .OR. this%sub_type == 'min')THEN
! nothing to do here
  ELSE
    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid: sub_type '// &
     TRIM(this%sub_type)//' is wrong')
    CALL raise_fatal_error()
  ENDIF

ELSE IF (this%trans_type == 'inter') THEN

  if (this%sub_type == 'near')then
! nothing to do here
  else if (this%sub_type == 'bilin')then
! nothing to do here
  else if (this%sub_type == 'linear')then
! nothing to do here
  else
    CALL l4f_category_log(this%category,L4F_ERROR,'inter: sub_type '// &
     TRIM(this%sub_type)//' is wrong')
    CALL raise_fatal_error()
  endif

ELSE IF (this%trans_type == 'boxinter' .OR. this%trans_type == 'polyinter')THEN

  IF (this%sub_type == 'average') THEN
    this%stat_info%percentile = rmiss
  ELSE IF (this%sub_type == 'max') THEN
    this%stat_info%percentile = 101.
  ELSE IF (this%sub_type == 'min') THEN
    this%stat_info%percentile = -1.
  ELSE IF (this%sub_type == 'percentile') THEN
    IF (.NOT.c_e(this%stat_info%percentile)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR,TRIM(this%trans_type)// &
       '/percentile: percentile value not provided')
      CALL raise_fatal_error()
    ELSE IF (this%stat_info%percentile >= 100.) THEN
      this%sub_type = 'max'
    ELSE IF (this%stat_info%percentile <= 0.) THEN
      this%sub_type = 'min'
    ENDIF
  ELSE
    CALL l4f_category_log(this%category,L4F_ERROR,TRIM(this%trans_type)// &
     ': sub_type '//TRIM(this%sub_type)//' is wrong')
    CALL raise_fatal_error()
  ENDIF

ELSE IF (this%trans_type == 'vertint') THEN

  IF (this%vertint%input_levtype == vol7d_level_miss) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'vertint parameter input_levtype not provided')
    CALL raise_fatal_error()
  ENDIF

  IF (this%vertint%output_levtype == vol7d_level_miss) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'vertint parameter output_levtype not provided')
    CALL raise_fatal_error()
  ENDIF

ELSE IF (this%trans_type == 'metamorphosis') THEN

  if (this%sub_type == 'all')then
! nothing to do here
  else if (this%sub_type == 'coordbb')then

    if (c_e(this%rect_coo%ilon) .and. c_e(this%rect_coo%ilat) .and. &
        c_e(this%rect_coo%flon) .and. c_e(this%rect_coo%flat)) then ! coordinates given
    else

      call l4f_category_log(this%category,L4F_ERROR,"metamorphosis: coordbb parameters missing")
      call raise_fatal_error()
        
    end if

  else
    CALL l4f_category_log(this%category,L4F_ERROR,'metamorphosis: sub_type '// &
     TRIM(this%sub_type)//' is wrong')
    CALL raise_fatal_error()
  endif

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

this%sub_type=cmiss

this%rect_ind%ix=imiss
this%rect_ind%iy=imiss
this%rect_ind%fx=imiss
this%rect_ind%fy=imiss

this%rect_coo%ilon=dmiss
this%rect_coo%ilat=dmiss
this%rect_coo%flon=dmiss
this%rect_coo%flat=dmiss

this%sub_type=cmiss

this%box_info%npx=imiss
this%box_info%npy=imiss

this%sub_type=cmiss

this%extrap=.false.

!chiudo il logger
call l4f_category_delete(this%category)

END SUBROUTINE transform_delete


!> Method for returning the contents of the object.
SUBROUTINE transform_get_val(this, time_definition, trans_type, sub_type)
type(transform_def),intent(in) :: this !< object to examine
INTEGER,INTENT(out),OPTIONAL :: time_definition !< 0=time is reference time, 1=time is validity time
CHARACTER(len=*),INTENT(out),OPTIONAL :: trans_type !< type of transformation
CHARACTER(len=*),INTENT(out),OPTIONAL :: sub_type !< subtype of transformation

IF (PRESENT(time_definition)) time_definition=this%time_definition
IF (PRESENT(trans_type)) trans_type = this%trans_type
IF (PRESENT(sub_type)) sub_type = this%sub_type

END SUBROUTINE transform_get_val


!> Constructor for a \a grid_transform object, defining a particular
!! grid-to-grid transformation. Cambiare!!!
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
SUBROUTINE grid_transform_levtype_levtype_init(this, trans, lev_in, lev_out, categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(vol7d_level),INTENT(in) :: lev_in(:) !< vol7d_level from input object
TYPE(vol7d_level),INTENT(in) :: lev_out(:) !< vol7d_level object defining target vertical grid
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

DOUBLE PRECISION :: coord_in(SIZE(lev_in)), coord_out(SIZE(lev_out))
LOGICAL :: mask_in(SIZE(lev_in)), mask_out(SIZE(lev_out))
INTEGER :: i, j, ni, no


CALL grid_transform_init_common(this, trans, categoryappend)

IF (this%trans%trans_type == 'vertint') THEN

  IF (this%trans%sub_type == 'linear') THEN

    IF (trans%vertint%input_levtype%level1 /= trans%vertint%output_levtype%level1 &
     .OR. &
     trans%vertint%input_levtype%level2 /= trans%vertint%output_levtype%level2) &
     THEN
      CALL l4f_category_log(this%category, L4F_ERROR, &
       'grid_transform_levtype_levtype_init: input and output level types &
       &must be the same (for now), ('// &
       TRIM(to_char(trans%vertint%input_levtype%level1))//','// &
       TRIM(to_char(trans%vertint%input_levtype%level2))//') /= ('// &
       TRIM(to_char(trans%vertint%output_levtype%level1))//','// &
       TRIM(to_char(trans%vertint%output_levtype%level2))//')')
      CALL raise_fatal_error()
    ENDIF

    mask_in(:) = (lev_in(:)%level1 == trans%vertint%input_levtype%level1) .AND. &
     (lev_in(:)%level2 == trans%vertint%input_levtype%level2)
    mask_out(:) = (lev_out(:)%level1 == trans%vertint%output_levtype%level1) .AND. &
     (lev_out(:)%level2 == trans%vertint%output_levtype%level2)

    CALL make_vert_coord(lev_in, mask_in, coord_in)
    CALL make_vert_coord(lev_out, mask_out, coord_out)
! compute here log of pressure if desired
    ni = COUNT(mask_in)
    no = COUNT(mask_out)

! set valid = .FALSE. here?
    IF (ni == 0) &
     CALL l4f_category_log(this%category, L4F_WARN, &
     'grid_transform_levtype_levtype_init: &
     &input contains no vertical levels of type ('// &
     TRIM(to_char(trans%vertint%input_levtype%level1))//','// &
     TRIM(to_char(trans%vertint%input_levtype%level2))// &
     ') suitable for interpolation')
    IF (no == 0) &
     CALL l4f_category_log(this%category, L4F_WARN, &
     'grid_transform_levtype_levtype_init: &
     &output contains no vertical levels of type ('// &
     TRIM(to_char(trans%vertint%output_levtype%level1))//','// &
     TRIM(to_char(trans%vertint%output_levtype%level2))// &
     ') suitable for interpolation')

! code up to this point may be common for all vertint?

    ALLOCATE(this%inter_index_z(no), this%inter_zp(no))
    IF (this%trans%extrap .AND. ni > 0) THEN
! set to first input point (extrapolate to constant value)
      this%inter_index_z(:) = 1
      this%inter_zp(:) = 1.0D0
    ELSE
! do not extrapolate
      this%inter_index_z(:) = imiss
      this%inter_zp(:) = dmiss
    ENDIF

    DO j = 1, no
      DO i = 2, ni
        IF (coord_in(i) >= coord_out(j)) THEN
          IF (coord_out(j) >= coord_in(i-1)) THEN
            this%inter_index_z(j) = i - 1
            this%inter_zp(j) = (coord_out(j)-coord_in(i)) / &
             (coord_in(i-1)-coord_in(i)) ! weight for (i-1)
          ENDIF
          EXIT ! exit in any case
        ENDIF
      ENDDO
      IF (i > ni .AND. this%trans%extrap .AND. ni > 0) THEN
! set to last input point (extrapolate to constant value)
        this%inter_index_z(j) = ni - 1
        this%inter_zp(j) = 0.0D0
      ENDIF
    ENDDO

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_grid_transform inter sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.

  ENDIF

  CALL l4f_category_log(this%category,L4F_WARN, &
   'init_grid_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  this%valid = .FALSE.

ENDIF

END SUBROUTINE grid_transform_levtype_levtype_init


! internal subroutine for computing vertical coordinate values
SUBROUTINE make_vert_coord(lev, mask, coord)
TYPE(vol7d_level),INTENT(in) :: lev(:)
LOGICAL,INTENT(in) :: mask(:)
DOUBLE PRECISION,INTENT(out) :: coord(:)

!IF (SIZE(mask) /= SIZE(coord)) THEN ! che fare?

IF (c_e(lev(1)%level2)) THEN ! layer between 2 levels
  WHERE(mask(:))
    coord(:) = (lev(:)%l1 + lev(:)%l2)*0.5D0
  ELSEWHERE
    coord(:) = dmiss
  END WHERE
ELSE
  WHERE(mask(:))
    coord(:) = lev(:)%l1
  ELSEWHERE
    coord(:) = dmiss
  END WHERE
ENDIF

END SUBROUTINE make_vert_coord


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
!! transformations.
!!
!! The generated \a grid_transform object is specific to the input and
!! output grids involved. The function \a c_e can be used in order to
!! check whether the object has been successfully initialised, if the
!! result is \a .FALSE., it should not be used further on.
SUBROUTINE grid_transform_init(this, trans, in, out, categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(inout) :: in !< griddim object to transform
TYPE(griddim_def),INTENT(inout) :: out !< griddim object defining target grid (input or output depending on type of transformation)
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: nx, ny, i, j, cf_i, cf_o
DOUBLE PRECISION :: xmin, xmax, ymin, ymax, steplon, steplat, &
 xmin_new, ymin_new, ellips_smaj_axis, ellips_flatt
DOUBLE PRECISION :: l1, l2
TYPE(geo_proj) :: proj_in, proj_out

CALL grid_transform_init_common(this, trans, categoryappend)

! output ellipsoid has to be the same as for the input (improve)
CALL get_val(in, ellips_smaj_axis=ellips_smaj_axis, ellips_flatt=ellips_flatt)
CALL set_val(out, ellips_smaj_axis=ellips_smaj_axis, ellips_flatt=ellips_flatt)

IF (this%trans%trans_type == 'zoom') THEN

  IF (this%trans%sub_type == 'coord') THEN

    CALL griddim_zoom_coord(in, &
     this%trans%rect_coo%ilon, this%trans%rect_coo%ilat,&
     this%trans%rect_coo%flon, this%trans%rect_coo%flat,&
     this%trans%rect_ind%ix, this%trans%rect_ind%iy, &
     this%trans%rect_ind%fx, this%trans%rect_ind%fy)

!    this%trans%sub_type = 'index'

  else if (this%trans%sub_type == 'coordbb') THEN

! compute coordinates of input grid in geo system
    CALL unproj(in)
    CALL get_val(in, nx=nx, ny=ny)

    ALLOCATE(this%point_mask(nx,ny))
    this%point_mask(:,:) = .FALSE.

! mark points falling into requested bounding-box
    DO j = 1, ny
      DO i = 1, nx
!        IF (geo_coord_inside_rectang()
        IF (in%dim%lon(i,j) > this%trans%rect_coo%ilon .AND. &
         in%dim%lon(i,j) < this%trans%rect_coo%flon .AND. &
         in%dim%lat(i,j) > this%trans%rect_coo%ilat .AND. &
         in%dim%lat(i,j) < this%trans%rect_coo%flat) THEN ! improve!
          this%point_mask(i,j) = .TRUE.
        ENDIF
      ENDDO
    ENDDO

! determine cut indices keeping all points which fall inside b-b
    DO i = 1, nx
      IF (ANY(this%point_mask(i,:))) EXIT
    ENDDO
    this%trans%rect_ind%ix = i
    DO i = nx, this%trans%rect_ind%ix, -1
      IF (ANY(this%point_mask(i,:))) EXIT
    ENDDO
    this%trans%rect_ind%fx = i

    DO j = 1, ny
      IF (ANY(this%point_mask(:,j))) EXIT
    ENDDO
    this%trans%rect_ind%iy = j
    DO j = ny, this%trans%rect_ind%iy, -1
      IF (ANY(this%point_mask(:,j))) EXIT
    ENDDO
    this%trans%rect_ind%fy = j

    DEALLOCATE(this%point_mask)

    IF (this%trans%rect_ind%ix > this%trans%rect_ind%fx .OR. &
     this%trans%rect_ind%iy > this%trans%rect_ind%fy) THEN

      CALL l4f_category_log(this%category,L4F_ERROR, &
       "zoom coordbb: no points inside bounding box "//&
       TRIM(to_char(this%trans%rect_coo%ilon))//","// &
       TRIM(to_char(this%trans%rect_coo%flon))//","// &
       TRIM(to_char(this%trans%rect_coo%ilat))//","// &
       TRIM(to_char(this%trans%rect_coo%flat)))
      CALL raise_fatal_error() ! really fatal error?

    ENDIF

  ELSE IF (this%trans%sub_type == 'index') THEN
! nothing particular to do
  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'grid_transform_init zoom sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.
    RETURN
    
  ENDIF
! to do in all zoom cases
  
  CALL get_val(in, nx=nx, ny=ny, xmin=xmin, xmax=xmax, &
   ymin=ymin, ymax=ymax, dx=steplon, dy=steplat)

! old indices
  this%iniox = min(max(this%trans%rect_ind%ix,1),nx) ! iox
  this%inioy = min(max(this%trans%rect_ind%iy,1),ny) ! ioy
  this%infox = max(min(this%trans%rect_ind%fx,nx),1) ! fox
  this%infoy = max(min(this%trans%rect_ind%fy,ny),1) ! foy
! new indices
  this%outinx = min(max(2-this%trans%rect_ind%ix,1),nx) ! inx
  this%outiny = min(max(2-this%trans%rect_ind%iy,1),ny) ! iny
  this%outfnx = min(this%trans%rect_ind%fx,nx)-this%trans%rect_ind%ix+1 ! fnx
  this%outfny = min(this%trans%rect_ind%fy,ny)-this%trans%rect_ind%iy+1 ! fny

  xmin=xmin+steplon*(this%trans%rect_ind%ix-1)
  ymin=ymin+steplat*(this%trans%rect_ind%iy-1)
  xmax=xmax+steplon*(this%trans%rect_ind%fx-nx)
  ymax=ymax+steplat*(this%trans%rect_ind%fy-ny)

  call copy(in,out)
! if unproj has been called for in, in%dim will contain allocated coordinates
! which will be copied to out%dim, but they are wrong
  call dealloc(out%dim)

  out%dim%nx = this%trans%rect_ind%fx - this%trans%rect_ind%ix + 1 ! newx
  out%dim%ny = this%trans%rect_ind%fy - this%trans%rect_ind%iy + 1 ! newy

  this%innx = nx
  this%inny = ny

  this%outnx=out%dim%nx
  this%outny=out%dim%ny

  CALL set_val(out, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

ELSE IF (this%trans%trans_type == 'boxregrid') THEN

  IF (this%trans%sub_type == 'average' .OR. &
   this%trans%sub_type == 'max' .OR. this%trans%sub_type == 'min')THEN

    CALL get_val(in, nx=nx, ny=ny, xmin=xmin, xmax=xmax, &
     ymin=ymin, ymax=ymax, dx=steplon, dy=steplat)

    this%innx = nx
    this%inny = ny

! new grid
    xmin_new = xmin + (this%trans%box_info%npx - 1)*0.5D0*steplon
    ymin_new = ymin + (this%trans%box_info%npy - 1)*0.5D0*steplat

    CALL l4f_category_log(this%category,L4F_DEBUG,"copying griddim in out")
    call copy(in, out)
    out%dim%nx = nx/this%trans%box_info%npx
    out%dim%ny = ny/this%trans%box_info%npy

    this%outnx=out%dim%nx
    this%outny=out%dim%ny
    steplon = steplon*this%trans%box_info%npx
    steplat = steplat*this%trans%box_info%npy

    CALL set_val(out, xmin=xmin_new, ymin=ymin_new, &
     xmax=xmin_new + DBLE(out%dim%nx-1)*steplon, dx=steplon, &
     ymax=ymin_new + DBLE(out%dim%ny-1)*steplat, dy=steplat)

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_grid_transform boxregrid sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.
    
  ENDIF
  
ELSE IF (this%trans%trans_type == 'inter') THEN

! set increments in new grid in order for all the baraque to work
  CALL griddim_setsteps(out, out%dim%nx, out%dim%ny)
! check component flag
  CALL get_val(in, proj=proj_in, component_flag=cf_i)
  CALL get_val(out, proj=proj_out, component_flag=cf_o)
  IF (proj_in == proj_out) THEN
! same projection: set output component flag equal to input regardless
! of its value
    CALL set_val(out, component_flag=cf_i)
  ELSE
! different projection, interpolation possible only with vector data
! referred to geograpical axes
    IF (cf_i == 1) THEN
      CALL l4f_category_log(this%category,L4F_WARN, &
       'trying to interpolate a grid with component flag 1 to a grid on a different projection')
      CALL l4f_category_log(this%category,L4F_WARN, &
       'vector fields will probably be wrong')
!      this%valid = .FALSE.
!      RETURN
    ELSE
      CALL set_val(out, component_flag=cf_i)
    ENDIF
  ENDIF

  IF (this%trans%sub_type == 'near' .OR. this%trans%sub_type == 'bilin' ) THEN
    
    CALL get_val(in, nx=this%innx, ny=this%inny, &
     xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
    CALL get_val(out, nx=this%outnx, ny=this%outny)
  
    ALLOCATE(this%inter_index_x(this%outnx,this%outny), &
     this%inter_index_y(this%outnx,this%outny))

    CALL find_index(in, this%trans%sub_type, &
     this%innx, this%inny, xmin, xmax, ymin, ymax, &
     out%dim%lon, out%dim%lat, this%trans%extrap, &
     this%inter_index_x, this%inter_index_y)

    IF ( this%trans%sub_type == 'bilin' ) THEN
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
     'init_grid_transform inter sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.

  ENDIF

ELSE IF (this%trans%trans_type == 'boxinter') THEN

  CALL get_val(in, nx=this%innx, ny=this%inny)
  CALL get_val(out, nx=this%outnx, ny=this%outny, &
   xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
! TODO now box size is ignored
! if box size not provided, use the actual grid step
  IF (.NOT.c_e(this%trans%area_info%boxdx)) &
   CALL get_val(out, dx=this%trans%area_info%boxdx)
  IF (.NOT.c_e(this%trans%area_info%boxdy)) &
   CALL get_val(out, dx=this%trans%area_info%boxdy)
! half size is actually needed
  this%trans%area_info%boxdx = this%trans%area_info%boxdx*0.5D0
  this%trans%area_info%boxdy = this%trans%area_info%boxdy*0.5D0
! unlike before, here index arrays must have the shape of input grid
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))

! compute coordinates of input grid in geo system
  CALL unproj(in) ! TODO costringe a dichiarare in INTENT(inout), si puo` evitare?
! use find_index in the opposite way, here extrap does not make sense
  CALL find_index(out, 'near', &
   this%outnx, this%outny, xmin, xmax, ymin, ymax, &
   in%dim%lon, in%dim%lat, .FALSE., &
   this%inter_index_x, this%inter_index_y)

ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

  CALL copy(in,out)

  CALL get_val(in, nx=nx, ny=ny)
  this%innx = nx
  this%inny = ny
  this%outnx = nx
  this%outny = ny

ELSE

  CALL l4f_category_log(this%category,L4F_WARN, &
   'init_grid_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  this%valid = .FALSE.

ENDIF

END SUBROUTINE grid_transform_init


!> Constructor for a \a grid_transform object, defining a particular
!! grid-to-sparse points transformation.
!! It defines an object describing a transformation from a rectangular
!! grid to a set of sparse points; the abstract type of transformation
!! is described in the transformation object \a trans (type
!! transform_def) which must have been properly initialised. The
!! additional information required here is the description of the
!! input grid \a in (type griddim_def), and, if required by the
!! transformation type, the information about the target sparse points
!! over which the transformation should take place:
!!
!!  - for 'inter' transformation, this is provided in the form of a
!!    vol7d object (\a v7d_out argument, input), which must have been
!!    initialized with the coordinates of desired sparse points
!!
!!  - for 'polyinter' transformation, this is a list of polygons (\a
!!    poly argument), and, in this case, \a v7d_out is an output
!!    argument which returns the coordinates of the target points
!!    (polygons' centroids)
!!
!!  - for 'metamorphosis' transformation, no target point information
!!    has to be provided in input (it is calculated on the basis of
!!    output grid), and, as for 'polyinter', this information is
!!    returned in output in \a v7d_out argument.
!!
!! The generated \a grid_transform object is specific to the grid and
!! sparse point list provided or computed. The function \a c_e can be
!! used in order to check whether the object has been successfully
!! initialised, if the result is \a .FALSE., it should not be used
!! further on.
SUBROUTINE grid_transform_grid_vol7d_init(this, trans, in, v7d_out, poly, &
 categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(inout) :: in !< griddim object to transform
TYPE(vol7d),INTENT(inout) :: v7d_out !< vol7d object with the coordinates of the sparse points to be used as transformation target (input or output depending on type of transformation)
TYPE(geo_coordvect),INTENT(inout),OPTIONAL :: poly(:) !< array of polygons indicating areas over which to interpolate (for transformation type 'polyinter')
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: nx, ny, i, j, n, nprev
DOUBLE PRECISION :: xmin, xmax, ymin, ymax, steplon, steplat,xmin_new, ymin_new
doubleprecision,pointer :: lon(:),lat(:)
integer :: ix,iy
TYPE(geo_coord) :: point


CALL grid_transform_init_common(this, trans, categoryappend)

IF (this%trans%trans_type == 'inter') THEN

  IF (this%trans%sub_type == 'near' .OR. this%trans%sub_type == 'bilin' ) THEN

    CALL get_val(in, nx=nx, ny=ny)
    this%innx=nx
    this%inny=ny

    this%outnx=SIZE(v7d_out%ana)
    this%outny=1

    ALLOCATE (this%inter_index_x(this%outnx,this%outny),&
     this%inter_index_y(this%outnx,this%outny))
    ALLOCATE(lon(this%outnx),lat(this%outnx))

    CALL get_val(in, &
     xmin=xmin, xmax=xmax,&
     ymin=ymin, ymax=ymax)

    CALL getval(v7d_out%ana(:)%coord,lon=lon,lat=lat)

    CALL find_index(in, this%trans%sub_type,&
     this%innx, this%inny, xmin, xmax, ymin, ymax, &
     lon, lat, this%trans%extrap, &
     this%inter_index_x(:,1), this%inter_index_y(:,1))

! To print the original coordinate
!    CALL unproj(in)
!    PRINT*,in%dim%lon(this%inter_index_x(1,1),this%inter_index_y(1,1)), &
!     in%dim%lat(this%inter_index_x(1,1),this%inter_index_y(1,1))

    IF ( this%trans%sub_type == 'bilin' ) THEN
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
     'grid_transform_init inter sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.
    
  ENDIF

ELSE IF (this%trans%trans_type == 'polyinter') THEN

  IF (.NOT.PRESENT(poly)) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'grid_transform_init poly argument missing for polyinter transformation')
    CALL raise_fatal_error()
  ENDIF

  CALL get_val(in, nx=this%innx, ny=this%inny)
! unlike before, here index arrays must have the shape of input grid
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))
  this%inter_index_x(:,:) = imiss
  this%inter_index_y(:,:) = 1

! compute coordinates of input grid in geo system
  CALL unproj(in) ! TODO costringe a dichiarare in INTENT(inout), si puo` evitare?

  nprev = 1
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(j, i, point) FIRSTPRIVATE(nprev)
  DO j = 1, this%inny
    inside_x: DO i = 1, this%innx
      CALL init(point, lon=in%dim%lon(i,j), lat=in%dim%lat(i,j))

      DO n = nprev, SIZE(poly) ! optimize starting from last matched polygon
        IF (inside(point, poly(n))) THEN ! stop at the first matching polygon
          this%inter_index_x(i,j) = n
          nprev = n
          CYCLE inside_x
        ENDIF
      ENDDO
      DO n = nprev-1, 1, -1 ! test the other polygons
        IF (inside(point, poly(n))) THEN ! stop at the first matching polygon
          this%inter_index_x(i,j) = n
          nprev = n
          CYCLE inside_x
        ENDIF
      ENDDO

!     CALL delete(point) ! speedup
    ENDDO inside_x
  ENDDO
!$OMP END PARALLEL

  this%outnx=SIZE(poly)
  this%outny=1
  CALL vol7d_alloc(v7d_out, nana=SIZE(poly))

! setup output point list, equal to average of polygon points
! warning, in case of concave areas points may coincide!
  DO n = 1, SIZE(poly)
    CALL getval(poly(n), lon=lon, lat=lat)
    CALL init(v7d_out%ana(n), lon=stat_average(lon), lat=stat_average(lat))
    DEALLOCATE(lon, lat)
  ENDDO

ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

  IF (this%trans%sub_type == 'all' ) THEN

    CALL get_val(in, nx=nx, ny=ny)
    this%innx=nx
    this%inny=ny
    this%outnx=nx*ny
    this%outny=1
    CALL vol7d_alloc(v7d_out, nana=nx*ny)

! compute coordinates of input grid in geo system
    CALL unproj(in) ! TODO costringe a dichiarare in INTENT(inout), si puo` evitare?
    do iy=1,this%inny
      do ix=1,this%innx
        CALL init(v7d_out%ana((iy-1)*this%innx+ix), &
         lon=in%dim%lon(ix,iy),lat=in%dim%lat(ix,iy))
      end do
    end do

  ELSE IF (this%trans%sub_type == 'coordbb' ) THEN

! compute coordinates of input grid in geo system
    CALL unproj(in)
    CALL get_val(in, nx=nx, ny=ny)
    this%innx=nx
    this%inny=ny

    ALLOCATE(this%point_mask(nx,ny))
    this%point_mask(:,:) = .FALSE.

! count and mark points falling into requested bounding-box
    this%outnx = 0
    this%outny = 1
    DO iy = 1, this%inny
      DO ix = 1, this%innx
!        IF (geo_coord_inside_rectang()
        IF (in%dim%lon(ix,iy) > this%trans%rect_coo%ilon .AND. &
         in%dim%lon(ix,iy) < this%trans%rect_coo%flon .AND. &
         in%dim%lat(ix,iy) > this%trans%rect_coo%ilat .AND. &
         in%dim%lat(ix,iy) < this%trans%rect_coo%flat) THEN ! improve!
          this%outnx = this%outnx + 1
          this%point_mask(ix,iy) = .TRUE.
        ENDIF
      ENDDO
    ENDDO

    IF (this%outnx <= 0) THEN

      CALL l4f_category_log(this%category,L4F_ERROR, &
       "metamorphosis coordbb: no points inside bounding box "//&
       TRIM(to_char(this%trans%rect_coo%ilon))//","// &
       TRIM(to_char(this%trans%rect_coo%flon))//","// &
       TRIM(to_char(this%trans%rect_coo%ilat))//","// &
       TRIM(to_char(this%trans%rect_coo%flat)))
      this%valid = .FALSE.
      RETURN
      !CALL raise_fatal_error() ! really fatal error?

    ENDIF

    CALL vol7d_alloc(v7d_out, nana=this%outnx)

! collect coordinates of points falling into requested bounding-box
    n = 0
    metamorphosis_coordbb: DO iy = 1, this%inny
      DO ix = 1, this%innx
!        IF (geo_coord_inside_rectang()
        IF (in%dim%lon(ix,iy) > this%trans%rect_coo%ilon .AND. &
         in%dim%lon(ix,iy) < this%trans%rect_coo%flon .AND. &
         in%dim%lat(ix,iy) > this%trans%rect_coo%ilat .AND. &
         in%dim%lat(ix,iy) < this%trans%rect_coo%flat) THEN ! improve!
          n = n + 1
          IF (n > this%outnx) EXIT metamorphosis_coordbb ! useless safety check
          CALL init(v7d_out%ana(n),lon=in%dim%lon(ix,iy),lat=in%dim%lat(ix,iy))
        ENDIF
      ENDDO
    ENDDO metamorphosis_coordbb

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'grid_transform_init metamorphosis sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.

  ENDIF

ELSE

  CALL l4f_category_log(this%category,L4F_WARN, &
   'init_grid_v7d_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  this%valid = .FALSE.

ENDIF

END SUBROUTINE grid_transform_grid_vol7d_init


!> Constructor for a \a grid_transform object, defining a particular
!! sparse points-to-grid transformation.
!! It defines an object describing a transformation from a set of
!! sparse points to a rectangular grid; the abstract type of
!! transformation is described in the transformation object \a trans
!! (type transform_def) which must have been properly initialised. The
!! additional information required here is the list of the input
!! sparse points in the form of a \a vol7d object (parameter \a v7d_in),
!! which can be the same volume that will be successively used for
!! interpolation, or a volume with just the same coordinate data, and
!! the description of the output grid \a griddim (a \a griddim_def
!! object).
!!
!! The generated \a grid_transform object is specific to the sparse
!! point list and grid provided. The function \a c_e can be used in
!! order to check whether the object has been successfully
!! initialised, if the result is \a .FALSE., it should not be used
!! further on.
SUBROUTINE grid_transform_vol7d_grid_init(this, trans, v7d_in, out, categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(vol7d),INTENT(in) :: v7d_in !< vol7d object with the coordinates of the sparse point to be used as input (only information about coordinates is used)
TYPE(griddim_def),INTENT(in) :: out !< griddim object defining target grid
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: nx, ny,i,j
DOUBLE PRECISION :: xmin, xmax, ymin, ymax, steplon, steplat,xmin_new, ymin_new
doubleprecision,allocatable :: lon(:),lat(:)


CALL grid_transform_init_common(this, trans, categoryappend)

IF (this%trans%trans_type == 'inter') THEN

  IF ( this%trans%sub_type == 'linear' ) THEN
    
    CALL get_val(out, nx=nx, ny=ny)
    this%outnx=nx
    this%outny=ny
  
    this%innx=SIZE(v7d_in%ana)
    this%inny=1
  
    ALLOCATE(lon(this%innx),lat(this%innx))
    ALLOCATE(this%inter_xp(this%innx,this%inny),this%inter_yp(this%innx,this%inny))
    ALLOCATE(this%inter_x(this%outnx,this%outny),this%inter_y(this%outnx,this%outny))

    CALL get_val(out, &
     xmin=xmin, xmax=xmax,&
     ymin=ymin, ymax=ymax)

    CALL getval(v7d_in%ana(:)%coord,lon=lon,lat=lat)

    CALL proj(out,&
     RESHAPE(lon,(/SIZE(lon),1/)),RESHAPE(lat,(/SIZE(lat),1/)),&
     this%inter_xp,this%inter_yp)

    CALL griddim_gen_coord(out, this%inter_x, this%inter_y)

    DEALLOCATE(lon,lat)

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_v7d_grid_transform inter sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.

  ENDIF

ELSE IF (this%trans%trans_type == 'boxinter') THEN

  this%innx=SIZE(v7d_in%ana)
  this%inny=1
  CALL get_val(out, nx=this%outnx, ny=this%outny, &
   xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
! TODO now box size is ignored
! if box size not provided, use the actual grid step
  IF (.NOT.c_e(this%trans%area_info%boxdx)) &
   CALL get_val(out, dx=this%trans%area_info%boxdx)
  IF (.NOT.c_e(this%trans%area_info%boxdy)) &
   CALL get_val(out, dx=this%trans%area_info%boxdy)
! half size is actually needed
  this%trans%area_info%boxdx = this%trans%area_info%boxdx*0.5D0
  this%trans%area_info%boxdy = this%trans%area_info%boxdy*0.5D0
! index arrays must have the shape of input grid
  ALLOCATE(lon(this%innx),lat(this%innx))
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))

! get coordinates of input grid in geo system
  CALL getval(v7d_in%ana(:)%coord,lon=lon,lat=lat)
! use find_index in the opposite way, here extrap does not make sense
  CALL find_index(out,'near',&
   this%outnx, this%outny , xmin, xmax, ymin, ymax, &
   lon, lat, .FALSE., &
   this%inter_index_x(:,1), this%inter_index_y(:,1))

ELSE

  CALL l4f_category_log(this%category,L4F_WARN, &
   'init_v7d_grid_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  this%valid = .FALSE.

ENDIF

END SUBROUTINE grid_transform_vol7d_grid_init


!> Constructor for a \a grid_transform object, defining a particular
!! sparse points-to-sparse points transformation.
!! It defines an object describing a transformation from a set of
!! sparse points to a set of sparse points; the abstract type of
!! transformation is described in the transformation object \a trans
!! (type transform_def) which must have been properly initialised. The
!! additional information required here is the list of the input
!! sparse points in the form of a \a vol7d object (parameter \a
!! v7d_in), which can be the same volume that will be successively
!! used for interpolation, or a volume with just the same coordinate
!! data, and the information about the target sparse points over which
!! the transformation should take place:
!!
!!  - for 'inter' transformation, this is provided in the form of a
!!    vol7d object (\a v7d_out argument, input), which must have been
!!    initialized with the coordinates of desired sparse points
!!
!!  - for 'polyinter' transformation, this is a list of polygons (\a
!!    poly argument), and, in this case, \a v7d_out is an output
!!    argument which returns the coordinates of the target points
!!    (polygons' centroids)
!!
!! The generated \a grid_transform object is specific to the input and
!! output sparse point lists provided or computed. The function \a c_e
!! can be used in order to check whether the object has been
!! successfully initialised, if the result is \a .FALSE., it should
!! not be used further on.
SUBROUTINE grid_transform_vol7d_vol7d_init(this, trans, v7d_in, v7d_out, poly, &
 categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(vol7d),INTENT(in) :: v7d_in !< vol7d object with the coordinates of the sparse point to be used as input (only information about coordinates is used)
TYPE(vol7d),INTENT(inout) :: v7d_out !< vol7d object with the coordinates of the sparse points to be used as transformation target (input or output depending on type of transformation, when output, it must have been initialised anyway)
TYPE(geo_coordvect),INTENT(inout),OPTIONAL :: poly(:) !< array of polygons indicating areas over which to interpolate (for transformation type 'polyinter')
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: i, n
DOUBLE PRECISION,POINTER :: lon(:), lat(:)


CALL grid_transform_init_common(this, trans, categoryappend)

IF (this%trans%trans_type == 'inter') THEN

  IF ( this%trans%sub_type == 'linear' ) THEN
    
    CALL vol7d_alloc_vol(v7d_out) ! be safe
    this%outnx=SIZE(v7d_out%ana)
    this%outny=1
  
    this%innx=SIZE(v7d_in%ana)
    this%inny=1
  
    ALLOCATE(this%inter_xp(this%innx,this%inny),this%inter_yp(this%innx,this%inny))
    ALLOCATE(this%inter_x(this%outnx,this%outny),this%inter_y(this%outnx,this%outny))

    CALL getval(v7d_in%ana(:)%coord,lon=this%inter_xp(:,1),lat=this%inter_yp(:,1))
    CALL getval(v7d_out%ana(:)%coord,lon=this%inter_x(:,1),lat=this%inter_y(:,1))

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'init_v7d_v7d_transform inter sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.

  ENDIF

ELSE IF (this%trans%trans_type == 'polyinter') THEN

  IF (.NOT.PRESENT(poly)) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'grid_transform_init poly argument missing for polyinter transformation')
    this%valid = .FALSE.
    RETURN
  ENDIF

  this%innx=SIZE(v7d_in%ana)
  this%inny=1
! unlike before, here index arrays must have the shape of input grid
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))
  this%inter_index_x(:,:) = imiss
  this%inter_index_y(:,:) = 1

! compute coordinates of polygons in geo system
!  DO n = 1, SIZE(poly)
!    CALL to_geo(poly(n))
!  ENDDO

  DO i = 1, SIZE(v7d_in%ana)
    DO n = 1, SIZE(poly)
      IF (inside(v7d_in%ana(i)%coord, poly(n))) THEN ! stop at the first matching polygon
        this%inter_index_x(i,1) = n
        EXIT
      ENDIF
    ENDDO
  ENDDO

  this%outnx=SIZE(poly)
  this%outny=1
  CALL vol7d_alloc(v7d_out, nana=SIZE(poly))

! setup output point list, equal to average of polygon points
! warning, in case of concave areas points may coincide!
  DO n = 1, SIZE(poly)
    CALL getval(poly(n), lon=lon, lat=lat)
    CALL init(v7d_out%ana(n), lon=stat_average(lon), lat=stat_average(lat))
    DEALLOCATE(lon, lat)
  ENDDO

ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

  IF (this%trans%sub_type == 'all' ) THEN

    this%innx=SIZE(v7d_in%ana)
    this%inny=1
    this%outnx=SIZE(v7d_in%ana)
    this%outny=1
    CALL vol7d_alloc(v7d_out, nana=SIZE(v7d_in%ana))
    v7d_out%ana = v7d_in%ana

  ELSE IF (this%trans%sub_type == 'coordbb' ) THEN

! compute coordinates of input grid in geo system
    this%innx=SIZE(v7d_in%ana)
    this%inny=1

    ALLOCATE(this%point_mask(this%innx,this%inny))
    this%point_mask(:,:) = .FALSE.
    ALLOCATE(lon(this%innx),lat(this%innx))

! count and mark points falling into requested bounding-box
    this%outnx = 0
    this%outny = 1
    CALL getval(v7d_in%ana(:)%coord,lon=lon,lat=lat)
    DO i = 1, this%innx
!      IF (geo_coord_inside_rectang()
      IF (lon(i) > this%trans%rect_coo%ilon .AND. &
       lon(i) < this%trans%rect_coo%flon .AND. &
       lat(i) > this%trans%rect_coo%ilat .AND. &
       lat(i) < this%trans%rect_coo%flat) THEN ! improve!
        this%outnx = this%outnx + 1
        this%point_mask(i,1) = .TRUE.
      ENDIF
    ENDDO

    IF (this%outnx <= 0) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       "metamorphosis coordbb: no points inside bounding box "//&
       TRIM(to_char(this%trans%rect_coo%ilon))//","// &
       TRIM(to_char(this%trans%rect_coo%flon))//","// &
       TRIM(to_char(this%trans%rect_coo%ilat))//","// &
       TRIM(to_char(this%trans%rect_coo%flat)))
      this%valid = .FALSE.
      DEALLOCATE(lon, lat)
      RETURN
      !CALL raise_fatal_error() ! really fatal error?
    ENDIF

    CALL vol7d_alloc(v7d_out, nana=this%outnx)

! collect coordinates of points falling into requested bounding-box
    n = 0
    DO i = 1, this%innx
      IF (this%point_mask(i,1)) THEN
        n = n + 1
        CALL init(v7d_out%ana(n),lon=lon(i),lat=lat(i))
      ENDIF
    ENDDO
    DEALLOCATE(lon, lat)

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN, &
     'grid_transform_init metamorphosis sub_type '//TRIM(this%trans%sub_type) &
     //' not supported')
    this%valid = .FALSE.

  ENDIF

ELSE

  CALL l4f_category_log(this%category,L4F_WARN, &
   'init_v7d_v7d_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  this%valid = .FALSE.
ENDIF

END SUBROUTINE grid_transform_vol7d_vol7d_init


! Private subroutine for performing operations common to all constructors
SUBROUTINE grid_transform_init_common(this, trans, categoryappend)
TYPE(grid_transform),INTENT(inout) :: this
TYPE(transform_def),INTENT(in) :: trans
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend

CHARACTER(len=512) :: a_name


this%valid = .TRUE.
CALL l4f_launcher(a_name, a_name_append= &
 TRIM(subcategory)//"."//TRIM(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,"start init_grid_transform")
#endif

this%trans=trans
NULLIFY(this%inter_index_x)
NULLIFY(this%inter_index_y)
NULLIFY(this%inter_x)
NULLIFY(this%inter_y)
NULLIFY(this%inter_xp)
NULLIFY(this%inter_yp)
NULLIFY(this%point_mask)

END SUBROUTINE grid_transform_init_common


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
if (associated(this%point_mask)) deallocate (this%point_mask)
this%valid = .FALSE.

! close the logger
call l4f_category_delete(this%category)

END SUBROUTINE grid_transform_delete


!> Returns \a .TRUE. if, after \a init , the corresponding \a grid_transform
!! object has been correctly initilised.
FUNCTION grid_transform_c_e(this)
TYPE(grid_transform),INTENT(in) :: this !< grid_transform object
LOGICAL :: grid_transform_c_e

grid_transform_c_e = this%valid

END FUNCTION grid_transform_c_e


!> Compute the output data array from input data array according to
!! the defined transformation. The \a grid_transform object \a this
!! must have been properly initialised, so that it contains all the
!! information needed for computing the transformation. This is the
!! grid-to-grid and grid-to-sparse points version. In the case of
!! grid-to-sparse points transformation, the output argument is still
!! a 2-d array with shape \a (np,1), which may have to be reshaped and
!! assigned to the target 1-d array after the subroutine call by means
!! of the \a RESHAPE() intrinsic function.
SUBROUTINE grid_transform_compute(this, field_in, field_out)
TYPE(grid_transform),INTENT(in) :: this !< grid_transformation object
REAL, INTENT(in) :: field_in(:,:,:) !< input array
REAL, INTENT(out) :: field_out(:,:,:) !< output array

INTEGER :: i, j, k, ii, jj, ie, je, navg
INTEGER,ALLOCATABLE :: nval(:,:)
real :: z1,z2,z3,z4
doubleprecision  :: x1,x3,y1,y3,xp,yp
INTEGER :: innx, inny, innz, outnx, outny, outnz


#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start grid_transform_compute")
#endif

field_out(:,:,:) = rmiss

IF (.NOT.this%valid) THEN
  call l4f_category_log(this%category,L4F_ERROR, &
   "refusing to perform a non valid transformation")
  RETURN
ENDIF

innx = SIZE(field_in,1); inny = SIZE(field_in,2); innz = SIZE(field_in,3)
outnx = SIZE(field_out,1); outny = SIZE(field_out,2); outnz = SIZE(field_out,3)

! check size of field_in, field_out
IF (innx /= this%innx .OR. inny /= this%inny) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent in shape: "//&
   TRIM(to_char(this%innx))//","//TRIM(to_char(this%inny))//" /= "//&
   TRIM(to_char(innx))//","//TRIM(to_char(inny)))
  CALL raise_error()
  RETURN
ENDIF

IF (outnx /= this%outnx .OR. outny /= this%outny) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent out shape: "//&
   TRIM(to_char(this%outnx))//","//TRIM(to_char(this%outny))//" /= "//&
   TRIM(to_char(outnx))//","//TRIM(to_char(outny)))
  CALL raise_error()
  RETURN
ENDIF

IF (innz /= outnz .AND. this%trans%trans_type /= 'vertint') THEN
  CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent vertical sizes: "//&
   TRIM(to_char(innz))//" /= "//TRIM(to_char(outnz)))
  CALL raise_error()
  RETURN
ENDIF

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG, &
   "start grid_transform_compute "//TRIM(this%trans%trans_type)//':'// &
   TRIM(this%trans%sub_type))
#endif

IF (this%trans%trans_type == 'zoom') THEN

  field_out(this%outinx:this%outfnx, &
   this%outiny:this%outfny,:) = &
   field_in(this%iniox:this%infox, &
   this%inioy:this%infoy,:)

ELSE IF (this%trans%trans_type == 'boxregrid') THEN

  field_out(:,:,:) = rmiss

  IF (this%trans%sub_type == 'average') THEN
    DO k = 1, innz
      jj = 0
      DO j = 1, this%inny - this%trans%box_info%npy + 1, this%trans%box_info%npy
        je = j+this%trans%box_info%npy-1
        jj = jj+1
        ii = 0
        DO i = 1, this%innx - this%trans%box_info%npx + 1, this%trans%box_info%npx
          ie = i+this%trans%box_info%npx-1
          ii = ii+1
          navg = COUNT(field_in(i:ie,j:je,k) /= rmiss)
          IF (navg > 0) THEN
            field_out(ii,jj,k) = SUM(field_in(i:ie,j:je,k)/navg, &
             MASK=(field_in(i:ie,j:je,k) /= rmiss))
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  ELSE IF (this%trans%sub_type == 'max') THEN
    DO k = 1, innz
      jj = 0
      DO j = 1, this%inny - this%trans%box_info%npy + 1, this%trans%box_info%npy
        je = j+this%trans%box_info%npy-1
        jj = jj+1
        ii = 0
        DO i = 1, this%innx - this%trans%box_info%npx + 1, this%trans%box_info%npx
          ie = i+this%trans%box_info%npx-1
          ii = ii+1
          navg = COUNT(field_in(i:ie,j:je,k) /= rmiss)
          IF (navg > 0) THEN
            field_out(ii,jj,k) = MAXVAL(field_in(i:ie,j:je,k), &
             MASK=(field_in(i:ie,j:je,k) /= rmiss))
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  ELSE IF (this%trans%sub_type == 'min') THEN
    DO k = 1, innz
      jj = 0
      DO j = 1, this%inny - this%trans%box_info%npy + 1, this%trans%box_info%npy
        je = j+this%trans%box_info%npy-1
        jj = jj+1
        ii = 0
        DO i = 1, this%innx - this%trans%box_info%npx + 1, this%trans%box_info%npx
          ie = i+this%trans%box_info%npx-1
          ii = ii+1
          navg = COUNT(field_in(i:ie,j:je,k) /= rmiss)
          IF (navg > 0) THEN
            field_out(ii,jj,k) = MINVAL(field_in(i:ie,j:je,k), &
             MASK=(field_in(i:ie,j:je,k) /= rmiss))
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  ENDIF

ELSE IF (this%trans%trans_type == 'inter') THEN

  IF (this%trans%sub_type == 'near') THEN

    DO k = 1, innz
      DO j = 1, this%outny 
        DO i = 1, this%outnx 

          if (c_e(this%inter_index_x(i,j)) .and. c_e(this%inter_index_y(i,j)))&
           field_out(i,j,k) = &
           field_in(this%inter_index_x(i,j),this%inter_index_y(i,j),k)

        ENDDO
      ENDDO
    ENDDO

  else if (this%trans%sub_type == 'bilin') THEN
    
    DO k = 1, innz
      DO j = 1, this%outny 
        DO i = 1, this%outnx 

          IF (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j)))THEN

            z1=field_in(this%inter_index_x(i,j),this%inter_index_y(i,j),k)
            z2=field_in(this%inter_index_x(i,j)+1,this%inter_index_y(i,j),k)
            z3=field_in(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1,k)
            z4=field_in(this%inter_index_x(i,j),this%inter_index_y(i,j)+1,k)

            IF (c_e(z1) .AND. c_e(z2) .AND. c_e(z3) .AND. c_e(z4)) THEN

              x1=this%inter_x(this%inter_index_x(i,j),this%inter_index_y(i,j))
              y1=this%inter_y(this%inter_index_x(i,j),this%inter_index_y(i,j))
              x3=this%inter_x(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1)
              y3=this%inter_y(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1)

              xp=this%inter_xp(i,j)
              yp=this%inter_yp(i,j)

              field_out(i,j,k) = hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp)

            END IF
          END IF

        ENDDO
      ENDDO
    ENDDO

  ENDIF
ELSE IF (this%trans%trans_type == 'boxinter' &
 .OR. this%trans%trans_type == 'polyinter') THEN

  IF (this%trans%sub_type == 'average') THEN
    
    ALLOCATE(nval(this%outnx, this%outny))
    field_out(:,:,:) = 0.0
    DO k = 1, innz
      nval(:,:) = 0
      DO j = 1, this%inny
        DO i = 1, this%innx
          IF (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j)) &
           .AND. c_e(field_in(i,j,k))) THEN
            field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k) = &
             field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k) + &
             field_in(i,j,k)
            nval(this%inter_index_x(i,j),this%inter_index_y(i,j)) = &
             nval(this%inter_index_x(i,j),this%inter_index_y(i,j)) + 1
          ENDIF
        ENDDO
      ENDDO
      WHERE (nval(:,:) /= 0)
        field_out(:,:,k) = field_out(:,:,k)/nval(:,:)
      ELSEWHERE
        field_out(:,:,k) = rmiss
      END WHERE
    ENDDO
    DEALLOCATE(nval)

  ELSE IF (this%trans%sub_type == 'max') THEN

    field_out(:,:,:) = rmiss
    DO k = 1, innz
      DO j = 1, this%inny
        DO i = 1, this%innx
          IF (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j))) THEN
            IF (c_e(field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k))) THEN
              field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k) = &
               MAX(field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k), &
               field_in(i,j,k))
            ELSE
              field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k) = &
               field_in(i,j,k)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO


  ELSE IF (this%trans%sub_type == 'min') THEN

    field_out(:,:,:) = rmiss
    DO j = 1, this%inny
      DO i = 1, this%innx
        IF (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j))) THEN
          IF (c_e(field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k))) THEN
            field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k) = &
             MIN(field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k), &
             field_in(i,j,k))
          ELSE
            field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k) = &
             field_in(i,j,k)
          ENDIF
        ENDIF
      ENDDO
    ENDDO

  ELSE IF (this%trans%sub_type == 'percentile') THEN

    DO k = 1, innz
      DO j = 1, this%outny
        DO i = 1, this%outnx
! da paura
          field_out(i:i,j,k) = stat_percentile( &
           RESHAPE(field_in(:,:,k), (/SIZE(field_in(:,:,k))/)), &
           (/REAL(this%trans%stat_info%percentile)/), &
           mask=RESHAPE((this%inter_index_x == i .AND. &
           this%inter_index_y == j), (/SIZE(field_in(:,:,k))/)))
        ENDDO
      ENDDO
    ENDDO

  ENDIF


ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

  IF (this%trans%sub_type == 'all') THEN

    field_out(:,:,:) = RESHAPE(field_in(:,:,:), (/this%outnx,this%outny,innz/))

  ELSE IF (this%trans%sub_type == 'coordbb') THEN

    DO k = 1, innz
      field_out(:,1,k) = PACK(field_in(:,:,k), this%point_mask(:,:))
    ENDDO
    
  ENDIF

ELSE IF (this%trans%trans_type == '' .OR. this%trans%trans_type == 'none') THEN

  field_out(:,:,:) = field_in(:,:,:)

ENDIF

END SUBROUTINE grid_transform_compute


!> Compute the output data array from input data array according to
!! the defined transformation. The \a grid_transform object \a this
!! must have been properly initialised, so that it contains all the
!! information needed for computing the transformation. This is the
!! sparse points-to-grid and sparse points-to-sparse points version.
SUBROUTINE grid_transform_v7d_grid_compute(this, field_in, field_out)
TYPE(grid_transform),INTENT(in) :: this !< grid_tranform object
REAL, INTENT(in) :: field_in(:,:) !< input array
REAL, INTENT(out):: field_out(:,:,:) !< output array

real,allocatable :: field_in_p(:),x_in_p(:),y_in_p(:)
real,allocatable :: x_out(:),y_out(:)
INTEGER :: inn_p, ier, k
INTEGER :: innx, inny, innz, outnx, outny, outnz

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start v7d_grid_transform_compute")
#endif

field_out(:,:,:) = rmiss

IF (.NOT.this%valid) THEN
  call l4f_category_log(this%category,L4F_ERROR, &
   "refusing to perform a non valid transformation")
  RETURN
ENDIF

innx = SIZE(field_in,1); inny = 1; innz = SIZE(field_in,2)
outnx = SIZE(field_out,1); outny = SIZE(field_out,2); outnz = SIZE(field_out,3)

! check size of field_in, field_out
IF (innx /= this%innx .OR. inny /= this%inny) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent in shape: "//&
   TRIM(to_char(this%innx))//","//TRIM(to_char(this%inny))//" /= "//&
   TRIM(to_char(innx))//","//TRIM(to_char(inny)))
  CALL raise_error()
  RETURN
end if

IF (outnx /= this%outnx .OR. outny /= this%outny) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent out shape: "//&
   TRIM(to_char(this%outnx))//","//TRIM(to_char(this%outny))//" /= "//&
   TRIM(to_char(outnx))//","//TRIM(to_char(outny)))
  CALL raise_error()
  RETURN
ENDIF

IF (innz /= outnz .AND. this%trans%trans_type /= 'vertint') THEN
  CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent vertical sizes: "//&
   TRIM(to_char(innz))//" /= "//TRIM(to_char(outnz)))
  CALL raise_error()
  RETURN
ENDIF

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG, &
   "start grid_transform_v7d_grid_compute "//TRIM(this%trans%trans_type)//':'// &
   TRIM(this%trans%sub_type))
#endif

IF (this%trans%trans_type == 'inter') THEN

  IF (this%trans%sub_type == 'linear') THEN

#ifdef HAVE_LIBNGMATH
! optimization, allocate only once with a safe size
    ALLOCATE(field_in_p(innx*inny), x_in_p(innx*inny), y_in_p(innx*inny))
    DO k = 1, innz
      inn_p = COUNT(c_e(field_in(:,k)))

      CALL l4f_category_log(this%category,L4F_INFO, &
       "Number of sparse data points: "//TRIM(to_char(inn_p)))

      IF (inn_p > 2) THEN

        field_in_p(1:inn_p) = PACK(field_in(:,k), c_e(field_in(:,k)))
        x_in_p(1:inn_p) = PACK(this%inter_xp(:,1), c_e(field_in(:,k)))
        y_in_p(1:inn_p) = PACK(this%inter_yp(:,1), c_e(field_in(:,k)))

        CALL NATGRIDS(inn_p, x_in_p, y_in_p, field_in_p, & ! (1:inn_p) omitted
         this%outnx, this%outny, REAL(this%inter_x(:,1)), & ! no f90 interface
         REAL(this%inter_y(1,:)), field_out(1,1,k), ier)

        IF (ier /= 0) THEN
          CALL l4f_category_log(this%category,L4F_ERROR, &
           "Error return from NATGRIDD = "//TRIM(to_char(ier)))
          CALL raise_error()
          EXIT
        ENDIF ! exit loop to deallocate
      ELSE

        CALL l4f_category_log(this%category,L4F_INFO, &
         "insufficient data in gridded region to triangulate")

      ENDIF
    ENDDO
    DEALLOCATE(field_in_p, x_in_p, y_in_p)

#else
    CALL l4f_category_log(this%category,L4F_ERROR, &
     "libsim compiled without NATGRIDD (ngmath ncarg library)")
    CALL raise_error()
    RETURN
#endif

  ENDIF

ELSE IF (this%trans%trans_type == 'boxinter' &
 .OR. this%trans%trans_type == 'polyinter') THEN ! use the grid-to-grid method

  CALL compute(this, &
   RESHAPE(field_in, (/SIZE(field_in,1), 1, SIZE(field_in,2)/)), field_out)

ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

  IF (this%trans%sub_type == 'all') THEN

    field_out(:,:,:) = RESHAPE(field_in(:,:), (/this%outnx,this%outny,innz/))

  ELSE IF (this%trans%sub_type == 'coordbb') THEN

    DO k = 1, innz
      field_out(:,1,k) = PACK(field_in(:,k), this%point_mask(:,1))
    ENDDO

  ENDIF

ENDIF

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
ELEMENTAL SUBROUTINE find_index(this, inter_type, nx, ny, xmin, xmax, ymin, ymax, &
 lon, lat, extrap, index_x, index_y)
TYPE(griddim_def),INTENT(in) :: this ! griddim object (from grid)
CHARACTER(len=*),INTENT(in) :: inter_type ! interpolation type (determine wich point is requested)
INTEGER,INTENT(in) :: nx,ny ! dimension (to grid)
DOUBLE PRECISION,INTENT(in) :: xmin, xmax, ymin, ymax ! extreme coordinate (to grid)
DOUBLE PRECISION,INTENT(in) :: lon,lat ! target coordinate
LOGICAL,INTENT(in) :: extrap ! extrapolate
INTEGER,INTENT(out) :: index_x,index_y ! index of point requested

INTEGER :: lnx, lny
DOUBLE PRECISION :: x,y

IF (inter_type == "near") THEN

  CALL proj(this,lon,lat,x,y)
  index_x = NINT((x-xmin)/((xmax-xmin)/DBLE(nx-1)))+1
  index_y = NINT((y-ymin)/((ymax-ymin)/DBLE(ny-1)))+1
  lnx = nx
  lny = ny

ELSE IF (inter_type == "bilin") THEN

  CALL proj(this,lon,lat,x,y)
  index_x = (x-xmin)/((xmax-xmin)/DBLE(nx-1))+1
  index_y = (y-ymin)/((ymax-ymin)/DBLE(ny-1))+1
  lnx = nx-1
  lny = ny-1

ELSE

  index_x=imiss
  index_y=imiss
  RETURN

ENDIF

IF (extrap) THEN ! trim indices outside grid for extrapolation
  index_x = MAX(index_x, 1)
  index_y = MAX(index_y, 1)
  index_x = MIN(index_x, lnx)
  index_y = MIN(index_y, lny)
ELSE ! nullify indices outside grid
  IF (index_x < 1 .OR. index_x > lnx) index_x = imiss
  IF (index_y < 1 .OR. index_y > lny) index_y = imiss
ENDIF

END SUBROUTINE find_index


END MODULE grid_transform_class
