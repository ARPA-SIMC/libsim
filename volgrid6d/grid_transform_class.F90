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
!! This module performs tranformations at a relatively low-level, on
!! 2d/3d sections of data, and it is meant primarily for use by higher
!! level methods in \a volgrid6d_class, which operate on full volumes
!! of physically determined quantities; however it is possible to use
!! it in a stand-alone way as well.
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
!!    - sub_type='projcoord' the corners of the zoomed/extended area
!!      are defined by projected coordinates
!!    - sub_type='index' the bounds of the zoomed/extended area are
!!      defined by grid indices.
!!
!!  - trans_type='boxregrid' regrids the input data grid on a new grid
!!    in which the value at every point is the result of a function
!!    computed over \a npx X \a npy points of the original grid,
!!    without changing the geographical reference system
!!    (grid-to-grid)
!!    - sub_type='average' the function used is the average.
!!    - sub_type='stddev' the function used is the standard deviation.
!!    - sub_type='stddevnm1' the function used is the standard
!!      deviation computed with n-1.
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!    - sub_type='percentile' the function used is a requested
!!      percentile of the input points distribution.
!!
!!  - trans_type='inter' interpolates the input data on a new set of
!!    specified points
!!    - sub_type='near' the interpolated value is that of the nearest
!!      input point (grid-to-grid, grid-to-sparse point)
!!    - sub_type='bilin' the interpolated value is computed as a
!!      bilinear interpolation of the 4 surrounding input points
!!      (grid-to-grid, grid-to-sparse point)
!!    - sub_type='linear' the interpolated value is computed as a
!!      linear interpolation of the 3 surrounding input points
!!      individuated by means of a triangulation procedure (sparse
!!      points-to-grid, sparse points-to-sparse points).
!!    - sub_type='shapiro_near' the interpolated value is that of sub_type=near
!!      after smoothing the input field with a shapiro filter of order 2.
!!
!!  - trans_type='boxinter' computes data on a new grid in which the
!!    value at every point is the result of a function computed over
!!    the input points lying inside the output point's grid box
!!    (grid-to-grid and sparse points-to-grid)
!!    - sub_type='average' the function used is the average
!!    - sub_type='stddev' the function used is the standard deviation
!!    - sub_type='stddevnm1' the function used is the standard
!!      deviation computed with n-1
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!    - sub_type='percentile' the function used is a requested
!!      percentile of the input points distribution
!!    - sub_type='frequency' the function used is the fraction of
!!      points in the box having value included in a specified
!!      interval.
!!
!!  - trans_type='polyinter' output data are the result of a function
!!    computed over the input points lying inside a set of
!!    georeferenced polygons; polygons cannot overlap each other; for
!!    output on sparse points, the number of output points is equal to
!!    the number of polygons and output point coordinates are defined
!!    as the centroids of the polygons, for output on a grid, the
!!    output grid is equal to the input grid and the value of the
!!    field is repeated for all points within each polygon, while it
!!    is undefined outside all polygons (grid-to-grid, grid-to-sparse
!!    points and sparse points-to-sparse points)
!!    - sub_type='average' the function used is the average
!!    - sub_type='stddev' the function used is the standard deviation.
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!    - sub_type='percentile' the function used is a requested
!!      percentile of the input points distribution.
!!
!!  - trans_type='stencilinter' computes data on a new set of
!!    specified points, for each of which the value is the result of a
!!    function computed over the input grid points forming a circular
!!    stencil centered on the nearest input grid point, with radius \a
!!    radius in input grid point units; stencils can overlap each
!!    other (grid-to-grid and grid-to-sparse points)
!!    - sub_type='average' the function used is the average
!!    - sub_type='stddev' the function used is the standard deviation.
!!    - sub_type='stddevnm1' the function used is the standard
!!      deviation computed with n-1.
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!    - sub_type='percentile' the function used is a requested
!!      percentile of the input points distribution.
!!
!!  - trans_type='maskinter' takes a 2D field on the same grid as the
!!    input points, it divides it in a number of subareas according to
!!    its values and and optional list of boundaries
!!    and every subarea is used as a mask for
!!    interpolation; data are thus computed on a new set of points,
!!    the number of which is equal to the number of subareas, and for
!!    each of which the value is the result of a function computed
!!    over those input points belonging to the corresponding subarea;
!!    the output point coordinates are defined as the centroids of the
!!    subareas (grid-to-sparse points)
!!    - sub_type='average' the function used is the average
!!    - sub_type='stddev' the function used is the standard deviation.
!!    - sub_type='stddevnm1' the function used is the standard
!!      deviation computed with n-1.
!!    - sub_type='max' the function used is the maximum
!!    - sub_type='min' the function used is the minimum
!!    - sub_type='percentile' the function used is a requested
!!      percentile of the input points distribution.
!!
!!  - trans_type='maskgen' generates a mask field, on the same grid as
!!    the input, suitable to be used for 'maskinter' interpolation
!!    (grid-to-grid)
!!    - sub_type='poly' the output mask field contains, at each point,
!!      integer values from 1 to the number of polygons provided,
!!      computed according to the polygon in which every point lies
!!    - sub_type='grid' the output mask field contains, at each point,
!!      integer values from 1 to the number of grid cells in the
!!      pseudo-output grid provided, depending on the index of the
!!      cell in which every point lies.
!!
!!  - trans_type='metamorphosis' the values of the output points are
!!    the same as the input ones, but something external in the data
!!    may change, e.g. only a subset of input data is kept in output
!!    or the underlying data structure changes, as from grid to sparse
!!    points (grid-to-grid, grid-to-sparse points, sparse
!!    points-to-sparse points)
!!    - sub_type='all' all the input points are kept in the output
!!      (grid-to-sparse points)
!!    - sub_type='coordbb' the input points which lie in the provided
!!      lon/lat bounding box are kept in the output (grid-to-sparse
!!      points or sparse points-to-sparse points).
!!    - sub_type='poly' the input points which lie in any of the
!!      polygons provided are kept in the output; points are marked
!!      with the number of polygon they belong to (grid-to-sparse
!!      points or sparse points-to-sparse points).
!!    - sub_type='mask' the input points which belong to any valid
!!      subarea defined by a mask field, as for
!!      trans_type='maskinter', are kept in the output; points are
!!      marked with the number of the subarea they belong to
!!      (grid-to-sparse points).
!!    - sub_type='maskvalid' the input points corresponding to points
!!      having valid data and optionally having values within
!!      requested bounds in a 2-D mask field, are kept in the output;
!!      the other points are filled with missing values
!!      (grid-to-grid).
!!    - sub_type='maskinvalid' the input points corresponding to points
!!      having non valid data in a 2-D mask field, are kept in the
!!      output; the other points are filled with missing values
!!      (grid-to-grid).
!!    - sub_type='setinvalidto' the input points having non valid data
!!      are set to a user-specified constant value (grid-to-grid).
!!    - sub_type='settoinvalid' the input points having values
!!      included in the requested bounds are set to an invalid value,
!!      the others are kept unchanged (grid-to-grid).
!!
!! \ingroup volgrid6d
MODULE grid_transform_class
USE vol7d_class
USE err_handling
USE geo_proj_class
USE grid_class
USE grid_dim_class
USE optional_values
USE array_utilities
USE georef_coord_class
USE simple_stat
IMPLICIT NONE

CHARACTER(len=255),PARAMETER:: subcategory="grid_transform_class"

! information for interpolation aver a rectangular area
TYPE area_info
  double precision :: boxdx ! longitudinal/x extension of the box for box interpolation, default the target x grid step
  double precision :: boxdy ! latitudinal/y extension of the box for box interpolation, default the target y grid step
  double precision :: radius ! radius in gridpoints for stencil interpolation
END TYPE area_info

! information for statistical processing of interpoland data
TYPE stat_info
  DOUBLE PRECISION :: percentile ! percentile [0,100] of the distribution of points in the box to use as interpolated value, if missing, the average is used, if 0.or 100. the MIN() and MAX() are used as a shortcut
END TYPE stat_info

! information for point interval
TYPE interval_info
  REAL :: gt=rmiss ! lower limit of interval, missing for -inf
  REAL :: lt=rmiss ! upper limit of interval, missing for +inf
  LOGICAL :: ge=.TRUE. ! if true >= otherwise >
  LOGICAL :: le=.TRUE. ! if true <= otherwise <
END TYPE interval_info

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
  PRIVATE
  CHARACTER(len=80) :: trans_type ! type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'inter', \c 'vertint' ...
  CHARACTER(len=80) :: sub_type ! subtype of transformation, can be \c 'linear'
  LOGICAL :: extrap ! enable elaboration outside data bounding box
  TYPE(rect_ind) :: rect_ind ! rectangle information by index
  TYPE(rect_coo) :: rect_coo ! rectangle information by coordinates
  TYPE(area_info) :: area_info ! 
  TYPE(arrayof_georef_coord_array) :: poly ! polygon information
  TYPE(stat_info) :: stat_info ! 
  TYPE(interval_info) :: interval_info ! 
  TYPE(box_info) :: box_info ! boxregrid specification
  TYPE(vertint) :: vertint ! vertical interpolation specification
  INTEGER :: time_definition ! time definition for interpolating to sparse points
  INTEGER :: category = 0 ! category for log4fortran
END TYPE transform_def


!> This object fully defines a transformation between a couple of
!! particular \a griddim_def or \a vol7d objects (any combination is
!! possible). It carries information about the objects' mutual
!! coordinates in order to speed up the transformation when it has to
!! be repeated on objects having the same coordinates and grid
!! projections.
TYPE grid_transform
  PRIVATE
  TYPE(transform_def),PUBLIC :: trans ! type of transformation required
  INTEGER :: innx = imiss
  INTEGER :: inny = imiss
  INTEGER :: innz = imiss
  INTEGER :: outnx = imiss
  INTEGER :: outny = imiss
  INTEGER :: outnz = imiss
  INTEGER :: levshift = imiss
  INTEGER :: levused = imiss
  INTEGER :: iniox,inioy,infox,infoy,outinx,outiny,outfnx,outfny
  INTEGER,POINTER :: inter_index_x(:,:) => NULL()
  INTEGER,POINTER :: inter_index_y(:,:) => NULL()
  INTEGER,POINTER :: inter_index_z(:) => NULL()
  INTEGER,POINTER :: point_index(:,:) => NULL()
  DOUBLE PRECISION,POINTER :: inter_x(:,:) => NULL()
  DOUBLE PRECISION,POINTER :: inter_y(:,:) => NULL()
  DOUBLE PRECISION,POINTER :: inter_xp(:,:) => NULL()
  DOUBLE PRECISION,POINTER :: inter_yp(:,:) => NULL()
  DOUBLE PRECISION,POINTER :: inter_zp(:) => NULL()
  DOUBLE PRECISION,POINTER :: vcoord_in(:) => NULL()
  DOUBLE PRECISION,POINTER :: vcoord_out(:) => NULL()
  LOGICAL,POINTER :: point_mask(:,:) => NULL()
  LOGICAL,POINTER :: stencil(:,:) => NULL()
  REAL,ALLOCATABLE :: coord_3d_in(:,:,:)
  REAL :: val1 = rmiss
  REAL :: val2 = rmiss
  LOGICAL :: recur = .FALSE.
  LOGICAL :: dolog = .FALSE. ! must compute log() of vert coord before vertint

!  type(volgrid6d) :: input_vertcoordvol ! volume which provides the input vertical coordinate if separated from the data volume itself (for vertint) cannot be here because of cross-use, should be an argument of compute
!  type(vol7d_level), pointer :: output_vertlevlist(:) ! list of vertical levels of output data (for vertint) can be here or an argument of compute, how to do?
  TYPE(vol7d_level),POINTER :: output_level_auto(:) => NULL() ! array of auto-generated levels, stored for successive query
  INTEGER :: category = 0 ! category for log4fortran
  LOGICAL :: valid = .FALSE. ! the transformation has been successfully initialised
  PROCEDURE(basic_find_index),NOPASS,POINTER :: find_index => basic_find_index ! allow a local implementation of find_index
END TYPE grid_transform


!> Constructors of the corresponding objects.
INTERFACE init
  MODULE PROCEDURE transform_init, grid_transform_levtype_levtype_init, &
   grid_transform_init, &
   grid_transform_grid_vol7d_init, grid_transform_vol7d_grid_init, &
   grid_transform_vol7d_vol7d_init
END INTERFACE

!> Destructors of the corresponding objects.
INTERFACE delete
  MODULE PROCEDURE transform_delete, grid_transform_delete
END INTERFACE

!> Method for returning the contents of the object.
INTERFACE get_val
  MODULE PROCEDURE transform_get_val, grid_transform_get_val
END INTERFACE

!> Compute the output data array from input data array according to
!! the defined transformation.
INTERFACE compute
  MODULE PROCEDURE grid_transform_compute, grid_transform_v7d_grid_compute
END INTERFACE

!> Returns \a .TRUE. if, after \a init , the corresponding \a grid_transform
!! object has been correctly initialised.
INTERFACE c_e
  MODULE PROCEDURE grid_transform_c_e
END INTERFACE

PRIVATE
PUBLIC init, delete, get_val, compute, c_e
PUBLIC transform_def, grid_transform
PUBLIC interval_info, interval_info_new, interval_info_valid, basic_find_index

CONTAINS


FUNCTION interval_info_new(interv_gt, interv_ge, interv_lt, interv_le) RESULT(this)
REAL,INTENT(in),OPTIONAL :: interv_gt !< greater than condition for defining interval
REAL,INTENT(in),OPTIONAL :: interv_ge !< greater equal condition for defining interval
REAL,INTENT(in),OPTIONAL :: interv_lt !< less than condition for defining interval
REAL,INTENT(in),OPTIONAL :: interv_le !< less equal condition for defining interval

TYPE(interval_info) :: this

IF (PRESENT(interv_gt)) THEN
  IF (c_e(interv_gt)) THEN
    this%gt = interv_gt
    this%ge = .FALSE.
  ENDIF
ENDIF
IF (PRESENT(interv_ge)) THEN
  IF (c_e(interv_ge)) THEN
    this%gt = interv_ge
    this%ge = .TRUE.
  ENDIF
ENDIF
IF (PRESENT(interv_lt)) THEN
  IF (c_e(interv_lt)) THEN
    this%lt = interv_lt
    this%le = .FALSE.
  ENDIF
ENDIF
IF (PRESENT(interv_le)) THEN
  IF (c_e(interv_le)) THEN
    this%lt = interv_le
    this%le = .TRUE.
  ENDIF
ENDIF

END FUNCTION interval_info_new

! Private method for testing whether \a val is included in \a this
! interval taking into account all cases, zero, one or two extremes,
! strict or non strict inclusion, empty interval, etc, while no check
! is made for val being missing. Returns 1.0 if val is in interval and
! 0.0 if not.
ELEMENTAL FUNCTION interval_info_valid(this, val)
TYPE(interval_info),INTENT(in) :: this
REAL,INTENT(in) :: val

REAL :: interval_info_valid

interval_info_valid = 1.0

IF (c_e(this%gt)) THEN
  IF (val < this%gt) interval_info_valid = 0.0
  IF (.NOT.this%ge) THEN
    IF (val == this%gt) interval_info_valid = 0.0
  ENDIF
ENDIF
IF (c_e(this%lt)) THEN
  IF (val > this%lt) interval_info_valid = 0.0
  IF (.NOT.this%le) THEN
    IF (val == this%lt) interval_info_valid = 0.0
  ENDIF
ENDIF

END FUNCTION interval_info_valid

!> Constructor for a \a transform_def object, defining an abstract
!! transformation between gridded and/or sparse point data.  The
!! parameters \a trans_type and \a sub_type define the type of
!! transformation, while all the other following parameters are
!! optional, they have to be passed in keyword mode and those required
!! by the transformation type and subtype chosen have to be present.
SUBROUTINE transform_init(this, trans_type, sub_type, &
 ix, iy, fx, fy, ilon, ilat, flon, flat, &
 npx, npy, boxdx, boxdy, radius, poly, percentile, &
 interv_gt, interv_ge, interv_lt, interv_le, &
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
DOUBLEPRECISION,INTENT(in),OPTIONAL :: radius !< radius of stencil in grid points (also fractionary values) for stencil interpolation
TYPE(arrayof_georef_coord_array),OPTIONAL :: poly !< array of polygons indicating areas over which to interpolate (for transformations 'polyinter' or 'metamorphosis:poly')
DOUBLEPRECISION,INTENT(in),OPTIONAL :: percentile !< percentile [0,100.] of the distribution of points in the box to use as interpolated value for 'percentile' subtype
REAL,INTENT(in),OPTIONAL :: interv_gt !< greater than condition for defining interval
REAL,INTENT(in),OPTIONAL :: interv_ge !< greater equal condition for defining interval
REAL,INTENT(in),OPTIONAL :: interv_lt !< less than condition for defining interval
REAL,INTENT(in),OPTIONAL :: interv_le !< less equal condition for defining interval
LOGICAL,INTENT(IN),OPTIONAL :: extrap !< activate extrapolation outside input domain (use with care!)
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< time definition for output vol7d object 0=time is reference time ; 1=time is validity time
TYPE(vol7d_level),INTENT(IN),OPTIONAL :: input_levtype !< type of vertical level of input data to be vertically interpolated (only type of first and second surface are used, level values are ignored)
TYPE(vol7d_var),INTENT(IN),OPTIONAL :: input_coordvar !< variable that defines the vertical coordinate in the input volume for vertical interpolation, if missing, the value of the vertical level defined with \a input_levtype is used
TYPE(vol7d_level),INTENT(IN),OPTIONAL :: output_levtype !< type of vertical level to which data should be vertically interpolated (only type of first and second surface are used, level values are ignored)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< suffix to append to log4fortran namespace category

character(len=512) :: a_name

IF (PRESENT(categoryappend)) THEN
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory)//"."// &
   TRIM(categoryappend))
ELSE
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory))
ENDIF
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
CALL optio(radius,this%area_info%radius)
IF (PRESENT(poly)) this%poly = poly
CALL optio(percentile,this%stat_info%percentile)

this%interval_info = interval_info_new(interv_gt, interv_ge, interv_lt, interv_le)

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

  IF (this%sub_type == 'coord' .OR. this%sub_type == 'projcoord')THEN

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

    IF (c_e(this%rect_ind%ix) .AND. c_e(this%rect_ind%iy) .AND. &
     c_e(this%rect_ind%fx) .AND. c_e(this%rect_ind%fy)) THEN

! check
      IF (this%rect_ind%ix > this%rect_ind%fx .OR. &
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

    ELSE

      CALL l4f_category_log(this%category,L4F_ERROR,&
       'zoom: index parameters ix, iy, fx, fy not provided')
      CALL raise_fatal_error()

    ENDIF

  ELSE
    CALL sub_type_error()
    RETURN
  END IF

ELSE IF (this%trans_type == 'inter') THEN

  IF (this%sub_type == 'near' .OR. this%sub_type == 'bilin' .OR. &
   this%sub_type == 'linear' .OR. this%sub_type == 'shapiro_near') THEN
! nothing to do here
  ELSE
    CALL sub_type_error()
    RETURN
  ENDIF

ELSE IF (this%trans_type == 'boxregrid' .OR. this%trans_type == 'boxinter' .OR. &
 this%trans_type == 'polyinter' .OR. this%trans_type == 'maskinter' .OR. &
 this%trans_type == 'stencilinter') THEN

  IF (this%trans_type == 'boxregrid') THEN
    IF (c_e(this%box_info%npx) .AND. c_e(this%box_info%npy)) THEN
      IF (this%box_info%npx <= 0 .OR. this%box_info%npy <= 0 ) THEN
        CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid: invalid parameters  '//&
         TRIM(to_char(this%box_info%npx))//' '//TRIM(to_char(this%box_info%npy)))
        CALL raise_fatal_error()
      ENDIF
    ELSE
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'boxregrid: parameters npx, npy missing')
      CALL raise_fatal_error()
    ENDIF
  ENDIF

  IF (this%trans_type == 'polyinter') THEN
    IF (this%poly%arraysize <= 0) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       "polyinter: poly parameter missing or empty")
      CALL raise_fatal_error()
    ENDIF
  ENDIF

  IF (this%trans_type == 'stencilinter') THEN
    IF (.NOT.c_e(this%area_info%radius)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       "stencilinter: radius parameter missing")
      CALL raise_fatal_error()
    ENDIF
  ENDIF

  IF (this%sub_type == 'average' .OR. this%sub_type == 'stddev' &
   .OR. this%sub_type == 'stddevnm1') THEN
    this%stat_info%percentile = rmiss
  ELSE IF (this%sub_type == 'max') THEN
    this%stat_info%percentile = 101.
  ELSE IF (this%sub_type == 'min') THEN
    this%stat_info%percentile = -1.
  ELSE IF (this%sub_type == 'percentile') THEN
    IF (.NOT.c_e(this%stat_info%percentile)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR,TRIM(this%trans_type)// &
       ':percentile: percentile value not provided')
      CALL raise_fatal_error()
    ELSE IF (this%stat_info%percentile >= 100.) THEN
      this%sub_type = 'max'
    ELSE IF (this%stat_info%percentile <= 0.) THEN
      this%sub_type = 'min'
    ENDIF
  ELSE IF (this%sub_type == 'frequency') THEN
    IF (.NOT.c_e(this%interval_info%gt) .AND. .NOT.c_e(this%interval_info%gt)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR,TRIM(this%trans_type)// &
       ':frequency: lower and/or upper limit not provided')
      CALL raise_fatal_error()
    ENDIF
  ELSE
    CALL sub_type_error()
    RETURN
  ENDIF

ELSE IF (this%trans_type == 'maskgen')THEN

  IF (this%sub_type == 'poly') THEN

    IF (this%poly%arraysize <= 0) THEN
      CALL l4f_category_log(this%category,L4F_ERROR,"maskgen:poly poly parameter missing or empty")
      CALL raise_fatal_error()
    ENDIF

  ELSE IF (this%sub_type == 'grid') THEN
! nothing to do for now

  ELSE
    CALL sub_type_error()
    RETURN
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

  IF (this%sub_type == 'linear' .OR. this%sub_type == 'linearsparse') THEN
! nothing to do here
  ELSE
    CALL sub_type_error()
    RETURN
  ENDIF

ELSE IF (this%trans_type == 'metamorphosis') THEN

  IF (this%sub_type == 'all') THEN
! nothing to do here
  ELSE IF (this%sub_type == 'coordbb')THEN

    IF (c_e(this%rect_coo%ilon) .AND. c_e(this%rect_coo%ilat) .AND. &
     c_e(this%rect_coo%flon) .AND. c_e(this%rect_coo%flat)) THEN ! coordinates given
    ELSE

      CALL l4f_category_log(this%category,L4F_ERROR,"metamorphosis: coordbb parameters missing")
      CALL raise_fatal_error()
        
    ENDIF

  ELSE IF (this%sub_type == 'poly')THEN

    IF (this%poly%arraysize <= 0) THEN
      CALL l4f_category_log(this%category,L4F_ERROR,"metamorphosis:poly: poly parameter missing or empty")
      CALL raise_fatal_error()
    ENDIF

  ELSE IF (this%sub_type == 'mask' .OR. this%sub_type == 'maskvalid' .OR. &
   this%sub_type == 'maskinvalid' .OR. this%sub_type == 'setinvalidto' .OR. &
   this%sub_type == 'settoinvalid') THEN
! nothing to do here
  ELSE
    CALL sub_type_error()
    RETURN
  ENDIF

ELSE
  CALL trans_type_error()
  RETURN
ENDIF

CONTAINS

SUBROUTINE sub_type_error()

CALL l4f_category_log(this%category, L4F_ERROR, TRIM(this%trans_type) &
 //': sub_type '//TRIM(this%sub_type)//' is not defined')
CALL raise_fatal_error()

END SUBROUTINE sub_type_error

SUBROUTINE trans_type_error()

CALL l4f_category_log(this%category, L4F_ERROR, 'trans_type '//this%trans_type &
 //' is not defined')
CALL raise_fatal_error()

END SUBROUTINE trans_type_error


END SUBROUTINE transform_init


!> Destructor of \a tranform_def object.
!! It releases any memory and data associated to the \a transform_def
!! object \a this, the logger category will be deleted too.
SUBROUTINE transform_delete(this)
TYPE(transform_def),INTENT(inout) :: this !< transformation object

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

this%box_info%npx=imiss
this%box_info%npy=imiss

this%extrap=.FALSE.

!chiudo il logger
CALL l4f_category_delete(this%category)

END SUBROUTINE transform_delete


!> Method for returning the contents of the object.
SUBROUTINE transform_get_val(this, time_definition, trans_type, sub_type, &
 input_levtype, output_levtype)
type(transform_def),intent(in) :: this !< object to examine
INTEGER,INTENT(out),OPTIONAL :: time_definition !< 0=time is reference time, 1=time is validity time
CHARACTER(len=*),INTENT(out),OPTIONAL :: trans_type !< type of transformation
CHARACTER(len=*),INTENT(out),OPTIONAL :: sub_type !< subtype of transformation
TYPE(vol7d_level),INTENT(out),OPTIONAL :: input_levtype
!< type of vertical level of input data (only type of first and second surface are used, level values are ignored)
TYPE(vol7d_level),INTENT(out),OPTIONAL :: output_levtype
!< type of vertical level of output data (only type of first and second surface are used, level values are ignored)

IF (PRESENT(time_definition)) time_definition=this%time_definition
IF (PRESENT(trans_type)) trans_type = this%trans_type
IF (PRESENT(sub_type)) sub_type = this%sub_type
IF (PRESENT(input_levtype)) input_levtype = this%vertint%input_levtype
IF (PRESENT(output_levtype)) output_levtype = this%vertint%output_levtype


END SUBROUTINE transform_get_val


!> Constructor for a \a grid_transform object, defining a particular
!! vertical transformation.
!! It defines an object describing a transformation involving
!! operations on the vertical direction only, thus it applies in the
!! same way to grid-to-grid and to sparse points-to-sparse points
!! transformations; the abstract type of the transformation is
!! described in the transformation object \a trans (type
!! transform_def) which must have been properly initialised. The
!! additional information required here is the list of input and
!! output levels and an optional 3-d field indicating the vertical
!! coordinates of the input dataset.
!!
!! The input level list \a lev_in is a 1-d array of \a vol7d_level
!! type, describing the vertical coordinate system of the whole input
!! dataset, only the vertical levels or layers matching the level type
!! indicated when initialising the transformation object \a trans will
!! be used, the others will be discarded in the vertical
!! transformation. However the relevant vertical levels must be
!! contiguous and sorted accordingly to the default \a vol7d_level
!! sort order. The argument \a lev_out describes the vertical
!! coordinate system of the output dataset. A particular case to be
!! considered is when \a SIZE(lev_out)==0, this means that the output
!! coordinates have to be computed automatically in the current
!! subroutine, this is supported only for hybrid levels/layers.
!!
!! When the input and output level types are different, the \a
!! coord_3d_in array must be provided, indicating the vertical
!! coordinate of every input grid point expressed in terms of the
!! output vertical coordinate system (e.g. height if interpolating to
!! constant height levels or pressure if interpolating to isobaric
!! levels). This array must contain, in the 3rd vertical dimension,
!! only the those levels/layers of \a lev_in that are actually used
!! for interpolation. The storage space of \a coord_3d_in is "stolen"
!! by this method, so the array will appear as unallocated and
!! unusable to the calling procedure after return from this
!! subroutine.
!!
!! Layers in the grib2 sense (i.e. layers between two surfaces) can be
!! handled by this class only when the upper and lower surfaces are of
!! the same type; in these cases the coordinate assigned to every
!! layer fro interpolation is the average (or log-average in case of
!! isobaric surfaces) between the coordinates of the corresponding
!! upper and lower surfaces.
SUBROUTINE grid_transform_levtype_levtype_init(this, trans, lev_in, lev_out, &
 coord_3d_in, categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(vol7d_level),INTENT(in) :: lev_in(:) !< vol7d_level from input object
TYPE(vol7d_level),INTENT(in) :: lev_out(:) !< vol7d_level object defining target vertical grid
REAL,INTENT(inout),OPTIONAL,ALLOCATABLE :: coord_3d_in(:,:,:) !< vertical coordinates of each input point in target reference system
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

DOUBLE PRECISION :: coord_in(SIZE(lev_in))
DOUBLE PRECISION,ALLOCATABLE :: coord_out(:)
LOGICAL :: mask_in(SIZE(lev_in))
LOGICAL,ALLOCATABLE :: mask_out(:)
LOGICAL :: dolog
INTEGER :: i, j, icache, inused, istart, iend, ostart, oend


CALL grid_transform_init_common(this, trans, categoryappend)
#ifdef DEBUG
CALL l4f_category_log(this%category, L4F_DEBUG, "grid_transform vertint")
#endif

IF (this%trans%trans_type == 'vertint') THEN

  IF (c_e(trans%vertint%input_levtype%level2) .AND. &
   trans%vertint%input_levtype%level1 /= trans%vertint%input_levtype%level2) THEN
    CALL l4f_category_log(this%category, L4F_ERROR, &
     'vertint: input upper and lower surface must be of the same type, '// &
     t2c(trans%vertint%input_levtype%level1)//'/='// &
     t2c(trans%vertint%input_levtype%level2))
    CALL raise_error()
    RETURN
  ENDIF
  IF (c_e(trans%vertint%output_levtype%level2) .AND. &
   trans%vertint%output_levtype%level1 /= trans%vertint%output_levtype%level2) THEN
    CALL l4f_category_log(this%category, L4F_ERROR, &
     'vertint: output upper and lower surface must be of the same type'// &
     t2c(trans%vertint%output_levtype%level1)//'/='// &
     t2c(trans%vertint%output_levtype%level2))
    CALL raise_error()
    RETURN
  ENDIF

  mask_in(:) = (lev_in(:)%level1 == trans%vertint%input_levtype%level1) .AND. &
   (lev_in(:)%level2 == trans%vertint%input_levtype%level2)
  CALL make_vert_coord(lev_in, mask_in, coord_in, dolog)
  this%innz = SIZE(lev_in)
  istart = firsttrue(mask_in)
  iend = lasttrue(mask_in)
  inused = iend - istart + 1
  IF (inused /= COUNT(mask_in)) THEN
    CALL l4f_category_log(this%category, L4F_ERROR, &
     'grid_transform_levtype_levtype_init: input levels badly sorted '//&
     t2c(inused)//'/'//t2c(COUNT(mask_in)))
    CALL raise_error()
    RETURN
  ENDIF
  this%levshift = istart-1
  this%levused = inused

  IF (trans%vertint%input_levtype%level1 /= trans%vertint%output_levtype%level1) THEN
#ifdef DEBUG
    CALL l4f_category_log(this%category, L4F_DEBUG, &
     'vertint: different input and output level types '// &
     t2c(trans%vertint%input_levtype%level1)//' '// &
     t2c(trans%vertint%output_levtype%level1))
#endif

    ALLOCATE(mask_out(SIZE(lev_out)), this%vcoord_out(SIZE(lev_out)))
    mask_out(:) = (lev_out(:)%level1 == trans%vertint%output_levtype%level1) .AND. &
     (lev_out(:)%level2 == trans%vertint%output_levtype%level2)
    CALL make_vert_coord(lev_out, mask_out, this%vcoord_out, dolog)
    this%outnz = SIZE(mask_out)
    DEALLOCATE(mask_out)

    IF (.NOT.PRESENT(coord_3d_in)) THEN
      CALL l4f_category_log(this%category, L4F_WARN, &
       'vertint: different input and output level types &
       &and no coord_3d_in, expecting vert. coord. in volume')
      this%dolog = dolog ! a little bit dirty, I must compute log later
    ELSE
      IF (SIZE(coord_3d_in,3) /= inused) THEN
        CALL l4f_category_log(this%category, L4F_ERROR, &
         'vertint: vertical size of coord_3d_in (vertical coordinate) &
         &different from number of input levels suitable for interpolation')
        CALL l4f_category_log(this%category, L4F_ERROR, &
         'coord_3d_in: '//t2c(SIZE(coord_3d_in,3))// &
         ', input levels for interpolation: '//t2c(inused))
        CALL raise_error()
        RETURN
      ENDIF

      CALL MOVE_ALLOC(coord_3d_in, this%coord_3d_in) ! steal allocation
      IF (dolog) THEN
        WHERE(c_e(this%coord_3d_in) .AND. this%coord_3d_in > 0.0)
          this%coord_3d_in = LOG(this%coord_3d_in)
        ELSE WHERE
          this%coord_3d_in = rmiss
        END WHERE
      ENDIF
    ENDIF

    this%valid = .TRUE. ! warning, no check of subtype

  ELSE
! here we assume that valid levels are contiguous and ordered

#ifdef DEBUG
    CALL l4f_category_log(this%category, L4F_DEBUG, &
     'vertint: equal input and output level types '// &
     t2c(trans%vertint%input_levtype%level1))
#endif

    IF (SIZE(lev_out) > 0) THEN ! output level list provided
      ALLOCATE(mask_out(SIZE(lev_out)), coord_out(SIZE(lev_out)))
      mask_out(:) = (lev_out(:)%level1 == trans%vertint%output_levtype%level1) .AND. &
       (lev_out(:)%level2 == trans%vertint%output_levtype%level2)
      CALL make_vert_coord(lev_out, mask_out, coord_out, dolog)

    ELSE ! output level list not provided, try to autogenerate
      IF (c_e(trans%vertint%input_levtype%level2) .AND. &
       .NOT.c_e(trans%vertint%output_levtype%level2)) THEN ! full -> half
        IF (trans%vertint%output_levtype%level1 == 105 .OR. &
         trans%vertint%output_levtype%level1 == 150) THEN
          ALLOCATE(this%output_level_auto(inused-1))
          CALL l4f_category_log(this%category,L4F_INFO, &
           'grid_transform_levtype_levtype_init: autogenerating '//t2c(inused-1) &
           //'/'//t2c(iend-istart)//' output levels (f->h)')
          DO i = istart, iend - 1
            CALL init(this%output_level_auto(i-istart+1), &
             trans%vertint%input_levtype%level1, lev_in(i)%l2)
          ENDDO
        ELSE
          CALL l4f_category_log(this%category, L4F_ERROR, &
           'grid_transform_levtype_levtype_init: automatic generation of output levels &
           &available only for hybrid levels')
          CALL raise_error()
          RETURN
        ENDIF
      ELSE IF (.NOT.c_e(trans%vertint%input_levtype%level2) .AND. &
       c_e(trans%vertint%output_levtype%level2)) THEN ! half -> full
        ALLOCATE(this%output_level_auto(inused-1))
        IF (trans%vertint%output_levtype%level1 == 105 .OR. &
         trans%vertint%output_levtype%level1 == 150) THEN
          CALL l4f_category_log(this%category,L4F_INFO, &
           'grid_transform_levtype_levtype_init: autogenerating '//t2c(inused-1) &
           //'/'//t2c(iend-istart)//' output levels (h->f)')
          DO i = istart, iend - 1
            CALL init(this%output_level_auto(i-istart+1), trans%vertint%input_levtype%level1, &
             lev_in(i)%l1, trans%vertint%input_levtype%level1, &
             lev_in(i)%l1+1)
          ENDDO
        ELSE
          CALL l4f_category_log(this%category, L4F_ERROR, &
           'grid_transform_levtype_levtype_init: automatic generation of output levels &
           &available only for hybrid levels')
          CALL raise_error()
          RETURN
        ENDIF
      ELSE
        CALL l4f_category_log(this%category, L4F_ERROR, &
         'grid_transform_levtype_levtype_init: strange situation'// &
         to_char(c_e(trans%vertint%input_levtype%level2))//' '// &
         to_char(c_e(trans%vertint%output_levtype%level2)))
        CALL raise_error()
        RETURN
      ENDIF
      ALLOCATE(coord_out(inused-1), mask_out(inused-1))
      mask_out(:) = .TRUE.
      CALL make_vert_coord(this%output_level_auto, mask_out, coord_out, dolog)
    ENDIF

    this%outnz = SIZE(mask_out)
    ostart = firsttrue(mask_out)
    oend = lasttrue(mask_out)

! set valid = .FALSE. here?
    IF (istart == 0) THEN
      CALL l4f_category_log(this%category, L4F_WARN, &
       'grid_transform_levtype_levtype_init: &
       &input contains no vertical levels of type ('// &
       TRIM(to_char(trans%vertint%input_levtype%level1))//','// &
       TRIM(to_char(trans%vertint%input_levtype%level2))// &
       ') suitable for interpolation')
      RETURN
!      iend = -1 ! for loops
    ELSE IF (istart == iend) THEN
      CALL l4f_category_log(this%category, L4F_WARN, &
       'grid_transform_levtype_levtype_init: &
       &input contains only 1 vertical level of type ('// &
       TRIM(to_char(trans%vertint%input_levtype%level1))//','// &
       TRIM(to_char(trans%vertint%input_levtype%level2))// &
       ') suitable for interpolation')
    ENDIF
    IF (ostart == 0) THEN
      CALL l4f_category_log(this%category, L4F_WARN, &
       'grid_transform_levtype_levtype_init: &
       &output contains no vertical levels of type ('// &
       TRIM(to_char(trans%vertint%output_levtype%level1))//','// &
       TRIM(to_char(trans%vertint%output_levtype%level2))// &
       ') suitable for interpolation')
      RETURN
!      oend = -1 ! for loops
    ENDIF

! end of code common for all vertint subtypes
    IF (this%trans%sub_type == 'linear') THEN

      ALLOCATE(this%inter_index_z(this%outnz), this%inter_zp(this%outnz))
      this%inter_index_z(:) = imiss
      this%inter_zp(:) = dmiss
      IF (this%trans%extrap .AND. istart > 0) THEN
        WHERE(mask_out)
! extrapolate down by default
          this%inter_index_z(:) = istart
          this%inter_zp(:) = 1.0D0
        ENDWHERE
      ENDIF

      icache = istart + 1
      outlev: DO j = ostart, oend
        inlev: DO i = icache, iend
          IF (coord_in(i) >= coord_out(j)) THEN
            IF (coord_out(j) >= coord_in(i-1)) THEN
              this%inter_index_z(j) = i - 1
              this%inter_zp(j) = (coord_out(j)-coord_in(i-1)) / &
               (coord_in(i)-coord_in(i-1)) ! weight for (i)
              icache = i ! speedup next j iteration
            ENDIF
            CYCLE outlev ! found or extrapolated down
          ENDIF
        ENDDO inlev
! if I'm here I must extrapolate up
        IF (this%trans%extrap .AND. iend > 1) THEN
          this%inter_index_z(j) = iend - 1
          this%inter_zp(j) = 0.0D0
        ENDIF
      ENDDO outlev

      DEALLOCATE(coord_out, mask_out)
      this%valid = .TRUE.

    ELSE IF (this%trans%sub_type == 'linearsparse') THEN
! just store vertical coordinates, dirty work is done later
      ALLOCATE(this%vcoord_in(this%levused),  this%vcoord_out(SIZE(coord_out)))
      this%vcoord_in(:) = coord_in(this%levshift+1:this%levshift+this%levused)
      this%vcoord_out(:) = coord_out(:)
      DEALLOCATE(coord_out, mask_out)
      this%valid = .TRUE.

    ENDIF

  ENDIF ! levels are different

!ELSE IF (this%trans%trans_type == 'verttrans') THEN

ENDIF

END SUBROUTINE grid_transform_levtype_levtype_init


! internal subroutine for computing vertical coordinate values, for
! pressure-based coordinates the logarithm is computed
SUBROUTINE make_vert_coord(lev, mask, coord, dolog)
TYPE(vol7d_level),INTENT(in) :: lev(:)
LOGICAL,INTENT(inout) :: mask(:)
DOUBLE PRECISION,INTENT(out) :: coord(:)
LOGICAL,INTENT(out) :: dolog

INTEGER :: k
DOUBLE PRECISION :: fact

dolog = .FALSE.
k = firsttrue(mask)
IF (k <= 0) RETURN
coord(:) = dmiss

IF (ANY(lev(k)%level1 == height_level)) THEN ! improve with a conversion table somewhere
  fact = 1.0D-3
ELSE IF (ANY(lev(k)%level1 == thermo_level)) THEN
  fact = 1.0D-1
ELSE IF (ANY(lev(k)%level1 == sigma_level)) THEN
  fact = 1.0D-4
ELSE
  fact = 1.0D0
ENDIF

IF (c_e(lev(k)%level2) .AND. lev(k)%level1 == lev(k)%level2) THEN ! layer between 2 levels
  IF (lev(k)%level1 == 100 .OR. lev(k)%level1 == 108) THEN ! pressure, compute log
    WHERE(mask(:) .AND. lev(:)%l1 > 0 .AND. lev(:)%l2 > 0)
      coord(:) = (LOG(DBLE(lev(:)%l1)*fact) + LOG(DBLE(lev(:)%l2)*fact))*0.5D0
    END WHERE
    dolog = .TRUE.
  ELSE
    WHERE(mask(:))
      coord(:) = (lev(:)%l1 + lev(:)%l2)*fact*0.5D0
    END WHERE
  ENDIF
ELSE ! half level
  IF (lev(k)%level1 == 100 .OR. lev(k)%level1 == 108) THEN ! pressure, compute log
    WHERE(mask(:) .AND. lev(:)%l1 > 0)
      coord(:) = LOG(DBLE(lev(:)%l1)*fact)
    END WHERE
    dolog = .TRUE.
  ELSE
    WHERE(mask(:))
      coord(:) = lev(:)%l1*fact
    END WHERE
  ENDIF
ENDIF

! refine mask
mask(:) = mask(:) .AND. c_e(coord(:))

END SUBROUTINE make_vert_coord


!> Constructor for a \a grid_transform object, defining a particular
!! grid-to-grid transformation.  It defines an object describing a
!! transformation from one rectangular grid to another; the abstract
!! type of transformation is described in the transformation object \a
!! trans (type transform_def) which must have been properly
!! initialised. The additional information required here is the
!! description of the input grid \a in (type griddim_def), the
!! description of the output grid \a out (type griddim_def as
!! well). The description of the output grid must be initialized for
!! interpolating type transformations ('inter' and 'boxinter'), while
!! it is generated by this constructor and returned in output for
!! 'zoom', 'boxregrid', 'maskgen' and 'polyinter' transformations.
!!
!! The generated \a grid_transform object is specific to the input and
!! output grids involved. The function \a c_e can be used in order to
!! check whether the object has been successfully initialised, if the
!! result is \a .FALSE., it should not be used further on.
SUBROUTINE grid_transform_init(this, trans, in, out, maskgrid, maskbounds, &
 categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(inout) :: in !< griddim object to transform
TYPE(griddim_def),INTENT(inout) :: out !< griddim object defining target grid (input or output depending on type of transformation)
REAL,INTENT(in),OPTIONAL :: maskgrid(:,:) !< 2D field to be used for defining valid points, it must have the same shape as the field to be interpolated (for transformation type 'metamorphosis:maskvalid')
REAL,INTENT(in),OPTIONAL :: maskbounds(:) !< array of boundary values for defining a subset of valid points where the values of \a maskgrid are within the first and last value of \a maskbounds (for transformation type 'metamorphosis:maskvalid/settoinvalid' and others)
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: nx, ny, i, j, ix, iy, n, nm, nr, cf_i, cf_o, nprev, &
 xnmin, xnmax, ynmin, ynmax
DOUBLE PRECISION :: xmin, xmax, ymin, ymax, steplon, steplat, &
 xmin_new, ymin_new, ellips_smaj_axis, ellips_flatt, r2
TYPE(geo_proj) :: proj_in, proj_out
TYPE(georef_coord) :: point
LOGICAL,ALLOCATABLE :: point_mask(:,:)
TYPE(griddim_def) :: lin, lout


CALL grid_transform_init_common(this, trans, categoryappend)
#ifdef DEBUG
CALL l4f_category_log(this%category, L4F_DEBUG, "grid_transform vg6d-vg6d")
#endif

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

  ELSE IF (this%trans%sub_type == 'projcoord') THEN

    CALL griddim_zoom_projcoord(in, &
     this%trans%rect_coo%ilon, this%trans%rect_coo%ilat,&
     this%trans%rect_coo%flon, this%trans%rect_coo%flat,&
     this%trans%rect_ind%ix, this%trans%rect_ind%iy, &
     this%trans%rect_ind%fx, this%trans%rect_ind%fy)

  ELSE IF (this%trans%sub_type == 'coordbb') THEN

! compute coordinates of input grid in geo system
    CALL copy(in, lin)
    CALL unproj(lin)
    CALL get_val(lin, nx=nx, ny=ny)

    ALLOCATE(point_mask(nx,ny))
    point_mask(:,:) = .FALSE.

! mark points falling into requested bounding-box
    DO j = 1, ny
      DO i = 1, nx
!        IF (geo_coord_inside_rectang())
        IF (lin%dim%lon(i,j) > this%trans%rect_coo%ilon .AND. &
         lin%dim%lon(i,j) < this%trans%rect_coo%flon .AND. &
         lin%dim%lat(i,j) > this%trans%rect_coo%ilat .AND. &
         lin%dim%lat(i,j) < this%trans%rect_coo%flat) THEN ! improve!
          point_mask(i,j) = .TRUE.
        ENDIF
      ENDDO
    ENDDO

! determine cut indices keeping all points which fall inside b-b
    DO i = 1, nx
      IF (ANY(point_mask(i,:))) EXIT
    ENDDO
    this%trans%rect_ind%ix = i
    DO i = nx, this%trans%rect_ind%ix, -1
      IF (ANY(point_mask(i,:))) EXIT
    ENDDO
    this%trans%rect_ind%fx = i

    DO j = 1, ny
      IF (ANY(point_mask(:,j))) EXIT
    ENDDO
    this%trans%rect_ind%iy = j
    DO j = ny, this%trans%rect_ind%iy, -1
      IF (ANY(point_mask(:,j))) EXIT
    ENDDO
    this%trans%rect_ind%fy = j

    DEALLOCATE(point_mask)

    IF (this%trans%rect_ind%ix > this%trans%rect_ind%fx .OR. &
     this%trans%rect_ind%iy > this%trans%rect_ind%fy) THEN

      CALL l4f_category_log(this%category,L4F_ERROR, &
       "zoom coordbb: no points inside bounding box "//&
       TRIM(to_char(this%trans%rect_coo%ilon))//","// &
       TRIM(to_char(this%trans%rect_coo%flon))//","// &
       TRIM(to_char(this%trans%rect_coo%ilat))//","// &
       TRIM(to_char(this%trans%rect_coo%flat)))
      CALL raise_error()
      RETURN

    ENDIF
    CALL delete(lin)
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

  CALL copy(in, out)
! deallocate coordinates if allocated because they will change
  CALL dealloc(out%dim)

  out%dim%nx = this%trans%rect_ind%fx - this%trans%rect_ind%ix + 1 ! newx
  out%dim%ny = this%trans%rect_ind%fy - this%trans%rect_ind%iy + 1 ! newy
  this%outnx = out%dim%nx
  this%outny = out%dim%ny
  this%innx = nx
  this%inny = ny

  CALL set_val(out, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

  this%valid = .TRUE. ! warning, no check of subtype
  
ELSE IF (this%trans%trans_type == 'boxregrid') THEN

  CALL get_val(in, nx=nx, ny=ny, xmin=xmin, xmax=xmax, &
   ymin=ymin, ymax=ymax, dx=steplon, dy=steplat)

  this%innx = nx
  this%inny = ny

! new grid
  xmin_new = xmin + (this%trans%box_info%npx - 1)*0.5D0*steplon
  ymin_new = ymin + (this%trans%box_info%npy - 1)*0.5D0*steplat

  CALL copy(in, out)
! deallocate coordinates if allocated because they will change
  CALL dealloc(out%dim)

  out%dim%nx = nx/this%trans%box_info%npx
  out%dim%ny = ny/this%trans%box_info%npy
  this%outnx = out%dim%nx
  this%outny = out%dim%ny
  steplon = steplon*this%trans%box_info%npx
  steplat = steplat*this%trans%box_info%npy

  CALL set_val(out, xmin=xmin_new, ymin=ymin_new, &
   xmax=xmin_new + DBLE(out%dim%nx-1)*steplon, dx=steplon, &
   ymax=ymin_new + DBLE(out%dim%ny-1)*steplat, dy=steplat)

  this%valid = .TRUE. ! warning, no check of subtype

ELSE IF (this%trans%trans_type == 'inter') THEN

  CALL outgrid_setup() ! common setup for grid-generating methods

  IF (this%trans%sub_type == 'near' .OR. this%trans%sub_type == 'bilin'&
    .OR. this%trans%sub_type == 'shapiro_near') THEN

    CALL get_val(in, nx=this%innx, ny=this%inny, &
     xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
    CALL get_val(out, nx=this%outnx, ny=this%outny)

    ALLOCATE(this%inter_index_x(this%outnx,this%outny), &
     this%inter_index_y(this%outnx,this%outny))
    CALL copy(out, lout)
    CALL unproj(lout)

    IF (this%trans%sub_type == 'bilin') THEN
      CALL this%find_index(in, .FALSE., &
       this%innx, this%inny, xmin, xmax, ymin, ymax, &
       lout%dim%lon, lout%dim%lat, this%trans%extrap, &
       this%inter_index_x, this%inter_index_y)

      ALLOCATE(this%inter_x(this%innx,this%inny), &
       this%inter_y(this%innx,this%inny))
      ALLOCATE(this%inter_xp(this%outnx,this%outny), &
       this%inter_yp(this%outnx,this%outny))

! compute coordinates of input grid
      CALL griddim_gen_coord(in, this%inter_x, this%inter_y)
! compute coordinates of output grid in input system
      CALL proj(in, lout%dim%lon, lout%dim%lat, this%inter_xp, this%inter_yp)

    ELSE ! near, shapiro_near
      CALL this%find_index(in, .TRUE., &
       this%innx, this%inny, xmin, xmax, ymin, ymax, &
       lout%dim%lon, lout%dim%lat, this%trans%extrap, &
       this%inter_index_x, this%inter_index_y)

    ENDIF

    CALL delete(lout)
    this%valid = .TRUE.
  ENDIF

ELSE IF (this%trans%trans_type == 'boxinter') THEN

  CALL outgrid_setup() ! common setup for grid-generating methods
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
  CALL copy(in, lin)
  CALL unproj(lin)
! use find_index in the opposite way, here extrap does not make sense
  CALL this%find_index(out, .TRUE., &
   this%outnx, this%outny, xmin, xmax, ymin, ymax, &
   lin%dim%lon, lin%dim%lat, .FALSE., &
   this%inter_index_x, this%inter_index_y)

  CALL delete(lin)
  this%valid = .TRUE. ! warning, no check of subtype

ELSE IF (this%trans%trans_type == 'stencilinter') THEN

  CALL outgrid_setup() ! common setup for grid-generating methods
! from inter:near
  CALL get_val(in, nx=this%innx, ny=this%inny, &
   xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
  CALL get_val(out, nx=this%outnx, ny=this%outny)

  ALLOCATE (this%inter_index_x(this%outnx,this%outny), &
   this%inter_index_y(this%outnx,this%outny))
  CALL copy(out, lout)
  CALL unproj(lout)
  CALL this%find_index(in, .TRUE., &
   this%innx, this%inny, xmin, xmax, ymin, ymax, &
   lout%dim%lon, lout%dim%lat, this%trans%extrap, &
   this%inter_index_x, this%inter_index_y)

! define the stencil mask
  nr = INT(this%trans%area_info%radius) ! integer radius
  n = nr*2+1 ! n. of points
  nm = nr + 1 ! becomes index of center
  r2 = this%trans%area_info%radius**2
  ALLOCATE(this%stencil(n,n))
  this%stencil(:,:) = .TRUE.
  DO iy = 1, n
    DO ix = 1, n
      IF ((ix-nm)**2+(iy-nm)**2 > r2) this%stencil(ix,iy) = .FALSE.
    ENDDO
  ENDDO

! set to missing the elements of inter_index too close to the domain
! borders
  xnmin = nr + 1
  xnmax = this%innx - nr
  ynmin = nr + 1
  ynmax = this%inny - nr
  DO iy = 1, this%outny
    DO ix = 1, this%outnx
      IF (this%inter_index_x(ix,iy) < xnmin .OR. &
       this%inter_index_x(ix,iy) > xnmax .OR. &
       this%inter_index_y(ix,iy) < ynmin .OR. &
       this%inter_index_y(ix,iy) > ynmax) THEN
        this%inter_index_x(ix,iy) = imiss
        this%inter_index_y(ix,iy) = imiss
      ENDIF
    ENDDO
  ENDDO

#ifdef DEBUG
  CALL l4f_category_log(this%category, L4F_DEBUG, &
   'stencilinter: stencil size '//t2c(n*n))
  CALL l4f_category_log(this%category, L4F_DEBUG, &
   'stencilinter: stencil points '//t2c(COUNT(this%stencil)))
#endif

  CALL delete(lout)
  this%valid = .TRUE. ! warning, no check of subtype

ELSE IF (this%trans%trans_type == 'maskgen') THEN

  IF (this%trans%sub_type == 'poly') THEN

    CALL copy(in, out)
    CALL get_val(in, nx=this%innx, ny=this%inny)
    this%outnx = this%innx
    this%outny = this%inny

! unlike before, here index arrays must have the shape of input grid
    ALLOCATE(this%inter_index_x(this%innx,this%inny), &
     this%inter_index_y(this%innx,this%inny))
    this%inter_index_x(:,:) = imiss
    this%inter_index_y(:,:) = 1

! compute coordinates of input grid in geo system
    CALL unproj(out) ! should be unproj(lin)

    nprev = 1
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(j, i, n, point) FIRSTPRIVATE(nprev)
    DO j = 1, this%inny
      inside_x: DO i = 1, this%innx
        point = georef_coord_new(x=out%dim%lon(i,j), y=out%dim%lat(i,j))

        DO n = nprev, this%trans%poly%arraysize ! optimize starting from last matched polygon
          IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
            this%inter_index_x(i,j) = n
            nprev = n
            CYCLE inside_x
          ENDIF
        ENDDO
        DO n = nprev-1, 1, -1 ! test the other polygons
          IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
            this%inter_index_x(i,j) = n
            nprev = n
            CYCLE inside_x
          ENDIF
        ENDDO

!     CALL delete(point) ! speedup
      ENDDO inside_x
    ENDDO
!$OMP END PARALLEL

  ELSE IF (this%trans%sub_type == 'grid') THEN
! here out(put grid) is abused for indicating the box-generating grid
! but the real output grid is the input grid
    CALL copy(out, lout) ! save out for local use
    CALL delete(out) ! needed before copy
    CALL copy(in, out)
    CALL get_val(in, nx=this%innx, ny=this%inny)
    this%outnx = this%innx
    this%outny = this%inny
    CALL get_val(lout, nx=nx, ny=ny, &
     xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

! unlike before, here index arrays must have the shape of input grid
    ALLOCATE(this%inter_index_x(this%innx,this%inny), &
     this%inter_index_y(this%innx,this%inny))

! compute coordinates of input/output grid in geo system
    CALL unproj(out)

! use find_index in the opposite way, here extrap does not make sense
    CALL this%find_index(lout, .TRUE., &
     nx, ny, xmin, xmax, ymin, ymax, &
     out%dim%lon, out%dim%lat, .FALSE., &
     this%inter_index_x, this%inter_index_y)
! transform indices to 1-d for mask generation
    WHERE(c_e(this%inter_index_x(:,:)))
      this%inter_index_x(:,:) = this%inter_index_x(:,:) + &
       (this%inter_index_y(:,:)-1)*nx
    END WHERE

    CALL delete(lout)
  ENDIF

  this%valid = .TRUE.

ELSE IF (this%trans%trans_type == 'polyinter') THEN

! this is the only difference wrt maskgen:poly
   this%recur = .TRUE. ! grid-to-grid polyinter is done in two steps!

  CALL copy(in, out)
  CALL get_val(in, nx=this%innx, ny=this%inny)
  this%outnx = this%innx
  this%outny = this%inny

! unlike before, here index arrays must have the shape of input grid
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))
  this%inter_index_x(:,:) = imiss
  this%inter_index_y(:,:) = 1

! compute coordinates of input grid in geo system
  CALL unproj(out) ! should be unproj(lin)

  nprev = 1
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(j, i, n, point) FIRSTPRIVATE(nprev)
  DO j = 1, this%inny
    inside_x_2: DO i = 1, this%innx
      point = georef_coord_new(x=out%dim%lon(i,j), y=out%dim%lat(i,j))

      DO n = nprev, this%trans%poly%arraysize ! optimize starting from last matched polygon
        IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
          this%inter_index_x(i,j) = n
          nprev = n
          CYCLE inside_x_2
        ENDIF
      ENDDO
      DO n = nprev-1, 1, -1 ! test the other polygons
        IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
          this%inter_index_x(i,j) = n
          nprev = n
          CYCLE inside_x_2
        ENDIF
      ENDDO

!     CALL delete(point) ! speedup
    ENDDO inside_x_2
  ENDDO
!$OMP END PARALLEL

  this%valid = .TRUE. ! warning, no check of subtype

ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

  CALL copy(in, out)
  CALL get_val(in, nx=this%innx, ny=this%inny)
  this%outnx = this%innx
  this%outny = this%inny

  IF (this%trans%sub_type == 'maskvalid' .OR. this%trans%sub_type == 'maskinvalid') THEN

    IF (.NOT.PRESENT(maskgrid)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskgrid argument missing for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      CALL raise_error()
      RETURN
    ENDIF

    IF (this%innx /= SIZE(maskgrid,1) .OR. this%inny /= SIZE(maskgrid,2)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init mask non conformal with input field')
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'mask: '//t2c(SIZE(maskgrid,1))//'x'//t2c(SIZE(maskgrid,2))// &
       ' input field:'//t2c(this%innx)//'x'//t2c(this%inny))
      CALL raise_error()
      RETURN
    ENDIF

    ALLOCATE(this%point_mask(this%innx,this%inny))

    IF (this%trans%sub_type == 'maskvalid') THEN
! behavior depends on the presence/usability of maskbounds,
! simplified wrt its use in metamorphosis:mask
      IF (.NOT.PRESENT(maskbounds)) THEN
        this%point_mask(:,:) = c_e(maskgrid(:,:))
      ELSE IF (SIZE(maskbounds) < 2) THEN
        this%point_mask(:,:) = c_e(maskgrid(:,:))
      ELSE
        this%point_mask(:,:) = c_e(maskgrid(:,:)) .AND. &
         maskgrid(:,:) > maskbounds(1) .AND. &
         maskgrid(:,:) <= maskbounds(SIZE(maskbounds))
      ENDIF
    ELSE ! reverse condition
      this%point_mask(:,:) = .NOT.c_e(maskgrid(:,:))
    ENDIF

    this%valid = .TRUE.

  ELSE IF (this%trans%sub_type == 'setinvalidto') THEN

    IF (.NOT.PRESENT(maskbounds)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskbounds missing for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      RETURN
    ELSE IF (SIZE(maskbounds) < 1) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskbounds empty for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      RETURN
    ELSE
      this%val1 = maskbounds(1)
#ifdef DEBUG
      CALL l4f_category_log(this%category, L4F_DEBUG, &
       "grid_transform_init setting invalid data to "//t2c(this%val1))
#endif
    ENDIF

    this%valid = .TRUE.

  ELSE IF (this%trans%sub_type == 'settoinvalid') THEN

    IF (.NOT.PRESENT(maskbounds)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskbounds missing for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      CALL raise_error()
      RETURN
    ELSE IF (SIZE(maskbounds) < 2) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskbounds must have at least 2 elements for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      CALL raise_error()
      RETURN
    ELSE
      this%val1 = maskbounds(1)
      this%val2 = maskbounds(SIZE(maskbounds))
#ifdef DEBUG
      CALL l4f_category_log(this%category, L4F_DEBUG, &
       "grid_transform_init setting to invalid interval ]"//t2c(this%val1)//','// &
       t2c(this%val2)//']')
#endif
    ENDIF

    this%valid = .TRUE.

  ENDIF

ENDIF

CONTAINS

! local subroutine to be called by all methods interpolating to a new
! grid, no parameters passed, used as a macro to avoid repeating code
SUBROUTINE outgrid_setup()

! set increments in new grid in order for all the baraque to work
CALL griddim_setsteps(out)
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
  ELSE
    CALL set_val(out, component_flag=cf_i)
  ENDIF
ENDIF
! rotate the input grid by n*360 degrees to bring it closer to the output grid
CALL griddim_set_central_lon(in, griddim_central_lon(out))

END SUBROUTINE outgrid_setup

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
!!  - for 'polyinter' transformation, no target point information has
!!    to be provided in input (it is calculated on the basis of input
!!    grid and \a trans object), and the coordinates of the target
!!    points (polygons' centroids) are returned in output in \a
!!    v7d_out argument
!!
!!  - for 'maskinter' transformation, this is a two dimensional real
!!    field (\a maskgrid argument), which, together with the \a
!!    maskbounds argument (optional with default), divides the input
!!    grid in a number of subareas according to the values of \a
!!    maskinter, and, in this case, \a v7d_out is an output argument
!!    which returns the coordinates of the target points (subareas'
!!    centroids)
!!
!!  - for 'metamorphosis' transformation, no target point information
!!    has to be provided in input (it is calculated on the basis of
!!    input grid and \a trans object), except for 'mask' subtype, for
!!    which the same information as for 'maskinter' transformation has
!!    to be provided; in all the cases, as for 'polyinter', the
!!    information about target points is returned in output in \a
!!    v7d_out argument.
!!
!! The generated \a grid_transform object is specific to the grid and
!! sparse point list provided or computed. The function \a c_e can be
!! used in order to check whether the object has been successfully
!! initialised, if the result is \a .FALSE., it should not be used
!! further on.
SUBROUTINE grid_transform_grid_vol7d_init(this, trans, in, v7d_out, &
 maskgrid, maskbounds, find_index, categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(in) :: in !< griddim object to transform
TYPE(vol7d),INTENT(inout) :: v7d_out !< vol7d object with the coordinates of the sparse points to be used as transformation target (input or output depending on type of transformation)
REAL,INTENT(in),OPTIONAL :: maskgrid(:,:) !< 2D field to be used for defining subareas according to its values, it must have the same shape as the field to be interpolated (for transformation type 'maskinter' and 'metamorphosis:mask')
REAL,INTENT(in),OPTIONAL :: maskbounds(:) !< array of boundary values for defining subareas from the values of \a maskgrid, the number of subareas is SIZE(maskbounds) - 1, if not provided a default based on extreme values of \a maskgrid is used
PROCEDURE(basic_find_index),POINTER,OPTIONAL :: find_index
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: ix, iy, n, nm, nr, nprev, nmaskarea, xnmin, xnmax, ynmin, ynmax, &
 time_definition
DOUBLE PRECISION :: xmin, xmax, ymin, ymax, r2, lonref
DOUBLE PRECISION,ALLOCATABLE :: lon1(:), lat1(:), lon(:,:), lat(:,:)
REAL,ALLOCATABLE :: lmaskbounds(:)
TYPE(georef_coord) :: point
TYPE(griddim_def) :: lin


IF (PRESENT(find_index)) THEN ! move in init_common?
  IF (ASSOCIATED(find_index)) THEN
    this%find_index => find_index
  ENDIF
ENDIF
CALL grid_transform_init_common(this, trans, categoryappend)
#ifdef DEBUG
CALL l4f_category_log(this%category, L4F_DEBUG, "grid_transform vg6d-v7d")
#endif

! used after in some transformations
CALL get_val(trans, time_definition=time_definition)
IF (.NOT. c_e(time_definition)) THEN
  time_definition=1  ! default to validity time
ENDIF

IF (this%trans%trans_type == 'inter') THEN

  IF (this%trans%sub_type == 'near' .OR. this%trans%sub_type == 'bilin' &
   .OR. this%trans%sub_type == 'shapiro_near') THEN

    CALL copy(in, lin)
    CALL get_val(lin, nx=this%innx, ny=this%inny)
    this%outnx = SIZE(v7d_out%ana)
    this%outny = 1

    ALLOCATE (this%inter_index_x(this%outnx,this%outny),&
     this%inter_index_y(this%outnx,this%outny))
    ALLOCATE(lon(this%outnx,1),lat(this%outnx,1))

    CALL getval(v7d_out%ana(:)%coord,lon=lon(:,1),lat=lat(:,1))
! equalize in/out coordinates
    lonref = 0.5D0*(MAXVAL(lon(:,1), mask=c_e(lon(:,1))) + MINVAL(lon(:,1), mask=c_e(lon(:,1))))
    CALL griddim_set_central_lon(lin, lonref)
    CALL get_val(lin, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

    IF (this%trans%sub_type == 'bilin') THEN
      CALL this%find_index(lin, .FALSE., &
       this%innx, this%inny, xmin, xmax, ymin, ymax, &
       lon, lat, this%trans%extrap, &
       this%inter_index_x, this%inter_index_y)

      ALLOCATE(this%inter_x(this%innx,this%inny),this%inter_y(this%innx,this%inny))
      ALLOCATE(this%inter_xp(this%outnx,this%outny),this%inter_yp(this%outnx,this%outny))

      CALL griddim_gen_coord(lin, this%inter_x, this%inter_y)
      CALL proj(lin, lon, lat, this%inter_xp, this%inter_yp)

    ELSE ! near shapiro_near
      CALL this%find_index(lin, .TRUE., &
       this%innx, this%inny, xmin, xmax, ymin, ymax, &
       lon, lat, this%trans%extrap, &
       this%inter_index_x, this%inter_index_y)

    ENDIF

    DEALLOCATE(lon,lat)
    CALL delete(lin)

    this%valid = .TRUE.

  ENDIF

ELSE IF (this%trans%trans_type == 'polyinter') THEN

  CALL get_val(in, nx=this%innx, ny=this%inny)
! unlike before, here index arrays must have the shape of input grid
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))
  this%inter_index_x(:,:) = imiss
  this%inter_index_y(:,:) = 1

! compute coordinates of input grid in geo system
  CALL copy(in, lin)
  CALL unproj(lin)

  this%outnx = this%trans%poly%arraysize
  this%outny = 1
  CALL delete(v7d_out) ! required to avoid leaks because intent(inout), dangerous
  CALL init(v7d_out, time_definition=time_definition)
  CALL vol7d_alloc(v7d_out, nana=this%outnx)

! equalize in/out coordinates
  ALLOCATE(lon(this%outnx,1))
  CALL getval(v7d_out%ana(:)%coord,lon=lon(:,1))
  lonref = 0.5D0*(MAXVAL(lon(:,1), mask=c_e(lon(:,1))) + MINVAL(lon(:,1), mask=c_e(lon(:,1))))
  CALL griddim_set_central_lon(lin, lonref)
  DEALLOCATE(lon)

! setup output point list, equal to average of polygon points
! warning, in case of concave areas points may coincide!
  CALL poly_to_coordinates(this%trans%poly, v7d_out)

  nprev = 1
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(iy, ix, n, point) FIRSTPRIVATE(nprev)
  DO iy = 1, this%inny
    inside_x: DO ix = 1, this%innx
      point = georef_coord_new(x=lin%dim%lon(ix,iy), y=lin%dim%lat(ix,iy))

      DO n = nprev, this%trans%poly%arraysize ! optimize starting from last matched polygon
        IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
          this%inter_index_x(ix,iy) = n
          nprev = n
          CYCLE inside_x
        ENDIF
      ENDDO
      DO n = nprev-1, 1, -1 ! test the other polygons
        IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
          this%inter_index_x(ix,iy) = n
          nprev = n
          CYCLE inside_x
        ENDIF
      ENDDO

!     CALL delete(point) ! speedup
    ENDDO inside_x
  ENDDO
!$OMP END PARALLEL

#ifdef DEBUG
  DO n = 1, this%trans%poly%arraysize
    CALL l4f_category_log(this%category, L4F_DEBUG, &
     'Polygon: '//t2c(n)//' grid points: '// &
     t2c(COUNT(this%inter_index_x(:,:) == n)))
  ENDDO
#endif

  CALL delete(lin)
  this%valid = .TRUE. ! warning, no check of subtype

ELSE IF (this%trans%trans_type == 'stencilinter') THEN
  
! from inter:near
  CALL copy(in, lin)
  CALL get_val(lin, nx=this%innx, ny=this%inny)
  this%outnx = SIZE(v7d_out%ana)
  this%outny = 1

  ALLOCATE (this%inter_index_x(this%outnx,this%outny),&
   this%inter_index_y(this%outnx,this%outny))
  ALLOCATE(lon(this%outnx,1),lat(this%outnx,1))

  CALL getval(v7d_out%ana(:)%coord,lon=lon(:,1),lat=lat(:,1))
! equalize in/out coordinates
  lonref = 0.5D0*(MAXVAL(lon(:,1), mask=c_e(lon(:,1))) + MINVAL(lon(:,1), mask=c_e(lon(:,1))))
  CALL griddim_set_central_lon(lin, lonref)

  CALL get_val(lin, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

  CALL this%find_index(lin, .TRUE., &
   this%innx, this%inny, xmin, xmax, ymin, ymax, &
   lon, lat, this%trans%extrap, &
   this%inter_index_x, this%inter_index_y)

! define the stencil mask
  nr = INT(this%trans%area_info%radius) ! integer radius
  n = nr*2+1 ! n. of points
  nm = nr + 1 ! becomes index of center
  r2 = this%trans%area_info%radius**2
  ALLOCATE(this%stencil(n,n))
  this%stencil(:,:) = .TRUE.
  DO iy = 1, n
    DO ix = 1, n
      IF ((ix-nm)**2+(iy-nm)**2 > r2) this%stencil(ix,iy) = .FALSE.
    ENDDO
  ENDDO

! set to missing the elements of inter_index too close to the domain
! borders
  xnmin = nr + 1
  xnmax = this%innx - nr
  ynmin = nr + 1
  ynmax = this%inny - nr
  DO iy = 1, this%outny
    DO ix = 1, this%outnx
      IF (this%inter_index_x(ix,iy) < xnmin .OR. &
       this%inter_index_x(ix,iy) > xnmax .OR. &
       this%inter_index_y(ix,iy) < ynmin .OR. &
       this%inter_index_y(ix,iy) > ynmax) THEN
        this%inter_index_x(ix,iy) = imiss
        this%inter_index_y(ix,iy) = imiss
      ENDIF
    ENDDO
  ENDDO

#ifdef DEBUG
  CALL l4f_category_log(this%category, L4F_DEBUG, &
   'stencilinter: stencil size '//t2c(n*n))
  CALL l4f_category_log(this%category, L4F_DEBUG, &
   'stencilinter: stencil points '//t2c(COUNT(this%stencil)))
#endif

  DEALLOCATE(lon,lat)
  CALL delete(lin)

  this%valid = .TRUE. ! warning, no check of subtype

ELSE IF (this%trans%trans_type == 'maskinter') THEN

  IF (.NOT.PRESENT(maskgrid)) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'grid_transform_init maskgrid argument missing for maskinter transformation')
    CALL raise_fatal_error()
  ENDIF

  CALL get_val(in, nx=this%innx, ny=this%inny)
  IF (this%innx /= SIZE(maskgrid,1) .OR. this%inny /= SIZE(maskgrid,2)) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'grid_transform_init mask non conformal with input field')
    CALL l4f_category_log(this%category,L4F_ERROR, &
     'mask: '//t2c(SIZE(maskgrid,1))//'x'//t2c(SIZE(maskgrid,2))// &
     ' input field:'//t2c(this%innx)//'x'//t2c(this%inny))
    CALL raise_fatal_error()
  ENDIF
! unlike before, here index arrays must have the shape of input grid
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))
  this%inter_index_x(:,:) = imiss
  this%inter_index_y(:,:) = 1

! generate the subarea boundaries according to maskgrid and maskbounds
  CALL gen_mask_class()

! compute coordinates of input grid in geo system
  CALL copy(in, lin)
  CALL unproj(lin)

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(iy, ix, n)
  DO iy = 1, this%inny
    DO ix = 1, this%innx
      IF (c_e(maskgrid(ix,iy))) THEN
        IF (maskgrid(ix,iy) <= lmaskbounds(nmaskarea+1)) THEN
          DO n = nmaskarea, 1, -1
            IF (maskgrid(ix,iy) > lmaskbounds(n)) THEN
              this%inter_index_x(ix,iy) = n
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDDO
  ENDDO
!$OMP END PARALLEL

  this%outnx = nmaskarea
  this%outny = 1
  CALL delete(v7d_out) ! required to avoid leaks because intent(inout), dangerous
  CALL init(v7d_out, time_definition=time_definition)
  CALL vol7d_alloc(v7d_out, nana=nmaskarea)

! setup output point list, equal to average of polygon points
! warning, in case of concave areas points may coincide!
  DO n = 1, nmaskarea
    CALL init(v7d_out%ana(n), &
     lon=stat_average(PACK(lin%dim%lon(:,:), &
     mask=(this%inter_index_x(:,:) == n))), &
     lat=stat_average(PACK(lin%dim%lat(:,:), &
     mask=(this%inter_index_x(:,:) == n))))
  ENDDO

  CALL delete(lin)
  this%valid = .TRUE. ! warning, no check of subtype

ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

! common to all metamorphosis subtypes
! compute coordinates of input grid in geo system
  CALL copy(in, lin)
  CALL unproj(lin)

  CALL get_val(in, nx=this%innx, ny=this%inny)
! allocate index array
  ALLOCATE(this%point_index(this%innx,this%inny))
  this%point_index(:,:) = imiss
! setup output coordinates
  CALL delete(v7d_out) ! required to avoid leaks because intent(inout), dangerous
  CALL init(v7d_out, time_definition=time_definition)

  IF (this%trans%sub_type == 'all' ) THEN

    this%outnx = this%innx*this%inny
    this%outny = 1
    CALL vol7d_alloc(v7d_out, nana=this%outnx)

    n = 0
    DO iy=1,this%inny
      DO ix=1,this%innx
        CALL init(v7d_out%ana((iy-1)*this%innx+ix), &
         lon=lin%dim%lon(ix,iy),lat=lin%dim%lat(ix,iy))
        n = n + 1
        this%point_index(ix,iy) = n
      ENDDO
    ENDDO

    this%valid = .TRUE.

  ELSE IF (this%trans%sub_type == 'coordbb' ) THEN

! count and mark points falling into requested bounding-box
    this%outnx = 0
    this%outny = 1
    DO iy = 1, this%inny
      DO ix = 1, this%innx
!        IF (geo_coord_inside_rectang()
        IF (lin%dim%lon(ix,iy) > this%trans%rect_coo%ilon .AND. &
         lin%dim%lon(ix,iy) < this%trans%rect_coo%flon .AND. &
         lin%dim%lat(ix,iy) > this%trans%rect_coo%ilat .AND. &
         lin%dim%lat(ix,iy) < this%trans%rect_coo%flat) THEN ! improve!
          this%outnx = this%outnx + 1
          this%point_index(ix,iy) = this%outnx
        ENDIF
      ENDDO
    ENDDO

    IF (this%outnx <= 0) THEN
      CALL l4f_category_log(this%category,L4F_WARN, &
       "metamorphosis:coordbb: no points inside bounding box "//&
       TRIM(to_char(this%trans%rect_coo%ilon))//","// &
       TRIM(to_char(this%trans%rect_coo%flon))//","// &
       TRIM(to_char(this%trans%rect_coo%ilat))//","// &
       TRIM(to_char(this%trans%rect_coo%flat)))
    ENDIF

    CALL vol7d_alloc(v7d_out, nana=this%outnx)

! collect coordinates of points falling into requested bounding-box
    n = 0
    DO iy = 1, this%inny
      DO ix = 1, this%innx
        IF (c_e(this%point_index(ix,iy))) THEN
          n = n + 1
          CALL init(v7d_out%ana(n), &
           lon=lin%dim%lon(ix,iy), lat=lin%dim%lat(ix,iy))
        ENDIF
      ENDDO
    ENDDO

    this%valid = .TRUE.

  ELSE IF (this%trans%sub_type == 'poly' ) THEN

! count and mark points falling into requested polygon
    this%outnx = 0
    this%outny = 1

! this OMP block has to be checked
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(iy, ix, point, n) REDUCTION(+:this%outnx)
    DO iy = 1, this%inny
      DO ix = 1, this%innx
        point = georef_coord_new(x=lin%dim%lon(ix,iy), y=lin%dim%lat(ix,iy))
        DO n = 1, this%trans%poly%arraysize
          IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
            this%outnx = this%outnx + 1
            this%point_index(ix,iy) = n
            EXIT
          ENDIF
        ENDDO
!     CALL delete(point) ! speedup
      ENDDO
    ENDDO
!$OMP END PARALLEL

    IF (this%outnx <= 0) THEN
      CALL l4f_category_log(this%category,L4F_WARN, &
       "metamorphosis:poly: no points inside polygons")
    ENDIF

    CALL vol7d_alloc(v7d_out, nana=this%outnx)
! collect coordinates of points falling into requested polygon
    n = 0
    DO iy = 1, this%inny
      DO ix = 1, this%innx
        IF (c_e(this%point_index(ix,iy))) THEN
          n = n + 1
          CALL init(v7d_out%ana(n), &
           lon=lin%dim%lon(ix,iy), lat=lin%dim%lat(ix,iy))
        ENDIF
      ENDDO
    ENDDO

    this%valid = .TRUE.

  ELSE IF (this%trans%sub_type == 'mask' ) THEN

    IF (.NOT.PRESENT(maskgrid)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskgrid argument missing for metamorphosis:mask transformation')
      CALL raise_error()
      RETURN
    ENDIF

    IF (this%innx /= SIZE(maskgrid,1) .OR. this%inny /= SIZE(maskgrid,2)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init mask non conformal with input field')
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'mask: '//t2c(SIZE(maskgrid,1))//'x'//t2c(SIZE(maskgrid,2))// &
       ' input field:'//t2c(this%innx)//'x'//t2c(this%inny))
      CALL raise_error()
      RETURN
    ENDIF

! generate the subarea boundaries according to maskgrid and maskbounds
    CALL gen_mask_class()

    this%outnx = 0
    this%outny = 1

! this OMP block has to be checked
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(iy, ix) REDUCTION(+:this%outnx)
    DO iy = 1, this%inny
      DO ix = 1, this%innx
        IF (c_e(maskgrid(ix,iy))) THEN
          IF (maskgrid(ix,iy) <= lmaskbounds(nmaskarea+1)) THEN
            DO n = nmaskarea, 1, -1
              IF (maskgrid(ix,iy) > lmaskbounds(n)) THEN
                this%outnx = this%outnx + 1
                this%point_index(ix,iy) = n
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
    ENDDO
!$OMP END PARALLEL

    IF (this%outnx <= 0) THEN
      CALL l4f_category_log(this%category,L4F_WARN, &
       "grid_transform_init no points inside mask for metamorphosis:mask transformation")
    ENDIF
#ifdef DEBUG
    DO n = 1, nmaskarea
      CALL l4f_category_log(this%category,L4F_INFO, &
       "points in subarea "//t2c(n)//": "// &
       t2c(COUNT(this%point_index(:,:) == n)))
    ENDDO
#endif
    CALL vol7d_alloc(v7d_out, nana=this%outnx)
! collect coordinates of points falling into requested polygon
    n = 0
    DO iy = 1, this%inny
      DO ix = 1, this%innx
        IF (c_e(this%point_index(ix,iy))) THEN
          n = n + 1
          CALL init(v7d_out%ana(n),lon=lin%dim%lon(ix,iy),lat=lin%dim%lat(ix,iy))
        ENDIF
      ENDDO
    ENDDO

    this%valid = .TRUE.

  ENDIF
  CALL delete(lin)
ENDIF

CONTAINS

SUBROUTINE gen_mask_class()
REAL :: startmaskclass, mmin, mmax
INTEGER :: i

IF (PRESENT(maskbounds)) THEN
  nmaskarea = SIZE(maskbounds) - 1
  IF (nmaskarea > 0) THEN
    lmaskbounds = maskbounds ! automatic f2003 allocation
    RETURN
  ELSE IF (nmaskarea == 0) THEN
    CALL l4f_category_log(this%category,L4F_WARN, &
     'only one value provided for maskbounds, '//t2c(maskbounds(1)) &
     //', it will be ignored')
    CALL l4f_category_log(this%category,L4F_WARN, &
     'at least 2 values are required for maskbounds')
  ENDIF
ENDIF

mmin = MINVAL(maskgrid, mask=c_e(maskgrid))
mmax = MAXVAL(maskgrid, mask=c_e(maskgrid))

nmaskarea = INT(mmax - mmin + 1.5)
startmaskclass = mmin - 0.5
! assign limits for each class
ALLOCATE(lmaskbounds(nmaskarea+1))
lmaskbounds(:) = (/(startmaskclass+REAL(i),i=0,nmaskarea)/)
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG, &
 'maskinter, '//t2c(nmaskarea)//' subareas defined, with boundaries:')
DO i = 1, SIZE(lmaskbounds)
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   'maskinter '//t2c(i)//' '//t2c(lmaskbounds(i)))
ENDDO
#endif

END SUBROUTINE gen_mask_class

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

INTEGER :: nx, ny
DOUBLE PRECISION :: xmin, xmax, ymin, ymax, lonref
DOUBLE PRECISION,ALLOCATABLE :: lon(:,:),lat(:,:)
TYPE(griddim_def) :: lout


CALL grid_transform_init_common(this, trans, categoryappend)
#ifdef DEBUG
CALL l4f_category_log(this%category, L4F_DEBUG, "grid_transform v7d-vg6d")
#endif

IF (this%trans%trans_type == 'inter') THEN

  IF ( this%trans%sub_type == 'linear' ) THEN
    
    this%innx=SIZE(v7d_in%ana)
    this%inny=1
    ALLOCATE(lon(this%innx,1),lat(this%innx,1))
    ALLOCATE(this%inter_xp(this%innx,this%inny),this%inter_yp(this%innx,this%inny))
    CALL getval(v7d_in%ana(:)%coord,lon=lon(:,1),lat=lat(:,1))

    CALL copy (out, lout)
! equalize in/out coordinates
    lonref = 0.5D0*(MAXVAL(lon(:,1), mask=c_e(lon(:,1))) + MINVAL(lon(:,1), mask=c_e(lon(:,1))))
    CALL griddim_set_central_lon(lout, lonref)

    CALL get_val(lout, nx=nx, ny=ny)
    this%outnx=nx
    this%outny=ny
    ALLOCATE(this%inter_x(this%outnx,this%outny),this%inter_y(this%outnx,this%outny))

    CALL get_val(lout, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
    CALL proj(lout, lon, lat, this%inter_xp, this%inter_yp)
    CALL griddim_gen_coord(lout, this%inter_x, this%inter_y)

    DEALLOCATE(lon,lat)
    CALL delete(lout)

    this%valid = .TRUE.

  ENDIF

ELSE IF (this%trans%trans_type == 'boxinter') THEN

  this%innx=SIZE(v7d_in%ana)
  this%inny=1
! index arrays must have the shape of input grid
  ALLOCATE(lon(this%innx,1),lat(this%innx,1))
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))
! get coordinates of input grid in geo system
  CALL getval(v7d_in%ana(:)%coord,lon=lon(:,1),lat=lat(:,1))

  CALL copy (out, lout)
! equalize in/out coordinates
  lonref = 0.5D0*(MAXVAL(lon(:,1), mask=c_e(lon(:,1))) + MINVAL(lon(:,1), mask=c_e(lon(:,1))))
  CALL griddim_set_central_lon(lout, lonref)

  CALL get_val(lout, nx=this%outnx, ny=this%outny, &
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

! use find_index in the opposite way, here extrap does not make sense
  CALL this%find_index(lout, .TRUE., &
   this%outnx, this%outny, xmin, xmax, ymin, ymax, &
   lon, lat, .FALSE., &
   this%inter_index_x, this%inter_index_y)

  DEALLOCATE(lon,lat)
  CALL delete(lout)

  this%valid = .TRUE. ! warning, no check of subtype

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
!! data, and, if required by the transformation type, the information
!! about the target sparse points over which the transformation should
!! take place:
!!
!!  - for 'inter' transformation, this is provided in the form of a
!!    vol7d object (\a v7d_out argument, input), which must have been
!!    initialized with the coordinates of desired sparse points
!!
!!  - for 'polyinter' transformation, no target point information has
!!    to be provided in input (it is calculated on the basis of input
!!    grid and \a trans object), and the coordinates of the target
!!    points (polygons' centroids) are returned in output in \a
!!    v7d_out argument
!!
!!  - for 'metamorphosis' transformation, no target point information
!!    has to be provided in input (it is calculated on the basis of
!!    input grid and \a trans object), and, as for 'polyinter', this
!!    information is returned in output in \a v7d_out argument.
!!
!! The generated \a grid_transform object is specific to the input and
!! output sparse point lists provided or computed. The function \a c_e
!! can be used in order to check whether the object has been
!! successfully initialised, if the result is \a .FALSE., it should
!! not be used further on.
SUBROUTINE grid_transform_vol7d_vol7d_init(this, trans, v7d_in, v7d_out, &
 maskbounds, categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(vol7d),INTENT(in) :: v7d_in !< vol7d object with the coordinates of the sparse point to be used as input (only information about coordinates is used)
TYPE(vol7d),INTENT(inout) :: v7d_out !< vol7d object with the coordinates of the sparse points to be used as transformation target (input or output depending on type of transformation, when output, it must have been initialised anyway)
REAL,INTENT(in),OPTIONAL :: maskbounds(:) !< array of boundary values for defining a subset of valid points where the values of \a maskgrid are within the first and last value of \a maskbounds (for transformation type 'metamorphosis:maskvalid/settoinvalid' and others)
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: i, n
DOUBLE PRECISION,ALLOCATABLE :: lon(:), lat(:)
! temporary, improve!!!!
DOUBLE PRECISION :: lon1, lat1
TYPE(georef_coord) :: point


CALL grid_transform_init_common(this, trans, categoryappend)
#ifdef DEBUG
CALL l4f_category_log(this%category, L4F_DEBUG, "grid_transform v7d-v7d")
#endif

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

    this%valid = .TRUE.

  ENDIF

ELSE IF (this%trans%trans_type == 'polyinter') THEN

  this%innx=SIZE(v7d_in%ana)
  this%inny=1
! unlike before, here index arrays must have the shape of input grid
  ALLOCATE(this%inter_index_x(this%innx,this%inny), &
   this%inter_index_y(this%innx,this%inny))
  this%inter_index_x(:,:) = imiss
  this%inter_index_y(:,:) = 1

  DO i = 1, SIZE(v7d_in%ana)
! temporary, improve!!!!
    CALL getval(v7d_in%ana(i)%coord,lon=lon1,lat=lat1)
    point = georef_coord_new(x=lon1, y=lat1)

    DO n = 1, this%trans%poly%arraysize
      IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
        this%inter_index_x(i,1) = n
        EXIT
      ENDIF
    ENDDO
  ENDDO

  this%outnx=this%trans%poly%arraysize
  this%outny=1
  CALL vol7d_alloc(v7d_out, nana=this%outnx)

! setup output point list, equal to average of polygon points
! warning, in case of concave areas points may coincide!
  CALL poly_to_coordinates(this%trans%poly, v7d_out)

  this%valid = .TRUE. ! warning, no check of subtype

ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

! common to all metamorphosis subtypes
  this%innx = SIZE(v7d_in%ana)
  this%inny = 1
! allocate index array
  ALLOCATE(this%point_index(this%innx,this%inny))
  this%point_index(:,:) = imiss

  IF (this%trans%sub_type == 'all' ) THEN

    CALL metamorphosis_all_setup()

  ELSE IF (this%trans%sub_type == 'coordbb' ) THEN

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
        this%point_index(i,1) = this%outnx
      ENDIF
    ENDDO

    IF (this%outnx <= 0) THEN
      CALL l4f_category_log(this%category,L4F_WARN, &
       "metamorphosis:coordbb: no points inside bounding box "//&
       TRIM(to_char(this%trans%rect_coo%ilon))//","// &
       TRIM(to_char(this%trans%rect_coo%flon))//","// &
       TRIM(to_char(this%trans%rect_coo%ilat))//","// &
       TRIM(to_char(this%trans%rect_coo%flat)))
    ENDIF

    CALL vol7d_alloc(v7d_out, nana=this%outnx)

! collect coordinates of points falling into requested bounding-box
    n = 0
    DO i = 1, this%innx
      IF (c_e(this%point_index(i,1))) THEN
        n = n + 1
        CALL init(v7d_out%ana(n),lon=lon(i),lat=lat(i))
      ENDIF
    ENDDO
    DEALLOCATE(lon, lat)

    this%valid = .TRUE.

  ELSE IF (this%trans%sub_type == 'poly' ) THEN

! count and mark points falling into requested polygon
    this%outnx = 0
    this%outny = 1
    DO i = 1, this%innx
! temporary, improve!!!!
      CALL getval(v7d_in%ana(i)%coord,lon=lon1,lat=lat1)
      point = georef_coord_new(x=lon1, y=lat1)
      DO n = 1, this%trans%poly%arraysize
        IF (inside(point, this%trans%poly%array(n))) THEN ! stop at the first matching polygon
          this%outnx = this%outnx + 1
          this%point_index(i,1) = n
          EXIT
        ENDIF
      ENDDO
!     CALL delete(point) ! speedup
    ENDDO

    IF (this%outnx <= 0) THEN
      CALL l4f_category_log(this%category,L4F_WARN, &
       "metamorphosis:poly: no points inside polygons")
    ENDIF

    CALL vol7d_alloc(v7d_out, nana=this%outnx)

! collect coordinates of points falling into requested polygon
    n = 0
    DO i = 1, this%innx
      IF (c_e(this%point_index(i,1))) THEN
        n = n + 1
! temporary, improve!!!!
        CALL getval(v7d_in%ana(i)%coord,lon=lon1,lat=lat1)
        CALL init(v7d_out%ana(n),lon=lon1,lat=lat1)
      ENDIF
    ENDDO

    this%valid = .TRUE.

  ELSE IF (this%trans%sub_type == 'setinvalidto' ) THEN

    IF (.NOT.PRESENT(maskbounds)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskbounds missing for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      RETURN
    ELSE IF (SIZE(maskbounds) < 1) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskbounds empty for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      RETURN
    ELSE
      this%val1 = maskbounds(1)
#ifdef DEBUG
      CALL l4f_category_log(this%category, L4F_DEBUG, &
       "grid_transform_init setting invalid data to "//t2c(this%val1))
#endif
    ENDIF

    CALL metamorphosis_all_setup()

  ELSE IF (this%trans%sub_type == 'settoinvalid' ) THEN

    IF (.NOT.PRESENT(maskbounds)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskbounds missing for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      CALL raise_error()
      RETURN
    ELSE IF (SIZE(maskbounds) < 2) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       'grid_transform_init maskbounds must have at least 2 elements for metamorphosis:'// &
       TRIM(this%trans%sub_type)//' transformation')
      CALL raise_error()
      RETURN
    ELSE
      this%val1 = maskbounds(1)
      this%val2 = maskbounds(SIZE(maskbounds))
#ifdef DEBUG
      CALL l4f_category_log(this%category, L4F_DEBUG, &
       "grid_transform_init setting to invalid interval ]"//t2c(this%val1)//','// &
       t2c(this%val2)//']')
#endif
    ENDIF

    CALL metamorphosis_all_setup()

  ENDIF
ENDIF

CONTAINS

! common code to metamorphosis transformations conserving the number
! of points
SUBROUTINE metamorphosis_all_setup()

this%outnx = SIZE(v7d_in%ana)
this%outny = 1
this%point_index(:,1) = (/(i,i=1,this%innx)/)
CALL vol7d_alloc(v7d_out, nana=SIZE(v7d_in%ana))
v7d_out%ana = v7d_in%ana

this%valid = .TRUE.

END SUBROUTINE metamorphosis_all_setup

END SUBROUTINE grid_transform_vol7d_vol7d_init


! Private subroutine for performing operations common to all constructors
SUBROUTINE grid_transform_init_common(this, trans, categoryappend)
TYPE(grid_transform),INTENT(inout) :: this
TYPE(transform_def),INTENT(in) :: trans
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend

CHARACTER(len=512) :: a_name

IF (PRESENT(categoryappend)) THEN
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory)//"."// &
   TRIM(categoryappend))
ELSE
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory))
ENDIF
this%category=l4f_category_get(a_name)

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,"start init_grid_transform")
#endif

this%trans=trans

END SUBROUTINE grid_transform_init_common

! internal subroutine to correctly initialise the output coordinates
! with polygon centroid coordinates
SUBROUTINE poly_to_coordinates(poly, v7d_out)
TYPE(arrayof_georef_coord_array),intent(in) :: poly
TYPE(vol7d),INTENT(inout) :: v7d_out

INTEGER :: n, sz
DOUBLE PRECISION,ALLOCATABLE :: lon(:), lat(:)

DO n = 1, poly%arraysize
  CALL getval(poly%array(n), x=lon, y=lat)
  sz = MIN(SIZE(lon), SIZE(lat))
  IF (lon(1) == lon(sz) .AND. lat(1) == lat(sz)) THEN ! closed polygon
    sz = sz - 1
  ENDIF
  CALL init(v7d_out%ana(n), lon=stat_average(lon(1:sz)), lat=stat_average(lat(1:sz)))
ENDDO

END SUBROUTINE poly_to_coordinates

!> Destructor of \a grid_tranform object.
!! It releases any memory and data associated to
!! \a grid_transform object \a this, the logger category will be deleted too.
SUBROUTINE grid_transform_delete(this)
TYPE(grid_transform),INTENT(inout) :: this !< grid_transform object

CALL delete(this%trans)

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
if (associated(this%inter_index_z)) deallocate (this%inter_index_z)
if (associated(this%point_index)) deallocate (this%point_index)

if (associated(this%inter_x)) deallocate (this%inter_x)
if (associated(this%inter_y)) deallocate (this%inter_y)

if (associated(this%inter_xp)) deallocate (this%inter_xp)
if (associated(this%inter_yp)) deallocate (this%inter_yp)
if (associated(this%inter_zp)) deallocate (this%inter_zp)
if (associated(this%vcoord_in)) deallocate (this%vcoord_in)
if (associated(this%vcoord_out)) deallocate (this%vcoord_out)
if (associated(this%point_mask)) deallocate (this%point_mask)
if (associated(this%stencil)) deallocate (this%stencil)
if (associated(this%output_level_auto)) deallocate (this%output_level_auto)
IF (ALLOCATED(this%coord_3d_in)) DEALLOCATE(this%coord_3d_in)
this%valid = .FALSE.

! close the logger
call l4f_category_delete(this%category)

END SUBROUTINE grid_transform_delete


!> Method for returning the contents of the object.
!! Only a few selected memebrs of \a grid_transform object can be
!! queried, this is mainly for use by \a volgrid6d_class, rather than
!! for public use.
SUBROUTINE grid_transform_get_val(this, output_level_auto, point_mask, &
 point_index, output_point_index, levshift, levused)
TYPE(grid_transform),INTENT(in) :: this !< object to examine
TYPE(vol7d_level),POINTER,OPTIONAL :: output_level_auto(:) !< array of auto-generated output levels
LOGICAL,INTENT(out),ALLOCATABLE,OPTIONAL :: point_mask(:) !< mask array indicating the input points that are kept in the output, for metamorphosis transformations
INTEGER,INTENT(out),ALLOCATABLE,OPTIONAL :: point_index(:) !< array of indices indicating the polygon to which every input point has been assigned, if applicable
INTEGER,INTENT(out),ALLOCATABLE,OPTIONAL :: output_point_index(:) !< array of indices indicating the polygon to which every output point has been assigned, if applicable
INTEGER,INTENT(out),OPTIONAL :: levshift !< shift between input and output levels for vertint
INTEGER,INTENT(out),OPTIONAL :: levused !< number of input levels used for vertint

INTEGER :: i

IF (PRESENT(output_level_auto)) output_level_auto => this%output_level_auto
IF (PRESENT(point_mask)) THEN
  IF (ASSOCIATED(this%point_index)) THEN
    point_mask = c_e(RESHAPE(this%point_index, (/SIZE(this%point_index)/)))
  ENDIF
ENDIF
IF (PRESENT(point_index)) THEN
  IF (ASSOCIATED(this%point_index)) THEN
    point_index = RESHAPE(this%point_index, (/SIZE(this%point_index)/))
  ENDIF
ENDIF
IF (PRESENT(output_point_index)) THEN
  IF (ASSOCIATED(this%point_index)) THEN
! metamorphosis, index is computed from input origin of output point
    output_point_index = PACK(this%point_index(:,:), c_e(this%point_index))
  ELSE IF (this%trans%trans_type == 'polyinter' .OR. &
   this%trans%trans_type == 'maskinter') THEN
! other cases, index is order of output point
    output_point_index = (/(i,i=1,this%outnx)/)
  ENDIF
ENDIF
IF (PRESENT(levshift)) levshift = this%levshift
IF (PRESENT(levused)) levused = this%levused

END SUBROUTINE grid_transform_get_val


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
RECURSIVE SUBROUTINE grid_transform_compute(this, field_in, field_out, var, &
 coord_3d_in)
TYPE(grid_transform),INTENT(in),TARGET :: this !< grid_transformation object
REAL,INTENT(in) :: field_in(:,:,:) !< input array
REAL,INTENT(out) :: field_out(:,:,:) !< output array
TYPE(vol7d_var),INTENT(in),OPTIONAL :: var !< physical variable to be interpolated, if provided, some ad-hoc algorithms may be used where possible
REAL,INTENT(in),OPTIONAL,TARGET :: coord_3d_in(:,:,:) !< input vertical coordinate for vertical interpolation, if not provided by other means

INTEGER :: i, j, k, ii, jj, ie, je, n, navg, kk, kkcache, kkup, kkdown, &
 kfound, kfoundin, inused, i1, i2, j1, j2, np, ns
INTEGER,ALLOCATABLE :: nval(:,:)
REAL :: z1,z2,z3,z4,z(4)
DOUBLE PRECISION  :: x1,x3,y1,y3,xp,yp
INTEGER :: innx, inny, innz, outnx, outny, outnz, vartype
REAL,ALLOCATABLE :: coord_in(:)
LOGICAL,ALLOCATABLE :: mask_in(:)
REAL,ALLOCATABLE :: val_in(:), field_tmp(:,:,:)
REAL,POINTER :: coord_3d_in_act(:,:,:)
TYPE(grid_transform) :: likethis
LOGICAL :: alloc_coord_3d_in_act, nm1


#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,"start grid_transform_compute")
#endif

field_out(:,:,:) = rmiss

IF (.NOT.this%valid) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "refusing to perform a non valid transformation")
  RETURN
ENDIF

IF (this%recur) THEN ! if recursive transformation, recur here and exit
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   "recursive transformation in grid_transform_compute")
#endif

  IF (this%trans%trans_type == 'polyinter') THEN
    likethis = this
    likethis%recur = .FALSE.
    likethis%outnx = this%trans%poly%arraysize
    likethis%outny = 1
    ALLOCATE(field_tmp(this%trans%poly%arraysize,1,SIZE(field_out,3)))
    CALL grid_transform_compute(likethis, field_in, field_tmp, var)

    DO k = 1, SIZE(field_out,3)
      DO j = 1, this%inny
        DO i = 1, this%innx
          IF (c_e(this%inter_index_x(i,j))) THEN
            field_out(i,j,k) = field_tmp(this%inter_index_x(i,j),this%inter_index_y(i,j),k)
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    DEALLOCATE(field_tmp)
  ENDIF

  RETURN
ENDIF

vartype = var_ord
IF (PRESENT(var)) THEN
  vartype = vol7d_vartype(var)
ENDIF

innx = SIZE(field_in,1); inny = SIZE(field_in,2); innz = SIZE(field_in,3)
outnx = SIZE(field_out,1); outny = SIZE(field_out,2); outnz = SIZE(field_out,3)

! check size of field_in, field_out
IF (this%trans%trans_type == 'vertint') THEN ! vertical interpolation

  IF (innz /= this%innz) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"vertical interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent input shape: "//&
     t2c(this%innz)//" /= "//t2c(innz))
    CALL raise_error()
    RETURN
  ENDIF

  IF (outnz /= this%outnz) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"vertical interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent output shape: "//&
     t2c(this%outnz)//" /= "//t2c(outnz))
    CALL raise_error()
    RETURN
  ENDIF

  IF (innx /= outnx .OR. inny /= outny) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"vertical interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent hor. sizes: "//&
     t2c(innx)//","//t2c(inny)//" /= "//&
     t2c(outnx)//","//t2c(outny))
    CALL raise_error()
    RETURN
  ENDIF

ELSE ! horizontal interpolation

  IF (innx /= this%innx .OR. inny /= this%inny) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"horizontal interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent input shape: "//&
     t2c(this%innx)//","//t2c(this%inny)//" /= "//&
     t2c(innx)//","//t2c(inny))
    CALL raise_error()
    RETURN
  ENDIF

  IF (outnx /= this%outnx .OR. outny /= this%outny) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"horizontal interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent output shape: "//&
     t2c(this%outnx)//","//t2c(this%outny)//" /= "//&
     t2c(outnx)//","//t2c(outny))
    CALL raise_error()
    RETURN
  ENDIF

  IF (innz /= outnz) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"horizontal interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent vert. sizes: "//&
     t2c(innz)//" /= "//t2c(outnz))
    CALL raise_error()
    RETURN
  ENDIF

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

  IF (this%trans%sub_type == 'average') THEN
    IF (vartype == var_dir360) THEN
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
              field_out(ii,jj,k) = find_prevailing_direction(field_in(i:ie,j:je,k), &
               0.0, 360.0, 5.0)
            ENDIF
          ENDDO
        ENDDO
      ENDDO

    ELSE
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
              field_out(ii,jj,k) = SUM(field_in(i:ie,j:je,k), &
               MASK=(field_in(i:ie,j:je,k) /= rmiss))/navg
            ENDIF
          ENDDO
        ENDDO
      ENDDO

    ENDIF

  ELSE IF (this%trans%sub_type == 'stddev' .OR. &
   this%trans%sub_type == 'stddevnm1') THEN

    IF (this%trans%sub_type == 'stddev') THEN
      nm1 = .FALSE.
    ELSE
      nm1 = .TRUE.
    ENDIF

    navg = this%trans%box_info%npx*this%trans%box_info%npy
    DO k = 1, innz
      jj = 0
      DO j = 1, this%inny - this%trans%box_info%npy + 1, this%trans%box_info%npy
        je = j+this%trans%box_info%npy-1
        jj = jj+1
        ii = 0
        DO i = 1, this%innx - this%trans%box_info%npx + 1, this%trans%box_info%npx
          ie = i+this%trans%box_info%npx-1
          ii = ii+1
          field_out(ii,jj,k) = stat_stddev( &
           RESHAPE(field_in(i:ie,j:je,k),(/navg/)), nm1=nm1)
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

  ELSE IF (this%trans%sub_type == 'percentile') THEN

    navg = this%trans%box_info%npx*this%trans%box_info%npy
    DO k = 1, innz
      jj = 0
      DO j = 1, this%inny - this%trans%box_info%npy + 1, this%trans%box_info%npy
        je = j+this%trans%box_info%npy-1
        jj = jj+1
        ii = 0
        DO i = 1, this%innx - this%trans%box_info%npx + 1, this%trans%box_info%npx
          ie = i+this%trans%box_info%npx-1
          ii = ii+1
          field_out(ii:ii,jj,k) = stat_percentile( &
           RESHAPE(field_in(i:ie,j:je,k),(/navg/)), &
           (/REAL(this%trans%stat_info%percentile)/))
        ENDDO
      ENDDO
    ENDDO

  ELSE IF (this%trans%sub_type == 'frequency') THEN

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
            field_out(ii,jj,k) = SUM(interval_info_valid( &
             this%trans%interval_info, field_in(i:ie,j:je,k)), &
             MASK=(field_in(i:ie,j:je,k) /= rmiss))/navg
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

          IF (c_e(this%inter_index_x(i,j))) field_out(i,j,k) = &
           field_in(this%inter_index_x(i,j),this%inter_index_y(i,j),k)

        ENDDO
      ENDDO
    ENDDO

  ELSE IF (this%trans%sub_type == 'bilin') THEN

    DO k = 1, innz
      DO j = 1, this%outny 
        DO i = 1, this%outnx 

          IF (c_e(this%inter_index_x(i,j))) THEN

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

            ENDIF
          ENDIF

        ENDDO
      ENDDO
    ENDDO
  ELSE IF (this%trans%sub_type == 'shapiro_near') THEN
    DO k = 1, innz
      DO j = 1, this%outny
        DO i = 1, this%outnx

          IF (c_e(this%inter_index_x(i,j))) THEN

            IF(this%inter_index_x(i,j)-1>0)THEN          
              z(1) = field_in(this%inter_index_x(i,j)-1,this%inter_index_y(i,j)  ,k)
            ELSE
              z(1) = rmiss
            END IF
            IF(this%inter_index_x(i,j)+1<=this%outnx)THEN
              z(3)=field_in(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)  ,k)
            ELSE
              z(3) = rmiss
            END IF
            IF(this%inter_index_y(i,j)+1<=this%outny)THEN          
              z(2)=field_in(this%inter_index_x(i,j)  ,this%inter_index_y(i,j)+1,k)
            ELSE
              z(2) = rmiss
            END IF
            IF(this%inter_index_y(i,j)-1>0)THEN
              z(4)=field_in(this%inter_index_x(i,j)  ,this%inter_index_y(i,j)-1,k)
            ELSE
              z(4) = rmiss
            END IF
            field_out(i,j,k) = shapiro (z,field_in(this%inter_index_x(i,j),this%inter_index_y(i,j),k))

          ENDIF

        ENDDO
      ENDDO
    ENDDO

  ENDIF
ELSE IF (this%trans%trans_type == 'boxinter' &
 .OR. this%trans%trans_type == 'polyinter' &
 .OR. this%trans%trans_type == 'maskinter') THEN

  IF (this%trans%sub_type == 'average') THEN

    IF (vartype == var_dir360) THEN
      DO k = 1, innz
        DO j = 1, this%outny
          DO i = 1, this%outnx
            field_out(i,j,k) = find_prevailing_direction(field_in(:,:,k), &
             0.0, 360.0, 5.0, &
             mask=this%inter_index_x(:,:) == i .AND. this%inter_index_y(:,:) == j)
          ENDDO
        ENDDO
      ENDDO

    ELSE
      ALLOCATE(nval(this%outnx, this%outny))
      field_out(:,:,:) = 0.0
      DO k = 1, innz
        nval(:,:) = 0
        DO j = 1, this%inny
          DO i = 1, this%innx
            IF (c_e(this%inter_index_x(i,j)) .AND. c_e(field_in(i,j,k))) THEN
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
    ENDIF

  ELSE IF (this%trans%sub_type == 'stddev' .OR. &
   this%trans%sub_type == 'stddevnm1') THEN

    IF (this%trans%sub_type == 'stddev') THEN
      nm1 = .FALSE.
    ELSE
      nm1 = .TRUE.
    ENDIF
    DO k = 1, innz
      DO j = 1, this%outny
        DO i = 1, this%outnx
! da paura
          field_out(i:i,j,k) = stat_stddev( &
           RESHAPE(field_in(:,:,k), (/SIZE(field_in(:,:,k))/)), &
           mask=RESHAPE((this%inter_index_x == i .AND. &
           this%inter_index_y == j), (/SIZE(field_in(:,:,k))/)), nm1=nm1)
        ENDDO
      ENDDO
    ENDDO

  ELSE IF (this%trans%sub_type == 'max') THEN

    DO k = 1, innz
      DO j = 1, this%inny
        DO i = 1, this%innx
          IF (c_e(this%inter_index_x(i,j)) .AND. c_e(field_in(i,j,k))) THEN
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

    DO k = 1, innz
      DO j = 1, this%inny
        DO i = 1, this%innx
          IF (c_e(this%inter_index_x(i,j)) .AND. c_e(field_in(i,j,k))) THEN
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

  ELSE IF (this%trans%sub_type == 'frequency') THEN

    ALLOCATE(nval(this%outnx, this%outny))
    field_out(:,:,:) = 0.0
    DO k = 1, innz
      nval(:,:) = 0
      DO j = 1, this%inny
        DO i = 1, this%innx
          IF (c_e(this%inter_index_x(i,j)) .AND. c_e(field_in(i,j,k))) THEN
            field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k) = &
             field_out(this%inter_index_x(i,j),this%inter_index_y(i,j),k) + &
             interval_info_valid(this%trans%interval_info, field_in(i,j,k))
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

  ENDIF

ELSE IF (this%trans%trans_type == 'stencilinter') THEN
  np = SIZE(this%stencil,1)/2
  ns = SIZE(this%stencil)

  IF (this%trans%sub_type == 'average') THEN

    IF (vartype == var_dir360) THEN
      DO k = 1, innz
        DO j = 1, this%outny
          DO i = 1, this%outnx
            IF (c_e(this%inter_index_x(i,j))) THEN
              i1 = this%inter_index_x(i,j) - np
              i2 = this%inter_index_x(i,j) + np
              j1 = this%inter_index_y(i,j) - np
              j2 = this%inter_index_y(i,j) + np
              field_out(i,j,k) = find_prevailing_direction(field_in(i1:i2,j1:j2,k), &
               0.0, 360.0, 5.0, &
               mask=this%stencil(:,:)) ! simpler and equivalent form
!               mask=field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))
            ENDIF
          ENDDO
        ENDDO
      ENDDO

    ELSE
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(i, j, k, i1, i2, j1, j2, n)
      DO k = 1, innz
        DO j = 1, this%outny
          DO i = 1, this%outnx
            IF (c_e(this%inter_index_x(i,j))) THEN
              i1 = this%inter_index_x(i,j) - np
              i2 = this%inter_index_x(i,j) + np
              j1 = this%inter_index_y(i,j) - np
              j2 = this%inter_index_y(i,j) + np
              n = COUNT(field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))
              IF (n > 0) THEN
                field_out(i,j,k) = SUM(field_in(i1:i2,j1:j2,k), &
                 mask=field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))/n
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!$OMP END PARALLEL
    ENDIF

  ELSE IF (this%trans%sub_type == 'stddev' .OR. &
   this%trans%sub_type == 'stddevnm1') THEN

    IF (this%trans%sub_type == 'stddev') THEN
      nm1 = .FALSE.
    ELSE
      nm1 = .TRUE.
    ENDIF

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(i, j, k, i1, i2, j1, j2)
    DO k = 1, innz
      DO j = 1, this%outny
        DO i = 1, this%outnx
          IF (c_e(this%inter_index_x(i,j))) THEN
            i1 = this%inter_index_x(i,j) - np
            i2 = this%inter_index_x(i,j) + np
            j1 = this%inter_index_y(i,j) - np
            j2 = this%inter_index_y(i,j) + np
! da paura
            field_out(i:i,j,k) = stat_stddev( &
             RESHAPE(field_in(i1:i2,j1:j2,k), (/ns/)), &
             mask=RESHAPE(field_in(i1:i2,j1:j2,k) /= rmiss .AND. &
             this%stencil(:,:), (/ns/)), nm1=nm1)
          ENDIF
        ENDDO
      ENDDO
    ENDDO
!$OMP END PARALLEL

  ELSE IF (this%trans%sub_type == 'max') THEN

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(i, j, k, i1, i2, j1, j2, n)
    DO k = 1, innz
      DO j = 1, this%outny
        DO i = 1, this%outnx
          IF (c_e(this%inter_index_x(i,j))) THEN
            i1 = this%inter_index_x(i,j) - np
            i2 = this%inter_index_x(i,j) + np
            j1 = this%inter_index_y(i,j) - np
            j2 = this%inter_index_y(i,j) + np
            n = COUNT(field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))
            IF (n > 0) THEN
              field_out(i,j,k) = MAXVAL(field_in(i1:i2,j1:j2,k), &
               mask=field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
!$OMP END PARALLEL

  ELSE IF (this%trans%sub_type == 'min') THEN

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(i, j, k, i1, i2, j1, j2, n)
    DO k = 1, innz
      DO j = 1, this%outny
        DO i = 1, this%outnx
          IF (c_e(this%inter_index_x(i,j))) THEN
            i1 = this%inter_index_x(i,j) - np
            i2 = this%inter_index_x(i,j) + np
            j1 = this%inter_index_y(i,j) - np
            j2 = this%inter_index_y(i,j) + np
            n = COUNT(field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))
            IF (n > 0) THEN
              field_out(i,j,k) = MINVAL(field_in(i1:i2,j1:j2,k), &
               mask=field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
!$OMP END PARALLEL

  ELSE IF (this%trans%sub_type == 'percentile') THEN

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(i, j, k, i1, i2, j1, j2)
    DO k = 1, innz
      DO j = 1, this%outny
        DO i = 1, this%outnx
          IF (c_e(this%inter_index_x(i,j))) THEN
            i1 = this%inter_index_x(i,j) - np
            i2 = this%inter_index_x(i,j) + np
            j1 = this%inter_index_y(i,j) - np
            j2 = this%inter_index_y(i,j) + np
! da paura
            field_out(i:i,j,k) = stat_percentile( &
             RESHAPE(field_in(i1:i2,j1:j2,k), (/ns/)), &
             (/REAL(this%trans%stat_info%percentile)/), &
             mask=RESHAPE(field_in(i1:i2,j1:j2,k) /= rmiss .AND. &
             this%stencil(:,:), (/ns/)))
          ENDIF
        ENDDO
      ENDDO
    ENDDO
!$OMP END PARALLEL

  ELSE IF (this%trans%sub_type == 'frequency') THEN

!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO PRIVATE(i, j, k, i1, i2, j1, j2, n)
    DO k = 1, innz
      DO j = 1, this%outny
        DO i = 1, this%outnx
          IF (c_e(this%inter_index_x(i,j))) THEN
            i1 = this%inter_index_x(i,j) - np
            i2 = this%inter_index_x(i,j) + np
            j1 = this%inter_index_y(i,j) - np
            j2 = this%inter_index_y(i,j) + np
            n = COUNT(field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))
            IF (n > 0) THEN
              field_out(i,j,k) = SUM(interval_info_valid( &
               this%trans%interval_info, field_in(i1:i2,j1:j2,k)), &
               mask=field_in(i1:i2,j1:j2,k) /= rmiss .AND. this%stencil(:,:))/n
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
!$OMP END PARALLEL
    
  ENDIF

ELSE IF (this%trans%trans_type == 'maskgen') THEN

  DO k = 1, innz
    WHERE(c_e(this%inter_index_x(:,:)))
      field_out(:,:,k) = REAL(this%inter_index_x(:,:))
    END WHERE
  ENDDO

ELSE IF (this%trans%trans_type == 'metamorphosis') THEN

  IF (this%trans%sub_type == 'all') THEN

    field_out(:,:,:) = RESHAPE(field_in(:,:,:), (/this%outnx,this%outny,innz/))

  ELSE IF (this%trans%sub_type == 'coordbb' .OR. this%trans%sub_type == 'poly' &
   .OR. this%trans%sub_type == 'mask') THEN

    DO k = 1, innz
! this is to sparse-points only, so field_out(:,1,k) is acceptable
      field_out(:,1,k) = PACK(field_in(:,:,k), c_e(this%point_index(:,:)))
    ENDDO

  ELSE IF (this%trans%sub_type == 'maskvalid' .OR. &
   this%trans%sub_type == 'maskinvalid') THEN

    DO k = 1, innz
      WHERE (this%point_mask(:,:))
        field_out(:,:,k) = field_in(:,:,k)
      END WHERE
    ENDDO

  ELSE IF (this%trans%sub_type == 'setinvalidto') THEN

    DO k = 1, innz
      WHERE (c_e(field_in(:,:,k)))
        field_out(:,:,k) = field_in(:,:,k)
      ELSE WHERE
        field_out(:,:,k) = this%val1
      END WHERE
    ENDDO

  ELSE IF (this%trans%sub_type == 'settoinvalid') THEN
    IF (c_e(this%val1) .AND. c_e(this%val2)) THEN
      WHERE (c_e(field_in(:,:,:)) .AND. field_in(:,:,:) > this%val1 &
       .AND. field_in(:,:,:) <= this%val2)
        field_out(:,:,:) = rmiss
      ELSE WHERE
        field_out(:,:,:) = field_in(:,:,:)
      END WHERE
    ELSE IF (c_e(this%val1)) THEN
      WHERE (c_e(field_in(:,:,:)) .AND. field_in(:,:,:) > this%val1)
        field_out(:,:,:) = rmiss
      ELSE WHERE
        field_out(:,:,:) = field_in(:,:,:)
      END WHERE
    ELSE IF (c_e(this%val2)) THEN
      WHERE (c_e(field_in(:,:,:)) .AND. field_in(:,:,:) <= this%val2)
        field_out(:,:,:) = rmiss
      ELSE WHERE
        field_out(:,:,:) = field_in(:,:,:)
      END WHERE
    ENDIF
  ENDIF

ELSE IF (this%trans%trans_type == 'vertint') THEN

  IF (this%trans%sub_type == 'linear') THEN

    alloc_coord_3d_in_act = .FALSE.
    IF (ASSOCIATED(this%inter_index_z)) THEN

      DO k = 1, outnz
        IF (c_e(this%inter_index_z(k))) THEN
          z1 = REAL(this%inter_zp(k)) ! weight for k+1
          z2 = REAL(1.0D0 - this%inter_zp(k)) ! weight for k
          SELECT CASE(vartype)

          CASE(var_dir360)
            DO j = 1, inny
              DO i = 1, innx
                IF (c_e(field_in(i,j,this%inter_index_z(k))) .AND. &
                 c_e(field_in(i,j,this%inter_index_z(k)+1))) THEN
                  field_out(i,j,k) = &
                   interp_var_360(field_in(i,j,this%inter_index_z(k)), &
                   field_in(i,j,this%inter_index_z(k)+1), z1, z2)
                ENDIF
              ENDDO
            ENDDO

          CASE(var_press)
            DO j = 1, inny
              DO i = 1, innx
                IF (c_e(field_in(i,j,this%inter_index_z(k))) .AND. &
                 c_e(field_in(i,j,this%inter_index_z(k)+1)) .AND. &
                 field_in(i,j,this%inter_index_z(k)) > 0. .AND. &
                 field_in(i,j,this%inter_index_z(k)+1) > 0.) THEN
                  field_out(i,j,k) = EXP( &
                   LOG(field_in(i,j,this%inter_index_z(k)))*z2 + &
                   LOG(field_in(i,j,this%inter_index_z(k)+1))*z1)
                ENDIF
              ENDDO
            ENDDO

          CASE default
            DO j = 1, inny
              DO i = 1, innx
                IF (c_e(field_in(i,j,this%inter_index_z(k))) .AND. &
                 c_e(field_in(i,j,this%inter_index_z(k)+1))) THEN
                  field_out(i,j,k) = &
                   field_in(i,j,this%inter_index_z(k))*z2 + &
                   field_in(i,j,this%inter_index_z(k)+1)*z1
                ENDIF
              ENDDO
            ENDDO

          END SELECT

        ENDIF
      ENDDO

    ELSE ! use coord_3d_in

      IF (ALLOCATED(this%coord_3d_in)) THEN
        coord_3d_in_act => this%coord_3d_in
#ifdef DEBUG
        CALL l4f_category_log(this%category,L4F_DEBUG, &
         "Using external vertical coordinate for vertint")
#endif
      ELSE
        IF (PRESENT(coord_3d_in)) THEN
#ifdef DEBUG
          CALL l4f_category_log(this%category,L4F_DEBUG, &
           "Using internal vertical coordinate for vertint")
#endif
          IF (this%dolog) THEN
            ALLOCATE(coord_3d_in_act(SIZE(coord_3d_in,1), &
             SIZE(coord_3d_in,2), SIZE(coord_3d_in,3)))
            alloc_coord_3d_in_act = .TRUE.
            WHERE (c_e(coord_3d_in) .AND. coord_3d_in > 0.0)
              coord_3d_in_act = LOG(coord_3d_in)
            ELSEWHERE
              coord_3d_in_act = rmiss
            END WHERE
          ELSE
            coord_3d_in_act => coord_3d_in
          ENDIF
        ELSE
          CALL l4f_category_log(this%category,L4F_ERROR, &
           "Missing internal and external vertical coordinate for vertint")
          CALL raise_error()
          RETURN
        ENDIF
      ENDIF

      inused = SIZE(coord_3d_in_act,3)
      IF (inused < 2) RETURN ! to avoid algorithm failure
      kkcache = 1

      DO k = 1, outnz
        IF (.NOT.c_e(this%vcoord_out(k))) CYCLE
        DO j = 1, inny
          DO i = 1, innx
            kfound = imiss
            DO kk = 1, MAX(inused-kkcache-1, kkcache) ! +-1
              kkup = kkcache + kk
              kkdown = kkcache - kk + 1

              IF (kkdown >= 1) THEN ! search down
                IF (this%vcoord_out(k) >= &
                 MIN(coord_3d_in_act(i,j,kkdown), coord_3d_in_act(i,j,kkdown+1)) .AND. &
                 this%vcoord_out(k) < &
                 MAX(coord_3d_in_act(i,j,kkdown), coord_3d_in_act(i,j,kkdown+1))) THEN
                  kkcache = kkdown
                  kfoundin = kkcache
                  kfound = kkcache + this%levshift
                  EXIT ! kk
                ENDIF
              ENDIF
              IF (kkup < inused) THEN ! search up
                IF (this%vcoord_out(k) >= &
                 MIN(coord_3d_in_act(i,j,kkup), coord_3d_in_act(i,j,kkup+1)) .AND. &
                 this%vcoord_out(k) < &
                 MAX(coord_3d_in_act(i,j,kkup), coord_3d_in_act(i,j,kkup+1))) THEN
                  kkcache = kkup
                  kfoundin = kkcache
                  kfound = kkcache + this%levshift
                  EXIT ! kk
                ENDIF
              ENDIF

            ENDDO

            IF (c_e(kfound)) THEN
              IF (c_e(field_in(i,j,kfound)) .AND. c_e(field_in(i,j,kfound+1))) THEN
                z1 = REAL((this%vcoord_out(k) - coord_3d_in_act(i,j,kfoundin))/ &
                 (coord_3d_in_act(i,j,kfoundin+1) - coord_3d_in_act(i,j,kfoundin)))
                z2 = 1.0 - z1
                SELECT CASE(vartype)

                CASE(var_dir360)
                  field_out(i,j,k) = &
                   interp_var_360(field_in(i,j,kfound), field_in(i,j,kfound+1), z1, z2)
                CASE(var_press)
                  field_out(i,j,k) = EXP(LOG(field_in(i,j,kfound))*z2 + &
                   LOG(field_in(i,j,kfound+1))*z1)

                CASE default
                  field_out(i,j,k) = field_in(i,j,kfound)*z2 + field_in(i,j,kfound+1)*z1
                END SELECT

              ENDIF
            ELSE
            ENDIF
          ENDDO ! i
        ENDDO ! j
      ENDDO ! k
      IF (alloc_coord_3d_in_act) DEALLOCATE(coord_3d_in_act)
    ENDIF

  ELSE IF (this%trans%sub_type == 'linearsparse') THEN


    IF (.NOT.ASSOCIATED(this%vcoord_in) .AND. .NOT.PRESENT(coord_3d_in)) THEN
      CALL l4f_category_log(this%category,L4F_ERROR, &
       "linearsparse interpolation, no input vert coord available")
      RETURN
    ENDIF

    ALLOCATE(coord_in(innz),val_in(innz),mask_in(innz))
    DO j = 1, inny
      DO i = 1, innx

        IF (ASSOCIATED(this%vcoord_in)) THEN
          mask_in = c_e(field_in(i,j,this%levshift+1:this%levshift+this%levused)) &
           .AND. c_e(this%vcoord_in(:))
        ELSE
          mask_in = c_e(field_in(i,j,this%levshift+1:this%levshift+this%levused)) &
           .AND. c_e(coord_3d_in(i,j,:))
        ENDIF

        IF (vartype == var_press) THEN
          mask_in(:) = mask_in(:) .AND. &
           (field_in(i,j,this%levshift+1:this%levshift+this%levused) > 0.0D0)
        ENDIF
        inused = COUNT(mask_in)
        IF (inused > 1) THEN
          IF (ASSOCIATED(this%vcoord_in)) THEN
            coord_in(1:inused) = PACK(this%vcoord_in(:), mask=mask_in)
          ELSE
            coord_in(1:inused) = PACK(coord_3d_in(i,j,:), mask=mask_in)
          ENDIF
          IF (vartype == var_press) THEN
            val_in(1:inused) = LOG(PACK( &
             field_in(i,j,this%levshift+1:this%levshift+this%levused), &
             mask=mask_in))
          ELSE
            val_in(1:inused) = PACK( &
             field_in(i,j,this%levshift+1:this%levshift+this%levused), &
             mask=mask_in)
          ENDIF
          kkcache = 1
          DO k = 1, outnz

            kfound = imiss
            DO kk = 1, MAX(inused-kkcache-1, kkcache) ! +-1
              kkup = kkcache + kk
              kkdown = kkcache - kk + 1

              IF (kkdown >= 1) THEN ! search down
                IF (this%vcoord_out(k) >= &
                 MIN(coord_in(kkdown), coord_in(kkdown+1)) .AND. &
                 this%vcoord_out(k) < &
                 MAX(coord_in(kkdown), coord_in(kkdown+1))) THEN
                  kkcache = kkdown
                  kfoundin = kkcache
                  kfound = kkcache
                  EXIT ! kk
                ENDIF
              ENDIF
              IF (kkup < inused) THEN ! search up
                IF (this%vcoord_out(k) >= &
                 MIN(coord_in(kkup), coord_in(kkup+1)) .AND. &
                 this%vcoord_out(k) < &
                 MAX(coord_in(kkup), coord_in(kkup+1))) THEN
                  kkcache = kkup
                  kfoundin = kkcache
                  kfound = kkcache
                  EXIT ! kk
                ENDIF
              ENDIF

            ENDDO

            IF (c_e(kfound)) THEN
              z1 = REAL((this%vcoord_out(k) - coord_in(kfound-1))/ &
               (coord_in(kfound) - coord_in(kfound-1)))
              z2 = 1.0 - z1
              IF (vartype == var_dir360) THEN
                field_out(i,j,k) = &
                 interp_var_360(val_in(kfound-1), val_in(kfound), z1, z2)
              ELSE IF (vartype == var_press) THEN
                field_out(i,j,k) = EXP(val_in(kfound-1)*z2 + val_in(kfound)*z1)
              ELSE
                field_out(i,j,k) = val_in(kfound-1)*z2 + val_in(kfound)*z1
              ENDIF
            ENDIF

          ENDDO

        ENDIF

      ENDDO
    ENDDO
    DEALLOCATE(coord_in,val_in)


  ENDIF

ELSE IF (this%trans%trans_type == '' .OR. this%trans%trans_type == 'none') THEN

  field_out(:,:,:) = field_in(:,:,:)

ENDIF


CONTAINS


! internal function for interpolating directions from 0 to 360 degree
! hope it is inlined by the compiler
FUNCTION interp_var_360(v1, v2, w1, w2)
REAL,INTENT(in) :: v1, v2, w1, w2
REAL :: interp_var_360

REAL :: lv1, lv2

IF (ABS(v1 - v2) > 180.) THEN
  IF (v1 > v2) THEN
    lv1 = v1 - 360.
    lv2 = v2
  ELSE
    lv1 = v1
    lv2 = v2 - 360.
  ENDIF
  interp_var_360 = MODULO(lv1*w2 + lv2*w1, 360.)
ELSE
  interp_var_360 = v1*w2 + v2*w1
ENDIF

END FUNCTION interp_var_360


RECURSIVE FUNCTION find_prevailing_direction(v1, l, h, res, mask) RESULT(prev)
REAL,INTENT(in) :: v1(:,:)
REAL,INTENT(in) :: l, h, res
LOGICAL,INTENT(in),OPTIONAL :: mask(:,:)
REAL :: prev

REAL :: m
INTEGER :: nh, nl
!REAL,PARAMETER :: res = 1.0

m = (l + h)/2.
IF ((h - l) <= res) THEN
  prev = m
  RETURN
ENDIF

IF (PRESENT(mask)) THEN
  nl = COUNT(v1 >= l .AND. v1 < m .AND. mask)
  nh = COUNT(v1 >= m .AND. v1 < h .AND. mask)
ELSE
  nl = COUNT(v1 >= l .AND. v1 < m)
  nh = COUNT(v1 >= m .AND. v1 < h)
ENDIF
IF (nh > nl) THEN
  prev = find_prevailing_direction(v1, m, h, res)
ELSE IF (nl > nh) THEN
  prev = find_prevailing_direction(v1, l, m, res)
ELSE IF (nl == 0 .AND. nh == 0) THEN
  prev = rmiss
ELSE
  prev = m
ENDIF

END FUNCTION find_prevailing_direction


END SUBROUTINE grid_transform_compute


!> Compute the output data array from input data array according to
!! the defined transformation. The \a grid_transform object \a this
!! must have been properly initialised, so that it contains all the
!! information needed for computing the transformation. This is the
!! sparse points-to-grid and sparse points-to-sparse points version.
SUBROUTINE grid_transform_v7d_grid_compute(this, field_in, field_out, var, &
 coord_3d_in)
TYPE(grid_transform),INTENT(in) :: this !< grid_tranform object
REAL, INTENT(in) :: field_in(:,:) !< input array
REAL, INTENT(out):: field_out(:,:,:) !< output array
TYPE(vol7d_var),INTENT(in),OPTIONAL :: var !< physical variable to be interpolated, if provided, some ad-hoc algorithms may be used where possible
REAL,INTENT(in),OPTIONAL,TARGET :: coord_3d_in(:,:,:) !< input vertical coordinate for vertical interpolation, if not provided by other means

real,allocatable :: field_in_p(:),x_in_p(:),y_in_p(:)
INTEGER :: inn_p, ier, k
INTEGER :: innx, inny, innz, outnx, outny, outnz

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start v7d_grid_transform_compute")
#endif

field_out(:,:,:) = rmiss

IF (.NOT.this%valid) THEN
  call l4f_category_log(this%category,L4F_ERROR, &
   "refusing to perform a non valid transformation")
  call raise_error()
  RETURN
ENDIF

innx = SIZE(field_in,1); inny = 1; innz = SIZE(field_in,2)
outnx = SIZE(field_out,1); outny = SIZE(field_out,2); outnz = SIZE(field_out,3)

! check size of field_in, field_out
IF (this%trans%trans_type == 'vertint') THEN ! vertical interpolation

  IF (innz /= this%innz) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"vertical interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent input shape: "//&
     t2c(this%innz)//" /= "//t2c(innz))
    CALL raise_error()
    RETURN
  ENDIF

  IF (outnz /= this%outnz) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"vertical interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent output shape: "//&
     t2c(this%outnz)//" /= "//t2c(outnz))
    CALL raise_error()
    RETURN
  ENDIF

  IF (innx /= outnx .OR. inny /= outny) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"vertical interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent hor. sizes: "//&
     t2c(innx)//","//t2c(inny)//" /= "//&
     t2c(outnx)//","//t2c(outny))
    CALL raise_error()
    RETURN
  ENDIF

ELSE ! horizontal interpolation

  IF (innx /= this%innx .OR. inny /= this%inny) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"horizontal interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent input shape: "//&
     t2c(this%innx)//","//t2c(this%inny)//" /= "//&
     t2c(innx)//","//t2c(inny))
    CALL raise_error()
    RETURN
  ENDIF

  IF (outnx /= this%outnx .OR. outny /= this%outny) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"horizontal interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent output shape: "//&
     t2c(this%outnx)//","//t2c(this%outny)//" /= "//&
     t2c(outnx)//","//t2c(outny))
    CALL raise_error()
    RETURN
  ENDIF

  IF (innz /= outnz) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,"horizontal interpolation")
    CALL l4f_category_log(this%category,L4F_ERROR,"inconsistent vert. sizes: "//&
     t2c(innz)//" /= "//t2c(outnz))
    CALL raise_error()
    RETURN
  ENDIF

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
       "Number of sparse data points: "//t2c(inn_p)//','//t2c(SIZE(field_in(:,k))))

      IF (inn_p > 2) THEN

        field_in_p(1:inn_p) = PACK(field_in(:,k), c_e(field_in(:,k)))
        x_in_p(1:inn_p) = PACK(this%inter_xp(:,1), c_e(field_in(:,k)))
        y_in_p(1:inn_p) = PACK(this%inter_yp(:,1), c_e(field_in(:,k)))

        IF (.NOT.this%trans%extrap) THEN
          CALL nnseti('ext', 0) ! 0 = no extrapolation
          CALL nnsetr('nul', rmiss)
        ENDIF

        CALL natgrids(inn_p, x_in_p, y_in_p, field_in_p, & ! (1:inn_p) omitted
         this%outnx, this%outny, REAL(this%inter_x(:,1)), & ! no f90 interface
         REAL(this%inter_y(1,:)), field_out(1,1,k), ier)

        IF (ier /= 0) THEN
          CALL l4f_category_log(this%category,L4F_ERROR, &
           "Error code from NCAR natgrids: "//t2c(ier))
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

ELSE IF (this%trans%trans_type == 'boxinter' .OR. &
 this%trans%trans_type == 'polyinter' .OR. &
 this%trans%trans_type == 'vertint' .OR. &
 this%trans%trans_type == 'metamorphosis') THEN ! use the grid-to-grid method

  CALL compute(this, &
   RESHAPE(field_in, (/SIZE(field_in,1), 1, SIZE(field_in,2)/)), field_out, var, &
   coord_3d_in)

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
ELEMENTAL FUNCTION hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp) RESULT(zp)
REAL,INTENT(in) :: z1,z2,z3,z4 ! Z values on the four points
DOUBLE PRECISION,INTENT(in):: x1,y1 ! coordinate of the lower left point
DOUBLE PRECISION,INTENT(in):: x3,y3 ! coordinate of the upper right point
DOUBLE PRECISION,INTENT(in):: xp,yp ! coordinate of point where interpolate
REAL :: zp

REAL :: p1,p2
REAL :: z5,z6


p2 = REAL((yp-y1)/(y3-y1))
p1 = REAL((xp-x1)/(x3-x1))

z5 = (z4-z1)*p2+z1
z6 = (z3-z2)*p2+z2

zp = (z6-z5)*(p1)+z5
      
END FUNCTION hbilin


! Shapiro filter of order 2
FUNCTION shapiro (z,zp) RESULT(zs)
!! z_smoothed(i,j) = z(i,j) * (1-S) + S * sum(z_vicini)/N 
!!                 = z(i,j) - S/N (N*z(i,j) - sum(z_vicini))
REAL,INTENT(in) :: z(:) ! Z values on the four surrounding points
REAL,INTENT(in) :: zp          ! Z values on the central point
REAL   :: zs                   ! Z smoothed value on the central point
INTEGER:: N

IF(c_e(zp))THEN
  N = count(c_e(z))
  zs = zp - 0.5* ( N*zp - sum(z, c_e(z)) )/N
ELSE
  zs = rmiss
END IF

END FUNCTION shapiro


! Locate index of requested point
SUBROUTINE basic_find_index(this, near, nx, ny, xmin, xmax, ymin, ymax, &
 lon, lat, extrap, index_x, index_y)
TYPE(griddim_def),INTENT(in) :: this ! griddim object (from grid)
logical,INTENT(in) :: near ! near or bilin interpolation (determine wich point is requested)
INTEGER,INTENT(in) :: nx,ny ! dimension (to grid)
DOUBLE PRECISION,INTENT(in) :: xmin, xmax, ymin, ymax ! extreme coordinate (to grid)
DOUBLE PRECISION,INTENT(in) :: lon(:,:),lat(:,:) ! target coordinate
LOGICAL,INTENT(in) :: extrap ! extrapolate
INTEGER,INTENT(out) :: index_x(:,:),index_y(:,:) ! index of point requested

INTEGER :: lnx, lny
DOUBLE PRECISION :: x(SIZE(lon,1),SIZE(lon,2)),y(SIZE(lon,1),SIZE(lon,2))

IF (near) THEN
  CALL proj(this,lon,lat,x,y)
  index_x = NINT((x-xmin)/((xmax-xmin)/DBLE(nx-1)))+1
  index_y = NINT((y-ymin)/((ymax-ymin)/DBLE(ny-1)))+1
  lnx = nx
  lny = ny
ELSE
  CALL proj(this,lon,lat,x,y)
  index_x = FLOOR((x-xmin)/((xmax-xmin)/DBLE(nx-1)))+1
  index_y = FLOOR((y-ymin)/((ymax-ymin)/DBLE(ny-1)))+1
  lnx = nx-1
  lny = ny-1
ENDIF

IF (extrap) THEN ! trim indices outside grid for extrapolation
  index_x = MAX(index_x, 1)
  index_y = MAX(index_y, 1)
  index_x = MIN(index_x, lnx)
  index_y = MIN(index_y, lny)
ELSE ! nullify indices outside grid
  WHERE(index_x < 1 .OR. index_x > lnx .OR. index_y < 1 .OR. index_y > lny)
    index_x = imiss
    index_y = imiss
  END WHERE
ENDIF

END SUBROUTINE basic_find_index

END MODULE grid_transform_class
