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
!> Module for describing geographically referenced regular grids.
!! This module defines classes and methods describing rectangular
!! georeferenced grids in different geographical projections. The grid
!! and projection definition can be specified explicitely by the
!! caller or they can be entirely imported from a grib file (through
!! grib_api) or from a file format supported by gdal (through gdal
!! fortran interface).
!!
!! The projection is internally stored following the WMO grib
!! conventions (gridType in grib_api). The projections currently
!! supported or for which support is planned are:
!!    - For grib edition 1 and 2:
!!          - regular_ll (works)
!!          - mercator (to be done)
!!          - lambert (works, to be completed)
!!          - polar_stereographic (to be tested)
!!          - albers (to be done)
!!          - rotated_ll (works)
!!          - stretched_ll (to be completed and tested)
!!          - stretched_rotated_ll (to be completed and tested)
!!    - For grib edition 2 only:
!!          - UTM (ARPA-SIM extension)
!!          - equatorial_azimuthal_equidistant (to be done)
!!    - For gdal-supported formats:
!!          - regular_ll
!!
!!
!! See the example program \include example_vg6d_1.f90
!!
!!\ingroup volgrid6d
MODULE grid_class
use geo_proj_class
use grid_dim_class
use grid_rect_class
use grid_id_class
use err_handling
USE missing_values
USE optional_values
use log4fortran
implicit none


character (len=255),parameter:: subcategory="grid_class"


!> This object, mainly for internal use, describes a grid on
!! a geographical projection, except the grid dimensions.
!! The object is opaque, thus all its members have to be set and
!! accessed through the constructor and the ::get_val and ::set_val
!! methods.
type grid_def
  private
  type(geo_proj) :: proj
  type(grid_rect) :: grid
  integer :: category = 0
end type grid_def


!> This object completely describes a grid on a geographic projection.
!! It is the main public object of this module. The grid definition \a
!! grid is separated from the definition of the grid dimensions \a dim
!! in order to make members of \a grid \a PRIVATE while maintaining
!! free access to the members of \a dim.
type griddim_def
  type(grid_def) :: grid !< grid and projection definition
  type(grid_dim) :: dim  !< grid dimensions definition
  integer :: category = 0 !< category for log4fortran
end type griddim_def


!> Logical equality operators for objects of the classes \a grid_def,
!! and \a griddim_def. They are all defined as \c
!! ELEMENTAL thus work also on arrays of any shape.
INTERFACE OPERATOR (==)
  MODULE PROCEDURE grid_eq, griddim_eq
END INTERFACE

!> Logical inequality operators for objects of the classes \a grid_def,
!! and \a griddim_def. They are all defined as \c
!! ELEMENTAL thus work also on arrays of any shape.
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE grid_ne, griddim_ne
END INTERFACE

!> Constructors of the corresponding objects.
INTERFACE init
  MODULE PROCEDURE griddim_init
END INTERFACE

!> Destructors of the corresponding objects.
INTERFACE delete
  MODULE PROCEDURE griddim_delete
END INTERFACE

!> Copy an object, creating a fully new instance.
INTERFACE copy
  MODULE PROCEDURE griddim_copy
END INTERFACE

!> Compute forward coordinate transformation from geographical system to
!! projected system.
INTERFACE proj
  MODULE PROCEDURE griddim_coord_proj!, griddim_proj
END INTERFACE

!> Compute backward coordinate transformation from projected system
!! to geographical system.
INTERFACE unproj
  MODULE PROCEDURE griddim_coord_unproj, griddim_unproj
END INTERFACE

!> Method for returning the contents of the object.
INTERFACE get_val
  MODULE PROCEDURE griddim_get_val
END INTERFACE

!> Method for setting the contents of the object.
INTERFACE set_val
  MODULE PROCEDURE griddim_set_val
END INTERFACE

!> Write the object on a formatted or unformatted file.
INTERFACE write_unit
  MODULE PROCEDURE griddim_write_unit
END INTERFACE

!> Read the object from a formatted or unformatted file.
INTERFACE read_unit
  MODULE PROCEDURE griddim_read_unit
END INTERFACE

!> Import griddim object from grid_id.
INTERFACE import
  MODULE PROCEDURE griddim_import_grid_id
END INTERFACE

!> Export griddim object to grid_id.
INTERFACE export
  MODULE PROCEDURE griddim_export_grid_id
END INTERFACE

!> Print a brief description on stdout.
INTERFACE display
  MODULE PROCEDURE griddim_display
END INTERFACE

#define VOL7D_POLY_TYPE TYPE(grid_def)
#define VOL7D_POLY_TYPES _grid
#include "array_utilities_pre.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES

#define VOL7D_POLY_TYPE TYPE(griddim_def)
#define VOL7D_POLY_TYPES _griddim
#include "array_utilities_pre.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES

INTERFACE wind_unrot
  MODULE PROCEDURE griddim_wind_unrot
END INTERFACE


PRIVATE

PUBLIC proj, unproj, griddim_unproj, griddim_gen_coord, &
 griddim_zoom_coord, griddim_zoom_projcoord, &
 griddim_setsteps, griddim_def, grid_def, grid_dim
PUBLIC init, delete, copy
PUBLIC get_val,set_val,write_unit,read_unit,display
PUBLIC OPERATOR(==),OPERATOR(/=)
PUBLIC count_distinct, pack_distinct, count_and_pack_distinct, &
 map_distinct, map_inv_distinct,index
PUBLIC wind_unrot, import, export
PUBLIC griddim_central_lon, griddim_set_central_lon
CONTAINS

!> Constructor for a \a griddim_def object.
SUBROUTINE griddim_init(this, nx, ny, &
 xmin, xmax, ymin, ymax, dx, dy, component_flag, &
 proj_type, lov, zone, xoff, yoff, &
 longitude_south_pole, latitude_south_pole, angle_rotation, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor, &
 latin1, latin2, lad, projection_center_flag, &
 ellips_smaj_axis, ellips_flatt, ellips_type, &
 categoryappend)
TYPE(griddim_def),INTENT(inout) :: this !< object to be created
INTEGER,INTENT(in),OPTIONAL :: nx !< number of points along the x axis
INTEGER,INTENT(in),OPTIONAL :: ny !< number of points along the y axis
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xmin !< lower bound for x coordinate on grid in projection units (degrees or meters depending on the projection type)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xmax !< upper bound for x coordinate
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ymin !< lower bound for y coordinate
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ymax !< upper bound for y coordinate
DOUBLE PRECISION,INTENT(in),OPTIONAL :: dx !< grid step in x direction
DOUBLE PRECISION,INTENT(in),OPTIONAL :: dy !< grid step in y direction
!> Resolved u- and v- components of vector quantities relative to 0=the easterly and northerly directions
!! 1=the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,INTENT(in),OPTIONAL :: component_flag
CHARACTER(len=*),INTENT(in),OPTIONAL :: proj_type !< type of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lov !< line of view, also known as reference longitude or orientation of the grid (polar projections)
INTEGER,INTENT(in),OPTIONAL :: zone !< Earth zone (mainly for UTM), sets lov to the correct zone central meridian
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xoff !< offset on x axis (false easting)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: yoff !< offset on y axis (false northing)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_south_pole !< longitude of the southern pole of projection 
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_south_pole !< latitude of the southern pole of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: angle_rotation !< angle of rotation of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_stretch_pole !< longitude of the pole of stretching
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_stretch_pole !< latitude of the pole of stretching
DOUBLE PRECISION,INTENT(in),OPTIONAL :: stretch_factor !< stretching factor
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latin1 !< first standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latin2 !< second standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lad !< latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,INTENT(in),OPTIONAL :: projection_center_flag !< flag indicating which pole is represented
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_smaj_axis !< Earth semi-major axis
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_flatt !< Earth flattening
INTEGER,INTENT(in),OPTIONAL :: ellips_type !< number in the interval [1,nellips] indicating a predefined ellipsoid, alternative to the previous arguments
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

CHARACTER(len=512) :: a_name

IF (PRESENT(categoryappend)) THEN
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory)//"."// &
   TRIM(categoryappend))
ELSE
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory))
ENDIF
this%category=l4f_category_get(a_name)

! geographical projection
this%grid%proj = geo_proj_new( &
 proj_type=proj_type, lov=lov, zone=zone, xoff=xoff, yoff=yoff, &
 longitude_south_pole=longitude_south_pole, &
 latitude_south_pole=latitude_south_pole, angle_rotation=angle_rotation, &
 longitude_stretch_pole=longitude_stretch_pole, &
 latitude_stretch_pole=latitude_stretch_pole, stretch_factor=stretch_factor, &
 latin1=latin1, latin2=latin2, lad=lad, projection_center_flag=projection_center_flag, &
 ellips_smaj_axis=ellips_smaj_axis, ellips_flatt=ellips_flatt, ellips_type=ellips_type)
! grid extension
this%grid%grid = grid_rect_new( &
 xmin, xmax, ymin, ymax, dx, dy, component_flag)
! grid size
this%dim = grid_dim_new(nx, ny)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"init gtype: "//this%grid%proj%proj_type )
#endif

END SUBROUTINE griddim_init


!> Destroy a \a griddim_def object.
SUBROUTINE griddim_delete(this)
TYPE(griddim_def),INTENT(inout) :: this !< object to be destroyed

CALL delete(this%dim)
CALL delete(this%grid%proj)
CALL delete(this%grid%grid)

call l4f_category_delete(this%category)

END SUBROUTINE griddim_delete


!> Create an independent copy of a \a griddim_def object.
SUBROUTINE griddim_copy(this, that, categoryappend)
TYPE(griddim_def),INTENT(in) :: this !< object to be copied
TYPE(griddim_def),INTENT(out) :: that !< copied object
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

CHARACTER(len=512) :: a_name

CALL init(that)

CALL copy(this%grid%proj, that%grid%proj)
CALL copy(this%grid%grid, that%grid%grid)
CALL copy(this%dim, that%dim)

! new category
IF (PRESENT(categoryappend)) THEN
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory)//"."// &
   TRIM(categoryappend))
ELSE
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory))
ENDIF
that%category=l4f_category_get(a_name)

END SUBROUTINE griddim_copy


!> Computes and returns coordinates in the projected system given the
!! geographical coordinates.
ELEMENTAL SUBROUTINE griddim_coord_proj(this, lon, lat, x, y)
TYPE(griddim_def),INTENT(in) :: this !< definition of projection
!> geographical coordinates
DOUBLE PRECISION,INTENT(in) :: lon, lat
!> projected coordinates
DOUBLE PRECISION,INTENT(out) :: x, y

CALL proj(this%grid%proj, lon, lat, x, y)

END SUBROUTINE griddim_coord_proj


!> Computes and returns geographical coordinates given the coordinates
!! in the projected system.
ELEMENTAL SUBROUTINE griddim_coord_unproj(this, x, y, lon, lat)
TYPE(griddim_def),INTENT(in) :: this !< definition of projection
!> projected coordinates
DOUBLE PRECISION,INTENT(in) :: x, y
!> geographical coordinates
DOUBLE PRECISION,INTENT(out) :: lon, lat

CALL unproj(this%grid%proj, x, y, lon, lat)

END SUBROUTINE griddim_coord_unproj


! Computes and sets the grid parameters required to compute
! coordinates of grid points in the projected system.
! probably meaningless
!SUBROUTINE griddim_proj(this)
!TYPE(griddim_def),INTENT(inout) :: this !< definition of projection and grid
!
!CALL proj(this, this%dim%lon(1,1), this%dim%lat(1,1), &
! this%grid%grid%xmin, this%grid%grid%ymin)
!
!CALL proj(this, this%dim%lon(this%dim%nx,this%dim%ny), &
! this%dim%lat(this%dim%nx,this%dim%ny), &
! this%grid%grid%xmax, this%grid%grid%ymax)
!
!END SUBROUTINE griddim_proj

!> Computes the geographical coordinates of all the grid points in the
!! \a griddim_def object and stores them in the object itself.  The \a
!! this::dim::lon and \a this::dim:lat arrays are allocated, and upon
!! return they will contain the coordinates' values. These arrays can
!! be directly accessed by the user, and their association status
!! should be checked with the \a ASSOCIATED() intrinsic before using
!! them.
SUBROUTINE griddim_unproj(this)
TYPE(griddim_def),INTENT(inout) :: this !< definition of projection and grid

IF (.NOT.c_e(this%dim%nx) .OR. .NOT.c_e(this%dim%ny)) RETURN
CALL alloc(this%dim)
CALL griddim_unproj_internal(this)

END SUBROUTINE griddim_unproj

! internal subroutine needed for allocating automatic arrays
SUBROUTINE griddim_unproj_internal(this)
TYPE(griddim_def),INTENT(inout) ::this ! definition of projection and grid

DOUBLE PRECISION :: x(this%dim%nx,this%dim%ny), y(this%dim%nx,this%dim%ny)

CALL grid_rect_coordinates(this%grid%grid, x, y)
CALL unproj(this, x, y, this%dim%lon, this%dim%lat)

END SUBROUTINE griddim_unproj_internal


!> Query the object content.
SUBROUTINE griddim_get_val(this, nx, ny, &
 xmin, xmax, ymin, ymax, dx, dy, component_flag, &
 proj, proj_type, lov, zone, xoff, yoff, &
 longitude_south_pole, latitude_south_pole, angle_rotation, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor, &
 latin1, latin2, lad, projection_center_flag, &
 ellips_smaj_axis, ellips_flatt, ellips_type)
TYPE(griddim_def),INTENT(in) :: this !< object to be queried
INTEGER,INTENT(out),OPTIONAL :: nx !< number of points along the x axis
INTEGER,INTENT(out),OPTIONAL :: ny !< number of points along the y axis
!> longitudini e latitudini minime e massime
DOUBLE PRECISION,INTENT(out),OPTIONAL :: xmin, xmax, ymin, ymax !< grid extremes in projection units (degrees or meters depending on the projection type)
!> grid steps in x and y directions
DOUBLE PRECISION,INTENT(out),OPTIONAL :: dx, dy
!> Resolved u- and v- components of vector quantities relative to 0=the easterly and northerly directions
!! 1=the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,INTENT(out),OPTIONAL :: component_flag
TYPE(geo_proj),INTENT(out),OPTIONAL :: proj !< the complete projection object associated
CHARACTER(len=*),INTENT(out),OPTIONAL :: proj_type !< type of projection
DOUBLE PRECISION,INTENT(out),OPTIONAL :: lov !< line of view, also known as reference longitude or orientation of the grid (polar projections)
INTEGER,INTENT(out),OPTIONAL :: zone !< Earth zone (mainly for UTM), sets lov to the correct zone central meridian
DOUBLE PRECISION,INTENT(out),OPTIONAL :: xoff !< offset on x axis (false easting)
DOUBLE PRECISION,INTENT(out),OPTIONAL :: yoff !< offset on y axis (false northing)
DOUBLE PRECISION,INTENT(out),OPTIONAL :: longitude_south_pole !< longitude of the southern pole of projection 
DOUBLE PRECISION,INTENT(out),OPTIONAL :: latitude_south_pole !< latitude of the southern pole of projection
DOUBLE PRECISION,INTENT(out),OPTIONAL :: angle_rotation !< angle of rotation of projection
DOUBLE PRECISION,INTENT(out),OPTIONAL :: longitude_stretch_pole !< longitude of the pole of stretching
DOUBLE PRECISION,INTENT(out),OPTIONAL :: latitude_stretch_pole !< latitude of the pole of stretching
DOUBLE PRECISION,INTENT(out),OPTIONAL :: stretch_factor !< stretching factor
DOUBLE PRECISION,INTENT(out),OPTIONAL :: latin1 !< first standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(out),OPTIONAL :: latin2 !< second standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(out),OPTIONAL :: lad !< latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,INTENT(out),OPTIONAL :: projection_center_flag !< flag indicating which pole is represented
DOUBLE PRECISION,INTENT(out),OPTIONAL :: ellips_smaj_axis !< Earth semi-major axis
DOUBLE PRECISION,INTENT(out),OPTIONAL :: ellips_flatt !< Earth flattening
INTEGER,INTENT(out),OPTIONAL :: ellips_type !< number in the interval [1,nellips] indicating a predefined ellipsoid, alternative to the previous arguments

IF (PRESENT(nx)) nx = this%dim%nx
IF (PRESENT(ny)) ny = this%dim%ny

IF (PRESENT(proj)) proj = this%grid%proj

CALL get_val(this%grid%proj, proj_type=proj_type, lov=lov, zone=zone, &
 xoff=xoff, yoff=yoff, &
 longitude_south_pole=longitude_south_pole, &
 latitude_south_pole=latitude_south_pole, angle_rotation=angle_rotation, &
 longitude_stretch_pole=longitude_stretch_pole, &
 latitude_stretch_pole=latitude_stretch_pole, stretch_factor=stretch_factor, &
 latin1=latin1, latin2=latin2, lad=lad, &
 projection_center_flag=projection_center_flag, &
 ellips_smaj_axis=ellips_smaj_axis, ellips_flatt=ellips_flatt, &
 ellips_type=ellips_type)

CALL get_val(this%grid%grid, &
 xmin, xmax, ymin, ymax, dx, dy, component_flag)

END SUBROUTINE griddim_get_val


!> Set the object content.
SUBROUTINE griddim_set_val(this, nx, ny, &
 xmin, xmax, ymin, ymax, dx, dy, component_flag, &
 proj_type, lov, zone, xoff, yoff, &
 longitude_south_pole, latitude_south_pole, angle_rotation, &
 longitude_stretch_pole, latitude_stretch_pole, stretch_factor, &
 latin1, latin2, lad, projection_center_flag, &
 ellips_smaj_axis, ellips_flatt, ellips_type)
TYPE(griddim_def),INTENT(inout) :: this !< object to be queried
INTEGER,INTENT(in),OPTIONAL :: nx !< number of points along the x axis
INTEGER,INTENT(in),OPTIONAL :: ny !< number of points along the y axis
!> longitudini e latitudini minime e massime
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xmin, xmax, ymin, ymax !< grid extremes in projection units (degrees or meters depending on the projection type)
!> grid steps in x and y directions
DOUBLE PRECISION,INTENT(in),OPTIONAL :: dx, dy
!> Resolved u- and v- components of vector quantities relative to 0=the easterly and northerly directions
!! 1=the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,INTENT(in),OPTIONAL :: component_flag
CHARACTER(len=*),INTENT(in),OPTIONAL :: proj_type !< type of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lov !< line of view, also known as reference longitude or orientation of the grid (polar projections)
INTEGER,INTENT(in),OPTIONAL :: zone !< Earth zone (mainly for UTM), sets lov to the correct zone central meridian
DOUBLE PRECISION,INTENT(in),OPTIONAL :: xoff !< offset on x axis (false easting)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: yoff !< offset on y axis (false northing)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_south_pole !< longitude of the southern pole of projection 
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_south_pole !< latitude of the southern pole of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: angle_rotation !< angle of rotation of projection
DOUBLE PRECISION,INTENT(in),OPTIONAL :: longitude_stretch_pole !< longitude of the pole of stretching
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latitude_stretch_pole !< latitude of the pole of stretching
DOUBLE PRECISION,INTENT(in),OPTIONAL :: stretch_factor !< stretching factor
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latin1 !< first standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: latin2 !< second standard latitude from main pole (Lambert)
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lad !< latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,INTENT(in),OPTIONAL :: projection_center_flag !< flag indicating which pole is represented
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_smaj_axis !< Earth semi-major axis
DOUBLE PRECISION,INTENT(in),OPTIONAL :: ellips_flatt !< Earth flattening
INTEGER,INTENT(in),OPTIONAL :: ellips_type !< number in the interval [1,nellips] indicating a predefined ellipsoid, alternative to the previous arguments

IF (PRESENT(nx)) this%dim%nx = nx
IF (PRESENT(ny)) this%dim%ny = ny

CALL set_val(this%grid%proj, proj_type=proj_type, lov=lov, zone=zone, &
 xoff=xoff, yoff=yoff, longitude_south_pole=longitude_south_pole, &
 latitude_south_pole=latitude_south_pole, angle_rotation=angle_rotation, &
 longitude_stretch_pole=longitude_stretch_pole, &
 latitude_stretch_pole=latitude_stretch_pole, stretch_factor=stretch_factor, &
 latin1=latin1, latin2=latin2, lad=lad, &
 projection_center_flag=projection_center_flag, &
 ellips_smaj_axis=ellips_smaj_axis, ellips_flatt=ellips_flatt, &
 ellips_type=ellips_type)

CALL set_val(this%grid%grid, &
 xmin, xmax, ymin, ymax, dx, dy, component_flag)

END SUBROUTINE griddim_set_val


!> This method reads from a Fortran file unit the contents of the
!! object \a this.  The record to be read must have been written with
!! the ::write_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE griddim_read_unit(this, unit) 
TYPE(griddim_def),INTENT(out) :: this !< object to be read
INTEGER, INTENT(in) :: unit !< unit from which to read, it must be an opened Fortran file unit


CALL read_unit(this%dim, unit)
CALL read_unit(this%grid%proj, unit)
CALL read_unit(this%grid%grid, unit)

END SUBROUTINE griddim_read_unit


!> This method writes on a Fortran file unit the contents of the
!! object \a this.  The record can successively be read by the
!! ::read_unit method.  The method works both on formatted and
!! unformatted files.
SUBROUTINE griddim_write_unit(this, unit)
TYPE(griddim_def),INTENT(in) :: this !< object to be written
INTEGER, INTENT(in) :: unit !< unit where to write, it must be an opened Fortran file unit


CALL write_unit(this%dim, unit)
CALL write_unit(this%grid%proj, unit)
CALL write_unit(this%grid%grid, unit)

END SUBROUTINE griddim_write_unit


!> Euristically determine the approximate central longitude of the
!! grid in degrees.
!! The method depends on the projection used.
FUNCTION griddim_central_lon(this) RESULT(lon)
TYPE(griddim_def),INTENT(inout) :: this !< grid descriptor

DOUBLE PRECISION :: lon

CALL griddim_pistola_central_lon(this, lon)

END FUNCTION griddim_central_lon


!> Euristically reset the approximate central longitude of the
!! grid to a value compatible to the provided longitude \a lonref.
!! The method depends on the projection used.
SUBROUTINE griddim_set_central_lon(this, lonref)
TYPE(griddim_def),INTENT(inout) :: this !< grid descriptor
DOUBLE PRECISION,INTENT(in) :: lonref !< reference longitude

DOUBLE PRECISION :: lon

CALL griddim_pistola_central_lon(this, lon, lonref)

END SUBROUTINE griddim_set_central_lon


! internal subroutine for performing tasks common to the prevous two
SUBROUTINE griddim_pistola_central_lon(this, lon, lonref)
TYPE(griddim_def),INTENT(inout) :: this ! grid descriptor
DOUBLE PRECISION,INTENT(inout) :: lon ! central longitude
DOUBLE PRECISION,INTENT(in),OPTIONAL :: lonref ! reference longitude

INTEGER :: unit
DOUBLE PRECISION :: lonsp, latsp, londelta, lov
CHARACTER(len=80) :: ptype

lon = dmiss
CALL get_val(this%grid%proj, unit=unit)
IF (unit == geo_proj_unit_meter) THEN ! it is a plane projection
  CALL get_val(this%grid%proj, lov=lon)
  IF (PRESENT(lonref)) THEN
    CALL long_reset_to_cart_closest(lov, lonref)
    CALL set_val(this%grid%proj, lov=lon)
  ENDIF

ELSE IF (unit == geo_proj_unit_degree) THEN ! it is a spheric projection
  CALL get_val(this%grid%proj, proj_type=ptype, &
   longitude_south_pole=lonsp, latitude_south_pole=latsp)
  SELECT CASE(ptype)
  CASE('rotated_ll','stretched_rotated_ll') ! use origin of rotated system
    IF (latsp < 0.0D0) THEN
      lon = lonsp
      IF (PRESENT(lonref)) THEN
        CALL long_reset_to_cart_closest(lov, lonref)
        CALL set_val(this%grid%proj, longitude_south_pole=lonref)
      ENDIF
    ELSE
      lon = MODULO(lonsp + 180.0D0, 360.0D0)
!      IF (PRESENT(lonref)) THEN
!        CALL long_reset_to_cart_closest(lov, lonref)
!        CALL set_val(this%grid%proj, longitude_south_pole=lonref)
!      ENDIF
    ENDIF
  CASE default ! use real grid limits
    IF (c_e(this%grid%grid%xmin) .AND. c_e(this%grid%grid%xmin)) THEN
      lon = 0.5D0*(this%grid%grid%xmin + this%grid%grid%xmax)
    ENDIF
      IF (PRESENT(lonref)) THEN
        londelta = lon
        CALL long_reset_to_cart_closest(londelta, lonref)
        londelta = londelta - lon
        this%grid%grid%xmin = this%grid%grid%xmin + londelta
        this%grid%grid%xmax = this%grid%grid%xmax + londelta
      ENDIF
  END SELECT
ENDIF

END SUBROUTINE griddim_pistola_central_lon


!> Generates coordinates of every point of a generic grid from the
!! grid description. The number of grid points along both direction is
!! guessed from the shape of x and y arrays, which must be conformal.
SUBROUTINE griddim_gen_coord(this, x, y)
TYPE(griddim_def),INTENT(in) :: this !< generic grid descriptor
DOUBLE PRECISION,INTENT(out) :: x(:,:) !< x coordinate of every point, linearly computed between grid extremes
DOUBLE PRECISION,INTENT(out) :: y(:,:) !< y coordinate of every point, linearly computed between grid extremes, it should have the same shape as x(:,:)


CALL grid_rect_coordinates(this%grid%grid, x, y)

END SUBROUTINE griddim_gen_coord


!> Compute and return grid steps.
SUBROUTINE griddim_steps(this, nx, ny, dx, dy)
TYPE(griddim_def), INTENT(in) :: this !< generic grid descriptor
INTEGER,INTENT(in) :: nx !< number of points along x direction
INTEGER,INTENT(in) :: ny !< number of points along y direction
DOUBLE PRECISION,INTENT(out) :: dx !< grid step along x direction
DOUBLE PRECISION,INTENT(out) :: dy !< grid step along y direction

CALL grid_rect_steps(this%grid%grid, nx, ny, dx, dy)

END SUBROUTINE griddim_steps


!> Compute and set grid steps.
SUBROUTINE griddim_setsteps(this)
TYPE(griddim_def), INTENT(inout) :: this !< generic grid descriptor

CALL grid_rect_setsteps(this%grid%grid, this%dim%nx, this%dim%ny)

END SUBROUTINE griddim_setsteps


! TODO
! bisogna sviluppare gli altri operatori
ELEMENTAL FUNCTION grid_eq(this, that) RESULT(res)
TYPE(grid_def),INTENT(IN) :: this, that
LOGICAL :: res

res = this%proj == that%proj .AND. &
 this%grid == that%grid

END FUNCTION grid_eq


ELEMENTAL FUNCTION griddim_eq(this, that) RESULT(res)
TYPE(griddim_def),INTENT(IN) :: this, that
LOGICAL :: res

res = this%grid == that%grid .AND. &
 this%dim == that%dim

END FUNCTION griddim_eq


ELEMENTAL FUNCTION grid_ne(this, that) RESULT(res)
TYPE(grid_def),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION grid_ne


ELEMENTAL FUNCTION griddim_ne(this, that) RESULT(res)
TYPE(griddim_def),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION griddim_ne


!> Import a griddim object from a grid_id object associated to a
!! supported gridded dataset driver (typically a grib message from
!! grib_api or a raster band from gdal).  The griddim object is
!! populated with all the grid information (size, projection, etc.)
!! carried by the grid_id object provided.
SUBROUTINE griddim_import_grid_id(this, ingrid_id)
#ifdef HAVE_LIBGDAL
USE gdal
#endif
TYPE(griddim_def),INTENT(inout) :: this !< griddim object
TYPE(grid_id),INTENT(in) :: ingrid_id !< grid_id object with information about the grid

#ifdef HAVE_LIBGRIBAPI
INTEGER :: gaid
#endif
#ifdef HAVE_LIBGDAL
TYPE(gdalrasterbandh) :: gdalid
#endif
CALL init(this)

#ifdef HAVE_LIBGRIBAPI
gaid = grid_id_get_gaid(ingrid_id)
IF (c_e(gaid)) CALL griddim_import_gribapi(this, gaid)
#endif
#ifdef HAVE_LIBGDAL
gdalid = grid_id_get_gdalid(ingrid_id)
IF (gdalassociated(gdalid)) CALL griddim_import_gdal(this, gdalid, &
 grid_id_get_gdal_options(ingrid_id))
#endif

END SUBROUTINE griddim_import_grid_id


!> Export a griddim object to a grid_id object associated to a
!! supported gridded dataset driver (typically a grib message from
!! grib_api). All the grid information (size, projection, etc.)
!! contained in the griddim object is exported to the grid_id object.
SUBROUTINE griddim_export_grid_id(this, outgrid_id)
#ifdef HAVE_LIBGDAL
USE gdal
#endif
TYPE(griddim_def),INTENT(in) :: this !< griddim object
TYPE(grid_id),INTENT(inout) :: outgrid_id !< grid_id object which will contain information about the grid

#ifdef HAVE_LIBGRIBAPI
INTEGER :: gaid
#endif
#ifdef HAVE_LIBGDAL
TYPE(gdalrasterbandh) :: gdalid
#endif

#ifdef HAVE_LIBGRIBAPI
gaid = grid_id_get_gaid(outgrid_id)
IF (c_e(gaid)) CALL griddim_export_gribapi(this, gaid)
#endif
#ifdef HAVE_LIBGDAL
gdalid = grid_id_get_gdalid(outgrid_id)
!IF (gdalassociated(gdalid)
! export for gdal not implemented, log?
#endif

END SUBROUTINE griddim_export_grid_id


#ifdef HAVE_LIBGRIBAPI
! grib_api driver
SUBROUTINE griddim_import_gribapi(this, gaid)
USE grib_api
TYPE(griddim_def),INTENT(inout) :: this ! griddim object
INTEGER, INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to import

DOUBLE PRECISION :: loFirst, loLast, laFirst, laLast, x1, y1
INTEGER :: EditionNumber, iScansNegatively, jScansPositively, zone, datum, &
 reflon, ierr

! Generic keys
CALL grib_get(gaid, 'typeOfGrid', this%grid%proj%proj_type)
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG, &
 "griddim_import_gribapi, grid type "//TRIM(this%grid%proj%proj_type))
#endif
CALL grib_get(gaid,'GRIBEditionNumber',EditionNumber)

! Keys valid for (almost?) all cases, Ni and Nj are universal aliases
CALL grib_get(gaid, 'Ni', this%dim%nx)
CALL grib_get(gaid, 'Nj', this%dim%ny)
CALL griddim_import_ellipsoid(this, gaid) ! placed here, not valid for utm datum /= 1

CALL grib_get(gaid,'iScansNegatively',iScansNegatively)
CALL grib_get(gaid,'jScansPositively',jScansPositively)
CALL grib_get(gaid,'uvRelativeToGrid',this%grid%grid%component_flag)

! Keys for rotated grids (checked through missing values)
CALL grib_get_dmiss(gaid,'longitudeOfSouthernPoleInDegrees', &
 this%grid%proj%rotated%longitude_south_pole)
CALL grib_get_dmiss(gaid,'latitudeOfSouthernPoleInDegrees', &
 this%grid%proj%rotated%latitude_south_pole)
CALL grib_get_dmiss(gaid,'angleOfRotationInDegrees', &
 this%grid%proj%rotated%angle_rotation)

! Keys for stretched grids (checked through missing values)
! units must be verified, still experimental in grib_api
! # TODO: Is it a float? Is it signed?
IF (EditionNumber == 1) THEN
  CALL grib_get_dmiss(gaid,'longitudeOfStretchingPoleInDegrees', &
   this%grid%proj%stretched%longitude_stretch_pole)
  CALL grib_get_dmiss(gaid,'latitudeOfStretchingPoleInDegrees', &
   this%grid%proj%stretched%latitude_stretch_pole)
  CALL grib_get_dmiss(gaid,'stretchingFactor', &
   this%grid%proj%stretched%stretch_factor)
ELSE IF (EditionNumber == 2) THEN
  CALL grib_get_dmiss(gaid,'longitudeOfThePoleOfStretching', &
   this%grid%proj%stretched%longitude_stretch_pole)
  CALL grib_get_dmiss(gaid,'latitudeOfThePoleOfStretching', &
   this%grid%proj%stretched%latitude_stretch_pole)
  CALL grib_get_dmiss(gaid,'stretchingFactor', &
   this%grid%proj%stretched%stretch_factor)
  IF (c_e(this%grid%proj%stretched%stretch_factor)) &
   this%grid%proj%stretched%stretch_factor = this%grid%proj%stretched%stretch_factor*1.0D-6
ENDIF

! Projection-dependent keys
SELECT CASE (this%grid%proj%proj_type)

! Keys for spherical coordinate systems
CASE ('regular_ll', 'rotated_ll', 'stretched_ll', 'stretched_rotated_ll')

  CALL grib_get(gaid,'longitudeOfFirstGridPointInDegrees',loFirst)
  CALL grib_get(gaid,'longitudeOfLastGridPointInDegrees',loLast)
  CALL grib_get(gaid,'latitudeOfFirstGridPointInDegrees',laFirst)
  CALL grib_get(gaid,'latitudeOfLastGridPointInDegrees',laLast)

! longitudes are sometimes wrongly coded even in grib2 and even by the
! Metoffice!
! longitudeOfFirstGridPointInDegrees = 354.911;
! longitudeOfLastGridPointInDegrees = 363.311;
  CALL long_reset_0_360(lofirst)
  CALL long_reset_0_360(lolast)

  IF (iScansNegatively  == 0) THEN
    this%grid%grid%xmin = loFirst
    this%grid%grid%xmax = loLast
  ELSE
    this%grid%grid%xmax = loFirst
    this%grid%grid%xmin = loLast
  ENDIF
  IF (jScansPositively == 0) THEN
    this%grid%grid%ymax = laFirst
    this%grid%grid%ymin = laLast
  ELSE
    this%grid%grid%ymin = laFirst
    this%grid%grid%ymax = laLast
  ENDIF

! reset longitudes in order to have a Cartesian plane
  IF (this%grid%grid%xmax < this%grid%grid%xmin) &
   this%grid%grid%xmin = this%grid%grid%xmin - 360.D0

! compute dx and dy (should we get them from grib?)
  CALL grid_rect_setsteps(this%grid%grid, this%dim%nx, this%dim%ny)

! Keys for polar projections
CASE ('polar_stereographic', 'lambert', 'albers')

  CALL grib_get(gaid,'DxInMetres', this%grid%grid%dx)
  CALL grib_get(gaid,'DyInMetres', this%grid%grid%dy)
! latin1/latin2 may be missing (e.g. stereographic)
  CALL grib_get_dmiss(gaid,'Latin1InDegrees',this%grid%proj%polar%latin1)
  CALL grib_get_dmiss(gaid,'Latin2InDegrees',this%grid%proj%polar%latin2)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   "griddim_import_gribapi, latin1/2 "// &
   TRIM(to_char(this%grid%proj%polar%latin1))//" "// &
   TRIM(to_char(this%grid%proj%polar%latin2)))
#endif
! projection center flag, aka hemisphere 
  CALL grib_get(gaid,'projectionCenterFlag',&
   this%grid%proj%polar%projection_center_flag, ierr)
  IF (ierr /= GRIB_SUCCESS) THEN ! try center/centre
    CALL grib_get(gaid,'projectionCentreFlag',&
     this%grid%proj%polar%projection_center_flag)
  ENDIF

  IF (IAND(this%grid%proj%polar%projection_center_flag,64) == 1) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     "griddim_import_gribapi, bi-polar projections not supported")
    CALL raise_error()
  ENDIF
! line of view, aka central meridian
  CALL grib_get(gaid,'LoVInDegrees',this%grid%proj%lov)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   "griddim_import_gribapi, central meridian "//TRIM(to_char(this%grid%proj%lov)))
#endif

! latitude at which dx and dy are valid
  IF (EditionNumber == 1) THEN
! ECMWF (gribex/grib_api) says: Grid lengths are in metres, at the
! 60-degree parallel nearest to the pole on the projection plane.
!  IF (IAND(this%projection_center_flag, 128) == 0) THEN
!    this%grid%proj%polar%lad = 60.D0 
!  ELSE
!    this%grid%proj%polar%lad = -60.D0 
!  ENDIF
! WMO says: Grid lengths are in units of metres, at the secant cone
! intersection parallel nearest to the pole on the projection plane.
    this%grid%proj%polar%lad = this%grid%proj%polar%latin1
  ELSE IF (EditionNumber == 2) THEN
    CALL grib_get(gaid,'LaDInDegrees',this%grid%proj%polar%lad)
  ENDIF
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   "griddim_import_gribapi, lad "//TRIM(to_char(this%grid%proj%polar%lad)))
#endif

! compute projected extremes from lon and lat of first point
  CALL grib_get(gaid,'longitudeOfFirstGridPointInDegrees',loFirst)
  CALL grib_get(gaid,'latitudeOfFirstGridPointInDegrees',laFirst)
  CALL long_reset_0_360(lofirst)
  CALL long_reset_to_cart_closest(this%grid%proj%lov, lofirst)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   "griddim_import_gribapi, longitude of first point "//TRIM(to_char(lofirst)))
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   "griddim_import_gribapi, central meridian reset "//TRIM(to_char(this%grid%proj%lov)))
#endif


  CALL proj(this, loFirst, laFirst, x1, y1)
  IF (iScansNegatively  == 0) THEN
    this%grid%grid%xmin = x1
    this%grid%grid%xmax = x1 + this%grid%grid%dx*DBLE(this%dim%nx - 1)
  ELSE
    this%grid%grid%xmax = x1
    this%grid%grid%xmin = x1 - this%grid%grid%dx*DBLE(this%dim%nx - 1)
  ENDIF
  IF (jScansPositively == 0) THEN
    this%grid%grid%ymax = y1
    this%grid%grid%ymin = y1 - this%grid%grid%dy*DBLE(this%dim%ny - 1)
  ELSE
    this%grid%grid%ymin = y1
    this%grid%grid%ymax = y1 + this%grid%grid%dy*DBLE(this%dim%ny - 1)
  ENDIF
! keep these values for personal pleasure
  this%grid%proj%polar%lon1 = loFirst
  this%grid%proj%polar%lat1 = laFirst

CASE ('UTM')

  CALL grib_get(gaid,'zone',zone)

  CALL grib_get(gaid,'datum',datum)
  IF (datum == 0) THEN
    CALL grib_get(gaid,'referenceLongitude',reflon)
    CALL grib_get(gaid,'falseEasting',this%grid%proj%xoff)
    CALL grib_get(gaid,'falseNorthing',this%grid%proj%yoff)
    CALL set_val(this%grid%proj, zone=zone, lov=refLon/1.0D6)
  ELSE
    CALL l4f_category_log(this%category,L4F_ERROR,'only datum 0 supported')
    CALL raise_fatal_error()
  ENDIF

  CALL grib_get(gaid,'eastingOfFirstGridPoint',loFirst)
  CALL grib_get(gaid,'eastingOfLastGridPoint',loLast)
  CALL grib_get(gaid,'northingOfFirstGridPoint',laFirst)
  CALL grib_get(gaid,'northingOfLastGridPoint',laLast)

  IF (iScansNegatively  == 0) THEN
    this%grid%grid%xmin = loFirst
    this%grid%grid%xmax = loLast
  ELSE
    this%grid%grid%xmax = loFirst
    this%grid%grid%xmin = loLast
  ENDIF
  IF (jScansPositively == 0) THEN
    this%grid%grid%ymax = laFirst
    this%grid%grid%ymin = laLast
  ELSE
    this%grid%grid%ymin = laFirst
    this%grid%grid%ymax = laLast
  ENDIF

! compute dx and dy (should we get them from grib?)
  CALL grid_rect_setsteps(this%grid%grid, this%dim%nx, this%dim%ny)

CASE default
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "griddim_import_gribapi, grid type "//TRIM(this%grid%proj%proj_type)//" not supported")
  CALL raise_error()

END SELECT

CONTAINS
! utilities routines for grib_api, is there a better place?
SUBROUTINE grib_get_dmiss(gaid, key, value)
INTEGER,INTENT(in) :: gaid
CHARACTER(len=*),INTENT(in) :: key
DOUBLE PRECISION,INTENT(out) :: value

INTEGER :: ierr

CALL grib_get(gaid, key, value, ierr)
IF (ierr /= GRIB_SUCCESS) value = dmiss

END SUBROUTINE grib_get_dmiss

SUBROUTINE grib_get_imiss(gaid, key, value)
INTEGER,INTENT(in) :: gaid
CHARACTER(len=*),INTENT(in) :: key
INTEGER,INTENT(out) :: value

INTEGER :: ierr

CALL grib_get(gaid, key, value, ierr)
IF (ierr /= GRIB_SUCCESS) value = imiss

END SUBROUTINE grib_get_imiss


SUBROUTINE griddim_import_ellipsoid(this, gaid)
TYPE(griddim_def),INTENT(inout) :: this
INTEGER,INTENT(in) :: gaid

INTEGER :: shapeofearth, iv, is
DOUBLE PRECISION :: r1, r2

IF (EditionNumber == 2) THEN
  CALL grib_get(gaid, 'shapeOfTheEarth', shapeofearth)
  SELECT CASE(shapeofearth)
  CASE(0) ! spherical
    CALL set_val(this, ellips_smaj_axis=6367470.0D0, ellips_flatt=0.0D0)
  CASE(1) ! spherical generic
    CALL grib_get(gaid, 'scaleFactorOfRadiusOfSphericalEarth', is)
    CALL grib_get(gaid, 'scaledValueOfRadiusOfSphericalEarth', iv)
    r1 = DBLE(iv) / 10**is
    CALL set_val(this, ellips_smaj_axis=r1, ellips_flatt=0.0D0)
  CASE(2) ! iau65
    CALL set_val(this, ellips_smaj_axis=6378160.0D0, ellips_flatt=1.0D0/297.0D0)
  CASE(3,7) ! ellipsoidal generic
    CALL grib_get(gaid, 'scaleFactorOfEarthMajorAxis', is)
    CALL grib_get(gaid, 'scaledValueOfEarthMajorAxis', iv)
    r1 = DBLE(iv) / 10**is
    CALL grib_get(gaid, 'scaleFactorOfEarthMinorAxis', is)
    CALL grib_get(gaid, 'scaledValueOfEarthMinorAxis', iv)
    r2 = DBLE(iv) / 10**is
    IF (shapeofearth == 3) THEN ! km->m
      r1 = r1*1000.0D0
      r2 = r2*1000.0D0
    ENDIF
    IF (ABS(r1) < 1.0D-6) THEN ! suspicious data read from grib
      CALL l4f_category_log(this%category,L4F_WARN,'zero Earth major axis '// &
      'read from grib, going on with spherical Earth but the results may be wrong')
      CALL set_val(this, ellips_smaj_axis=6367470.0D0, ellips_flatt=0.0D0)
    ELSE
      CALL set_val(this, ellips_smaj_axis=r1, ellips_flatt=(r1-r2)/r1)
    ENDIF
  CASE(4) ! iag-grs80
    CALL set_val(this, ellips_type=ellips_grs80)
  CASE(5) ! wgs84
    CALL set_val(this, ellips_type=ellips_wgs84)
  CASE(6) ! spherical
    CALL set_val(this, ellips_smaj_axis=6371229.0D0, ellips_flatt=0.0D0)
!  CASE(7) ! google earth-like?
  CASE default
    CALL l4f_category_log(this%category,L4F_ERROR,'shapeOfTheEarth '// &
     t2c(shapeofearth)//' not supported in grib2')
    CALL raise_fatal_error()

  END SELECT

ELSE

  CALL grib_get(gaid, 'earthIsOblate', shapeofearth)
  IF (shapeofearth == 0) THEN ! spherical
    CALL set_val(this, ellips_smaj_axis=6367470.0D0, ellips_flatt=0.0D0)
  ELSE ! iau65
    CALL set_val(this, ellips_smaj_axis=6378160.0D0, ellips_flatt=1.0D0/297.0D0)
  ENDIF

ENDIF

END SUBROUTINE griddim_import_ellipsoid


END SUBROUTINE griddim_import_gribapi


! grib_api driver
SUBROUTINE griddim_export_gribapi(this, gaid) 
USE grib_api
TYPE(griddim_def),INTENT(in) :: this ! griddim object
INTEGER, INTENT(inout) :: gaid ! grib_api id of the grib loaded in memory to export

INTEGER :: EditionNumber, iScansNegatively, jScansPositively, nv, pvl, zone, ierr
DOUBLE PRECISION :: loFirst, loLast, laFirst, laLast, reflon
DOUBLE PRECISION :: sdx, sdy, ratio, tol


! Generic keys
CALL grib_get(gaid,'GRIBEditionNumber',EditionNumber)
CALL grib_set(gaid,'typeOfGrid' ,this%grid%proj%proj_type)
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG, &
 "griddim_export_gribapi, grid type "//this%grid%proj%proj_type)
#endif

! Edition dependent setup
IF (EditionNumber == 1) THEN
  ratio = 1.d3
ELSE IF (EditionNumber == 2) THEN
  ratio = 1.d6
ELSE
  ratio = 0.0d0 ! signal error?!
ENDIF

! Keys valid for (almost?) all cases, Ni and Nj are universal aliases
CALL griddim_export_ellipsoid(this, gaid)
CALL grib_set(gaid, 'Ni', this%dim%nx)
CALL grib_set(gaid, 'Nj', this%dim%ny)
CALL grib_set(gaid,'uvRelativeToGrid',this%grid%grid%component_flag)

CALL grib_get(gaid,'iScansNegatively',iScansNegatively)
CALL grib_get(gaid,'jScansPositively',jScansPositively)

! Keys for rotated grids (checked through missing values and/or error code)
!SELECT CASE (this%grid%proj%proj_type)
!CASE ('rotated_ll', 'stretched_rotated_ll', 'polar_stereographic', 'lambert', 'albers')
CALL grib_set_dmiss(gaid,'longitudeOfSouthernPoleInDegrees', &
 this%grid%proj%rotated%longitude_south_pole, 0.0D0)
CALL grib_set_dmiss(gaid,'latitudeOfSouthernPoleInDegrees', &
 this%grid%proj%rotated%latitude_south_pole, -90.0D0)
IF (EditionNumber == 1) THEN
  CALL grib_set_dmiss(gaid,'angleOfRotationInDegrees', &
   this%grid%proj%rotated%angle_rotation, 0.0D0)
ELSE IF (EditionNumber == 2)THEN
  CALL grib_set_dmiss(gaid,'angleOfRotationOfProjectionInDegrees', &
   this%grid%proj%rotated%angle_rotation, 0.0D0)
ENDIF

! Keys for stretched grids (checked through missing values and/or error code)
! units must be verified, still experimental in grib_api
! # TODO: Is it a float? Is it signed?
IF (EditionNumber == 1) THEN
  CALL grib_set_dmiss(gaid,'longitudeOfStretchingPoleInDegrees', &
   this%grid%proj%stretched%longitude_stretch_pole, 0.0D0)
  CALL grib_set_dmiss(gaid,'latitudeOfStretchingPoleInDegrees', &
   this%grid%proj%stretched%latitude_stretch_pole, -90.0D0)
  CALL grib_set_dmiss(gaid,'stretchingFactor', &
   this%grid%proj%stretched%stretch_factor, 1.0D0)
ELSE IF (EditionNumber == 2) THEN
  CALL grib_set_dmiss(gaid,'longitudeOfThePoleOfStretching', &
   this%grid%proj%stretched%longitude_stretch_pole, 0.0D0)
  CALL grib_set_dmiss(gaid,'latitudeOfThePoleOfStretching', &
   this%grid%proj%stretched%latitude_stretch_pole, -90.0D0)
  CALL grib_set_dmiss(gaid,'stretchingFactor', &
   this%grid%proj%stretched%stretch_factor, 1.0D6, 1.0D6)
ENDIF

! Projection-dependent keys
SELECT CASE (this%grid%proj%proj_type)

! Keys for sphaerical coordinate systems
CASE ('regular_ll', 'rotated_ll', 'stretched_ll', 'stretched_rotated_ll')

  IF (iScansNegatively  == 0) THEN
    loFirst = this%grid%grid%xmin
    loLast = this%grid%grid%xmax
  ELSE
    loFirst = this%grid%grid%xmax
    loLast = this%grid%grid%xmin
  ENDIF
  IF (jScansPositively == 0) THEN
    laFirst = this%grid%grid%ymax
    laLast = this%grid%grid%ymin
  ELSE
    laFirst = this%grid%grid%ymin
    laLast = this%grid%grid%ymax
  ENDIF

! reset lon in standard grib 2 definition [0,360]
  IF (EditionNumber == 1) THEN
    CALL long_reset_m180_360(loFirst)
    CALL long_reset_m180_360(loLast)
  ELSE IF (EditionNumber == 2) THEN
    CALL long_reset_0_360(loFirst)
    CALL long_reset_0_360(loLast)
  ENDIF

  CALL grib_set(gaid,'longitudeOfFirstGridPointInDegrees',loFirst)
  CALL grib_set(gaid,'longitudeOfLastGridPointInDegrees',loLast)
  CALL grib_set(gaid,'latitudeOfFirstGridPointInDegrees',laFirst)
  CALL grib_set(gaid,'latitudeOfLastGridPointInDegrees',laLast)

! test relative coordinate truncation error with respect to tol
! tol should be tuned
  sdx = this%grid%grid%dx*ratio
  sdy = this%grid%grid%dy*ratio
  tol = 1.0d0/ratio

  IF (ABS(NINT(sdx)/sdx - 1.0d0) > tol .OR. ABS(NINT(sdy)/sdy - 1.0d0) > tol) THEN
    CALL l4f_category_log(this%category,L4F_INFO, &
     "griddim_export_gribapi, increments not given: inaccurate!")
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG, &
     "griddim_export_gribapi, dlon relative error: "//&
     TRIM(to_char(ABS(NINT(sdx)/sdx - 1.0d0)))//">"//TRIM(to_char(tol)))
    CALL l4f_category_log(this%category,L4F_DEBUG, &
     "griddim_export_gribapi, dlat relative error: "//&
     TRIM(to_char(ABS(NINT(sdy)/sdy - 1.0d0)))//">"//TRIM(to_char(tol)))
#endif
    CALL grib_set_missing(gaid,'Di')
    CALL grib_set_missing(gaid,'Dj')
    CALL grib_set(gaid,'ijDirectionIncrementGiven',0)
  ELSE
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG, &
     "griddim_export_gribapi, setting increments: "// &
     TRIM(to_char(this%grid%grid%dx))//' '//TRIM(to_char(this%grid%grid%dy)))
#endif
    CALL grib_set(gaid,'ijDirectionIncrementGiven',1)
    CALL grib_set(gaid,'iDirectionIncrement',NINT(this%grid%grid%dx*ratio))
    CALL grib_set(gaid,'jDirectionIncrement',NINT(this%grid%grid%dy*ratio))
! this does not work in grib_set
!    CALL grib_set(gaid,'iDirectionIncrementInDegrees',this%grid%grid%dx)
!    CALL grib_set(gaid,'jDirectionIncrementInDegrees',this%grid%grid%dy)
  ENDIF

! Keys for polar projections
CASE ('polar_stereographic', 'lambert', 'albers')
! increments are required
  CALL grib_set(gaid,'DxInMetres', this%grid%grid%dx)
  CALL grib_set(gaid,'DyInMetres', this%grid%grid%dy)
  CALL grib_set(gaid,'ijDirectionIncrementGiven',1)
! latin1/latin2 may be missing (e.g. stereographic)
  CALL grib_set_dmiss(gaid,'Latin1InDegrees',this%grid%proj%polar%latin1)
  CALL grib_set_dmiss(gaid,'Latin2InDegrees',this%grid%proj%polar%latin2)
! projection center flag, aka hemisphere 
  CALL grib_set(gaid,'projectionCenterFlag',&
   this%grid%proj%polar%projection_center_flag, ierr)
  IF (ierr /= GRIB_SUCCESS) THEN ! try center/centre
    CALL grib_set(gaid,'projectionCentreFlag',&
     this%grid%proj%polar%projection_center_flag)
  ENDIF


! line of view, aka central meridian
  CALL grib_set(gaid,'LoVInDegrees',this%grid%proj%lov)
! latitude at which dx and dy are valid
  IF (EditionNumber == 2) THEN
    CALL grib_set(gaid,'LaDInDegrees',this%grid%proj%polar%lad)
  ENDIF

! compute lon and lat of first point from projected extremes
  IF (iScansNegatively  == 0) THEN
    IF (jScansPositively == 0) THEN
      CALL unproj(this, this%grid%grid%xmin, this%grid%grid%ymax, loFirst, laFirst)
    ELSE
      CALL unproj(this, this%grid%grid%xmin, this%grid%grid%ymin, loFirst, laFirst)
    ENDIF
  ELSE
    IF (jScansPositively == 0) THEN
      CALL unproj(this, this%grid%grid%xmax, this%grid%grid%ymax, loFirst, laFirst)
    ELSE
      CALL unproj(this, this%grid%grid%xmax, this%grid%grid%ymin, loFirst, laFirst)
    ENDIF
  ENDIF
! use the values kept for personal pleasure ?
!  loFirst = this%grid%proj%polar%lon1
!  laFirst = this%grid%proj%polar%lat1
! reset lon in standard grib 2 definition [0,360]
  IF (EditionNumber == 1) THEN
    CALL long_reset_m180_360(loFirst)
  ELSE IF (EditionNumber == 2) THEN
    CALL long_reset_0_360(loFirst)
  ENDIF
  CALL grib_set(gaid,'longitudeOfFirstGridPointInDegrees',loFirst)
  CALL grib_set(gaid,'latitudeOfFirstGridPointInDegrees',laFirst)

CASE ('UTM')

  CALL grib_set(gaid,'datum',0)
  CALL get_val(this, zone=zone, lov=reflon)
  CALL grib_set(gaid,'referenceLongitude',NINT(refLon/1.0D6))
  CALL grib_set(gaid,'falseEasting',this%grid%proj%xoff)
  CALL grib_set(gaid,'falseNorthing',this%grid%proj%yoff)

  CALL grib_set(gaid,'iDirectionIncrement',this%grid%grid%dx)
  CALL grib_set(gaid,'jDirectionIncrement',this%grid%grid%dy)

!res/scann ??

  CALL grib_set(gaid,'zone',zone)

  IF (iScansNegatively  == 0) THEN
    loFirst = this%grid%grid%xmin
    loLast = this%grid%grid%xmax
  ELSE
    loFirst = this%grid%grid%xmax
    loLast = this%grid%grid%xmin
  ENDIF
  IF (jScansPositively == 0) THEN
    laFirst = this%grid%grid%ymax
    laLast = this%grid%grid%ymin
  ELSE
    laFirst = this%grid%grid%ymin
    laLast = this%grid%grid%ymax
  ENDIF

  CALL grib_set(gaid,'eastingOfFirstGridPoint',loFirst)
  CALL grib_set(gaid,'eastingOfLastGridPoint',loLast)
  CALL grib_set(gaid,'northingOfFirstGridPoint',laFirst)
  CALL grib_set(gaid,'northingOfLastGridPoint',laLast)

CASE default
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "griddim_export_gribapi, grid type "//TRIM(this%grid%proj%proj_type)//" not supported")
  CALL raise_error()

END SELECT

! hack for position of vertical coordinate parameters
! buggy in grib_api
IF (EditionNumber == 1) THEN
!  CALL grib_get(gaid,"PVPresent",pvp) ! alias, probably useless
  CALL grib_get(gaid,"NV",nv)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,"griddim_export_gribapi, coding "// &
   TRIM(to_char(nv))//" vertical coordinate parameters")
#endif

  IF (nv == 0) THEN
    pvl = 255
  ELSE
    SELECT CASE (this%grid%proj%proj_type)
    CASE ('regular_ll') ! check whether "29-32 Set to zero (reserved)" are required
      pvl = 33
    CASE ('polar_stereographic')
      pvl = 33
    CASE ('rotated_ll', 'stretched_ll', 'lambert', 'albers')
      pvl = 43
    CASE ('stretched_rotated_ll')
      pvl = 43
    CASE DEFAULT
      pvl = 43 !?
    END SELECT
  ENDIF

  CALL grib_set(gaid,"pvlLocation",pvl)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,"griddim_export_gribapi, coding "// &
   TRIM(to_char(pvl))//" as vertical coordinate parameter location")
#endif

ENDIF


CONTAINS
! utilities routines for grib_api, is there a better place?
SUBROUTINE grib_set_dmiss(gaid, key, val, default, factor)
INTEGER,INTENT(in) :: gaid
CHARACTER(len=*),INTENT(in) :: key
DOUBLE PRECISION,INTENT(in) :: val
DOUBLE PRECISION,INTENT(in),OPTIONAL :: default
DOUBLE PRECISION,INTENT(in),OPTIONAL :: factor

INTEGER :: ierr

IF (c_e(val)) THEN
  IF (PRESENT(factor)) THEN
    CALL grib_set(gaid, key, val*factor, ierr)
  ELSE
    CALL grib_set(gaid, key, val, ierr)
  ENDIF
ELSE IF (PRESENT(default)) THEN
  CALL grib_set(gaid, key, default, ierr)
ENDIF

END SUBROUTINE grib_set_dmiss

SUBROUTINE grib_set_imiss(gaid, key, value, default)
INTEGER,INTENT(in) :: gaid
CHARACTER(len=*),INTENT(in) :: key
INTEGER,INTENT(in) :: value
INTEGER,INTENT(in),OPTIONAL :: default

INTEGER :: ierr

IF (c_e(value)) THEN
  CALL grib_set(gaid, key, value, ierr)
ELSE IF (PRESENT(default)) THEN
  CALL grib_set(gaid, key, default, ierr)
ENDIF

END SUBROUTINE grib_set_imiss

SUBROUTINE griddim_export_ellipsoid(this, gaid)
TYPE(griddim_def),INTENT(in) :: this
INTEGER,INTENT(in) :: gaid

INTEGER :: ellips_type
DOUBLE PRECISION :: r1, r2, f

CALL get_val(this, ellips_smaj_axis=r1, ellips_flatt=f, ellips_type=ellips_type)

IF (EditionNumber == 2) THEN

  CALL grib_set_missing(gaid, 'scaleFactorOfRadiusOfSphericalEarth')
  CALL grib_set_missing(gaid, 'scaledValueOfRadiusOfSphericalEarth')
  CALL grib_set_missing(gaid, 'scaleFactorOfEarthMajorAxis')
  CALL grib_set_missing(gaid, 'scaledValueOfEarthMajorAxis')
  CALL grib_set_missing(gaid, 'scaleFactorOfEarthMinorAxis')
  CALL grib_set_missing(gaid, 'scaledValueOfEarthMinorAxis')

  SELECT CASE(ellips_type)
  CASE(ellips_grs80) ! iag-grs80
    CALL grib_set(gaid, 'shapeOfTheEarth', 4)
  CASE(ellips_wgs84) ! wgs84
    CALL grib_set(gaid, 'shapeOfTheEarth', 5)
  CASE default
    IF (f == 0.0D0) THEN ! spherical Earth
      IF (r1 == 6367470.0D0) THEN ! spherical
        CALL grib_set(gaid, 'shapeOfTheEarth', 0)
      ELSE IF (r1 == 6371229.0D0) THEN ! spherical
        CALL grib_set(gaid, 'shapeOfTheEarth', 6)
      ELSE ! spherical generic
        CALL grib_set(gaid, 'shapeOfTheEarth', 1)
        CALL grib_set(gaid, 'scaleFactorOfRadiusOfSphericalEarth', 2)
        CALL grib_set(gaid, 'scaledValueOfRadiusOfSphericalEarth', INT(r1*100.0D0))
      ENDIF
    ELSE ! ellipsoidal
      IF (r1 == 6378160.0D0 .AND. f == 1.0D0/297.0D0) THEN ! iau65
        CALL grib_set(gaid, 'shapeOfTheEarth', 2)
      ELSE ! ellipsoidal generic
        CALL grib_set(gaid, 'shapeOfTheEarth', 3)
        r2 = r1*(1.0D0 - f)
        CALL grib_set(gaid, 'scaleFactorOfEarthMajorAxis', 5)
        CALL grib_set(gaid, 'scaledValueOfEarthMajorAxis', &
         INT(r1*100.0D0))
        CALL grib_set(gaid, 'scaleFactorOfEarthMinorAxis', 5)
        CALL grib_set(gaid, 'scaledValueOfEarthMinorAxis', &
         INT(r2*100.0D0))
      ENDIF
    ENDIF
  END SELECT

ELSE

  IF (r1 == 6367470.0D0 .AND. f == 0.0D0) THEN ! spherical
    CALL grib_set(gaid, 'earthIsOblate', 0)
  ELSE IF (r1 == 6378160.0D0 .AND. f == 1.0D0/297.0D0) THEN ! iau65
    CALL grib_set(gaid, 'earthIsOblate', 1)
  ELSE IF (f == 0.0D0) THEN ! generic spherical
    CALL grib_set(gaid, 'earthIsOblate', 0)
    CALL l4f_category_log(this%category,L4F_WARN,'desired spherical Earth radius &
     &not supported in grib 1, coding with default radius of 6367470 m')
  ELSE ! generic ellipsoidal
    CALL grib_set(gaid, 'earthIsOblate', 1)
    CALL l4f_category_log(this%category,L4F_WARN,'desired Earth ellipsoid &
     &not supported in grib 1, coding with default iau65 ellipsoid')
  ENDIF

ENDIF

END SUBROUTINE griddim_export_ellipsoid

END SUBROUTINE griddim_export_gribapi
#endif


#ifdef HAVE_LIBGDAL
! gdal driver
SUBROUTINE griddim_import_gdal(this, gdalid, gdal_options)
USE gdal
TYPE(griddim_def),INTENT(inout) :: this ! griddim object
TYPE(gdalrasterbandh),INTENT(in) :: gdalid ! gdal rasterband pointer
TYPE(gdal_file_id_options),INTENT(in) :: gdal_options

TYPE(gdaldataseth) :: hds
REAL(kind=c_double) :: geotrans(6) !, invgeotrans(6), x1, y1, x2, y2
INTEGER(kind=c_int) :: offsetx, offsety
INTEGER :: ier

hds = gdalgetbanddataset(gdalid) ! go back to dataset
ier = gdalgetgeotransform(hds, geotrans)

IF (ier /= 0) THEN
  CALL l4f_category_log(this%category, L4F_ERROR, &
   'griddim_import_gdal: error in accessing gdal dataset')
  CALL raise_error()
  RETURN
ENDIF
IF (geotrans(3) /= 0.0_c_double .OR. geotrans(5) /= 0.0_c_double) THEN ! dataset has not a diagonal transformation
  CALL l4f_category_log(this%category, L4F_ERROR, &
   'griddim_import_gdal: dataset has a non-diagonal transformation matrix, unsupported')
  CALL raise_error()
  RETURN
ENDIF

CALL gdaldatasetbbsize_f(hds, gdal_options%xmin, gdal_options%ymin, &
 gdal_options%xmax, gdal_options%ymax, &
 this%dim%nx, this%dim%ny, offsetx, offsety, &
 this%grid%grid%xmin, this%grid%grid%ymin, this%grid%grid%xmax, this%grid%grid%ymax)

IF (this%dim%nx == 0 .OR. this%dim%ny == 0) THEN
  CALL l4f_category_log(this%category, L4F_WARN, &
   'griddim_import_gdal: requested bounding box '//t2c(gdal_options%xmin)//','// &
   t2c(gdal_options%ymin)//','//t2c(gdal_options%xmax)//','//&
   t2c(gdal_options%ymax))
  CALL l4f_category_log(this%category, L4F_WARN, &
   'determines an empty dataset '//t2c(this%dim%nx)//'x'//t2c(this%dim%ny))
ENDIF

! get grid corners
!CALL gdalapplygeotransform(geotrans, 0.5_c_double, 0.5_c_double, x1, y1)
!CALL gdalapplygeotransform(geotrans, gdalgetrasterbandxsize(gdalid)-0.5_c_double, &
! gdalgetrasterbandysize(gdalid)-0.5_c_double, x2, y2)

!IF (geotrans(3) == 0.0_c_double .AND. geotrans(5) == 0.0_c_double) THEN ! transformation is diagonal, no transposing
!  this%dim%nx = gdalgetrasterbandxsize(gdalid)
!  this%dim%ny = gdalgetrasterbandysize(gdalid)
!  this%grid%grid%xmin = MIN(x1, x2)
!  this%grid%grid%xmax = MAX(x1, x2)
!  this%grid%grid%ymin = MIN(y1, y2)
!  this%grid%grid%ymax = MAX(y1, y2)
!ELSE IF (geotrans(2) == 0.0_c_double .AND. geotrans(6) == 0.0_c_double) THEN ! transformation is anti-diagonal, transposing will have to be done
!
!  this%dim%nx = gdalgetrasterbandysize(gdalid)
!  this%dim%ny = gdalgetrasterbandxsize(gdalid)
!  this%grid%grid%xmin = MIN(y1, y2)
!  this%grid%grid%xmax = MAX(y1, y2)
!  this%grid%grid%ymin = MIN(x1, x2)
!  this%grid%grid%ymax = MAX(x1, x2)
!
!ELSE ! transformation is a rotation, not supported
!ENDIF

this%grid%proj%proj_type = 'regular_ll' ! forced, only one supported (check?)

CALL grid_rect_setsteps(this%grid%grid, this%dim%nx, this%dim%ny)
this%grid%grid%component_flag = 0

END SUBROUTINE griddim_import_gdal
#endif


!> Display on the screen a brief content of the object.
SUBROUTINE griddim_display(this) 
TYPE(griddim_def),INTENT(in) :: this !< griddim object to display

PRINT*,"<<<<<<<<<<<<<<< griddim >>>>>>>>>>>>>>>>"

CALL display(this%grid%proj)
CALL display(this%grid%grid)
CALL display(this%dim)

PRINT*,"<<<<<<<<<<<<<<< griddim >>>>>>>>>>>>>>>>"

END SUBROUTINE griddim_display


#define VOL7D_POLY_TYPE TYPE(grid_def)
#define VOL7D_POLY_TYPES _grid
#include "array_utilities_inc.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES

#define VOL7D_POLY_TYPE TYPE(griddim_def)
#define VOL7D_POLY_TYPES _griddim
#include "array_utilities_inc.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES


!> Compute rotation matrix for wind unrotation. It allocates and
!! computes a matrix suitable for recomputing wind components in the
!! geographical system from the components in the projected
!! system. The rotation matrix \a rot_mat has to be passed as a
!! pointer and successively deallocated by the caller; it is a
!! 3-dimensional array where the first two dimensions are lon and lat
!! and the third, with extension 4, contains the packed rotation
!! matrix for the given grid point. It should work for every
!! projection. In order for the method to work, the \a griddim_unproj
!! method must have already been called for the \a griddim_def object.
!! \todo Check the algorithm and add some orthogonality tests.
SUBROUTINE griddim_wind_unrot(this, rot_mat)
USE doubleprecision_phys_const
TYPE(griddim_def), INTENT(in) :: this !< object describing the grid
DOUBLE PRECISION, POINTER :: rot_mat(:,:,:) !< rotation matrix for every grid point, to be deallocated by the caller, if \c .NOT. \c ASSOCIATED() an error occurred

DOUBLE PRECISION :: dlat_i, dlat_j,dlon_i,dlon_j,dist_i,dist_j
DOUBLE PRECISION :: lat_factor
INTEGER :: i, j, ip1, im1, jp1, jm1;

IF (this%dim%nx <= 0 .OR. this%dim%ny <= 0 .OR. &
 .NOT. ASSOCIATED(this%dim%lon) .OR. .NOT. ASSOCIATED(this%dim%lat)) THEN
  CALL l4f_category_log(this%category, L4F_ERROR, 'rotate_uv must be called after correct init of griddim object')
  NULLIFY(rot_mat)
  RETURN
ENDIF

ALLOCATE(rot_mat(this%dim%nx, this%dim%ny, 4))

DO j = 1, this%dim%ny
  jp1 = MIN(j+1, this%dim%ny)
  jm1 = MAX(j-1, 1)
  DO i = 1, this%dim%nx
    ip1 = MIN(i+1, this%dim%nx)
    im1 = MAX(i-1, 1)

    dlat_i = this%dim%lat(ip1,j) - this%dim%lat(im1,j)
    dlat_j = this%dim%lat(i,jp1) - this%dim%lat(i,jm1)

    dlon_i = this%dim%lon(ip1,j) - this%dim%lon(im1,j)
!	if ( dlon_i >   pi  ) dlon_i -= 2*pi;
!	if ( dlon_i < (-pi) ) dlon_i += 2*pi;
    dlon_j = this%dim%lon(i,jp1) - this%dim%lon(i,jm1)
!	if ( dlon_j >   pi  ) dlon_j -= 2*pi;
!	if ( dlon_j < (-pi) ) dlon_j += 2*pi;

! check whether this is really necessary !!!!
    lat_factor = COS(degrad*this%dim%lat(i,j))
    dlon_i = dlon_i * lat_factor
    dlon_j = dlon_j * lat_factor

    dist_i = SQRT(dlon_i*dlon_i + dlat_i*dlat_i)
    dist_j = SQRT(dlon_j*dlon_j + dlat_j*dlat_j)

    IF (dist_i > 0.D0) THEN
      rot_mat(i,j,1) = dlon_i/dist_i
      rot_mat(i,j,2) = dlat_i/dist_i
    ELSE
      rot_mat(i,j,1) = 0.0D0
      rot_mat(i,j,2) = 0.0D0
    ENDIF
    IF (dist_j > 0.D0) THEN
      rot_mat(i,j,3) = dlon_j/dist_j
      rot_mat(i,j,4) = dlat_j/dist_j
    ELSE
      rot_mat(i,j,3) = 0.0D0
      rot_mat(i,j,4) = 0.0D0
    ENDIF

  ENDDO
ENDDO

END SUBROUTINE griddim_wind_unrot


! compute zoom indices from geographical zoom coordinates
SUBROUTINE griddim_zoom_coord(this, ilon, ilat, flon, flat, ix, iy, fx, fy)
TYPE(griddim_def),INTENT(in) :: this
DOUBLE PRECISION,INTENT(in) :: ilon, ilat, flon, flat
INTEGER,INTENT(out) :: ix, iy, fx, fy

DOUBLE PRECISION :: ix1, iy1, fx1, fy1

! compute projected coordinates of vertices of desired lonlat rectangle
CALL proj(this, ilon, ilat, ix1, iy1)
CALL proj(this, flon, flat, fx1, fy1)

CALL griddim_zoom_projcoord(this, ix1, iy1, fx1, fy1, ix, iy, fx, fy)

END SUBROUTINE griddim_zoom_coord


! compute zoom indices from projected zoom coordinates
SUBROUTINE griddim_zoom_projcoord(this, ix1, iy1, fx1, fy1, ix, iy, fx, fy)
TYPE(griddim_def),INTENT(in) :: this
DOUBLE PRECISION,INTENT(in) :: ix1, iy1, fx1, fy1
INTEGER,INTENT(out) :: ix, iy, fx, fy

INTEGER :: lix, liy, lfx, lfy

! compute projected indices
lix = NINT((ix1-this%grid%grid%xmin)/this%grid%grid%dx) + 1
liy = NINT((iy1-this%grid%grid%ymin)/this%grid%grid%dy) + 1
lfx = NINT((fx1-this%grid%grid%xmin)/this%grid%grid%dx) + 1
lfy = NINT((fy1-this%grid%grid%ymin)/this%grid%grid%dy) + 1
! swap projected indices, in case grid is "strongly rotated"
ix = MIN(lix, lfx)
fx = MAX(lix, lfx)
iy = MIN(liy, lfy)
fy = MAX(liy, lfy)

END SUBROUTINE griddim_zoom_projcoord


!> Reset a longitude value in the interval [0-360[.
!! The value is reset in place. This is usually useful in connection
!! with grib2 coding/decoding.
SUBROUTINE long_reset_0_360(lon)
DOUBLE PRECISION,INTENT(inout) :: lon !< the longitude to reset

IF (.NOT.c_e(lon)) RETURN
DO WHILE(lon < 0.0D0)
  lon = lon + 360.0D0
END DO
DO WHILE(lon >= 360.0D0)
  lon = lon - 360.0D0
END DO

END SUBROUTINE long_reset_0_360


!> Reset a longitude value in the interval [-180-360[.
!! The value is reset in place. This is usually useful in connection
!! with grib1 coding/decoding.
SUBROUTINE long_reset_m180_360(lon)
DOUBLE PRECISION,INTENT(inout) :: lon !< the longitude to reset

IF (.NOT.c_e(lon)) RETURN
DO WHILE(lon < -180.0D0)
  lon = lon + 360.0D0
END DO
DO WHILE(lon >= 360.0D0)
  lon = lon - 360.0D0
END DO

END SUBROUTINE long_reset_m180_360


!> Reset a longitude value in the interval [-90-270[.
!! The value is reset in place. This is usually useful in connection
!! with grib2 coding/decoding.
!SUBROUTINE long_reset_m90_270(lon)
!DOUBLE PRECISION,INTENT(inout) :: lon !< the longitude to reset
!
!IF (.NOT.c_e(lon)) RETURN
!DO WHILE(lon < -90.0D0)
!  lon = lon + 360.0D0
!END DO
!DO WHILE(lon >= 270.0D0)
!  lon = lon - 360.0D0
!END DO
!
!END SUBROUTINE long_reset_m90_270


!> Reset a longitude value in the interval [-180-180[.
!! The value is reset in place. This is usually useful in connection
!! with grib2 coding/decoding.
SUBROUTINE long_reset_m180_180(lon)
DOUBLE PRECISION,INTENT(inout) :: lon !< the longitude to reset

IF (.NOT.c_e(lon)) RETURN
DO WHILE(lon < -180.0D0)
  lon = lon + 360.0D0
END DO
DO WHILE(lon >= 180.0D0)
  lon = lon - 360.0D0
END DO

END SUBROUTINE long_reset_m180_180


SUBROUTINE long_reset_to_cart_closest(lon, lonref)
DOUBLE PRECISION,INTENT(inout) :: lon !< the longitude to reset
DOUBLE PRECISION,INTENT(in) :: lonref !< the longitude to compare

IF (.NOT.c_e(lon) .OR. .NOT.c_e(lonref)) RETURN
IF (ABS(lon-lonref) < 180.0D0) RETURN ! nothing to do
lon = lon - NINT((lon-lonref)/360.0D0)*360.0D0 ! se non e` vera e` ben trovata

END SUBROUTINE long_reset_to_cart_closest


END MODULE grid_class

