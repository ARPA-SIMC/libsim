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
!> Module for describing rectanguler geographical grids.
!! This module defines classes and methods describing rectanguler
!! georefererenced grids in different projections.
!!
!! See the example program \include example_vg6d_1.f90
!!
!!\ingroup volgrid6d
module grid_class
use gridpar_generic_class
use gridpar_rotated_class
use gridpar_stretched_class
use gridpar_polarproj_class
use geo_transforms
use log4fortran
use grid_id_class
use err_handling
use grid_dim_class
implicit none


character (len=255),parameter:: subcategory="grid_class"

!> This object, for internal use only, describes a geographical projection
!! following the WMO grib conventions (gridType in grib_api). In the
!! case of import from grib, the type of projection is computed from
!! the grid description section in grib file, the projections
!! currently supported or for which support is planned are:
!!
!!    - For grib edition 1 and 2:
!!          - regular_ll (works)
!!          - mercator (to be done)
!!          - lambert (works, to be completed)
!!          - polar_stereographic (to be tested)
!!          - UTM (to be done, needs standardization)
!!          - albers (to be done)
!!          - rotated_ll (works)
!!          - stretched_ll (to be completed and tested)
!!          - stretched_rotated_ll (to be completed and tested)
!!    - For grib edition 2 only:
!!          - equatorial_azimuthal_equidistant (to be done)
type grid_type
  character(len=80) :: type !< the type of grid
end type grid_type

! For equatorial projections like Mercator?
!TYPE gridpar_equatorialproj

!> This object, mainly for internal use, describes a grid on
!! a geographic projection except the grid dimensions.
type grid_def
  private
  type(grid_type) :: type
  type(gridpar_generic) :: generic
  type(gridpar_rotated) :: rotated
  type(gridpar_stretched) :: stretched
  type(gridpar_polarproj) :: polarproj

  integer :: category !< category for log4fortran
end type grid_def

!> This object completely describes a grid on a geographic projection.
!! It is the main public object of this module. The grid definition \a
!! grid is separated from the definition of the grid dimensions \a dim
!! in order to make members of \a grid \a PRIVATE while maintaining
!! free access to the members of \a dim.
type griddim_def
  type(grid_def) :: grid !< grid and projection definition
  type(grid_dim) :: dim  !< grid dimensions definition
  integer :: category !< log4fortran
end type griddim_def


!> Logical equality operators for objects of the classes \a grid_def,
!! \a griddim_def and \a grid_type. They are all defined as \c
!! ELEMENTAL thus work also on arrays of any shape.
INTERFACE OPERATOR (==)
  MODULE PROCEDURE grid_eq, grid_type_eq, griddim_eq
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
  MODULE PROCEDURE griddim_coord_proj, griddim_proj
END INTERFACE

!> Compute backward coordinate transformation from projected system
!! geographical system.
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

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct_griddim,count_distinct_grid_type,count_distinct_grid
END INTERFACE

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct_griddim,pack_distinct_grid_type,pack_distinct_grid
END INTERFACE

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct_griddim,map_distinct_grid_type,map_distinct_grid
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct_griddim,map_inv_distinct_grid_type,map_inv_distinct_grid
END INTERFACE

INTERFACE index
  MODULE PROCEDURE index_griddim,index_grid_type,index_grid
END INTERFACE

INTERFACE wind_unrot
  MODULE PROCEDURE griddim_wind_unrot
END INTERFACE


PRIVATE

PUBLIC proj, unproj, griddim_proj, griddim_unproj, griddim_gen_coord, &
 griddim_zoom_coord, griddim_setsteps, griddim_def, grid_def, grid_dim
PUBLIC init, delete, copy
public get_val,set_val,write_unit,read_unit,display
public operator(==),count_distinct,pack_distinct,map_distinct,map_inv_distinct,index
public wind_unrot
PUBLIC import,export

CONTAINS

!> Inizializza un oggetto griddim
SUBROUTINE griddim_init(this, type,&
 nx, ny, &
 lon_min, lon_max, lat_min, lat_max, dx, dy, component_flag, &
 latitude_south_pole, longitude_south_pole, angle_rotation, &
 latitude_stretch_pole, longitude_stretch_pole, stretch_factor, &
 latin1, latin2, lov, lad, projection_center_flag, &
 categoryappend)
TYPE(griddim_def) :: this !< oggetto da creare
CHARACTER(len=*),INTENT(in),OPTIONAL :: TYPE !< type of grid definition
INTEGER,OPTIONAL :: nx !< numero dei punti in x
INTEGER,OPTIONAL :: ny !< numero dei punti in y
!> longitudini e latitudini minime e massime
DOUBLE PRECISION,OPTIONAL :: lon_min, lon_max, lat_min, lat_max
doubleprecision,OPTIONAL,INTENT(in) :: dx, dy !< Grid steps in x and y directions
!> Resolution and Component Flags
!! -  bit 1	
!!            -  0	i direction increments not given
!!            -  1	i direction increments given
!! -  bit 2	
!!            -  0	j direction increments not given
!!            -  1	j direction increments given
!! -  bit 3	
!!            -  0 	Resolved u- and v- components of vector quantities relative to easterly and northerly directions
!!            -  1 	Resolved u- and v- components of vector quantities relative to the defined grid in the direction of increasing x and y (or i and j) coordinates respectively (0=north, 128=south)
INTEGER,OPTIONAL :: component_flag
doubleprecision,OPTIONAL :: latitude_south_pole !< Latitude of the southern pole of projection
doubleprecision,OPTIONAL :: longitude_south_pole !< Longitude of the southern pole of projection 
doubleprecision,OPTIONAL :: angle_rotation !< Angle of rotation of projection
doubleprecision,OPTIONAL :: latitude_stretch_pole !< Latitude of the pole of stretching
doubleprecision,OPTIONAL :: longitude_stretch_pole !< Longitude of the pole of stretching
doubleprecision,OPTIONAL :: stretch_factor !< Stretching factor
doubleprecision,OPTIONAL :: latin1 !< First standard latitude from main pole (Lambert)
doubleprecision,OPTIONAL :: latin2 !< Second standard latitude from main pole (Lambert)
doubleprecision,OPTIONAL :: lov !< Line of view, also known as reference longitude or orientation of the grid (polar projections)
doubleprecision,OPTIONAL :: lad !< Latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,OPTIONAL :: projection_center_flag !< Flag indicating which pole is represented

CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

CHARACTER(len=512) :: a_name

CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory)//"."// &
 TRIM(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

this%dim = grid_dim_init(nx, ny)
!this%grid%dim => this%dim

IF (PRESENT(type))THEN
  this%grid%type%type = type
ELSE
  this%grid%type%type = cmiss
ENDIF

this%grid%generic = gridpar_generic_new(lon_min, lon_max, &
 lat_min, lat_max, dx, dy, component_flag)
this%grid%rotated = gridpar_rotated_new(longitude_south_pole, &
 latitude_south_pole, angle_rotation)
this%grid%stretched = gridpar_stretched_new(longitude_stretch_pole, &
 latitude_stretch_pole, stretch_factor)
this%grid%polarproj = gridpar_polarproj_new(latin1, latin2, lov, lad, &
 lon_min, lat_min, projection_center_flag)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"init gtype: "//this%grid%type%type )
#endif

END SUBROUTINE griddim_init


!> Cancellazione oggetto griddim
SUBROUTINE griddim_delete(this)
type(griddim_def) :: this !< oggetto griddim da cancellare

CALL delete(this%dim)
!NULLIFY(this%grid%dim)

CALL delete(this%grid%generic)
CALL delete(this%grid%rotated)
CALL delete(this%grid%stretched)
CALL delete(this%grid%polarproj)

this%grid%type%type=cmiss

!chiudo il logger
call l4f_category_delete(this%category)

END SUBROUTINE griddim_delete


!> Clona un oggetto griddim creandone una nuova istanza
SUBROUTINE griddim_copy(this, that, categoryappend)
type(griddim_def),intent(in) :: this !< oggetto da clonare
type(griddim_def),intent(out) :: that !< oggetto clonato
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

CHARACTER(len=512) :: a_name

CALL init(that)

that%grid%type = this%grid%type

CALL copy(this%grid%generic, that%grid%generic)
CALL copy(this%grid%rotated, that%grid%rotated)
CALL copy(this%grid%stretched, that%grid%stretched)
CALL copy(this%grid%polarproj, that%grid%polarproj)

CALL copy(this%dim, that%dim)

!new category
call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
that%category=l4f_category_get(a_name)

END SUBROUTINE griddim_copy


!> Proietta coordinate geografiche nel sistema definito
ELEMENTAL SUBROUTINE griddim_coord_proj(this, lon, lat, x, y)
TYPE(griddim_def),INTENT(in) :: this !< definizione della proiezione
!> coordinate geografiche da proiettare
doubleprecision, INTENT(in) :: lon, lat
!> coordinate proiettate
doubleprecision, INTENT(out) :: x, y


SELECT CASE(this%grid%type%type)

CASE("regular_ll")
  CALL proj_regular_ll(lon, lat, x, y)

CASE("rotated_ll")
  CALL proj_rotated_ll(lon, lat, x, y, this%grid%rotated%longitude_south_pole, &
   this%grid%rotated%latitude_south_pole, this%grid%rotated%angle_rotation)
  
CASE("lambert")
  CALL proj_lambert(lon, lat, x, y, this%grid%polarproj%latin1, &
   this%grid%polarproj%latin2, this%grid%polarproj%lov, this%grid%polarproj%lad, &
   this%grid%polarproj%projection_center_flag)

CASE("polar_stereographic")
  CALL proj_polar_stereographic(lon, lat, x, y, this%grid%polarproj%lov, &
   this%grid%polarproj%lad, this%grid%polarproj%projection_center_flag)
  
CASE default
  x = dmiss
  y = dmiss

END SELECT

END SUBROUTINE griddim_coord_proj


!>Calcola le coordinate geografiche date le coordinate nel sistema definito
ELEMENTAL SUBROUTINE griddim_coord_unproj(this, x, y, lon, lat)
TYPE(griddim_def),INTENT(in) :: this !< definizione della proiezione
!> coordinate proiettate
DOUBLE PRECISION, INTENT(in) :: x, y
!> coordinate geografiche
DOUBLE PRECISION, INTENT(out) :: lon, lat

! dove metto un risultato intermedio?
! posso fare, essendo elemental,
! double precision :: tmpx(size(lon,1),size(lon(2)), tmpy(size(lon,1),size(lon(2)) ?

SELECT CASE(this%grid%type%type)

CASE("regular_ll")
  CALL unproj_regular_ll(x, y, lon, lat)

CASE("rotated_ll")
  CALL unproj_rotated_ll(x, y, lon, lat, &
   this%grid%rotated%longitude_south_pole, &
   this%grid%rotated%latitude_south_pole, this%grid%rotated%angle_rotation)
  
CASE("lambert")
  CALL unproj_lambert(x, y, lon, lat, &
   this%grid%polarproj%latin1, this%grid%polarproj%latin2, &
   this%grid%polarproj%lov, this%grid%polarproj%lad, &
   this%grid%polarproj%projection_center_flag)

CASE("polar_stereographic")
  CALL unproj_polar_stereographic(x, y, lon, lat, this%grid%polarproj%lov, &
   this%grid%polarproj%lad, this%grid%polarproj%projection_center_flag)

CASE default
  lon = dmiss
  lat = dmiss

END SELECT

END SUBROUTINE griddim_coord_unproj

!> Proietta un oggetto griddim nel sistema definito.
!! Effettua una serie di conti per avere informazioni nello spazio di
!! proiezione; l'oggetto contiene le informazioni di proiezione e dati
!! relativi al grigliato espresse nel sistema proiettato e geografico.
SUBROUTINE griddim_proj(this)
TYPE(griddim_def),INTENT(inout) :: this !< oggetto che definisce la proiezione e con info relative al grigliato associato

CALL proj(this, this%dim%lon(1,1), this%dim%lat(1,1), &
 this%grid%generic%x1, this%grid%generic%y1)

CALL proj(this, this%dim%lon(this%dim%nx,this%dim%ny), &
 this%dim%lat(this%dim%nx,this%dim%ny), &
 this%grid%generic%x2, this%grid%generic%y2)

END SUBROUTINE griddim_proj


!> Calcola informazioni nel sistema geografico di un oggetto griddim.
!! Effettua una serie di conti per avere informazioni nello spazio
!! geografico; l'oggetto contiene le informazioni di proiezione e dati
!! relativi al grigliato espresse nel sistema proiettato e geografico.
SUBROUTINE griddim_unproj(this)
TYPE(griddim_def),INTENT(inout) :: this !< oggetto che definisce la proiezione e con info relative al grigliato associato

IF (.NOT.c_e(this%dim%nx) .OR. .NOT.c_e(this%dim%ny)) RETURN
CALL alloc(this%dim)
CALL griddim_unproj_internal(this)

END SUBROUTINE griddim_unproj


SUBROUTINE griddim_unproj_internal(this)
TYPE(griddim_def),INTENT(inout) ::this !< oggetto che definisce la proiezione e con info relative al grigliato associato

DOUBLE PRECISION :: x(this%dim%nx,this%dim%ny), y(this%dim%nx,this%dim%ny)

CALL gridpar_coordinates(this%grid%generic, x, y)
CALL griddim_coord_unproj(this, x, y, this%dim%lon, this%dim%lat)

END SUBROUTINE griddim_unproj_internal


!> restituisce il contenuto dell'oggetto
SUBROUTINE griddim_get_val(this, type, &
 nx, ny, &
 lon_min, lon_max, lat_min, lat_max, dx, dy, component_flag, &
 latitude_south_pole, longitude_south_pole, angle_rotation, &
 latitude_stretch_pole, longitude_stretch_pole, stretch_factor, &
 latin1, latin2, lov, lad, projection_center_flag)
TYPE(griddim_def),INTENT(in) :: this !< oggetto da esaminare
CHARACTER(len=*),INTENT(out),OPTIONAL :: type !< type of grid definition
INTEGER,OPTIONAL,INTENT(out) :: nx !< numero dei punti in X 
INTEGER,OPTIONAL,INTENT(out) :: ny !< numero dei punti in Y
!> longitudini minima e massima
doubleprecision,OPTIONAL,INTENT(out) :: lon_min, lon_max
!> latitudini minima e massima
doubleprecision,OPTIONAL,INTENT(out) :: lat_min, lat_max
doubleprecision,OPTIONAL,INTENT(out) :: dx, dy !< Grid steps in x and y directions
INTEGER,OPTIONAL,INTENT(out) :: component_flag !< Resolution and Component Flags
doubleprecision,OPTIONAL,INTENT(out) :: latitude_south_pole !< Latitude of the southern pole of projection
doubleprecision,OPTIONAL,INTENT(out) :: longitude_south_pole !< Longitude of the southern pole of projection 
doubleprecision,OPTIONAL,INTENT(out) :: angle_rotation !< Angle of rotation of projection
doubleprecision,OPTIONAL,INTENT(out) :: latitude_stretch_pole !< Latitude of the pole of stretching
doubleprecision,OPTIONAL,INTENT(out) :: longitude_stretch_pole !< Longitude of the pole of stretching
doubleprecision,OPTIONAL,INTENT(out) :: stretch_factor !< Stretching factor
doubleprecision,OPTIONAL,INTENT(out) :: latin1 !< First standard latitude from main pole (Lambert)
doubleprecision,OPTIONAL,INTENT(out) :: latin2 !< Second standard latitude from main pole (Lambert)
doubleprecision,OPTIONAL,INTENT(out) :: lov !< Line of view, also known as reference longitude or orientation of the grid (polar projections)
doubleprecision,OPTIONAL,INTENT(out) :: lad !< Latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,OPTIONAL,INTENT(out) :: projection_center_flag !< Flag indicating which pole is represented


IF (PRESENT(type)) type = this%grid%type%type
IF (PRESENT(nx)) nx = this%dim%nx
IF (PRESENT(ny)) ny = this%dim%ny

CALL get_val(this%grid%generic, &
 lon_min, lon_max, lat_min, lat_max, dx, dy, component_flag)
CALL get_val(this%grid%rotated, &
 latitude_south_pole, longitude_south_pole, angle_rotation)
CALL get_val(this%grid%stretched, &
 latitude_stretch_pole, longitude_stretch_pole, stretch_factor)
CALL get_val(this%grid%polarproj, &
 latin1, latin2, lov, lad, projection_center_flag=projection_center_flag)

END SUBROUTINE griddim_get_val


!> Imposta il contenuto dell'oggetto
SUBROUTINE griddim_set_val(this, type, &
 nx, ny, &
 lon_min, lon_max, lat_min, lat_max, dx, dy, component_flag, &
 latitude_south_pole, longitude_south_pole, angle_rotation, &
 latitude_stretch_pole, longitude_stretch_pole, stretch_factor, &
 latin1, latin2, lov, lad,projection_center_flag)
TYPE(griddim_def),INTENT(out) :: this
CHARACTER(len=*),INTENT(in),OPTIONAL :: type !< type of grid definition
INTEGER,OPTIONAL,INTENT(in) :: nx !< numero dei punti in X 
INTEGER,OPTIONAL,INTENT(in) :: ny !< numero dei punti in Y
!> longitudini minima e massima
doubleprecision,OPTIONAL,INTENT(in) :: lon_min, lon_max
!> latitudini minima e massima
doubleprecision,OPTIONAL,INTENT(in) :: lat_min, lat_max
doubleprecision,OPTIONAL,INTENT(in) :: dx, dy !< Grid steps in x and y directions
INTEGER,OPTIONAL,INTENT(in) :: component_flag !< Resolution and Component Flags
doubleprecision,OPTIONAL,INTENT(in) :: latitude_south_pole !< Latitude of the southern pole of projection
doubleprecision,OPTIONAL,INTENT(in) :: longitude_south_pole !< Longitude of the southern pole of projection
doubleprecision,OPTIONAL,INTENT(in) :: angle_rotation !< Angle of rotation of projection
doubleprecision,OPTIONAL,INTENT(in) :: latitude_stretch_pole !< Latitude of the pole of stretching
doubleprecision,OPTIONAL,INTENT(in) :: longitude_stretch_pole !< Longitude of the pole of stretching
doubleprecision,OPTIONAL,INTENT(in) :: stretch_factor !< Stretching factor
doubleprecision,OPTIONAL,INTENT(in) :: latin1 !< First standard latitude from main pole (Lambert)
doubleprecision,OPTIONAL,INTENT(in) :: latin2 !< Second standard latitude from main pole (Lambert)
doubleprecision,OPTIONAL,INTENT(in) :: lov !< Line of view, also known as reference longitude or orientation of the grid (polar projections)
doubleprecision,OPTIONAL,INTENT(in) :: lad !< Latitude at which dx and dy (in m) are specified (Lambert, grib2 only)
INTEGER,OPTIONAL,INTENT(in) :: projection_center_flag !< Flag indicating which pole is represented


IF (PRESENT(type)) this%grid%type%type = type
IF (PRESENT(nx)) this%dim%nx = nx
IF (PRESENT(ny)) this%dim%ny = ny

CALL set_val(this%grid%generic, &
 lon_min, lon_max, lat_min, lat_max, dx, dy, component_flag)
CALL set_val(this%grid%rotated, &
 latitude_south_pole, longitude_south_pole, angle_rotation)
CALL set_val(this%grid%stretched, &
 latitude_stretch_pole, longitude_stretch_pole, stretch_factor)
CALL set_val(this%grid%polarproj, &
 latin1, latin2, lov, lad, lon_min, lat_min, projection_center_flag)

END SUBROUTINE griddim_set_val


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE griddim_read_unit(this, unit) 
TYPE(griddim_def),INTENT(out) :: this !< oggetto griddim da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere


CALL read_unit(this%dim, unit)
CALL read_unit(this%grid%generic, unit)
CALL read_unit(this%grid%rotated, unit)
CALL read_unit(this%grid%stretched, unit)
CALL read_unit(this%grid%polarproj, unit)

END SUBROUTINE griddim_read_unit


!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE griddim_write_unit(this, unit)
TYPE(griddim_def),INTENT(in) :: this !< oggetto griddim da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere


CALL write_unit(this%dim, unit)
CALL write_unit(this%grid%generic, unit)
CALL write_unit(this%grid%rotated, unit)
CALL write_unit(this%grid%stretched, unit)
CALL write_unit(this%grid%polarproj, unit)

END SUBROUTINE griddim_write_unit


!> Generates coordinates of every point of a generic grid from the
!! grid description. The number of grid points along both direction is
!! guessed from the shape of x and y arrays, which must be conformal.
SUBROUTINE griddim_gen_coord(this, x, y)
TYPE(griddim_def),INTENT(in) :: this !< generic grid descriptor
DOUBLE PRECISION,INTENT(out) :: x(:,:) !< x coordinate of every point, linearly computed between grid extremes
DOUBLE PRECISION,INTENT(out) :: y(:,:) !< y coordinate of every point, linearly computed between grid extremes, it should have the same shape as x(:,:)


CALL gridpar_coordinates(this%grid%generic, x, y)

END SUBROUTINE griddim_gen_coord


!> Compute grid steps
SUBROUTINE griddim_steps(this, nx, ny, dx, dy)
TYPE(griddim_def), INTENT(in) :: this !< generic grid descriptor
INTEGER,INTENT(in) :: nx !< number of points along x direction
INTEGER,INTENT(in) :: ny !< number of points along y direction
DOUBLE PRECISION,INTENT(out) :: dx !< grid step along x direction
DOUBLE PRECISION,INTENT(out) :: dy !< grid step along y direction

CALL gridpar_steps(this%grid%generic, nx, ny, dx, dy)

END SUBROUTINE griddim_steps


!> Compute and set grid steps
SUBROUTINE griddim_setsteps(this, nx, ny)
TYPE(griddim_def), INTENT(inout) :: this !< generic grid descriptor
INTEGER,INTENT(in) :: nx !< number of points along x direction
INTEGER,INTENT(in) :: ny !< number of points along y direction

CALL gridpar_setsteps(this%grid%generic, nx, ny)

END SUBROUTINE griddim_setsteps


! TODO
! bisogna sviluppare gli altri operatori
ELEMENTAL FUNCTION grid_eq(this, that) RESULT(res)
TYPE(grid_def),INTENT(IN) :: this, that
LOGICAL :: res

res = this%type == that%type .AND. &
 this%generic == that%generic .AND. &
 this%rotated == that%rotated .AND. &
 this%stretched == that%stretched .AND. &
 this%polarproj == that%polarproj

END FUNCTION grid_eq


ELEMENTAL FUNCTION griddim_eq(this, that) RESULT(res)
TYPE(griddim_def),INTENT(IN) :: this, that
LOGICAL :: res

res = this%grid == that%grid .AND. &
 this%dim == that%dim

END FUNCTION griddim_eq


ELEMENTAL FUNCTION grid_type_eq(this, that) RESULT(res)
TYPE(grid_type),INTENT(IN) :: this, that
LOGICAL :: res

res = this%type == that%type

END FUNCTION grid_type_eq


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
IF (gdalassociated(gdalid)) CALL griddim_import_gdal(this, gdalid)
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
INTEGER :: EditionNumber, iScansNegatively, jScansPositively


! Generic keys
CALL grib_get(gaid, 'typeOfGrid', this%grid%type%type)
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG, &
 "griddim_import_gribapi, grid type "//TRIM(this%grid%type%type))
#endif
CALL grib_get(gaid,'GRIBEditionNumber',EditionNumber)

! Keys valid for (almost?) all cases, Ni and Nj are universal aliases
CALL grib_get(gaid, 'Ni', this%dim%nx)
CALL grib_get(gaid, 'Nj', this%dim%ny)

CALL grib_get(gaid,'iScansNegatively',iScansNegatively)
CALL grib_get(gaid,'jScansPositively',jScansPositively)
CALL grib_get(gaid,'uvRelativeToGrid',this%grid%generic%component_flag)

! Keys for rotated grids (checked through missing values)
CALL grib_get_dmiss(gaid,'longitudeOfSouthernPoleInDegrees', &
 this%grid%rotated%longitude_south_pole)
CALL grib_get_dmiss(gaid,'latitudeOfSouthernPoleInDegrees', &
 this%grid%rotated%latitude_south_pole)

IF (EditionNumber == 1) THEN
  CALL grib_get_dmiss(gaid,'angleOfRotationInDegrees', &
   this%grid%rotated%angle_rotation)
ELSE IF (EditionNumber == 2)THEN
  CALL grib_get_dmiss(gaid,'angleOfRotationOfProjectionInDegrees', &
   this%grid%rotated%angle_rotation)
ENDIF

! Keys for stretched grids (checked through missing values)
! units must be verified, still experimental in grib_api
! # TODO: Is it a float? Is it signed?
IF (EditionNumber == 1) THEN
  CALL grib_get_dmiss(gaid,'longitudeOfStretchingPoleInDegrees', &
   this%grid%stretched%longitude_stretch_pole)
  CALL grib_get_dmiss(gaid,'latitudeOfStretchingPoleInDegrees', &
   this%grid%stretched%latitude_stretch_pole)
  CALL grib_get_dmiss(gaid,'stretchingFactor', &
   this%grid%stretched%stretch_factor)
ELSE IF (EditionNumber == 2) THEN
  CALL grib_get_dmiss(gaid,'longitudeOfThePoleOfStretching', &
   this%grid%stretched%longitude_stretch_pole)
  CALL grib_get_dmiss(gaid,'latitudeOfThePoleOfStretching', &
   this%grid%stretched%latitude_stretch_pole)
  CALL grib_get_dmiss(gaid,'stretchingFactor', &
   this%grid%stretched%stretch_factor)
  IF (c_e(this%grid%stretched%stretch_factor)) &
   this%grid%stretched%stretch_factor = this%grid%stretched%stretch_factor*1.0D-6
ENDIF

! Projection-dependent keys
SELECT CASE (this%grid%type%type)

! Keys for sphaerical coordinate systems
CASE ('regular_ll', 'rotated_ll', 'stretched_ll', 'stretched_rotated_ll')

  CALL grib_get(gaid,'longitudeOfFirstGridPointInDegrees',loFirst)
  CALL grib_get(gaid,'longitudeOfLastGridPointInDegrees',loLast)
  CALL grib_get(gaid,'latitudeOfFirstGridPointInDegrees',laFirst)
  CALL grib_get(gaid,'latitudeOfLastGridPointInDegrees',laLast)

  IF (iScansNegatively  == 0) THEN
    this%grid%generic%x1 = loFirst
    this%grid%generic%x2 = loLast
  ELSE
    this%grid%generic%x2 = loFirst
    this%grid%generic%x1 = loLast
  ENDIF
  IF (jScansPositively == 0) THEN
    this%grid%generic%y2 = laFirst
    this%grid%generic%y1 = laLast
  ELSE
    this%grid%generic%y1 = laFirst
    this%grid%generic%y2 = laLast
  ENDIF

! reset longitudes in order to have a Cartesian plane
  IF (this%grid%generic%x2-this%grid%generic%x1 < 0) &
   this%grid%generic%x1 = this%grid%generic%x1 - 360.D0

! compute dx and dy (should we get them from grib?)
  CALL gridpar_setsteps(this%grid%generic, this%dim%nx, this%dim%ny)

! Keys for polar projections
CASE ('polar_stereographic', 'lambert', 'albers')

  CALL grib_get(gaid,'DxInMetres', this%grid%generic%dx)
  CALL grib_get(gaid,'DyInMetres', this%grid%generic%dy)
! latin1/latin2 may be missing (e.g. stereographic)
  CALL grib_get_dmiss(gaid,'Latin1InDegrees',this%grid%polarproj%latin1)
  CALL grib_get_dmiss(gaid,'Latin2InDegrees',this%grid%polarproj%latin2)
! projection center flag, aka hemisphere 
  CALL grib_get(gaid,'projectionCenterFlag',&
   this%grid%polarproj%projection_center_flag)
  IF (IAND(this%grid%polarproj%projection_center_flag,64) == 1) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     "griddim_import_gribapi, bi-polar projections not supported")
    CALL raise_error()
  ENDIF
! line of view, aka central meridian
  CALL grib_get(gaid,'LoVInDegrees',this%grid%polarproj%lov)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   "griddim_import_gribapi, centralMeridian "//TRIM(to_char(this%grid%polarproj%lov)))
#endif

! latitude at which dx and dy are valid
  IF (EditionNumber == 1) THEN
! ECMWF (gribex/grib_api) says: Grid lengths are in metres, at the
! 60-degree parallel nearest to the pole on the projection plane.
!  IF (IAND(this%projection_center_flag, 128) == 0) THEN
!    this%grid%polarproj%lad = 60.D0 
!  ELSE
!    this%grid%polarproj%lad = -60.D0 
!  ENDIF
! WMO says: Grid lengths are in units of metres, at the secant cone
! intersection parallel nearest to the pole on the projection plane.
    this%grid%polarproj%lad = this%grid%polarproj%latin1
  ELSE IF (EditionNumber == 2) THEN
    CALL grib_get(gaid,'LaDInDegrees',this%grid%polarproj%latin1)
  ENDIF

! compute projected extremes from lon and lat of first point
  CALL grib_get(gaid,'longitudeOfFirstGridPointInDegrees',loFirst)
  CALL grib_get(gaid,'latitudeOfFirstGridPointInDegrees',laFirst)
  CALL proj(this, loFirst, laFirst, x1, y1)
  IF (iScansNegatively  == 0) THEN
    this%grid%generic%x1 = x1
    this%grid%generic%x2 = x1 + this%grid%generic%dx*DBLE(this%dim%nx - 1)
  ELSE
    this%grid%generic%x2 = x1
    this%grid%generic%x1 = x1 - this%grid%generic%dx*DBLE(this%dim%nx - 1)
  ENDIF
  IF (jScansPositively == 0) THEN
    this%grid%generic%y2 = y1
    this%grid%generic%y1 = y1 - this%grid%generic%dy*DBLE(this%dim%ny - 1)
  ELSE
    this%grid%generic%y1 = y1
    this%grid%generic%y2 = y1 + this%grid%generic%dy*DBLE(this%dim%ny - 1)
  ENDIF
! keep these values for personal pleasure
  this%grid%polarproj%lon1 = loFirst
  this%grid%polarproj%lat1 = laFirst

CASE default
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "griddim_import_gribapi, grid type "//TRIM(this%grid%type%type)//" not supported")
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

END SUBROUTINE griddim_import_gribapi


! grib_api driver
SUBROUTINE griddim_export_gribapi(this, gaid) 
USE grib_api
TYPE(griddim_def),INTENT(in) :: this ! griddim object
INTEGER, INTENT(inout) :: gaid ! grib_api id of the grib loaded in memory to export

INTEGER :: EditionNumber, iScansNegatively, jScansPositively, nv, pvl
DOUBLE PRECISION :: loFirst, loLast, laFirst, laLast
DOUBLE PRECISION :: sdx, sdy, ratio, tol


! Generic keys
CALL grib_get(gaid,'GRIBEditionNumber',EditionNumber)
CALL grib_set(gaid,'typeOfGrid' ,this%grid%type%type)
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG, &
 "griddim_export_gribapi, grid type "//this%grid%type%type)
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
CALL grib_set(gaid, 'Ni', this%dim%nx)
CALL grib_set(gaid, 'Nj', this%dim%ny)
CALL grib_set(gaid,'uvRelativeToGrid',this%grid%generic%component_flag)

CALL grib_get(gaid,'iScansNegatively',iScansNegatively)
CALL grib_get(gaid,'jScansPositively',jScansPositively)

! Keys for rotated grids (checked through missing values and/or error code)
!SELECT CASE (this%grid%type%type)
!CASE ('rotated_ll', 'stretched_rotated_ll', 'polar_stereographic', 'lambert', 'albers')
CALL grib_set_dmiss(gaid,'longitudeOfSouthernPoleInDegrees', &
 this%grid%rotated%longitude_south_pole, 0.0D0)
CALL grib_set_dmiss(gaid,'latitudeOfSouthernPoleInDegrees', &
 this%grid%rotated%latitude_south_pole, -90.0D0)
IF (EditionNumber == 1) THEN
  CALL grib_set_dmiss(gaid,'angleOfRotationInDegrees', &
   this%grid%rotated%angle_rotation, 0.0D0)
ELSE IF (EditionNumber == 2)THEN
  CALL grib_set_dmiss(gaid,'angleOfRotationOfProjectionInDegrees', &
   this%grid%rotated%angle_rotation, 0.0D0)
ENDIF

! Keys for stretched grids (checked through missing values and/or error code)
! units must be verified, still experimental in grib_api
! # TODO: Is it a float? Is it signed?
IF (EditionNumber == 1) THEN
  CALL grib_set_dmiss(gaid,'longitudeOfStretchingPoleInDegrees', &
   this%grid%stretched%longitude_stretch_pole, 0.0D0)
  CALL grib_set_dmiss(gaid,'latitudeOfStretchingPoleInDegrees', &
   this%grid%stretched%latitude_stretch_pole, -90.0D0)
  CALL grib_set_dmiss(gaid,'stretchingFactor', &
   this%grid%stretched%stretch_factor, 1.0D0)
ELSE IF (EditionNumber == 2) THEN
  CALL grib_set_dmiss(gaid,'longitudeOfThePoleOfStretching', &
   this%grid%stretched%longitude_stretch_pole, 0.0D0)
  CALL grib_set_dmiss(gaid,'latitudeOfThePoleOfStretching', &
   this%grid%stretched%latitude_stretch_pole, -90.0D0)
  CALL grib_set_dmiss(gaid,'stretchingFactor', &
   this%grid%stretched%stretch_factor*1.0D6, 1.0D6)
ENDIF


! Projection-dependent keys
SELECT CASE (this%grid%type%type)

! Keys for sphaerical coordinate systems
CASE ('regular_ll', 'rotated_ll', 'stretched_ll', 'stretched_rotated_ll')

  IF (iScansNegatively  == 0) THEN
    loFirst = this%grid%generic%x1
    loLast = this%grid%generic%x2
  ELSE
    loFirst = this%grid%generic%x2
    loLast = this%grid%generic%x1
  ENDIF
  IF (jScansPositively == 0) THEN
    laFirst = this%grid%generic%y2
    laLast = this%grid%generic%y1
  ELSE
    laFirst = this%grid%generic%y1
    laLast = this%grid%generic%y2
  ENDIF

  CALL grib_set(gaid,'longitudeOfFirstGridPointInDegrees',loFirst)
  CALL grib_set(gaid,'longitudeOfLastGridPointInDegrees',loLast)
  CALL grib_set(gaid,'latitudeOfFirstGridPointInDegrees',laFirst)
  CALL grib_set(gaid,'latitudeOfLastGridPointInDegrees',laLast)

! reset lon in standard grib 2 definition [0,360]
  IF (EditionNumber == 2) THEN
    IF (loFirst < 0.d0) loFirst = loFirst + 360.d0
    IF (loLast < 0.d0) loLast = loLast + 360.d0
  ENDIF

! test relative coordinate truncation error with respect to tol
! tol should be tuned
  sdx = this%grid%generic%dx*ratio
  sdy = this%grid%generic%dy*ratio
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
     TRIM(to_char(this%grid%generic%dx))//' '//TRIM(to_char(this%grid%generic%dy)))
#endif
    CALL grib_set(gaid,'ijDirectionIncrementGiven',1)
    CALL grib_set(gaid,'iDirectionIncrement',NINT(this%grid%generic%dx*ratio))
    CALL grib_set(gaid,'jDirectionIncrement',NINT(this%grid%generic%dy*ratio))
! this does not work in grib_set
!    CALL grib_set(gaid,'iDirectionIncrementInDegrees',this%grid%generic%dx)
!    CALL grib_set(gaid,'jDirectionIncrementInDegrees',this%grid%generic%dy)
  ENDIF

! Keys for polar projections
CASE ('polar_stereographic', 'lambert', 'albers')
! increments are required
  CALL grib_set(gaid,'DxInMetres', this%grid%generic%dx)
  CALL grib_set(gaid,'DyInMetres', this%grid%generic%dy)
  CALL grib_set(gaid,'ijDirectionIncrementGiven',1)
! latin1/latin2 may be missing (e.g. stereographic)
  CALL grib_set_dmiss(gaid,'Latin1InDegrees',this%grid%polarproj%latin1)
  CALL grib_set_dmiss(gaid,'Latin2InDegrees',this%grid%polarproj%latin2)
! projection center flag, aka hemisphere 
  CALL grib_set(gaid,'projectionCenterFlag',&
   this%grid%polarproj%projection_center_flag)
! line of view, aka central meridian
  CALL grib_set(gaid,'LoVInDegrees',this%grid%polarproj%lov)
! latitude at which dx and dy are valid
  IF (EditionNumber == 2) THEN
    CALL grib_set(gaid,'LaDInDegrees',this%grid%polarproj%lad)
  ENDIF

! compute lon and lat of first point from projected extremes
  IF (iScansNegatively  == 0) THEN
    IF (jScansPositively == 0) THEN
      CALL unproj(this, this%grid%generic%x1, this%grid%generic%y2, loFirst, laFirst)
    ELSE
      CALL unproj(this, this%grid%generic%x1, this%grid%generic%y1, loFirst, laFirst)
    ENDIF
  ELSE
    IF (jScansPositively == 0) THEN
      CALL unproj(this, this%grid%generic%x2, this%grid%generic%y2, loFirst, laFirst)
    ELSE
      CALL unproj(this, this%grid%generic%x2, this%grid%generic%y1, loFirst, laFirst)
    ENDIF
  ENDIF
! use the values kept for personal pleasure ?
!  loFirst = this%grid%polarproj%lon1
!  laFirst = this%grid%polarproj%lat1
  CALL grib_set(gaid,'longitudeOfFirstGridPointInDegrees',loFirst)
  CALL grib_set(gaid,'latitudeOfFirstGridPointInDegrees',laFirst)

CASE default
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "griddim_export_gribapi, grid type "//TRIM(this%grid%type%type)//" not supported")
  CALL raise_error()

END SELECT

!TODO
!parrebbe che:
!nelle griglie regular_ll i pvl non si riescono a mettere
!nelle griglie rotated_ll i pvl non si riescono a togliere
! per ora devo fare questo ma poi i PV dovranno essere gestiti
! rifare meglio
!  if (typeOfGrid == "regular_ll") then
!    call l4f_category_log(this%category,L4F_WARN,"Elimino PVL per bug in grib_api")
!    call grib_set(gaid,"numberOfVerticalCoordinateValues",0)
!    call grib_set(gaid,"pvlLocation",33)
!    call grib_set(gaid,"PVPresent",0)
!    call grib_set(gaid,"PLPresent",0)
!  end if

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
    SELECT CASE (this%grid%type%type)
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
SUBROUTINE grib_set_dmiss(gaid, key, value, default)
INTEGER,INTENT(in) :: gaid
CHARACTER(len=*),INTENT(in) :: key
DOUBLE PRECISION,INTENT(in) :: value
DOUBLE PRECISION,INTENT(in),OPTIONAL :: default

INTEGER :: ierr

IF (c_e(value)) THEN
  CALL grib_set(gaid, key, value, ierr)
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

END SUBROUTINE griddim_export_gribapi
#endif


#ifdef HAVE_LIBGDAL
! gdal driver
SUBROUTINE griddim_import_gdal(this, gdalid)
USE gdal
TYPE(griddim_def),INTENT(inout) :: this ! griddim object
TYPE(gdalrasterbandh),INTENT(in) :: gdalid ! gdal rasterband pointer

TYPE(gdaldataseth) :: hds
REAL(kind=c_double) :: geotrans(6), invgeotrans(6), x1, y1, x2, y2
INTEGER :: ier

hds = gdalgetbanddataset(gdalid) ! go back to dataset
ier = gdalgetgeotransform(hds, geotrans)
! get grid corners
CALL gdalapplygeotransform(geotrans, 0.5_c_double, 0.5_c_double, x1, y1)
CALL gdalapplygeotransform(geotrans, gdalgetrasterbandxsize(gdalid)-0.5_c_double, &
 gdalgetrasterbandysize(gdalid)-0.5_c_double, x2, y2)

IF (geotrans(3) == 0.0_c_double .AND. geotrans(5) == 0.0_c_double) THEN
! transformation is diagonal, no transposing
  this%dim%nx =  gdalgetrasterbandxsize(gdalid)
  this%dim%ny =  gdalgetrasterbandysize(gdalid)
  this%grid%generic%x1 = MIN(x1, x2)
  this%grid%generic%x2 = MAX(x1, x2)
  this%grid%generic%y1 = MIN(y1, y2)
  this%grid%generic%y2 = MAX(y1, y2)

ELSE IF (geotrans(2) == 0.0_c_double .AND. geotrans(6) == 0.0_c_double) THEN
! transformation is anti-diagonal, transposing will have to be done
  this%dim%nx =  gdalgetrasterbandysize(gdalid)
  this%dim%ny =  gdalgetrasterbandxsize(gdalid)
  this%grid%generic%x1 = MIN(y1, y2)
  this%grid%generic%x2 = MAX(y1, y2)
  this%grid%generic%y1 = MIN(x1, x2)
  this%grid%generic%y2 = MAX(x1, x2)

ELSE ! transformation is a rotation, not supported
ENDIF

this%grid%type%type = 'regular_ll' ! forced, only one supported (check?)

CALL gridpar_setsteps(this%grid%generic, this%dim%nx, this%dim%ny)
this%grid%generic%component_flag = 0

END SUBROUTINE griddim_import_gdal
#endif


!> Display on the screen a brief content of griddim object.
SUBROUTINE griddim_display(this) 
TYPE(griddim_def),INTENT(in) :: this !< griddim object to display

PRINT*,"<<<<<<<<<<<<<<< ",TRIM(this%grid%type%type)," >>>>>>>>>>>>>>>>"

CALL display(this%dim)
CALL display(this%grid%generic)
CALL display(this%grid%rotated)
CALL display(this%grid%stretched)
CALL display(this%grid%polarproj)

PRINT*,"<<<<<<<<<<<<<<< ---------- >>>>>>>>>>>>>>>>"

END SUBROUTINE griddim_display


! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(grid_type)
#define VOL7D_POLY_TYPES _grid_type
#include "../vol7d/vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES

! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(grid_def)
#define VOL7D_POLY_TYPES _grid
#include "../vol7d/vol7d_distinct.F90"
#undef VOL7D_POLY_TYPE
#undef VOL7D_POLY_TYPES

! Definisce le funzioni count_distinct e pack_distinct
#define VOL7D_POLY_TYPE TYPE(griddim_def)
#define VOL7D_POLY_TYPES _griddim
#include "../vol7d/vol7d_distinct.F90"
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


SUBROUTINE griddim_zoom_coord(this, ilon, ilat, flon, flat, ix, iy, fx, fy)
TYPE(griddim_def),INTENT(in) :: this
!TYPE(grid_dim),INTENT(in) :: dim
DOUBLE PRECISION,INTENT(in) :: ilon,ilat,flon,flat
INTEGER,INTENT(out) :: ix, iy, fx, fy

DOUBLE PRECISION :: dx, dy
DOUBLE PRECISION :: ix1, iy1, fx1, fy1
INTEGER :: lix, liy, lfx, lfy


! compute projected coordinates of vertices of desired lonlat rectangle
CALL proj(this, ilon, ilat, ix1, iy1)
CALL proj(this, flon, flat, fx1, fy1)
! compute projected indices
lix = NINT((ix1-this%grid%generic%x1)/this%grid%generic%dx) + 1
liy = NINT((iy1-this%grid%generic%y1)/this%grid%generic%dy) + 1
lfx = NINT((fx1-this%grid%generic%x1)/this%grid%generic%dx) + 1
lfy = NINT((fy1-this%grid%generic%y1)/this%grid%generic%dy) + 1
! swap projected indices, in case grid is "strongly rotated"
ix = MIN(lix, lfx)
fx = MAX(lix, lfx)
iy = MIN(liy, lfy)
fy = MAX(liy, lfy)

END SUBROUTINE griddim_zoom_coord


end module grid_class


!>\example example_vg6d_1.f90
!!\brief Programma esempio semplice per la definizione di griddim.
!!
!! Programma che crea un oggetto griddim e ne stampa alcuni valori a
!! schermo. Comprende anche una demo dell'uso di log4fortran.

