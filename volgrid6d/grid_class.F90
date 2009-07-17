#include "config.h"
!> Classe per la gestione delle aree geografiche associate a
!! dati su grigliato.
!!
!! Questo modulo definisce gli oggetti e i metodi per gestire le aree
!! geografiche in proiezione e non, associate a dati su grigliato
!! (gridded).  Vengono gestiti differenti sistemi di coordinate
!! geografiche e proiezioni.
!!
!! Programma esempio semplice \include example_vg6d_1.f90
!!
!!\ingroup volgrid6d
module grid_class

use gridpar_generic_class
use gridpar_rotated_class
use gridpar_stretched_class
use gridpar_polarproj_class
use geo_transforms
use log4fortran
use grib_api
use vol7d_class
use err_handling
use grid_dim_class
use optional_values
use simple_stat

implicit none


character (len=255),parameter:: subcategory="grid_class"

!>\brief Oggetto che descrive il grid definition secondo la logica della codifica grib.
!!(gridType in grib_api)
!!The type of grid computed from the grid description section in grib format.
!!
!!    - For both editions:
!!          - regular_ll
!!          - reduced_ll
!!          - mercator
!!          - lambert
!!          - polar_stereographic
!!          - UTM
!!          - simple_polyconic
!!          - albers
!!          - miller
!!          - rotated_ll
!!          - stretched_ll
!!          - stretched_rotated_ll
!!          - regular_gg
!!          - rotated_gg
!!          - stretched_gg
!!          - stretched_rotated_gg
!!          - reduced_gg
!!          - sh
!!          - rotated_sh
!!          - stretched_sh
!!          - stretched_rotated_sh
!!          - space_view
!!    - For edition 2 only:
!!          - triangular_grid
!!          - equatorial_azimuthal_equidistant
!!          - azimuth_range
!!          - cross_section
!!          - Hovmoller
!!          - time_section


type grid_type
  character(len=80) :: type !< The type of grid
end type grid_type

! For equatorial projections like Mercator?
!TYPE gridpar_equatorialproj

!> Definizione di tutte le tipologie di grigliato.
type grid_def
  private
  type(grid_type) :: type !< type of grid definition
  type(gridpar_generic) :: generic
  type(gridpar_rotated) :: rotated
  type(gridpar_stretched) :: stretched
  type(gridpar_polarproj) :: polarproj
!  TYPE(grid_dim), POINTER :: dim

  integer :: category !< category for log4fortran
end type grid_def


!> Definizione del grigliato e delle sue dimensioni. L'oggetto di tipo
!! \a griddim_def è mantenuto separato in modo da permettere che i
!! suoi membri siano \c PRIVATE, mentre \c dim rimane pubblico.
type griddim_def
  type(grid_def) :: grid !< definizione del grigliato
  type(grid_dim) :: dim  !< definizione delle dimensioni

  integer :: category !< log4fortran
end type griddim_def


!>  subtype nearest information
type inter_near
  logical :: external !< enable elaboration outside data bounding box
end type inter_near

!>  subtype bilinear information
type inter_bilin
  logical :: external !< enable elaboration outside data bounding box
end type inter_bilin

!>  subtype linear information
type inter_linear
  logical :: external !< enable elaboration outside data bounding box
end type inter_linear

!>  subtype box information
type inter_box
  double precision :: boxdx !< longitudinal/x extension of the box for box interpolation
  double precision :: boxdy !< latitudinal/y extension of the box for box interpolation, default the target y grid step
  double precision :: boxpercentile !< percentile [0,100] of the distribution of points in the box to use as interpolated value, if missing, the average is used
  logical :: external !< enable elaboration outside data bounding box
end type inter_box

!>  interpolation information 
type inter
  CHARACTER(len=80) :: sub_type !< subtype of transformation, can be \c 'near' \c 'bilin'
  type(inter_near) :: near !< subtype nearest information
  type(inter_bilin) :: bilin !< subtype bilinear information
  type(inter_linear) :: linear !< subtype linear information
  type(inter_box) :: box !< subtype box information
end type inter


!> zoom subtype index information
type zoom_ind
  INTEGER :: ix !< index of initial point of new grid on x
  INTEGER :: iy !< index of initial point of new grid on y
  INTEGER :: fx !< index of final point of new grid on x
  INTEGER :: fy !< index of final point of new grid on y
end type zoom_ind

!> zoom subtype coord information
type zoom_coo
  DOUBLEPRECISION ilon !< coordinate of initial point of new grid on x
  DOUBLEPRECISION ilat !< coordinate of initial point of new grid on y
  DOUBLEPRECISION flon !< coordinate of final point of new grid on x
  DOUBLEPRECISION flat !< coordinate of final point of new grid on y
end type zoom_coo

!> zoom information
type zoom
  CHARACTER(len=80) :: sub_type !< subtype of transformation, can be \c 'index', \c 'coord'
  type(zoom_ind) :: index !< zoom providing index
  type(zoom_coo) :: coord !< zoom providing coordinates
end type zoom

!!> boxregrid subtype average information
!type boxregrid_average
!
!end type boxregrid_average
! no extra information needed now

!> boxregrid  information
type boxregrid
  CHARACTER(len=80) :: sub_type !< subtype of transformation, can be \c 'average'
  INTEGER :: npx !< number of points to average along x direction
  INTEGER :: npy !< number of points to average along y direction
!  type(boxregrid_average) :: average
end type boxregrid


!> \brief transformation object
!! Specifica il tipo di traformazione richiesto
TYPE transform_def

  private

  CHARACTER(len=80) :: trans_type !< type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'interp', ...
  type(zoom) :: zoom !< zoom specification
  type(boxregrid) :: boxregrid !< boxregrid specification
  type(inter) :: inter !< interpolation specification

  integer :: time_definition
  integer :: category !< category for log4fortran

END TYPE transform_def


!> \brief transformation object for grid/vol7d
!! Specifica il tipo di traformazione richiesto associato a una definizione di grid/vol7d
TYPE grid_transform

  private

  TYPE(transform_def) :: trans !<  Specifica il tipo di traformazione richiesto

  integer :: innx,  inny
  integer :: outnx, outny
  integer :: iniox,inioy,infox,infoy,outinx,outiny,outfnx,outfny
  integer,pointer :: inter_index_x(:,:),inter_index_y(:,:)
  doubleprecision,pointer :: inter_x(:,:),inter_y(:,:)

  doubleprecision,pointer :: inter_xp(:,:),inter_yp(:,:)

  integer :: category !< category for log4fortran

END TYPE grid_transform




!> Operatore logico di uguaglianza tra oggetti della classe grid.
!! Funziona anche per confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE grid_eq, grid_type_eq, griddim_eq
END INTERFACE

!> Costruttore dell'oggetto
INTERFACE init
  MODULE PROCEDURE griddim_init, init_grid_transform, init_grid_v7d_transform, init_v7d_grid_transform, init_transform
END INTERFACE

!> Distruttore dell'oggetto
INTERFACE delete
  MODULE PROCEDURE griddim_delete, delete_grid_transform, delete_transform
END INTERFACE

!> Copia l'ggetto creando una nuova istanza
INTERFACE copy
  MODULE PROCEDURE griddim_copy
END INTERFACE

!> Proietta la coordinate geografiche nel relativo sistema di rappresentazione
INTERFACE proj
  MODULE PROCEDURE griddim_coord_proj, griddim_proj
END INTERFACE

!> Rstituisce le coordinate geografiche dal sistema di rappresentazione specifico
INTERFACE unproj
  MODULE PROCEDURE griddim_coord_unproj, griddim_unproj
END INTERFACE

!> Ritorna il contenuto dell'oggetto
INTERFACE get_val
  MODULE PROCEDURE griddim_get_val, get_val_transform
END INTERFACE

!> Imposta il contenuto dell'oggeto
INTERFACE set_val
  MODULE PROCEDURE griddim_set_val
END INTERFACE

!> Scrive l'ggetto su file formatted o unformatted
INTERFACE write_unit
  MODULE PROCEDURE griddim_write_unit
END INTERFACE

!> Legge l'oggetto da file formatted o unformatted
INTERFACE read_unit
  MODULE PROCEDURE griddim_read_unit
END INTERFACE

#ifdef HAVE_LIBGRIBAPI
!> Importazione 
INTERFACE import
  MODULE PROCEDURE griddim_import_gribapi
END INTERFACE

!> Exportazione
INTERFACE export
  MODULE PROCEDURE griddim_export_gribapi
END INTERFACE
#endif

!> visualizzazione su schermo
INTERFACE display
  MODULE PROCEDURE griddim_display
END INTERFACE

!> Calcola i nuovi dati secondo la trasformazione specificata
INTERFACE compute
  MODULE PROCEDURE grid_transform_compute,v7d_grid_transform_compute
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

PUBLIC proj, unproj, griddim_proj, griddim_unproj, griddim_def, grid_def, grid_dim
PUBLIC init, delete, copy
public get_val,set_val,write_unit,read_unit,import,export,display,compute
public operator(==),count_distinct,pack_distinct,map_distinct,map_inv_distinct,index
public transform_def,grid_transform
public wind_unrot

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

CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< accoda questo suffisso al namespace category di log4fortran

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
SUBROUTINE griddim_coordinates(this, x, y)
TYPE(griddim_def),INTENT(in) :: this !< generic grid descriptor
DOUBLE PRECISION,INTENT(out) :: x(:,:) !< x coordinate of every point, linearly computed between grid extremes
DOUBLE PRECISION,INTENT(out) :: y(:,:) !< y coordinate of every point, linearly computed between grid extremes, it should have the same shape as x(:,:)


CALL gridpar_coordinates(this%grid%generic, x, y)

END SUBROUTINE griddim_coordinates


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


#ifdef HAVE_LIBGRIBAPI
!> Import griddim object from id of the grib loaded in memory.
!! The griddim object is populated with all the grid information
!! (size, projection, etc.) carried by the grib message represented by
!! the grib_api id provided.
SUBROUTINE griddim_import_gribapi(this, gaid) 
TYPE(griddim_def),INTENT(out) :: this !< griddim object
INTEGER, INTENT(in) :: gaid !< grib_api id of the grib loaded in memory to import

DOUBLE PRECISION :: loFirst, loLast, laFirst, laLast, x1, y1
INTEGER :: EditionNumber, iScansNegatively, jScansPositively

CALL init(this)

! Generic keys
CALL grib_get(gaid, 'typeOfGrid', this%grid%type%type)
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG, &
 "griddim_import: grid type "//TRIM(this%grid%type%type))
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
     "griddim_import: bi-polar projections not supported")
    CALL raise_error("griddim_import: bi-polar projections not supported")
  ENDIF
! line of view, aka central meridian
  CALL grib_get(gaid,'LoVInDegrees',this%grid%polarproj%lov)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG, &
   "griddim_import centralMeridian "//TRIM(to_char(this%grid%polarproj%lov)))
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
   "griddim_import: grid type "//TRIM(this%grid%type%type)//" not supported")
  CALL raise_error("griddim_import: grid type not supported")

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


!> Export griddim object to id of the grib loaded in memory.
!! The necessary grib_api keys are set according to the grid
!! information (size, projection, etc.) contained in the object.
SUBROUTINE griddim_export_gribapi(this, gaid) 
TYPE(griddim_def),INTENT(in) :: this !< griddim object
INTEGER, INTENT(inout) :: gaid !< grib_api id of the grib loaded in memory to export

INTEGER :: EditionNumber, iScansNegatively, jScansPositively, nv, pvl
DOUBLE PRECISION :: loFirst, loLast, laFirst, laLast
DOUBLE PRECISION :: sdx, sdy, ratio, tol

IF (.NOT. c_e(gaid))RETURN

! Generic keys
CALL grib_get(gaid,'GRIBEditionNumber',EditionNumber)
CALL grib_set(gaid,'typeOfGrid' ,this%grid%type%type)
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG, &
 "griddim_export grid type "//this%grid%type%type)
#endif

! Keys valid for (almost?) all cases, Ni and Nj are universal aliases
CALL grib_set(gaid, 'Ni', this%dim%nx)
CALL grib_set(gaid, 'Nj', this%dim%ny)

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

  IF (EditionNumber == 1) THEN
    ratio = 1.d3
  ELSE IF (EditionNumber == 2) THEN
    ratio = 1.d6
! reset lon in standard grib 2 definition [0,360]
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
     "increments not given: inaccurate!")
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,"dlon relative error: "//&
     TRIM(to_char(ABS(NINT(sdx)/sdx - 1.0d0)))//">"//TRIM(to_char(tol)))
    CALL l4f_category_log(this%category,L4F_DEBUG,"dlat relative error: "//&
     TRIM(to_char(ABS(NINT(sdy)/sdy - 1.0d0)))//">"//TRIM(to_char(tol)))
#endif
    CALL grib_set(gaid,'resolutionAndComponentFlags',0)
    CALL grib_set_missing(gaid,'Di')
    CALL grib_set_missing(gaid,'Dj')
! these do not work
!  CALL grib_set(gaid,'ijDirectionIncrementGiven', 0)
!  CALL grib_set(gaid,'iDirectionIncrementGiven', 0)
!  CALL grib_set(gaid,'jDirectionIncrementGiven', 0)
!  CALL grib_set_missing(gaid,'geography.iInc')
!  CALL grib_set_missing(gaid,'geography.jInc')

  ELSE
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,"setting increments: "// &
     TRIM(to_char(this%grid%generic%dx))//' '//TRIM(to_char(this%grid%generic%dy)))
#endif
    CALL grib_set(gaid,'resolutionAndComponentFlags',128)
    CALL grib_set(gaid,'iDirectionIncrementInDegrees',this%grid%generic%dx)
    CALL grib_set(gaid,'jDirectionIncrementInDegrees',this%grid%generic%dy)
  ENDIF

! Keys for polar projections
CASE ('polar_stereographic', 'lambert', 'albers')
! increments are required
  CALL grib_set(gaid,'DxInMetres', this%grid%generic%dx)
  CALL grib_set(gaid,'DyInMetres', this%grid%generic%dy)
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
   "griddim_export: grid type "//TRIM(this%grid%type%type)//" not supported")
  CALL raise_error("griddim_export: grid type not supported")

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
  CALL l4f_category_log(this%category,L4F_DEBUG,"griddim_export_gribapi: coding "// &
   TRIM(to_char(nv))//" vertical coordinate parameters")
#endif

  IF (nv == 0) THEN
    pvl = 255
  ELSE
    SELECT CASE (this%grid%type%type)
    CASE ('regular_ll', 'polar_stereographic')
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
  CALL l4f_category_log(this%category,L4F_DEBUG,"griddim_export_gribapi: coding "// &
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

! TODO
! bisogna sviluppare gli altri operatori


!> operatore di uguaglianza tra due oggetti grid
ELEMENTAL FUNCTION grid_eq(this, that) RESULT(res)
!> oggetti da confrontare
TYPE(grid_def),INTENT(IN) :: this, that
LOGICAL :: res

res = this%type == that%type .AND. &
 this%generic == that%generic .AND. &
 this%rotated == that%rotated .AND. &
 this%stretched == that%stretched .AND. &
 this%polarproj == that%polarproj

END FUNCTION grid_eq


!> operatore di uguaglianza tra due oggetti griddim
ELEMENTAL FUNCTION griddim_eq(this, that) RESULT(res)
!> oggetti da confrontare
TYPE(griddim_def),INTENT(IN) :: this, that

LOGICAL :: res

res = this%grid == that%grid .AND. &
 this%dim == that%dim

END FUNCTION griddim_eq


!> operatore di uguaglianza tra due oggetti grid_type
elemental FUNCTION grid_type_eq(this, that) RESULT(res)
!> oggetti da confrontare
TYPE(grid_type),INTENT(IN) :: this, that
LOGICAL :: res

res = this%type == that%type

END FUNCTION grid_type_eq


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


SUBROUTINE zoom_coord(this, ilon, ilat, flon, flat, ix, iy, fx, fy)
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

END SUBROUTINE zoom_coord


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



!> Initialises an object that defines a transformation on a grid.
!! trans_type='zoom' cuts or extends \a grid on a new grid adding
!! or removing points on the four sides (zoom).
!! trans_type='box_regrid' regrids \a grid on a new grid in which
!! every point is the average over \a npx X \a npy points of the
!! original grid (box average).
!! All the proper optional parameters, after \a trans_type, should
!! be passed in keyword mode.
SUBROUTINE init_transform(this, trans_type,sub_type, &
 ix, iy, fx, fy, ilon, ilat, flon, flat, &
 npx, npy, boxdx, boxdy, boxpercentile, &
 zoom_type,boxregrid_type,inter_type,external,time_definition,categoryappend)
TYPE(transform_def),INTENT(out) :: this !< transformation object
CHARACTER(len=*) :: trans_type !< type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'interp', ...
CHARACTER(len=*) :: sub_type !< sub type of transformation, depend on trans_type and is an alternative to zoom_type, boxregrid_type, inter_type
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
DOUBLEPRECISION,INTENT(in),OPTIONAL :: boxdx !< longitudinal/x extension of the box for box interpolation, default the target x grid step
DOUBLEPRECISION,INTENT(in),OPTIONAL :: boxdy !< latitudinal/y extension of the box for box interpolation, default the target y grid step
DOUBLEPRECISION,INTENT(in),OPTIONAL :: boxpercentile !< percentile [0,1] of the distribution of points in the box to use as interpolated value, if missing, the average is used
CHARACTER(len=*),INTENT(IN),OPTIONAL :: zoom_type !< type of zoom
CHARACTER(len=*),INTENT(IN),OPTIONAL :: boxregrid_type !< type of regrid
CHARACTER(len=*),INTENT(IN),OPTIONAL :: inter_type !< type of interpolation
LOGICAL,INTENT(IN),OPTIONAL :: external !< activate external area interpolation (for interpolation)(not enabled !)
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< 0=time is reference time ; 1=time is validity time
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran
character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

call optio(trans_type,this%trans_type)

call optio(zoom_type,this%zoom%sub_type)
if (trans_type == "zoom".and. .not. c_e(this%zoom%sub_type))call optio(sub_type,this%zoom%sub_type)

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
  call raise_fatal_error("Error in time_definition")
end if

call optio(boxregrid_type,this%boxregrid%sub_type)
if (trans_type == "boxregrid".and. .not. c_e(this%boxregrid%sub_type))call optio(sub_type,this%boxregrid%sub_type)

call optio(npx,this%boxregrid%npx)
call optio(npy,this%boxregrid%npy)

call optio(inter_type,this%inter%sub_type)
if (trans_type == "inter".and. .not. c_e(this%inter%sub_type))call optio(sub_type,this%inter%sub_type)
call optio(external,this%inter%near%external)
call optio(external,this%inter%bilin%external)
call optio(external,this%inter%linear%external)


if (this%trans_type == 'zoom') then

  if (this%zoom%sub_type == 'coord')then

    if (c_e(this%zoom%coord%ilon) .and. c_e(this%zoom%coord%ilat) .and. &
        c_e(this%zoom%coord%flon) .and. c_e(this%zoom%coord%flat)) then ! coordinates given
    
                                !check
      if ( this%zoom%coord%ilon > this%zoom%coord%flon .or. this%zoom%coord%ilat > this%zoom%coord%flat ) then

        call l4f_category_log(this%category,L4F_ERROR,"zoom coordinates are wrong: ")
        call l4f_category_log(this%category,L4F_ERROR,to_char(ilon)//to_char(this%zoom%coord%ilat))
        call l4f_category_log(this%category,L4F_ERROR,to_char(flon)//to_char(this%zoom%coord%flat))
        call raise_fatal_error("zoom coordinates are wrong")
      end if

    else

      call l4f_category_log(this%category,L4F_ERROR,"zoom: coord missing parameter")
      call raise_fatal_error("zoom: coord missing parameter")
        
    end if

  else if (this%zoom%sub_type == 'index')then

    if (c_e(this%zoom%index%ix) .and. c_e(this%zoom%index%iy) .or. &
        c_e(this%zoom%index%fx) .or. c_e(this%zoom%index%fy)) then


                                ! check
      if (this%zoom%index%ix > this%zoom%index%fx .OR. this%zoom%index%iy > this%zoom%index%fy) then

        CALL l4f_category_log(this%category,L4F_ERROR,'invalid zoom indices: '//&
         to_char(this%zoom%index%ix)//to_char(this%zoom%index%iy)//&
         to_char(this%zoom%index%fx)//to_char(this%zoom%index%fy))
        CALL raise_fatal_error('invalid zoom indices')
      ENDIF

    else

      CALL l4f_category_log(this%category,L4F_ERROR,'zoom: index parameters ix, iy, fx, fy not provided')
      CALL raise_fatal_error('zoom: index parameters ix, iy, fx, fy not provided')

    ENDIF
  

  else

    CALL l4f_category_log(this%category,L4F_ERROR,'zoom: sub_type '// &
     TRIM(this%zoom%sub_type)//' is wrong')
    CALL raise_fatal_error('zoom: sub_type '//TRIM(this%zoom%sub_type)//' is wrong')

  end if


else if (this%trans_type == 'boxregrid') then

  IF (c_e(this%boxregrid%npx) .AND. c_e(this%boxregrid%npy)) THEN

    IF (this%boxregrid%npx <= 0 .OR. this%boxregrid%npy <= 0 ) THEN
      CALL l4f_category_log(this%category,L4F_ERROR,'invalid regrid parameters: '//&
       TRIM(to_char(this%boxregrid%npx))//' '//TRIM(to_char(this%boxregrid%npy)))
      CALL raise_error('invalid regrid parameters')
    ENDIF

  ELSE

    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid parameters npx, npy not provided')
    CALL raise_fatal_error('boxregrid parameters npx, npy not provided')

  ENDIF

  IF (this%boxregrid%sub_type == 'average')THEN
! nothing to do for now
  ELSE
    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid: sub_type '// &
     TRIM(this%boxregrid%sub_type)//' is wrong')
    CALL raise_fatal_error('boxregrid: sub_type '// &
     TRIM(this%boxregrid%sub_type)//' is wrong')
  ENDIF


else if (this%trans_type == 'inter') then

  if (this%inter%sub_type == 'near')then


  else if (this%inter%sub_type == 'bilin')then

!..

  else if (this%inter%sub_type == 'linear')then

!..
  else if (this%inter%sub_type == 'box')then

    CALL optio(boxdx,this%inter%box%boxdx)
    CALL optio(boxdy,this%inter%box%boxdy)
    CALL optio(boxpercentile,this%inter%box%boxpercentile)

  else

    CALL l4f_category_log(this%category,L4F_ERROR,'inter: sub_type '// &
     TRIM(this%inter%sub_type)//' is wrong')
    CALL raise_fatal_error('inter: sub_type '// &
     TRIM(this%inter%sub_type)//' is wrong')
    
  end if

else

  CALL l4f_category_log(this%category,L4F_ERROR,'trans_type '// &
   TRIM(this%trans_type)//' is wrong')
  CALL raise_fatal_error('trans_type '// &
   TRIM(this%trans_type)//' is wrong')

end IF


end SUBROUTINE init_transform



!> \brief destructor of tranform object
!! release any memory and data associated to transformation object
!! the logger category will be deleted too
SUBROUTINE delete_transform(this)

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


end SUBROUTINE delete_transform


!> restituisce il contenuto dell'oggetto
SUBROUTINE get_val_transform(this,time_definition)
type(transform_def),intent(in) :: this !< oggetto da esaminare
integer,INTENT(out),OPTIONAL :: time_definition !< 0=time is reference time ; 1=time is validity time

if ( present(time_definition)) time_definition=this%time_definition

END SUBROUTINE get_val_transform


!> Initialises an object that defines a transformation on a grid.
!! Questo metodo definisce la trasformazione da un grigliato in un'altro seguendo le indicazioni contenute nell'oggetto
!! di trasformazione. Devono essere quindi forniti il grigliato da trasformare e l'oggetto di trasformazione.
!! Vengono generati un oggetto di trasformazione associato ai grigliati e un nuovo grigliato prodotto
!! dalla trasformazione.
SUBROUTINE init_grid_transform(this,trans,in,out,categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(inout) :: in !< griddim object to transform
TYPE(griddim_def),INTENT(out) :: out !< griddim transformated object
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

INTEGER :: nx, ny,i,j
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

    CALL zoom_coord(in, &
     this%trans%zoom%coord%ilon, this%trans%zoom%coord%ilat,&
     this%trans%zoom%coord%flon, this%trans%zoom%coord%flat,&
     this%trans%zoom%index%ix, this%trans%zoom%index%iy, &
     this%trans%zoom%index%fx, this%trans%zoom%index%fy)

    this%trans%zoom%sub_type = 'index'
    
  end if


  if (this%trans%zoom%sub_type == 'index') THEN

    CALL get_val(in, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
     lat_min=lat_min, lat_max=lat_max, dx=steplon, dy=steplat)

                                ! old indices
    this%iniox = min(max(this%trans%zoom%index%ix,1),nx) ! iox
    this%inioy = min(max(this%trans%zoom%index%iy,1),ny) ! ioy
    this%infox = max(min(this%trans%zoom%index%fx,nx),1) ! fox
    this%infoy = max(min(this%trans%zoom%index%fy,ny),1) ! foy
                                ! new indices
    this%outinx = min(max(2-this%trans%zoom%index%ix,1),nx)! inx
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
    
  else

    CALL l4f_category_log(this%category,L4F_WARN,'sub_type '//TRIM(this%trans%zoom%sub_type) &
     //' not supported')
    CALL raise_warning('sub_type '//TRIM(this%trans%zoom%sub_type)//' not supported')

  end if

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
    call copy (in,out)
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

    CALL l4f_category_log(this%category,L4F_WARN,'trans_type '//TRIM(this%trans%boxregrid%sub_type) &
     //' not supported')
    CALL raise_warning('trans_type '//TRIM(this%trans%boxregrid%sub_type)//' not supported')

  ENDIF
  
ELSE IF (this%trans%trans_type == 'inter') THEN

! set increments in new grid in order for all the baraque to work
  CALL griddim_setsteps(out, out%dim%nx, out%dim%ny)

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
      CALL griddim_coordinates(in, this%inter_x, this%inter_y)
! TODO chi mi garantisce che e` stata chiamata la unproj(out)?
! compute coordinates of output grid in input system
      CALL proj(in,out%dim%lon,out%dim%lat,this%inter_xp,this%inter_yp)

    ENDIF
  ELSE IF (this%trans%inter%sub_type == 'box') THEN

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

    CALL l4f_category_log(this%category,L4F_WARN,'init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    CALL raise_warning('init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type)//' not supported')
    
  ENDIF
    
ELSE

  CALL l4f_category_log(this%category,L4F_WARN,'init_grid_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  CALL raise_warning('init_grid_transform trans type '//TRIM(this%trans%trans_type)//' not supported')

ENDIF

END SUBROUTINE init_grid_transform


!> Initialises an object that defines a transformation from a grid to sparse data.
!! Questo metodo definisce la trasformazione da un grigliato a dati sparsi seguendo le indicazioni contenute nell'oggetto
!! di trasformazione. Devono essere quindi forniti il grigliato da trasformare e l'oggetto di trasformazione.
!! Deve essere fornito anche un oggetto contenetente le informazioni relative ai dati sparsi su cui elaborare la trasformazione.
!! Viene generato un oggetto di trasformazione associato al grigliato e dati dati sparsi.
SUBROUTINE init_grid_v7d_transform(this,trans,in,v7d,categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(in) :: in !< griddim object to transform
TYPE(vol7d),INTENT(in) :: v7d !< vol7d objects where (or to use) to transform
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran


INTEGER :: nx, ny,i,j
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

! TODO verificare che siano equivalenti e cancellare le righe commentate
!        do i=1, this%innx
!          do J=1, this%inny
!            this%inter_x(i,j)=lon_min+(((lon_max-lon_min)/dble(this%innx-1))*(i-1))
!            this%inter_y(i,j)=lat_min+(((lat_max-lat_min)/dble(this%inny-1))*(j-1))
!          end do
!        end do
      CALL griddim_coordinates(in, this%inter_x, this%inter_y)

      CALL proj(in,&
       RESHAPE(lon,(/SIZE(lon),1/)),RESHAPE(lat,(/SIZE(lat),1/)),&
       this%inter_xp,this%inter_yp)

    ENDIF

    DEALLOCATE(lon,lat)

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN,'init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    CALL raise_warning('init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type)//' not supported')
    
  ENDIF

ELSE

  CALL l4f_category_log(this%category,L4F_WARN,'init_grid_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  CALL raise_warning('init_grid_transform trans type '//TRIM(this%trans%trans_type)//' not supported')

ENDIF

END SUBROUTINE init_grid_v7d_transform


!> Initialises an object that defines a transformation from sparse data to a grid.
!! Questo metodo definisce la trasformazione da dati sparsi a grigliato seguendo le indicazioni contenute nell'oggetto
!! di trasformazione. Devono essere quindi forniti il grigliato da trasformare e l'oggetto di trasformazione.
!! Deve essere fornito anche un oggetto contenetente le informazioni relative ai dati sparsi su cui elaborare la trasformazione.
!! Viene generato un oggetto di trasformazione associato al grigliato e dati dati sparsi.
SUBROUTINE init_v7d_grid_transform(this,trans,v7d,griddim,categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< grid transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(vol7d),INTENT(in) :: v7d !< vol7d objects to transform
TYPE(griddim_def),INTENT(in) :: griddim !< grid transformated object
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran


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

  if ( this%trans%inter%sub_type == 'linear' ) THEN
    
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

! TODO verificare che siano equivalenti e cancellare le righe commentate
!    DO i=1, this%outnx
!      DO J=1, this%outny
!        this%inter_x(i,j)=lon_min+(((lon_max-lon_min)/DBLE(this%outnx-1))*(i-1))
!        this%inter_y(i,j)=lat_min+(((lat_max-lat_min)/DBLE(this%outny-1))*(j-1))
!      END DO
!    END DO
    CALL griddim_coordinates(griddim, this%inter_x, this%inter_y)

    DEALLOCATE(lon,lat)

  ELSE IF (this%trans%inter%sub_type == 'box') THEN

    this%innx=SIZE(v7d%ana)
    this%inny=1
    CALL get_val(griddim, nx=this%outnx, ny=this%outny)
! if box size not provided, use the actual grid step
    IF (.NOT.c_e(this%trans%inter%box%boxdx)) &
     CALL get_val(griddim, dx=this%trans%inter%box%boxdx)
    IF (.NOT.c_e(this%trans%inter%box%boxdy)) &
     CALL get_val(griddim, dx=this%trans%inter%box%boxdy)
! half size is actually needed
    this%trans%inter%box%boxdx = this%trans%inter%box%boxdx*0.5D0
    this%trans%inter%box%boxdy = this%trans%inter%box%boxdy*0.5D0
! unlike before, here index arrays must have the shape of input grid
    ALLOCATE(lon(this%innx),lat(this%innx))
    ALLOCATE(this%inter_index_x(this%innx,this%inny), &
     this%inter_index_y(this%innx,this%inny), &
     this%inter_x(this%innx,this%inny), &
     this%inter_y(this%innx,this%inny))

! compute coordinates of input grid in output system
    CALL getval(v7d%ana(:)%coord,lon=lon,lat=lat)
    CALL proj(griddim,&
     RESHAPE(lon,(/this%innx,1/)),RESHAPE(lat,(/this%innx,1/)),&
     this%inter_x,this%inter_y)
! find index of output box where every input point falls
    CALL find_index_in_box(griddim, this%inter_x, this%inter_y, &
     this%trans%inter%box%boxdx, this%trans%inter%box%boxdy, &
     this%inter_index_x, this%inter_index_y)
! not needed anymore
    DEALLOCATE(this%inter_x, this%inter_y)

  ELSE

    CALL l4f_category_log(this%category,L4F_WARN,'init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    CALL raise_warning('init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type)//' not supported')
    
  end if
    
ELSE

  CALL l4f_category_log(this%category,L4F_WARN,'init_grid_transform trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  CALL raise_warning('init_grid_transform trans type '//TRIM(this%trans%trans_type)//' not supported')

ENDIF

END SUBROUTINE init_v7d_grid_transform


!> \brief destructor of grid_tranform object
!! release any memory and data associated to grid_transformation object
!! the logger category will be deleted too
SUBROUTINE delete_grid_transform(this)

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

!chiudo il logger
call l4f_category_delete(this%category)

end SUBROUTINE delete_grid_transform


!> From input data array compute the output data array.
!! Grid_transform object contains any information needed for computation.
!! Field_out will be computed.
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
  call raise_fatal_error("inconsistent shape")
end if

if (any(shape(field_out) /= (/this%outnx,this%outny/))) then

  call l4f_category_log(this%category,L4F_ERROR,"inconsistent out shape: "//&
   TRIM(to_char(this%outnx))//","//TRIM(to_char(this%outny))//" /= "//&
   TRIM(to_char(SIZE(field_out,1)))//","//TRIM(to_char(SIZE(field_out,2))))
  call raise_fatal_error("inconsistent shape")
end if


field_out(:,:) = rmiss

IF (this%trans%trans_type == 'zoom') THEN

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,"start grid_transform_compute zoom")
#endif

  field_out(this%outinx:this%outfnx, &
   this%outiny:this%outfny) = &
   field_in(this%iniox:this%infox, &
   this%inioy:this%infoy)

ELSE IF (this%trans%trans_type == 'boxregrid') THEN

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,"start grid_transform_compute boxregrid")
#endif

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

#ifdef DEBUG
  call l4f_category_log(this%category,L4F_DEBUG,"start grid_transform_compute inter")
#endif

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

        IF   (c_e(this%inter_index_x(i,j)) .AND. c_e(this%inter_index_y(i,j)))THEN

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

  else if (this%trans%inter%sub_type == 'box') THEN
    IF (c_e(this%trans%inter%box%boxpercentile)) THEN ! percentile
      IF (this%trans%inter%box%boxpercentile >= 100.0D0) THEN ! optimize for max
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

!        DO j = 1, this%outny
!          DO i = 1, this%outnx
!            field_out(i,j) = MAXVAL(field_in, &
!             mask=(this%inter_index_x == i .AND. &
!             this%inter_index_y == j))
!          ENDDO
!        ENDDO
      ELSE IF (this%trans%inter%box%boxpercentile <= 0.0D0) THEN ! optimize for min
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

      ELSE ! full percentile
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
    ELSE ! average
!      DO j = 1, this%outny
!        DO i = 1, this%outnx
!          field_out(i,j) = stat_average(field_in, &
!           mask=(this%inter_index_x == i .AND. &
!           this%inter_index_y == j))
!        ENDDO
!      ENDDO
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
    ENDIF
  else

    call l4f_category_log(this%category,L4F_ERROR,"sub_type not right here: "//this%trans%inter%sub_type)
    call raise_fatal_error("sub_type not right here")
    
  END IF

else

  call l4f_category_log(this%category,L4F_ERROR,"trans_type not right here: "//this%trans%trans_type)
  call raise_fatal_error("trans_type not right here")

ENDIF

END SUBROUTINE grid_transform_compute


!> From input data vector compute the output data array.
!! Grid_transform object contains any information needed for computation.
!! Field_out will be computed.
SUBROUTINE v7d_grid_transform_compute(this, field_in, field_out)
TYPE(grid_transform),INTENT(in) :: this !< grid_tranform object
REAL, INTENT(in) :: field_in(:) !< input vector
REAL, INTENT(out):: field_out(:,:) !< output matrix

real,allocatable :: field_in_p(:),x_in_p(:),y_in_p(:)
real,allocatable :: x_out(:),y_out(:)
integer :: inn_p,ier

!!$INTEGER :: i, j, ii, jj, ie, je, navg
!!$real :: z1,z2,z3,z4
!!$doubleprecision  :: x1,x3,y1,y3,xp,yp

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start v7d_grid_transform_compute")
#endif

! check size of field_in, field_out

if (size(field_in) /= this%innx) then

  call l4f_category_log(this%category,L4F_ERROR,"inconsistent in shape: "//&
   trim(to_char(this%innx)))
  call raise_fatal_error("inconsistent shape")
end if

if (any(shape(field_out) /= (/this%outnx,this%outny/))) then

  call l4f_category_log(this%category,L4F_ERROR,"inconsistent out shape: "//&
   trim(to_char(this%outny))//" - "//trim(to_char(this%outny)))
  call raise_fatal_error("inconsistent shape")
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
       this%outnx ,this%outny ,real(this%inter_x(:,1)),real(this%inter_y(1,:)),field_out,IER)
#else
      call l4f_category_log(this%category,L4F_ERROR,"libsim compiled without NATGRIDD (ngmath ncarg library)")
      call raise_fatal_error("libsim compiled without NATGRIDD (ngmath ncarg library)")

#endif

      IF (IER .NE. 0) THEN
        call l4f_category_log(this%category,L4F_ERROR,"Error return from NATGRIDD = "//to_char(ier))
        call raise_fatal_error("Error return from NATGRIDD")
      ENDIF

      deallocate(field_in_p,x_in_p,y_in_p)

    else

      call l4f_category_log(this%category,L4F_INFO,"Insufficient data in gridded region to triangulate")

    end if


  else
      
    call l4f_category_log(this%category,L4F_ERROR,"sub_type not right here: "//this%trans%inter%sub_type)
    call raise_fatal_error("sub_type not right here")
      
  END IF

else

  call l4f_category_log(this%category,L4F_ERROR,"trans_type not right here: "//this%trans%trans_type)
  call raise_fatal_error("trans_type not right here")
  
END IF

END SUBROUTINE v7d_grid_transform_compute


!> \brief bilinear interpolation
!!     effettua interpolazione bilineare dati i valori nei punti
!!     1,2,3,4 e le coordinate dei punti 1 e 3 oltre a quelle
!!     del punto p dove viene valutato il campo.
!!_____________________________________________________________
!!				disposizione punti
!!	4	3
!!
!!	  p
!!
!!	1	2
!! _____________________________________________________________

elemental real function hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp) result (zp)

doubleprecision,intent(in):: x1,y1 !< coordinate of the lower left point
doubleprecision,intent(in):: x3,y3 !< coordinate of the upper right point
doubleprecision,intent(in):: xp,yp !< coordinate of point where interpolate
real,intent(in) :: z1,z2,z3,z4 !< Z values on the four points

doubleprecision :: p1,p2
real :: z5,z6


p2=((yp-y1)/(y3-y1))
p1=((xp-x1)/(x3-x1))

z5=(z4-z1)*p2+z1
z6=(z3-z2)*p2+z2

zp=(z6-z5)*(p1)+z5
      

end function hbilin


!> Locate index of requested point
elemental subroutine find_index(this,inter_type,&
 nx,ny, lon_min, lon_max, lat_min,lat_max,&
 lon,lat,index_x,index_y)

type(griddim_def),intent(in) :: this !< griddim object (from grid)
character(len=*),intent(in) :: inter_type !< interpolation type (determine wich point is requested)
!> dimension (to grid)
integer,intent(in) :: nx,ny 
!> extreme coordinate (to grid)
doubleprecision,intent(in) :: lon_min, lon_max, lat_min, lat_max
!> target coordinate
doubleprecision,intent(in) :: lon,lat
!> index of point requested
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

! logging not possibile in elemental subroutine
!  call l4f_category_log(this%category,L4F_ERROR,"inter_type not supported: "//trim(inter_type))
!  call raise_fatal_error("inter_type not supported")

end if

end subroutine find_index


!> Locate index of requested point
SUBROUTINE find_index_in_box(this, xin, yin, dx, dy, index_x, index_y)
TYPE(griddim_def),INTENT(in) :: this
DOUBLE PRECISION,INTENT(in) :: xin(:,:), yin(:,:)
DOUBLE PRECISION,INTENT(in) :: dx, dy
INTEGER,INTENT(out) :: index_x(:,:), index_y(:,:)

DOUBLE PRECISION :: xout(this%dim%nx,this%dim%ny), yout(this%dim%nx,this%dim%ny)
INTEGER :: i, j
! compute coordinates of output grid
CALL griddim_coordinates(this, xout, yout)

index_x(:,:) = imiss
index_x(:,:) = imiss

DO j = 1, this%dim%ny
  DO i = 1, this%dim%nx
    WHERE(xin(:,:) >= xout(i,j)-dx .AND. xin(:,:) < xout(i,j)+dx .AND. &
     yin(:,:) >= yout(i,j)-dy .AND. yin(:,:) < yout(i,j)+dy)
      index_x(:,:) = i
      index_y(:,:) = j
    END WHERE
  ENDDO
ENDDO

END SUBROUTINE find_index_in_box


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

    dist_i = SQRT(dlon_i*dlon_i + dlat_i*dlat_i);
    dist_j = SQRT(dlon_j*dlon_j + dlat_j*dlat_j);

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


end module grid_class


!>\example example_vg6d_1.f90
!!\brief Programma esempio semplice per la definizione di griddim.
!!
!! Programma che crea un oggetto griddim e ne stampa alcuni valori a schermo. Comprende anche una demo dell'uso di log4fortran

