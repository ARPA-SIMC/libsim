#include "config.h"
!>\brief  classe per la gestione delle aree geografiche associate a dati su grigliato.
!!
!! Questo modulo definisce gli oggetti e i metodi per gestire
!! le aree geografiche in proiezione e non, associate a dati su grigliato (gridded).
!! Vengono gestiti differenti sistemi di coordinate geografiche e proiezioni.
!!
!! Programma esempio semplice \include example_vg6d_1.f90
!!
!!\ingroup volgrid6d

module grid_class

use regular_ll_class
use rotated_ll_class
use log4fortran
use grib_api
use vol7d_class
use err_handling
use optional_values

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


!>\brief definizione di tutte le tipologie di grigliato
type grid_def

  private

  type(grid_type)   :: type !< type of grid definition
  type(grid_regular_ll) :: regular_ll !< regular lat lon grid definition
  type(grid_rotated_ll) :: rotated_ll !< rotated lat lon grid definition

  integer :: category !< category for log4fortran

end type grid_def



!>\brief definizione del grigliato  e delle sue dimensioni
type griddim_def


  type(grid_def)   :: grid !< definizione del grigliato
  type(grid_dim)   :: dim  !< definizione delle dimensioni

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

!>  interpolation information 
type inter
  CHARACTER(len=80) :: sub_type !< subtype of transformation, can be \c 'near' \c 'bilin'
  type(inter_near) :: near !< subtype nearest information
  type(inter_bilin) :: bilin !< subtype bilinear information
  type(inter_linear) :: linear !< subtype linear information
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
!  type(interp) :: interp

  integer :: category !< catecory for log4fortran

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
  MODULE PROCEDURE grid_eq, grid_type_eq,griddim_eq
END INTERFACE

!> Costruttore dell'oggetto
INTERFACE init
  MODULE PROCEDURE init_griddim, init_grid_transform, init_grid_v7d_transform, init_v7d_grid_transform, init_transform
END INTERFACE

!> Distruttore dell'oggetto
INTERFACE delete
  MODULE PROCEDURE delete_griddim, delete_grid_transform, delete_transform
END INTERFACE

!> Copia l'ggetto creando una nuova istanza
INTERFACE copy
  MODULE PROCEDURE copy_griddim
END INTERFACE

!> Proietta la coordinate geografiche nel relativo sistema di rappresentazione
INTERFACE proj
  MODULE PROCEDURE generic_proj
END INTERFACE

!> Rstituisce le coordinate geografiche dal sistema di rappresentazione specifico
INTERFACE unproj
  MODULE PROCEDURE generic_unproj
END INTERFACE

!> Ritorna il contenuto dell'oggetto
INTERFACE get_val
  MODULE PROCEDURE get_val_griddim
END INTERFACE

!> Imposta il contenuto dell'oggeto
INTERFACE set_val
  MODULE PROCEDURE set_val_griddim
END INTERFACE

!> Scrive l'ggetto su file formatted o unformatted
INTERFACE write_unit
  MODULE PROCEDURE write_unit_griddim
END INTERFACE

!> Legge l'oggetto da file formatted o unformatted
INTERFACE read_unit
  MODULE PROCEDURE read_unit_griddim
END INTERFACE

!> Importazione 
INTERFACE import
  MODULE PROCEDURE import_griddim
END INTERFACE

!> Exportazione
INTERFACE export
  MODULE PROCEDURE export_griddim
END INTERFACE

!> visualizzazione su schermo
INTERFACE display
  MODULE PROCEDURE display_griddim
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


private

PUBLIC proj, unproj, griddim_proj,griddim_unproj,griddim_def,grid_def,grid_dim
public init,delete,copy
public get_val,set_val,write_unit,read_unit,import,export,display,compute
public operator(==),count_distinct,pack_distinct,map_distinct,map_inv_distinct,index
public transform_def,grid_transform
public wind_unrot
contains


!> \brief init of griddim object
!! inizializza un oggetto griddim
!! 
subroutine init_griddim(this,type,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,angle_rotation, &
 categoryappend)

type(griddim_def) :: this !< oggetto da creare

character(len=*),INTENT(in),OPTIONAL :: type !< type of grid definition
integer,optional :: nx !< numero dei punti in X 
integer,optional :: ny !< numero dei punti in Y
!> longitudini e latitudini minime e massime
doubleprecision,optional :: lon_min, lon_max, lat_min, lat_max
doubleprecision,optional :: latitude_south_pole !< Latitude of the southern pole of projection
doubleprecision,optional :: longitude_south_pole !< Longitude of the southern pole of projection 
doubleprecision,optional :: angle_rotation !< Angle of rotation of projection
!> Resolution and Component Flags
!! -  bit 1	
!!            -  0	i direction increments not given
!!            -  1	i direction increments given
!! -  bit 2	
!!            -  0	j direction increments not given
!!            -  1	j direction increments given
!! -  bit 3	
!!            -  0 	Resolved u- and v- components of vector quantities relative to easterly and northerly directions
!!            -  1 	Resolved u- and v- components of vector quantities relative to the defined grid in the direction of increasing x and y (or i and j) coordinates respectively
integer,optional :: component_flag

character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

call init(this%grid%regular_ll,this%dim)
call init(this%grid%rotated_ll,this%dim)

if (present(type))then
  this%grid%type%type=type
else
  this%grid%type%type=cmiss

  return

end if

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"init gtype: "//this%grid%type%type )
#endif

select case ( this%grid%type%type)

case ( "regular_ll")
  call init(this%grid%regular_ll,this%dim,&
   nx,ny, &
   lon_min, lon_max, lat_min, lat_max, component_flag, &
   categoryappend=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))

case ( "rotated_ll")
  call init(this%grid%rotated_ll,this%dim,&
   nx,ny, &
   lon_min, lon_max, lat_min, lat_max, component_flag, &
   latitude_south_pole,longitude_south_pole,angle_rotation, &
   categoryappend=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"init griddim gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("init griddim gtype non gestita")

end select


end subroutine init_griddim

!> Cancellazione oggetto griddim
subroutine delete_griddim(this)
type(griddim_def) :: this !< oggetto griddim da cancellare

select case ( this%grid%type%type)

case ( "regular_ll")
  call delete(this%grid%regular_ll,this%dim)

case ( "rotated_ll")
  call delete(this%grid%rotated_ll,this%dim)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"delete griddim gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("delete griddim gtype non gestita")

end select

!chiudo il logger
call l4f_category_delete(this%category)


end subroutine delete_griddim

!> Clona un oggetto griddim creandone una nuova istanza
subroutine copy_griddim(this,that,categoryappend)

type(griddim_def),intent(in) :: this !< oggetto da clonare
type(griddim_def),intent(out) :: that !< oggetto clonato
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

that%grid%type=this%grid%type

select case (this%grid%type%type)

case ( "regular_ll")

  call copy(this%grid%regular_ll,that%grid%regular_ll,categoryappend=categoryappend)
  call copy (this%dim,that%dim)

case ( "rotated_ll")

  call copy(this%grid%rotated_ll,that%grid%rotated_ll,categoryappend=categoryappend)
  call copy (this%dim,that%dim)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"copy_griddim gtype: "//TRIM(this%grid%type%type)//" non gestita")
  call raise_error("copy_griddim gtype: "//TRIM(this%grid%type%type)//" non gestita")

end select


!new category
call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
that%category=l4f_category_get(a_name)

end subroutine copy_griddim

!> Proietta coordinate geografiche nel sistema definito
elemental subroutine generic_proj (this,lon,lat,x,y)

type(griddim_def),intent(in) :: this !< definizione della proiezione
!> coordinate geografiche da proiettare
doubleprecision, intent(in) :: lon,lat 
!> coordinate proiettate
doubleprecision, intent(out)  :: x,y


select case ( this%grid%type%type)

case ( "regular_ll")
  call proj(this%grid%regular_ll,lon,lat,x,y)

case ( "rotated_ll")
  call proj(this%grid%rotated_ll,lon,lat,x,y)
  
case default
  x=dmiss
  y=dmiss
!  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
!  call raise_error("gtype non gestita")

end select

end subroutine generic_proj

!>Calcola le coordinate geografiche date le coordinate nel sistema definito
elemental subroutine generic_unproj (this,x,y,lon,lat)

type(griddim_def),intent(in) ::this !< definizione della proiezione
!> coordinate proiettate
doubleprecision, intent(in) :: x,y
!> coordinate geografiche
doubleprecision, intent(out)  :: lon,lat

select case ( this%grid%type%type)

case ( "regular_ll")
  call unproj(this%grid%regular_ll,x,y,lon,lat)

case ( "rotated_ll")
  call unproj(this%grid%rotated_ll,x,y,lon,lat)

case default
!  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
!  call raise_error("gtype non gestita")

end select

end subroutine generic_unproj


!> Proietta un oggetto griddim nel sistema definito.
!! Effettua una serie di conti per avere informazioni nello spazio di
!! proiezione; l'oggetto contiene le informazioni di proiezione e dati
!! relativi al grigliato espresse nel sistema proiettato e geografico.
subroutine griddim_proj (this)

type(griddim_def),intent(in) :: this !< oggetto che definisce la proiezione e con info relative al grigliato associato

select case ( this%grid%type%type)

case ( "regular_ll")
  call grid_proj(this%grid%regular_ll,this%dim)

case ( "rotated_ll")
  call grid_proj(this%grid%rotated_ll,this%dim)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"griddim proj gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("griddim proj gtype non gestita")

end select

end subroutine griddim_proj


!> Calcola informazioni nel sistema geografico di un oggetto griddim.
!! Effettua una serie di conti per avere informazioni nello spazio
!! geografico; l'oggetto contiene le informazioni di proiezione e dati
!! relativi al grigliato espresse nel sistema proiettato e geografico.
subroutine griddim_unproj(this)

type(griddim_def),intent(in) ::this !< oggetto che definisce la proiezione e con info relative al grigliato associato

select case ( this%grid%type%type)

case ( "regular_ll")
  call grid_unproj(this%grid%regular_ll, this%dim)

case ( "rotated_ll")
  call grid_unproj(this%grid%rotated_ll, this%dim)

case default
  call l4f_category_log(this%category,L4F_ERROR,"griddim unproj gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("griddim unproj gtype non gestita")

end select

end subroutine griddim_unproj


!> restituisce il contenuto dell'oggetto
subroutine get_val_griddim(this,type,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,angle_rotation)

type(griddim_def),intent(in) :: this !< oggetto da esaminare

character(len=*),INTENT(out),OPTIONAL :: type !< type of grid definition
integer,optional,intent(out) :: nx !< numero dei punti in X 
integer,optional,intent(out) :: ny !< numero dei punti in Y
!> longitudini minima e massima
doubleprecision,optional,intent(out) :: lon_min, lon_max
!> latitudini minima e massima
doubleprecision,optional,intent(out) :: lat_min, lat_max
doubleprecision,optional,intent(out) :: latitude_south_pole !< Latitude of the southern pole of projection
doubleprecision,optional,intent(out) :: longitude_south_pole !< Longitude of the southern pole of projection 
doubleprecision,optional,intent(out) :: angle_rotation !< Angle of rotation of projection
integer,optional,intent(out) :: component_flag !< Resolution and Component Flags



if (present(lon_min))lon_min=dmiss
if (present(lon_max))lon_max=dmiss
if (present(lat_min))lat_min=dmiss
if (present(lat_max))lat_max=dmiss
if (present(latitude_south_pole))latitude_south_pole=dmiss
if (present(longitude_south_pole))longitude_south_pole=dmiss
if (present(angle_rotation))angle_rotation=dmiss
if (present(component_flag))component_flag=imiss


if (present(type)) type = this%grid%type%type
if (this%grid%type%type == cmiss) return

select case (this%grid%type%type)

case ( "regular_ll")
  call get_val(this%grid%regular_ll,this%dim,&
   nx,ny, &
   lon_min, lon_max, lat_min, lat_max, component_flag)

case ( "rotated_ll")
  call get_val(this%grid%rotated_ll,this%dim,&
   nx,ny, &
   lon_min, lon_max, lat_min, lat_max, component_flag, &
   latitude_south_pole,longitude_south_pole,angle_rotation)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"get_val_griddim gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("get_val_griddim gtype non gestita")

end select


end subroutine get_val_griddim

!> Imposta il contenuto dell'oggetto
subroutine set_val_griddim(this,type,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,angle_rotation)

type(griddim_def),intent(out) :: this

character(len=*),INTENT(in),OPTIONAL :: type !< type of grid definition
integer,optional,intent(in) :: nx !< numero dei punti in X 
integer,optional,intent(in) :: ny !< numero dei punti in Y
!> longitudini minima e massima
doubleprecision,optional,intent(in) :: lon_min, lon_max
!> latitudini minima e massima
doubleprecision,optional,intent(in) :: lat_min, lat_max
doubleprecision,optional,intent(in) :: latitude_south_pole !< Latitude of the southern pole of projection
doubleprecision,optional,intent(in) :: longitude_south_pole !< Longitude of the southern pole of projection 
doubleprecision,optional,intent(in) :: angle_rotation !< Angle of rotation of projection
integer,optional,intent(in) :: component_flag !< Resolution and Component Flags

if (present(type)) this%grid%type%type = type
if (this%grid%type%type == cmiss) return

select case (this%grid%type%type)

case ( "regular_ll")
  call set_val(this%grid%regular_ll,this%dim,&
   nx,ny, &
   lon_min, lon_max, lat_min, lat_max, component_flag)

case ( "rotated_ll")
  call set_val(this%grid%rotated_ll,this%dim,&
   nx,ny, &
   lon_min, lon_max, lat_min, lat_max, component_flag, &
   latitude_south_pole,longitude_south_pole,angle_rotation)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"set_val_griddim gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("set_val_griddim gtype non gestita")

end select


end subroutine set_val_griddim


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE read_unit_griddim(this,unit) 

type(griddim_def),intent(out) :: this !< oggetto griddim da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere


select case ( this%grid%type%type)

case ( "regular_ll")
  call read_unit(this%grid%regular_ll,unit)

case ( "rotated_ll")
  call read_unit(this%grid%rotated_ll,unit)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

end select


call read_unit(this%dim,unit)


END SUBROUTINE read_unit_griddim



!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE write_unit_griddim(this, unit)

type(griddim_def),intent(in) :: this !< oggetto griddim da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere


select case ( this%grid%type%type)

case ( "regular_ll")
  call write_unit(this%grid%regular_ll,unit)

case ( "rotated_ll")
  call write_unit(this%grid%rotated_ll,unit)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"write_unit_griddim gtype: "//trim(this%grid%type%type)//" non gestita" )
  call raise_error("write_unit_griddim gtype non gestita")

end select

call write_unit(this%dim,unit)


END SUBROUTINE write_unit_griddim


!> \brief import griddim object from id of the grib loaded in memory
!! avendo a disposizione un id di un grib fornito dalla grib_api viene impostato un oggetto griddim
!! contenente tutte le informazioni sul grigliato (rappresentazione e dimensioni)
SUBROUTINE import_griddim(this, gaid) 

type(griddim_def),intent(out) :: this !< object griddim
INTEGER, INTENT(in) :: gaid !< grib_api id of the grib loaded in memory to import

call grib_get(gaid,'typeOfGrid' ,this%grid%type%type)
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"import_griddim gtype: "//this%grid%type%type)
#endif

select case ( this%grid%type%type)

case ( "regular_ll")
  call import(this%grid%regular_ll,this%dim,gaid)

case ( "rotated_ll")
  call import(this%grid%rotated_ll,this%dim,gaid)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"import griddim gtype non gestita: "//trim(this%grid%type%type))
  call raise_error("import griddim gtype non gestita")

end select

END SUBROUTINE import_griddim


!> \brief export from griddim object to id of the grib loaded in memory
!! da un oggetto griddim contenente tutte le informazioni sul grigliato (rappresentazione e dimensioni)
!! vengono impostate le chiavi relative di un id di un grib fornito dalla grib_api
SUBROUTINE export_griddim(this, gaid) 

type(griddim_def),intent(in) :: this !< object griddim
INTEGER, INTENT(inout) :: gaid !< grib_api id of the grib loaded in memory to import

if (.not. c_e(gaid))return

call grib_set(gaid,'typeOfGrid' ,this%grid%type%type)
#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"export_griddim gtype: "//this%grid%type%type)
#endif

select case ( this%grid%type%type)

case ( "regular_ll")
  call export(this%grid%regular_ll,this%dim,gaid)

case ( "rotated_ll")
  call export(this%grid%rotated_ll,this%dim,gaid)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"export griddim gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("export griddim gtype non gestita")

end select

END SUBROUTINE export_griddim


! TODO
! bisogna sviluppare gli altri operatori


!> operatore di uguaglianza tra due oggetti grid
elemental FUNCTION grid_eq(this, that) RESULT(res)
!> oggetti da confrontare
TYPE(grid_def),INTENT(IN) :: this, that
LOGICAL :: res

res = this%type == that%type .and. &
 this%regular_ll == that%regular_ll .and. &
 this%rotated_ll == that%rotated_ll

END FUNCTION grid_eq


!> operatore di uguaglianza tra due oggetti griddim
elemental FUNCTION griddim_eq(this, that) RESULT(res)
!> oggetti da confrontare
TYPE(griddim_def),INTENT(IN) :: this, that

LOGICAL :: res

res = this%grid == that%grid .and. &
 this%dim == that%dim

END FUNCTION griddim_eq



!> operatore di uguaglianza tra due oggetti grid_type
elemental FUNCTION grid_type_eq(this, that) RESULT(res)
!> oggetti da confrontare
TYPE(grid_type),INTENT(IN) :: this, that
LOGICAL :: res

res = this%type == that%type

END FUNCTION grid_type_eq


!> \brief display on the screen a brief content of griddim object
SUBROUTINE display_griddim(this) 

type(griddim_def),intent(in) :: this !< griddim object to display

select case ( this%grid%type%type)

case ( "regular_ll")

  print*,"<<<<<<<<<<<<<<< regular_ll >>>>>>>>>>>>>>>>"
  call display(this%grid%regular_ll,this%dim)
  print*,"<<<<<<<<<<<<<<< ---------- >>>>>>>>>>>>>>>>"

case ( "rotated_ll")
  print*,"<<<<<<<<<<<<<<< rotated_ll >>>>>>>>>>>>>>>>"
  call display(this%grid%rotated_ll,this%dim)
  print*,"<<<<<<<<<<<<<<< ---------- >>>>>>>>>>>>>>>>"
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"display griddim gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("display griddim gtype non gestita")

end select

end SUBROUTINE display_griddim



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
 npx, npy, &
 zoom_type,boxregrid_type,inter_type,external,categoryappend)

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
logical,INTENT(IN),OPTIONAL :: external !< activate external area interpolation (for interpolation)(not enabled !)
CHARACTER(len=*),INTENT(IN),OPTIONAL :: zoom_type !< type of zoom
CHARACTER(len=*),INTENT(IN),OPTIONAL :: boxregrid_type !< type of regrid
CHARACTER(len=*),INTENT(IN),OPTIONAL :: inter_type !< type of interpolation

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

    CALL l4f_category_log(this%category,L4F_ERROR,'zoom: sub_type is wrong')
    CALL raise_fatal_error('zoom: sub_type is wrong')

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
    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid: sub_type is wrong')
    CALL raise_fatal_error('boxregrid: sub_type is wrong')
  ENDIF


else if (this%trans_type == 'inter') then

  if (this%inter%sub_type == 'near')then


  else if (this%inter%sub_type == 'bilin')then

!..

  else if (this%inter%sub_type == 'linear')then

!..

  else

    CALL l4f_category_log(this%category,L4F_ERROR,'inter: sub_type is wrong')
    CALL raise_fatal_error('inter: sub_type is wrong')
    
  end if

else

  CALL l4f_category_log(this%category,L4F_ERROR,'trans_type is wrong')
  CALL raise_fatal_error('trans_type is wrong')

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


!> Initialises an object that defines a transformation on a grid.
!! Questo metodo definisce la trasformazione da un grigliato in un'altro seguendo le indicazioni contenute nell'oggetto
!! di trasformazione. Devono essere quindi forniti il grigliato da trasformare e l'oggetto di trasformazione.
!! Vengono generati un oggetto di trasformazione associato ai grigliati e un nuovo grigliato prodotto
!! dalla trasformazione.
SUBROUTINE init_grid_transform(this,trans,in,out,categoryappend)

TYPE(grid_transform),INTENT(out) :: this !< grid_transformation object
TYPE(transform_def),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(in) :: in !< griddim object to transform
TYPE(griddim_def),INTENT(out) :: out !< griddim transformated object
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appende questo suffisso al namespace category di log4fortran

INTEGER :: nx, ny,i,j
DOUBLE PRECISION :: lon_min, lon_max, lat_min, lat_max, steplon, steplat,lon_min_new, lat_min_new
character(len=512) :: a_name

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

    select case ( in%grid%type%type )

    case ( "regular_ll")

      call zoom_coord(in%grid%regular_ll,in%dim, &
       this%trans%zoom%coord%ilon, this%trans%zoom%coord%ilat,&
       this%trans%zoom%coord%flon, this%trans%zoom%coord%flat,&
       this%trans%zoom%index%ix, this%trans%zoom%index%iy, &
       this%trans%zoom%index%fx, this%trans%zoom%index%fy)

    case ( "rotated_ll")

      call zoom_coord(in%grid%rotated_ll,in%dim, &
       this%trans%zoom%coord%ilon, this%trans%zoom%coord%ilat,&
       this%trans%zoom%coord%flon, this%trans%zoom%coord%flat,&
       this%trans%zoom%index%ix, this%trans%zoom%index%iy, &
       this%trans%zoom%index%fx, this%trans%zoom%index%fy)

    case default

      call l4f_category_log(this%category,L4F_ERROR,"init_grid_transform zoom coord gtype: "&
       //trim(in%grid%type%type)//" non gestita" )
      call raise_fatal_error("init_grid_transform zoom coord gtype non gestita")
      
    end select

    this%trans%zoom%sub_type = 'index'
    
  end if


  if (this%trans%zoom%sub_type == 'index') THEN

    select case ( in%grid%type%type )
      
    case ( "regular_ll","rotated_ll")
      
      CALL get_val(in, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
       lat_min=lat_min, lat_max=lat_max)
      
      steplon=(lon_max-lon_min)/dble(nx-1)
      steplat=(lat_max-lat_min)/dble(ny-1)
      
      
    case default
      call l4f_category_log(this%category,L4F_ERROR,"init_grid_transform zoom index gtype: "&
       //trim(in%grid%type%type)//" non gestita" )
      call raise_fatal_error("init_grid_transform zoom index gtype non gestita")
      
    end select

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
     lon_min = lon_min,  lon_max = lon_max ,&
     lat_min = lat_min,  lat_max = lat_max )
    
  else

    CALL l4f_category_log(this%category,L4F_WARN,'sub_type '//TRIM(this%trans%zoom%sub_type) &
     //' not supported')
    CALL raise_warning('sub_type '//TRIM(this%trans%zoom%sub_type)//' not supported')

  end if

ELSE IF (this%trans%trans_type == 'boxregrid') THEN

  if (this%trans%boxregrid%sub_type == 'average') THEN

    select case ( in%grid%type%type )

    case ( "regular_ll","rotated_ll")
      
      CALL get_val(in, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
       lat_min=lat_min, lat_max=lat_max)
      
    case default
      call l4f_category_log(this%category,L4F_ERROR,"init_grid_transform boxregrid average gtype: "&
       //trim(in%grid%type%type)//" non gestita" )
      call raise_fatal_error("init_grid_transform boxregrid average gtype non gestita")
    
    end select

    this%innx = nx
    this%inny = ny

    steplon=(lon_max-lon_min)/(nx-1)
    steplat=(lat_max-lat_min)/(ny-1)

! new grid
    lon_min_new = lon_min + (this%trans%boxregrid%npx - 1)*0.5D0*steplon
    lat_min_new = lat_min + (this%trans%boxregrid%npy - 1)*0.5D0*steplat

    CALL l4f_category_log(this%category,L4F_DEBUG,"copying griddim in out")
    call copy (in,out)
    out%dim%nx = nx/this%trans%boxregrid%npx
    out%dim%ny = ny/this%trans%boxregrid%npy

    this%outnx=out%dim%nx
    this%outny=out%dim%ny

    call set_val( out,lon_min = lon_min_new )
    call set_val( out,lat_min = lat_min_new )

    steplon = steplon*this%trans%boxregrid%npx
    steplat = steplat*this%trans%boxregrid%npy

    call set_val( out, lon_max = lon_min_new + (out%dim%nx - 1)*steplon )
    call set_val( out, lat_max = lat_min_new + (out%dim%ny - 1)*steplat )

  else

    CALL l4f_category_log(this%category,L4F_WARN,'trans_type '//TRIM(this%trans%boxregrid%sub_type) &
     //' not supported')
    CALL raise_warning('trans_type '//TRIM(this%trans%boxregrid%sub_type)//' not supported')

  end if
  
ELSE IF (this%trans%trans_type == 'inter') THEN

  if (this%trans%inter%sub_type == 'near' .or. this%trans%inter%sub_type == 'bilin' ) THEN
    
    select case ( in%grid%type%type )
      
    case ( "regular_ll","rotated_ll")


      CALL get_val(in, nx=nx, ny=ny)
      
      this%innx=nx
      this%inny=ny
      
      CALL get_val(out, nx=nx, ny=ny)
      
      this%outnx=nx
      this%outny=ny
      
      allocate (this%inter_index_x(nx,ny),this%inter_index_y(nx,ny))
    
      CALL get_val(in, &
       lon_min=lon_min, lon_max=lon_max,&
       lat_min=lat_min, lat_max=lat_max)

      call find_index(in,this%trans%inter%sub_type,&
       nx=this%innx, ny=this%inny ,&
       lon_min=lon_min, lon_max=lon_max,&
       lat_min=lat_min, lat_max=lat_max,&
       lon=out%dim%lon,lat=out%dim%lat,&
       index_x=this%inter_index_x,index_y=this%inter_index_y)


      if ( this%trans%inter%sub_type == 'bilin' ) THEN
        allocate (this%inter_x(this%innx,this%inny),this%inter_y(this%innx,this%inny))
        allocate (this%inter_xp(this%outnx,this%outny),this%inter_yp(this%outnx,this%outny))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! TODO ora utilizzo le latmin etc. ma il caso non è generale
! la getval in altri casi non mi restituisce niente e quindi bisognerà inventarsi
! un piano di proiezione X,Y (da 0. a 1. ?) a cui far riferimento
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do i=1, this%innx
          do J=1, this%inny

            this%inter_x(i,j)=lon_min+(((lon_max-lon_min)/dble(this%innx-1))*(i-1))
            this%inter_y(i,j)=lat_min+(((lat_max-lat_min)/dble(this%inny-1))*(j-1))
          
          end do
        end do

        call proj(in,out%dim%lon,out%dim%lat,this%inter_xp,this%inter_yp)

      end if

    case default
      call l4f_category_log(this%category,L4F_ERROR,"init_grid_transform inter gtype: "//trim(in%grid%type%type)//" non gestita" )
      call raise_fatal_error("init_grid_transform inter gtype non gestita")
      
    end select

  else

    CALL l4f_category_log(this%category,L4F_WARN,'init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    CALL raise_warning('init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type)//' not supported')
    
  end if
    
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

  if (this%trans%inter%sub_type == 'near' .or. this%trans%inter%sub_type == 'bilin' ) THEN
    
    select case ( in%grid%type%type )
      
    case ( "regular_ll","rotated_ll")


      CALL get_val(in, nx=nx, ny=ny)
      
      this%innx=nx
      this%inny=ny
      
      this%outnx=size(v7d%ana)
      this%outny=1
      
      allocate (this%inter_index_x(this%outnx,this%outny),&
       this%inter_index_y(this%outnx,this%outny))
      allocate(lon(this%outnx),lat(this%outnx))

      CALL get_val(in, &
       lon_min=lon_min, lon_max=lon_max,&
       lat_min=lat_min, lat_max=lat_max)

      call getval(v7d%ana(:)%coord,lon=lon,lat=lat)

      call find_index(in,this%trans%inter%sub_type,&
       nx=this%innx, ny=this%inny ,&
       lon_min=lon_min, lon_max=lon_max,&
       lat_min=lat_min, lat_max=lat_max,&
       lon=reshape(lon,(/size(lon),1/)),lat=reshape(lat,(/size(lat),1/)),&
       index_x=this%inter_index_x,index_y=this%inter_index_y)


      if ( this%trans%inter%sub_type == 'bilin' ) THEN
        allocate (this%inter_x(this%innx,this%inny),this%inter_y(this%innx,this%inny))
        allocate (this%inter_xp(this%outnx,this%outny),this%inter_yp(this%outnx,this%outny))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! TODO ora utilizzo le latmin etc. ma il caso non è generale
! la getval in altri casi non mi restituisce niente e quindi bisognerà inventarsi
! un piano di proiezione X,Y (da 0. a 1. ?) a cui far riferimento
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do i=1, this%innx
          do J=1, this%inny

            this%inter_x(i,j)=lon_min+(((lon_max-lon_min)/dble(this%innx-1))*(i-1))
            this%inter_y(i,j)=lat_min+(((lat_max-lat_min)/dble(this%inny-1))*(j-1))
          
          end do
        end do

        call proj(in,&
       reshape(lon,(/size(lon),1/)),reshape(lat,(/size(lat),1/)),&
       this%inter_xp,this%inter_yp)

      end if

      deallocate(lon,lat)

    case default
      call l4f_category_log(this%category,L4F_ERROR,"init_grid_transform inter gtype: "//trim(in%grid%type%type)//" non gestita" )
      call raise_fatal_error("init_grid_transform inter gtype non gestita")
      
    end select

  else

    CALL l4f_category_log(this%category,L4F_WARN,'init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type) &
     //' not supported')
    CALL raise_warning('init_grid_transform inter sub_type '//TRIM(this%trans%inter%sub_type)//' not supported')
    
  end if
    
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
    
    select case ( griddim%grid%type%type )
      
    case ( "regular_ll","rotated_ll")


      CALL get_val(griddim, nx=nx, ny=ny)
      
      this%outnx=nx
      this%outny=ny
      
      this%innx=size(v7d%ana)
      this%inny=1
      
      allocate (lon(this%innx),lat(this%innx))
      allocate (this%inter_xp(this%innx,this%inny),this%inter_yp(this%innx,this%inny))
      allocate (this%inter_x(this%outnx,this%outny),this%inter_y(this%outnx,this%outny))

      CALL get_val(griddim, &
       lon_min=lon_min, lon_max=lon_max,&
       lat_min=lat_min, lat_max=lat_max)

      call getval(v7d%ana(:)%coord,lon=lon,lat=lat)

      call proj(griddim,&
       reshape(lon,(/size(lon),1/)),reshape(lat,(/size(lat),1/)),&
       this%inter_xp,this%inter_yp)
      

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! TODO ora utilizzo le latmin etc. ma il caso non è generale
! la getval in altri casi non mi restituisce niente e quindi bisognerà inventarsi
! un piano di proiezione X,Y (da 0. a 1. ?) a cui far riferimento
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do i=1, this%outnx
        do J=1, this%outny

          this%inter_x(i,j)=lon_min+(((lon_max-lon_min)/dble(this%outnx-1))*(i-1))
          this%inter_y(i,j)=lat_min+(((lat_max-lat_min)/dble(this%outny-1))*(j-1))
          
        end do
      end do


      deallocate(lon,lat)


    case default
      call l4f_category_log(this%category,L4F_ERROR,"init_grid_transform inter gtype: "//&
       trim(griddim%grid%type%type)//" non gestita" )
      call raise_fatal_error("init_grid_transform inter gtype non gestita")
      
    end select

  else

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

!> \brief from input data matrix compute the output data matrix
!! Grid_transform object contains any information needed for computation.
!! Field_out will bee computed.
SUBROUTINE grid_transform_compute(this, field_in, field_out)
TYPE(grid_transform),INTENT(in) :: this !< grid_transformation object
REAL, INTENT(in) :: field_in(:,:) !< input matrix
REAL, INTENT(out) :: field_out(:,:) !< output matrix

INTEGER :: i, j, ii, jj, ie, je, navg
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

        if   (c_e(this%inter_index_x(i,j)) .and. c_e(this%inter_index_y(i,j)))then

          z1=field_in(this%inter_index_x(i,j),this%inter_index_y(i,j))         
          z2=field_in(this%inter_index_x(i,j)+1,this%inter_index_y(i,j))     
          z3=field_in(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1) 
          z4=field_in(this%inter_index_x(i,j),this%inter_index_y(i,j)+1)     

          if (c_e(z1) .and. c_e(z2) .and. c_e(z3) .and. c_e(z4)) then 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! TODO ora utilizzo le latmin etc. ma il caso non è generale
! la getval in altri casi non mi restituisce niente e quindi bisognerà inventarsi
! un piano di proiezione X,Y (da 0. a 1. ?) a cui far riferimento
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            x1=this%inter_x(this%inter_index_x(i,j),this%inter_index_y(i,j))         
            y1=this%inter_y(this%inter_index_x(i,j),this%inter_index_y(i,j))     
            x3=this%inter_x(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1) 
            y3=this%inter_y(this%inter_index_x(i,j)+1,this%inter_index_y(i,j)+1)     
            
            xp=this%inter_xp(i,j)
            yp=this%inter_yp(i,j)
            
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            field_out(i,j) = hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp)

          end if
        end if

      ENDDO
    ENDDO

  else

    call l4f_category_log(this%category,L4F_ERROR,"sub_type not right here: "//this%trans%inter%sub_type)
    call raise_fatal_error("sub_type not right here")
    
  END IF

else

  call l4f_category_log(this%category,L4F_ERROR,"trans_type not right here: "//this%trans%trans_type)
  call raise_fatal_error("trans_type not right here")

ENDIF

END SUBROUTINE grid_transform_compute


!> \brief from input data vector compute the output data matrix
!! Grid_transform object contains any information needed for computation.
!! Field_out will bee computed.
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

