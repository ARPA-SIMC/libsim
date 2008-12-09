!>\brief  classe per la gestione di volumi di dati regolari (gridded)
!!
!!Questo modulo definisce gli oggetti e i metodi per gestire
!!l'importazione e l'esportazione di volumi regolari (gridded) 
!!e della loro gestione nei sistemi di coordinate geografiche e proiezioni


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

type grid_type

  character(len=80) :: type

end type grid_type


!>\brief definizione del grigliato in genere
type grid_def

  private

  type(grid_type)   :: type
  type(grid_regular_ll) :: regular_ll
  type(grid_rotated_ll) :: rotated_ll

  integer :: category !< log4fortran

end type grid_def



!>\brief definizione del grigliato in genere e delle sue dimensioni
type griddim_def


  type(grid_def)   :: grid
  type(grid_dim)   :: dim

  integer :: category !< log4fortran

end type griddim_def



!>  subtype nearest information
type inter_near
  logical :: external !< enable external elaboration
end type inter_near

!>  interpolation information 
type inter
  CHARACTER(len=80) :: sub_type !< subtype of transformation, can be \c 'near'
  type(inter_near) :: near !< subtype nearest information
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
  type(zoom_coo) :: coord !< zoom priding coordinates
end type zoom

!> boxregrid subtype average information
type boxregrid_average
INTEGER :: npx !< number of points to average along x direction
INTEGER :: npy !< number of points to average along y direction

end type boxregrid_average

!> boxregrid  information
type boxregrid
  CHARACTER(len=80) :: sub_type !< subtype of transformation, can be \c 'average'
  type(boxregrid_average) :: average
end type boxregrid

TYPE transform

  private

  CHARACTER(len=80) :: trans_type !< type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'interp', ...
  type(zoom) :: zoom
  type(boxregrid) :: boxregrid
  type(inter) :: inter
!  type(interp) :: interp

  integer :: category !< log4fortran

END TYPE transform


TYPE grid_transform

  private

  TYPE(transform) :: trans

  integer :: innx,  inny
  integer :: outnx, outny
  integer :: iniox,inioy,infox,infoy,outinx,outiny,outfnx,outfny
  
  integer,pointer :: inter_index_x_near(:),inter_index_y_near(:)

  integer :: category !< log4fortran

END TYPE grid_transform




!> Operatore logico di uguaglianza tra oggetti della classe grid.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE grid_eq, grid_type_eq,griddim_eq
END INTERFACE


INTERFACE init
  MODULE PROCEDURE init_griddim, init_grid_transform,init_transform
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_griddim, delete_grid_transform, delete_transform
END INTERFACE

INTERFACE proj
  MODULE PROCEDURE generic_proj
END INTERFACE

INTERFACE unproj
  MODULE PROCEDURE generic_unproj
END INTERFACE

INTERFACE get_val
  MODULE PROCEDURE get_val_griddim
END INTERFACE

INTERFACE set_val
  MODULE PROCEDURE set_val_griddim
END INTERFACE

INTERFACE write_unit
  MODULE PROCEDURE write_unit_griddim
END INTERFACE

INTERFACE read_unit
  MODULE PROCEDURE read_unit_griddim
END INTERFACE

INTERFACE import
  MODULE PROCEDURE import_griddim
END INTERFACE

INTERFACE export
  MODULE PROCEDURE export_griddim
END INTERFACE

INTERFACE display
  MODULE PROCEDURE display_griddim
END INTERFACE

INTERFACE compute
  MODULE PROCEDURE grid_transform_compute
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



private

PUBLIC proj, unproj, griddim_proj,griddim_unproj,griddim_def,grid_def,grid_dim
public init,delete
public get_val,set_val,write_unit,read_unit,import,export,display,compute
public operator(==),count_distinct,pack_distinct,map_distinct,map_inv_distinct,index
public transform,grid_transform
contains



subroutine init_griddim(this,type,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,angle_rotation, &
 categoryappend)

type(griddim_def) :: this

character(len=*),INTENT(in),OPTIONAL :: type
integer,optional :: nx, ny
doubleprecision,optional :: lon_min, lon_max, lat_min, lat_max
doubleprecision,optional :: latitude_south_pole,longitude_south_pole,angle_rotation
integer,optional :: component_flag
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

character(len=512) :: a_name


call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

call init(this%grid%regular_ll,this%dim)
call init(this%grid%rotated_ll,this%dim)

if (present(type))then
  this%grid%type%type=type
else
  this%grid%type%type=cmiss

  return

end if


call l4f_category_log(this%category,L4F_DEBUG,"init gtype: "//this%grid%type%type )

select case ( this%grid%type%type)

case ( "regular_ll")
  call init(this%grid%regular_ll,this%dim,&
   nx,ny, &
   lon_min, lon_max, lat_min, lat_max, component_flag, &
   categoryappend=trim(subcategory)//"."//trim(categoryappend))

case ( "rotated_ll")
  call init(this%grid%rotated_ll,this%dim,&
   nx,ny, &
   lon_min, lon_max, lat_min, lat_max, component_flag, &
   latitude_south_pole,longitude_south_pole,angle_rotation, &
   categoryappend=trim(subcategory)//"."//trim(categoryappend))
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

end select


end subroutine init_griddim


subroutine delete_griddim(this)
type(griddim_def) :: this

select case ( this%grid%type%type)

case ( "regular_ll")
  call delete(this%grid%regular_ll,this%dim)

case ( "rotated_ll")
  call delete(this%grid%rotated_ll,this%dim)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

end select

!chiudo il logger
call l4f_category_delete(this%category)


end subroutine delete_griddim



elemental subroutine generic_proj (this,lon,lat,x,y)

type(griddim_def),intent(in) :: this
doubleprecision, intent(in) :: lon,lat
doubleprecision, intent(out)  :: x,y


select case ( this%grid%type%type)

case ( "regular_ll")
  call proj(this%grid%regular_ll,lon,lat,x,y)

case ( "rotated_ll")
  call proj(this%grid%rotated_ll,lon,lat,x,y)
  
case default
!  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
!  call raise_error("gtype non gestita")

end select

end subroutine generic_proj


elemental subroutine generic_unproj (this,x,y,lon,lat)

type(griddim_def),intent(in) ::this
doubleprecision, intent(in) :: x,y
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



subroutine griddim_proj (this)

type(griddim_def),intent(in) :: this

select case ( this%grid%type%type)

case ( "regular_ll")
  call grid_proj(this%grid%regular_ll,this%dim)

case ( "rotated_ll")
  call grid_proj(this%grid%rotated_ll,this%dim)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

end select

end subroutine griddim_proj


subroutine griddim_unproj (this)

type(griddim_def),intent(in) ::this

select case ( this%grid%type%type)

case ( "regular_ll")
  call grid_unproj(this%grid%regular_ll, this%dim)

case ( "rotated_ll")
  call grid_unproj(this%grid%rotated_ll, this%dim)

case default
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

end select

end subroutine griddim_unproj


subroutine get_val_griddim(this,type,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,angle_rotation)

type(griddim_def),intent(in) :: this

character(len=*),INTENT(out),OPTIONAL :: type
integer,optional,intent(out) :: nx, ny
doubleprecision,optional,intent(out) :: lon_min, lon_max, lat_min, lat_max
doubleprecision,optional,intent(out) :: latitude_south_pole,longitude_south_pole,angle_rotation
integer,optional,intent(out) :: component_flag


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
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

end select


end subroutine get_val_griddim


subroutine set_val_griddim(this,type,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,angle_rotation)

type(griddim_def),intent(out) :: this

character(len=*),INTENT(in),OPTIONAL :: type
integer,optional,intent(in) :: nx, ny
doubleprecision,optional,intent(in) :: lon_min, lon_max, lat_min, lat_max
doubleprecision,optional,intent(in) :: latitude_south_pole,longitude_south_pole,angle_rotation
integer,optional,intent(in) :: component_flag

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
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

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
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

end select

call write_unit(this%dim,unit)


END SUBROUTINE write_unit_griddim



SUBROUTINE import_griddim(this, gaid) 

type(griddim_def),intent(out) :: this !< oggetto griddim
INTEGER, INTENT(in) :: gaid !< grib_api id da cui leggere

call grib_get(gaid,'typeOfGrid' ,this%grid%type%type)
call l4f_category_log(this%category,L4F_DEBUG,"gtype: "//this%grid%type%type)

select case ( this%grid%type%type)

case ( "regular_ll")
  call import(this%grid%regular_ll,this%dim,gaid)

case ( "rotated_ll")
  call import(this%grid%rotated_ll,this%dim,gaid)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"gtype non gestita: "//trim(this%grid%type%type))
  call raise_error("gtype non gestita")

end select

END SUBROUTINE import_griddim


SUBROUTINE export_griddim(this, gaid) 

type(griddim_def),intent(out) :: this !< oggetto griddim
INTEGER, INTENT(in) :: gaid !< grib_api id da cui leggere


select case ( this%grid%type%type)

case ( "regular_ll")
  call export(this%grid%regular_ll,this%dim,gaid)

case ( "rotated_ll")
  call export(this%grid%rotated_ll,this%dim,gaid)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

end select

END SUBROUTINE export_griddim


! TODO
! bisogna sviluppare gli altri operatori


elemental FUNCTION grid_eq(this, that) RESULT(res)
TYPE(grid_def),INTENT(IN) :: this, that
LOGICAL :: res

res = this%type == that%type .and. &
 this%regular_ll == that%regular_ll .and. &
 this%rotated_ll == that%rotated_ll

END FUNCTION grid_eq


elemental FUNCTION griddim_eq(this, that) RESULT(res)
TYPE(griddim_def),INTENT(IN) :: this, that
LOGICAL :: res

res = this%grid == that%grid .and. &
 this%dim == that%dim

END FUNCTION griddim_eq



elemental FUNCTION grid_type_eq(this, that) RESULT(res)
TYPE(grid_type),INTENT(IN) :: this, that
LOGICAL :: res

res = this%type == that%type

END FUNCTION grid_type_eq



SUBROUTINE display_griddim(this) 

type(griddim_def),intent(in) :: this !< oggetto griddim

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
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")

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





!!$SUBROUTINE zoom_coord(this,ilon,ilat,flon,flat,newx,newy) 
!!$
!!$type(griddim_def),intent(in) :: this !< oggetto griddim
!!$doubleprecision,intent(in) ::ilon,ilat,flon,flat !< zoom geographical coordinate
!!$integer, intent(out):: newx,newy  !< new dimension for the future field
!!$
!!$!check
!!$
!!$if ( ilon > flon .or. ilat > flat ) then
!!$    
!!$  call l4f_category_log(this%category,L4F_ERROR,"zoom coordinate are wrong: "//&
!!$   to_char(ilon)//to_char(ilat)//to_char(flon)//to_char(flat))
!!$  call raise_error("zoom coordinate are wrong")
!!$end if
!!$
!!$
!!$
!!$select case ( this%grid%type%type )
!!$
!!$case ( "regular_ll")
!!$
!!$  call zoom(this%grid%regular_ll,this%dim,ilon,ilat,flon,flat,newx,newy)
!!$  
!!$case ( "rotated_ll")
!!$  call zoom(this%grid%rotated_ll,this%dim,ilon,ilat,flon,flat,newx,newy)
!!$  
!!$case default
!!$  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
!!$  call raise_error("gtype non gestita")
!!$  
!!$end select
!!$
!!$
!!$end SUBROUTINE zoom_coord





!> Initialises an object that defines a transformation on a grid.
!! trans_type='zoom' cuts or extends \a grid on a new grid adding
!! or removing points on the four sides (zoom).
!! trans_type='box_regid' regrids \a grid on a new grid in which
!! every point is the average over \a npx X \a npy points of the
!! original grid (box average).
!! All the proper optional parameters, after \a trans_type, should
!! be passed in keyword mode.
SUBROUTINE init_transform(this, trans_type, &
 ix, iy, fx, fy, ilon, ilat, flon, flat, &
 npx, npy, &
 zoom_type,boxregrid_type,inter_type,external,categoryappend)

TYPE(transform),INTENT(out) :: this !< transformation object
CHARACTER(len=*) :: trans_type !< type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'interp', ...
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
logical,INTENT(IN),OPTIONAL :: external !< activate external area interpolation (for interpolation nearest)
CHARACTER(len=*),INTENT(IN),OPTIONAL :: zoom_type !< type of zoom
CHARACTER(len=*),INTENT(IN),OPTIONAL :: boxregrid_type !< type of regrid
CHARACTER(len=*),INTENT(IN),OPTIONAL :: inter_type !< type of interpolation

character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

call get_opt(trans_type,this%trans_type)

call get_opt(zoom_type,this%zoom%sub_type)

call get_opt(ix,this%zoom%index%ix)
call get_opt(iy,this%zoom%index%iy)
call get_opt(fx,this%zoom%index%fx)
call get_opt(fy,this%zoom%index%fy)

call get_opt(ilon,this%zoom%coord%ilon)
call get_opt(ilat,this%zoom%coord%ilat)
call get_opt(flon,this%zoom%coord%flon)
call get_opt(flat,this%zoom%coord%flat)


call get_opt(boxregrid_type,this%boxregrid%sub_type)

call get_opt(npx,this%boxregrid%average%npx)
call get_opt(npy,this%boxregrid%average%npy)


call get_opt(inter_type,this%inter%sub_type)
call get_opt(external,this%inter%near%external)


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

  if (this%boxregrid%sub_type == 'average')then

    IF ( c_e(this%boxregrid%average%npx) .and. c_e(this%boxregrid%average%npy)) THEN

                                ! check
      IF (this%boxregrid%average%npx <= 0 .OR. this%boxregrid%average%npy <= 0 ) THEN
        CALL l4f_category_log(this%category,L4F_ERROR,'invalid regrid parameters: '//&
         TRIM(to_char(this%boxregrid%average%npx))//' '//TRIM(to_char(this%boxregrid%average%npy)))
        CALL raise_error('invalid regrid parameters')
      ENDIF
          

    else

      CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid parameters npx, npy not provided')
      CALL raise_fatal_error('boxregrid parameters npx, npy not provided')

    end if

  else

    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid: sub_type is wrong')
    CALL raise_fatal_error('boxregrid: sub_type is wrong')
    
  end if


else if (this%trans_type == 'inter') then

  if (this%inter%sub_type == 'near')then

!!$    IF ( c_e(this%boxregrid%average%npx) .and. c_e(this%boxregrid%average%npy)) THEN
!!$
!!$                                ! check
!!$      IF (this%boxregrid%average%npx <= 0 .OR. this%boxregrid%average%npy <= 0 ) THEN
!!$        CALL l4f_category_log(this%category,L4F_ERROR,'invalid regrid parameters: '//&
!!$         TRIM(to_char(this%boxregrid%average%npx))//' '//TRIM(to_char(this%boxregrid%average%npy)))
!!$        CALL raise_error('invalid regrid parameters')
!!$      ENDIF
!!$          
!!$
!!$    else
!!$
!!$      CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid parameters npx, npy not provided')
!!$      CALL raise_fatal_error('boxregrid parameters npx, npy not provided')
!!$
!!$    end if

  else

    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid: sub_type is wrong')
    CALL raise_fatal_error('boxregrid: sub_type is wrong')
    
  end if

else

  CALL l4f_category_log(this%category,L4F_ERROR,'trans_type is wrong')
  CALL raise_fatal_error('trans_type is wrong')

end IF


end SUBROUTINE init_transform




SUBROUTINE delete_transform(this)

TYPE(transform),INTENT(out) :: this !< transformation object

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

this%boxregrid%average%npx=imiss
this%boxregrid%average%npy=imiss

this%inter%sub_type=cmiss

this%inter%near%external=.false.

!chiudo il logger
call l4f_category_delete(this%category)


end SUBROUTINE delete_transform


!> Initialises an object that defines a transformation on a grid.
!! trans_type='zoom' cuts or extends \a grid on a new grid adding
!! or removing points on the four sides (zoom).
!! trans_type='box_regid' regrids \a grid on a new grid in which
!! every point is the average over \a npx X \a npy points of the
!! original grid (box average).
!! All the proper optional parameters, after \a trans_type, should
!! be passed in keyword mode.

SUBROUTINE init_grid_transform(this,trans,in,out,categoryappend)

TYPE(grid_transform),INTENT(out) :: this !< grid transformation object
TYPE(transform),INTENT(in) :: trans !< transformation object
TYPE(griddim_def),INTENT(in) :: in !< grid transformated object
TYPE(griddim_def),INTENT(out) :: out !< grid transformated object


INTEGER :: nx, ny,i,j
DOUBLE PRECISION :: lon_min, lon_max, lat_min, lat_max, steplon, steplat,lon_min_new, lat_min_new


character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

this%trans=trans

nullify (this%inter_index_x_near)
nullify (this%inter_index_y_near)


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

      call l4f_category_log(this%category,L4F_ERROR,"gtype: "//trim(in%grid%type%type)//" non gestita" )
      call raise_fatal_error("gtype non gestita")
      
    end select

    this%trans%zoom%sub_type = 'index'
    
  end if


  if (this%trans%zoom%sub_type == 'index') THEN

    select case ( in%grid%type%type )

    case ( "regular_ll","rotated_ll")

      CALL get_val(in, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
       lat_min=lat_min, lat_max=lat_max)

      steplon=(lon_max-lon_min)/(nx-1)
      steplat=(lat_max-lat_min)/(ny-1)

      
    case default
      call l4f_category_log(this%category,L4F_ERROR,"gtype: "//trim(in%grid%type%type)//" non gestita" )
      call raise_fatal_error("gtype non gestita")
      
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

! TODO verificare che questa copia non copi puntatori facendo casini
    out=in
    
    out%dim%nx = this%trans%zoom%index%fx - this%trans%zoom%index%ix + 1 ! newx
    out%dim%ny = this%trans%zoom%index%fy - this%trans%zoom%index%iy + 1 ! newy

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
    call l4f_category_log(this%category,L4F_ERROR,"gtype: "//trim(in%grid%type%type)//" non gestita" )
    call raise_fatal_error("gtype non gestita")
    
  end select


! old grid
!  this%intpar(1) = this%trans%boxregrid%average%npx
!  this%intpar(2) = this%trans%boxregrid%average%npy
!  this%intpar(3) = nx
!  this%intpar(4) = ny

  this%innx = nx
  this%inny = ny

  steplon=(lon_max-lon_min)/(nx-1)
  steplat=(lat_max-lat_min)/(ny-1)

! new grid
  lon_min_new = lon_min + (this%trans%boxregrid%average%npx - 1)*0.5D0*steplon
  lat_min_new = lat_min + (this%trans%boxregrid%average%npy - 1)*0.5D0*steplat

  out%dim%nx = nx/this%trans%boxregrid%average%npx
  out%dim%ny = ny/this%trans%boxregrid%average%npy

  call set_val( out,lon_min = lon_min_new )
  call set_val( out,lat_min = lat_min_new )

  steplon = steplon/this%trans%boxregrid%average%npx
  steplat = steplat/this%trans%boxregrid%average%npy

  call set_val( out, lon_max = lon_min_new + (out%dim%nx - 1)*steplon )
  call set_val( out, lat_max = lat_min_new + (out%dim%ny - 1)*steplat )


  else

    CALL l4f_category_log(this%category,L4F_WARN,'trans_type '//TRIM(this%trans%boxregrid%sub_type) &
     //' not supported')
    CALL raise_warning('trans_type '//TRIM(this%trans%boxregrid%sub_type)//' not supported')

  end if

ELSE IF (this%trans%trans_type == 'inter') THEN

  if (this%trans%inter%sub_type == 'near') THEN

  select case ( in%grid%type%type )

  case ( "regular_ll","rotated_ll")

    CALL get_val(in, nx=nx, ny=ny )
    this%innx=nx
    this%inny=ny

    CALL get_val(out, nx=nx, ny=ny )
    this%outnx=nx
    this%outny=ny

!    if (associated(this%inter_index_x_near)) deallocate (this%inter_index_x_near)
!    if (associated(this%inter_index_y_near)) deallocate (this%inter_index_y_near)
    allocate (this%inter_index_x_near(nx),this%inter_index_y_near(ny))

    do i=1,this%outnx
      do j=1,this%outny

        call interpolation(in,this%trans%inter%sub_type,out%dim%lon(i,j),out%dim%lat(i,j),&
         index_x=this%inter_index_x_near(i),index_y=this%inter_index_y_near(j))
        
      end do
    end do
    
  case default
    call l4f_category_log(this%category,L4F_ERROR,"gtype: "//trim(in%grid%type%type)//" non gestita" )
    call raise_fatal_error("gtype non gestita")
    
  end select


  end if

ELSE

  CALL l4f_category_log(this%category,L4F_WARN,'trans type '//TRIM(this%trans%trans_type) &
   //' not supported')
  CALL raise_warning('trans type '//TRIM(this%trans%trans_type)//' not supported')

ENDIF

END SUBROUTINE init_grid_transform




SUBROUTINE delete_grid_transform(this)

TYPE(grid_transform),INTENT(inout) :: this !< grid transformation object

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

if (associated(this%inter_index_x_near)) deallocate (this%inter_index_x_near)
if (associated(this%inter_index_y_near)) deallocate (this%inter_index_y_near)

!chiudo il logger
call l4f_category_delete(this%category)

end SUBROUTINE delete_grid_transform

!!$!> Initialises an object that defines a transformation on a grid.
!!$!! trans_type='zoom' cuts or extends \a grid on a new grid adding
!!$!! or removing points on the four sides (zoom).
!!$!! trans_type='box_regid' regrids \a grid on a new grid in which
!!$!! every point is the average over \a npx X \a npy points of the
!!$!! original grid (box average).
!!$!! All the proper optional parameters, after \a trans_type, should
!!$!! be passed in keyword mode.
!!$RECURSIVE SUBROUTINE init_grid_transform(this, griddim, trans_type, &
!!$ ix, iy, fx, fy, ilon, ilat, flon, flat, &
!!$ npx, npy, &
!!$ griddim_out, v7d_out, & ! varmap?
!!$ interp_type,categoryappend)
!!$TYPE(grid_transform),INTENT(out) :: this !< griddim transformation object
!!$TYPE(griddim_def),INTENT(in) :: griddim !< griddim to be transformed
!!$CHARACTER(len=*) :: trans_type !< type of transformation, can be \c 'zoom', \c 'boxregrid', \c 'interp', ...
!!$INTEGER,INTENT(in),OPTIONAL :: ix !< index of initial point of new grid on x (for zoom)
!!$INTEGER,INTENT(in),OPTIONAL :: iy !< index of initial point of new grid on y (for zoom)
!!$INTEGER,INTENT(in),OPTIONAL :: fx !< index of final point of new grid on x (for zoom)
!!$INTEGER,INTENT(in),OPTIONAL :: fy !< index of final point of new grid on y (for zoom)
!!$DOUBLEPRECISION,INTENT(in),OPTIONAL :: ilon !< coordinate of initial point of new grid on x (for zoom)
!!$DOUBLEPRECISION,INTENT(in),OPTIONAL :: ilat !< coordinate of initial point of new grid on y (for zoom)
!!$DOUBLEPRECISION,INTENT(in),OPTIONAL :: flon !< coordinate of final point of new grid on x (for zoom)
!!$DOUBLEPRECISION,INTENT(in),OPTIONAL :: flat !< coordinate of final point of new grid on y (for zoom)
!!$INTEGER,INTENT(IN),OPTIONAL :: npx !< number of points to average along x direction (for boxregrid)
!!$INTEGER,INTENT(IN),OPTIONAL :: npy !< number of points to average along y direction (for boxregrid)
!!$TYPE(griddim_def),INTENT(inout),OPTIONAL :: griddim_out !< output griddim (for interp on a grid)
!!$TYPE(vol7d),INTENT(inout),OPTIONAL :: v7d_out !< output data volume (for interp on sparse points)
!!$CHARACTER(len=*),INTENT(IN),OPTIONAL :: interp_type !< type of interpolation (for interp), can be (nearest point, bilinear, ...?)
!!$
!!$INTEGER :: nx, ny
!!$DOUBLE PRECISION :: lon_min, lon_max, lat_min, lat_max, steplon, steplat,lon_min_new, lat_min_new
!!$INTEGER :: lix, liy, lfx, lfy
!!$character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
!!$
!!$character(len=512) :: a_name
!!$
!!$
!!$call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
!!$this%category=l4f_category_get(a_name)
!!$
!!$IF (trans_type == 'zoom') THEN
!!$  IF (PRESENT(ilon) .AND. PRESENT(ilat) .AND. PRESENT(flon) &
!!$   .AND. PRESENT(flat)) THEN ! coordinates given
!!$    
!!$!check
!!$    if ( ilon > flon .or. ilat > flat ) then
!!$
!!$      call l4f_category_log(this%category,L4F_ERROR,"zoom coordinates are wrong: ")
!!$      call l4f_category_log(this%category,L4F_ERROR,to_char(ilon)//to_char(ilat))
!!$      call l4f_category_log(this%category,L4F_ERROR,to_char(flon)//to_char(flat))
!!$      call raise_fatal_error("zoom coordinates are wrong")
!!$    end if
!!$
!!$    select case ( griddim%grid%type%type )
!!$
!!$    case ( "regular_ll")
!!$
!!$      call zoom_coord(griddim%grid%regular_ll,griddim%dim, &
!!$       ilon,ilat,flon,flat,lix,liy,lfx,lfy)
!!$
!!$    case ( "rotated_ll")
!!$      call zoom_coord(griddim%grid%rotated_ll,griddim%dim, &
!!$       ilon,ilat,flon,flat,lix,liy,lfx,lfy)
!!$
!!$    case default
!!$      call l4f_category_log(this%category,L4F_ERROR,"gtype: "//trim(griddim%grid%type%type)//" non gestita" )
!!$      call raise_fatal_error("gtype non gestita")
!!$
!!$    end select
!!$
!!$! chiama se stessa con i parametri appena calcolati ed esce
!!$! use the index version
!!$    CALL init(this, griddim, trans_type, ix=lix, iy=liy, fx=lfx, fy=lfy)
!!$    RETURN
!!$  ENDIF
!!$
!!$
!!$! check
!!$  IF (.NOT.PRESENT(ix) .OR. .NOT.PRESENT(iy) .OR. &
!!$   .NOT.PRESENT(fx) .OR. .NOT.PRESENT(fy)) THEN
!!$    CALL l4f_category_log(this%category,L4F_ERROR,'zoom parameters ix, iy, fx, fy not provided')
!!$    CALL raise_fatal_error('zoom parameters ix, iy, fx, fy not provided')
!!$  ENDIF
!!$  IF (ix > fx .OR. iy > fy) THEN
!!$    CALL l4f_category_log(this%category,L4F_ERROR,'invalid zoom indices: '//&
!!$     to_char(ix)//to_char(iy)//to_char(fx)//to_char(fy))
!!$    CALL raise_fatal_error('invalid zoom indices')
!!$  ENDIF
!!$
!!$  this%griddim_in = griddim
!!$  this%griddim_out = griddim
!!$  this%type = 'zoom'
!!$
!!$
!!$  select case ( griddim%grid%type%type )
!!$
!!$  case ( "regular_ll","rotated_ll")
!!$      
!!$    CALL get_val(griddim, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
!!$     lat_min=lat_min, lat_max=lat_max)
!!$    steplon=(lon_max-lon_min)/(nx-1)
!!$    steplat=(lat_max-lat_min)/(ny-1)
!!$    
!!$  case default
!!$    call l4f_category_log(this%category,L4F_ERROR,"gtype: "//trim(griddim%grid%type%type)//" non gestita" )
!!$    call raise_fatal_error("gtype non gestita")
!!$    
!!$  end select
!!$
!!$
!!$! old indices
!!$  this%intpar(1) = min(max(ix,1),nx) ! iox
!!$  this%intpar(2) = min(max(iy,1),ny) ! ioy
!!$  this%intpar(3) = max(min(fx,nx),1) ! fox
!!$  this%intpar(4) = max(min(fy,ny),1) ! foy
!!$! new indices
!!$  this%intpar(5) = min(max(2-ix,1),nx)! inx
!!$  this%intpar(6) = min(max(2-iy,1),ny) ! iny
!!$  this%intpar(7) = min(fx,nx)-ix+1 ! fnx
!!$  this%intpar(8) = min(fy,ny)-iy+1 ! fny
!!$
!!$  this%griddim_out%dim%nx = fx - ix + 1 ! newx
!!$  this%griddim_out%dim%ny = fy - iy + 1 ! newy
!!$
!!$  lon_min=lon_min+steplon*(ix-1)
!!$  lat_min=lat_min+steplat*(iy-1)
!!$  lon_max=lon_max+steplon*(fx-nx)
!!$  lat_max=lat_max+steplat*(fy-ny)
!!$
!!$!  this%griddim_out%dim%nx = this%nx_out
!!$!  this%griddim_out%dim%ny = this%ny_out
!!$
!!$
!!$  call set_val (this%griddim_out,&
!!$   lon_min = lon_min,  lon_max = lon_max ,&
!!$   lat_min = lat_min,  lat_max = lat_max )
!!$
!!$
!!$ELSE IF (trans_type == 'boxregrid') THEN
!!$! check
!!$  IF (.NOT.PRESENT(npx) .OR. .NOT.PRESENT(npy)) THEN
!!$    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid parameters npx, npy not provided')
!!$    CALL raise_fatal_error('boxregrid parameters npx, npy not provided')
!!$  ENDIF
!!$  IF (npx <= 0 .OR. npy <= 0 .OR. npx > griddim%dim%nx .OR. npy > griddim%dim%ny) THEN
!!$    CALL l4f_category_log(this%category,L4F_ERROR,'invalid regrid parameters: '//&
!!$     TRIM(to_char(npx))//' '//TRIM(to_char(npy)))
!!$    CALL raise_error('invalid regrid parameters')
!!$  ENDIF
!!$
!!$  this%griddim_in = griddim
!!$  this%griddim_out = griddim
!!$  this%type = 'boxregrid'
!!$
!!$
!!$  select case ( griddim%grid%type%type )
!!$
!!$  case ( "regular_ll","rotated_ll")
!!$      
!!$  CALL get_val(griddim, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
!!$   lat_min=lat_min, lat_max=lat_max)
!!$    
!!$  case default
!!$    call l4f_category_log(this%category,L4F_ERROR,"gtype: "//trim(griddim%grid%type%type)//" non gestita" )
!!$    call raise_fatal_error("gtype non gestita")
!!$    
!!$  end select
!!$
!!$
!!$! old grid
!!$  this%intpar(1) = npx
!!$  this%intpar(2) = npy
!!$  this%intpar(3) = nx
!!$  this%intpar(4) = ny
!!$  steplon=(lon_max-lon_min)/(nx-1)
!!$  steplat=(lat_max-lat_min)/(ny-1)
!!$
!!$! new grid
!!$  lon_min_new = lon_min + (npx - 1)*0.5D0*steplon
!!$  lat_min_new = lat_min + (npy - 1)*0.5D0*steplat
!!$
!!$  call set_val( this%griddim_out,lon_min = lon_min_new )
!!$  call set_val( this%griddim_out,lat_min = lat_min_new )
!!$  this%griddim_out%dim%nx = nx/npx
!!$  this%griddim_out%dim%ny = ny/npy
!!$  steplon = steplon/npx
!!$  steplat = steplat/npy
!!$
!!$  call set_val( this%griddim_out, lon_max = lon_min_new + (this%griddim_out%dim%nx - 1)*steplon )
!!$  call set_val( this%griddim_out, lat_max = lat_min_new + (this%griddim_out%dim%ny - 1)*steplat )
!!$!  this%griddim_out%dim%nx = this%nx_out
!!$!  this%griddim_out%dim%ny = this%ny_out
!!$
!!$ELSE
!!$  CALL l4f_category_log(this%category,L4F_WARN,'trans_type '//TRIM(trans_type) &
!!$   //' not supported')
!!$  CALL raise_warning('trans_type '//TRIM(trans_type)//' not supported')
!!$  this%type = cmiss
!!$ENDIF
!!$
!!$END SUBROUTINE init_grid_transform
!!$

SUBROUTINE grid_transform_compute(this, field_in, field_out)
TYPE(grid_transform),INTENT(out) :: this
REAL, INTENT(in) :: field_in(:,:)
REAL, INTENT(out) :: field_out(:,:)

INTEGER :: i, j, ii, jj, ie, je, navg


! check size of field_in, field_out

if (any(shape(field_in) /= (/this%innx,this%inny/))) then

  call l4f_category_log(this%category,L4F_ERROR,"inconsistent shape: "//to_char(this%innx))
  call raise_fatal_error("inconsistent shape")
end if

if (any(shape(field_out) /= (/this%outnx,this%outny/))) then
  call l4f_category_log(this%category,L4F_ERROR,"inconsistent shape: "//to_char(this%inny))
  call raise_fatal_error("inconsistent shape")
end if


field_out(:,:) = rmiss

IF (this%trans%trans_type == 'zoom') THEN

  field_out(this%outinx:this%outfnx, &
   this%outiny:this%outfny) = &
   field_in(this%iniox:this%infox, &
   this%inioy:this%infoy)

ELSE IF (this%trans%trans_type == 'boxregrid') THEN

  jj = 0
  DO j = 1, this%inny - this%trans%boxregrid%average%npy + 1, this%trans%boxregrid%average%npy
    je = j+this%trans%boxregrid%average%npy-1
    jj = jj+1
    ii = 0
    DO i = 1, this%innx - this%innx + 1, this%innx
      ie = i+this%trans%boxregrid%average%npx-1
      ii = ii+1
      navg = COUNT(field_in(i:ie,j:je) /= rmiss)
      IF (navg > 0) THEN
        field_out(ii,jj) = SUM(field_in(i:ie,j:je)/navg, &
         MASK=(field_in(i:ie,j:je) /= rmiss))
      ENDIF
    ENDDO
  ENDDO

ELSE IF (this%trans%trans_type == 'inter') THEN

  DO j = 1, this%inny 
    DO i = 1, this%innx 

        field_out(i,j) = field_in(this%inter_index_x_near(i),this%inter_index_y_near(j))

    ENDDO
  ENDDO

ENDIF

END SUBROUTINE grid_transform_compute




subroutine interpolation(this,inter_type,lon,lat,index_x,index_y)

type(griddim_def),intent(in) :: this
character(len=*),intent(in) :: inter_type
doubleprecision,intent(in) :: lon,lat
integer,optional,intent(out) :: index_x,index_y

doubleprecision :: x,y,lon_min, lon_max, lat_min, lat_max
integer :: nx,ny

if (inter_type == "near") then

  call proj(this,lon,lat,x,y)

  call get_val(this,nx=nx,ny=ny, lon_min=lon_min, lon_max=lon_max,&
   lat_min=lat_min, lat_max=lat_max)

  if (present(index_x))then
    index_x=nint((x-lon_min)/(lon_max-lon_min)/nx)
  else
    index_x=imiss
  end if

  if (present(index_y))then
    index_y=nint((y-lat_min)/(lat_max-lat_min)/ny)
  else
    index_y=imiss
  end if

else

  index_x=imiss
  index_y=imiss

  call l4f_category_log(this%category,L4F_ERROR,"inter_type not supported: "//trim(inter_type))
  call raise_fatal_error("inter_type not supported")

end if

end subroutine interpolation


end module grid_class
