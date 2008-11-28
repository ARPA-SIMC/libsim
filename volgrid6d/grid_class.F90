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


TYPE grid_transform
  TYPE(griddim_def) :: griddim_in, griddim_out
  CHARACTER(len=80) :: type
  INTEGER :: intpar(20)
  DOUBLE PRECISION :: realpar(20)

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
  MODULE PROCEDURE init_griddim, grid_transform_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_griddim
END INTERFACE

INTERFACE get_val
  MODULE PROCEDURE get_val_griddim
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

PUBLIC griddim_proj,griddim_unproj,griddim_def,grid_def,grid_dim,grid_transform,init,delete
public get_val,write_unit,read_unit,import,export,display,compute
public operator(==),count_distinct,pack_distinct,map_distinct,map_inv_distinct,index
!public zoom_index,zoom_field
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




subroutine griddim_proj (this)

type(griddim_def) :: this

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

type(griddim_def) ::this

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

type(griddim_def) :: this

character(len=*),INTENT(out),OPTIONAL :: type
integer,optional,intent(out) :: nx, ny
doubleprecision,optional,intent(out) :: lon_min, lon_max, lat_min, lat_max
doubleprecision,optional,intent(out) :: latitude_south_pole,longitude_south_pole,angle_rotation
integer,optional,intent(out) :: component_flag

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
RECURSIVE SUBROUTINE grid_transform_init(this, griddim, trans_type, &
 ix, iy, fx, fy, ilon, ilat, flon, flat, &
 npx, npy, &
 griddim_out, v7d_out, & ! varmap?
 interp_type,categoryappend)
TYPE(grid_transform),INTENT(out) :: this !< griddim transformation object
TYPE(griddim_def),INTENT(in) :: griddim !< griddim to be transformed
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
TYPE(griddim_def),INTENT(inout),OPTIONAL :: griddim_out !< output griddim (for interp on a grid)
TYPE(vol7d),INTENT(inout),OPTIONAL :: v7d_out !< output data volume (for interp on sparse points)
CHARACTER(len=*),INTENT(IN),OPTIONAL :: interp_type !< type of interpolation (for interp), can be (nearest point, bilinear, ...?)

INTEGER :: nx, ny
DOUBLE PRECISION :: lon_min, lon_max, lat_min, lat_max, steplon, steplat
INTEGER :: lix, liy, lfx, lfy
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

character(len=512) :: a_name


call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

IF (trans_type == 'zoom') THEN
  IF (PRESENT(ilon) .AND. PRESENT(ilat) .AND. PRESENT(flon) &
   .AND. PRESENT(flat)) THEN ! coordinates given
    
!check
    if ( ilon > flon .or. ilat > flat ) then

      call l4f_category_log(this%category,L4F_ERROR,"zoom coordinates are wrong: ")
      call l4f_category_log(this%category,L4F_ERROR,to_char(ilon)//to_char(ilat))
      call l4f_category_log(this%category,L4F_ERROR,to_char(flon)//to_char(flat))
      call raise_fatal_error("zoom coordinates are wrong")
    end if

    select case ( griddim%grid%type%type )

    case ( "regular_ll")

      call zoom_coord(griddim%grid%regular_ll,griddim%dim, &
       ilon,ilat,flon,flat,lix,liy,lfx,lfy)

    case ( "rotated_ll")
      call zoom_coord(griddim%grid%rotated_ll,griddim%dim, &
       ilon,ilat,flon,flat,lix,liy,lfx,lfy)

    case default
      call l4f_category_log(this%category,L4F_ERROR,"gtype: "//trim(griddim%grid%type%type)//" non gestita" )
      call raise_fatal_error("gtype non gestita")

    end select

! use the index version
    CALL grid_transform_init(this, griddim, trans_type, ix=lix, iy=liy, fx=lfx, fy=lfy)
    RETURN
  ENDIF


! check
  IF (.NOT.PRESENT(ix) .OR. .NOT.PRESENT(iy) .OR. &
   .NOT.PRESENT(fx) .OR. .NOT.PRESENT(fy)) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,'zoom parameters ix, iy, fx, fy not provided')
    CALL raise_fatal_error('zoom parameters ix, iy, fx, fy not provided')
  ENDIF
  IF (ix > fx .OR. iy > fy) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,'invalid zoom indices: '//&
     to_char(ix)//to_char(iy)//to_char(fx)//to_char(fy))
    CALL raise_fatal_error('invalid zoom indices')
  ENDIF

  this%griddim_in = griddim
  this%griddim_out = griddim
  this%type = 'zoom'

  CALL get_val(griddim, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
   lat_min=lat_min, lat_max=lat_max)
  steplon=(lon_max-lon_min)/(nx-1)
  steplat=(lat_max-lat_min)/(ny-1)

! old indices
  this%intpar(1) = min(max(ix,1),nx) ! iox
  this%intpar(2) = min(max(iy,1),ny) ! ioy
  this%intpar(3) = max(min(fx,nx),1) ! fox
  this%intpar(4) = max(min(fy,ny),1) ! foy
! new indices
  this%intpar(5) = min(max(2-ix,1),nx)! inx
  this%intpar(6) = min(max(2-iy,1),ny) ! iny
  this%intpar(7) = min(fx,nx)-ix+1 ! fnx
  this%intpar(8) = min(fy,ny)-iy+1 ! fny

  this%griddim_out%dim%nx = fx - ix + 1 ! newx
  this%griddim_out%dim%ny = fy - iy + 1 ! newy

  lon_min=lon_min+steplon*(ix-1)
  lat_min=lat_min+steplat*(iy-1)
  lon_max=lon_max+steplon*(fx-nx)
  lat_max=lat_max+steplat*(fy-ny)

!  this%griddim_out%dim%nx = this%nx_out
!  this%griddim_out%dim%ny = this%ny_out

! da rifare, non va con le rotated_ll, come fare?
  this%griddim_out%grid%regular_ll%lon_min = lon_min
  this%griddim_out%grid%regular_ll%lon_max = lon_max
  this%griddim_out%grid%regular_ll%lat_min = lat_min
  this%griddim_out%grid%regular_ll%lat_max = lat_max

ELSE IF (trans_type == 'boxregrid') THEN
! check
  IF (.NOT.PRESENT(npx) .OR. .NOT.PRESENT(npy)) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,'boxregrid parameters npx, npy not provided')
    CALL raise_fatal_error('boxregrid parameters npx, npy not provided')
  ENDIF
  IF (npx <= 0 .OR. npy <= 0 .OR. npx > griddim%dim%nx .OR. npy > griddim%dim%ny) THEN
    CALL l4f_category_log(this%category,L4F_ERROR,'invalid regrid parameters: '//&
     TRIM(to_char(npx))//' '//TRIM(to_char(npy)))
    CALL raise_error('invalid regrid parameters')
  ENDIF

  this%griddim_in = griddim
  this%griddim_out = griddim
  this%type = 'boxregrid'

  CALL get_val(griddim, nx=nx, ny=ny, lon_min=lon_min, lon_max=lon_max, &
   lat_min=lat_min, lat_max=lat_max)

! old grid
  this%intpar(1) = npx
  this%intpar(2) = npy
  this%intpar(3) = nx
  this%intpar(4) = ny
  steplon=(lon_max-lon_min)/(nx-1)
  steplat=(lat_max-lat_min)/(ny-1)
! new grid
  this%griddim_out%grid%regular_ll%lon_min = lon_min + (npx - 1)*0.5D0*steplon
  this%griddim_out%grid%regular_ll%lat_min = lat_min + (npy - 1)*0.5D0*steplat
  this%griddim_out%dim%nx = nx/npx
  this%griddim_out%dim%ny = ny/npy
  steplon = steplon/npx
  steplat = steplat/npy
  this%griddim_out%grid%regular_ll%lon_max = &
   this%griddim_out%grid%regular_ll%lon_min + (this%griddim_out%dim%nx - 1)*steplon
  this%griddim_out%grid%regular_ll%lat_max = &
   this%griddim_out%grid%regular_ll%lat_min + (this%griddim_out%dim%ny - 1)*steplat
!  this%griddim_out%dim%nx = this%nx_out
!  this%griddim_out%dim%ny = this%ny_out

ELSE
  CALL l4f_category_log(this%category,L4F_WARN,'trans_type '//TRIM(trans_type) &
   //' not supported')
  CALL raise_warning('trans_type '//TRIM(trans_type)//' not supported')
  this%type = cmiss
ENDIF

END SUBROUTINE grid_transform_init


SUBROUTINE grid_transform_compute(this, field_in, field_out)
TYPE(grid_transform),INTENT(out) :: this
REAL, INTENT(in) :: field_in(:,:)
REAL, INTENT(out) :: field_out(:,:)

INTEGER :: i, j, ii, jj, ie, je, navg

field_out(:,:) = rmiss
! check size of field_in, field_out?

IF (this%type == 'zoom') THEN
  field_out(this%intpar(5):this%intpar(7), this%intpar(6):this%intpar(8)) = &
   field_in(this%intpar(1):this%intpar(3), this%intpar(2):this%intpar(4))

ELSE IF (this%type == 'boxregrid') THEN
  jj = 0
  DO j = 1, this%intpar(4) - this%intpar(2) + 1, this%intpar(2)
    je = j+this%intpar(2)-1
    jj = jj+1
    ii = 0
    DO i = 1, this%intpar(3) - this%intpar(3) + 1, this%intpar(3)
      ie = i+this%intpar(1)-1
      ii = ii+1
      navg = COUNT(field_in(i:ie,j:je) /= rmiss)
      IF (navg > 0) THEN
        field_out(ii,jj) = SUM(field_in(i:ie,j:je), &
         MASK=(field_in(i:ie,j:je) /= rmiss))/navg
      ENDIF
    ENDDO
  ENDDO
ENDIF

END SUBROUTINE grid_transform_compute

end module grid_class
