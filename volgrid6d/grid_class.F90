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



!> Operatore logico di uguaglianza tra oggetti della classe grid.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE grid_eq, grid_type_eq,griddim_eq
END INTERFACE


INTERFACE init
  MODULE PROCEDURE init_griddim
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

INTERFACE zoom_coord
  MODULE PROCEDURE zoom_coord_grid
END INTERFACE



private

public griddim_proj,griddim_unproj,griddim_def,grid_def,grid_dim,init,delete
public get_val,write_unit,read_unit,import,export,display
public operator(==),count_distinct,pack_distinct,map_distinct,map_inv_distinct,index
public zoom_coord,zoom_index,zoom_field
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





SUBROUTINE zoom_coord_grid(this,ilon,ilat,flon,flat,ix,iy,fx,fy) 

type(griddim_def),intent(in) :: this !< oggetto griddim
doubleprecision,intent(in) ::ilon,ilat,flon,flat !< zoom geographical coordinate
integer, intent(out):: ix,iy,fx,fy  !< new index coordinate for the future field

!check

if ( ilon > flon .or. ilat > flat ) then
    
  call l4f_category_log(this%category,L4F_ERROR,"zoom coordinate are wrong: "//&
   to_char(ilon)//to_char(ilat)//to_char(flon)//to_char(flat))
  call raise_error("zoom coordinate are wrong")
end if


select case ( this%grid%type%type )

case ( "regular_ll")

  call zoom_coord(this%grid%regular_ll,this%dim,ilon,ilat,flon,flat,ix,iy,fx,fy)
  
case ( "rotated_ll")
  call zoom_coord(this%grid%rotated_ll,this%dim,ilon,ilat,flon,flat,ix,iy,fx,fy)
  
case default
  call l4f_category_log(this%category,L4F_ERROR,"gtype: "//this%grid%type%type//" non gestita" )
  call raise_error("gtype non gestita")
  
end select


end SUBROUTINE zoom_coord_grid


SUBROUTINE zoom_index(this,that,ix,iy,fx,fy,&
 iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy) 
type(griddim_def),intent(in) :: this !< oggetto griddim in
type(griddim_def),intent(out) :: that !< oggetto griddim zoomato
integer, intent(in)  :: ix,iy,fx,fy  !< zoom index coordinate
integer, intent(out) :: newx,newy    !< new dimension for the future field
integer, intent(out) :: iox,ioy,fox,foy
integer, intent(out) :: inx,iny,fnx,fny

integer :: nx,ny
doubleprecision ::  lon_min, lon_max, lat_min, lat_max,steplon,steplat
character(len=80) :: type
integer :: cix,ciy ,cfx,cfy,ciox,cioy,cfox,cfoy, cinx,ciny,cfnx,cfny,nsx,nsy

call l4f_category_log(this%category,L4F_DEBUG,'zoom indices: '//&
   to_char(ix)//to_char(iy)//to_char(fx)//to_char(fy))

!check
if ( ix > fx .or. iy > fy ) then
  call l4f_category_log(this%category,L4F_ERROR,'zoom indices are wrong: '//&
   to_char(ix)//to_char(iy)//to_char(fx)//to_char(fy))
  call raise_error('zoom indices are wrong')
end if


call get_val(this,type,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max)

call l4f_category_log(this%category,L4F_DEBUG,'nx,ny: '//&
   to_char(nx)//to_char(ny))

!coordinate cartesiane
cix=ix-1
ciy=iy-1
cfx=fx-1
cfy=fy-1
nsx=nx-1
nsy=ny-1

ciox=min(max(cix,0),nsx)
cioy=min(max(ciy,0),nsy)

cfox=max(min(cfx,nsx),0)
cfoy=max(min(cfy,nsy),0)

!dimensione nuovo grigliato
newx=cfx-cix+1
newy=cfy-ciy+1

!test di intersezione
if ((ix > 0 .or. ix <= nx) .or.&
    (fx > 0 .or. fx <= nx) .or.& 
    (iy > 0 .or. iy <= ny) .or.&
    (fy > 0 .or. fy <= ny) )then

!calcolo le coordinate della vecchia matrice nella nuova
   cinx=min(max(-cix,0),nsx)
   ciny=min(max(-ciy,0),nsy)

   cfnx=min(cfx,nsx)-cix
   cfny=min(cfy,nsy)-ciy

else
   
   inx=imiss
   iny=imiss
   fnx=imiss
   fny=imiss

end if

steplon=(lon_max-lon_min)/(nx-1)
steplat=(lat_max-lat_min)/(ny-1)

lon_min=lon_min+steplon*(iox-1)
lat_min=lat_min+steplat*(ioy-1)

lon_max=lon_max+steplon*(fox-nx)
lat_max=lat_max+steplat*(foy-ny)

that%dim%nx= newx
that%dim%ny= newy

that%grid%regular_ll%lon_min=lon_min
that%grid%regular_ll%lon_max=lon_max
that%grid%regular_ll%lat_min=lat_min
that%grid%regular_ll%lat_max=lat_max

!torno alle coordinate originali
iox=ciox+1
ioy=cioy+1
fox=cfox+1
foy=cfoy+1
inx=cinx+1
iny=ciny+1
fnx=cfnx+1
fny=cfny+1


end SUBROUTINE zoom_index


SUBROUTINE zoom_field(field,fieldz,iox,ioy,fox,foy,inx,iny,fnx,fny) 

real,intent(in)      :: field(:,:)      !< matrice in ingresso
real,intent(out)     :: fieldz(:,:)     !< matrice zoommata in uscita
integer, intent(out) :: iox,ioy,fox,foy
integer, intent(out) :: inx,iny,fnx,fny

fieldz=rmiss

if (c_e(inx).and.c_e(iny).and.c_e(fnx).and.c_e(fny))then

   fieldz(inx:fnx,iny:fny)=field(iox:fox,ioy:foy)

end if

end SUBROUTINE zoom_field


!> Rigriglia \a this su una nuova griglia in cui ogni punto
!! è uguale alla media di \a ngx X \a ngy punti della griglia originaria
!! (medie su box).
SUBROUTINE regrid_index(this, that, npx, npy)
type(griddim_def),intent(in) :: this !> oggetto griddim in
type(griddim_def),intent(out) :: that !> oggetto griddim rigrigliato
INTEGER, INTENT(IN) :: npx !> numero di punti su cui fare la media lungo l'asse x
INTEGER, INTENT(IN) :: npy !> numero di punti su cui fare la media lungo l'asse y


!integer, intent(in)  :: ix,iy,fx,fy  !< zoom index coordinate
!integer, intent(out) :: newx,newy    !< new dimension for the future field
!integer, intent(out) :: iox,ioy,fox,foy
!integer, intent(out) :: inx,iny,fnx,fny

integer :: nx,ny
doubleprecision ::  lon_min, lon_max, lat_min, lat_max,steplon,steplat
character(len=80) :: type

!check

IF (npx <= 0 .OR. npy <= 0 .OR. npx > this%dim%nx .OR. npy > this%dim%ny) THEN
  CALL l4f_category_log(this%category,L4F_ERROR,'invalid regrid parameters: '//&
   TRIM(to_char(npx))//' '//TRIM(to_char(npy)))
  CALL raise_error('invalid regrid parameters')
ENDIF

IF (npx == 1 .AND. npy == 1) THEN ! Nothing to do
  that = this


!that%lon_min = this%x1+(ngx-1)*0.5*this%dx
!that%y1 = this%y1+(ngy-1)*0.5*this%dy
!that%grid%regular_ll%lon_min = this%grid%regular_ll%lon_min+(npx-1)*0.5*this%dx
!that%grid%regular_ll%lon_max = lon_max
!that%grid%regular_ll%lat_min = lat_min
!that%grid%regular_ll%lat_max = lat_max
!
!that%dim%nx = this%dim%nx/ngx
!that%dim%ny = this%dim%ny/ngy
!this%dx = this%dx*ngx
!this%dy = this%dy*ngy
!this%x2 = this%x1+(this%nx-1)*this%dx
!this%y2 = this%y1+(this%ny-1)*this%dy

END IF

END SUBROUTINE regrid_index


end module grid_class
