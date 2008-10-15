module regular_ll_class

! REGULAR LAT LON

use log4fortran

implicit none


character (len=255),parameter:: subcategory="regular_ll_class"


INTERFACE init
  MODULE PROCEDURE init_regular_ll
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_regular_ll
END INTERFACE


!>\brief trasforma grigliato da coordinate geografiche a coordinate x,y in proiezione
INTERFACE grid_proj
  MODULE PROCEDURE grid_proj_regular_ll
END INTERFACE

!>\brief trasforma grigliato da coordinate x,y in proiezione a coordinate geografiche
INTERFACE grid_unproj
  MODULE PROCEDURE grid_unproj_regular_ll
END INTERFACE



!>\brief trasforma da coordinate geografiche a coordinate x,y in proiezione
INTERFACE proj
  MODULE PROCEDURE proj_regular_ll
END INTERFACE

!>\brief trasforma da coordinate x,y in proiezione a coordinate geografiche
INTERFACE unproj
  MODULE PROCEDURE unproj_regular_ll
END INTERFACE

!>\brief ritorna i valori descrittivi del grigliato
INTERFACE get_val
  MODULE PROCEDURE get_val_regular_ll
END INTERFACE


private
public init,delete,grid_proj,grid_unproj,proj,unproj,get_val
public grid_dim,grid_regular_ll

!>\brief dimensioni del grigliato lat lon con eventuali vettori di coordinate
type grid_dim

  INTEGER :: nx, ny
  doubleprecision, pointer ::lat(:,:),lon(:,:)

end type grid_dim



!>\brief definizione del grigliato regular lat lon
type grid_regular_ll

!  private

  doubleprecision :: lon_min, lon_max, lat_min, lat_max
  integer :: component_flag
  integer :: category !< log4fortran
  
end type grid_regular_ll


contains


subroutine init_regular_ll(this,dim,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 categoryappend)

type(grid_regular_ll) ::this
type(grid_dim) :: dim
integer :: nx, ny
doubleprecision :: lon_min, lon_max, lat_min, lat_max
integer :: component_flag
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

nullify(dim%lon)
nullify(dim%lat)

this%lon_min =lon_min
this%lon_max =lon_max
this%lat_min =lat_min
this%lat_max =lat_max
this%component_flag=component_flag
dim%nx = nx
dim%ny = ny

end subroutine init_regular_ll


subroutine delete_regular_ll(this,dim)

type(grid_regular_ll) ::this
type(grid_dim) :: dim

if (associated(dim%lon)) then
  deallocate(dim%lon)
end if


if (associated(dim%lat)) then
  deallocate(dim%lat)
end if

!chiudo il logger
call l4f_category_delete(this%category)

end subroutine delete_regular_ll



subroutine grid_unproj_regular_ll(this,dim)

type(grid_regular_ll) ::this
type(grid_dim) :: dim

integer :: i,j
doubleprecision :: dlat,dlon


call l4f_category_log(this%category,L4F_DEBUG,"dimensioni: "//to_char(dim%nx)//to_char(dim%ny))

if (.not.associated(dim%lon)) then
  allocate (dim%lon(dim%nx,dim%ny))
  call l4f_category_log(this%category,L4F_DEBUG,"size lon: "//to_char(size(dim%lon)))
end if

if (.not.associated(dim%lat)) then
  allocate (dim%lat(dim%nx,dim%ny))
  call l4f_category_log(this%category,L4F_DEBUG,"size lat: "//to_char(size(dim%lat)))
end if


dlat= (this%lat_max - this%lat_min) / dble(dim%ny - 1 )
dlon= (this%lon_max - this%lon_min) / dble(dim%nx - 1 )

call unproj_regular_ll(this,&
 reshape((/ ((this%lon_min+dlon*dble(i) ,i=0,dim%nx), &
 j=0,dim%ny) /),(/dim%nx,dim%ny/)), &
 reshape((/ ((this%lat_min+dlat*dble(i) ,j=0,dim%ny), &
 i=0,dim%nx) /),(/dim%nx,dim%ny/)), &
 dim%lon,dim%lat)


end subroutine grid_unproj_regular_ll



subroutine grid_proj_regular_ll(this,dim)

type(grid_regular_ll) ::this
type(grid_dim) :: dim


call proj_regular_ll(this &
 ,dim%lon(1,1) &
 ,dim%lat(1,1) &
 ,this%lon_min &
 ,this%lat_min )

call proj_regular_ll(this &
 ,dim%lon(dim%nx,dim%ny) &
 ,dim%lat(dim%nx,dim%ny) &
 ,this%lon_max &
 ,this%lat_max )


end subroutine grid_proj_regular_ll




elemental subroutine unproj_regular_ll(this,x,y,lon,lat)

type(grid_regular_ll), intent(in) :: this
doubleprecision, intent(in)  :: x,y
doubleprecision, intent(out) :: lon,lat

lon=x
lat=y

end subroutine unproj_regular_ll



elemental subroutine proj_regular_ll(this,lon,lat,x,y)

type(grid_regular_ll), intent(in) ::this
doubleprecision, intent(in)  :: lon,lat
doubleprecision, intent(out) :: x,y

x=lon
y=lat

end subroutine proj_regular_ll









subroutine get_val_regular_ll(this,dim,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag)

type(grid_regular_ll),intent(in) ::this
type(grid_dim),intent(in) :: dim
integer,intent(out),optional :: nx, ny
doubleprecision,intent(out),optional :: lon_min, lon_max, lat_min, lat_max
integer,intent(out),optional :: component_flag

character(len=512) :: a_name



if (present(nx))nx=dim%nx
if (present(ny))ny=dim%ny 

da ribaltare anche qui sotto
 
this%lon_min =lon_min
this%lon_max =lon_max
this%lat_min =lat_min
this%lat_max =lat_max
this%component_flag=component_flag

end subroutine init_regular_ll














end module regular_ll_class
