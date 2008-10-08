module regular_ll_class

! REGULAR LAT LON

implicit none


INTERFACE init
  MODULE PROCEDURE init_regular_ll
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_regular_ll
END INTERFACE


!>\brief traforma grigliato da coordinate geografiche a coordinate x,y in proiezione
INTERFACE grid_proj
  MODULE PROCEDURE grid_proj_regular_ll
END INTERFACE

!>\brief traforma grigliato da coordinate x,y in proiezione a coordinate geografiche
INTERFACE grid_unproj
  MODULE PROCEDURE grid_unproj_regular_ll
END INTERFACE



!>\brief traforma da coordinate geografiche a coordinate x,y in proiezione
INTERFACE proj
  MODULE PROCEDURE proj_regular_ll
END INTERFACE

!>\brief traforma da coordinate x,y in proiezione a coordinate geografiche
INTERFACE unproj
  MODULE PROCEDURE unproj_regular_ll
END INTERFACE




!>\brief dimensioni del grigliato lat lon con eventuali vettori di coordinate
type grid_dim_ll

  INTEGER :: nx, ny
  doubleprecision, pointer ::lat(:,:),lon(:,:)

end type grid_dim_ll



!>\brief definizione del grigliato regular lat lon
type grid_regular_ll

  doubleprecision :: lon_min, lon_max, lat_min, lat_max
  integer :: component_flag

end type grid_regular_ll


contains


subroutine init_regular_ll(this,grid_dim)

type(grid_regular_ll) ::this
type(grid_dim_ll) :: grid_dim

nullify(grid_dim%lon)
nullify(grid_dim%lat)

end subroutine init_regular_ll


subroutine delete_regular_ll(this,grid_dim)

type(grid_regular_ll) ::this
type(grid_dim_ll) :: grid_dim

if (associated(grid_dim%lon)) then
  deallocate(grid_dim%lon)
end if


if (associated(grid_dim%lat)) then
  deallocate(grid_dim%lat)
end if


end subroutine delete_regular_ll



subroutine grid_unproj_regular_ll(this,grid_dim)

type(grid_regular_ll) ::this
type(grid_dim_ll) :: grid_dim

integer :: i,j
doubleprecision :: dlat,dlon


print*,grid_dim%nx,grid_dim%ny,associated(grid_dim%lon)

if (.not.associated(grid_dim%lon)) then
  allocate (grid_dim%lon(grid_dim%nx,grid_dim%ny))
  print *,shape(grid_dim%lon),size(grid_dim%lon)
end if

if (.not.associated(grid_dim%lat)) then
  allocate (grid_dim%lat(grid_dim%nx,grid_dim%ny))
  print *,shape(grid_dim%lat),size(grid_dim%lat)
end if


dlat= (this%lat_max - this%lat_min) / dble(grid_dim%ny - 1 )
dlon= (this%lon_max - this%lon_min) / dble(grid_dim%nx - 1 )

call unproj_regular_ll(this,&
 reshape((/ ((this%lon_min+dlon*dble(i) ,i=0,grid_dim%nx), &
 j=0,grid_dim%ny) /),(/grid_dim%nx,grid_dim%ny/)), &
 reshape((/ ((this%lat_min+dlat*dble(i) ,j=0,grid_dim%ny), &
 i=0,grid_dim%nx) /),(/grid_dim%nx,grid_dim%ny/)), &
 grid_dim%lon,grid_dim%lat)


end subroutine grid_unproj_regular_ll



subroutine grid_proj_regular_ll(this,grid_dim)

type(grid_regular_ll) ::this
type(grid_dim_ll) :: grid_dim


call proj_regular_ll(this &
 ,grid_dim%lon(1,1) &
 ,grid_dim%lat(1,1) &
 ,this%lon_min &
 ,this%lat_min )

call proj_regular_ll(this &
 ,grid_dim%lon(grid_dim%nx,grid_dim%ny) &
 ,grid_dim%lat(grid_dim%nx,grid_dim%ny) &
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



end module regular_ll_class
