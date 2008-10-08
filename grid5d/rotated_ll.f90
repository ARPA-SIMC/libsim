module rotated_ll_class

! ROTATED LAT LON


use regular_ll_class
USE phys_const

implicit none


INTERFACE init
  MODULE PROCEDURE init_rotated_ll
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_rotated_ll
END INTERFACE


!>\brief traforma grigliato da coordinate geografiche a coordinate x,y in proiezione
INTERFACE grid_proj
  MODULE PROCEDURE  grid_proj_rotated_ll 
END INTERFACE

!>\brief traforma grigliato da coordinate x,y in proiezione a coordinate geografiche
INTERFACE grid_unproj
  MODULE PROCEDURE  grid_unproj_rotated_ll 
END INTERFACE



!>\brief traforma da coordinate geografiche a coordinate x,y in proiezione
INTERFACE proj
  MODULE PROCEDURE  proj_rotated_ll
END INTERFACE

!>\brief traforma da coordinate x,y in proiezione a coordinate geografiche
INTERFACE unproj
  MODULE PROCEDURE  unproj_rotated_ll
END INTERFACE




!>\brief definizione del grigliato rotated lat lon
type grid_rotated_ll

  type(grid_regular_ll) :: regular_ll

  doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation

end type grid_rotated_ll


contains


subroutine init_rotated_ll(this,grid_dim)

type(grid_rotated_ll) ::this
type(grid_dim_ll) :: grid_dim

call init_regular_ll(this%regular_ll,grid_dim)

end subroutine init_rotated_ll


subroutine delete_rotated_ll(this,grid_dim)

type(grid_rotated_ll) ::this
type(grid_dim_ll) :: grid_dim

call delete_regular_ll(this%regular_ll,grid_dim)

end subroutine delete_rotated_ll



subroutine grid_unproj_rotated_ll(this,grid_dim)

type(grid_rotated_ll) ::this
type(grid_dim_ll) :: grid_dim

integer :: i,j
doubleprecision :: dlat,dlon



if (.not.associated(grid_dim%lon)) then
  allocate (grid_dim%lon(grid_dim%nx,grid_dim%ny))
end if

if (.not.associated(grid_dim%lat)) then
  allocate (grid_dim%lat(grid_dim%nx,grid_dim%ny))
end if


dlat= (this%regular_ll%lat_max - this%regular_ll%lat_min) / dble(grid_dim%ny - 1 )
dlon= (this%regular_ll%lon_max - this%regular_ll%lon_min) / dble(grid_dim%nx - 1 )

call unproj_rotated_ll(this,&
 reshape((/ ((this%regular_ll%lon_min+dlon*dble(i) ,i=0,grid_dim%nx),&
 j=0,grid_dim%ny) /),(/ grid_dim%nx,grid_dim%ny /)), &
 reshape((/ ((this%regular_ll%lat_min+dlat*dble(i) ,j=0,grid_dim%ny),&
 i=0,grid_dim%nx) /),(/ grid_dim%nx,grid_dim%ny /)), &
 grid_dim%lon,grid_dim%lat)


end subroutine grid_unproj_rotated_ll




subroutine grid_proj_rotated_ll(this,grid_dim)

type(grid_rotated_ll) ::this
type(grid_dim_ll) :: grid_dim


call proj_rotated_ll(this &
 ,grid_dim%lon(1,1) &
 ,grid_dim%lat(1,1) &
 ,this%regular_ll%lon_min &
 ,this%regular_ll%lat_min )

call proj_rotated_ll(this &
 ,grid_dim%lon(grid_dim%nx,grid_dim%ny) &
 ,grid_dim%lat(grid_dim%nx,grid_dim%ny) &
 ,this%regular_ll%lon_max &
 ,this%regular_ll%lat_max )


end subroutine grid_proj_rotated_ll


elemental subroutine proj_rotated_ll(this, lon,lat,x,y )

type(grid_rotated_ll), intent(in) ::this
doubleprecision, intent(in)  :: lon,lat
doubleprecision, intent(out) :: x,y

!.........
x=lon
y=lat

end subroutine proj_rotated_ll



elemental subroutine unproj_rotated_ll(this,x,y,lon,lat )

type(grid_rotated_ll),intent(in) ::this
doubleprecision, intent(in)  :: x,y
doubleprecision, intent(out) :: lon,lat

doubleprecision  :: cy0,sy0

cy0 = cos(degrad*this%latitude_south_pole)
sy0 = sin(degrad*this%latitude_south_pole)

lon = raddeg*asin(sy0*cos(degrad*y)*cos(degrad*x)+cy0*sin(degrad*y))
lat = this%longitude_south_pole + raddeg*asin(sin(degrad*x)*cos(degrad*y)/cos(degrad*y))

end subroutine unproj_rotated_ll



!!$subroutine unproj_rotated_ll(this,x,y,lon,lat )
!!$
!!$type(grid_rotated_ll) ::this
!!$doubleprecision lon,lat,x,y
!!$doubleprecision :: cy0,sy0
!!$
!!$cy0 = cosd(this%latitude_south_pole)
!!$sy0 = sind(this%latitude_south_pole)
!!$
!!$!/todo testare dimensioni
!!$
!!$lon = asind(sy0*cosd(y)*cosd(x)+cy0*sind(y))
!!$lat = this%longitude_south_pole + asind(sind(x)*cosd(y)/cosd(y))
!!$
!!$!CALL rtlld( x0, cy0, sy0, x,y, (/ lon /) ,(/ lat /))
!!$
!!$end subroutine unproj_rotated_ll
!!$
!!$
!!$
!!$subroutine unproj_rotated_ll_v(this,x,y,lon,lat )
!!$
!!$type(grid_rotated_ll) ::this
!!$doubleprecision lon(:),lat(:),x(:),y(:)
!!$doubleprecision :: cy0,sy0
!!$
!!$cy0 = cosd(this%latitude_south_pole)
!!$sy0 = sind(this%latitude_south_pole)
!!$
!!$!/todo testare dimensioni
!!$
!!$lon = asind(sy0*cosd(y)*cosd(x)+cy0*sind(y))
!!$lat = this%longitude_south_pole + asind(sind(x)*cosd(y)/cosd(y))
!!$
!!$!CALL rtlld( x0, cy0, sy0, x,y, lon,lat)
!!$
!!$end subroutine unproj_rotated_ll_v


!!$elemental subroutine rtlld(x0, cy0, sy0, x, y, lon ,lat) 
!!$doubleprecision, INTENT(out) :: lon(:), lat(:)
!!$doubleprecision, INTENT(in) :: x(:), y(:), x0, cy0, sy0
!!$
!!$!/todo testare dimensioni
!!$
!!$lon = asind(sy0*cosd(y)*cosd(x)+cy0*sind(y))
!!$lat = x0 + asind(sind(x)*cosd(y)/cosd(y))
!!$
!!$
!!$END SUBROUTINE rtlld


end module rotated_ll_class
