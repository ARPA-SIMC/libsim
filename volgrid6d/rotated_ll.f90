module rotated_ll_class

! ROTATED LAT LON

use log4fortran
use regular_ll_class
USE phys_const
use grib_api
use err_handling

implicit none

character (len=255),parameter:: subcategory="regular_ll_class"


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

!>\brief ritorna i valori descrittivi del grigliato
INTERFACE get_val
  MODULE PROCEDURE get_val_rotated_ll
END INTERFACE


!>\brief ritorna i valori descrittivi del grigliato
INTERFACE write_unit
  MODULE PROCEDURE write_unit_rotated_ll
END INTERFACE

!>\brief ritorna i valori descrittivi del grigliato
INTERFACE read_unit
  MODULE PROCEDURE read_unit_rotated_ll
END INTERFACE


!> Import
!! Legge i valori dal grib e li imposta appropriatamente
INTERFACE import
  MODULE PROCEDURE import_rotated_ll
END INTERFACE

!> Export
!! Imposta i valori nel grib
INTERFACE export
  MODULE PROCEDURE export_rotated_ll
END INTERFACE



!>\brief definizione del grigliato rotated lat lon
type grid_rotated_ll

  private 

  type(grid_regular_ll) :: regular_ll

  doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation
  integer :: category !< log4fortran

end type grid_rotated_ll


private
public init,delete,grid_proj,grid_unproj,proj,unproj,get_val,read_unit,write_unit,import,export
public grid_rotated_ll


contains


subroutine init_rotated_ll(this,dim, &
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,angle_rotation, &
 categoryappend)

type(grid_rotated_ll) ::this
type(grid_dim) :: dim
integer :: nx, ny
doubleprecision :: lon_min, lon_max, lat_min, lat_max
doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation
integer :: component_flag
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

call init(this%regular_ll,dim,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 categoryappend)

this%latitude_south_pole=latitude_south_pole
this%longitude_south_pole=longitude_south_pole
this%angle_rotation=angle_rotation

end subroutine init_rotated_ll


subroutine delete_rotated_ll(this,dim)

type(grid_rotated_ll) ::this
type(grid_dim) :: dim

call delete(this%regular_ll,dim)

!chiudo il logger
call l4f_category_delete(this%category)

end subroutine delete_rotated_ll



subroutine grid_unproj_rotated_ll(this,dim)

type(grid_rotated_ll) ::this
type(grid_dim) :: dim

integer :: i,j
doubleprecision :: dlat,dlon



if (.not.associated(dim%lon)) then
  allocate (dim%lon(dim%nx,dim%ny))
end if

if (.not.associated(dim%lat)) then
  allocate (dim%lat(dim%nx,dim%ny))
end if


dlat= (this%regular_ll%lat_max - this%regular_ll%lat_min) / dble(dim%ny - 1 )
dlon= (this%regular_ll%lon_max - this%regular_ll%lon_min) / dble(dim%nx - 1 )

call unproj_rotated_ll(this,&
 reshape((/ ((this%regular_ll%lon_min+dlon*dble(i) ,i=0,dim%nx),&
 j=0,dim%ny) /),(/ dim%nx,dim%ny /)), &
 reshape((/ ((this%regular_ll%lat_min+dlat*dble(i) ,j=0,dim%ny),&
 i=0,dim%nx) /),(/ dim%nx,dim%ny /)), &
 dim%lon,dim%lat)


end subroutine grid_unproj_rotated_ll




subroutine grid_proj_rotated_ll(this,dim)

type(grid_rotated_ll) ::this
type(grid_dim) :: dim


call proj_rotated_ll(this &
 ,dim%lon(1,1) &
 ,dim%lat(1,1) &
 ,this%regular_ll%lon_min &
 ,this%regular_ll%lat_min )

call proj_rotated_ll(this &
 ,dim%lon(dim%nx,dim%ny) &
 ,dim%lat(dim%nx,dim%ny) &
 ,this%regular_ll%lon_max &
 ,this%regular_ll%lat_max )


end subroutine grid_proj_rotated_ll


elemental subroutine proj_rotated_ll(this, lon,lat,x,y )

type(grid_rotated_ll), intent(in) ::this
doubleprecision, intent(in)  :: lon,lat
doubleprecision, intent(out) :: x,y

!call l4f_category_log(this%category,L4F_ERROR,"QUESTO CODICE è ANCORA DA SCRIVERE")

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



subroutine get_val_rotated_ll(this,dim, &
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,angle_rotation)

type(grid_rotated_ll),intent(in) ::this
type(grid_dim),intent(in) :: dim
integer,intent(out),optional :: nx, ny
doubleprecision,intent(out),optional :: lon_min, lon_max, lat_min, lat_max
doubleprecision,intent(out),optional :: latitude_south_pole,longitude_south_pole,angle_rotation
integer,intent(out),optional :: component_flag


call get_val(this%regular_ll,dim, &
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag)

if (present(latitude_south_pole))  latitude_south_pole=this%latitude_south_pole
if (present(longitude_south_pole)) longitude_south_pole=this%longitude_south_pole
if (present(angle_rotation))       angle_rotation=this%angle_rotation

end subroutine get_val_rotated_ll



!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE read_unit_rotated_ll(this, unit) 

type(grid_rotated_ll),intent(out) :: this !< oggetto def da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form


INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this
ELSE
  READ(unit)this
ENDIF


END SUBROUTINE read_unit_rotated_ll



!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE write_unit_rotated_ll(this, unit)

type(grid_rotated_ll),intent(in) :: this !< oggetto def da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form


INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this
ELSE
  WRITE(unit)this
ENDIF

END SUBROUTINE write_unit_rotated_ll



subroutine import_rotated_ll(this,dim,gaid)

type(grid_rotated_ll),intent(out) ::this
type(grid_dim),intent(out) :: dim
integer,INTENT(in)              :: gaid
integer ::EditionNumber

call import (this%regular_ll,dim,gaid)

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

call grib_get(gaid,'longitudeOfSouthernPoleInDegrees',this%longitude_south_pole)
call grib_get(gaid,'latitudeOfSouthernPoleInDegrees',this%latitude_south_pole)

if (EditionNumber == 1)then

   call grib_get(gaid,'angleOfRotationInDegrees',this%angle_rotation)

else if (EditionNumber == 2)then

  call grib_get(gaid,'angleOfRotationOfProjectionInDegrees',this%angle_rotation)
  
else

  CALL raise_error('GribEditionNumber not supported')

end if
                                ! da capire come ottenere 

end subroutine import_rotated_ll



subroutine export_rotated_ll(this,dim,gaid)
type(grid_rotated_ll),intent(in) ::this
type(grid_dim),intent(in) :: dim
integer,INTENT(in)             :: gaid
integer ::EditionNumber

call export (this%regular_ll,dim,gaid)

call grib_set(gaid,'longitudeOfSouthernPoleInDegrees',this%longitude_south_pole)
call grib_set(gaid,'latitudeOfSouthernPoleInDegrees',this%latitude_south_pole)

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

   call grib_set(gaid,'angleOfRotationInDegrees',this%angle_rotation)

else if (EditionNumber == 2)then

   call grib_set(gaid,'angleOfRotationOfProjectionInDegrees',this%angle_rotation)

else

  CALL raise_error('GribEditionNumber not supported')

end if
                                ! da capire come ottenere 
end subroutine export_rotated_ll



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

