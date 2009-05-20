module lambert_class

! ROTATED LAT LON

use log4fortran
use regular_ll_class
USE doubleprecision_phys_const
use grib_api
use err_handling
use optional_values

implicit none

character (len=255),parameter:: subcategory="lamber_conformal_class"

!> Operatore logico di uguaglianza tra oggetti della classe lambert.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE lambert_eq
END INTERFACE


INTERFACE init
  MODULE PROCEDURE init_lambert
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_lambert
END INTERFACE

INTERFACE copy
  MODULE PROCEDURE copy_grid_lambert
END INTERFACE


!>\brief trasforma grigliato da coordinate geografiche a coordinate x,y in proiezione
INTERFACE grid_proj
  MODULE PROCEDURE  grid_proj_lambert 
END INTERFACE

!>\brief trasforma grigliato da coordinate x,y in proiezione a coordinate geografiche
INTERFACE grid_unproj
  MODULE PROCEDURE  grid_unproj_lambert 
END INTERFACE

!>\brief trasforma da coordinate geografiche a coordinate x,y in proiezione
INTERFACE proj
  MODULE PROCEDURE  proj_lambert
END INTERFACE

!>\brief trasforma da coordinate x,y in proiezione a coordinate geografiche
INTERFACE unproj
  MODULE PROCEDURE  unproj_lambert
END INTERFACE

!>\brief ritorna i valori descrittivi del grigliato
INTERFACE get_val
  MODULE PROCEDURE get_val_lambert
END INTERFACE

!>\brief imposta i valori descrittivi del grigliato
INTERFACE set_val
  MODULE PROCEDURE set_val_lambert
END INTERFACE

!>\brief scrive l'oggetto su un'unità I/O
INTERFACE write_unit
  MODULE PROCEDURE write_unit_lambert
END INTERFACE

!>\brief legge l'oggetto da un'unità I/O
INTERFACE read_unit
  MODULE PROCEDURE read_unit_lambert
END INTERFACE

!> Legge i valori dal grib e li imposta appropriatamente
INTERFACE import
  MODULE PROCEDURE import_lambert
END INTERFACE

!> Imposta i valori nel grib
INTERFACE export
  MODULE PROCEDURE export_lambert
END INTERFACE


INTERFACE display
  MODULE PROCEDURE display_lambert
END INTERFACE

INTERFACE zoom_coord
  MODULE PROCEDURE zoom_coord_lambert
END INTERFACE


!>\brief definizione del grigliato Lambert confomal
type grid_lambert

  private 

  type(grid_regular_ll) :: regular_ll

  doubleprecision :: latitude_south_pole,longitude_south_pole, &
   latin1,latin2,lov,lad
  integer :: projection_center_flag
  integer :: category !< log4fortran

end type grid_lambert


private
public init,delete,copy,grid_proj,grid_unproj,proj,unproj,get_val,set_val
public read_unit,write_unit,import,export,operator(==)
public display,zoom_coord
public grid_lambert


contains


subroutine init_lambert(this,dim, &
 nx,ny, &
 lon_min, lon_max, component_flag, &
 latitude_south_pole,longitude_south_pole,latin1,latin2, &
 lov,lad,projection_center_flag, &
 categoryappend)

type(grid_lambert) ::this
type(grid_dim) :: dim
integer,optional :: nx, ny
doubleprecision,optional :: lon_min, lon_max
doubleprecision,OPTIONAL :: latitude_south_pole,longitude_south_pole,latin1,latin2,lov,lad
INTEGER,OPTIONAL :: component_flag,projection_center_flag
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< accoda questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

call init(this%regular_ll,dim,&
 nx,ny, &
 lon_min, lon_max, dmiss, dmiss, component_flag, &
 categoryappend)

if (present(latitude_south_pole))then
   this%latitude_south_pole=latitude_south_pole
else
   this%latitude_south_pole=dmiss
end if

if (present(longitude_south_pole))then
   this%longitude_south_pole=longitude_south_pole
else
   this%longitude_south_pole=dmiss
end if

if (present(latin1))then
   this%latin1=latin1
else
   this%latin1=dmiss
end if

if (present(latin2))then
   this%latin2=latin2
else
   this%latin2=dmiss
end if

if (present(lov))then
  this%lov=lov
else
  this%lov=dmiss
end if

if (present(lad))then
  this%lad=lad
else
  this%lad=dmiss
end if

if (present(projection_center_flag))then
  this%projection_center_flag=projection_center_flag
else
  this%projection_center_flag=imiss
end if

end subroutine init_lambert


subroutine delete_lambert(this,dim)

type(grid_lambert) ::this
type(grid_dim) :: dim

call delete(this%regular_ll,dim)

this%latitude_south_pole=dmiss
this%longitude_south_pole=dmiss
this%latin1=dmiss
this%latin2=dmiss
this%lov=dmiss
this%lad=dmiss
this%projection_center_flag=imiss

!chiudo il logger
call l4f_category_delete(this%category)

end subroutine delete_lambert



subroutine copy_grid_lambert(this,that,categoryappend)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< accoda questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

type(grid_lambert),intent(in) ::this
type(grid_lambert),intent(out) ::that

that=this

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
that%category=l4f_category_get(a_name)

end subroutine copy_grid_lambert


! TODO da rivedere!!!!
subroutine grid_unproj_lambert(this,dim)

type(grid_lambert) ::this
type(grid_dim) :: dim

integer :: i,j
doubleprecision :: xmin, ymin, dlat,dlon


if (associated(dim%lon)) then
  if (dim%nx /= size(dim%lon,1) .or. dim%ny /= size(dim%lon,2)) then
    call l4f_category_log(this%category,L4F_WARN,"reallocate lon in grid_unproj_lambert")
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"dealloc size lon: "//to_char(size(dim%lon)))
#endif
    deallocate(dim%lon)
  end if
end if

if (associated(dim%lat)) then
  if (dim%nx /= size(dim%lat,1) .or. dim%ny /= size(dim%lat,2)) then
    call l4f_category_log(this%category,L4F_WARN,"reallocate lat in grid_unproj_lambert")
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"dealloc size lat: "//to_char(size(dim%lat)))
#endif
    deallocate (dim%lat)
  end if
end if


if (.not.associated(dim%lon)) then
  allocate (dim%lon(dim%nx,dim%ny))
end if

if (.not.associated(dim%lat)) then
  allocate (dim%lat(dim%nx,dim%ny))
end if

! compute projection coordinates of origin
CALL proj_lambert(this, this%regular_ll%lon_min, this%regular_ll%lat_min, &
 xmin, ymin)

dlon= (this%regular_ll%lon_max - this%regular_ll%lon_min) / dble(dim%nx - 1 )
dlat= (this%regular_ll%lat_max - this%regular_ll%lat_min) / dble(dim%ny - 1 )

call unproj_lambert(this,&
 reshape((/ &
 ((xmin+dlon*dble(i) ,i=0,dim%nx-1), j=0,dim%ny-1) /),&
 (/ dim%nx,dim%ny /)), &
 reshape((/ &
 ((ymin+dlat*dble(j) ,i=0,dim%nx-1), j=0,dim%ny-1) /),&
 (/ dim%nx,dim%ny /)), &
 dim%lon,dim%lat)


end subroutine grid_unproj_lambert


! TODO da rivedere!!!!
subroutine grid_proj_lambert(this,dim)

type(grid_lambert) ::this
type(grid_dim) :: dim


call proj_lambert(this &
 ,dim%lon(1,1) &
 ,dim%lat(1,1) &
 ,this%regular_ll%lon_min &
 ,this%regular_ll%lat_min )

call proj_lambert(this &
 ,dim%lon(dim%nx,dim%ny) &
 ,dim%lat(dim%nx,dim%ny) &
 ,this%regular_ll%lon_max &
 ,this%regular_ll%lat_max )


end subroutine grid_proj_lambert


! Formulas and notation from:
! http://mathworld.wolfram.com/LambertConformalConicProjection.html
! http://en.wikipedia.org/wiki/Lambert_conformal_conic_projection
! http://fr.wikipedia.org/wiki/Projection_conique_conforme_de_Lambert
! with the following guess:
! projection is always polar, so reference latitude=+-90 according to
! projectionCenterFlag; reference longitude is LoV.
! how coordinates of south pole should be treated? Metview ignores them.
elemental subroutine proj_lambert(this,lon,lat,x,y)
type(grid_lambert), intent(in) ::this
doubleprecision, intent(in)  :: lon,lat
doubleprecision, intent(out) :: x,y

doubleprecision  :: n, f, ro0, ro, cs1, cs2, cs3, pollat

IF (IAND(this%projection_center_flag, 128) == 0) THEN
  pollat = 90.D0
ELSE
  pollat = -90.D0
ENDIF
cs1 = COS(degrad*this%latin1)
cs2 = TAN(pi*.25D0 + degrad*this%latin1*.5D0)

IF (this%latin1 == this%latin2) THEN
  n = 1.0D0 ! verify that n->1 when latin2->latin1
ELSE
  n = LOG(cs1/COS(degrad*this%latin2)) / &
   LOG(TAN(pi*.25D0 + degrad*this%latin2*.5D0) / cs2)
ENDIF
f = cs1*cs2**n/n
ro0 = f/SIN(pi*.25D0 + degrad*pollat*.5D0)**n

ro = f/SIN(pi*.25D0 + degrad*lat*.5D0)**n

cs3 = degrad*n*(lon - this%lov)

x = ro*SIN(cs3)
y = ro0 - ro*COS(cs3)


end subroutine proj_lambert


elemental subroutine unproj_lambert(this,x,y,lon,lat)
type(grid_lambert),intent(in) ::this
doubleprecision, intent(in)  :: x,y
doubleprecision, intent(out) :: lon,lat

doubleprecision  :: n, f, ro0, ro, theta, cs1, cs2, pollat

IF (IAND(this%projection_center_flag, 128) == 0) THEN
  pollat = 90.D0
ELSE
  pollat = -90.D0
ENDIF
cs1 = COS(degrad*this%latin1)
cs2 = TAN(pi*.25D0 + degrad*this%latin1*.5D0)

IF (this%latin1 == this%latin2) THEN
  n = 1.0D0 ! verify limit
ELSE
  n = LOG(cs1/COS(degrad*this%latin2)) / &
   LOG(TAN(pi*.25D0 + degrad*this%latin2*.5D0) / cs2)
ENDIF
f = cs1*cs2**n/n
ro0 = f/SIN(pi*.25D0 + degrad*pollat*.5D0)**n

ro = SIGN(SQRT(x*x + (ro0-y)*(ro0-y)), n) ! check SIGN
theta = raddeg*ATAN2(x, ro0-y)

lon = this%lov + theta/n
lat = raddeg*(2.D0*ATAN((f/ro)**(1.D0/n)) - pi*.5D0)

end subroutine unproj_lambert


! eliminare lon_max e lat_max che sono inutili?
subroutine get_val_lambert(this,dim, &
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,latin1,latin2, &
 lov,lad,projection_center_flag)

type(grid_lambert),intent(in) ::this
type(grid_dim),intent(in) :: dim
integer,intent(out),optional :: nx, ny
doubleprecision,intent(out),optional :: lon_min, lon_max, lat_min, lat_max
doubleprecision,INTENT(out),OPTIONAL :: latitude_south_pole,longitude_south_pole,latin1,latin2,lov,lad
INTEGER,INTENT(out),OPTIONAL :: component_flag,projection_center_flag


call get_val(this%regular_ll,dim, &
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag)

if (present(latitude_south_pole))  latitude_south_pole=this%latitude_south_pole
if (present(longitude_south_pole)) longitude_south_pole=this%longitude_south_pole
if (present(latin1))               latin1=this%latin1
if (present(latin2))               latin2=this%latin2
if (present(lov))                  lov=this%lov
if (present(lad))                  lad=this%lad
if (present(projection_center_flag)) projection_center_flag=this%projection_center_flag

end subroutine get_val_lambert

subroutine set_val_lambert(this,dim, &
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag, &
 latitude_south_pole,longitude_south_pole,latin1,latin2, &
 lov,lad,projection_center_flag)

type(grid_lambert),intent(out) ::this
type(grid_dim),intent(out) :: dim
integer,intent(in),optional :: nx, ny
doubleprecision,intent(in),optional :: lon_min, lon_max, lat_min, lat_max
doubleprecision,intent(in),optional :: latitude_south_pole,longitude_south_pole,latin1,latin2,lov,lad
INTEGER,INTENT(in),OPTIONAL :: component_flag,projection_center_flag


call set_val(this%regular_ll,dim, &
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag)

if (present(latitude_south_pole))  this%latitude_south_pole=latitude_south_pole
if (present(longitude_south_pole)) this%longitude_south_pole=longitude_south_pole
if (present(latin1))               this%latin1=latin1
if (present(latin2))               this%latin2=latin2
if (present(lov))                  this%lov=lov
if (present(lad))                  this%lad=lad
if (present(projection_center_flag)) this%projection_center_flag=projection_center_flag

end subroutine set_val_lambert



!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE read_unit_lambert(this, unit) 

type(grid_lambert),intent(out) :: this !< oggetto def da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form


INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this
ELSE
  READ(unit)this
ENDIF


END SUBROUTINE read_unit_lambert



!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE write_unit_lambert(this, unit)

type(grid_lambert),intent(in) :: this !< oggetto def da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form


INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this
ELSE
  WRITE(unit)this
ENDIF

END SUBROUTINE write_unit_lambert


! da modificare import/export_dim in regular_ll_class sostituendo
! numberOfPointsAlongAParallel ecc. con gli alias ni,nj o nx,ny
! in modo che funzioni anche con Lambert e altre proiezioni
subroutine import_lambert(this,dim,gaid)

type(grid_lambert),intent(out) ::this
type(grid_dim),intent(out) :: dim
integer,INTENT(in)              :: gaid
integer ::EditionNumber

call import (this%regular_ll,dim,gaid)

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

call grib_get(gaid,'longitudeOfSouthernPoleInDegrees',this%longitude_south_pole)
call grib_get(gaid,'latitudeOfSouthernPoleInDegrees',this%latitude_south_pole)
call grib_get(gaid,'Latin1InDegrees',this%latin1)
call grib_get(gaid,'Latin2InDegrees',this%latin2)
call grib_get(gaid,'LoVInDegrees',this%lov)

CALL grib_get(gaid,'projectionCenterFlag',this%projection_center_flag)

if (EditionNumber == 1) then
! ECMWF (gribex/grib_api) says: Grid lengths are in metres, at the
! 60-degree parallel nearest to the pole on the projection plane.
!  IF (IAND(this%projection_center_flag, 128) == 0) THEN
!    this%lad = 60.D0 
!  ELSE
!    this%lad = -60.D0 
!  ENDIF
! WMO says: Grid lengths are in units of metres, at the secant cone
! intersection parallel nearest to the pole on the projection plane.
  this%lad = this%latin1
else if (EditionNumber == 2) then
  CALL grib_get(gaid,'LaDInDegrees',this%lad)
endif

end subroutine import_lambert



subroutine export_lambert(this,dim,gaid)
type(grid_lambert),intent(in) ::this
type(grid_dim),intent(in) :: dim
integer,INTENT(in)             :: gaid
integer ::EditionNumber

call export (this%regular_ll,dim,gaid)

call grib_set(gaid,'longitudeOfSouthernPoleInDegrees',this%longitude_south_pole)
call grib_set(gaid,'latitudeOfSouthernPoleInDegrees',this%latitude_south_pole)

! Latin 1 - first latitude from the pole at which the secant cone
! cuts the sphere
CALL grib_set(gaid,'Latin1InDegrees',this%latin1)
! Latin 2 - second latitude from the pole at which the secant cone
! cuts the sphere
CALL grib_set(gaid,'Latin2InDegrees',this%latin2)

! LoV - Longitude of meridian parallel to Y-axis along which
! latitude increases as the Y-coordinate increases
call grib_set(gaid,'LoVInDegrees',this%lov)

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 2)then
! LaD - Latitude where Dx and Dy are specified
  CALL grib_set(gaid,'LaDInDegrees',this%lad)
endif

!TODO??
!ponghino per bug grib_api
!  call grib_set(gaid,"pvlLocation",43)
!  call grib_set(gaid,'angleOfRotationInDegrees',this%angle_rotation)
!else if (EditionNumber == 2)then
!  call grib_set(gaid,'angleOfRotationOfProjectionInDegrees',this%angle_rotation)
!else
!  CALL raise_error('GribEditionNumber not supported')
!end if

end subroutine export_lambert



subroutine display_lambert(this,dim)
type(grid_lambert),intent(in) ::this
type(grid_dim),intent(in) :: dim

call display (this%regular_ll,dim)

print*,'longitude Of Southern Pole In Degrees     ',this%longitude_south_pole
print*,'latitude Of Southern Pole In Degrees      ',this%latitude_south_pole
print*,'secant latitude 1                         ',this%latin1
print*,'secant latitude 2                         ',this%latin2

end subroutine display_lambert


elemental FUNCTION lambert_eq(this, that) RESULT(res)
TYPE(grid_lambert),INTENT(IN) :: this, that
LOGICAL :: res


res = this%regular_ll == that%regular_ll .and. &
 this%latitude_south_pole == that%latitude_south_pole .and. &
 this%longitude_south_pole == that%longitude_south_pole .and. &
 this%latin1 == that%latin1 .and. &
 this%latin2 == that%latin2

END FUNCTION lambert_eq


subroutine zoom_coord_lambert(this,dim,&
 ilon,ilat,flon,flat,ix,iy,fx,fy)
type(grid_lambert),intent(in) ::this
type(grid_dim),intent(in)        :: dim
doubleprecision,intent(in) :: ilon,ilat,flon,flat
integer,intent(out) :: ix,iy,fx,fy
doubleprecision :: step_lon,step_lat
doubleprecision :: iplon,iplat,fplon,fplat

! questo non è proprio corretto perchè si ritiene che in regular_ll 
! x e y siano lon e lat
! che è vero ma formalmente sbagliato
call proj(this,ilon,ilat,iplon,iplat )
call proj(this,flon,flat,fplon,fplat )

call zoom_coord(this%regular_ll,dim,&
 iplon,iplat,fplon,fplat,ix,iy,fx,fy)

end subroutine zoom_coord_lambert


end module lambert_class

