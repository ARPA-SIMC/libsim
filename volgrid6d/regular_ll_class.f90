module regular_ll_class

! REGULAR LAT LON

use log4fortran
use char_utilities
use grib_api
use err_handling
use optional_values

implicit none


character (len=255),parameter:: subcategory="regular_ll_class"

!> Operatore logico di uguaglianza tra oggetti della classe regular_ll.
!! Funziona anche per 
!! confronti di tipo array-array (qualsiasi n. di dimensioni) e di tipo
!! scalare-vettore(1-d) (ma non vettore(1-d)-scalare o tra array con più
!! di 1 dimensione e scalari).
INTERFACE OPERATOR (==)
  MODULE PROCEDURE regular_ll_eq,dim_eq
END INTERFACE

INTERFACE init
  MODULE PROCEDURE init_regular_ll
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_regular_ll
END INTERFACE

INTERFACE copy
  MODULE PROCEDURE copy_dim, copy_grid_regular_ll
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

!>\brief imposta i valori descrittivi del grigliato
INTERFACE set_val
  MODULE PROCEDURE set_val_regular_ll
END INTERFACE


!>\brief ritorna i valori descrittivi del grigliato
INTERFACE write_unit
  MODULE PROCEDURE write_unit_regular_ll,write_unit_dim
END INTERFACE

!>\brief ritorna i valori descrittivi del grigliato
INTERFACE read_unit
  MODULE PROCEDURE read_unit_regular_ll,read_unit_dim
END INTERFACE


!> Import
!! Legge i valori dal grib e li imposta appropriatamente
INTERFACE import
  MODULE PROCEDURE import_regular_ll
END INTERFACE

!> Export
!! Imposta i valori nel grib
INTERFACE export
  MODULE PROCEDURE export_regular_ll
END INTERFACE


INTERFACE display
  MODULE PROCEDURE display_regular_ll,display_dim
END INTERFACE

INTERFACE zoom_coord
  MODULE PROCEDURE zoom_coord_regular_ll
END INTERFACE


private
public init,delete,copy,grid_proj,grid_unproj,proj,unproj,get_val,set_val
public read_unit,write_unit,import,export,operator(==)
public grid_dim,grid_regular_ll
public display,zoom_coord

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
integer,optional :: nx, ny
doubleprecision,optional :: lon_min, lon_max, lat_min, lat_max
integer,optional :: component_flag
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< accoda questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
this%category=l4f_category_get(a_name)

nullify(dim%lon)
nullify(dim%lat)

if (present(lon_min))then
   this%lon_min =lon_min
else
   this%lon_min =dmiss
end if

if (present(lon_max)) then
   this%lon_max =lon_max
else
   this%lon_max =dmiss
end if

if (present(lat_min)) then
   this%lat_min =lat_min
else
   this%lat_min =dmiss
end if

if (present(lat_max)) then
   this%lat_max =lat_max
else
   this%lat_max =dmiss
end if

if (present(component_flag)) then
   this%component_flag=component_flag
else
   this%component_flag=imiss
end if

if (present(nx)) then
   dim%nx = nx
else
   dim%nx = imiss
end if

if (present(ny)) then
   dim%ny = ny
else
   dim%ny = imiss
end if

end subroutine init_regular_ll


subroutine delete_regular_ll(this,dim)

type(grid_regular_ll) ::this
type(grid_dim) :: dim

this%lon_min =dmiss
this%lon_max =dmiss
this%lat_min =dmiss
this%lat_max =dmiss
this%component_flag=imiss
dim%nx = imiss
dim%ny = imiss

if (associated(dim%lon)) then
  deallocate(dim%lon)
end if


if (associated(dim%lat)) then
  deallocate(dim%lat)
end if

!chiudo il logger
call l4f_category_delete(this%category)

end subroutine delete_regular_ll


subroutine copy_grid_regular_ll(this,that,categoryappend)
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< accoda questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

type(grid_regular_ll),intent(in) ::this
type(grid_regular_ll),intent(out) ::that

that=this

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(optio_c(categoryappend,255)))
that%category=l4f_category_get(a_name)

call l4f_category_log(that%category,L4F_DEBUG,"end copy_grid_regular_ll")


end subroutine copy_grid_regular_ll

subroutine copy_dim(this,that)

type(grid_dim),intent(in) :: this
type(grid_dim),intent(out) :: that

that%nx=this%nx
that%ny=this%ny

if (associated(this%lon))then
  if (.not.associated(that%lon)) then
    allocate (that%lon(that%nx,that%ny))
    that%lon(:,:)=this%lon(:,:)
  end if
else
  nullify (that%lon)
end if


if (associated(this%lat))then
  if (.not.associated(that%lat)) then
    allocate (that%lat(that%nx,that%ny))
    that%lat(:,:)=this%lat(:,:)
  end if
else
  nullify (that%lat)
end if


end subroutine copy_dim



subroutine grid_unproj_regular_ll(this,dim)

type(grid_regular_ll) ::this
type(grid_dim) :: dim

integer :: i,j
doubleprecision :: dlat,dlon


call l4f_category_log(this%category,L4F_DEBUG,"dimensioni: "//to_char(dim%nx)//to_char(dim%ny))

if (associated(dim%lon)) then
  if (dim%nx /= size(dim%lon,1) .or. dim%ny /= size(dim%lon,2)) then
    call l4f_category_log(this%category,L4F_WARN,"reallocate lon in grid_unproj_regular_ll")
    call l4f_category_log(this%category,L4F_DEBUG,"dealloc size lon: "//to_char(size(dim%lon)))
    deallocate(dim%lon)
  end if
end if

if (associated(dim%lat)) then
  if (dim%nx /= size(dim%lat,1) .or. dim%ny /= size(dim%lat,2)) then
    call l4f_category_log(this%category,L4F_WARN,"reallocate lat in grid_unproj_regular_ll")
    call l4f_category_log(this%category,L4F_DEBUG,"dealloc size lat: "//to_char(size(dim%lat)))
    deallocate (dim%lat)
  end if
end if


if (.not.associated(dim%lon)) then
  allocate (dim%lon(dim%nx,dim%ny))
  call l4f_category_log(this%category,L4F_DEBUG,"size lon matrix: "//to_char(size(dim%lon)))
end if

if (.not.associated(dim%lat)) then
  allocate (dim%lat(dim%nx,dim%ny))
  call l4f_category_log(this%category,L4F_DEBUG,"size lat matrix: "//to_char(size(dim%lat)))
end if

dlon= (this%lon_max - this%lon_min) / dble(dim%nx - 1 )
dlat= (this%lat_max - this%lat_min) / dble(dim%ny - 1 )

call unproj_regular_ll(this,&
 reshape((/ &
 ((this%lon_min+(dlon*dble(i)) ,i=0,dim%nx-1), j=0,dim%ny-1) /),&
 (/dim%nx,dim%ny/)), &
 reshape((/ &
 ((this%lat_min+(dlat*dble(j)) ,i=0,dim%nx-1), j=0,dim%ny-1) /),&
 (/dim%nx,dim%ny/)), &
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
 
if (present(lon_min)) lon_min=this%lon_min 
if (present(lon_max)) lon_max=this%lon_max
if (present(lat_min)) lat_min=this%lat_min
if (present(lat_max)) lat_max=this%lat_max
if (present(component_flag)) component_flag=this%component_flag

end subroutine get_val_regular_ll



subroutine set_val_regular_ll(this,dim,&
 nx,ny, &
 lon_min, lon_max, lat_min, lat_max, component_flag)

type(grid_regular_ll),intent(out) ::this
type(grid_dim),intent(out) :: dim
integer,intent(in),optional :: nx, ny
doubleprecision,intent(in),optional :: lon_min, lon_max, lat_min, lat_max
integer,intent(in),optional :: component_flag

character(len=512) :: a_name

if (present(nx))dim%nx=nx
if (present(ny))dim%ny=ny 
 
if (present(lon_min)) this%lon_min=lon_min 
if (present(lon_max)) this%lon_max=lon_max
if (present(lat_min)) this%lat_min=lat_min
if (present(lat_max)) this%lat_max=lat_max
if (present(component_flag)) this%component_flag=component_flag

end subroutine set_val_regular_ll



!> Restituisce il passo di griglia lungo x
!FUNCTION regular_ll_dlon(this) RESULT(delta)
!TYPE(grid_regular_ll), INTENT(in) :: this
!DOUBLEPRECISION :: delta
!
!delta = (this%lon_max - this%lon_min) / dble(dim%nx - 1 )
!
!END FUNCTION regular_ll_dlon
!
!
!> Restituisce il passo di griglia lungo y
!FUNCTION regular_ll_dlat(this) RESULT(delta)
!TYPE(grid_regular_ll), INTENT(in) :: this
!DOUBLEPRECISION :: delta
!
!delta = (this%lat_max - this%lat_min) / dble(dim%ny - 1 )
!
!END FUNCTION regular_ll_dlat


!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE read_unit_dim(this, unit) 

type(grid_dim),intent(out) :: this !< oggetto def da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form


INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this%nx,this%ny
  if (associated(this%lon).and.associated(this%lat))read(unit,*)this%lon,this%lat
ELSE
  READ(unit)this%nx,this%ny
  if (associated(this%lon).and.associated(this%lat))read(unit)this%lon,this%lat
ENDIF


END SUBROUTINE read_unit_dim



!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE write_unit_dim(this, unit)

type(grid_dim),intent(in) :: this !< oggetto def da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form


INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this%nx,this%ny
  if (associated(this%lon).and.associated(this%lat))WRITE(unit,*)this%lon,this%lat

ELSE
  WRITE(unit)this%nx,this%ny
  if (associated(this%lon).and.associated(this%lat))WRITE(unit)this%lon,this%lat
ENDIF

END SUBROUTINE write_unit_dim



!> Legge da un'unità di file il contenuto dell'oggetto \a this.
!! Il record da leggere deve essere stato scritto con la ::write_unit
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE read_unit_regular_ll(this, unit) 

type(grid_regular_ll),intent(out) :: this !< oggetto def da leggere
INTEGER, INTENT(in) :: unit !< unità da cui leggere

CHARACTER(len=40) :: form


INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  READ(unit,*)this
ELSE
  READ(unit)this
ENDIF


END SUBROUTINE read_unit_regular_ll



!> Scrive su un'unità di file il contenuto dell'oggetto \a this.
!! Il record scritto potrà successivamente essere letto con la ::read_unit.
!! Il metodo controlla se il file è
!! aperto per un I/O formattato o non formattato e fa la cosa giusta.
SUBROUTINE write_unit_regular_ll(this, unit)

type(grid_regular_ll),intent(in) :: this !< oggetto def da scrivere
INTEGER, INTENT(in) :: unit !< unità su cui scrivere

CHARACTER(len=40) :: form


INQUIRE(unit, form=form)
IF (form == 'FORMATTED') THEN
  WRITE(unit,*)this
ELSE
  WRITE(unit)this
ENDIF

END SUBROUTINE write_unit_regular_ll



subroutine import_regular_ll(this,dim,gaid)

type(grid_regular_ll),intent(out) ::this
type(grid_dim),intent(out)        :: dim
integer,INTENT(in)                :: gaid

doubleprecision :: loFirst,loLast,laFirst,laLast
integer ::iScansNegatively,jScansPositively
!!$integer ::EditionNumber

! TODO
! gestire component flag

!non usati
!geography.gridWestEast
!geography.gridNorthSouth

call import_dim(dim,gaid)

call grib_get(gaid,'geography.loFirst' ,loFirst)
call grib_get(gaid,'geography.loLast'  ,loLast)
call grib_get(gaid,'geography.laFirst' ,laFirst)
call grib_get(gaid,'geography.laLast'  ,laLast)

call grib_get(gaid,'iScansNegatively',iScansNegatively)
call grib_get(gaid,'jScansPositively',jScansPositively)

IF (iScansNegatively  == 0) THEN

  this%lon_min = loFirst
  this%lon_max = loLast 

else

  this%lon_max = loFirst
  this%lon_min = loLast 

end IF

IF (jScansPositively == 0) THEN

  this%lat_max = laFirst
  this%lat_min = laLast 

else

  this%lat_min = laFirst
  this%lat_max = laLast 

end IF

!cerco di esprimere le coordinate come in un piano cartesiano
if (this%lon_max-this%lon_min < 0) this%lon_min=this%lon_min-360.d0

end subroutine import_regular_ll



subroutine export_regular_ll(this,dim,gaid)
type(grid_regular_ll),intent(in) ::this
type(grid_dim),intent(in)        :: dim
integer,INTENT(in)               :: gaid
doubleprecision :: loFirst,loLast,laFirst,laLast, dlon, dlat, ratio
integer ::iScansNegatively,jScansPositively

integer ::EditionNumber

!TODO:
! qui c'è un serio problema
! questo messaggio a volte (subarea) sparisce e poi risulta 
! impossibile proseguire per problemi in grib_api
! BISOGNA INDAGARE
call l4f_category_log(this%category,L4F_DEBUG,"start export_regular_ll")

call export_dim(dim,gaid)

! TODO
! gestire component flag

call grib_get(gaid,'iScansNegatively',iScansNegatively)
call grib_get(gaid,'jScansPositively',jScansPositively)

IF (iScansNegatively  == 0) THEN

  loFirst = this%lon_min
  loLast  = this%lon_max

else

  loFirst = this%lon_max
  loLast  = this%lon_min

end IF

IF (jScansPositively == 0) THEN

  laFirst = this%lat_max
  laLast  = this%lat_min

else

  laFirst = this%lat_min
  laLast  = this%lat_max

end IF

!report lon in standard grib 2 definition
if (loFirst < 0.d0)loFirst=loFirst+360.d0
if (loLast < 0.d0)loLast=loLast+360.d0

call grib_set(gaid,'geography.loFirst' ,loFirst)
call grib_set(gaid,'geography.loLast'  ,loLast)
call grib_set(gaid,'geography.laFirst' ,laFirst)
call grib_set(gaid,'geography.laLast'  ,laLast)

! Recompute and code grid steps if possible
dlat = (this%lat_max - this%lat_min) / dble(dim%ny - 1 )
dlon = (this%lon_max - this%lon_min) / dble(dim%nx - 1 )

CALL grib_get(gaid,'GRIBEditionNumber',EditionNumber)
IF (EditionNumber == 1) THEN
  ratio = 1.d3

!TODO
! per ora devo fare questo ma poi i PV dovranno essere gestiti
!   call grib_set(gaid,"numberOfVerticalCoordinateValues",0)
!   call grib_set(gaid,"pvlLocation",255)


ELSE IF (EditionNumber == 2) THEN
  ratio = 1.d6
ELSE
  call l4f_category_log(this%category,L4F_ERROR,"GribEditionNumber not supported: "//trim(to_char(editionNumber)))
  CALL raise_error('GribEditionNumber not supported')
ENDIF


!TODO da verificare questo test (la costante 0.5 è casuale)
IF (ABS(NINT(dlon*ratio) - dlon*ratio) > 0.5 .OR. &
 ABS(NINT(dlat*ratio) - dlat*ratio) > 0.5) THEN ! Increments not accurate

  call l4f_category_log(this%category,L4F_INFO,"incremets not given: inaccurate!")
  call l4f_category_log(this%category,L4F_DEBUG,"lon incremets difference: "//&
   to_char(ABS(NINT(dlon*ratio) - dlon*ratio)))
  call l4f_category_log(this%category,L4F_DEBUG,"lat incremets difference: "//&
   to_char(ABS(NINT(dlat*ratio) - dlat*ratio)))

  CALL grib_set(gaid,'resolutionAndComponentFlags',0)
  CALL grib_set_missing(gaid,'iDirectionIncrement')
  CALL grib_set_missing(gaid,'jDirectionIncrement')

! questo non va
!  CALL grib_set(gaid,'ijDirectionIncrementGiven', 0)
!  CALL grib_set(gaid,'iDirectionIncrementGiven', 0)
!  CALL grib_set(gaid,'jDirectionIncrementGiven', 0)
!  CALL grib_set_missing(gaid,'geography.iInc')
!  CALL grib_set_missing(gaid,'geography.jInc')

ELSE
  call l4f_category_log(this%category,L4F_DEBUG,"setting incremets: "//trim(to_char(dlon))//trim(to_char(dlat)))

  CALL grib_set(gaid,'resolutionAndComponentFlags',128)
  CALL grib_set(gaid,'iDirectionIncrement',dlon*ratio)
  CALL grib_set(gaid,'jDirectionIncrement',dlat*ratio)

! questo non va
!  CALL grib_set(gaid,'ijDirectionIncrementGiven', 1)
!  CALL grib_set(gaid,'iDirectionIncrementGiven', 1)
!  CALL grib_set(gaid,'jDirectionIncrementGiven', 1)
!  CALL grib_set(gaid,'geography.iInc', dlon)
!  CALL grib_set(gaid,'geography.jInc', dlat)

ENDIF

end subroutine export_regular_ll



subroutine import_dim(this,gaid)
type(grid_dim),intent(out) :: this
integer,INTENT(in)             :: gaid

   call grib_get(gaid,'numberOfPointsAlongAParallel',this%nx)
   call grib_get(gaid,'numberOfPointsAlongAMeridian',this%ny)

end subroutine import_dim



subroutine export_dim(this,gaid)
type(grid_dim),intent(in) :: this
integer,INTENT(in)             :: gaid

   call grib_set(gaid,'numberOfPointsAlongAParallel',this%nx)
   call grib_set(gaid,'numberOfPointsAlongAMeridian',this%ny)

end subroutine export_dim


! TODO
! bisogna sviluppare gli altri operatori


elemental FUNCTION regular_ll_eq(this, that) RESULT(res)
TYPE(grid_regular_ll),INTENT(IN) :: this, that
LOGICAL :: res

res = this%lon_min == that%lon_min .and. &
 this%lon_max == that%lon_max .and. &
 this%lat_min == that%lat_min .and. &
 this%lat_max == that%lat_max .and. &
 this%component_flag == that%component_flag

END FUNCTION regular_ll_eq

elemental FUNCTION dim_eq(this, that) RESULT(res)
TYPE(grid_dim),INTENT(IN) :: this, that
LOGICAL :: res

res = this%nx == that%nx .and. &
 this%ny == that%ny

END FUNCTION dim_eq



subroutine zoom_coord_regular_ll(this,dim,&
 ilon,ilat,flon,flat,ix,iy,fx,fy)
type(grid_regular_ll),intent(in) ::this
type(grid_dim),intent(in)        :: dim
doubleprecision,intent(in) :: ilon,ilat,flon,flat
integer,intent(out) :: ix,iy,fx,fy
doubleprecision :: step_lon,step_lat
doubleprecision :: iplon,iplat,fplon,fplat

call proj(this,ilon,ilat,iplon,iplat )
call proj(this,flon,flat,fplon,fplat )

step_lon=(this%lon_max-this%lon_min)/dble(dim%nx-1)
step_lat=(this%lat_max-this%lat_min)/dble(dim%ny-1)

ix=nint((ilon-this%lon_min)/step_lon)+1
iy=nint((ilat-this%lat_min)/step_lat)+1
fx=nint((flon-this%lon_min)/step_lon)+1
fy=nint((flat-this%lat_min)/step_lat)+1

end subroutine zoom_coord_regular_ll

subroutine display_regular_ll(this,dim)
type(grid_regular_ll),intent(in) ::this
type(grid_dim),intent(in)        :: dim
doubleprecision :: loFirst,loLast,laFirst,laLast


call display_dim(dim)

print*,"loFirst", this%lon_min
print*,"loLast ", this%lon_max
print*,"laFirst", this%lat_min
print*,"laLast ", this%lat_max

end subroutine display_regular_ll


subroutine display_dim(this)
type(grid_dim),intent(in) :: this

print*,'number Of Points Along A Parallel',this%nx
print*,'number Of Points Along A Meridian',this%ny

end subroutine display_dim


end module regular_ll_class
