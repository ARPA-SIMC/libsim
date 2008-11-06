module regular_ll_class

! REGULAR LAT LON

use log4fortran
use char_utilities
use grib_api
use err_handling

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


private
public init,delete,grid_proj,grid_unproj,proj,unproj,get_val,read_unit,write_unit,import,export,operator(==)
public grid_dim,grid_regular_ll
public display

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
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
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
 
if (present(lon_min)) lon_min=this%lon_min 
if (present(lon_max)) lon_max=this%lon_max
if (present(lat_min)) lat_min=this%lat_min
if (present(lat_max)) lat_max=this%lat_max
if (present(component_flag)) component_flag=this%component_flag

end subroutine get_val_regular_ll



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

!!$integer ::EditionNumber

! TODO
! gestire component flag
! component_flag


!non usati
!geography.gridWestEast
!geography.gridNorthSouth


call import_dim(dim,gaid)

call grib_get(gaid,'geography.loFirst' ,loFirst)
call grib_get(gaid,'geography.loLast'  ,loLast)
call grib_get(gaid,'geography.laFirst' ,laFirst)
call grib_get(gaid,'geography.laLast'  ,laLast)

!TODO
! la questione è piu' complicata
! per avere un processo reversibile vanno calcolate usando lo scan mode

this%lon_min=min(loFirst,loLast)
this%lon_max=max(loFirst,loLast)
this%lat_min=min(laFirst,laLast)
this%lat_max=max(laFirst,laLast)


!!$call grib_get(gaid,'GRIBEditionNumber',EditionNumber)
!!$
!!$if (EditionNumber == 1)then
!!$
!!$   call grib_get(gaid,'',this%)
!!$
!!$else if (EditionNumber == 2)then
!!$
!!$   call grib_get(gaid,'',this%)
!!$  
!!$else
!!$
!!$  CALL raise_error('GribEditionNumber not supported')
!!$
!!$end if

end subroutine import_regular_ll



subroutine export_regular_ll(this,dim,gaid)
type(grid_regular_ll),intent(in) ::this
type(grid_dim),intent(in)        :: dim
integer,INTENT(in)               :: gaid
doubleprecision :: loFirst,loLast,laFirst,laLast
!!$integer ::EditionNumber

call export_dim(dim,gaid)

! TODO
! gestire component flag
! component_flag


! TODO
! la questione è piu' complicata
! per avere un processo reversibile vanno calcolate usando lo scan mode
! iScansNegatively
! jScansPositively
! jPointsAreConsecutive
! alternativeRowScanning

! QUESTO E' SBAGLIATO

loFirst = this%lon_min
loLast  = this%lon_max
laFirst = this%lat_min
laLast  = this%lat_max

call grib_set(gaid,'geography.loFirst' ,loFirst)
call grib_set(gaid,'geography.loLast'  ,loLast)
call grib_set(gaid,'geography.laFirst' ,laFirst)
call grib_set(gaid,'geography.laLast'  ,laLast)

! TODO
! bisogna anche eventualmente ricalcolare i passi


!!$call grib_get(gaid,'GRIBEditionNumber',EditionNumber)
!!$
!!$if (EditionNumber == 1)then
!!$
!!$   call grib_set(gaid,'',this%)
!!$
!!$else if (EditionNumber == 2)then
!!$
!!$   call grib_set(gaid,'',this%)
!!$
!!$else
!!$
!!$  CALL raise_error('GribEditionNumber not supported')
!!$
!!$end if

end subroutine export_regular_ll



subroutine import_dim(this,gaid)
type(grid_dim),intent(out) :: this
integer,INTENT(in)             :: gaid

   call grib_get(gaid,'numberOfPointsAlongAMeridian',this%nx)
   call grib_get(gaid,'numberOfPointsAlongAParallel',this%ny)

end subroutine import_dim



subroutine export_dim(this,gaid)
type(grid_dim),intent(in) :: this
integer,INTENT(in)             :: gaid

   call grib_set(gaid,'numberOfPointsAlongAMeridian',this%nx)
   call grib_set(gaid,'numberOfPointsAlongAParallel',this%ny)

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

print*,'number Of Points Along A Meridian',this%nx
print*,'number Of Points Along A Parallel',this%ny

end subroutine display_dim


end module regular_ll_class
