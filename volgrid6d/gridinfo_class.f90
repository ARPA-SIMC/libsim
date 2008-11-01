module gridinfo_class

USE grid_class
USE datetime_class
USE vol7d_timerange_class
USE vol7d_level_class
USE volgrid6d_var_class
use log4fortran


IMPLICIT NONE

character (len=255),parameter:: subcategory="gridinfo_class"


!> Definisce un oggetto contenente le informazioni relative a un grib
type gridinfo


!> descrittore del grigliato
  type(grid_def) :: grid
  type(grid_dim) :: dim
!> descrittore della dimensione tempo
  TYPE(datetime) :: time
!> descrittore della dimensione intervallo temporale (timerange)
  TYPE(vol7d_timerange) :: timerange
!> descrittore della dimensione livello verticale
  TYPE(vol7d_level) :: level
!> vettore descrittore della dimensione variabile di anagrafica
  TYPE(volgrid6d_var) :: var
!> id del grib come da grib_api
  integer ::  gaid

  integer :: category !< log4fortran

end type gridinfo



INTERFACE init
  MODULE PROCEDURE init_gridinfo
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_gridinfo
END INTERFACE


!> Import
!! Legge i valori dal grib e li imposta appropriatamente
INTERFACE import
  MODULE PROCEDURE import_time,import_timerange,import_level
END INTERFACE

!> Export
!! Imposta i valori nel grib
INTERFACE export
  MODULE PROCEDURE export_time,export_timerange,export_level
END INTERFACE


private

public gridinfo,init,delete

contains

!> Inizializza un oggetto di tipo gridinfo.
SUBROUTINE init_gridinfo(this,gaid,grid,dim,time,timerange,level,var,categoryappend)
TYPE(gridinfo),intent(out) :: this !< oggetto da inizializzare

!> id del grib come da grib_api
integer,intent(in),optional ::  gaid
!> descrittore del grigliato
type(grid_def),intent(in),optional :: grid
type(grid_dim),intent(in),optional :: dim
!> descrittore della dimensione tempo
TYPE(datetime),intent(in),optional :: time
!> descrittore della dimensione intervallo temporale (timerange)
TYPE(vol7d_timerange),intent(in),optional :: timerange
!> descrittore della dimensione livello verticale
TYPE(vol7d_level),intent(in),optional :: level
!> vettore descrittore della dimensione variabile di anagrafica
TYPE(volgrid6d_var),intent(in),optional :: var

character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran

character(len=512) :: a_name

call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
this%category=l4f_category_get(a_name)

!call l4f_category_log(this%category,L4F_DEBUG,"init grid type: "//this%grid%type%type )


if (present(gaid))then
  this%gaid=gaid
else
  this%gaid=imiss
end if

if (present(grid).and.present(dim))then
  this%grid=grid
  this%dim=dim
else
  call init(this%grid,this%dim)
end if

if (present(time))then
  this%time=time
else
  call init(this%time)
end if

if (present(timerange))then
  this%timerange=timerange
else
  call init(this%timerange)
end if

if (present(level))then
  this%level=level
else
  call init(this%level)
end if

if (present(var))then
  this%var=var
else
  call init(this%var)
end if

end SUBROUTINE init_gridinfo


subroutine delete_gridinfo (this)
TYPE(gridinfo),intent(out) :: this !< oggetto da eliminare


call delete(this%grid,this%dim)
call delete(this%time)
call delete(this%timerange)
call delete(this%level)
call delete(this%var)

!chiudo il logger
call l4f_category_delete(this%category)


end subroutine delete_gridinfo



subroutine import_gridinfo (this)

TYPE(gridinfo),intent(out) :: this !< oggetto da eliminare


call l4f_category_log(this%category,L4F_DEBUG,"ora provo ad importare da grib " )


call import(this%grid,this%dim,this%gaid)
call import(this%time,this%gaid)
call import(this%timerange,this%gaid)
call import(this%level,this%gaid)
call import(this%var,this%gaid)


end subroutine import_gridinfo


subroutine export_gridinfo (this)

TYPE(gridinfo),intent(out) :: this !< oggetto da eliminare


call l4f_category_log(this%category,L4F_DEBUG,"ora provo ad importare da grib " )


call export(this%grid,this%dim,this%gaid)
call export(this%time,this%gaid)
call export(this%timerange,this%gaid)
call export(this%level,this%gaid)
call export(this%var,this%gaid)


end subroutine export_gridinfo





subroutine import_time(this,gaid)

TYPE(datetime),INTENT(out) :: this
integer,INTENT(in)         :: gaid
integer                    :: EditionNumber
character(len=8)           :: date
character(len=9)           :: time


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or.EditionNumber == 2 )then

  call grib_get(gaid,'dataDate',date )
  call grib_get(gaid,'dataTime',time(:4) )

  call init (this,simpledate=date//time(:4))

else

  CALL raise_error('GribEditionNumber not supported')

end if

end subroutine import_time



subroutine export_time(this,gaid)

TYPE(datetime),INTENT(in) :: this
integer,INTENT(in)        :: gaid
integer                   :: EditionNumber
character(len=17)         :: date_time


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or.EditionNumber == 2 )then

! datetime is AAAAMMGGhhmmssmsc
  call getval (this,simpledate=date_time)
  call grib_set(gaid,'dataDate',date_time(:8))
  call grib_set(gaid,'dataTime',date_time(9:14))

else

  CALL raise_error('GribEditionNumber not supported')

end if


end subroutine export_time



subroutine import_level(this,gaid)

TYPE(vol7d_level),INTENT(out) :: this
integer,INTENT(in)          :: gaid
integer                     :: EditionNumber,level1,l1,level2,l2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then
  
  call grib_get(gaid,'indicatorOfTypeOfLevel',level1)
  call grib_get(gaid,'topLevel',l1)
  level2=imiss
  call grib_get(gaid,'bottomLevel',l2)

  call init (this,level1,l1,level2,l2)

else if (EditionNumber == 2)then

  call grib_get(gaid,'typeOfFirstFixedSurface',level1)
  call grib_get(gaid,'scaledValueOfFirstFixedSurface',l1)
  call grib_get(gaid,'typeOfSecondFixedSurface',level2)     ! (missing=255)
  call grib_get(gaid,'scaledValueOfSecondFixedSurface',l2)

  call init (this,level1,l1,level2,l2)
  
else

  call raise_error('GribEditionNumber not supported')

end if

end subroutine import_level



subroutine export_level(this,gaid)

TYPE(vol7d_level),INTENT(in) :: this
integer,INTENT(in)         :: gaid
integer                     :: EditionNumber,level1,l1,level2,l2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  call grib_set(gaid,'indicatorOfTypeOfLevel',this%level1)
  call grib_set(gaid,'topLevel',this%l1)
  call grib_set(gaid,'bottomLevel',this%l2)


else if (EditionNumber == 2)then

  call grib_set(gaid,'typeOfFirstFixedSurface',this%level1)     ! (missing=255)
  call grib_set(gaid,'scaledValueOfFirstFixedSurface',this%l1)
  call grib_set(gaid,'typeOfSecondFixedSurface',this%level2)
  call grib_set(gaid,'scaledValueOfSecondFixedSurface',this%l2)

else

  call raise_error('GribEditionNumber not supported')

end if


end subroutine export_level



subroutine import_timerange(this,gaid)

TYPE(vol7d_timerange),INTENT(out) :: this
integer,INTENT(in)          :: gaid
integer ::EditionNumber,timerange,p1,p2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or. EditionNumber == 2)then
  
  call grib_get(gaid,'typeOfStatisticalProcessing',timerange)
  call grib_get(gaid,'endStepInHours',p1)
  call grib_get(gaid,'lengthOfTimeRange',p2)
  
  call init (this, timerange,p1,p2)

else

  call raise_error('GribEditionNumber not supported')

end if

end subroutine import_timerange



subroutine export_timerange(this,gaid)

TYPE(vol7d_timerange),INTENT(in) :: this
integer,INTENT(in)         :: gaid
integer ::EditionNumber,timerange,p1,p2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1 .or. EditionNumber == 2)then

  call grib_set(gaid,'typeOfStatisticalProcessing',this%timerange)
  call grib_set(gaid,'endStepInHours',this%p1)
  call grib_set(gaid,'lengthOfTimeRange',this%p2)

else

  call raise_error('GribEditionNumber not supported')

end if



end subroutine export_timerange



end module gridinfo_class
