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

integer :: category !< log4fortran

end type gridinfo



INTERFACE init
  MODULE PROCEDURE init_gridinfo
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE delete_gridinfo
END INTERFACE

private

public gridinfo,init,delete

contains

!> Inizializza un oggetto di tipo gridinfo.
SUBROUTINE init_gridinfo(this,grid,dim,time,timerange,level,var,categoryappend)
TYPE(gridinfo),intent(out) :: this !< oggetto da inizializzare

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


end module gridinfo_class
