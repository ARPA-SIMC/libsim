program demo

use grid_class
use volgrid6d_var_class
use log4fortran

implicit none

integer :: category,ier
character(len=512):: a_name
doubleprecision :: val

type (volgrid6d_var) :: var
type (griddim_def) :: griddim

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo")

!imposta a_name
category=l4f_category_get(a_name//".main")

!init di log4fortran
ier=l4f_init()

!leggo da qualche parte i dati su grigliato


call init (griddim,type="regular_ll", &
 nx = 10,ny = 15, &
 lon_min = -2.D0, &
 lon_max = 24.D0, &
 lat_min = 35.D0, &
 lat_max = 51.D0, &
 component_flag=1,&
 categoryappend="grigliato regolare manuale")


call init (var)

print *,var

call griddim_unproj(griddim)


!call l4f_category_log(category,L4F_INFO,&
!         "unproj ritorna "//to_char(grid%dim%lat(1,1))//to_char(grid%dim%lon(1,1)))

call get_val(griddim,lat_max=val)

call l4f_category_log(category,L4F_INFO,&
         "get_val ritorna "//to_char(val))

call delete(griddim)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo
