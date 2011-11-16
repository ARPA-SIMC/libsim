#include "config.h"

use space_utilities
use log4fortran
use char_utilities
#ifdef HAVE_LIBNCARG
USE ncar_plot_class
#endif


IMPLICIT NONE
integer,dimension(:),allocatable :: seed
integer :: k,i

integer,parameter :: ndp=10000
double precision, DIMENSION(ndp) :: x,y 
integer ::  status
character(len=512):: a_name
INTEGER :: category, ier
type(triangles) :: tri

#ifdef HAVE_LIBNCARG
type(ncar_plot) :: plot
#endif

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="example_space")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"start")

! create replicatable random numbers
call random_seed(size=k)
allocate (seed(k))
seed=5
call random_seed(put=seed)
call random_number(x)
call random_number(y)

tri=triangles_new(ndp)

status = triangles_compute(x,y,tri)
call l4f_category_log(category,L4F_INFO,"contng status="//t2c(status))

call l4f_category_log(category,L4F_INFO,"number of triangles="//t2c(tri%nt))

#ifdef HAVE_LIBNCARG

call l4f_category_log(category,L4F_INFO,"start plot")
call init(plot,PSTYPE='PS', ORIENT='LANDSCAPE',COLOR='COLOR',file="example_space_utilities.ps")
call plot_triangles(plot,x,y,tri,"SIMC")
call delete(plot)

#endif

call delete(tri)

call l4f_category_log(category,L4F_INFO,"terminated")
!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END program
