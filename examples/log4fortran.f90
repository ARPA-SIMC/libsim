program testlog

use log4fortran

integer :: category,ier

ier=log4fortran_init()
print *,ier

category=log4fortran_category_get("test")

call log4fortran_category_log(category,L4F_ERROR,"ciao bello !")
call log4fortran_category_log(category,L4F_INFO,"me ne vado")

call log4fortran_category_delete(category)

ier=log4fortran_fini()
print*,ier

end program testlog
