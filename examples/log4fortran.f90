program testlog

use log4fortran

integer :: category,ier
character(len=512):: a_name

!questa chiamata prende dal launcher il nome univoco
call log4fortran_launcher(a_name)

!init di log4fortran
ier=log4fortran_init()

!change the default verbosity level (dummy routine only !)
log4fortran_priority=L4F_DEBUG

!imposta a_name
category=log4fortran_category_get(a_name)

call log4fortran_category_log(category,L4F_ERROR,"erroraccio in log4fortran")
call log4fortran_category_log(category,L4F_INFO,"info in log4fortran")

! aggiungo una comunicazione in stderr
write(0,*) "erroraccio in stderr"

!chiudo il logger
call log4fortran_category_delete(category)
ier=log4fortran_fini()

!aggiungo una comunicazione in stdout
write(6,*) "log4fortran_fini",ier

end program testlog
