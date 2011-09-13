program alchimiavg6d

USE alchimia
USE termo
USE volgrid6d_class
USE volgrid6d_alchimia_class
use log4fortran

IMPLICIT NONE

type(fndsv) :: vfn
type(fndsv),allocatable :: vfnoracle(:)
character(len=10), allocatable:: mybout(:)
type(volgrid6d),pointer :: myin(:),myout(:)

character(len=255) :: filenamein,filenameout

integer :: category,ier
character(len=512):: a_name

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name)

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name)

call l4f_category_log(category,L4F_INFO,"Start")

mybout = [character(len=10) :: "B12192"]
filenamein="../data/t_p.grb"
filenameout="../data/tp.grb"

call register_termo(vfn)

CALL import(myin,filename=filenamein,decode=.true., time_definition=0, categoryappend="input")

if (alchemy(myin,vfn,mybout,myout,copy=.true.,vfnoracle=vfnoracle) /= 0) stop 1

call display(vfnoracle)

call export(myout,filenameout)

call delete(myout)
call delete(myin)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program alchimiavg6d
