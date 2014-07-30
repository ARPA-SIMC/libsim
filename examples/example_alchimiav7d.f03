program alchimiav7d

USE alchimia
USE termo
USE vol7d_class
USE vol7d_dballe_class
USE vol7d_alchimia_class
USE vol7d_var_class
use log4fortran

IMPLICIT NONE
type(fndsv) :: vfn,vfnoracle
character(len=10), allocatable:: mybout(:)
type(vol7d_dballe) :: myin,myout
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
filenamein="../data/example_temp.bufr"
filenameout="../data/tp.bufr"

call register_termo(vfn)

call init(myin,filename=filenamein, file=.true., categoryappend="input")
call init(myout,filename=filenameout, file=.true., write=.true., wipe=.true., categoryappend="output",template="generic")

!CALL import(myin,var=(/"B12101","B10004"/),varkind=(/"r","r"/))
CALL import(myin)

call display(myin%vol7d)

if (alchemy(myin%vol7d,vfn,mybout,myout%vol7d,copy=.true.,vfnoracle=vfnoracle) /= 0 ) then
  print*, "I cannot make ",mybout
  
  if (.not. shoppinglist(mybout,vfn,vfnoracle)) then
    print*, " error shoppinglist"
    stop 2
  else
        call display(compile_sl(vfnoracle))
    stop 3
  end if
end if

call display(vfnoracle)

call display(myout%vol7d)
call export(myout)

call delete(myout)
call delete(myin)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program alchimiav7d
