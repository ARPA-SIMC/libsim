program demo8

use log4fortran
USE vol7d_dballe_class
USE vol7d_class

implicit none

integer :: category,ier
character(len=512):: a_name,filename="synop_t.bufr"
TYPE(vol7d_dballe) :: v7d_dba

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo8")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d_dba,file=.true.,write=.false.,filename=filename,&
 categoryappend="importBUFR",format="BUFR")

call import (v7d_dba,var=(/"B12001"/),varkind=(/"r"/))

call l4f_category_log(category,L4F_INFO,"importato vol7d")

call display(v7d_dba%vol7d)

call l4f_category_log(category,L4F_INFO,"export to ana file")

open (unit=1,file="ana.txt",status="unknown",form="unformatted")

write(1)size(v7d_dba%vol7d%ana)
call write_unit(v7d_dba%vol7d%ana,unit=1)

close(unit=1)

call l4f_category_log(category,L4F_INFO,"terminato")

call delete (v7d_dba)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo8
