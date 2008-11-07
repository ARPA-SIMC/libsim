program demo3

use gridinfo_class
use log4fortran
use grib_api
use volgrid6d_class

implicit none

integer :: category,ier
character(len=512):: a_name
type (gridinfo_type),pointer :: gridinfo(:),gridinfoout(:)
type (volgrid6d),pointer  :: volgrid(:)

integer                            ::  ifile
integer                            ::  iret
integer                            ::  gaid
integer  :: ngrib

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo3")

!imposta a_name
category=l4f_category_get(a_name//".main")

!init di log4fortran
ier=l4f_init()


ngrib=0

call grib_open_file(ifile, 'gribmix.grb','r')


call grib_count_in_file(ifile,ngrib)

call l4f_category_log(category,L4F_INFO,&
         "Numero totale di grib: "//to_char(ngrib))

allocate (gridinfo(ngrib))

ngrib=0

call grib_open_file(ifile, 'gribmix.grb','r')

! Loop on all the messages in a file.

!     a new grib message is loaded from file
!     gaid is the grib id to be used in subsequent calls

gaid=-1
call  grib_new_from_file(ifile,gaid, iret) 


LOOP: DO WHILE (iret == GRIB_SUCCESS)

   call l4f_category_log(category,L4F_INFO,"import gridinfo")

   ngrib=ngrib+1
   call init (gridinfo(ngrib),gaid=gaid)
   call import(gridinfo(ngrib))

   gaid=-1
   call grib_new_from_file(ifile,gaid, iret)
   
end do LOOP

call grib_close_file(ifile)

call display(gridinfo)

call l4f_category_log(category,L4F_INFO,"import")

call import (volgrid,gridinfo,categoryappend="volume di test")


call l4f_category_log(category,L4F_INFO,"delete gridinfo")

do ngrib=1,size(gridinfo)
   call delete (gridinfo(ngrib))
enddo

! qui posso fare tutti i conti possibili

call l4f_category_log(category,L4F_INFO,"export")

call export (volgrid,gridinfoout)


call grib_open_file(ifile, 'gribnew.grb','w')


do ngrib=1,size(gridinfoout)
   !     write the new message to a file
   call grib_write(gridinfoout(ngrib)%gaid,ifile)
end do

call grib_close_file(ifile)

call l4f_category_log(category,L4F_INFO,"delete gridinfo")

do ngrib=1,size(gridinfoout)
   call delete (gridinfoout(ngrib))
enddo

call l4f_category_log(category,L4F_INFO,"terminato ")

deallocate(gridinfo)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo3
