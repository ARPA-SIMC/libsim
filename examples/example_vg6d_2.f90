program demo2

use gridinfo_class
use log4fortran
use grib_api

implicit none

integer :: category,ier
character(len=512):: a_name
type (gridinfo_def),allocatable :: gridinfo(:)

integer                            ::  ifile
integer                            ::  iret
integer                            ::  gaid
integer  :: ngrib

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo2")

!imposta a_name
category=l4f_category_get(a_name//".main")

!init di log4fortran
ier=l4f_init()


ngrib=0

call grib_open_file(ifile, '../data/in.grb','r')

! Loop on all the messages in a file.

call  grib_new_from_file(ifile,gaid, iret) 

DO WHILE (iret == GRIB_SUCCESS)

   ngrib=ngrib+1
   call grib_release(gaid)
   call grib_new_from_file(ifile,gaid, iret)
   
end do

call l4f_category_log(category,L4F_INFO,&
         "Numero totale di grib: "//to_char(ngrib))

call grib_close_file(ifile)

allocate (gridinfo(ngrib))

ngrib=0

call grib_open_file(ifile, '../data/in.grb','r')

! Loop on all the messages in a file.

!     a new grib message is loaded from file
!     gaid is the grib id to be used in subsequent calls
call  grib_new_from_file(ifile,gaid, iret) 


LOOP: DO WHILE (iret == GRIB_SUCCESS)

   call l4f_category_log(category,L4F_INFO,"import grib")

   ngrib=ngrib+1

   call init (gridinfo(ngrib),gaid=gaid)
   call import(gridinfo(ngrib))

   call grib_new_from_file(ifile,gaid, iret)
   
end do LOOP

call grib_close_file(ifile)


call display (gridinfo)


do ngrib=1,size(gridinfo)

   call delete (gridinfo(ngrib))

enddo

call l4f_category_log(category,L4F_INFO,"terminato ")

deallocate(gridinfo)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo2
