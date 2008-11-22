program demo4

use gridinfo_class
use grid_class
use log4fortran
use grib_api
use volgrid6d_class
use char_utilities
implicit none

integer :: category,ier
character(len=512):: a_name
type (gridinfo_type) :: gridinfo

integer                            ::  ifile,ofile,gaid
integer                            ::  iret

integer :: ix,iy,fx,fy,iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy
real, allocatable :: field(:,:),fieldz(:,:)


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo4")

!imposta a_name
category=l4f_category_get(a_name//".main")

!init di log4fortran
ier=l4f_init()


call grib_open_file(ifile, 'gribmix.grb','r')
call grib_open_file(ofile, 'gribnew.grb','w')


! Loop on all the messages in a file.

!     a new grib message is loaded from file
!     gaid is the grib id to be used in subsequent calls

gaid=-1
call  grib_new_from_file(ifile,gaid, iret) 


DO WHILE (iret == GRIB_SUCCESS)

   call l4f_category_log(category,L4F_INFO,"import gridinfo")

   call init (gridinfo,gaid=gaid,categoryappend="test")
   call import(gridinfo)

   gaid=-1
   call grib_new_from_file(ifile,gaid, iret)
   
   call display(gridinfo)

   call l4f_category_log(category,L4F_INFO,"import")

   ix=-5
   iy=-6
   fx=100
   fy=150

   allocate (field(gridinfo%griddim%dim%nx,gridinfo%griddim%dim%ny))

   field=decode_gridinfo(gridinfo)

   call zoom_index(gridinfo%griddim,gridinfo%griddim,ix,iy,fx,fy,&
    iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy) 

   allocate (fieldz(newx,newy))
   
   call zoom_field(field,fieldz,iox,ioy,fox,foy,inx,iny,fnx,fny) 
   
   call encode_gridinfo(gridinfo,fieldz)
   call export (gridinfo)

   call grib_write(gridinfo%gaid,ofile)
   call delete (gridinfo)

   deallocate (field,fieldz)

end do

call grib_close_file(ifile)
call grib_close_file(ofile)

call l4f_category_log(category,L4F_INFO,"terminato")

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo4
