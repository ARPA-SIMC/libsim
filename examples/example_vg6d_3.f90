! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
program demo3

use gridinfo_class
use missing_values
use log4fortran
use grib_api
use volgrid6d_class
use char_utilities
implicit none

integer :: category,ier
character(len=512):: a_name
type (gridinfo_def),pointer :: gridinfo(:),gridinfoout(:)
type (volgrid6d),pointer  :: volgrid(:)

integer                            ::  ifile
integer                            ::  iret
integer                            ::  gaid,gaid_template
integer  :: ngrib

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo3")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")


ngrib=0

call grib_open_file(ifile, '../data/in.grb','r')


call grib_count_in_file(ifile,ngrib)

call l4f_category_log(category,L4F_INFO,&
         "Numero totale di grib: "//to_char(ngrib))

allocate (gridinfo(ngrib))

ngrib=0

! Loop on all the messages in a file.

!     a new grib message is loaded from file
!     gaid is the grib id to be used in subsequent calls

gaid=-1
call  grib_new_from_file(ifile,gaid, iret) 


LOOP: DO WHILE (iret == GRIB_SUCCESS)

   call l4f_category_log(category,L4F_INFO,"import gridinfo")

   ngrib=ngrib+1
   call init (gridinfo(ngrib),gaid=gaid,categoryappend=to_char(ngrib))
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

call grib_new_from_template (gaid_template,"regular_ll_sfc_grib1")

call l4f_category_log(category,L4F_INFO,"export a un grib fatto come voglio io")

call export (volgrid,gridinfoout,gaid_template=gaid_template)

call grib_open_file(ifile, 'out.grb','w')


do ngrib=1,size(gridinfoout)
   !     write the new message to a file

   if(c_e(gridinfoout(ngrib)%gaid)) then
      call export (gridinfoout(ngrib))
      call grib_write(gridinfoout(ngrib)%gaid,ifile)
      call delete (gridinfoout(ngrib))
   end if
end do

call grib_close_file(ifile)


call l4f_category_log(category,L4F_INFO,"terminato")

deallocate(gridinfo)
deallocate(gridinfoout)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo3
