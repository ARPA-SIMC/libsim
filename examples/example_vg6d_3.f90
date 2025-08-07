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
use grid_id_class
use volgrid6d_class
use char_utilities
implicit none

integer :: ier
type(l4f_handle) :: category
character(len=512):: a_name
type(arrayof_gridinfo) :: gridinfoin, gridinfoout
type(volgrid6d),pointer  :: volgrid(:)

TYPE(grid_file_id) :: ifile
TYPE(grid_id) :: gaid, gaid_template
INTEGER :: ngrib

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo3")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get_handle(a_name//".main")


ngrib=0

ifile = grid_file_id_new('../data/in.grb','r')
ngrib = grid_file_id_count(ifile)

call l4f_category_log(category,L4F_INFO,&
 "Numero totale di grib: "//to_char(ngrib))

! aggiungo ngrib elementi vuoti
CALL insert(gridinfoin, nelem=ngrib)

ngrib=0

! Loop on all the messages in a file.
DO WHILE (.TRUE.)
  gaid = grid_id_new(ifile)
  IF (.NOT.c_e(gaid)) EXIT

  CALL l4f_category_log(category,L4F_INFO,"import gridinfoin")
  ngrib = ngrib + 1
  CALL init (gridinfoin%array(ngrib), gaid=gaid, categoryappend=TRIM(to_char(ngrib)))
  CALL import(gridinfoin%array(ngrib))
ENDDO

call delete(ifile)
call display(gridinfoin)

call l4f_category_log(category,L4F_INFO,"import")

call import(volgrid, gridinfoin, categoryappend="volume di test")

call l4f_category_log(category,L4F_INFO,"delete gridinfoin")

CALL delete(gridinfoin)

! qui posso fare tutti i conti possibili

gaid_template = grid_id_new(grib_api_template="regular_ll_sfc_grib1")

call l4f_category_log(category,L4F_INFO,"export a un grib fatto come voglio io")

call export(volgrid, gridinfoout, gaid_template=gaid_template)

ifile = grid_file_id_new('out.grb','w')

do ngrib=1,gridinfoout%arraysize
   !     write the new message to a file

   if(c_e(gridinfoout%array(ngrib)%gaid)) then
      call export(gridinfoout%array(ngrib))
      call export(gridinfoout%array(ngrib)%gaid,ifile)
   end if
end do

call delete(ifile)

call l4f_category_log(category,L4F_INFO,"terminato")

call delete(gridinfoout)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo3
