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
program demo2

use gridinfo_class
use char_utilities
use log4fortran
use grid_id_class

implicit none

integer :: category,ier
character(len=512):: a_name
type(gridinfo_def),allocatable :: gridinfo(:)

TYPE(grid_file_id) :: ifile
TYPE(grid_id) :: gaid
INTEGER :: ngrib

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo2")

!imposta a_name
category=l4f_category_get(a_name//".main")

!init di log4fortran
ier=l4f_init()


ngrib=0

ifile = grid_file_id_new('../data/in.grb','r')
! Loop on all the messages in a file.
DO WHILE (.TRUE.)
  gaid = grid_id_new(ifile)
  IF (.NOT.c_e(gaid)) EXIT

  ngrib = ngrib + 1
  CALL delete(gaid)
ENDDO

CALL delete(ifile)

CALL l4f_category_log(category,L4F_INFO,&
 "Numero totale di grib: "//to_char(ngrib))

allocate(gridinfo(ngrib))

ngrib=0

ifile = grid_file_id_new('../data/in.grb','r')
! Loop on all the messages in a file.
DO WHILE (.TRUE.)
  gaid = grid_id_new(ifile)
  IF (.NOT.c_e(gaid)) EXIT

  CALL l4f_category_log(category,L4F_INFO,"import grib")
  ngrib = ngrib + 1
  CALL init(gridinfo(ngrib),gaid=gaid)
  CALL import(gridinfo(ngrib))
ENDDO


call delete(ifile)
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
