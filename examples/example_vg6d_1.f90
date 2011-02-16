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
program demo

use grid_class
use char_utilities
use log4fortran

implicit none

integer :: category,ier
character(len=512):: a_name
doubleprecision :: val

type (griddim_def) :: griddim

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo1")

!imposta a_name
category=l4f_category_get(a_name//".main")

!init di log4fortran
ier=l4f_init()

!imposto i dati su grigliato

call init (griddim,proj_type="regular_ll", &
 nx = 10,ny = 15, &
 xmin = -2.D0, &
 xmax = 16.D0, &
 ymin = 37.D0, &
 ymax = 51.D0, &
 component_flag=1,&
 categoryappend="grigliato regolare manuale")


call griddim_unproj(griddim)

call l4f_category_log(category,L4F_INFO,&
         "unproj ritorna "//to_char(griddim%dim%lat(1,1))//to_char(griddim%dim%lon(1,1)))

call get_val(griddim,ymax=val)

call l4f_category_log(category,L4F_INFO,&
         "get_val ritorna "//to_char(val))

call display(griddim)

call delete(griddim)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo
