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
program demo6

use log4fortran
use volgrid6d_class
use grid_class
use grid_transform_class
USE vol7d_dballe_class
USE vol7d_class

implicit none

integer :: category,ier,i,nana
character(len=512):: a_name,filename="out.bufr"
type (volgrid6d),pointer  :: volgrid(:),volgrid_out(:)
type(transform_def) :: trans
type(vol7d) :: v7d
type(vol7d) :: vol7d_out
TYPE(vol7d_dballe) :: v7d_exp


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo6")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

!!$ nana=5
!!$ allocate(ana(nana))
!!$ !target points 
!!$ call init(ana(1),lat=45.D0,lon=11.D0)
!!$ call init(ana(2),lat=45.6D0,lon=11.8D0)
!!$ call init(ana(3),lat=46.6D0,lon=12.8D0)
!!$ call init(ana(4),lat=40.6D0,lon=11.8D0)
!!$ call init(ana(5),lat=40.0D0,lon=10.0D0)

call init(v7d)

call import (v7d,filename="ana.v7d")

call display(v7d)

!trasformation object
call init(trans, trans_type="inter",sub_type="bilin", categoryappend="trasformation")

call import (volgrid,filename="in.grb",categoryappend="volume letto")

call transform(trans, volgrid6d_in=volgrid, vol7d_out=vol7d_out, v7d=v7d, categoryappend="trasforma")

call l4f_category_log(category,L4F_INFO,"trasformato")

if (associated(volgrid)) call delete(volgrid)

call l4f_category_log(category,L4F_INFO,"export to BUFR")


! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_exp,file=.true.,write=.true.,wipe=.true.,filename=filename,&
categoryappend="exportBUFR",format="BUFR",TEMPLATE="synop")

CALL display(vol7d_out)
v7d_exp%vol7d = vol7d_out
CALL export(v7d_exp)

CALL l4f_category_log(category,L4F_INFO,"terminato")

CALL delete (v7d_exp)

!chiudo il logger
CALL l4f_category_delete(category)
ier=l4f_fini()

end program demo6
