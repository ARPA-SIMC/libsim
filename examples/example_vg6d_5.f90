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
program demo5

use log4fortran
use grib_api
use volgrid6d_class
use grid_class
use grid_transform_class
use char_utilities
implicit none

integer :: ier,gaid_template
type(l4f_handle) :: category
integer :: i,j
character(len=512):: a_name
type (volgrid6d),pointer  :: volgrid(:),volgrid_out(:)
type(transform_def) :: trans

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo5")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get_handle(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

call import (volgrid,filename="../data/in.grb",categoryappend="volume letto")

! qui posso fare tutti i conti possibili
!!$print*,"-------------"
!!$
!!$call griddim_unproj(volgrid(1)%griddim)
!!$
!!$print*,"sotto"
!!$
!!$do i=1,volgrid(1)%griddim%dim%nx,10
!!$  j=1
!!$  print*,volgrid(1)%griddim%dim%lon(i,j),volgrid(1)%griddim%dim%lat(i,j)
!!$end do
!!$
!!$print*,"destra"
!!$
!!$do j=1,volgrid(1)%griddim%dim%ny,10
!!$  i=volgrid(1)%griddim%dim%nx
!!$  print*,volgrid(1)%griddim%dim%lon(i,j),volgrid(1)%griddim%dim%lat(i,j)
!!$end do
!!$
!!$print*,"sopra"
!!$
!!$do i=volgrid(1)%griddim%dim%nx,1,-10
!!$  j=volgrid(1)%griddim%dim%ny
!!$  print*,volgrid(1)%griddim%dim%lon(i,j),volgrid(1)%griddim%dim%lat(i,j)
!!$end do
!!$
!!$print*,"sinistra"
!!$
!!$do j=volgrid(1)%griddim%dim%ny,1,-10
!!$  i=1
!!$  print*,volgrid(1)%griddim%dim%lon(i,j),volgrid(1)%griddim%dim%lat(i,j)
!!$end do
!!$
!!$
!!$print*,"-------------"

call init(trans, trans_type="zoom",sub_type="coord", &
 ilon=-10.d0,ilat=40.d0,flon=10.d0,flat=50.d0,&
 categoryappend="trasformation")

!call transform(trans,griddim, volgrid, volgrid_out,categoryappend="trasforma")
call transform(trans, volgrid6d_in=volgrid, volgrid6d_out=volgrid_out,clone=.true.,categoryappend="trasforma")

call display(volgrid_out(1)%griddim)

call l4f_category_log(category,L4F_INFO,"trasformato")

if (associated(volgrid)) call delete(volgrid)

!call grib_new_from_template (gaid_template,"regular_ll_sfc_grib1")

call l4f_category_log(category,L4F_INFO,"export a un nuovo file grib")

!call export (volgrid,filename="out.grb",gaid_template=gaid_template,categoryappend="exportazione")
call export (volgrid_out,filename="out.grb",categoryappend="exportazione")

call l4f_category_log(category,L4F_INFO,"terminato")

if (associated(volgrid_out)) call delete(volgrid_out)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo5
