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
program demo4

use gridinfo_class
use grid_class
use grid_transform_class
use log4fortran
use grid_id_class
use volgrid6d_class
use char_utilities

implicit none

integer :: category,ier
character(len=512):: a_name,infile='../data/in.grb',outfile='out.grb'
type (gridinfo_def) :: gridinfo
character:: ch
type(grid_file_id) :: ifile,ofile
type(grid_id) :: gaid
!integer                            ::  iret

integer :: ix,iy,fx,fy,iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy
doubleprecision ::  ilon=0.,ilat=30.,flon=30.,flat=60.
REAL, ALLOCATABLE :: field(:,:,:),fieldz(:,:,:)
type(griddim_def) :: griddim_out
type(transform_def) :: trans
type(grid_transform) :: grid_trans

integer :: nx=30,ny=30,component_flag=0
doubleprecision :: xmin=0., xmax=30., ymin=30., ymax=60.
doubleprecision :: latitude_south_pole=-32.5,longitude_south_pole=10.,angle_rotation=0.
character(len=80) :: type='regular_ll',trans_type='inter',sub_type='near'

doubleprecision ::x,y,lon,lat


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo4")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(infile))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(outfile))
if(trans_type == 'inter')then

  call init(griddim_out,&
   proj_type=type,nx=nx,ny=ny, &
   xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, component_flag=component_flag, &
   latitude_south_pole=latitude_south_pole,longitude_south_pole=longitude_south_pole,angle_rotation=angle_rotation, &
   categoryappend="regular_ll")

  call griddim_unproj(griddim_out)

  print*,'grid di interpolazione >>>>>>>>>>>>>>>>>>>>'
  call display(griddim_out)

end if

call init(trans, trans_type=trans_type,sub_type=sub_type, &
 ilon=ilon,ilat=ilat,flon=flon,flat=flat,&
 categoryappend="trasformation")

ifile = grid_file_id_new(trim(infile),'r')
ofile = grid_file_id_new(trim(outfile),'w')

! Loop on all the messages in a file.
DO WHILE (.TRUE.)
  gaid = grid_id_new(ifile)
  IF (.NOT.c_e(gaid)) EXIT

  call l4f_category_log(category,L4F_INFO,"import gridinfo")

  call init (gridinfo,gaid=gaid,categoryappend="importato")
  call import(gridinfo)

  call display(gridinfo,namespace="")

  call l4f_category_log(category,L4F_INFO,"import")

  ALLOCATE (field(gridinfo%griddim%dim%nx,gridinfo%griddim%dim%ny,1))

  field(:,:,1)=decode_gridinfo(gridinfo)

  call init(grid_trans, trans, in=gridinfo%griddim,out=griddim_out,categoryappend="gridtrasformato")

  call display(griddim_out)

  ALLOCATE (fieldz(griddim_out%dim%nx,griddim_out%dim%ny,1))

  call compute(grid_trans, field, fieldz)

  call delete(gridinfo%griddim)
  call copy(griddim_out,gridinfo%griddim,categoryappend="clonato")

! oppure per mantenere il vecchio gridinfo
!   call clone(gridinfo , gridinfo_out)
!   call delete(gridinfo_out%griddim)
!   call copy(griddim_out,gridinfo_out%griddim)

  call encode_gridinfo(gridinfo,fieldz(:,:,1))
  call export (gridinfo)
  call display(gridinfo,namespace="")

  call export(gridinfo%gaid,ofile)

  call delete (grid_trans)
  call delete (gridinfo)
  deallocate (field,fieldz)
   
end do

call delete (trans)
call delete(griddim_out)

call delete(ifile)
call delete(ofile)

call l4f_category_log(category,L4F_INFO,"terminato")

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo4

