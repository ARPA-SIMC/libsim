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

!!!!!!!!!!!!!!!!
!  Example to create a grib editionNumber = 2 file from data generated in memory using a grib_api template.
!!!!!!!!!!!!!!!!

program demo9

use log4fortran
use gridinfo_class
use grid_class
use grid_id_class

USE vol7d_level_class
USE vol7d_timerange_class
USE volgrid6d_var_class
USE datetime_class


implicit none

integer :: category,ier,i
character(len=512):: a_name,filename="out.grib"
TYPE(gridinfo_def) :: gridinfo

type(griddim_def) :: griddim

integer,parameter :: nx=40, ny=50, component_flag=0
type(grid_id) :: gaid_template
type(vol7d_level) :: level
type(vol7d_timerange) :: timerange
type(volgrid6d_var) :: var
type(datetime) :: date_time
doubleprecision :: xmin=0., xmax=30., ymin=30., ymax=60.
!doubleprecision :: latitude_south_pole=-32.5,longitude_south_pole=10.,angle_rotation=0.
character(len=80) :: type='regular_ll'
real :: field(nx,ny)=5.

! get launcher name
call l4f_launcher(a_name,a_name_force="demo9")

! log4fortran init
ier=l4f_init()

! set a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"start")

call init(griddim,&
 proj_type=type,nx=nx,ny=ny, &
 xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, component_flag=component_flag, &
 !latitude_south_pole=latitude_south_pole,longitude_south_pole=longitude_south_pole,angle_rotation=angle_rotation, &
 categoryappend="generated")
gaid_template = grid_id_new(grib_api_template="regular_ll_sfc_grib2")
call init(date_time,year=2011, month=04, day=12, hour=12, minute=00, msec=00)
call init(timerange, timerange=4, p1=3600, p2=900)
call init(level, level1=105, l1=200, level2=imiss, l2=imiss)
call init(var,centre=200, category=3, number=61, discipline=4)
call init (gridinfo, gaid_template, griddim, date_time, timerange, level, var, clone=.false., categoryappend='inventato')

! here you can change the default template
call grib_set(grid_id_get_gaid(gridinfo%gaid),"generatingProcessIdentifier",178)

!encode the data
call encode_gridinfo (gridinfo, field)

call display(gridinfo)
call l4f_category_log(category,L4F_INFO,"export to GRIB")

CALL export((/gridinfo/), filename=filename, categoryappend="gridinfo scritto")

call l4f_category_log(category,L4F_INFO,"end")

call delete (gridinfo)

! close logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo9
