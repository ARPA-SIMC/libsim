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

PROGRAM demo9
USE log4fortran
USE gridinfo_class
USE grid_class
USE grid_id_class
USE vol7d_level_class
USE vol7d_timerange_class
USE volgrid6d_var_class
USE datetime_class
IMPLICIT NONE

integer :: ier
type(l4f_handle) :: category
character(len=512):: a_name
TYPE(arrayof_gridinfo) :: gridinfo

type(griddim_def) :: griddim

integer,parameter :: nx=31, ny=16, component_flag=0
type(grid_id) :: gaid_template
type(vol7d_level) :: level
type(vol7d_timerange) :: timerange
type(volgrid6d_var) :: var
type(datetime) :: date_time
doubleprecision :: xmin=0., xmax=30., ymin=35., ymax=50.
!doubleprecision :: latitude_south_pole=-32.5,longitude_south_pole=10.,angle_rotation=0.
character(len=80) :: type='regular_ll'
REAL :: field(nx,ny)
INTEGER :: i, j

! get launcher name
call l4f_launcher(a_name,a_name_force="demo9")

! log4fortran init
ier=l4f_init()

! set a_name
category=l4f_category_get_handle(TRIM(a_name)//".main")

call l4f_category_log(category,L4F_INFO,"start")

! make room for two elements
CALL insert(gridinfo, nelem=2)
! define grib1 template
gaid_template = grid_id_new(grib_api_template="regular_ll_sfc_grib1")
! here you can change the default template
CALL grib_set(grid_id_get_gaid(gaid_template), "centre", 80)
CALL grib_set(grid_id_get_gaid(gaid_template), "jScansPositively", 1)

! first element
! define metadata
CALL init(griddim, proj_type=type, nx=nx, ny=ny, &
 xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, component_flag=component_flag, &
 categoryappend="generated")
CALL init(date_time, year=2019, month=1, day=20, hour=0, minute=0)
CALL init(timerange, timerange=254, p1=0, p2=0)
CALL init(level, level1=1, l1=0, level2=imiss, l2=imiss)
! define parameter land fraction
CALL init(var, centre=80, category=2, number=81, discipline=255)
! fill gridinfo with all metadata
CALL init(gridinfo%array(1), gaid_template, griddim, date_time, timerange, level, &
 var, clone=.TRUE., categoryappend='inventato')

! define data
! create a N-S coastline at the center of the domain
field(:nx/2,:) = 1.
field(nx/2+1,:) = 0.
CALL encode_gridinfo(gridinfo%array(1), field)

! second element
! define parameter geometric height
CALL init(var, centre=80, category=2, number=8, discipline=255)
! fill gridinfo with all metadata
CALL init(gridinfo%array(2), gaid_template, griddim, date_time, timerange, level, &
 var, clone=.TRUE., categoryappend='inventato')

! define data
! create an E-W slope W of the coastline, 0 at the E
DO i = 1, nx
  field(i,:) = MAX(REAL(nx/2-i+1)/REAL(nx/2)*500., 0.)
ENDDO
CALL encode_gridinfo(gridinfo%array(2), field)

CALL display(gridinfo)
CALL l4f_category_log(category,L4F_INFO,"export to GRIB")
CALL export(gridinfo, filename='const.grib', categoryappend="gridinfo scritto")

CALL delete(gaid_template)
CALL delete(gridinfo)

! make room for one element
CALL insert(gridinfo, nelem=1)
! define grib2 template
gaid_template = grid_id_new(grib_api_template="regular_ll_sfc_grib2")

! redefine some metadata
CALL init(timerange, timerange=254, p1=6, p2=0)
CALL init(level, level1=103, l1=2000, level2=imiss, l2=imiss)
! define parameter temperature
CALL init(var, centre=80, category=0, number=0, discipline=0)
! fill gridinfo with all metadata
CALL init(gridinfo%array(1), gaid_template, griddim, date_time, timerange, level, &
 var, clone=.FALSE., categoryappend='inventato')
! here you can change the template, after cloning but before coding metadata
! different approach than above
CALL grib_set(grid_id_get_gaid(gridinfo%array(1)%gaid), "centre", 80)
CALL grib_set(grid_id_get_gaid(gridinfo%array(1)%gaid), "jScansPositively", 1)

! define data
! create an unrealistic field with all different values
DO j = 1, ny
  DO i = 1, nx
    field(i,j) = 200. + (i-1)*0.5 + j*15.
   ENDDO
ENDDO
CALL encode_gridinfo(gridinfo%array(1), field)

CALL display(gridinfo)
CALL l4f_category_log(category,L4F_INFO,"export to GRIB")
CALL export(gridinfo, filename='t2m.grib', categoryappend="gridinfo scritto")

CALL l4f_category_log(category,L4F_INFO,"end")

CALL delete(gaid_template)
CALL delete(gridinfo)

! close logger
CALL l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM demo9
