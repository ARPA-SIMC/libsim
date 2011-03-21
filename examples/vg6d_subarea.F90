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
PROGRAM vg6d_subarea
#include "config.h"
use gridinfo_class
use grid_class
use grid_transform_class
use log4fortran
use grid_id_class
use grib_api
use volgrid6d_class
use char_utilities
use optionparser_class

implicit none

integer :: category,ier
character(len=512):: a_name,infile='in.grb',outfile='out.grb'
type (gridinfo_def) :: gridinfo
TYPE(grid_file_id) :: ifile,ofile
TYPE(grid_id) :: input_grid_id
INTEGER :: gaid

doubleprecision ::  ilon,ilat,flon,flat
REAL, ALLOCATABLE :: field(:,:,:),fieldz(:,:,:)
type(griddim_def) :: griddim_out
type(transform_def) :: trans
type(grid_transform) :: grid_trans

integer :: nx,ny,component_flag,npx,npy
doubleprecision :: xmin, xmax, ymin, ymax
INTEGER :: ix, iy, fx, fy
doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation
character(len=80) :: type,trans_type,sub_type
CHARACTER(len=3) :: set_scmode
LOGICAL :: version, ldisplay

doubleprecision ::x,y,lon,lat
type(optionparser) :: opt
integer :: optind
integer :: iargc

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="subarea")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Grib to grib trasformation application. It reads grib edition 1 and 2 &
 &and zooms, interpolates or regrids data according to optional parameters.', &
 usage_msg='Usage: vg6d_subarea [options] inputfile outputfile')

! define command-line options
CALL optionparser_add(opt, 'v', 'trans-type', trans_type, 'inter', help= &
 'transformation type: ''inter'' for interpolation, ''zoom'' for zooming, ''boxregrid'' for resolution reduction')
CALL optionparser_add(opt, 'z', 'sub-type', sub_type, 'near', help= &
 'transformation subtype, for inter: ''near'', ''bilin'', &
 &for ''boxinter'' and ''boxregrid'': ''average'', ''max'', ''min'', &
 &for zoom: ''index'', ''coord'', ''coordbb''')

CALL optionparser_add(opt, 'u', 'type', type, 'regular_ll', help= &
 'type of interpolated grid: ''regular_ll'', ''rotated_ll''')
CALL optionparser_add(opt, 'i', 'nx', nx, 31, help= &
 'number of nodes along x axis on interpolated grid')
CALL optionparser_add(opt, 'l', 'ny', ny, 31, help= &
 'number of nodes along y axis on interpolated grid')
CALL optionparser_add(opt, 'm', 'x-min', xmin, 0.0D0, help= &
 'x coordinate of the lower left corner of interpolated grid')
CALL optionparser_add(opt, 'o', 'y-min', ymin, 30.0D0, help= &
 'y coordinate of the lower left corner of interpolated grid')
CALL optionparser_add(opt, 'n', 'x-max', xmax, 30.0D0, help= &
 'x coordinate of the upper right corner of interpolated grid')
CALL optionparser_add(opt, 'p', 'y-max', ymax, 60.0D0, help= &
 'y coordinate of the upper right corner of interpolated grid')

CALL optionparser_add(opt, 'q', 'latitude-south-pole', latitude_south_pole, &
 -32.5D0, help='latitude of south pole for rotated grid')
CALL optionparser_add(opt, 'r', 'longitude-south-pole', longitude_south_pole, &
 10.0D0, help='longitude of south pole for rotated grid')
CALL optionparser_add(opt, 's', 'angle-rotation', angle_rotation, &
 0.0D0, help='angle of rotation for rotated grid')

CALL optionparser_add(opt, 'a', 'ilon', ilon, 0.0D0, help= &
 'longitude of the southwestern zooming/bounding box corner')
CALL optionparser_add(opt, 'b', 'ilat', ilat, 30.D0, help= &
 'latitude of the southwestern zooming/bounding box corner')
CALL optionparser_add(opt, 'c', 'flon', flon, 30.D0, help= &
 'longitude of the northeastern zooming/bounding box corner')
CALL optionparser_add(opt, 'd', 'flat', flat, 60.D0, help= &
 'latitude of the northeastern zooming/bounding box corner')
CALL optionparser_add(opt, ' ', 'ix', ix, 1, help= &
 'x-index of the southwestern zooming corner')
CALL optionparser_add(opt, ' ', 'iy', iy, 1, help= &
 'y-index of the southwestern zooming corner')
CALL optionparser_add(opt, ' ', 'fx', fx, 31, help= &
 'x-index of the northeastern zooming corner')
CALL optionparser_add(opt, ' ', 'fy', fy, 31, help= &
 'y-index of the northeastern zooming corner')

CALL optionparser_add(opt, 'f', 'npx', npx, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')
CALL optionparser_add(opt, 'g', 'npy', npy, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')

!CALL optionparser_add(opt, 'e', 'a-grid', c2agrid, help= &
! 'interpolate U/V points of an Arakawa C grid on the corresponding T points &
! &of an Arakawa A grid')

!CALL optionparser_add(opt, 't', 'component-flag', component_flag, &
! 0, help='wind component flag in interpolated grid, 0=wind components referred to &
! &geographic E an N directions, 1=wind components referred to grid x and y &
! &directions')
! impossible to change it now, the value of input grid will be kept in output,
! but results are wrong if grids are differently oriented and vector fields
! are interpolated
component_flag = 0

CALL optionparser_add(opt, ' ', 'set-scmode', set_scmode, 'xxx', &
 help='set output grid scanning mode to a particular standard value: &
 &3 binary digits indicating respectively iScansNegatively, jScansPositively and &
 &jPointsAreConsecutive (grib_api jargon), 0 for false, 1 for true, &
 &000 for ECMWF-like grids, 010 for COSMO and Cartesian-like grids. &
 &Any other character indicates to keep the &
 &corresponding original scanning mode value')

! display option
CALL optionparser_add(opt, ' ', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
optind = optionparser_parse(opt)
IF (optind <= 0) THEN
  CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters')
  CALL EXIT(1)
ENDIF

IF (version) THEN
  WRITE(*,'(A,1X,A)')'vg6d_subarea',VERSION
  CALL exit(0)
ENDIF

if (optind <= iargc()) then
  call getarg(optind, infile)
  optind=optind+1
else
  call l4f_category_log(category,L4F_ERROR,'input file missing')
  call optionparser_printhelp(opt)
  call exit(1)
end if

if (optind <= iargc()) then
  call getarg(optind, outfile)
  optind=optind+1
else
  call l4f_category_log(category,L4F_ERROR,'output file missing')
  call optionparser_printhelp(opt)
  call exit(1)
end if

CALL delete(opt)

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(infile))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(outfile))

if(trans_type == 'inter')then

  call init(griddim_out, proj_type=type, nx=nx, ny=ny, &
   xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, &
   component_flag=component_flag, latitude_south_pole=latitude_south_pole, &
   longitude_south_pole=longitude_south_pole, angle_rotation=angle_rotation, &
   categoryappend="requested_grid")

  call griddim_unproj(griddim_out)

  IF (ldisplay) THEN
    PRINT*,'output grid >>>>>>>>>>>>>>>>>>>>'
    CALL display(griddim_out)
  ENDIF

end if

call init(trans, trans_type=trans_type, sub_type=sub_type, &
 ix=ix, iy=iy, fx=fx, fy=fy, &
 ilon=ilon, ilat=ilat, flon=flon, flat=flat, npx=npx, npy=npy, &
 percentile=0.5D0, categoryappend="transformation")

ifile = grid_file_id_new(infile,'r')
ofile = grid_file_id_new(outfile,'w')

DO WHILE (.TRUE.)
  input_grid_id = grid_id_new(ifile)
  IF (.NOT.c_e(input_grid_id)) THEN ! THEN because of a bug in gfortran?!
    EXIT
  ENDIF

  call l4f_category_log(category,L4F_INFO,"import gridinfo")

  call init(gridinfo, gaid=input_grid_id, categoryappend="imported")
  call import(gridinfo)

  IF (ldisplay) THEN
    CALL display(gridinfo,namespace="ls")
  ENDIF

  call l4f_category_log(category,L4F_INFO,"import")

  ALLOCATE (field(gridinfo%griddim%dim%nx,gridinfo%griddim%dim%ny,1))

  field(:,:,1)=decode_gridinfo(gridinfo)

  call init(grid_trans, trans, in=gridinfo%griddim, out=griddim_out,&
   categoryappend="gridtransformed")

  IF (ldisplay) THEN
    CALL display(griddim_out)
  ENDIF

  ALLOCATE (fieldz(griddim_out%dim%nx,griddim_out%dim%ny,1))

  call compute(grid_trans, field, fieldz)

  call delete(gridinfo%griddim)
  call copy(griddim_out,gridinfo%griddim,categoryappend="cloned")

! oppure per mantenere il vecchio gridinfo
!   call clone(gridinfo , gridinfo_out)
!   call delete(gridinfo_out%griddim)
!   call copy(griddim_out,gridinfo_out%griddim)

! per bug della grib_api
!   call grib_release(gridinfo%gaid)
!   call grib_new_from_template(gridinfo%gaid,"regular_ll_pl_grib1")

! set scanning mode if grib_api
  IF (grid_id_get_driver(gridinfo%gaid) == 'grib_api') THEN
    gaid = grid_id_get_gaid(gridinfo%gaid)
    IF (set_scmode(1:1) == '0') THEN
      CALL grib_set(gaid, 'iScansNegatively', 0)
    ELSE IF (set_scmode(1:1) == '1') THEN
      CALL grib_set(gaid, 'iScansNegatively', 1)
    ENDIF
    IF (set_scmode(2:2) == '0') THEN
      CALL grib_set(gaid, 'jScansPositively', 0)
    ELSE IF (set_scmode(2:2) == '1') THEN
      CALL grib_set(gaid, 'jScansPositively', 1)
    ENDIF
    IF (set_scmode(3:3) == '0') THEN
      CALL grib_set(gaid, 'jPointsAreConsecutive', 0)
    ELSE IF (set_scmode(3:3) == '1') THEN
      CALL grib_set(gaid, 'jPointsAreConsecutive', 1)
    ENDIF
  ENDIF

  CALL encode_gridinfo(gridinfo, fieldz(:,:,1))
  call export(gridinfo)
  IF (ldisplay) THEN
    CALL display(gridinfo,namespace="ls")
  ENDIF

  CALL export(gridinfo%gaid, ofile)

  call delete(grid_trans)
  call delete(gridinfo)
  DEALLOCATE(field, fieldz)

end do

call delete(trans)
call delete(griddim_out)

call delete(ifile)
call delete(ofile)

call l4f_category_log(category,L4F_INFO,"end")

! Close the logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM vg6d_subarea
