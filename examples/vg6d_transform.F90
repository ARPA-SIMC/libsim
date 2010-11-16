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
PROGRAM vg6d_transform
#include "config.h"
use log4fortran
use volgrid6d_class
use grid_class
use grid_transform_class
use grid_id_class
use err_handling
use char_utilities
use getopt_m

implicit none

integer :: category,ier,i,nana
character(len=512):: a_name,infile,outfile
type (volgrid6d),pointer  :: volgrid(:),volgrid_out(:)

doubleprecision ::  ilon,ilat,flon,flat

type(griddim_def) :: griddim_out
type(transform_def) :: trans
type(grid_transform) :: grid_trans

integer :: nx,ny,component_flag,npx,npy
doubleprecision :: lon_min, lon_max, lat_min, lat_max
INTEGER :: ix, iy, fx, fy
doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation
character(len=80) :: type,trans_type,sub_type

doubleprecision ::x,y,lon,lat
logical :: c2agrid
type(op_option) :: options(40) ! remember to update dimension when adding options
type(optionparser) :: opt
integer :: iargc
!CHARACTER(len=3) :: set_scmode
LOGICAL :: version, ldisplay
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(grid_file_id) :: file_template
TYPE(grid_id) :: gaid_template

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="volgrid6dtransform")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

! define command-line options
CALL op_option_nullify(options)

options(1) = op_option_new('v', 'trans-type', trans_type, 'none', help= &
 'transformation type: ''inter'' for interpolation, ''boxinter'' for &
 &statistical interpolation on boxes, ''zoom'' for zooming, &
 &''boxregrid'' for resolution reduction, ''none'' for no operation')
options(2) = op_option_new('z', 'sub-type', sub_type, 'near', help= &
 'transformation subtype, for inter: ''near'', ''bilin'', &
 &for ''boxinter'' and ''boxregrid'': ''average'', ''max'', ''min'', &
 &for zoom: ''index'', ''coord'', ''coordbb''')

options(3) = op_option_new('u', 'type', type, 'regular_ll', help= &
 'type of interpolated grid: ''regular_ll'', ''rotated_ll''')
options(4) = op_option_new('i', 'nx', nx, 31, help= &
 'number of nodes along x axis on interpolated grid')
options(5) = op_option_new('l', 'ny', ny, 31, help= &
 'number of nodes along y axis on interpolated grid')
options(6) = op_option_new('m', 'x-min', lon_min, 0.0D0, help= &
 'x coordinate of the lower left corner of interpolated grid')
options(7) = op_option_new('o', 'y-min', lat_min, 30.0D0, help= &
 'y coordinate of the lower left corner of interpolated grid')
options(8) = op_option_new('n', 'x-max', lon_max, 30.0D0, help= &
 'x coordinate of the upper right corner of interpolated grid')
options(9) = op_option_new('p', 'y-max', lat_max, 60.0D0, help= &
 'y coordinate of the upper right corner of interpolated grid')

options(10) = op_option_new('q', 'latitude-south-pole', latitude_south_pole, &
 -32.5D0, help='latitude of south pole for rotated grid')
options(11) = op_option_new('r', 'longitude-south-pole', longitude_south_pole, &
 10.0D0, help='longitude of south pole for rotated grid')
options(12) = op_option_new('s', 'angle-rotation', angle_rotation, &
 0.0D0, help='angle of rotation for rotated grid')

options(13) = op_option_new('a', 'ilon', ilon, 0.0D0, help= &
 'longitude of the southwestern zooming/bounding box corner')
options(14) = op_option_new('b', 'ilat', ilat, 30.D0, help= &
 'latitude of the southwestern zooming/bounding box corner')
options(15) = op_option_new('c', 'flon', flon, 30.D0, help= &
 'longitude of the northeastern zooming/bounding box corner')
options(16) = op_option_new('d', 'flat', flat, 60.D0, help= &
 'latitude of the northeastern zooming/bounding box corner')
options(17) = op_option_new(' ', 'ix', ix, 1, help= &
 'x-index of the southwestern zooming corner')
options(18) = op_option_new(' ', 'iy', iy, 1, help= &
 'y-index of the southwestern zooming corner')
options(19) = op_option_new(' ', 'fx', fx, 31, help= &
 'x-index of the northeastern zooming corner')
options(20) = op_option_new(' ', 'fy', fy, 31, help= &
 'y-index of the northeastern zooming corner')

options(21) = op_option_new('f', 'npx', npx, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')
options(22) = op_option_new('g', 'npy', npy, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')

options(28)= op_option_new('e', 'a-grid', c2agrid, help= &
 'interpolate U/V points of an Arakawa C grid on the corresponding T points &
 &of an Arakawa A grid')

options(29) = op_option_new('t', 'component-flag', component_flag, &
 0, help='wind component flag in interpolated grid (0/1)')

!options(30) = op_option_new(' ', 'set-scmode', set_scmode, 'xxx', &
! help='set output grid scanning mode to a particular standard value: &
! &3 binary digits indicating respectively iScansNegatively, jScansPositively and &
! &jPointsAreConsecutive (grib_api jargon), 0 for false, 1 for true, &
! &000 for ECMWF-like grids, 010 for COSMO and Cartesian-like grids. &
! &Any other character indicates to keep the &
! &corresponding original scanning mode value')

! display option
options(38) = op_option_new(' ', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
! help options
options(39) = op_option_help_new('h', 'help', help= &
 'show an help message and exit')
options(40) = op_option_new(' ', 'version', version, help= &
 'show version and exit')

! define the option parser
opt = optionparser_new(options, description_msg= &
 'Grib to grib trasformation application. It reads grib edition 1 and 2 &
 &and zooms, interpolates or regrids data according to optional parameters. &
 &The whole grib data file is read and organized in memory, transformed, and &
 &written on output. So it is possible to perform multi-field elaborations &
 &like wind component transformation, but memory constraints limit the number &
 &of input fields. More different date, timeranges, levels and parameters &
 &are imported, even with empty combinations, and more memory will be required', &
 usage_msg='Usage: vg6d_transform [options] inputfile outputfile')

! parse options and check for errors
optind = optionparser_parseoptions(opt)
IF (optind <= 0) THEN
  CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters')
  CALL EXIT(1)
ENDIF

IF (version) THEN
  WRITE(*,'(A,1X,A)')'vg6d_transform',VERSION
  CALL exit(0)
ENDIF

if ( optind <= iargc()) then
  call getarg(optind, infile)
  optind=optind+1
else
  call l4f_category_log(category,L4F_ERROR,'input file missing')
  call optionparser_printhelp(opt)
  call exit(1)
end if

if ( optind <= iargc()) then
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

if (trans_type == 'inter' .OR. trans_type == 'boxinter') then ! griddim_out needed

  call init(griddim_out,&
   type=type,nx=nx,ny=ny, &
   lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max, component_flag=component_flag, &
   latitude_south_pole=latitude_south_pole,longitude_south_pole=longitude_south_pole,angle_rotation=angle_rotation, &
   categoryappend="requested_grid")

  call griddim_unproj(griddim_out)

  IF (ldisplay) THEN
    PRINT*,'output grid >>>>>>>>>>>>>>>>>>>>'
    CALL display(griddim_out)
  ENDIF

end if


CALL import(volgrid,filename=infile,decode=.FALSE.,categoryappend="input")
if (c2agrid) call vg6d_c2a(volgrid)


if (trans_type == 'none') then

  !exportazione
  i = word_split(outfile, w_s, w_e, ':')

  IF (i == 3) THEN ! template requested (grib_api:template_file:output_file)
    file_template = grid_file_id_new(outfile(w_s(1):w_e(2)), 'r')
    gaid_template = grid_id_new(file_template)
    IF (c_e(gaid_template)) THEN
      CALL export (volgrid, filename=outfile(w_s(1):w_e(1))//':'// &
       outfile(w_s(3):w_e(3)), gaid_template=gaid_template, &
       categoryappend="export_template")
    ELSE
      CALL l4f_category_log(category,L4F_FATAL, &
       "opening output template "//TRIM(outfile))
      CALL raise_fatal_error()
    ENDIF

  ELSE ! simple export
    CALL export(volgrid,filename=outfile,categoryappend="export")

  ENDIF
  call l4f_category_log(category,L4F_INFO,"end")

  DEALLOCATE(w_s, w_e)
  if (associated(volgrid)) call delete (volgrid)

else

 !trasformation object
  call init(trans, trans_type=trans_type, sub_type=sub_type, &
   ix=ix, iy=iy, fx=fx, fy=fy, &
   ilon=ilon, ilat=ilat, flon=flon, flat=flat, npx=npx, npy=npy, &
   percentile=0.5D0, &
   categoryappend="trasformation")

  if (trans_type == 'inter' .OR. trans_type == 'boxinter') then
    CALL transform(trans,griddim_out,volgrid6d_in=volgrid, &
     volgrid6d_out=volgrid_out,clone=.TRUE.,categoryappend="trasformato")
  else
    CALL transform(trans,volgrid6d_in=volgrid, volgrid6d_out=volgrid_out, &
     clone=.TRUE.,categoryappend="trasformato")
  endif

  call l4f_category_log(category,L4F_INFO,"trasformato")
  if (associated(volgrid)) call delete(volgrid)


  !exportazione
  call export (volgrid_out,filename=outfile,categoryappend="exportazione")

  call l4f_category_log(category,L4F_INFO,"end")

  call delete (volgrid_out)

end if


!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM vg6d_transform
