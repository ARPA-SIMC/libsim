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
use volgrid6d_class_compute
#ifdef VAPOR
use volgrid6d_vapor_class
#endif
use grid_class
use grid_transform_class
use grid_id_class
use err_handling
use char_utilities
USE array_utilities
use optionparser_class
USE datetime_class
USE georef_coord_class
USE vol7d_level_class
#ifdef ALCHIMIA
USE alchimia
use volgrid6d_alchimia_class
use vol7d_alchimia_class
USE termo
#endif

implicit none

INTEGER :: category, ier, i, n
CHARACTER(len=12) :: coord_format
CHARACTER(len=10), ALLOCATABLE :: vl(:)
!CHARACTER(len=10) :: level_type
CHARACTER(len=512) :: a_name, coord_file, input_file, output_file, output_format, output_template, output_variable_list
TYPE(arrayof_integer) :: trans_level_type, trans_level_list, trans_botlevel_list
TYPE(vol7d_level) :: ilevel, olevel
TYPE(vol7d_level),ALLOCATABLE :: olevel_list(:)
TYPE(volgrid6d),POINTER  :: volgrid(:), volgrid_coord_tmp(:), volgrid_out(:), volgrid_tmp(:)
TYPE(volgrid6d)  :: volgrid_coord
DOUBLE PRECISION :: ilon, ilat, flon, flat, radius

type(griddim_def) :: griddim_out
type(transform_def) :: trans

integer :: nx,ny,component_flag,npx,npy
doubleprecision :: xmin, xmax, ymin, ymax
INTEGER :: ix, iy, fx, fy, time_definition
doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation
character(len=80) :: proj_type,trans_type,sub_type

TYPE(arrayof_georef_coord_array) :: poly
LOGICAL :: extrap, c2agrid, decode, round
TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
TYPE(csv_record) :: argparse
INTEGER :: iargc
!CHARACTER(len=3) :: set_scmode
LOGICAL :: version, ldisplay
#ifdef VAPOR
LOGICAL :: rzscan
#endif
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(grid_file_id) :: file_template
TYPE(grid_id) :: gaid_template
#ifdef ALCHIMIA
type(fndsv) :: vfn, vfnoracle
#endif

! for computing
CHARACTER(len=13) :: comp_stat_proc
CHARACTER(len=23) :: comp_step, comp_start
INTEGER :: istat_proc, ostat_proc
LOGICAL :: comp_full_steps
TYPE(timedelta) :: c_i
TYPE(datetime) :: c_s
!REAL :: comp_frac_valid

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="volgrid6dtransform")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")


                                ! define the option parser
opt = optionparser_new(description_msg= &
 'Grib to grib transformation application. It reads grib edition 1 and 2 &
 &and zooms, interpolates or regrids data according to optional parameters. &
 &The whole grib data file is read and organized in memory, transformed, and &
 &written on output. So it is possible to perform multi-field elaborations &
 &like wind component transformation, but memory constraints limit the number &
 &of input fields. More different date, timeranges, levels and parameters &
 &are imported, even with empty combinations, and more memory will be required', &
 usage_msg='Usage: vg6d_transform [options] inputfile outputfile')

                                ! define command-line options
CALL optionparser_add(opt, 'v', 'trans-type', trans_type, 'none', help= &
 'transformation type: ''inter'' for interpolation, ''boxinter'' for &
 &statistical interpolation on boxes, ''zoom'' for zooming, &
 &''boxregrid'' for resolution reduction, ''metamorphosis'' for &
 &keeping the same grid but changing e.g. the component flag, &
 &''none'' for no transformation (input/output only)')
CALL optionparser_add(opt, 'z', 'sub-type', sub_type, 'near', help= &
 'transformation subtype, for inter: ''near'', ''bilin'', &
 &for boxinter and boxregrid: ''average'', ''stddev'', ''max'', ''min'', &
 &for zoom: ''index'', ''coord'', ''coordbb'', &
 &for metamorphosis: ''all''')
CALL optionparser_add(opt, ' ', 'extrap', extrap, help= &
 'enable extrapolation outside input grid, it works only for ''inter'' &
 &transformations, use with care')

CALL optionparser_add(opt, 'u', 'type', proj_type, 'regular_ll', help= &
 'projection and parameters of interpolated grid: it is a string &
 &as ''regular_ll'', ''rotated_ll'', ''UTM''')
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
radius = dmiss
CALL optionparser_add(opt, ' ', 'radius', radius, help= &
 'radius of stencil in gridpoint units, fractionary values accepted, &
 &for ''stencilinter'' interpolation')

CALL optionparser_add(opt, 'f', 'npx', npx, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')
CALL optionparser_add(opt, 'g', 'npy', npy, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')

!CALL optionparser_add(opt, ' ', 'trans-level-type', level_type, '100,,100,', help= &
! 'type of input and output level for vertical interpolation &
! &in the form intop,inbot,outtop,outbot, from grib2 table, at the moment &
! &intop and outtop type must be the same, inbot and outbot can either be empty &
! &(single surface) &
! &or equal to the corresponding top value (layer between 2 surfaces)')
!CALL optionparser_add(opt, ' ', 'trans-level-list', level_list, '50000,70000,85000,100000', help= &
! 'list of output levels for vertical interpolation, the unit is determined &
! &by the value of level-type and taken from grib2 table')

CALL optionparser_add(opt, ' ', 'trans-level-type', trans_level_type, help= &
 'type of input and output level for vertical interpolation &
 &in the form intop,inbot,outtop,outbot, from grib2 table, at the moment &
 &intop and outtop type must be the same, inbot and outbot can either be empty &
 &(single surface) &
 &or equal to the corresponding top value (layer between 2 surfaces)')
CALL optionparser_add(opt, ' ', 'trans-level-list', trans_level_list, help= &
 'list of output levels (or top surfaces) for vertical interpolation, the unit is determined &
 &by the value of level-type and taken from grib2 table')
CALL optionparser_add(opt, ' ', 'trans-botlevel-list', trans_botlevel_list, help= &
 'list of output bottom surfaces for vertical interpolation, the unit is determined &
 &by the value of level-type and taken from grib2 table')

CALL optionparser_add(opt, ' ', 'rounding', round, help= &
 'simplifies volume, merging similar levels and timeranges')

coord_file=cmiss
#if defined (HAVE_SHAPELIB) || defined (HAVE_LIBGRIBAPI)
CALL optionparser_add(opt, ' ', 'coord-file', coord_file, help= &
#ifdef HAVE_SHAPELIB
 'file in shp format with coordinates of polygons, required for maskgen transformation' &
#endif
#if defined (HAVE_SHAPELIB) && defined (HAVE_LIBGRIBAPI)
 //' or '// &
#endif
#ifdef HAVE_LIBGRIBAPI
 'file in grib format providing vertical coordinate of input data for vertical interpolation' &
#endif
 )
#endif
coord_format=cmiss
#if defined (HAVE_SHAPELIB) || defined (HAVE_LIBGRIBAPI)
CALL optionparser_add(opt, ' ', 'coord-format', coord_format, help= &
 'format of coord file (shp or grib_api)')
#endif

output_template = ''
CALL optionparser_add(opt, ' ', 'output-format', output_format, &
#ifdef HAVE_LIBGRIBAPI
'grib_api', &
#else
'', &
#endif
help='format of output file, in the form ''name[:template]'''&
#ifdef HAVE_LIBGRIBAPI
 //'; ''grib_api'' for gridded output in grib format, template is the &
 &path name of a grib file in which the first message defines the output grid and &
 &is used as a template for the output grib messages'&
#endif
#ifdef VAPOR
 //'; ''vapor'' for gridded output in vdf format'&
#endif
 //'; if this option includes a template, --type &
 &argument &c. are ignored, otherwise --type &c. define the output grid')

CALL optionparser_add(opt, 'e', 'a-grid', c2agrid, help= &
 'interpolate U/V points of an Arakawa C grid on the corresponding T points &
 &of an Arakawa A grid')

CALL optionparser_add(opt, 't', 'component-flag', component_flag, &
 0, help='wind component flag in interpolated grid (0/1)')

!CALL optionparser_add(opt, ' ', 'set-scmode', set_scmode, 'xxx', &
! help='set output grid scanning mode to a particular standard value: &
! &3 binary digits indicating respectively iScansNegatively, jScansPositively and &
! &jPointsAreConsecutive (grib_api jargon), 0 for false, 1 for true, &
! &000 for ECMWF-like grids, 010 for COSMO and Cartesian-like grids. &
! &Any other character indicates to keep the &
! &corresponding original scanning mode value')

! this option has been commented because it is not handled in
! volgrid_class, it makes sense only in vg6d_getpoint for determining
! the time_definition of output v7d volume
time_definition = 0
!CALL optionparser_add(opt, ' ', 'time-definition', time_definition, 0, help= &
! 'time definition for import volume, 0 for reference time (more suitable for &
! &presenting forecast data) and 1 for verification time (more suitable for &
! &comparing forecasts with observations)')

! for computing
CALL optionparser_add(opt, ' ', 'comp-stat-proc', comp_stat_proc, '', help= &
 'statistically process data with an operator specified in the form [isp:]osp &
 &where isp is the statistical process of input data which has to be processed &
 &and osp is the statistical process to apply and which will appear in output &
 &timerange; possible values for isp and osp are 0=average, 1=accumulated, &
 &2=maximum, 3=minimum, 254=instantaneous, but not all the combinations &
 &make sense; if isp is not provided it is assumed to be equal to osp')

CALL optionparser_add(opt, ' ', 'comp-step', comp_step, '0000000001 00:00:00.000', help= &
 'length of regularization or statistical processing step in the format &
 &''YYYYMMDDDD hh:mm:ss.msc'', it can be simplified up to the form ''D hh''')

CALL optionparser_add(opt, ' ', 'comp-start', comp_start, '', help= &
 'start of statistical processing interval, an empty value means &
 &take the initial time step of the available data; the format is YYYY-MM-DD HH:MM')
!CALL optionparser_add(opt, ' ', 'comp-frac-valid', comp_frac_valid, 1., help= &
! 'specify the fraction of input data that has to be valid in order to consider a &
! &statistically processed value acceptable')

CALL optionparser_add(opt, ' ', 'comp-full-steps', comp_full_steps, help= &
 'compute statistical processing by differences only on intervals with forecast &
 &time equal to a multiple of comp-step, otherwise all reasonable combinations &
 &of forecast times are computed')

! display option
CALL optionparser_add(opt, ' ', 'display', ldisplay, help= &
 'briefly display the data volume imported and exported, warning: this option is incompatible &
 &with output on stdout.')

#ifdef VAPOR
CALL optionparser_add(opt, ' ', 'reverse-vapor-z-order', rzscan, help= &
 'reverse the scan order for Z (level) coordinate during export to vdf files for vapor.')
#endif

#ifdef ALCHIMIA
CALL optionparser_add(opt, '', 'output-variable-list', output_variable_list, '', help= &
 'list of data variables you require in output; if they are not in input they will be computed if possible. &
 &The output_variable_list is expressed in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B13011,B12101''')
#endif


                                ! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

                                ! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0) ! generate a clean manpage
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line parameters')
  CALL raise_fatal_error()
ENDIF
IF (version) THEN
  WRITE(*,'(A,1X,A)')'vg6d_transform',VERSION
  CALL exit(0)
ENDIF

if ( optind <= iargc()) then
  call getarg(optind, input_file)
  optind=optind+1
else
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_ERROR,'input file missing')
  call raise_fatal_error()

  call exit(1)
end if

if ( optind <= iargc()) then
  call getarg(optind, output_file)
  optind=optind+1
else
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_ERROR,'output file missing')
  call raise_fatal_error()
end if

if (optind <= iargc()) then
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_FATAL, &
   'extra arguments after input and output file names')
  call raise_fatal_error()
end if

! generate variable lists
IF (LEN_TRIM(output_variable_list) > 0) THEN
  n = word_split(output_variable_list, w_s, w_e, ',')
  ALLOCATE(vl(n))
  DO i = 1, n
    vl(i) = output_variable_list(w_s(i):w_e(i))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF

! time-related arguments
c_i = timedelta_new(isodate=comp_step)
IF (comp_start /= '') THEN
  c_s = datetime_new(isodate=comp_start)
ELSE
  c_s = datetime_miss
ENDIF

! make ilevel and olevel
DO WHILE(trans_level_type%arraysize < 4) ! complete up to 4 elements
  CALL insert(trans_level_type, imiss)
ENDDO
CALL init(ilevel, level1=trans_level_type%array(1), level2=trans_level_type%array(2))
CALL init(olevel, level1=trans_level_type%array(3), level2=trans_level_type%array(4))
CALL delete(trans_level_type)

! make olevel_list
ALLOCATE(olevel_list(trans_level_list%arraysize))
IF (c_e(olevel%level2) .AND. trans_botlevel_list%arraysize >= trans_level_list%arraysize) THEN
  DO i = 1, trans_level_list%arraysize
    CALL init(olevel_list(i), olevel%level1, trans_level_list%array(i), &
     olevel%level2, trans_botlevel_list%array(i))
  ENDDO
ELSE
  DO i = 1, trans_level_list%arraysize
    CALL init(olevel_list(i), olevel%level1, trans_level_list%array(i))
  ENDDO
ENDIF
CALL delete(trans_level_list)
CALL delete(trans_botlevel_list)

! check comp_stat_proc
istat_proc = imiss
ostat_proc = imiss
IF (comp_stat_proc /= '') THEN
  CALL init(argparse, comp_stat_proc, ':', nfield=n)
  IF (n == 1) THEN
    CALL csv_record_getfield(argparse, ostat_proc)
    istat_proc = ostat_proc
  ELSE  IF (n == 2) THEN
    CALL csv_record_getfield(argparse, istat_proc)
    CALL csv_record_getfield(argparse, ostat_proc)
  ENDIF
  CALL delete(argparse)
  IF (.NOT.c_e(istat_proc) .OR. .NOT.c_e(ostat_proc)) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, wrong syntax for --comp-stat-proc: ' &
     //TRIM(comp_stat_proc))
    CALL raise_fatal_error()
  ENDIF
ENDIF

CALL delete(opt)

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(input_file))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(output_file))
#ifdef HAVE_SHAPELIB
IF (coord_format == 'shp' .AND. c_e(coord_file)) THEN
  CALL import(poly, coord_file)
  IF (poly%arraysize <= 0) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error importing shapefile '//TRIM(coord_file))
    CALL raise_fatal_error()
  ENDIF
ENDIF
#endif

CALL init(volgrid_coord)
#ifdef HAVE_LIBGRIBAPI
IF (coord_format == 'grib_api' .AND. c_e(coord_file)) THEN
  CALL import(volgrid_coord_tmp, filename=coord_file, decode=.TRUE., &
   time_definition=time_definition, categoryappend="input_coord")
  CALL init(volgrid_coord)
  IF (ASSOCIATED(volgrid_coord_tmp)) THEN
    IF (SIZE(volgrid_coord_tmp) == 1) THEN ! assign the volume and cleanup
      volgrid_coord = volgrid_coord_tmp(1)
      CALL init(volgrid_coord_tmp(1))
      DEALLOCATE(volgrid_coord_tmp)
    ELSE ! zero or >1 different grids obtained
      CALL l4f_category_log(category, L4F_ERROR, &
       'error importing volgrid6d coord_file '//TRIM(coord_file)//', '// &
       t2c(SIZE(volgrid_coord_tmp))//' different grids obtained instead of 1')
      CALL raise_fatal_error()
    ENDIF
  ELSE ! error in importing
    CALL l4f_category_log(category, L4F_ERROR, &
     'error importing volgrid6d coord_file '//TRIM(coord_file))
    CALL raise_fatal_error()
  ENDIF
ENDIF
#endif

i = word_split(output_format, w_s, w_e, ':')
IF (i >= 2) THEN ! grid from a grib template
!  output_template = output_format(w_s(2):w_e(2))
!  output_format(w_e(1)+1:) = ' '
! open grib template file and import first message
  file_template = grid_file_id_new(output_format, 'r')
  gaid_template = grid_id_new(file_template)
  IF (c_e(gaid_template)) THEN
    CALL import(griddim_out, gaid_template)
    CALL delete(gaid_template)
    CALL delete(file_template)
  ELSE
    CALL l4f_category_log(category,L4F_ERROR, &
     'cannot read any grib message from template file '//TRIM(output_format))
    CALL raise_fatal_error()
  ENDIF
ELSE
  CALL init(griddim_out,&
   proj_type=proj_type,nx=nx,ny=ny, &
   xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, component_flag=component_flag, &
   latitude_south_pole=latitude_south_pole, &
   longitude_south_pole=longitude_south_pole, angle_rotation=angle_rotation, &
   categoryappend="requested_grid") ! explicit parameters for the grid

ENDIF
DEALLOCATE(w_s, w_e)

CALL griddim_unproj(griddim_out)

!other operations have proper decode inside
IF (output_format == "vapor") THEN
  decode=.true.
else
  decode=.false.
endif

! import input volume
CALL import(volgrid, filename=input_file, decode=decode, &
 time_definition=time_definition, categoryappend="input_volume")
IF (.NOT.ASSOCIATED(volgrid)) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error importing input volume from file '//TRIM(input_file))
  CALL raise_fatal_error()
ENDIF

IF (ldisplay) THEN
  PRINT*,'input grid >>>>>>>>>>>>>>>>>>>>'
  CALL display(volgrid)
ENDIF

IF (c2agrid) CALL vg6d_c2a(volgrid)

IF (trans_type /=  'none') THEN ! transform

! transformation object
  CALL init(trans, trans_type=trans_type, sub_type=sub_type, extrap=extrap, &
   ix=ix, iy=iy, fx=fx, fy=fy, &
   ilon=ilon, ilat=ilat, flon=flon, flat=flat, npx=npx, npy=npy, &
   radius=radius, poly=poly, percentile=0.5D0, &
   input_levtype=ilevel, output_levtype=olevel, &
   categoryappend="transformation")

  CALL transform(trans, griddim_out, volgrid6d_in=volgrid, &
   volgrid6d_out=volgrid_out, lev_out=olevel_list, volgrid6d_coord_in=volgrid_coord, &
   clone=.TRUE., categoryappend="transform")

  CALL l4f_category_log(category,L4F_INFO,"transformation completed")
  CALL delete(volgrid)

ELSE
  
  volgrid_out => volgrid

ENDIF

if (round .and. ASSOCIATED(volgrid_out)) then
  call rounding(volgrid_out,volgrid_tmp,level=almost_equal_levels,nostatproc=.true.)
  CALL delete(volgrid_out)
  volgrid_out => volgrid_tmp
  NULLIFY(volgrid_tmp)
end if

#ifdef ALCHIMIA
if (ASSOCIATED(volgrid_out) .and. output_variable_list /= " ") then

  call register_termo(vfn)
  IF (ldisplay ) call display(vfn)

  if ( alchemy(volgrid_out,vfn,vl,volgrid_tmp,copy=.true.,vfnoracle=vfnoracle) == 0 ) then
    call display(vfnoracle)
    CALL delete(volgrid_out)
    volgrid_out => volgrid_tmp
    NULLIFY(volgrid_tmp)
  else
    print *,"Impossible solution"
    CALL l4f_category_log(category, L4F_ERROR, 'Cannot make variable you have requested')

    if (.not. shoppinglist(vl,vfn,vfnoracle,copy=.false.)) then
      CALL l4f_category_log(category, L4F_ERROR, 'shoppinglist: generic error')
    else
      call sl_display_pretty(compile_sl(vfnoracle))
      IF (ldisplay ) call display(vfn)
    end if
    CALL l4f_category_log(category, L4F_ERROR, 'Exit for error')
    CALL raise_fatal_error()
  end if

  CALL l4f_category_log(category,L4F_INFO,"alchemy completed")
  
end if
#endif

IF (c_e(istat_proc) .AND. c_e(ostat_proc) .AND. ASSOCIATED(volgrid_out)) THEN ! stat_proc
  CALL l4f_category_log(category,L4F_INFO,"computing stat_proc")
  ALLOCATE(volgrid_tmp(SIZE(volgrid_out)))
  DO i = 1, SIZE(volgrid_out)
    CALL volgrid6d_compute_stat_proc(volgrid_out(i), volgrid_tmp(i), &
     istat_proc, ostat_proc, c_i, full_steps=comp_full_steps, start=c_s, clone=.TRUE.)
  ENDDO
  CALL delete(volgrid_out)
  volgrid_out => volgrid_tmp
  NULLIFY(volgrid_tmp)
ENDIF

IF (ldisplay .and. ASSOCIATED(volgrid_out)) THEN ! done here in order to print final ellipsoid
  PRINT*,'output grid >>>>>>>>>>>>>>>>>>>>'
  CALL display(volgrid_out)
ENDIF

! export
CALL write_to_file_out(volgrid_out)

IF (ASSOCIATED(volgrid_out)) CALL delete(volgrid_out)

#ifdef ALCHIMIA
call delete(vfn)
call delete(vfnoracle)
#endif

CALL l4f_category_log(category,L4F_INFO,"end")

                                !chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

contains


subroutine write_to_file_out(myvolgrid)

type (volgrid6d),pointer  :: myvolgrid(:)

i = word_split(output_file, w_s, w_e, ':')

IF (i == 3) THEN ! template requested (grib_api:template_file:output_file)
  file_template = grid_file_id_new(output_file(w_s(1):w_e(2)), 'r')
  gaid_template = grid_id_new(file_template)
  IF (c_e(gaid_template)) THEN
    CALL export (myvolgrid, filename=output_file(w_s(1):w_e(1))//':'// &
     output_file(w_s(3):w_e(3)), gaid_template=gaid_template, &
     categoryappend="export_tmpl")
  ELSE
    CALL l4f_category_log(category,L4F_FATAL, &
     "opening output template "//output_file(w_s(1):w_e(2)))
    CALL raise_fatal_error()
  ENDIF

ELSE

  if (output_format(1:8) == "grib_api") then 
#ifdef HAVE_LIBGRIBAPI
    CALL export(myvolgrid,filename=output_file,categoryappend="exporttofilegrib")
#else
    CALL l4f_category_log(category,L4F_FATAL, &
     "export to grib_api disabled at compile time")
    CALL raise_fatal_error()
#endif

  else if (output_format == "vapor") then 

#ifdef VAPOR
    do i =1,size(myvolgrid)
      CALL l4f_category_log(category,L4F_INFO, &
       "exporting to vapor vdf file: "//trim(output_file)//"_"//t2c(i)//".vdf")
      call export (myvolgrid(i),normalize=.True.,rzscan=rzscan,&
       filename=trim(output_file)//"_"//t2c(i)//".vdf")
    end do
#else
    CALL l4f_category_log(category,L4F_FATAL, &
     "export to vapor vdf file disabled at compile time")
    CALL raise_fatal_error()
#endif

  else
    CALL l4f_category_log(category,L4F_FATAL, &
     "output format unknown: "//trim(output_format))
    CALL raise_fatal_error()

  end if


ENDIF

CALL l4f_category_log(category,L4F_INFO,"end export")

DEALLOCATE(w_s, w_e)

end subroutine write_to_file_out



END PROGRAM vg6d_transform
