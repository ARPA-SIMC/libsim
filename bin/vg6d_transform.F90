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
use gridinfo_class
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
CHARACTER(len=10), ALLOCATABLE :: vl(:), avl(:)
!CHARACTER(len=10) :: level_type
CHARACTER(len=512) :: a_name, coord_file, input_file, output_file, &
 output_format, output_template, anavariable_list, output_variable_list
TYPE(arrayof_integer) :: trans_level_type, trans_level_list, trans_botlevel_list
TYPE(vol7d_level) :: ilevel, olevel
TYPE(vol7d_level),ALLOCATABLE :: olevel_list(:)
TYPE(volgrid6d),POINTER  :: volgrid(:), volgrid_coord_tmp(:), volgrid_out(:), volgrid_tmp(:)
TYPE(volgrid6d)  :: volgrid_coord
TYPE(arrayof_gridinfo) :: maskgrid
REAL,ALLOCATABLE :: maskfield(:,:)
DOUBLE PRECISION :: ilon, ilat, flon, flat, radius, percentile
TYPE(arrayof_real) :: maskbounds

type(griddim_def) :: griddim_out
type(transform_def) :: trans

INTEGER :: nx,ny,component_flag,npx,npy,dup_mode
doubleprecision :: xmin, xmax, ymin, ymax, xoff, yoff
INTEGER :: ix, iy, fx, fy, time_definition, utm_zone
doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation
character(len=80) :: proj_type,trans_type,sub_type

TYPE(arrayof_georef_coord_array) :: poly
LOGICAL :: extrap, c2agrid, decode, round
TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
TYPE(csv_record) :: argparse
INTEGER :: iargc
CHARACTER(len=3) :: set_scmode
LOGICAL :: version, ldisplay
#ifdef VAPOR
LOGICAL :: rzscan,reusevdf
#endif
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(grid_file_id) :: file_grid
TYPE(grid_id) :: gaid_grid
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

! from vg6d_subarea
TYPE(gridinfo_def) :: gridinfo
TYPE(grid_file_id) :: ifile,ofile
TYPE(grid_id) :: input_grid_id
INTEGER :: gaid
REAL, ALLOCATABLE :: field(:,:,:),fieldz(:,:,:)
TYPE(grid_transform) :: grid_trans
CHARACTER(len=1) :: trans_mode

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="volgrid6dtransform")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Gridded-field to gridded-field transformation application. &
 &It reads grib edition 1 and 2 and gdal-supported formats &
 &and zooms, interpolates or regrids data according to optional arguments. &
 &In the default ''p'' mode, the whole input data file is read and organized &
 &in memory, transformed, and written on output, &
 &so it is possible to perform multi-field processing &
 &like wind component transformation, but memory constraints limit the number &
 &of input fields. When running in the optional ''s'' mode, the input data is &
 &processed one horizontal slice at a time with less memory footprint and less &
 &processing capabilities. The output file is specified in the form &
 &[output_driver:[output_template:]]pathname, when output_driver is grib_api, &
 &output_template may specify a file contaning a grib message &
 &to be used as a template for the output file.', &
 usage_msg='Usage: vg6d_transform [options] inputfile outputfile')

! define command-line options
CALL optionparser_add(opt, ' ', 'trans-mode', trans_mode, 'p', help= &
 'transformation mode: either ''p''rosciutto or ''s''alsiccia; in ''p'' mode &
 &the input data are accomodated into an expensive 6-dimensional prosciutto (ham) &
 &which can be processed, converted, sliced or baked along all possible dimensions; &
 &in ''s'' mode, the input data are processed as a cheap and infinitely &
 &long salsiccia (sausage), one field at a time, so only operations on single horizontal slices &
 &are allowed in this mode; many options are thus silently ignored or may &
 &generate unexpected errors in ''s'' mode')

CALL optionparser_add_sep(opt, 'The following options are mostly valid  both in &
 &''p'' mode and in ''s'' mode (see --trans-mode)')

CALL optionparser_add(opt, 'v', 'trans-type', trans_type, 'none', help= &
 'transformation type: ''inter'' for interpolation, ''boxinter'' for &
 &statistical interpolation on boxes, ''zoom'' for zooming, &
 &''boxregrid'' for resolution reduction, ''metamorphosis'' for &
 &keeping the same grid but changing e.g. the component flag, '&
#ifdef HAVE_SHAPELIB
 //'maskgen for generating a mask field on polygons, polyinter for intepolating &
 &on polygons, '&
#endif
 //'''none'' for no transformation (input/output only)')
sub_type = ''
CALL optionparser_add(opt, 'z', 'sub-type', sub_type, help= &
 'transformation subtype, for inter: ''near'', ''bilin'', &
 &for boxinter, boxregrid'&
#ifdef HAVE_SHAPELIB
 //', polyinter'&
#endif
//': ''average'', ''stddev'', ''max'', ''min'', ''percentile'', &
 &for zoom: ''index'', ''coord'', ''coordbb'', ''projcoord'', '&
#ifdef HAVE_SHAPELIB
 //'for maskgen: ''poly'', '&
#endif
 //'for metamorphosis: ''all'', ''maskvalid'', ''maskinvalid'', &
 &''setinvalidto'', ''settoinvalid''')
CALL optionparser_add(opt, ' ', 'extrap', extrap, help= &
 'enable extrapolation outside input grid, it works only for ''inter'' &
 &transformations, use with care')

! display option
CALL optionparser_add(opt, ' ', 'display', ldisplay, help= &
 'briefly display the data volume imported and exported, warning: this option is incompatible &
 &with output on stdout.')

CALL optionparser_add(opt, 'u', 'type', proj_type, 'regular_ll', help= &
 'projection and parameters of interpolated grid: it is a string &
 &as ''regular_ll'', ''rotated_ll'', ''UTM''')
CALL optionparser_add(opt, 'i', 'nx', nx, 31, help= &
 'number of nodes along x axis on interpolated grid')
CALL optionparser_add(opt, 'l', 'ny', ny, 31, help= &
 'number of nodes along y axis on interpolated grid')
CALL optionparser_add(opt, 'm', 'x-min', xmin, 0.0D0, help= &
 'x coordinate of the lower left corner of interpolated grid (degrees or meters)')
CALL optionparser_add(opt, 'o', 'y-min', ymin, 30.0D0, help= &
 'y coordinate of the lower left corner of interpolated grid (degrees or meters)')
CALL optionparser_add(opt, 'n', 'x-max', xmax, 30.0D0, help= &
 'x coordinate of the upper right corner of interpolated grid (degrees or meters)')
CALL optionparser_add(opt, 'p', 'y-max', ymax, 60.0D0, help= &
 'y coordinate of the upper right corner of interpolated grid (degrees or meters)')
CALL optionparser_add(opt, 'n', 'x-off', xoff, 0.0D0, help= &
 'x coordinate offset (also known as false easting) in interpolated grid')
CALL optionparser_add(opt, 'p', 'y-off', yoff, 0.0D0, help= &
 'y coordinate offset (also known as false northing) in interpolated grid')
CALL optionparser_add(opt, ' ', 'utm-zone', utm_zone, 32, help= &
 'zone number for UTM projections')

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
CALL optionparser_add(opt, ' ', 'maskbounds', maskbounds, help= &
 'comma-separated list of boundary values for some sub-types of &
 &''metamorphosis'' transformation: &
 &for ''maskvalid'' it defines an optional range of mask field values &
 &defining the area of valid points (2 values), &
 &for ''setinvalidto'' it sets the constant value to be used (1 value), &
 &for ''settoinvalid'' it defines the range of values to become invalid (2 values)')

CALL optionparser_add(opt, 'f', 'npx', npx, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')
CALL optionparser_add(opt, 'g', 'npy', npy, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')

CALL optionparser_add(opt, ' ', 'trans-level-type', trans_level_type, help= &
 'type of input and output level for vertical interpolation &
 &in the form intop,inbot,outtop,outbot, from grib2 table; inbot and outbot &
 &can either be empty (single surface) &
 &or equal to the corresponding top value (layer between 2 surfaces)')
CALL optionparser_add(opt, ' ', 'trans-level-list', trans_level_list, help= &
 'list of output levels (or top surfaces) for vertical interpolation, the unit is determined &
 &by the value of level-type and taken from grib2 table')
CALL optionparser_add(opt, ' ', 'trans-botlevel-list', trans_botlevel_list, help= &
 'list of output bottom surfaces for vertical interpolation, the unit is determined &
 &by the value of level-type and taken from grib2 table')
CALL optionparser_add(opt, ' ', 'percentile', percentile, 50.0D0, help= &
 'desired percentile, [0.,100.], for ''*:percentile'' transformations')

coord_file=cmiss
#if defined (HAVE_SHAPELIB) || defined (HAVE_LIBGRIBAPI)
CALL optionparser_add(opt, ' ', 'coord-file', coord_file, help= &
#ifdef HAVE_SHAPELIB
'file in shp format with coordinates of polygons, required for maskgen and polyinter transformation' &
#endif
#if defined (HAVE_SHAPELIB) && defined (HAVE_LIBGRIBAPI)
//' or '// &
#endif
#ifdef HAVE_LIBGRIBAPI
'file in grib format providing vertical coordinate of input data for vertical interpolation' &
#endif
)
#endif
coord_format=''
#if defined (HAVE_SHAPELIB) || defined (HAVE_LIBGRIBAPI)
CALL optionparser_add(opt, ' ', 'coord-format', coord_format, help= &
 'format of coord file (shp or grib_api)')
#endif

CALL optionparser_add(opt, ' ', 'anavariable-list', anavariable_list, '', help= &
 'list of variables in the form of a comma-separated list of B-table &
 &alphanumeric codes, e.g. ''B10007,B29192'' to be considered as &
 &time-independent and assigned to all times and timeranges')

output_template = ''
CALL optionparser_add(opt, ' ', 'output-format', output_format, &
#ifdef HAVE_LIBGRIBAPI
'grib_api', &
#else
'', &
#endif
help='format of output file, in the form ''name[:grid_definition]'''&
#ifdef HAVE_LIBGRIBAPI
//'; ''grib_api'' for gridded output in grib format, grid_definition is the &
 &path name of a grib file in which the first message is used as a template &
 &for defining the output grid'&
#endif
#ifdef VAPOR
//'; ''vapor'' for gridded output in vdf format'&
#endif
//'; if this option includes a grid_definition, --type &
 &argument &c. are ignored, otherwise --type &c. define the output grid')

! display option
CALL optionparser_add(opt, ' ', 'display', ldisplay, help= &
 'briefly display the data volume imported and exported, warning: this option is incompatible &
 &with output on stdout.')

CALL optionparser_add_sep(opt, 'The following options are valid only in ''p'' mode &
 &(see --trans-mode)')

#ifdef ALCHIMIA
CALL optionparser_add(opt, '', 'output-variable-list', output_variable_list, '', help= &
 'list of data variables required in output; if they are not in input they will be computed if possible. &
 &The output_variable_list is expressed in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B13011,B12101''')
#endif

CALL optionparser_add(opt, ' ', 'rounding', round, help= &
 'simplify volume, merging similar levels and timeranges')

CALL optionparser_add(opt, 'e', 'a-grid', c2agrid, help= &
 'interpolate U/V points of an Arakawa C grid on the corresponding T points &
 &of an Arakawa A grid')

CALL optionparser_add(opt, 't', 'component-flag', component_flag, &
 0, help='wind component flag in interpolated grid (0/1)')

CALL optionparser_add(opt, ' ', 'time-definition', time_definition, 0, help= &
 'time definition for imported volume, 0 for reference time (more suitable for &
 &presenting forecast data) and 1 for verification time (more suitable for &
 &comparing forecasts with observations)')

CALL optionparser_add(opt, ' ', 'dup-mode', dup_mode, 0, help= &
 'behavior in case of duplicated input metadata: 0=overwrite fields, &
 &1=merge fields taking into account missing data and with priority to &
 &the second field')

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

#ifdef VAPOR
CALL optionparser_add(opt, ' ', 'reverse-vapor-z-order', rzscan, help= &
 'reverse the scan order for Z (level) coordinate during export to vdf files for vapor.')

CALL optionparser_add(opt, ' ', 'reuse-vapor-vdf-file', reusevdf, help= &
 'reuse and modify an existing vdf file appending data to a vapor data collection during export to vapor.')
#endif

CALL optionparser_add_sep(opt, 'The following option is valid only in ''s'' mode &
 &(see --trans-mode)')

CALL optionparser_add(opt, ' ', 'set-scmode', set_scmode, 'xxx', &
 help='set output grid scanning mode to a particular standard value: &
 &3 binary digits indicating respectively iScansNegatively, jScansPositively and &
 &jPointsAreConsecutive (grib_api jargon), 0 for false, 1 for true, &
 &000 for ECMWF-like grids, 010 for COSMO and Cartesian-like grids. &
 &Any other character indicates to keep the &
 &corresponding original scanning mode value; &
 &available only with --trans-mode=s')


! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0) ! generate a clean manpage
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line arguments')
  CALL raise_fatal_error()
ENDIF
IF (version) THEN
  WRITE(*,'(A,1X,A)')'vg6d_transform',VERSION
  CALL exit(0)
ENDIF

IF (optind <= iargc()) THEN
  CALL getarg(optind, input_file)
  IF (input_file == '-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit')
    input_file = '/dev/stdin'
  ENDIF
  optind = optind+1
ELSE
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category, L4F_FATAL, 'input file missing')
  CALL raise_fatal_error()

  CALL EXIT(1)
ENDIF

IF (optind <= iargc()) THEN
  CALL getarg(optind, output_file)
  IF (output_file == '-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit')
    output_file = '/dev/stdout'
  ENDIF
  optind = optind+1
ELSE
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category, L4F_FATAL, 'output file missing')
  CALL raise_fatal_error()
ENDIF

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
IF (LEN_TRIM(anavariable_list) > 0) THEN
  n = word_split(anavariable_list, w_s, w_e, ',')
  ALLOCATE(avl(n))
  DO i = 1, n
    avl(i) = anavariable_list(w_s(i):w_e(i))
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
     'error in command-line arguments, wrong syntax for --comp-stat-proc: ' &
     //TRIM(comp_stat_proc))
    CALL raise_fatal_error()
  ENDIF
ENDIF

! pack maskbounds array or allocate it to zero length for further
! correct behavior
CALL packarray(maskbounds)

CALL delete(opt)

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(input_file))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(output_file))

CALL init(volgrid_coord)

IF (c_e(coord_file)) THEN
  IF (.FALSE.) THEN ! dummy clause
#ifdef HAVE_SHAPELIB
  ELSE IF (coord_format == 'shp') THEN
    CALL import(poly, coord_file)
    IF (poly%arraysize <= 0) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error importing shapefile '//TRIM(coord_file))
      CALL raise_fatal_error()
    ENDIF
#endif
#ifdef HAVE_LIBGRIBAPI
  ELSE IF (coord_format == 'grib_api') THEN

    IF (trans_type == 'vertint') THEN ! for vertint I need a complete volume
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

    ELSE ! otherwise just a single 2d field
      CALL import(maskgrid, coord_file, categoryappend='maskgrid')
      IF (maskgrid%arraysize < 1) THEN
        CALL l4f_category_log(category, L4F_ERROR, &
         'error importing mask grid file '//TRIM(coord_file))
        CALL raise_fatal_error()
      ENDIF
      CALL import(maskgrid%array(1))
      ALLOCATE(maskfield(maskgrid%array(1)%griddim%dim%nx, maskgrid%array(1)%griddim%dim%ny))
      maskfield(:,:) = decode_gridinfo(maskgrid%array(1))
      CALL delete(maskgrid)

    ENDIF
#endif
  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line arguments, format '// &
     TRIM(coord_format)//' in --coord-format not valid or not supported.')
    CALL raise_fatal_error()
  ENDIF
ENDIF

i = word_split(output_format, w_s, w_e, ':')
IF (i >= 2) THEN ! grid from a grib template
! open grib template file and import first message
  file_grid = grid_file_id_new(output_format, 'r')
  gaid_grid = grid_id_new(file_grid)
  IF (c_e(gaid_grid)) THEN
    CALL import(griddim_out, gaid_grid)
    CALL delete(gaid_grid)
    CALL delete(file_grid)
  ELSE
    CALL l4f_category_log(category,L4F_ERROR, &
     'cannot read any grib message from template file '//TRIM(output_format))
    CALL raise_fatal_error()
  ENDIF
ELSE
  CALL init(griddim_out,&
   proj_type=proj_type,nx=nx,ny=ny, zone=utm_zone, xoff=xoff, yoff=yoff, &
   xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, component_flag=component_flag, &
   latitude_south_pole=latitude_south_pole, &
   longitude_south_pole=longitude_south_pole, angle_rotation=angle_rotation, &
   categoryappend="requested_grid") ! explicit parameters for the grid

ENDIF
DEALLOCATE(w_s, w_e)

CALL griddim_unproj(griddim_out)

IF (trans_type /= 'none') THEN ! define transform
  CALL l4f_category_log(category,L4F_DEBUG,'defining transform')
! transformation object
  CALL init(trans, trans_type=trans_type, sub_type=sub_type, extrap=extrap, &
   ix=ix, iy=iy, fx=fx, fy=fy, &
   ilon=ilon, ilat=ilat, flon=flon, flat=flat, npx=npx, npy=npy, &
   radius=radius, poly=poly, percentile=percentile, &
   input_levtype=ilevel, output_levtype=olevel, &
   categoryappend="transformation")
ENDIF

IF (trans_mode == "p") THEN ! run in prosciutto (volume) mode

  decode = output_format == "vapor" .OR. dup_mode > 0
! import input volume
  CALL import(volgrid, filename=input_file, decode=decode, dup_mode=dup_mode, &
   time_definition=time_definition, anavar=avl, categoryappend="input_volume")
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

  IF (trans_type /= 'none') THEN ! transform
    CALL l4f_category_log(category,L4F_DEBUG,'execute transform')

    CALL transform(trans, griddim_out, volgrid6d_in=volgrid, &
     volgrid6d_out=volgrid_out, lev_out=olevel_list, &
     volgrid6d_coord_in=volgrid_coord, &
     maskgrid=maskfield, maskbounds=maskbounds%array, &
     clone=.TRUE., categoryappend="transform")

    CALL l4f_category_log(category,L4F_INFO,"transformation completed")
    CALL delete(volgrid)

  ELSE

    CALL l4f_category_log(category,L4F_DEBUG,'clone in to out')
    volgrid_out => volgrid

  ENDIF

  if (round .and. ASSOCIATED(volgrid_out)) then
    CALL l4f_category_log(category,L4F_DEBUG,'execute rounding')
    call rounding(volgrid_out,volgrid_tmp,level=almost_equal_levels,nostatproc=.true.)
    CALL delete(volgrid_out)
    volgrid_out => volgrid_tmp
    NULLIFY(volgrid_tmp)
  end if

#ifdef ALCHIMIA
  if (ASSOCIATED(volgrid_out) .and. output_variable_list /= " ") then

    CALL l4f_category_log(category,L4F_DEBUG,'execute alchemy')
    CALL register_termo(vfn)
    IF (ldisplay) CALL display(vfn)

    IF (alchemy(volgrid_out,vfn,vl,volgrid_tmp,copy=.TRUE.,vfnoracle=vfnoracle) == 0) THEN
      IF (ldisplay) CALL display(vfnoracle)
      CALL delete(volgrid_out)
      volgrid_out => volgrid_tmp
      NULLIFY(volgrid_tmp)
    ELSE
      CALL l4f_category_log(category, L4F_ERROR, 'Cannot make variable you have requested')

      IF (.NOT. shoppinglist(vl,vfn,vfnoracle,copy=.FALSE.)) THEN
        CALL l4f_category_log(category, L4F_ERROR, 'shoppinglist: generic error')
      ELSE
        CALL l4f_category_log(category, L4F_ERROR, 'use --display to get more information')
        IF (ldisplay) CALL sl_display_pretty(compile_sl(vfnoracle))
        IF (ldisplay) CALL display(vfn)
      ENDIF
      CALL l4f_category_log(category, L4F_ERROR, 'Exit for error')
      CALL raise_fatal_error()
    ENDIF

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

  CALL l4f_category_log(category,L4F_DEBUG,'execute export')
  CALL write_to_file_out(volgrid_out)

  IF (ASSOCIATED(volgrid_out)) CALL delete(volgrid_out)

#ifdef ALCHIMIA
  CALL delete(vfn)
  CALL delete(vfnoracle)
#endif


ELSE ! run in salsiccia (serial) mode

  ifile = grid_file_id_new(input_file,'r')
  ofile = grid_file_id_new(output_file,'w')

  DO WHILE (.TRUE.)
    input_grid_id = grid_id_new(ifile)
    IF (.NOT.c_e(input_grid_id)) THEN ! THEN because of a bug in gfortran?!
      EXIT
    ENDIF

    CALL l4f_category_log(category,L4F_INFO,"importing gridinfo")
    CALL init(gridinfo, gaid=input_grid_id, categoryappend="imported")
    CALL import(gridinfo)
    IF (ldisplay) THEN
      CALL display(gridinfo,namespace="ls")
    ENDIF
    CALL l4f_category_log(category,L4F_INFO,"gridinfo imported")

    ALLOCATE(field(gridinfo%griddim%dim%nx,gridinfo%griddim%dim%ny,1))
    field(:,:,1) = decode_gridinfo(gridinfo)

    IF (trans_type /= 'none') THEN ! transform
      CALL init(grid_trans, trans, in=gridinfo%griddim, out=griddim_out, &
       categoryappend="gridtransformed")
      IF (ldisplay) THEN
        CALL display(griddim_out)
      ENDIF

      ALLOCATE (fieldz(griddim_out%dim%nx,griddim_out%dim%ny,1))
      CALL compute(grid_trans, field, fieldz)
      CALL delete(grid_trans)
      CALL delete(gridinfo%griddim)
      CALL copy(griddim_out, gridinfo%griddim, categoryappend="cloned")

    ELSE

      fieldz = field

    ENDIF
    
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
    CALL export(gridinfo)
    IF (ldisplay) THEN
      CALL display(gridinfo,namespace="ls")
    ENDIF

    CALL export(gridinfo%gaid, ofile)

    CALL delete(gridinfo)
    DEALLOCATE(field, fieldz)

  ENDDO

  CALL delete(ifile)
  CALL delete(ofile)

ENDIF ! run mode

! general cleanup
CALL delete(griddim_out)
IF (trans_type /= 'none') THEN
  CALL delete(trans)
ENDIF

CALL l4f_category_log(category,L4F_INFO,"end")

!chiudo il logger
CALL l4f_category_delete(category)
ier=l4f_fini()

CONTAINS

SUBROUTINE write_to_file_out(myvolgrid)
TYPE(volgrid6d),POINTER :: myvolgrid(:)

TYPE(grid_file_id) :: file_template
TYPE(grid_id) :: gaid_template

i = word_split(output_file, w_s, w_e, ':')

IF (i == 3) THEN ! template requested (grib_api:template_file:output_file)
  file_template = grid_file_id_new(output_file(w_s(1):w_e(2)), 'r')
  gaid_template = grid_id_new(file_template)
  IF (c_e(gaid_template)) THEN
    CALL export (myvolgrid, filename=output_file(w_s(1):w_e(1))//':'// &
     output_file(w_s(3):w_e(3)), gaid_template=gaid_template, &
     categoryappend="exportgrib_tmpl")
  ELSE
    CALL l4f_category_log(category,L4F_FATAL, &
     "opening output template "//output_file(w_s(1):w_e(2)))
    CALL raise_fatal_error()
  ENDIF

ELSE

  if (output_format(1:8) == "grib_api") then 
#ifdef HAVE_LIBGRIBAPI
    CALL export(myvolgrid,filename=output_file,categoryappend="exportgrib")
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
       filename=trim(output_file)//"_"//t2c(i)//".vdf",reusevdf=reusevdf)
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

END SUBROUTINE write_to_file_out


END PROGRAM vg6d_transform
