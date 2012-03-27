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
PROGRAM v7d_transform
#include "config.h"
USE log4fortran
USE char_utilities
USE file_utilities
USE optionparser_class
USE io_units
USE vol7d_class
USE vol7d_class_compute
USE datetime_class
#ifdef HAVE_ORSIM
USE vol7d_oraclesim_class
#endif
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif
#ifdef HAVE_LIBGRIBAPI
USE grid_id_class
USE grid_class
USE gridinfo_class
#endif
USE grid_transform_class
use volgrid6d_class
USE georef_coord_class
USE vol7d_csv
USE modqc
!USE ISO_FORTRAN_ENV
#ifdef ALCHIMIA
USE alchimia
use vol7d_alchimia_class
USE termo
#endif

IMPLICIT NONE

TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
TYPE(csv_record) :: argparse
CHARACTER(len=8) :: input_format, coord_format

CHARACTER(len=512) :: input_file, output_file, output_format, output_template, &
 network_list, variable_list, anavariable_list, attribute_list, coord_file, output_variable_list, trans_level_list
CHARACTER(len=160) :: pre_trans_type
TYPE(vol7d_network), ALLOCATABLE :: nl(:)
CHARACTER(len=10) :: trans_level_type
CHARACTER(len=10), ALLOCATABLE :: vl(:), avl(:), al(:), alqc(:),vl_alc(:)
CHARACTER(len=23) :: start_date, end_date
CHARACTER(len=20) :: levelc, timerangec
TYPE(datetime) :: s_d, e_d
TYPE(vol7d_level) :: level
TYPE(vol7d_timerange) :: timerange
INTEGER :: ilevel_type, olevel_type
TYPE(vol7d_level) :: ilevel, olevel
TYPE(vol7d_level),ALLOCATABLE :: olevel_list(:)
INTEGER :: iun, ier, i, l, n, ninput, iargc, i1, i2, i3, i4
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d) :: v7d, v7d_coord, v7dtmp, v7d_comp1, v7d_comp2, v7d_comp3
TYPE(arrayof_georef_coord_array) :: poly
DOUBLE PRECISION,ALLOCATABLE :: lon_array(:), lat_array(:)
INTEGER :: polytopo
DOUBLE PRECISION ::  ilon, ilat, flon, flat
DOUBLE PRECISION ::  ielon, ielat, felon, felat
TYPE(geo_coord) :: coordmin,coordmax 
TYPE(transform_def) :: trans
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_dba, v7d_dba_out
#endif
#ifdef HAVE_ORSIM
TYPE(vol7d_oraclesim) :: v7d_osim
#endif
TYPE(vol7d_network) :: set_network_obj
CHARACTER(len=network_name_len) :: set_network
CHARACTER(len=32) :: dsn, user, password
LOGICAL :: version, ldisplay, disable_qc, comp_qc_ndi, comp_qc_perc
CHARACTER(len=512):: a_name
INTEGER :: category

! for computing
LOGICAL :: comp_filter_time, comp_keep, comp_sort, comp_fill_data, obso
LOGICAL :: file, lconvr, round
CHARACTER(len=13) :: comp_stat_proc
CHARACTER(len=9) :: comp_cyclicdatetime=cmiss
CHARACTER(len=23) :: comp_step, comp_fill_tolerance, comp_start, comp_stop
INTEGER :: istat_proc, ostat_proc
TYPE(timedelta) :: c_i, f_t
TYPE(datetime) :: c_s, comp_e
TYPE(cyclicdatetime) :: cyclicdt

REAL :: comp_frac_valid

! for grib output
#ifdef HAVE_LIBGRIBAPI
TYPE(grid_file_id) :: ifile
TYPE(grid_id) :: gaid
TYPE(griddim_def) :: grid_out
TYPE(volgrid6d) :: vg6d(1)
character(len=160) :: post_trans_type
#endif
#ifdef ALCHIMIA
type(fndsv) :: vfn,vfnoracle
!character(len=10), allocatable:: mybin(:)
#endif

!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name,a_name_force="v7d_transform")
!init di log4fortran
ier = l4f_init()
!imposta a_name
category = l4f_category_get(a_name//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Vol7d transformation application, it imports a vol7d volume of sparse point data &
 &from a native vol7d file'&
#ifdef HAVE_DBALLE
 //', from a dbAll.e database, from a BUFR/CREX file'&
#endif
#ifdef HAVE_ORSIM
 //', from SIM Oracle database'&
#endif
 //' and exports it into a native v7d file'&
#ifdef HAVE_DBALLE
 //', into a BUFR/CREX file'&
#endif
#ifdef HAVE_LIBGRIBAPI
 //', into a GRIB file'&
#endif
 //', or into a configurable formatted csv file. &
 &If input-format is of file type, inputfile ''-'' indicates stdin, &
 &if input-format is of database type, inputfile specifies &
 &database access info in the form user/password@dsn, &
 &if empty or ''-'', a suitable default is used. &
 &If output-format is of file type, outputfile ''-'' indicates stdout.', &
 usage_msg='Usage: v7d_transform [options] inputfile1 [inputfile2...] outputfile')

! options for defining input
CALL optionparser_add(opt, ' ', 'input-format', input_format, 'native', help= &
 'format of input, ''native'' for vol7d native binary file'&
#ifdef HAVE_DBALLE
 //', ''BUFR'' for BUFR file with generic template, ''CREX'' for CREX file&
 &, ''dba'' for dballe database'&
#endif
#ifdef HAVE_ORSIM
 //', ''orsim'' for SIM Oracle database'&
#endif
 )
CALL optionparser_add(opt, 'c', 'coord-file', coord_file, help= &
 'file with coordinates of interpolation points, required if a geographical &
 &transformation is requested')
coord_file=cmiss
CALL optionparser_add(opt, ' ', 'coord-format', coord_format, &
#ifdef HAVE_DBALLE
 'BUFR', &
#else
 'native', &
#endif 
 & help='format of input file with coordinates, ''native'' for vol7d native binary file'&
#ifdef HAVE_DBALLE
 //', ''BUFR'' for BUFR file, ''CREX'' for CREX file (sparse points)'&
#endif
#ifdef HAVE_SHAPELIB
 //', ''shp'' for shapefile (sparse points or polygons)'&
#endif
 )

! input database options
CALL optionparser_add(opt, 's', 'start-date', start_date, '', help= &
 'if input-format is of database type, initial date for extracting data')
CALL optionparser_add(opt, 'e', 'end-date', end_date, '', help= &
 'if input-format is of database type, final date for extracting data')
CALL optionparser_add(opt, 'n', 'network-list', network_list, '', help= &
 'if input-format is of database type, list of station networks to be extracted &
 &in the form of a comma-separated list of alphanumeric network identifiers')
CALL optionparser_add(opt, 'v', 'variable-list', variable_list, '', help= &
 'if input-format is of database type, list of data variables to be extracted &
 &in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B13011,B12101''')
CALL optionparser_add(opt, ' ', 'anavariable-list', anavariable_list, '', help= &
 'if input-format is of database type, list of station variables to be extracted &
 &in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B01192,B01193,B07001''')
CALL optionparser_add(opt, ' ', 'attribute-list', attribute_list, '', help= &
 'if input-format is of database type, list of data attributes to be extracted &
 &in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B33196,B33197''')
CALL optionparser_add(opt, ' ', 'level', levelc, ',,,', help= &
 'if input-format is of database type, vertical level to be extracted &
 &in the form level1,l1,level2,l2 empty fields indicate missing data, &
&default is all levels in database')
CALL optionparser_add(opt, ' ', 'timerange', timerangec, ',,', help= &
 'if input-format is of database type, timerange to be extracted &
 &in the form timerange,p1,p2 empty fields indicate missing data, &
 &default is all timeranges in database')
CALL optionparser_add(opt, ' ', 'set-network', set_network, '', help= &
 'if input-format is of database type, collapse all the input data into a single &
 &pseudo-network with the given name, empty for keeping the original networks')
CALL optionparser_add(opt, ' ', 'disable-qc', disable_qc, help= &
 'desable data removing based on SIMC quality control.')

! option for displaying/processing
CALL optionparser_add(opt, 'd', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
CALL optionparser_add(opt, ' ', 'comp-filter-time', comp_filter_time, help= &
 'filter the time series keeping only the data selected by comp-start comp-end comp-step comp-cyclicdatetime')

CALL optionparser_add(opt, ' ', 'comp-cyclicdatetime', comp_cyclicdatetime, help= &
'date and time in the format \c TMMGGhhmm  where any repeated group of char should be / for missing. &
&Take in account only selected year/month/day/hour/minute. &
&You need it to specify for example every january in all years or &
&the same time for all days and so on')

CALL optionparser_add(opt, ' ', 'comp-stat-proc', comp_stat_proc, '', help= &
 'statistically process data with an operator specified in the form [isp:]osp &
 &where isp is the statistical process of input data which has to be processed &
 &and osp is the statistical process to apply and which will appear in output &
 &timerange; possible values for isp and osp are 0=average, 1=accumulated, &
 &2=maximum, 3=minimum, 254=instantaneous, but not all the combinations &
 &make sense; if isp is not provided it is assumed to be equal to osp')

CALL optionparser_add(opt, ' ', 'comp-average', obso, help= &
 'recompute average of averaged fields on a different time step, &
 &obsolete, use --comp-stat-proc 0 instead')
CALL optionparser_add(opt, ' ', 'comp-cumulate', obso, help= &
 'recompute cumulation of accumulated fields on a different time step, &
 &obsolete, use --comp-stat-proc 1 instead')
CALL optionparser_add(opt, ' ', 'comp-step', comp_step, '0000000001 00:00:00.000', help= &
 'length of regularization or statistical processing step in the format &
 &''YYYYMMDDDD hh:mm:ss.msc'', it can be simplified up to the form ''D hh''')
CALL optionparser_add(opt, ' ', 'comp-start', comp_start, '', help= &
 'start of regularization, or statistical processing interval, an empty value means &
 &take the initial time step of the available data; the format is the same as for &
 &--start-date parameter')
CALL optionparser_add(opt, ' ', 'comp-stop', comp_stop, '', help= &
 'stop of filter interval, an empty value means &
 &take the ending time step of the available data; the format is the same as for &
 &--start-date parameter')
CALL optionparser_add(opt, ' ', 'comp-keep', comp_keep, help= &
 'keep the data that are not the result of the requested statistical processing, &
 &merging them with the result of the processing')
CALL optionparser_add(opt, ' ', 'comp-frac-valid', comp_frac_valid, 1., help= &
 'specify the fraction of input data that has to be valid in order to consider a &
 &statistically processed value acceptable')
CALL optionparser_add(opt, ' ', 'comp-sort', comp_sort, help= &
 'sort all sortable dimensions of the volume after the computations')
CALL optionparser_add(opt, ' ', 'comp-fill-data', comp_fill_data, help= &
 'fill missing istantaneous data with nearest in time inside fill-step')
CALL optionparser_add(opt, ' ', 'comp-fill-tolerance', comp_fill_tolerance, '0000000001 00:00:00.000', help= &
 'length of filling step in the format &
 &''YYYYMMDDDD hh:mm:ss.msc'', it can be simplified up to the form ''D hh''')

! option for interpolation processing
CALL optionparser_add(opt, ' ', 'pre-trans-type', pre_trans_type, '', help= &
 'transformation type (sparse points to sparse points) to be applied before &
 &other computations, in the form ''trans-type:subtype''; &
 &''inter'' for interpolation, with subtypes ''near'', ''linear'', ''bilin'''&
#ifdef HAVE_SHAPELIB
 //'; ''polyinter'' for statistical processing within given polygons, &
 &with subtype ''average'', ''stddev'', ''max'', ''min'''&
#endif
 //'; ''metamorphosis'' with subtype ''coordbb'' for selecting only data &
 &within a given bounding box&
 &; empty for no transformation')

CALL optionparser_add(opt, ' ', 'trans-level-type', trans_level_type, '100', help= &
 'type of input and output level for vertical interpolation &
 &in the form [inlev:]outlev, from grib2 table, at the moment &
 &input and output type must be the same and only single levels are supported')
CALL optionparser_add(opt, ' ', 'trans-level-list', trans_level_list, '50000,70000,85000,100000', help= &
 'list of output levels for vertical interpolation, the unit is determined &
 &by the value of level-type and taken from grib2 table')

#ifdef HAVE_LIBGRIBAPI
CALL optionparser_add(opt, ' ', 'post-trans-type', post_trans_type, '', help= &
 'transformation type (sparse points to grid) to be applied after &
 &other computations, in the form ''trans-type:subtype''; &
 &''inter'' for interpolation, with subtype ''linear''; &
 &''boxinter'' for statistical processing within output grid box, &
 &with subtype ''average'', ''stddev'', ''max'', ''min''; &
 &empty for no transformation; this option is compatible with output &
 &on gridded format only (see output-format)')
#endif

CALL optionparser_add(opt, ' ', 'ilon', ilon, 0.0D0, help= &
 'longitude of the southwestern bounding box corner for pre-transformation')
CALL optionparser_add(opt, ' ', 'ilat', ilat, 30.D0, help= &
 'latitude of the southwestern bounding box corner for pre-transformation')
CALL optionparser_add(opt, ' ', 'flon', flon, 30.D0, help= &
 'longitude of the northeastern bounding box corner for pre-transformation')
CALL optionparser_add(opt, ' ', 'flat', flat, 60.D0, help= &
 'latitude of the northeastern bounding box corner for pre-transformation')


CALL optionparser_add(opt, ' ', 'ielon', ielon, dmiss, help= &
 'longitude of the southwestern bounding box corner for import')
CALL optionparser_add(opt, ' ', 'ielat', ielat, dmiss, help= &
 'latitude of the southwestern bounding box corner for import')
CALL optionparser_add(opt, ' ', 'felon', felon, dmiss, help= &
 'longitude of the northeastern bounding box corner for import')
CALL optionparser_add(opt, ' ', 'felat', felat, dmiss, help= &
 'latitude of the northeastern bounding box corner for import')


CALL optionparser_add(opt, ' ', 'comp-qc-ndi', comp_qc_ndi, help= &
 'enable compute of index (NDI) for use by Quality Control.')
CALL optionparser_add(opt, ' ', 'comp-qc-perc', comp_qc_perc, help= &
 'enable compute of index (percentile) for use by Quality Control.')

! options for defining output
output_template = ''
CALL optionparser_add(opt, ' ', 'output-format', output_format, 'native', help= &
 'format of output file, in the form ''name[:template]''; ''native'' for vol7d &
 &native binary format (no template to be specified)'&
#ifdef HAVE_DBALLE
 //'; ''BUFR'' and ''CREX'' for corresponding formats, with template in the form &
 &''category.subcategory.localcategory'' or as an alias like ''synop'', ''metar'', &
 &''temp'', ''generic'', empty for ''generic'''&
#endif
#ifdef HAVE_LIBGRIBAPI
 //'; ''grib_api'' for gridded output in grib format, template (required) is the &
 &path name of a grib file in which the first message defines the output grid and &
 &is used as a template for the output grib messages, (see also post-trans-type)'&
#endif
 //'; csv for formatted csv format (no template to be specified)')

! options for configuring csv output
CALL optionparser_add(opt, ' ', 'csv-volume', csv_volume, 'all', help= &
 'vol7d volumes to be output to csv: ''all'' for all volumes, &
 &''ana'' for station volumes only or ''data'' for data volumes only')
CALL optionparser_add(opt, ' ', 'csv-column', csv_column, &
 'time,timerange,ana,level,network', help= &
 'list of columns (excluding variables) that have to appear in csv output: &
 &a comma-separated combination of ''time,timerange,level,ana,network'' &
 &in the desired order')
CALL optionparser_add(opt, ' ', 'csv-columnorder', csv_columnorder, &
 'time,timerange,ana,level,network', help= &
 'order of looping on columns (excluding variables) that have to appear in &
 &csv output, the format is the same as for the --csv-column parameter &
 &but here all the column identifiers have to be present')
CALL optionparser_add(opt, ' ', 'csv-variable', csv_variable, 'all', help= &
 'list of variables that have to appear in the data columns of csv output: &
 &''all'' or a comma-separated list of B-table alphanumeric codes, e.g. &
 &''B10004,B12101'' in the desired order')
CALL optionparser_add(opt, ' ', 'csv-header', csv_header, 2, help= &
 'write 0 to 2 header lines at the beginning of csv output')
CALL optionparser_add(opt, ' ', 'csv-keep-miss', csv_keep_miss, help= &
 'keep records containing only missing values in csv output')
CALL optionparser_add(opt, ' ', 'csv-norescale', csv_no_rescale, help= &
 'do not rescale in output integer variables according to their scale factor')

! obsolete options
CALL optionparser_add(opt, ' ', 'comp-discard', obso, help= &
 'obsolete option, use --comp-keep with opposite meaning')
CALL optionparser_add(opt, ' ', 'csv-skip-miss', obso, help= &
 'obsolete option, use --csv-keep-miss with opposite meaning')

#ifdef ALCHIMIA
CALL optionparser_add(opt, '', 'output-variable-list', output_variable_list, '', help= &
 'list of data variables you require in output; if they are not in input they will be computed if possible. &
 &The output_variable_list is expressed in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B13011,B12101''')
#endif

CALL optionparser_add(opt, ' ', 'rounding', round, help= &
 'simplifies volume, merging similar levels and timeranges')

! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add_html(opt, ' ', 'html-form', help= &
 &'print the options as an html form')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0) ! generate a clean manpage
ELSE IF (optstatus == optionparser_html) THEN
  CALL exit(0) ! generate a clean form
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line parameters')
  CALL raise_fatal_error()
ENDIF
IF (version) THEN
  WRITE(*,'(A,1X,A)')'v7d_transform',VERSION
  CALL exit(0)
ENDIF

! check input/output files
i = iargc() - optind
IF (i < 0) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'input file missing')
  CALL raise_fatal_error()
ELSE IF (i < 1) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'output file missing')
  CALL raise_fatal_error()
ENDIF
CALL getarg(iargc(), output_file)

! check obsolete arguments
IF (obso) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category, L4F_ERROR, &
   'arguments --comp-average --comp-cumulate --csv-skip-miss --comp-discard')
  CALL l4f_category_log(category, L4F_ERROR, &
   'are obsolete, please read help')
    CALL raise_fatal_error()
ENDIF

! generate network
IF (LEN_TRIM(network_list) > 0) THEN
  n = word_split(network_list, w_s, w_e, ',')
  ALLOCATE(nl(n))
  DO i = 1, n
    CALL init(nl(i), name=network_list(w_s(i):w_e(i)))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF
! generate variable lists
IF (LEN_TRIM(variable_list) > 0) THEN
  n = word_split(variable_list, w_s, w_e, ',')
  ALLOCATE(vl(n))
  DO i = 1, n
    vl(i) = variable_list(w_s(i):w_e(i))
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
IF (LEN_TRIM(attribute_list) > 0) THEN

  n = word_split(attribute_list, w_s, w_e, ',')
  ALLOCATE(al(n))
  DO i = 1, n
    al(i) = attribute_list(w_s(i):w_e(i))
  ENDDO
  DEALLOCATE(w_s, w_e)

  IF (.NOT.disable_qc) THEN ! add qc variables not specified yet to alqc
! al is the list of attributes requested by the user
! alqc is al completed by the attributes required by qc
    DO i = 1, nqcattrvars
      IF (ALL(qcattrvarsbtables(i) /= al(:))) THEN
        n = n + 1
      ENDIF
    ENDDO
    ALLOCATE(alqc(n))
    alqc(1:SIZE(al)) = al(:)

    n = SIZE(al)
    DO i = 1, nqcattrvars
      IF (ALL(qcattrvarsbtables(i) /= al(:))) THEN
        n = n + 1
        alqc(n) = qcattrvarsbtables(i)
      ENDIF
    ENDDO

  ELSE ! duplicate al
! al is the list of attributes requested by the user
! alqc is the same list
    ALLOCATE(alqc(n))
    alqc(1:SIZE(al)) = al(:)

  ENDIF

ELSE ! no attributes requested
  ALLOCATE(al(0)) ! an empty al is required for safety
  IF (.NOT.disable_qc) THEN ! set alqc to qc variables
! al is empty
! alqc is the list of the attributes required by qc
    ALLOCATE(alqc(nqcattrvars))
    alqc(:) = qcattrvarsbtables(:)
  ELSE ! an empty alqc is required for safety
! al is empty
! alqc is empty
    ALLOCATE(alqc(0))
  ENDIF

ENDIF


! generate variable lists
IF (LEN_TRIM(output_variable_list) > 0) THEN
  n = word_split(output_variable_list, w_s, w_e, ',')
  ALLOCATE(vl_alc(n))
  DO i = 1, n
    vl_alc(i) = output_variable_list(w_s(i):w_e(i))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF


! time-related arguments
s_d = datetime_new(isodate=start_date)
e_d = datetime_new(isodate=end_date)
c_i = timedelta_new(isodate=comp_step)
f_t = timedelta_new(isodate=comp_fill_tolerance)

IF (comp_start /= '') THEN
  c_s = datetime_new(isodate=comp_start)
ELSE
  c_s = datetime_miss
ENDIF

IF (comp_stop /= '') THEN
  comp_e = datetime_new(isodate=comp_stop)
ELSE
  comp_e = datetime_miss
ENDIF

! check level
CALL init(argparse, levelc, ',', nfield=n)
IF (n < 4) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, wrong syntax for --level: ' &
   //TRIM(levelc))
  CALL raise_fatal_error()
ENDIF
CALL csv_record_getfield(argparse, i1)
CALL csv_record_getfield(argparse, i2)
CALL csv_record_getfield(argparse, i3)
CALL csv_record_getfield(argparse, i4)
CALL init(level, level1=i1, l1=i2, level2=i3, l2=i4)
CALL delete(argparse)

! check timerange
CALL init(argparse, timerangec, ',', nfield=n)
IF (n < 3) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, wrong syntax for --timerange: ' &
   //TRIM(timerangec))
  CALL raise_fatal_error()
ENDIF
CALL csv_record_getfield(argparse, i1)
CALL csv_record_getfield(argparse, i2)
CALL csv_record_getfield(argparse, i3)
CALL init(timerange, timerange=i1, p1=i2, p2=i3)
CALL delete(argparse)

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

CALL init(v7d_coord)
! import coord_file
IF (c_e(coord_file)) THEN
  IF (coord_format == 'native') THEN
    CALL import(v7d_coord, filename=input_file)

#ifdef HAVE_DBALLE
  ELSE IF (coord_format == 'BUFR' .OR. coord_format == 'CREX') THEN
    CALL init(v7d_dba, filename=coord_file, format=coord_format, file=.TRUE., &
     write=.FALSE., categoryappend="anagrafica")
    CALL import(v7d_dba, anaonly=.TRUE.)
    v7d_coord = v7d_dba%vol7d
! destroy v7d_ana without deallocating the contents passed to v7d
    CALL init(v7d_dba%vol7d)
    CALL delete(v7d_dba)

#endif
#ifdef HAVE_SHAPELIB
  ELSE IF (coord_format == 'shp') THEN
    CALL import(poly, coord_file)
    IF (poly%arraysize <= 0) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error importing shapefile '//TRIM(coord_file))
      CALL raise_fatal_error()
    ENDIF
    CALL getval(poly%array(1), topo=polytopo)
    IF (polytopo == georef_coord_array_point) THEN ! topology suitable for sparse points
      CALL vol7d_alloc(v7d_coord, nana=poly%arraysize)
      CALL vol7d_alloc_vol(v7d_coord)
      DO i = 1, poly%arraysize
        CALL getval(poly%array(i), x=lon_array,y=lat_array) ! improve!!!!
        CALL init(v7d_coord%ana(i), lon=lon_array(1), lat=lat_array(1))
      ENDDO
      CALL delete(poly)
      CALL l4f_category_log(category,L4F_INFO, &
       'shapefile '//TRIM(coord_file)//' interpreted as sparse point list')
    ELSE ! topology suitable for polygons, nothing to do
      CALL l4f_category_log(category,L4F_INFO, &
       'shapefile '//TRIM(coord_file)//' interpreted as polygon list')
    ENDIF

#endif
  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(coord_format)//' in --coord-format not valid or not supported.')
      CALL raise_fatal_error()
  ENDIF

ENDIF

! check level_type
ilevel_type = imiss
olevel_type = imiss
IF (trans_level_type /= '') THEN
  CALL init(argparse, trans_level_type, ':', nfield=n)
  IF (n == 1) THEN
    CALL csv_record_getfield(argparse, olevel_type)
    ilevel_type = olevel_type
  ELSE  IF (n == 2) THEN
    CALL csv_record_getfield(argparse, ilevel_type)
    CALL csv_record_getfield(argparse, olevel_type)
  ENDIF
  CALL delete(argparse)
  IF (.NOT.c_e(ilevel_type) .OR. .NOT.c_e(olevel_type)) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, wrong syntax for --trans-level-type: ' &
     //TRIM(trans_level_type))
    CALL raise_fatal_error()
  ENDIF
! temporary
  IF (ilevel_type /= olevel_type) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, input and output level types must be equal: ' &
     //TRIM(trans_level_type))
    CALL raise_fatal_error()
  ENDIF
ENDIF
CALL init(ilevel, ilevel_type)
CALL init(olevel, olevel_type)

! make level_list
CALL init(argparse, trans_level_list, ',', nfield=n)
IF (n > 0 .AND. c_e(olevel_type)) THEN
  ALLOCATE(olevel_list(n))
  DO i = 1, n
    CALL csv_record_getfield(argparse, l)
    CALL init(olevel_list(i), olevel_type, l)
  ENDDO
ENDIF
CALL delete(argparse)

! check csv-column
CALL parse_v7d_column(csv_column, icsv_column, '--csv-column', .FALSE.)
CALL parse_v7d_column(csv_columnorder, icsv_columnorder, '--csv-columnorder', .TRUE.)

! check output format/template
n = word_split(output_format, w_s, w_e, ':')
IF (n >= 2) THEN ! set output template if present
  output_template = output_format(w_s(2):w_e(2))
  output_format(w_e(1)+1:) = ' '
ENDIF
DEALLOCATE(w_s, w_e)


IF (.NOT.ALLOCATED(vl)) ALLOCATE(vl(0)) ! allocate if missing
IF (.NOT.ALLOCATED(avl)) ALLOCATE(avl(0)) ! allocate if missing
IF (.NOT.ALLOCATED(al)) ALLOCATE(al(0)) ! allocate if missing
IF (.NOT.ALLOCATED(nl)) allocate (nl(0))
IF( .NOT.ALLOCATED(vl)) allocate (vl(0))

IF (set_network /= '') THEN
  CALL init(set_network_obj, name=set_network)
ELSE
  set_network_obj = vol7d_network_miss
ENDIF

call init(coordmin, lon=ielon, lat=ielat)
call init(coordmax, lon=felon, lat=felat)

! import data looping on input files
CALL init(v7d)
DO ninput = optind, iargc()-1
  CALL getarg(ninput, input_file)

  IF (input_format == 'native') THEN

    IF (c_e(coordmin) .OR. c_e(coordmax)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       '--ielat, --ielon, --felat, --felon not usable with native source')
      CALL raise_fatal_error()
    ENDIF

    IF (c_e(level) .OR. c_e(timerange) .OR. SIZE(avl) > 0 .OR. SIZE(al) > 0 &
     .OR. c_e(set_network_obj) .OR. c_e(s_d) .OR. c_e(e_d)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'selection options like date, level timerange or others not usable with native source')
      CALL raise_fatal_error()
    ENDIF

    IF (input_file == '-') THEN ! stdin_unit does not work with unformatted
      CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit.')
      input_file='/dev/stdin'
    ENDIF
    CALL import(v7dtmp, filename=input_file)

#ifdef HAVE_DBALLE
  ELSE IF (input_format == 'BUFR' .OR. input_format == 'CREX' .OR. input_format == 'dba') THEN

    IF (input_format == 'BUFR' .OR. input_format == 'CREX') then

      IF (input_file == '-') THEN
        CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit.')
        input_file='/dev/stdin'
      ENDIF
      file=.TRUE.

    ELSE IF (input_format == 'dba') THEN
      CALL parse_dba_access_info(input_file, dsn, user, password)
      file=.FALSE.
    ENDIF
    
    CALL init(v7d_dba, filename=input_file, FORMAT=input_format, &
     dsn=dsn, user=user, password=password, file=file)

    CALL import(v7d_dba, vl, nl, &
     level=level, timerange=timerange, &
     anavar=avl, attr=alqc, set_network=set_network_obj, &
     coordmin=coordmin, coordmax=coordmax, &
     timei=s_d, timef=e_d)
    
    v7dtmp = v7d_dba%vol7d
    CALL init(v7d_dba%vol7d) ! nullify without deallocating
    CALL delete(v7d_dba)

#endif

#ifdef HAVE_ORSIM
  ELSE IF (input_format == 'orsim') THEN

    if (c_e(coordmin) .or. c_e(coordmax)) then
      CALL l4f_category_log(category, L4F_ERROR, &
       '--ielat, --ielon, --felat, --felon not usable with SIM Oracle source')
      CALL raise_fatal_error()
    end if

    IF (.NOT.ALLOCATED(nl) .OR. (.NOT.ALLOCATED(vl) .AND. .NOT.ALLOCATED(avl))) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error in command-line parameters, it is necessary to provide --network-list')
      CALL l4f_category_log(category, L4F_ERROR, &
       'and either --variable-list or --anavariable-list with SIM Oracle source')
      CALL raise_fatal_error()
    ENDIF
    CALL parse_dba_access_info(input_file, dsn, user, password)
    CALL init(v7d_osim, dsn=dsn, user=user, password=password, time_definition=0)
    IF (SIZE(vl) > 0) THEN ! data requested
      CALL import(v7d_osim, vl, nl, timei=s_d, timef=e_d, &
       level=level, timerange=timerange, &
!       coordmin=coordmin, coordmax=coordmax, &
       anavar=avl, attr=alqc, set_network=set_network_obj)
    ELSE ! ana requested
      CALL import(v7d_osim, nl, anavar=avl, set_network=set_network_obj &
!       , coordmin=coordmin, coordmax=coordmax &
       )
    ENDIF
    v7dtmp = v7d_osim%vol7d
    CALL init(v7d_osim%vol7d) ! nullify without deallocating
#endif

  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(input_format)//' in --input-format not valid or not supported.')
    CALL raise_fatal_error()
  ENDIF

  CALL vol7d_merge(v7d, v7dtmp) ! smart merge in v7d
  CALL delete(v7dtmp)

ENDDO

CALL delete(opt) ! check whether I can already get rid of this stuff now

! displaying/processing
#ifdef HAVE_DBALLE
CALL vol7d_dballe_set_var_du(v7d)
#endif

IF (ldisplay) then
  print*," >>>>> Input Volume <<<<<"
  CALL display(v7d)
end IF

! apply quality control data removing
IF (.NOT.disable_qc) THEN
#ifdef HAVE_ORSIM
  IF (input_format == 'orsim') THEN ! no attributes means no attributes!
    CALL vol7d_peeling(v7d, keep_attr=al)
  ELSE
#endif
    IF (SIZE(al) == 0) THEN ! no attributes means all attributes!
      CALL vol7d_peeling(v7d, delete_attr=al)
    ELSE
      CALL vol7d_peeling(v7d, keep_attr=al)
    ENDIF
#ifdef HAVE_ORSIM
  ENDIF
#endif
ENDIF

! conversion to real required in these cases
IF ((c_e(istat_proc) .AND. c_e(ostat_proc)) .OR. pre_trans_type /= '' &
#ifdef HAVE_LIBGRIBAPI
 .OR. (post_trans_type /= '' .AND. output_template /= '') &
#endif
 .or. comp_fill_data .or. comp_qc_ndi .or. comp_qc_perc &
 ) THEN

  lconvr=.false.
  IF (ASSOCIATED(v7d%dativar%d)) THEN
    if (SIZE(v7d%dativar%d) > 0) lconvr=.true.
  ENDIF
  IF (ASSOCIATED(v7d%dativar%i)) THEN
    if (SIZE(v7d%dativar%i) > 0) lconvr=.true.
  ENDIF
  IF (ASSOCIATED(v7d%dativar%b)) THEN
    if (SIZE(v7d%dativar%b) > 0) lconvr=.true.
  ENDIF
  IF (ASSOCIATED(v7d%dativar%c)) THEN
    if (SIZE(v7d%dativar%c) > 0) lconvr=.true.
  ENDIF

  IF (lconvr) THEN
    CALL l4f_category_log(category, L4F_INFO, 'Converting input data to real for processing.')
    call vol7d_convr(v7d,v7dtmp)
    call delete(v7d)
    v7d=v7dtmp
    CALL init(v7dtmp) ! detach it
  end if
ENDIF

IF (pre_trans_type /= '') THEN
  n = word_split(pre_trans_type, w_s, w_e, ':')
  IF (n >= 2) THEN ! syntax is correct
!    IF (poly%arraysize <= 0) THEN ! improve
! if/elseprobably not needed anymore, delete!!!
      CALL init(trans, trans_type=pre_trans_type(w_s(1):w_e(1)), &
       ilon=ilon, ilat=ilat, flon=flon, flat=flat, poly=poly, &
       input_levtype=ilevel, output_levtype=olevel, &
       sub_type=pre_trans_type(w_s(2):w_e(2)), categoryappend="transformation1")
!    ELSE
!      CALL init(trans, trans_type=pre_trans_type(w_s(1):w_e(1)), &
!       ilon=ilon, ilat=ilat, flon=flon, flat=flat, &
!       input_levtype=ilevel, output_levtype=olevel, &
!       sub_type=pre_trans_type(w_s(2):w_e(2)), categoryappend="transformation1")
!    ENDIF
    CALL transform(trans, vol7d_in=v7d, vol7d_out=v7d_comp1, v7d=v7d_coord, &
     lev_out=olevel_list, categoryappend="transform1")
    CALL delete(trans)
  ELSE ! syntax is wrong
    CALL init(v7d_comp1)
    CALL l4f_category_log(category, L4F_ERROR, &
     'pre-transformation syntax '//TRIM(pre_trans_type)//' non valid')
    CALL raise_fatal_error()
  ENDIF
  DEALLOCATE(w_s, w_e)

  IF (.NOT.c_e(v7d_comp1)) THEN ! empty volume
    CALL l4f_category_log(category, L4F_WARN, &
     'pre-transformation '//TRIM(pre_trans_type)//' returned no points')
  ENDIF

  v7d = v7d_comp1
  CALL init(v7d_comp1) ! detach it
ENDIF



IF (comp_fill_data ) THEN

  cyclicdt = cyclicdatetime_new(chardate=comp_cyclicdatetime)

  CALL init(v7d_comp1)
  CALL vol7d_fill_time(v7d, v7d_comp1, step=c_i, start=c_s, stopp=comp_e, cyclicdt=cyclicdt)
  CALL delete(v7d)
  v7d = v7d_comp1
  CALL init(v7d_comp1) ! detach it

  CALL vol7d_fill_data(v7d, step=c_i, start=c_s, stopp=comp_e, tolerance=f_t)

ENDIF



IF (comp_filter_time ) THEN

  cyclicdt = cyclicdatetime_new(chardate=comp_cyclicdatetime)

  CALL init(v7d_comp1)
  CALL vol7d_filter_time(v7d, v7d_comp1, step=c_i, start=c_s, stopp=comp_e, cyclicdt=cyclicdt)
  CALL delete(v7d)
  v7d = v7d_comp1
  CALL init(v7d_comp1) ! detach it
ENDIF


IF (c_e(istat_proc) .AND. c_e(ostat_proc)) THEN
  CALL init(v7d_comp1, time_definition=v7d%time_definition)
  CALL init(v7d_comp2, time_definition=v7d%time_definition)

  lconvr=.false.
  IF (ASSOCIATED(v7d%dativar%d)) THEN
    if (SIZE(v7d%dativar%d) > 0) lconvr=.true.
  ENDIF
  IF (ASSOCIATED(v7d%dativar%i)) THEN
    if (SIZE(v7d%dativar%i) > 0) lconvr=.true.
  ENDIF
  IF (ASSOCIATED(v7d%dativar%b)) THEN
    if (SIZE(v7d%dativar%b) > 0) lconvr=.true.
  ENDIF
  IF (ASSOCIATED(v7d%dativar%c)) THEN
    if (SIZE(v7d%dativar%c) > 0) lconvr=.true.
  ENDIF

  !IF (input_format == 'BUFR' .OR. input_format == 'CREX') THEN
  IF (lconvr) THEN
    call vol7d_convr(v7d,v7dtmp)
    call delete(v7d)
    v7d=v7dtmp
    CALL init(v7dtmp) ! detach it
  end if

  CALL vol7d_compute_stat_proc(v7d, v7d_comp1, istat_proc, ostat_proc, c_i, c_s, &
   full_steps=.TRUE., frac_valid=comp_frac_valid, &
   max_step=timedelta_depop(c_i)/10, weighted=.TRUE., other=v7d_comp3)

  CALL delete(v7d)
  v7d = v7d_comp3
  CALL init(v7d_comp3) ! detach it

! merge the two computed fields
  IF (.NOT. comp_keep) THEN ! the user is not interested in the other volume
    CALL delete(v7d)
    v7d = v7d_comp1
    CALL init(v7d_comp1) ! detach it
  ELSE
    CALL vol7d_merge(v7d, v7d_comp1, sort=.TRUE.)
  ENDIF
ENDIF




! sort
IF (comp_sort) THEN
  CALL vol7d_smart_sort(v7d, lsort_time=.TRUE., lsort_timerange=.TRUE., lsort_level=.TRUE.)
ENDIF


if (round) then
  call rounding(v7d,v7dtmp,level=almost_equal_levels,nostatproc=.true.)
  CALL delete(v7d)
  v7d= v7dtmp
  CALL init(v7dtmp) ! detach it
end if


#ifdef ALCHIMIA
if (output_variable_list /= " ") then

  call register_termo(vfn)

  if (alchemy(v7d,vfn,vl_alc,v7dtmp,copy=.true., vfnoracle=vfnoracle) == 0 ) then
    call display(vfnoracle)
    CALL delete(v7d)
    v7d = v7dtmp
    CALL init(v7dtmp) ! detach it
  else
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
end if
#endif


if (comp_qc_ndi) then
  call vol7d_compute_NormalizedDensityIndex(v7d,v7dtmp, perc_vals=(/(10.*i,i=0,10)/))
  call delete(v7d)
  v7d=v7dtmp
  CALL init(v7dtmp) ! detach it
end if

if (comp_qc_perc) then
  call vol7d_compute_percentile(v7d,v7dtmp, perc_vals=(/25.,50.,75./))
  call delete(v7d)
  v7d=v7dtmp
  CALL init(v7dtmp) ! detach it
end if

if (ldisplay) then
  print*," >>>>> Output Volume <<<<<"
  CALL display(v7d)
end if

! output
IF (output_format == 'native') THEN
  IF (output_file == '-') THEN ! stdout_unit does not work with unformatted
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  iun = getunit()
  OPEN(iun, file=output_file, form='UNFORMATTED', access=stream_if_possible)
  CALL export(v7d, unit=iun)
  CLOSE(iun)
  CALL delete(v7d)

ELSE IF (output_format == 'csv') THEN
  IF (output_file == '-') THEN
    iun = stdout_unit
  ELSE
    iun = getunit()
    OPEN(iun, file=output_file, form='FORMATTED', access='SEQUENTIAL')
  ENDIF
  CALL csv_export(v7d, iun)
  IF (output_file /= '-') CLOSE(iun)
  CALL delete(v7d)

#ifdef HAVE_DBALLE
ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX' .OR. output_format == 'dba') THEN
  IF (output_format == 'BUFR' .OR. output_format == 'CREX') THEN
    IF (output_file == '-') THEN
      CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
      output_file='/dev/stdout'
    ENDIF
    file=.TRUE.

  ELSE IF (output_format == 'dba') THEN
    CALL parse_dba_access_info(output_file, dsn, user, password)
    file=.FALSE.
  ENDIF

  IF (output_template == '') output_template = 'generic'
! check whether wipe=file is reasonable
  CALL init(v7d_dba_out, filename=output_file, FORMAT=output_format, &
   dsn=dsn, user=user, password=password, file=file, WRITE=.TRUE., wipe=file)

  v7d_dba_out%vol7d = v7d
  CALL init(v7d) ! nullify without deallocating
  CALL export(v7d_dba_out, template=output_template)
  CALL delete(v7d_dba_out)
#endif

#ifdef HAVE_LIBGRIBAPI
ELSE IF (output_format == 'grib_api') THEN

  IF (post_trans_type /= '' .AND. output_template /= '') THEN
    n = word_split(post_trans_type, w_s, w_e, ':')
    IF (n >= 2) THEN ! syntax is correct

! initialize transform
      CALL init(trans, trans_type=post_trans_type(w_s(1):w_e(1)), &
       sub_type=post_trans_type(w_s(2):w_e(2)), categoryappend="transformation2")
! open grib template file and import first message, format:template is
! reconstructed here, improve
      ifile = grid_file_id_new(TRIM(output_format)//':'//TRIM(output_template), 'r')
      gaid = grid_id_new(ifile)
      IF (c_e(gaid)) THEN

! use the message  as a template for defining the grid
        CALL import(grid_out, gaid)
! interpolate sparse data over the requested grid
        CALL transform(trans, grid_out, v7d, vg6d(1), categoryappend="transform2")
! TODO check here whether the transformation succeeded export the
! interpolated volume to file keeping the same grib template used for
! the grid
        CALL export(vg6d, output_file, gaid_template=gaid)
        CALL delete(vg6d(1))

      ELSE
        CALL l4f_category_log(category,L4F_ERROR, &
         'cannot read any grib message from template file '//TRIM(output_template))
      ENDIF
      CALL delete(trans)

    ELSE ! syntax is wrong
      CALL l4f_category_log(category, L4F_ERROR, &
       'post-transformation syntax '//TRIM(post_trans_type)//' non valid')
    ENDIF
    DEALLOCATE(w_s, w_e)

  ELSE
    CALL l4f_category_log(category,L4F_ERROR, &
     'output format '//TRIM(output_format)// &
     ' requires post-trans-type and output-template to be defined')
    CALL l4f_category_log(category,L4F_ERROR, &
     'post-trans-type: '//TRIM(post_trans_type)// &
     '; output template '//TRIM(output_template))
    CALL raise_fatal_error()
  ENDIF

#endif

ELSE IF (output_format /= '') THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, format '// &
   TRIM(output_format)//' in --output-format not valid or not supported.')
  CALL raise_fatal_error()
ENDIF

! cleanly close the databases
IF (input_format == 'native') THEN
  CALL delete(v7d) ! controllare? input native output bufr
#ifdef HAVE_ORSIM
ELSE IF (input_format == 'orsim') THEN
  CALL delete(v7d_osim)
#endif
ENDIF


#ifdef ALCHIMIA
call delete(vfn)
call delete(vfnoracle)
#endif


ier = l4f_fini()

CONTAINS

SUBROUTINE parse_dba_access_info(string, dsn, user, password)
CHARACTER(len=*),INTENT(in) :: string
CHARACTER(len=*),INTENT(out) :: dsn
CHARACTER(len=*),INTENT(out) :: user
CHARACTER(len=*),INTENT(out) :: password

INTEGER :: bar, at

IF (string == '-' .OR. string == '') THEN
  dsn = cmiss
  user = cmiss
  password = cmiss
ELSE
  bar = INDEX(string, '/')
  at = INDEX(string, '@')
  IF (bar > 0 .AND. at > bar) THEN
    user = string(:bar-1)
    password = string(bar+1:at-1)
    dsn = string(at+1:)
  ELSE
    CALL optionparser_printhelp(opt)
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, database access info '// &
     TRIM(string)//' not valid.')
    CALL raise_fatal_error()
  ENDIF
ENDIF

END SUBROUTINE parse_dba_access_info


SUBROUTINE parse_v7d_column(ccol, icol, par_name, check_all)
CHARACTER(len=*),INTENT(in) :: ccol
INTEGER,INTENT(out) :: icol(:)
CHARACTER(len=*),INTENT(in) :: par_name
LOGICAL,INTENT(in) :: check_all

INTEGER :: i, j, nc
INTEGER,POINTER :: w_s(:), w_e(:)

nc = word_split(ccol, w_s, w_e, ',')
j = 0
icol(:) = -1
DO i = 1, MIN(nc, SIZE(icol))
  SELECT CASE(ccol(w_s(i):w_e(i)))
  CASE('time')
    j = j + 1
    icol(j) = vol7d_time_d
  CASE('timerange')
    j = j + 1
    icol(j) = vol7d_timerange_d
  CASE('level')
    j = j + 1
    icol(j) = vol7d_level_d
  CASE('ana')
    j = j + 1
    icol(j) = vol7d_ana_d
  CASE('var')
    j = j + 1
    icol(j) = vol7d_var_d
  CASE('network')
    j = j + 1
    icol(j) = vol7d_network_d
  CASE('value')
    j = j + 1
    icol(j) = 7
  CASE default
    CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters, column '// &
     ccol(w_s(i):w_e(i))//' in '//TRIM(par_name)//' not valid.')
    CALL raise_fatal_error()
  END SELECT
ENDDO
nc = j
DEALLOCATE(w_s, w_e)

IF (check_all) THEN
  IF (ALL(icol /= vol7d_time_d) .OR. ALL(icol /= vol7d_timerange_d) .OR. &
   ALL(icol /= vol7d_level_d) .OR. ALL(icol /= vol7d_ana_d) .OR. &
   ALL(icol /= vol7d_network_d)) THEN
    CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters, some columns missing in '//TRIM(par_name)//' .')
    CALL raise_fatal_error()
  ENDIF
  IF (ANY(icol == 7)) THEN
    CALL l4f_category_log(category,L4F_ERROR,"column 'value' not valid in "// &
     TRIM(par_name)//' .')
    CALL raise_fatal_error()
  ENDIF
ENDIF

END SUBROUTINE parse_v7d_column

END PROGRAM v7d_transform

