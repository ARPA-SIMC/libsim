PROGRAM vg6d_getpoint
#include "config.h"
use log4fortran
use volgrid6d_class
use grid_class
use grid_transform_class
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif
USE vol7d_class
use getopt_m
USE io_units
USE geo_coord_class

implicit none

TYPE(op_option) :: options(40) ! remember to update dimension when adding options
TYPE(optionparser) :: opt
CHARACTER(len=8) :: coord_format, output_format
CHARACTER(len=512) :: a_name, coord_file, input_file, output_file, &
 network_list, variable_list
INTEGER :: category, ier, i, iun, iargc
character(len=network_name_len) :: network
type(volgrid6d),pointer :: volgrid(:)
type(transform_def) :: trans
type(vol7d) :: v7d_coord
type(vol7d),pointer :: v7d_out(:)
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_ana, v7d_dba_out
#endif
TYPE(geo_coordvect),POINTER :: poly(:)
doubleprecision :: lon, lat
character(len=80) :: output_template,trans_type,sub_type
INTEGER :: output_td
LOGICAL :: version, ldisplay

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="getpoint")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

! define command-line options
CALL op_option_nullify(options)

! options for transformation
options(1) = op_option_new('a', 'lon', lon, 0.D0, help= &
 'longitude of single interpolation point, alternative to --coord-file')
options(2) = op_option_new('b', 'lat', lat, 45.D0, help= &
 'latitude of single interpolation point, alternative to --coord-file')
options(3) = op_option_new('c', 'coord-file', coord_file, help= &
 'file with coordinates of interpolation points, alternative to --lon, --lat; &
 &no coordinate information is required for metamorphosis transformation')
coord_file=cmiss
options(4) = op_option_new(' ', 'coord-format', coord_format, &
#ifdef HAVE_DBALLE
'BUFR', &
#else
'native', &
#endif 
& help='format of input file with coordinates, ''native'' for vol7d native binary file &
#ifdef HAVE_DBALLE
 &, ''BUFR'' for BUFR file, ''CREX'' for CREX file&
#endif
#ifdef HAVE_LIBSHP_FORTRAN
 &, ''shp'' for shapefile (interpolation on polygons)&
#endif
 &')
options(10) = op_option_new('v', 'trans-type', trans_type, 'inter', help= &
 'transformation type, ''inter'' for interpolation or ''metamorphosis'' &
 &for keeping the same data but changing the container from grib to v7d&
#ifdef HAVE_LIBSHP_FORTRAN
 &, ''polyinter'' for statistical processing within given polygons&
#endif
 &')
options(11) = op_option_new('z', 'sub-type', sub_type, 'bilin', help= &
 'transformation subtype, for inter: ''near'', ''bilin'', for metamorphosis: ''all''&
#ifdef HAVE_LIBSHP_FORTRAN
 &, for ''polyinter'': ''average''&
#endif
&')
options(12) = op_option_new('n', 'network', network, 'generic', help= &
 'string identifying network for output data')

! options for defining output
options(20) = op_option_new('f', 'output-format', output_format, &
#ifdef HAVE_DBALLE
'BUFR', &
#else
'native', &
#endif 
& help='format of output file, ''native'' for vol7d native binary format&
#ifdef HAVE_DBALLE
 &, ''BUFR'' for BUFR with generic template, ''CREX'' for CREX format&
#endif
 &')
#ifdef HAVE_DBALLE
options(21) = op_option_new('t', 'output-template', output_template, 'generic', help= &
 'output template for BUFR/CREX, in the form ''category.subcategory.localcategory'',&
& or an alias like ''synop'', ''metar'', ''temp'', ''generic''')
#endif
options(22) = op_option_new(' ', 'output-td', output_td, 1, help= &
 'time definition for output vol7d volume, 0 for reference time (more suitable for &
 &presenting forecast data) and 1 for verification time (more suitable for &
 &comparing forecasts with observations)')

! help options
options(38) = op_option_new('d', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
options(39) = op_option_help_new('h', 'help', help= &
 'show an help message and exit')
options(40) = op_option_new(' ', 'version', version, help= &
 'show version and exit')

! define the option parser
opt = optionparser_new(options, description_msg= &
 'Grib to sparse points transformation application. It reads grib edition 1 and 2, &
 &interpolates data over specified points and exports data into a native v7d file&
#ifdef HAVE_DBALLE
 &, or into a BUFR/CREX file&
#endif
 &.', usage_msg='vg6d_getpoint [options] inputfile outputfile')

! parse options and check for errors
optind = optionparser_parseoptions(opt)
IF (optind <= 0) THEN
  CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters')
  CALL EXIT(1)
ENDIF

IF (version) THEN
  WRITE(*,'(A,1X,A)')'vg6d_getpoint',VERSION
  CALL exit(0)
ENDIF

if (optind <= iargc()) then
  call getarg(optind, input_file)
  optind=optind+1
else
  call l4f_category_log(category,L4F_ERROR,'input file missing')
  call optionparser_printhelp(opt)
  call exit(1)
end if

if (optind <= iargc()) then
  call getarg(optind,output_file)
  optind=optind+1
else
  call l4f_category_log(category,L4F_ERROR,'output file missing')
  call optionparser_printhelp(opt)
  call exit(1)
end if

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(input_file))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(output_file))

IF (c_e(coord_file)) THEN
  IF (coord_format == 'native') THEN
    CALL import(v7d_coord, filename=coord_file)

#ifdef HAVE_DBALLE
  ELSE IF (coord_format == 'BUFR' .OR. coord_format == 'CREX') THEN
    CALL init(v7d_ana, filename=coord_file, format=coord_format, file=.TRUE., &
     write=.FALSE., categoryappend="anagrafica")
    CALL import(v7d_ana, anaonly=.TRUE.)
    v7d_coord = v7d_ana%vol7d
! destroy v7d_ana without deallocating the contents passed to v7d
    CALL init(v7d_ana%vol7d)
    CALL delete(v7d_ana)
#endif

#ifdef HAVE_LIBSHP_FORTRAN
  ELSE IF (coord_format == 'shp') THEN
    NULLIFY(poly)
    CALL import(poly, shpfile=coord_file)
    IF (.NOT.ASSOCIATED(poly)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error importing shapefile '//TRIM(coord_file))
      CALL EXIT(1)
    ENDIF

#endif
  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(coord_format)//' in --coord-format not valid or not supported.')
    CALL EXIT(1)
  ENDIF
ELSE

  CALL init(v7d_coord)
  IF (trans_type == 'inter') THEN ! set coordinates for interpolation
    CALL vol7d_alloc(v7d_coord, nana=1)
    CALL vol7d_alloc_vol(v7d_coord)
    CALL init(v7d_coord%ana(1), lat=lat, lon=lon)
  ENDIF

ENDIF

IF (ldisplay) CALL display(v7d_coord)

!trasformation object
CALL init(trans, trans_type=trans_type, sub_type=sub_type, &
 categoryappend="transformation", time_definition=output_td)
call import(volgrid, filename=input_file, categoryappend="volume letto")

IF (ldisplay) call display(volgrid)

call transform(trans, volgrid6d_in=volgrid, vol7d_out=v7d_out, v7d=v7d_coord, &
 poly=poly, networkname=network, categoryappend="transform")

call l4f_category_log(category,L4F_INFO,"transformation done")
if (associated(volgrid)) call delete(volgrid)

! output
IF (output_format == 'native') THEN
  IF (output_file == '-') THEN ! stdout_unit does not work with unformatted
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  iun = getunit()
  OPEN(iun, file=output_file, form='UNFORMATTED', access=stream_if_possible)
  DO i = 2, SIZE(v7d_out)
    CALL vol7d_merge(v7d_out(1), v7d_out(i), sort=(i == SIZE(v7d_out)))
  ENDDO
  CALL export(v7d_out(1), unit=iun)
  CLOSE(iun)
  CALL delete(v7d_out(1))

#ifdef HAVE_DBALLE
ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX') THEN
  IF (output_file == '-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  CALL init(v7d_dba_out, filename=output_file, format=output_format, file=.TRUE., &
   write=.TRUE., wipe=.TRUE., categoryappend="export")
  DO i = 1, SIZE(v7d_out)
    v7d_dba_out%vol7d = v7d_out(i) 
    CALL export (v7d_dba_out, template=output_template)
    CALL delete(v7d_dba_out%vol7d)
  END DO
  CALL delete(v7d_dba_out)
#endif

ELSE IF (output_format /= '') THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, format '// &
   TRIM(output_format)//' in --output-format not valid or not supported.')
  CALL EXIT(1)
ENDIF

call l4f_category_log(category,L4F_INFO,"exported to "//trim(output_format))
call l4f_category_log(category,L4F_INFO,"end")
deallocate (v7d_out)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM vg6d_getpoint
