PROGRAM vg6d_getpoint
#include "config.h"
use log4fortran
use volgrid6d_class
use grid_class
use grid_transform_class
USE vol7d_dballe_class
USE vol7d_class
use getopt_m
USE io_units

implicit none

TYPE(op_option) :: options(40) ! remember to update dimension when adding options
TYPE(optionparser) :: opt
CHARACTER(len=8) :: ana_format, output_format
CHARACTER(len=512) :: input_file, output_file, network_list, variable_list
CHARACTER(len=512) :: a_name, ana_file=cmiss
INTEGER :: category, ier, i, iun
character(len=network_name_len) :: network
type(volgrid6d),pointer :: volgrid(:)
type(transform_def) :: trans
type(vol7d) :: v7d
type(vol7d_ana) :: ana
type(vol7d),pointer :: v7d_out(:)
TYPE(vol7d_dballe) :: v7d_ana, v7d_dba_out
doubleprecision :: lon, lat
character(len=80) :: output_template,trans_type,sub_type
INTEGER :: output_td

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
 'longitude of single interpolation point, alternative to --ana-file')
options(2) = op_option_new('b', 'lat', lat, 45.D0, help= &
 'latitude of single interpolation point, alternative to --ana-file')
options(3) = op_option_new('c', 'ana-file', ana_file, help= &
 'file with coordinates of points to interpolate, alternative to --ana-file')
options(4) = op_option_new(' ', 'ana-format', ana_format, &
#ifdef HAVE_DBALLE
'BUFR', &
#else
'native', &
#endif 
& help='format of input file with coordinates, ''native'' for vol7d native binary file &
#ifdef HAVE_DBALLE
 &, ''BUFR'' for BUFR file, ''CREX'' for CREX file&
#endif 
 &')
options(10) = op_option_new('v', 'trans-type', trans_type, 'inter', help= &
 'transformation type, ''inter'' for interpolation is the only one supprted')
options(11) = op_option_new('z', 'sub-type', sub_type, 'bilin', help= &
 'transformation subtype, for inter: ''near'', ''bilin''')
options(12) = op_option_new('n', 'network', network, 'generic', help= &
 'string identifying network for output data')
! options for output
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
 'output template for BUFR/CREX, in the form ''category.subcategory.localcategory'', or &
& an alias like ''synop'', ''metar'',''temp'',''generic''')
#endif
options(22) = op_option_new(' ', 'output-td', output_td, 1, help= &
 'time definition for output vol7d volume, 0 for reference time (more suitable for &
 &presenting forecast data) and 1 for verification time (more suitable for &
 &comparing forecasts with observations)')
! help option
options(40) = op_option_help_new('h', 'help', help= &
 'show an help message')

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

IF (c_e(ana_file)) THEN
  IF (ana_format == 'native') THEN
    iun = getunit()
    OPEN(iun, file=ana_file, form='UNFORMATTED', access='SEQUENTIAL')
    CALL init(v7d, time_definition=0)
    CALL import(v7d, unit=iun)
    CLOSE(iun)

#ifdef HAVE_DBALLE
  ELSE IF (ana_format == 'BUFR' .OR. ana_format == 'CREX') THEN
    CALL init(v7d_ana, filename=ana_file, format=ana_format, file=.TRUE., &
     write=.FALSE., categoryappend="anagrafica")
    CALL import(v7d_ana, anaonly=.TRUE.)
    CALL vol7d_copy(v7d_ana%vol7d, v7d)
!    CALL init(v7d)
!    CALL vol7d_alloc(v7d,nana=SIZE(v7d_ana%vol7d%ana))
!    CALL vol7d_alloc_vol(v7d)
!    v7d%ana=v7d_ana%vol7d%ana
    CALL delete(v7d_ana)

#endif
  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(ana_format)//' in --ana-format not valid or not supported.')
    CALL EXIT(1)
  ENDIF
ELSE

  call init(v7d)
  call vol7d_alloc(v7d,nana=1)
  call vol7d_alloc_vol(v7d)
  call init(ana,lat=lat,lon=lon)
  v7d%ana(1)=ana

ENDIF

CALL display(v7d)

!trasformation object
CALL init(trans, trans_type=trans_type, sub_type=sub_type, &
 categoryappend="transformation", time_definition=output_td)
call import(volgrid, filename=input_file, categoryappend="volume letto")

call display(volgrid)

call transform(trans,v7d, volgrid6d_in=volgrid, vol7d_out=v7d_out, &
 networkname=network, categoryappend="transform")

call l4f_category_log(category,L4F_INFO,"transformation done")
if (associated(volgrid)) call delete(volgrid)

! output
IF (output_format == 'native') THEN
  IF (output_file == '-') THEN
    iun = stdout_unit
  ELSE
    iun = getunit()
    OPEN(iun, file=output_file, form='UNFORMATTED', access='SEQUENTIAL')
  ENDIF
  DO i = 2, SIZE(v7d_out)
    CALL vol7d_merge(v7d_out(1), v7d_out(i), sort=(i == SIZE(v7d_out)))
  ENDDO
  CALL export(v7d_out(1), unit=iun)
  IF (output_file /= '-') CLOSE(iun)
  CALL delete(v7d_out(1))

#ifdef HAVE_DBALLE
ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX') THEN
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
