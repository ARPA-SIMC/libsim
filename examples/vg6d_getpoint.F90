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
PROGRAM vg6d_getpoint
#include "config.h"
use log4fortran
USE vol7d_class
use volgrid6d_class
use grid_class
use grid_transform_class
use gridinfo_class
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif
#ifdef HAVE_LIBGRIBAPI
USE grib_api_csv
#endif
use optionparser_class
USE io_units
USE geo_coord_class

implicit none

TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
CHARACTER(len=12) :: coord_format, output_format
CHARACTER(len=512) :: a_name, coord_file, input_file, output_file, &
 network_list, variable_list
INTEGER :: category, ier, i, iun, iargc
CHARACTER(len=network_name_len) :: network
TYPE(volgrid6d),POINTER :: volgrid(:)
TYPE(gridinfo_def),POINTER :: maskgrid(:)
REAL,ALLOCATABLE :: maskfield(:,:)
TYPE(transform_def) :: trans
TYPE(vol7d) :: v7d_coord
TYPE(vol7d) :: v7d_out
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_ana, v7d_dba_out
#endif
TYPE(geo_coordvect),POINTER :: poly(:) => NULL()
doubleprecision :: lon, lat
doubleprecision :: ilon,ilat,flon,flat
character(len=80) :: output_template,trans_type,sub_type
INTEGER :: output_td
LOGICAL :: version, ldisplay
logical :: c2agrid

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="getpoint")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Grib to sparse points transformation application. It reads grib edition 1 and 2, &
 &interpolates data over specified points and exports data into a native v7d file'&
#ifdef HAVE_DBALLE
 //', or into a BUFR/CREX file'&
#endif
 //'.', usage_msg='Usage: vg6d_getpoint [options] inputfile outputfile')

! define command-line options
! options for transformation
CALL optionparser_add(opt, 'a', 'lon', lon, 0.D0, help= &
 'longitude of single interpolation point, alternative to --coord-file')
CALL optionparser_add(opt, 'b', 'lat', lat, 45.D0, help= &
 'latitude of single interpolation point, alternative to --coord-file')
CALL optionparser_add(opt, 'c', 'coord-file', coord_file, help= &
 'file with coordinates of interpolation points, alternative to --lon, --lat; &
 &no coordinate information is required for metamorphosis transformation')
coord_file=cmiss
CALL optionparser_add(opt, ' ', 'coord-format', coord_format, &
#ifdef HAVE_DBALLE
'BUFR', &
#else
'native', &
#endif 
& help='format of input file with coordinates, ''native'' for vol7d native binary file '&
#ifdef HAVE_DBALLE
 //', ''BUFR'' for BUFR file, ''CREX'' for CREX file'&
#endif
#ifdef HAVE_LIBSHP_FORTRAN
 //', ''shp'' for shapefile (interpolation on polygons)'&
#endif
 )
CALL optionparser_add(opt, 'v', 'trans-type', trans_type, 'inter', help= &
 'transformation type, ''inter'' for interpolation, ''metamorphosis'' &
 &for keeping the same data but changing the container from grib to v7d'&
#ifdef HAVE_LIBSHP_FORTRAN
 //', ''polyinter'' for statistical processing within given polygons'&
#endif
 )
CALL optionparser_add(opt, 'z', 'sub-type', sub_type, 'bilin', help= &
 'transformation subtype, for inter: ''near'', ''bilin'',&
 & for metamorphosis: ''all'', ''coordbb'''&
#ifdef HAVE_LIBSHP_FORTRAN
 //', for ''polyinter'': ''average'', ''stddev'', ''max'', ''min'''&
#endif
)

CALL optionparser_add(opt, 'a', 'ilon', ilon, 0.0D0, help= &
 'longitude of the southwestern bounding box corner')
CALL optionparser_add(opt, 'b', 'ilat', ilat, 30.D0, help= &
 'latitude of the southwestern bounding box corner')
CALL optionparser_add(opt, 'c', 'flon', flon, 30.D0, help= &
 'longitude of the northeastern bounding box corner')
CALL optionparser_add(opt, 'd', 'flat', flat, 60.D0, help= &
 'latitude of the northeastern bounding box corner')

CALL optionparser_add(opt, 'n', 'network', network, 'generic', help= &
 'string identifying network for output data')

CALL optionparser_add(opt, 'e', 'a-grid', c2agrid, help= &
 'interpolate U/V points of an Arakawa C grid on the corresponding T points &
 &of an Arakawa A grid')


! options for defining output
CALL optionparser_add(opt, 'f', 'output-format', output_format, &
#ifdef HAVE_DBALLE
'BUFR', &
#else
'native', &
#endif 
& help='format of output file, ''native'' for vol7d native binary format'&
#ifdef HAVE_DBALLE
 //', ''BUFR'' for BUFR with generic template, ''CREX'' for CREX format'&
#endif
#ifdef HAVE_LIBGRIBAPI
 //', ''grib_api_csv'' for an ASCII csv file with grib_api keys as columns'&
#endif
 )
#ifdef HAVE_DBALLE
CALL optionparser_add(opt, 't', 'output-template', output_template, 'generic', help= &
 'output template for BUFR/CREX, in the form ''category.subcategory.localcategory'',&
& or an alias like ''synop'', ''metar'', ''temp'', ''generic''')
#endif
CALL optionparser_add(opt, ' ', 'output-td', output_td, 1, help= &
 'time definition for output vol7d volume, 0 for reference time (more suitable for &
 &presenting forecast data) and 1 for verification time (more suitable for &
 &comparing forecasts with observations)')

#ifdef HAVE_LIBGRIBAPI
CALL grib_api_csv_add_options(opt) ! add options specific to grib_api_csv output
#endif

! help options
CALL optionparser_add(opt, 'd', 'display', ldisplay, help= &
 'briefly display the data volume imported and exported, &
 &warning: this option is incompatible with output on stdout.')
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
  WRITE(*,'(A,1X,A)')'vg6d_getpoint',VERSION
  CALL exit(0)
ENDIF

if (optind <= iargc()) then
  call getarg(optind, input_file)
  optind=optind+1
else
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_ERROR,'input file missing')
  call raise_fatal_error()
end if

if (optind <= iargc()) then
  call getarg(optind,output_file)
  optind=optind+1
else
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_ERROR,'output file missing')
  call raise_fatal_error()
end if

CALL delete(opt)

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(input_file))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(output_file))

CALL init(v7d_coord)
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
    CALL import(poly, shpfile=coord_file)
    IF (.NOT.ASSOCIATED(poly)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error importing shapefile '//TRIM(coord_file))
      CALL raise_fatal_error()
    ENDIF

#endif

#ifdef HAVE_LIBGRIBAPI
  ELSE IF (coord_format == 'grib_api') THEN
    CALL import(maskgrid, coord_file, categoryappend='maskgrid')
    IF (.NOT.ASSOCIATED(maskgrid)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error importing mask grid file '//TRIM(coord_file))
      CALL raise_fatal_error()
    ENDIF
    IF (SIZE(maskgrid) < 1) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error importing mask grid file '//TRIM(coord_file))
      CALL raise_fatal_error()
    ENDIF
    CALL import(maskgrid(1))
    ALLOCATE(maskfield(maskgrid(1)%griddim%dim%nx, maskgrid(1)%griddim%dim%ny))
    maskfield(:,:) = decode_gridinfo(maskgrid(1))
    CALL delete(maskgrid(1))
    DEALLOCATE(maskgrid)

#endif

  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(coord_format)//' in --coord-format not valid or not supported.')
    CALL EXIT(1)
  ENDIF
ELSE

  IF (trans_type == 'inter') THEN ! set coordinates for interpolation
    CALL vol7d_alloc(v7d_coord, nana=1)
    CALL vol7d_alloc_vol(v7d_coord)
    CALL init(v7d_coord%ana(1), lat=lat, lon=lon)
  ENDIF

ENDIF

IF (ldisplay) CALL display(v7d_coord)

#ifdef HAVE_LIBGRIBAPI
IF (output_format == 'grib_api_csv') THEN
  output_td = 0
  CALL l4f_category_log(category,L4F_INFO, &
   "setting output time definition to 0 for grib_api_csv output")
ENDIF
#endif

! trasformation object
CALL init(trans, trans_type=trans_type, sub_type=sub_type, &
 ilon=ilon, ilat=ilat, flon=flon, flat=flat, poly=poly, &
 categoryappend="transformation", time_definition=output_td)
CALL import(volgrid, filename=input_file, decode=.FALSE., categoryappend="input volume")

IF (ldisplay) CALL display(volgrid)

IF (c2agrid) CALL vg6d_c2a(volgrid)

CALL transform(trans, volgrid6d_in=volgrid, vol7d_out=v7d_out, v7d=v7d_coord, &
 maskgrid=maskfield, networkname=network, categoryappend="transform")

CALL l4f_category_log(category,L4F_INFO,"transformation completed")

! output
IF (output_format == 'native') THEN
  IF (output_file == '-') THEN ! stdout_unit does not work with unformatted
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  iun = getunit()
  OPEN(iun, file=output_file, form='UNFORMATTED', access=stream_if_possible)
  IF (ldisplay) CALL display(v7d_out)

  CALL export(v7d_out, unit=iun)
  CLOSE(iun)
  CALL delete(v7d_out)

#ifdef HAVE_DBALLE
ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX') THEN
  IF (output_file == '-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  CALL init(v7d_dba_out, filename=output_file, format=output_format, file=.TRUE., &
   write=.TRUE., wipe=.TRUE., categoryappend="export")
  IF (ldisplay) CALL display(v7d_out)
  v7d_dba_out%vol7d = v7d_out
  CALL export (v7d_dba_out, template=output_template)
  CALL delete(v7d_dba_out)
#endif
#ifdef HAVE_LIBGRIBAPI
ELSE IF (output_format == 'grib_api_csv') THEN
  IF (output_file == '-') THEN
    iun = stdout_unit
  ELSE
    iun = getunit()
    OPEN(iun, file=output_file, form='FORMATTED', access='SEQUENTIAL')
  ENDIF
! TODO handle volgrid array
  CALL grib_api_csv_export(v7d_out, volgrid(1), iun)
  IF (output_file /= '-') CLOSE(iun)

#endif

ELSE IF (output_format /= '') THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, format '// &
   TRIM(output_format)//' in --output-format not valid or not supported.')
  CALL EXIT(1)
ENDIF

IF (ASSOCIATED(volgrid)) CALL delete(volgrid)
call l4f_category_log(category,L4F_INFO,"exported to "//trim(output_format))
call l4f_category_log(category,L4F_INFO,"end")

! Close the logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM vg6d_getpoint
