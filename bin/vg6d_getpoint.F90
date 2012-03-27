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
USE georef_coord_class

implicit none

TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
CHARACTER(len=12) :: coord_format, output_format
CHARACTER(len=512) :: a_name, coord_file, input_file, output_file
INTEGER :: category, ier, i, iun, iargc
CHARACTER(len=network_name_len) :: network
TYPE(volgrid6d),POINTER :: volgrid(:)
TYPE(arrayof_gridinfo) :: maskgrid
REAL,ALLOCATABLE :: maskfield(:,:)
TYPE(transform_def) :: trans
TYPE(vol7d) :: v7d_coord
TYPE(vol7d) :: v7d_out
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_ana, v7d_dba_out
#endif
TYPE(arrayof_georef_coord_array) :: poly
DOUBLE PRECISION :: lon, lat
DOUBLE PRECISION,ALLOCATABLE :: lon_array(:), lat_array(:)
INTEGER :: polytopo
DOUBLE PRECISION :: ilon, ilat, flon, flat, radius
character(len=80) :: output_template,trans_type,sub_type
INTEGER :: output_td
LOGICAL :: version, ldisplay
logical :: c2agrid, noconvert

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
CALL optionparser_add(opt, 'a', 'lon', lon, 10.D0, help= &
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
 //', ''BUFR'' for BUFR file, ''CREX'' for CREX file (sparse points)'&
#endif
#ifdef HAVE_SHAPELIB
 //', ''shp'' for shapefile (sparse points or polygons)'&
#endif
 )
CALL optionparser_add(opt, 'v', 'trans-type', trans_type, 'inter', help= &
 'transformation type, ''inter'' for interpolation, ''metamorphosis'' &
 &for keeping the same data but changing the container from grib to v7d'&
#ifdef HAVE_SHAPELIB
 //', ''polyinter'' for statistical processing within given polygons'&
#endif
 )
CALL optionparser_add(opt, 'z', 'sub-type', sub_type, 'bilin', help= &
 'transformation subtype, for inter: ''near'', ''bilin'',&
 & for metamorphosis: ''all'', ''coordbb'''&
#ifdef HAVE_SHAPELIB
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
radius = dmiss
CALL optionparser_add(opt, ' ', 'radius', radius, help= &
 'radius of stencil in gridpoint units, fractionary values accepted, &
 &for ''stencilinter'' interpolation')

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

CALL optionparser_add(opt, 'i', 'noconvert', noconvert, help= &
 'do not convert values fron grib definition to standard vol7d model data, &
 &this option sets vol7d variables to missing so it is not possible to export &
 &to some formats, useful with --output-format=grib_api_csv option.')

! help options
CALL optionparser_add(opt, 'g', 'display', ldisplay, help= &
 'briefly display the data volume imported and exported, &
 &warning: this option is incompatible with output on stdout.')
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0) ! generate a clean manpage
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_FATAL,'in command-line parameters')
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
  call l4f_category_log(category,L4F_FATAL,'input file missing')
  call raise_fatal_error()
end if

if (optind <= iargc()) then
  call getarg(optind,output_file)
  optind=optind+1
else
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_FATAL,'output file missing')
  call raise_fatal_error()
end if

if (optind <= iargc()) then
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_FATAL, &
   'extra arguments after input and output file names')
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

#ifdef HAVE_LIBGRIBAPI
  ELSE IF (coord_format == 'grib_api') THEN
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

#endif

  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(coord_format)//' in --coord-format not valid or not supported.')
    CALL EXIT(1)
  ENDIF
ENDIF

IF (.NOT.c_e(v7d_coord)) THEN ! fallback, initialise v7d_coord from single point
  CALL vol7d_alloc(v7d_coord, nana=1)
  CALL vol7d_alloc_vol(v7d_coord)
  CALL init(v7d_coord%ana(1), lat=lat, lon=lon)
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
 ilon=ilon, ilat=ilat, flon=flon, flat=flat, poly=poly, radius=radius, &
 categoryappend="transformation", time_definition=output_td)
CALL import(volgrid, filename=input_file, decode=.FALSE., categoryappend="input volume")

IF (ldisplay) CALL display(volgrid)

IF (c2agrid) CALL vg6d_c2a(volgrid)

IF (output_format /= 'grib_api_csv') THEN ! otherwise postpone
  CALL transform(trans, volgrid6d_in=volgrid, vol7d_out=v7d_out, v7d=v7d_coord, &
   maskgrid=maskfield, networkname=network, noconvert=noconvert, &
   categoryappend="transform")
  CALL l4f_category_log(category,L4F_INFO,"transformation completed")
ENDIF

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

  DO i = 1, SIZE(volgrid) ! transform one volume at a time
    CALL transform(trans, volgrid6d_in=volgrid(i), vol7d_out=v7d_out, v7d=v7d_coord, &
     maskgrid=maskfield, networkname=network, noconvert=noconvert, &
     categoryappend="transform")
    CALL grib_api_csv_export(v7d_out, volgrid(i), iun, i == 1)
  ENDDO
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
