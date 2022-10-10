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
PROGRAM vg6d_getpoint_pkauf
#include "config.h"
use log4fortran
USE vol7d_class
USE volgrid6d_var_class
USE volgrid6d_class
USE grid_class
USE grid_transform_class
USE gridinfo_class
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif
USE grib_api_csv
USE optionparser_class
USE io_units
USE georef_coord_class

IMPLICIT NONE

TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
CHARACTER(len=12) :: coord_format, output_format
CHARACTER(len=512) :: a_name, coord_file, coord_file_grid, input_file, output_file
INTEGER :: category, ier, i, iun, iargc, hindex
CHARACTER(len=network_name_len) :: network
TYPE(volgrid6d),POINTER :: volgrid(:), volgrid_coord(:)
TYPE(transform_def) :: trans
TYPE(vol7d) :: v7d_coord
TYPE(vol7d) :: v7d_out
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_ana, v7d_dba_out
#endif
CHARACTER(len=80) :: output_template, trans_type, sub_type
INTEGER :: output_td
LOGICAL :: version, ldisplay
LOGICAL :: noconvert
REAL,POINTER :: fr_land(:,:), orography(:,:)
TYPE(vol7d_var) :: varbufr
PROCEDURE(basic_find_index),POINTER :: find_index

!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name,a_name_force="vg6d_getpoint")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(TRIM(a_name)//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Grib to sparse points transformation application. It reads grib edition 1 and 2, &
 &interpolates data over specified points using the &
 &"optimally nearest point" algorithm (by P. Kaufmann) &
 &and exports data into a native v7d file'&
#ifdef HAVE_DBALLE
 //', or into a BUFR/CREX/JSON file'&
#endif
 //'.', usage_msg='Usage: vg6d_getpoint_pkauf [options] inputfile outputfile')

! define command-line options
! options for transformation
trans_type = 'inter'
sub_type = 'near'
!CALL optionparser_add(opt, 'v', 'trans-type', trans_type, 'inter', help= &
! 'transformation type, ''inter'' for interpolation, &
! &''stencilinter'' for interpolation using a stencil at each point' &
!#ifdef HAVE_SHAPELIB
! //', ''polyinter'' for statistical processing within given polygons' &
!#endif
! //', ''maskinter'' for statistical processing within subareas defined by &
! &a mask of gridpoints, &
! &''metamorphosis'' for keeping the same data but changing the container &
! &from grid to sparse points, ')
!CALL optionparser_add(opt, 'z', 'sub-type', sub_type, 'bilin', help= &
! 'transformation subtype, for ''inter'': ''near'', ''bilin'', &
! &for ''stencilinter'', ' &
!#ifdef HAVE_SHAPELIB
! //'''polyinter'', '&
!#endif
! //'''maskinter'': ''average'', ''stddev'', ''max'', ''min'', ''percentile'', &
! &for ''metamorphosis'': ''all'', ''coordbb''')

CALL optionparser_add(opt, 'c', 'coord-file', coord_file, help= &
 'file with coordinates of interpolation points, &
 &it must contain also station height information')
coord_file=cmiss
CALL optionparser_add(opt, ' ', 'coord-format', coord_format, &
#ifdef HAVE_DBALLE
'BUFR', &
#else
'native', &
#endif 
& help='format of input file with coordinates, ''native'' for vol7d native binary file '&
#ifdef HAVE_DBALLE
 //', ''BUFR'' for BUFR file, ''CREX'' for CREX file, ''JSON'' for json file (sparse points)'&
#endif
 )

CALL optionparser_add(opt, ' ', 'coord-file-grid', coord_file_grid, help= &
 'file with orography and land-sea mask of input gridded data, &
 &it must be on the same grid and format as input data')
coord_file_grid=cmiss

! options for defining output
CALL optionparser_add(opt, 'f', 'output-format', output_format, &
#ifdef HAVE_DBALLE
'BUFR', &
#else
'native', &
#endif 
& help='format of output file, ''native'' for vol7d native binary format'&
#ifdef HAVE_DBALLE
 //', ''BUFR'', ''CREX'' and ''JSON'' for corresponding formats, with generic template'&
#endif
 //', ''grib_api_csv'' for an ASCII csv file with grib_api keys as columns'&
 )
#ifdef HAVE_DBALLE
CALL optionparser_add(opt, 't', 'output-template', output_template, 'generic', help= &
 'output template for BUFR/CREX, as alias like ''synop'', ''metar'', ''temp'', ''generic''')
#endif
CALL optionparser_add(opt, ' ', 'output-td', output_td, 1, help= &
 'time definition for output vol7d volume, 0 for reference time (more suitable for &
 &presenting forecast data) and 1 for verification time (more suitable for &
 &comparing forecasts with observations)')

CALL grib_api_csv_add_options(opt) ! add options specific to grib_api_csv output

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
  CALL l4f_category_log(category,L4F_FATAL,'in command-line arguments')
  CALL raise_fatal_error()
ENDIF
IF (version) THEN
  WRITE(*,'(A,1X,A)')'vg6d_getpoint_pkauf',VERSION
  CALL exit(0)
ENDIF

IF (.NOT.c_e(coord_file)) THEN
  CALL l4f_category_log(category,L4F_FATAL, &
   'coord-file argument is compulsory')
  CALL raise_fatal_error()
ENDIF

IF (.NOT.c_e(coord_file_grid)) THEN
  CALL l4f_category_log(category,L4F_FATAL, &
   'coord-file-grid argument is compulsory')
  CALL raise_fatal_error()
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
ENDIF

IF (optind <= iargc()) THEN
  CALL getarg(optind, output_file)
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

CALL delete(opt)

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(input_file))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(output_file))

CALL init(v7d_coord)

IF (c_e(coord_file)) THEN
  CALL l4f_category_log(category,L4F_DEBUG,'start import coord')
  IF (coord_format == 'native') THEN
    CALL l4f_category_log(category,L4F_DEBUG,'execute import coord native')
    CALL import(v7d_coord, filename=coord_file)

#ifdef HAVE_DBALLE
  ELSE IF (coord_format == 'BUFR' .OR. coord_format == 'CREX' .OR. coord_format == 'JSON') THEN
    CALL l4f_category_log(category,L4F_DEBUG,'execute import coord v7d')
    CALL init(v7d_ana, filename=coord_file, format=coord_format, file=.TRUE., &
     write=.FALSE., categoryappend="anagrafica")
    CALL import(v7d_ana, anaonly=.TRUE.)
    CALL vol7d_convr(v7d_ana%vol7d, v7d_coord, anaconv=.TRUE.)
!    v7d_coord = v7d_ana%vol7d
! destroy v7d_ana without deallocating the contents passed to v7d
!    CALL init(v7d_ana%vol7d)
    CALL delete(v7d_ana)
#endif

  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line arguments, format '// &
     TRIM(coord_format)//' in --coord-format not valid or not supported.')
    CALL raise_fatal_error()
  ENDIF

  CALL l4f_category_log(category,L4F_DEBUG,'end import coord')
ENDIF

IF (.NOT.c_e(v7d_coord)) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error importing coord-file '//TRIM(coord_file))
  CALL raise_fatal_error()
ENDIF

IF (ldisplay) CALL display(v7d_coord)

! look for desired variables
hindex = -1
IF (ASSOCIATED(v7d_coord%anavar%r)) THEN
  hindex = firsttrue(v7d_coord%anavar%r(:)%btable == 'B07030')
  IF (hindex < 1) hindex = firsttrue(v7d_coord%anavar%r(:)%btable == 'B07031')
  IF (hindex < 1) hindex = firsttrue(v7d_coord%anavar%r(:)%btable == 'B07002')
  IF (hindex < 1) hindex = firsttrue(v7d_coord%anavar%r(:)%btable == 'B07007')
ENDIF

IF (hindex < 1) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error station height not found in file '//TRIM(coord_file))
  CALL raise_fatal_error()
ENDIF

CALL IMPORT(volgrid_coord, filename=coord_file_grid, decode=.TRUE., &
 categoryappend="coord_file_grid")
IF (.NOT.ASSOCIATED(volgrid_coord)) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error importing coord-file-grid volume from file '//TRIM(coord_file_grid))
  CALL raise_fatal_error()
ENDIF
! check volume
IF (SIZE(volgrid_coord) > 1 .OR. SIZE(volgrid_coord(1)%level) /= 1 .OR. &
 SIZE(volgrid_coord(1)%time) /= 1 .OR. SIZE(volgrid_coord(1)%timerange) /= 1) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error coord-file-grid must have strictly 1 grid, 1 level, 1 time and 1 timerange')
  CALL raise_fatal_error()
ENDIF

IF (ldisplay) CALL display(volgrid_coord)

! look for desired variables
fr_land => NULL()
orography => NULL()
DO i = 1, SIZE(volgrid_coord(1)%var)
  varbufr = convert(volgrid_coord(1)%var(i))
  IF (varbufr%btable == 'B29192') THEN
    fr_land => volgrid_coord(1)%voldati(:,:,1,1,1,i)
  ENDIF
  IF (varbufr%btable == 'B10007') THEN
    orography => volgrid_coord(1)%voldati(:,:,1,1,1,i)
  ENDIF
ENDDO

IF (.NOT.ASSOCIATED(fr_land)) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error fraction of land not found in file '//TRIM(coord_file_grid))
  CALL raise_fatal_error()
ENDIF
IF (.NOT.ASSOCIATED(orography)) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error orography not found in file '//TRIM(coord_file_grid))
  CALL raise_fatal_error()
ENDIF

IF (output_format == 'grib_api_csv') THEN
  output_td = 0
  CALL l4f_category_log(category,L4F_INFO, &
   "setting output time definition to 0 for grib_api_csv output")
ENDIF

! trasformation object
CALL init(trans, trans_type=trans_type, sub_type=sub_type, &
 categoryappend="transformation", time_definition=output_td)

! import input volume
CALL import(volgrid, filename=input_file, decode=.FALSE., categoryappend="input volume")
IF (.NOT.ASSOCIATED(volgrid)) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error importing input volume from file '//TRIM(input_file))
  CALL raise_fatal_error()
ENDIF
! check grid
IF (volgrid_coord(1)%griddim%dim%nx /= volgrid(1)%griddim%dim%nx .OR. &
 volgrid_coord(1)%griddim%dim%ny /= volgrid(1)%griddim%dim%ny) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error, coord-file-grid and input volume must have the same grid: '// &
   t2c(volgrid_coord(1)%griddim%dim%nx)//','//t2c(volgrid_coord(1)%griddim%dim%ny)//':'// &
   t2c(volgrid(1)%griddim%dim%nx)//','//t2c(volgrid(1)%griddim%dim%ny))
  CALL raise_fatal_error()
ENDIF

IF (ldisplay) CALL display(volgrid)

find_index => find_index_pkaufmann
IF (output_format /= 'grib_api_csv') THEN ! otherwise postpone
  CALL transform(trans, volgrid6d_in=volgrid, vol7d_out=v7d_out, v7d=v7d_coord, &
   networkname=network, noconvert=noconvert, find_index=find_index, &
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
  OPEN(iun, file=output_file, form='UNFORMATTED', access='STREAM')
  IF (ldisplay) CALL display(v7d_out)

  CALL export(v7d_out, unit=iun)
  CLOSE(iun)
  CALL delete(v7d_out)

#ifdef HAVE_DBALLE
ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX' .OR. output_format == 'JSON') THEN
  IF (output_file == '-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  CALL init(v7d_dba_out, filename=output_file, format=output_format, file=.TRUE., &
   write=.TRUE., wipe=.TRUE., categoryappend="export", template=output_template)
  IF (ldisplay) CALL display(v7d_out)
  v7d_dba_out%vol7d = v7d_out
  CALL export (v7d_dba_out)
  CALL delete(v7d_dba_out)
#endif
ELSE IF (output_format == 'grib_api_csv') THEN
  IF (output_file == '-') THEN
    iun = stdout_unit
  ELSE
    iun = getunit()
    OPEN(iun, file=output_file, form='FORMATTED', access='SEQUENTIAL')
  ENDIF

  DO i = 1, SIZE(volgrid) ! transform one volume at a time
    CALL transform(trans, volgrid6d_in=volgrid(i), vol7d_out=v7d_out, v7d=v7d_coord, &
     networkname=network, noconvert=noconvert, find_index=find_index, &
     categoryappend="transform")
    CALL grib_api_csv_export(v7d_out, volgrid(i), iun, i == 1)
  ENDDO
  IF (output_file /= '-') CLOSE(iun)

ELSE IF (output_format /= '') THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line arguments, format '// &
   TRIM(output_format)//' in --output-format not valid or not supported.')
  CALL EXIT(1)
ENDIF

IF (ASSOCIATED(volgrid)) CALL delete(volgrid)
CALL l4f_category_log(category,L4F_INFO,"exported to "//TRIM(output_format))
CALL l4f_category_log(category,L4F_INFO,"end")

! Close the logger
CALL l4f_category_delete(category)
ier=l4f_fini()

CONTAINS

! Subroutine find_index modified for computing the nearest point
! according to the Pirmin Kaufmann distance method
! https://drive.google.com/a/arpae.it/file/d/0B098fbLuj7EzZWU2YkZOd25nWUZaV0pRLUwxeEg0VXJCZnlr
SUBROUTINE find_index_pkaufmann(this, near, nx, ny, xmin, xmax, ymin, ymax, &
 lon, lat, extrap, index_x, index_y)
TYPE(griddim_def),INTENT(in) :: this ! griddim object (from grid)
logical,INTENT(in) :: near ! near or bilin interpolation (determine which point is requested)
INTEGER,INTENT(in) :: nx, ny ! dimension of input grid
DOUBLE PRECISION,INTENT(in) :: xmin, xmax, ymin, ymax ! extreme coordinate of input grid
DOUBLE PRECISION,INTENT(in) :: lon(:,:),lat(:,:) ! target coordinates (2nd dim =1 here)
LOGICAL,INTENT(in) :: extrap ! extrapolate
INTEGER,INTENT(out) :: index_x(:,:),index_y(:,:) ! index of point requested, shape like lon, lat

INTEGER :: lnx, lny, ni, nj, i, j, xf, xl, yf, yl, ii, jj
DOUBLE PRECISION :: x(SIZE(lon,1),SIZE(lon,2)),y(SIZE(lon,1),SIZE(lon,2)), &
 gx(SIZE(lon,1),SIZE(lon,2)), gy(SIZE(lon,1),SIZE(lon,2)), hd, r, findist, tmpdist
DOUBLE PRECISION,PARAMETER :: &
 rl=1.415D0, & ! radius in grid point units for land points
 rw=2.0D0, & ! radius in grid point units for sea points
 fve=500.D0 ! vertical emphasis factor
LOGICAL :: allsea
TYPE(griddim_def) :: lgrid

! explicit sizes
ni = SIZE(lon, 1)
nj = SIZE(lon, 2)
CALL proj(this, lon, lat, x, y)
CALL copy(this, lgrid)
CALL unproj(lgrid) ! necessary? yes!
! real grid coordinates
gx = (x-xmin)/((xmax-xmin)/DBLE(nx-1)) + 1
gy = (y-ymin)/((ymax-ymin)/DBLE(ny-1)) + 1
! integer grid coordinates (nearest point)
index_x = NINT((x-xmin)/((xmax-xmin)/DBLE(nx-1)))+1
index_y = NINT((y-ymin)/((ymax-ymin)/DBLE(ny-1)))+1
! avoid extrapolation
WHERE(index_x < 1 .OR. index_x > nx .OR. index_y < 1 .OR. index_y > ny)
  index_x = imiss
  index_y = imiss
END WHERE

DO j = 1, nj
  DO i = 1, ni
    IF (c_e(index_x(i,j))) THEN ! point is inside domain
! integer limits of maximum rectangular search area
      xf = MAX(1, NINT(gx(i,j)-1.5001D0))
      xl = MIN(nx, NINT(gx(i,j)+1.5001D0))
      yf = MAX(1, NINT(gy(i,j)-1.5001D0))
      yl = MIN(ny, NINT(gy(i,j)+1.5001D0))
      IF (fr_land(index_x(i,j),index_y(i,j)) > 0.5) THEN ! land
        r = rl
      ELSE ! water
        r = rw
      ENDIF

! here we could optimize defining a local mask of points
! if all points are of sea type the algorithm is different
      allsea = .TRUE.
      allsea_loop: DO jj = yf, yl
        DO ii = xf, xl
          hd = SQRT((DBLE(ii)-gx(i,j))**2+(DBLE(jj)-gy(i,j))**2)
          IF (hd <= r) THEN ! within means < or <= ?
            IF (fr_land(ii,jj) > 0.5) THEN
              allsea = .FALSE.
              EXIT allsea_loop
            ENDIF
          ENDIF
        ENDDO
      ENDDO allsea_loop

      findist = dmiss
      DO jj = yf, yl
        DO ii = xf, xl
          hd = SQRT((DBLE(ii)-gx(i,j))**2+(DBLE(jj)-gy(i,j))**2)
          IF (hd <= r) THEN ! within means < or <= ?
            IF (fr_land(ii,jj) > 0.5 .OR. allsea) THEN
              tmpdist = dist(georef_coord_new(lon(i,j), lat(i,j)), &
               georef_coord_new(lgrid%dim%lon(ii,jj), lgrid%dim%lat(ii,jj))) + &
               ABS(v7d_coord%volanar(i,hindex,1) - orography(ii,jj))*fve
              IF (.NOT.c_e(findist) .OR. tmpdist < findist) THEN ! up to now best point
                index_x(i,j) = ii
                index_y(i,j) = jj
                findist = tmpdist
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDIF

  ENDDO
ENDDO
END SUBROUTINE find_index_pkaufmann

END PROGRAM vg6d_getpoint_pkauf
