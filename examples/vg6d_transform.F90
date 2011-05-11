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
#ifdef VAPOR
use volgrid6d_vapor_class
#endif
use grid_class
use grid_transform_class
use grid_id_class
use err_handling
use char_utilities
use optionparser_class

implicit none

integer :: category,ier,i,nana
CHARACTER(len=512):: a_name, infile, outfile, output_format, output_template
type (volgrid6d),pointer  :: volgrid(:),volgrid_out(:)

doubleprecision ::  ilon,ilat,flon,flat

type(griddim_def) :: griddim_out
type(transform_def) :: trans

integer :: nx,ny,component_flag,npx,npy
doubleprecision :: xmin, xmax, ymin, ymax
INTEGER :: ix, iy, fx, fy, time_definition
doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation
character(len=80) :: proj_type,trans_type,sub_type

doubleprecision ::x,y,lon,lat
logical :: c2agrid, decode
type(optionparser) :: opt
INTEGER :: optind, optstatus
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
CALL optionparser_add(opt, 'v', 'trans-type', trans_type, 'inter', help= &
 'transformation type: ''inter'' for interpolation, ''boxinter'' for &
 &statistical interpolation on boxes, ''zoom'' for zooming, &
 &''boxregrid'' for resolution reduction, ''metamorphosis'' for change in format only')
CALL optionparser_add(opt, 'z', 'sub-type', sub_type, 'near', help= &
 'transformation subtype, for inter: ''near'', ''bilin'', &
 &for ''boxinter'' and ''boxregrid'': ''average'', ''max'', ''min'', &
 &for zoom: ''index'', ''coord'', ''coordbb''')

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

CALL optionparser_add(opt, 'f', 'npx', npx, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')
CALL optionparser_add(opt, 'g', 'npy', npy, 4, help= &
 'number of nodes along x axis on input grid, over which to apply function for boxregrid')

output_template = ''
CALL optionparser_add(opt, ' ', 'output-format', output_format, &
#ifdef HAVE_LIBGRIBAPI
'grib_api', &
#else
'', &
#endif
help='format of output file, in the form ''name[:template]''&
#ifdef HAVE_LIBGRIBAPI
 &; ''grib_api'' for gridded output in grib format, template is the &
 &path name of a grib file in which the first message defines the output grid and &
 &is used as a template for the output grib messages&
#endif
#ifdef VAPOR
 &; ''vapor'' for gridded output in vdf format&
#endif
 &; if this option includes a template, --type &
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



CALL optionparser_add(opt, ' ', 'time-definition', time_definition, 0, help= &
 'time definition for inport volume, 0 for reference time (more suitable for &
 &presenting forecast data) and 1 for verification time (more suitable for &
 &comparing forecasts with observations)')


                                ! display option
CALL optionparser_add(opt, ' ', 'display', ldisplay, help= &
 'briefly display the data volume imported and exported, warning: this option is incompatible &
 &with output on stdout.')
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
  call getarg(optind, infile)
  optind=optind+1
else
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_ERROR,'input file missing')
  call raise_fatal_error()

  call exit(1)
end if

if ( optind <= iargc()) then
  call getarg(optind, outfile)
  optind=optind+1
else
  call optionparser_printhelp(opt)
  call l4f_category_log(category,L4F_ERROR,'output file missing')
  call raise_fatal_error()
end if

CALL delete(opt)

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(infile))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(outfile))

IF (trans_type == 'inter' .OR. trans_type == 'boxinter') THEN ! griddim_out needed

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

ENDIF

IF (trans_type == 'metamorphosis') THEN 
  decode=.true.
else
  decode=.false.
endif

CALL import(volgrid,filename=infile,decode=decode, time_definition=time_definition, categoryappend="input")
if (c2agrid) call vg6d_c2a(volgrid)
IF (ldisplay) THEN
  IF (ASSOCIATED(volgrid)) THEN
    DO i = 1,SIZE(volgrid)
      PRINT*,'input grid >>>>>>>>>>>>>>>>>>>>'
      CALL display(volgrid(i))
    ENDDO
  ENDIF
ENDIF


IF (trans_type == 'metamorphosis') THEN  ! export with no operation

  call write_to_file_out(volgrid)

  if (associated(volgrid)) call delete (volgrid)

else

                                ! transformation object
  CALL init(trans, trans_type=trans_type, sub_type=sub_type, &
   ix=ix, iy=iy, fx=fx, fy=fy, &
   ilon=ilon, ilat=ilat, flon=flon, flat=flat, npx=npx, npy=npy, &
   percentile=0.5D0, &
   categoryappend="transformation")

  IF (trans_type == 'inter' .OR. trans_type == 'boxinter') THEN
    CALL transform(trans,griddim_out,volgrid6d_in=volgrid, &
     volgrid6d_out=volgrid_out,clone=.TRUE.,categoryappend="transformed")

    IF (ldisplay) THEN ! done here in order to print final ellipsoid
      DO i = 1,SIZE(volgrid)
        PRINT*,'output grid >>>>>>>>>>>>>>>>>>>>'
        IF (ASSOCIATED(volgrid_out)) CALL display(volgrid_out(1))
      END DO
    ENDIF

  ELSE
    CALL transform(trans,volgrid6d_in=volgrid, volgrid6d_out=volgrid_out, &
     clone=.TRUE.,categoryappend="transformed")

  ENDIF

  CALL l4f_category_log(category,L4F_INFO,"transformation completed")
  IF (ASSOCIATED(volgrid)) CALL delete(volgrid)


  call write_to_file_out(volgrid_out)

  CALL delete (volgrid_out)

ENDIF

CALL l4f_category_log(category,L4F_INFO,"end")


                                !chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

contains


subroutine write_to_file_out(myvolgrid)

type (volgrid6d),pointer  :: myvolgrid(:)

i = word_split(outfile, w_s, w_e, ':')

IF (i == 3) THEN ! template requested (grib_api:template_file:output_file)
  file_template = grid_file_id_new(outfile(w_s(1):w_e(2)), 'r')
  gaid_template = grid_id_new(file_template)
  IF (c_e(gaid_template)) THEN
    CALL export (myvolgrid, filename=outfile(w_s(1):w_e(1))//':'// &
     outfile(w_s(3):w_e(3)), gaid_template=gaid_template, &
     categoryappend="export_tmpl")
  ELSE
    CALL l4f_category_log(category,L4F_FATAL, &
     "opening output template "//TRIM(outfile))
    CALL raise_fatal_error()
  ENDIF

ELSE

  if (output_format == "grib_api") then 
#ifdef HAVE_LIBGRIBAPI
    CALL export(myvolgrid,filename=outfile,categoryappend="exporttofilegrib")
#else
    CALL l4f_category_log(category,L4F_FATAL, &
     "export to grib_api disabled at compile time")
    CALL raise_fatal_error()
#endif

  else if (output_format == "vapor") then 

#ifdef VAPOR
    do i =1,size(myvolgrid)
      CALL l4f_category_log(category,L4F_INFO, &
       "exporting to vapor vdf file: "//trim(outfile)//"_"//t2c(i)//".vdf")
      call export (myvolgrid(i),normalize=.True.,&
       filename=trim(outfile)//"_"//t2c(i)//".vdf")
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
