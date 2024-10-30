PROGRAM create_projgrib
USE log4fortran
USE optionparser_class
USE grid_class
USE err_handling
USE missing_values
USE volgrid6d_class
USE grid_transform_class
IMPLICIT NONE

INTEGER :: category, ier
CHARACTER(len=512) :: a_name, input_file, output_file
TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
LOGICAL :: version

INTEGER :: nx, ny, component_flag, projection_center_flag, pole
DOUBLE PRECISION :: xmin, xmax, ymin, ymax, xoff, yoff
INTEGER :: ix, iy, fx, fy, time_definition, utm_zone
DOUBLE PRECISION :: latitude_south_pole, longitude_south_pole, angle_rotation
CHARACTER(len=80) :: proj_type

DOUBLE PRECISION :: lov, latin1, latin2, lad

TYPE(griddim_def) :: griddim_out
!TYPE(arrayof_gridinfo) :: input_data
TYPE(volgrid6d),POINTER  :: volgrid(:), volgrid_out(:)
TYPE(transform_def) :: trans

CALL l4f_launcher(a_name, a_name_force="create_projgrib")
ier = l4f_init()
category=l4f_category_get(TRIM(a_name)//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Create a grib with a specified grid and geographic projection &
 &from a template. All the parameters for defining a grid and a &
 &geographic projection in libsim are available. Not all of them have &
 &to be specified, since different parameters are required for different &
 &(groups of) grid', &
 usage_msg='Usage: prodsim_create_projgrib [options] gribtemplate outputfile')

! define command-line options
CALL optionparser_add(opt, ' ', 'type', proj_type, 'regular_ll', help= &
 'type of projection desired, possible vaues: regular_ll, rotated_ll, &
 &stretched_ll, stretched_rotated_ll, &
 &lambert, polar_stereographic, UTM')
CALL optionparser_add(opt, ' ', 'nx', nx, 31, help= &
 'number of nodes along x axis on target grid')
CALL optionparser_add(opt, ' ', 'ny', ny, 31, help= &
 'number of nodes along y axis on target grid')
CALL optionparser_add(opt, ' ', 'x-min', xmin, 0.0D0, help= &
 'x coordinate of the lower left corner of target grid (degrees or meters)')
CALL optionparser_add(opt, ' ', 'y-min', ymin, 30.0D0, help= &
 'y coordinate of the lower left corner of target grid (degrees or meters)')
CALL optionparser_add(opt, ' ', 'x-max', xmax, 30.0D0, help= &
 'x coordinate of the upper right corner of target grid (degrees or meters)')
CALL optionparser_add(opt, ' ', 'y-max', ymax, 60.0D0, help= &
 'y coordinate of the upper right corner of target grid (degrees or meters)')
CALL optionparser_add(opt, ' ', 'x-off', xoff, 0.0D0, help= &
 'x coordinate offset (also known as false easting) in target grid')
CALL optionparser_add(opt, ' ', 'y-off', yoff, 0.0D0, help= &
 'y coordinate offset (also known as false northing) in target grid')
lov = dmiss
CALL optionparser_add(opt, ' ', 'lov', lov, help= &
 'line of view, also known as reference longitude or orientation of the grid &
 &(polar projections)')
utm_zone = imiss
CALL optionparser_add(opt, ' ', 'utm-zone', utm_zone, help= &
 'zone number for UTM projections')
latitude_south_pole = dmiss
CALL optionparser_add(opt, ' ', 'latitude-south-pole', latitude_south_pole, &
 help='latitude of south pole for rotated grid')
longitude_south_pole = dmiss
CALL optionparser_add(opt, ' ', 'longitude-south-pole', longitude_south_pole, &
 help='longitude of south pole for rotated grid')
CALL optionparser_add(opt, ' ', 'angle-rotation', angle_rotation, &
 0.0D0, help='angle of rotation for rotated grid')
CALL optionparser_add(opt, ' ', 'component-flag', component_flag, &
 0, help='wind component flag in target grid (0/1)')
latin1 = dmiss
CALL optionparser_add(opt, ' ', 'latin1', latin1, &
 help='first latitude at which the projection plane intesects the sphere')
latin2 = dmiss
CALL optionparser_add(opt, ' ', 'latin2', latin2, &
 help='second latitude at which the projection plane intesects the sphere')
lad = dmiss
CALL optionparser_add(opt, ' ', 'lad', lad, &
 help='latitudine at which dx and dy (in m) are specified')
CALL optionparser_add(opt, ' ', 'pole', pole, &
 1, help='pole for polar projections, 1=N -1=S')

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
  WRITE(*,'(A,1X,A)')'prodsim_create_projgrib',VERSION
  CALL exit(0)
ENDIF
projection_center_flag = 64*(1-pole)

IF (optind + 1 <= iargc()) THEN
  CALL getarg(optind, input_file)
  IF (input_file == '-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit')
    input_file = '/dev/stdin'
  ENDIF

  optind = optind+1
  CALL getarg(optind, output_file)
  IF (output_file == '-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit')
    output_file = '/dev/stdout'
  ENDIF

ELSE
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category, L4F_FATAL, 'input or output file missing')
  CALL raise_fatal_error()
ENDIF

CALL IMPORT(volgrid, filename=input_file, decode=.TRUE., &
 categoryappend="input_volume")

CALL init(griddim_out, nx=nx, ny=ny, &
 xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, component_flag=component_flag, &
 proj_type=proj_type, lov=lov, zone=utm_zone, xoff=xoff, yoff=yoff, &
 longitude_south_pole=longitude_south_pole, latitude_south_pole=latitude_south_pole, angle_rotation=angle_rotation, &
 latin1=latin1, latin2=latin2, lad=lad, projection_center_flag=projection_center_flag, &
 categoryappend="output_grid") ! explicit parameters for the grid
! ellips_smaj_axis, ellips_flatt, ellips_type, &

CALL init(trans, trans_type='inter', sub_type='near', &
 categoryappend="transformation")

CALL transform(trans, griddim_out, volgrid6d_in=volgrid, &
 volgrid6d_out=volgrid_out, &
 clone=.TRUE., categoryappend="transform")

CALL export(volgrid_out, filename=output_file, categoryappend="output_volume")

CALL delete(volgrid)
CALL delete(volgrid_out)
CALL delete(trans)

END PROGRAM create_projgrib
