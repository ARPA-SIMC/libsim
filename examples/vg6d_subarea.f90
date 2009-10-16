PROGRAM subarea

use gridinfo_class
use grid_class
use grid_transform_class
use log4fortran
use grib_api
use volgrid6d_class
use char_utilities
use getopt_m

implicit none

integer :: category,ier
character(len=512):: a_name,infile='in.grb',outfile='out.grb'
type (gridinfo_def) :: gridinfo
integer                            ::  ifile,ofile,gaid
integer                            ::  iret

integer :: ix,iy,fx,fy,iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy
doubleprecision ::  ilon,ilat,flon,flat
real, allocatable :: field(:,:),fieldz(:,:)
type(griddim_def) :: griddim_out
type(transform_def) :: trans
type(grid_transform) :: grid_trans

integer :: nx,ny,component_flag,npx,npy
doubleprecision :: lon_min, lon_max, lat_min, lat_max
doubleprecision :: latitude_south_pole,longitude_south_pole,angle_rotation
character(len=80) :: type,trans_type,sub_type

doubleprecision ::x,y,lon,lat
type(op_option) :: options(19) ! remember to update dimension when adding options
type(optionparser) :: opt
integer :: iargc

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="subarea")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

! define command-line options
options(1) = op_option_new('v', 'trans-type', trans_type, 'inter', help= &
 'transformation type: ''inter'' for interpolation, ''zoom'' for zooming, ''boxregrid'' for resolution reduction')
options(2) = op_option_new('z', 'sub-type', sub_type, 'near', help= &
 'transformation subtype, for inter: ''near'', ''bilin'', ''boxaverage'', &
 &for zoom: ''index'', ''coord'', for boxregrid: ''average''')

options(3) = op_option_new('u', 'type', type, 'regular_ll', help= &
 'type of interpolated grid: ''regular_ll'', ''rotated_ll''')
options(4) = op_option_new('i', 'nx', nx, 31, help= &
 'number of nodes along x axis on interpolated grid')
options(5) = op_option_new('l', 'ny', ny, 31, help= &
 'number of nodes along y axis on interpolated grid')
options(6) = op_option_new('m', 'x-min', lon_min, 0.0D0, help= &
 'x coordinate of the lower left corner of interpolated grid')
options(7) = op_option_new('o', 'y-min', lat_min, 30.0D0, help= &
 'y coordinate of the lower left corner of interpolated grid')
options(8) = op_option_new('n', 'x-max', lon_max, 30.0D0, help= &
 'x coordinate of the upper right corner of interpolated grid')
options(9) = op_option_new('p', 'y-max', lat_max, 60.0D0, help= &
 'y coordinate of the upper right corner of interpolated grid')

options(10) = op_option_new('q', 'latitude-south-pole', latitude_south_pole, &
 -32.5D0, help='latitude of south pole for rotated grid')
options(11) = op_option_new('r', 'longitude-south-pole', longitude_south_pole, &
 10.0D0, help='longitude of south pole for rotated grid')
options(12) = op_option_new('s', 'angle-rotation', angle_rotation, &
 0.0D0, help='angle of rotation for rotated grid')

options(13) = op_option_new('a', 'ilon', ilon, 0.0D0, help= &
 'longitude of the southwestern zooming corner')
options(14) = op_option_new('b', 'ilat', ilat, 30.D0, help= &
 'latitude of the southwestern zooming corner')
options(15) = op_option_new('c', 'flon', flon, 30.D0, help= &
 'longitude of the northeastern zooming corner')
options(16) = op_option_new('d', 'flat', flat, 60.D0, help= &
 'latitude of the northeastern zooming corner')

options(17) = op_option_new('f', 'npx', npx, 4, help= &
 'number of nodes along x axis on input grid, over which to perform average for boxregrid')
options(18) = op_option_new('g', 'npy', npy, 4, help= &
 'number of nodes along x axis on input grid, over which to perform average for boxregrid')

!options(19) = op_option_new('e', 'a-grid', c2agrid, help= &
! 'interpolate U/V points of an Arakawa C grid on the corresponding T points &
! &of an Arakawa A grid')

!options(20) = op_option_new('t', 'component-flag', component_flag, &
! 0, help='wind component flag in interpolated grid (0/1)')


options(19) = op_option_help_new('h', 'help', help= &
 'show an help message')

! define the option parser
opt = optionparser_new(options, description_msg= &
 'Grib to grib trasformation application. It reads grib edition 1 and 2 &
 &and zooms, interpolates or regrids data according to optional parameters.', &
 usage_msg='Usage: vg6d_subarea [options] inputfile outputfile')

! parse options and check for errors
optind = optionparser_parseoptions(opt)
IF (optind <= 0) THEN
  CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters')
  CALL EXIT(1)
ENDIF

if ( optind <= iargc()) then
  call getarg(optind, infile)
  optind=optind+1
else
  call l4f_category_log(category,L4F_ERROR,'input file missing')
  call optionparser_printhelp(opt)
  call exit(1)
end if

if ( optind <= iargc()) then
  call getarg(optind, outfile)
  optind=optind+1
else
  call l4f_category_log(category,L4F_ERROR,'output file missing')
  call optionparser_printhelp(opt)
  call exit(1)
end if

CALL delete(opt)

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(infile))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(outfile))
!call l4f_category_log(category,L4F_INFO,"AREA:"//to_char(ilon)//to_char(ilat)//to_char(flon)//to_char(flat))

!!call grib_new_from_template(igrib, "regular_ll_sfc_grib2")

if(trans_type == 'inter')then

  call init(griddim_out,&
   type=type,nx=nx,ny=ny, &
   lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max, component_flag=component_flag, &
   latitude_south_pole=latitude_south_pole,longitude_south_pole=longitude_south_pole,angle_rotation=angle_rotation, &
   categoryappend="regular_ll")

  call griddim_unproj(griddim_out)

  print*,'grid di interpolazione >>>>>>>>>>>>>>>>>>>>'
  call display(griddim_out)

end if

!call init(trans, trans_type=trans_type,sub_type=sub_type, &
! ilon=ilon,ilat=ilat,flon=flon,flat=flat,&
! categoryappend="trasformation")
call init(trans, trans_type=trans_type, sub_type=sub_type, &
 ilon=ilon, ilat=ilat, flon=flon, flat=flat, npx=npx, npy=npy, &
 boxpercentile=0.5D0, &
 categoryappend="trasformation")

call grib_open_file(ifile, trim(infile),'r')
call grib_open_file(ofile, trim(outfile),'w')


! Loop on all the messages in a file.

!     a new grib message is loaded from file
!     gaid is the grib id to be used in subsequent calls

gaid=-1
call  grib_new_from_file(ifile,gaid, iret) 


DO WHILE (iret == GRIB_SUCCESS)

   call l4f_category_log(category,L4F_INFO,"import gridinfo")

   call init (gridinfo,gaid=gaid,categoryappend="importato")
   call import(gridinfo)

   call display(gridinfo,namespace="ls")

   call l4f_category_log(category,L4F_INFO,"import")

   allocate (field(gridinfo%griddim%dim%nx,gridinfo%griddim%dim%ny))

   field=decode_gridinfo(gridinfo)

   call init(grid_trans, trans, in=gridinfo%griddim,out=griddim_out,categoryappend="gridtrasformato")

   call display(griddim_out)

   allocate (fieldz(griddim_out%dim%nx,griddim_out%dim%ny))

   call compute(grid_trans, field, fieldz)

   call delete(gridinfo%griddim)
   call copy(griddim_out,gridinfo%griddim,categoryappend="clonato")

! oppure per mantenere il vecchio gridinfo
!   call clone(gridinfo , gridinfo_out)
!   call delete(gridinfo_out%griddim)
!   call copy(griddim_out,gridinfo_out%griddim)

! per bug della grib_api
!   call grib_release(gridinfo%gaid)
!   call grib_new_from_template(gridinfo%gaid,"regular_ll_pl_grib1")

   call encode_gridinfo(gridinfo,fieldz)
   call export (gridinfo)
   call display(gridinfo,namespace="ls")

   call grib_write(gridinfo%gaid,ofile)

   call delete (grid_trans)
   call delete (gridinfo)
   deallocate (field,fieldz)

   gaid=-1
   call grib_new_from_file(ifile,gaid, iret)
   
end do

call delete (trans)
call delete(griddim_out)

call grib_close_file(ifile)
call grib_close_file(ofile)

call l4f_category_log(category,L4F_INFO,"terminato")

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program subarea
