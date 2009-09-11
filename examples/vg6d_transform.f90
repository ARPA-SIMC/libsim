program volgrid6dtransform

use log4fortran
use volgrid6d_class
use grid_class
use getopt_m

implicit none

integer :: category,ier,i,nana
character(len=512):: a_name,infile,outfile
type (volgrid6d),pointer  :: volgrid(:),volgrid_out(:)

doubleprecision ::  ilon=0.,ilat=30.,flon=30.,flat=60.

type(griddim_def) :: griddim_out
type(transform_def) :: trans
type(grid_transform) :: grid_trans

INTEGER :: nx=30,ny=30,component_flag=1,npx=4,npy=4
doubleprecision :: lon_min=0., lon_max=30., lat_min=30., lat_max=60.
doubleprecision :: latitude_south_pole=-32.5,longitude_south_pole=10.,angle_rotation=0.
character(len=80) :: type='regular_ll',trans_type='none',sub_type='near'

doubleprecision ::x,y,lon,lat
logical :: c2agrid=.false.

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="volgrid6dtransform")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

do
  select case( getopt( "a:b:c:d:ef:g:hi:l:m:n:o:p:q:r:s:t:u:v:z:"))

  case( char(0))
    exit
  case( 'a' )
    read(optarg,*,iostat=ier)ilon
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'a option argument error')
      call help()
      call exit(ier)
    end if

  case( 'b' )
    read(optarg,*,iostat=ier)ilat
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'b option argument error')
      call help()
      call exit(ier)
    end if

  case( 'c' )
    read(optarg,*,iostat=ier)flon
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'c option argument error')
      call help()
      call exit(ier)
    end if

  case( 'd' )
    read(optarg,*,iostat=ier)flat
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'d option argument error')
      call help()
      call exit(ier)
    end if

  case( 'e' )
    c2agrid=.true.

  case( 'f' )
    read(optarg,*,iostat=ier)npx
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'f option argument error')
      call help()
      call exit(ier)
    end if
  case( 'g' )
    read(optarg,*,iostat=ier)npy
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'g option argument error')
      call help()
      call exit(ier)
    end if
  case( 'i' )
    read(optarg,*,iostat=ier)nx
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'i option argument error')
      call help()
      call exit(ier)
    end if
  case( 'l' )
    read(optarg,*,iostat=ier)ny
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'l option argument error')
      call help()
      call exit(ier)
    end if
  case( 'm' )
    read(optarg,*,iostat=ier)lon_min
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'m option argument error')
      call help()
      call exit(ier)
    end if
  case( 'n' )
    read(optarg,*,iostat=ier)lon_max
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'n option argument error')
      call help()
      call exit(ier)
    end if
  case( 'o' )
    read(optarg,*,iostat=ier)lat_min
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'o option argument error')
      call help()
      call exit(ier)
    end if
  case( 'p' )
    read(optarg,*,iostat=ier)lat_max
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'p option argument error')
      call help()
      call exit(ier)
    end if
  case( 'q' )
    read(optarg,*,iostat=ier)latitude_south_pole
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'q option argument error')
      call help()
      call exit(ier)
    end if
  case( 'r' )
    read(optarg,*,iostat=ier)longitude_south_pole
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'r option argument error')
      call help()
      call exit(ier)
    end if
  case( 's' )
    read(optarg,*,iostat=ier)angle_rotation
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'s option argument error')
      call help()
      call exit(ier)
    end if
  case( 't' )
    read(optarg,*,iostat=ier)component_flag
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'t option argument error')
      call help()
      call exit(ier)
    end if
 case( 'u' )
    type=optarg
 case( 'v' )
    trans_type=optarg
 case( 'z' )
    sub_type=optarg

  case( 'h' )
    call help()
    call exit(0)
  case( '?' )
    call l4f_category_log(category,L4F_ERROR,'unknown option '//optopt)
    call help()
    call exit(1)

  case default
    call l4f_category_log(category,L4F_ERROR,'unhandled option '// optopt// '(this is a bug)')
    call help()
    call exit(1)
  end select
end do
if ( optind <= iargc()) then
  call getarg( optind,infile)
  optind=optind+1
else
    call l4f_category_log(category,L4F_ERROR,'input file missing')
    call help()
    call exit(1)
end if

if ( optind <= iargc()) then
  call getarg( optind,outfile)
  optind=optind+1
else
    call l4f_category_log(category,L4F_ERROR,'output file missing')
    call help()
    call exit(1)
end if



call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(infile))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(outfile))

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


call import (volgrid,filename=infile,categoryappend="input")
if (c2agrid) call vg6d_c2a(volgrid)


if (trans_type == 'none') then

  !call display(volgrid)
  !exportazione
  call export (volgrid,filename=outfile,categoryappend="exportazione")

  call l4f_category_log(category,L4F_INFO,"end")

  if (associated(volgrid)) call delete (volgrid)

else

 !trasformation object
  call init(trans, trans_type=trans_type, sub_type=sub_type, &
   ilon=ilon, ilat=ilat, flon=flon, flat=flat, npx=npx, npy=npy, &
   boxpercentile=0.5D0, &
   categoryappend="trasformation")

  if (trans_type == 'inter') then
    CALL transform(trans,griddim_out,volgrid6d_in=volgrid, &
     volgrid6d_out=volgrid_out,clone=.TRUE.,categoryappend="trasformato")
  else
    CALL transform(trans,volgrid6d_in=volgrid, volgrid6d_out=volgrid_out, &
     clone=.TRUE.,categoryappend="trasformato")
  endif

  call l4f_category_log(category,L4F_INFO,"trasformato")
  if (associated(volgrid)) call delete(volgrid)


  !exportazione
  call export (volgrid_out,filename=outfile,categoryappend="exportazione")

  call l4f_category_log(category,L4F_INFO,"end")

  call delete (volgrid_out)

end if


!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program volgrid6dtransform

subroutine help()

print*,"Grib to grib trasformation application."
print*,"Read grib edition 1 and 2 and transform data according with optional parameters"
print*,"Whole grib data file is read and organized in memory, transformed, and written on output."
print*,"So is possible to elaborate multi field function like wind component transformation, but"
print*,"hardware constrains are more important and limit the number of field possible in input."
print*,"More different date, timeranges, levels and parameters with missing field for each,"
print*,"and more memory will be required"
print*,""
print*,""
print*,"subarea [-h] [-a ilon] [-b ilat] [-c flon] [-d flat] "
print*,"           [-e]"
print*,"           [-f npx] [-g npy]"
print*,"           [-i nx] [-l ny] [-m lon_min] [-n lon_max] [-o lat_min] [-p lat_max]"
print*,"           [-q latitude_south_pole] [-r longitude_south_pole] [-s angle_rotation] [-t component_flag]"
print*,"           [-u type] [-v trans_type] [-z sub_type=optarg]"
print*,"           infile outfile"
print*,""
print*,"-h  this help message"
print*,"-e  interpolate U/V points of C grid in the relative T points like A grid"
print*,"ilon,ilat  lon and lat in the lower left point"
print*,"flon,flat  lon and lat in the upper right point"
PRINT*,"npx,npy    number of points along x and y for boxregrid"
print*,"trans_type transformation type; inter for interpolation, zomm for zomming, boxrregrid for resolution change"
print*,"sub_type   transformation sub_type"
print*,"           inter: near , bilin"
print*,"           zoom: index , coord"
print*,"           boxregrid: average"
print*,"           none: no operations"
print*,"infile,outfile  input and output file"
print*,""
print*,"only interpolation options:"
print*,"type             grid type: regular_ll , rotated_ll" 
print*,"nx, ny           number of nodes on interpolated grid"
print*,"lon_min lat_min  lon and lat in the left down point of interpolated grid" 
print*,"lat_min lat_max  lon and lat in the right up  point of interpolated grid" 
print*,"latitude_south_pole, longitude_south_pole, angle_rotation   rotated grid parameters"
print*,""
print *,"default : ilon=0. ilat=30. flon=30. flat=60."
print*,"           nx=30 ny=30 lon_min=0. lon_max=30. lat_min=30. lat_max=60"
print*,"           latitude_south_pole=-32.5 longitude_south_pole=10. angle_rotation=0. component_flag=1"
print*,"           type=regular_ll trans_type=none sub_type=near"

end subroutine help
