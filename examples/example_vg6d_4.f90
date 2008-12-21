program demo4

use gridinfo_class
use grid_class
use log4fortran
use grib_api
use volgrid6d_class
use char_utilities
use getopt_m

implicit none

integer :: category,ier
character(len=512):: a_name,infile='in.grb',outfile='out.grb'
type (gridinfo_type) :: gridinfo
character:: ch
integer                            ::  ifile,ofile,gaid
integer                            ::  iret

integer :: ix,iy,fx,fy,iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy
doubleprecision ::  ilon=0.,ilat=30.,flon=30.,flat=60.
real, allocatable :: field(:,:),fieldz(:,:)
type(griddim_def) :: griddim_out
type(transform) :: trans
type(grid_transform) :: grid_trans

integer :: nx=30,ny=30,component_flag=0
doubleprecision :: lon_min=0., lon_max=30., lat_min=30., lat_max=60.
doubleprecision :: latitude_south_pole=-32.5,longitude_south_pole=10.,angle_rotation=0.
character(len=80) :: type='regular_ll',trans_type='inter',sub_type='near'

doubleprecision ::x,y,lon,lat


!da eliminare
integer :: i,j


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo4")

!imposta a_name
category=l4f_category_get(a_name//".main")

!init di log4fortran
ier=l4f_init()

opterr=.false.
do
  select case( getopt( "a:b:c:d:hi:l:m:n:o:p:q:r:s:t:u:v:z:"))

  case( char(0))
    exit
  case( 'a' )
    read(optarg,*,iostat=ier)ilon
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if

  case( 'b' )
    read(optarg,*,iostat=ier)ilat
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if

  case( 'c' )
    read(optarg,*,iostat=ier)flon
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if

  case( 'd' )
    read(optarg,*,iostat=ier)flat
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if

  case( 'i' )
    read(optarg,*,iostat=ier)nx
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 'l' )
    read(optarg,*,iostat=ier)ny
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 'm' )
    read(optarg,*,iostat=ier)lon_min
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 'n' )
    read(optarg,*,iostat=ier)lon_max
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 'o' )
    read(optarg,*,iostat=ier)lat_min
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 'p' )
    read(optarg,*,iostat=ier)lat_max
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 'q' )
    read(optarg,*,iostat=ier)latitude_south_pole
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 'r' )
    read(optarg,*,iostat=ier)longitude_south_pole
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 's' )
    read(optarg,*,iostat=ier)angle_rotation
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
      call help()
      call exit(ier)
    end if
  case( 't' )
    read(optarg,*,iostat=ier)component_flag
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'option argument error')
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
end if

if ( optind <= iargc()) then
  call getarg( optind,outfile)
  optind=optind+1
end if

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

call init(trans, trans_type=trans_type,sub_type=sub_type, &
 ilon=ilon,ilat=ilat,flon=flon,flat=flat,&
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

   gaid=-1
   call grib_new_from_file(ifile,gaid, iret)
   
   call display(gridinfo)

   call l4f_category_log(category,L4F_INFO,"import")

   allocate (field(gridinfo%griddim%dim%nx,gridinfo%griddim%dim%ny))

   field=decode_gridinfo(gridinfo)

   call init(grid_trans, trans, in=gridinfo%griddim,out=griddim_out,categoryappend="gridtrasformato")

   call display(griddim_out)

   allocate (fieldz(griddim_out%dim%nx,griddim_out%dim%ny))

   call compute(grid_trans, field, fieldz)

   call delete(gridinfo%griddim)
   call copy(griddim_out,gridinfo%griddim)

! oppure per mantenere il vecchio gridinfo
!   call clone(gridinfo , gridinfo_out)
!   call delete(gridinfo_out%griddim)
!   call copy(griddim_out,gridinfo_out%griddim)

   call encode_gridinfo(gridinfo,fieldz)
   call export (gridinfo)

   call grib_write(gridinfo%gaid,ofile)

   call delete (grid_trans)
   call delete (gridinfo)
   deallocate (field,fieldz)

end do

call delete (trans)
call delete(griddim_out)

call grib_close_file(ifile)
call grib_close_file(ofile)

call l4f_category_log(category,L4F_INFO,"terminato")

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo4


subroutine help()

print*,"demo4 [-h] [-a ilon] [-b ilat] [-c flon] [-d flat] "
print*,"           [-i nx] [-l ny] [-m lon_min] [-n lon_max] [-o lat_min] [-p lat_max]"
print*,"           [-q latitude_south_pole] [-r longitude_south_pole] [-s angle_rotation] [-t component_flag]"
print*,"           [-u type] [-v trans_type] [-z sub_type=optarg]"
print*,"           [infile] [outfile]"
print*,""
print*,"-h  this help message"
print*,"ilon,ilat  lon and lat in the left down point"
print*,"flon,flat  lon and lat in the right up  point"
print*,"trans_type transformation type; inter for interpolation, zomm for zomming, boxrregrid for resolution change"
print*,"sub_type   transformation sub_type"
print*,"           inter: near , bilin"
print*,"           zoom: index , coord"
print*,"           boxregrid: average"
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
print*,"           latitude_south_pole=-32.5 longitude_south_pole=10. angle_rotation=0. (component_flag=not.impl.)"
print*,"           type=regular_ll trans_type=inter sub_type=near"

end subroutine help
