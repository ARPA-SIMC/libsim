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
character(len=512):: a_name,infile,outfile
type (gridinfo_type) :: gridinfo
character:: ch
integer                            ::  ifile,ofile,gaid
integer                            ::  iret

integer :: ix,iy,fx,fy,iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy
doubleprecision ::  ilon,ilat,flon,flat
real, allocatable :: field(:,:),fieldz(:,:)


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo4")

!imposta a_name
category=l4f_category_get(a_name//".main")

!init di log4fortran
ier=l4f_init()


ilon=-5.
ilat=30.
flon=30.
flat=50.
infile='gribmix.grb'
outfile='gribnew.grb'

opterr=.false.
do
  select case( getopt( "a:b:c:d:"))
  case( char(0))
    exit
  case( 'a' )
    read(optarg,*,err=99,end=99)ilon
  case( 'b' )
    read(optarg,*,err=99,end=99)ilat
  case( 'c' )
    read(optarg,*,err=99,end=99)flat
  case( 'd' )
    read(optarg,*,err=99,end=99)flon
  case( '?' )
    call l4f_category_log(category,L4F_ERROR,'unknown option '//optopt)
    call exit(1)
  case default
    call l4f_category_log(category,L4F_ERROR,'unhandled option '// optopt// '(this is a bug)')
    call exit(2)
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

call l4f_category_log(category,L4F_INFO,"zooming fron file:"//trim(infile))
call l4f_category_log(category,L4F_INFO,"zooming to   file:"//trim(outfile))
call l4f_category_log(category,L4F_INFO,"AREA:"//to_char(ilon)//to_char(ilat)//to_char(flon)//to_char(flat))


call grib_open_file(ifile, trim(infile),'r')
call grib_open_file(ofile, trim(outfile),'w')


! Loop on all the messages in a file.

!     a new grib message is loaded from file
!     gaid is the grib id to be used in subsequent calls

gaid=-1
call  grib_new_from_file(ifile,gaid, iret) 


DO WHILE (iret == GRIB_SUCCESS)

   call l4f_category_log(category,L4F_INFO,"import gridinfo")

   call init (gridinfo,gaid=gaid,categoryappend="test")
   call import(gridinfo)

   gaid=-1
   call grib_new_from_file(ifile,gaid, iret)
   
   call display(gridinfo)

   call l4f_category_log(category,L4F_INFO,"import")

   allocate (field(gridinfo%griddim%dim%nx,gridinfo%griddim%dim%ny))

   field=decode_gridinfo(gridinfo)

   call zoom_coord(gridinfo%griddim,ilon,ilat,flon,flat,ix,iy,fx,fy)

!   ix=-5
!   iy=-6
!   fx=100
!   fy=150

   call zoom_index(gridinfo%griddim,gridinfo%griddim,ix,iy,fx,fy,&
    iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy) 

   allocate (fieldz(newx,newy))
   
   call zoom_field(field,fieldz,iox,ioy,fox,foy,inx,iny,fnx,fny) 
   
   call encode_gridinfo(gridinfo,fieldz)
   call export (gridinfo)

   call grib_write(gridinfo%gaid,ofile)
   call delete (gridinfo)

   deallocate (field,fieldz)

end do

call grib_close_file(ifile)
call grib_close_file(ofile)

call l4f_category_log(category,L4F_INFO,"terminato")

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

call exit(0)

99 call l4f_category_log(category,L4F_ERROR,'option argument error')
call exit(1)

end program demo4
