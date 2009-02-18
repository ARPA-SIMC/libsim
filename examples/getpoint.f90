program getpoint

use log4fortran
use volgrid6d_class
use grid_class
USE vol7d_dballe_class
USE vol7d_class
use getopt_m

implicit none

integer :: category,ier,i,nana
character(len=512):: a_name,infile,outfile
type (volgrid6d),pointer  :: volgrid(:),volgrid_out(:)
type(transform_def) :: trans
type(vol7d) :: v7d
type(vol7d_ana) :: ana
type(vol7d),pointer :: vol7d_out(:)
TYPE(vol7d_dballe) :: v7d_exp
doubleprecision :: lon=0.D0,lat=45.D0
character(len=4) :: format="bufr"
character(len=80) :: template="generic",trans_type="inter",sub_type="bilin"

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="getpoint")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")



do
  select case( getopt( "a:b:f:t:v:z:"))

  case( char(0))
    exit
  case( 'a' )
    read(optarg,*,iostat=ier)lon
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'a option argument error')
      call help()
      call exit(ier)
    end if

  case( 'b' )
    read(optarg,*,iostat=ier)lat
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'b option argument error')
      call help()
      call exit(ier)
    end if
  case( 'f' )
    format=optarg
  case( 't' )
    template=optarg
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
call l4f_category_log(category,L4F_INFO,"POINT:"//to_char(lon)//to_char(lat))

call init(v7d)
call vol7d_alloc(v7d,nana=1)
call vol7d_alloc_vol(v7d)

call init(ana,lat=lat,lon=lon)
v7d%ana(1)=ana

!trasformation object
call init(trans, trans_type=trans_type,sub_type=sub_type, categoryappend="trasformation")
call import (volgrid,filename=infile,categoryappend="volume letto")
call transform(trans,v7d, volgrid6d_in=volgrid, vol7d_out=vol7d_out,categoryappend="trasforma")

call l4f_category_log(category,L4F_INFO,"trasformato")
if (associated(volgrid)) call delete(volgrid)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_exp,file=.true.,write=.true.,wipe=.true.,filename=outfile,&
categoryappend="export_BUFR",format=format)

do i = 1 , size(vol7d_out)
  v7d_exp%vol7d = vol7d_out(i) 
  call export (v7d_exp,template=template)
  call delete(v7d_exp%vol7d)
end do
call l4f_category_log(category,L4F_INFO,"exportato to BUFR")

call l4f_category_log(category,L4F_INFO,"end")

call delete (v7d_exp)
deallocate (vol7d_out)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program getpoint

subroutine help()

print*,"Grib to bufr/crex trasformation application."
print*,"Read grib edition 1 and 2 and interpolate data over specified single point"
print*,""
print*,""
print*,"getpoint [-h] [-a lon] [-b lat] [-f format] [-t template] [-v trans_type] [-z sub_type] infile outfile"
print*,""
print*,"-h         this help message"
print*,"lon,lat    lon and lat of the target point"
print*,"format     specificando category.subcategory.localcategory oppure un alias"
print*,"           ('synop', 'metar','temp','generic') forza l'exportazione ad uno specifico template BUFR/CREX"
print*,"template   template of BUFR/CREX messages"
print*,"trans_type transformation type; inter for interpolation"
print*,"sub_type   transformation sub_type"
print*,"           inter: near , bilin"
print*,""
print*,""
print *,"default : lon=0. lat=45. format=bufr template=generic trans_type=inter sub_type=bilin"

end subroutine help
