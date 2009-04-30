! Plot temp bufr messages with  herlofson termodinamic rappresentation 

program example_ncarg_sounding

use log4fortran
USE vol7d_dballe_class
USE vol7d_class_compute
USE vol7d_class
USE ncar_plot_class
use getopt_m

implicit none

integer :: category,ier
integer :: wstype=8 ! default out on X11 display
character(len=512):: a_name,infile,outfile
TYPE(vol7d_dballe) :: v7d_dba
TYPE(vol7d) :: v7d_profile
type(ncar_plot) :: plot
integer   :: time,ana,timerange,network

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="readtemp")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

do
  select case( getopt( "w:h"))

  case( char(0))
    exit

  case( 'w' )
    read(optarg,*,iostat=ier)wstype
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'w option argument error')
      call help()
      call exit(ier)
    end if

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



! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d_dba,file=.true.,write=.false.,filename=infile,&
 categoryappend="importBUFR",format="BUFR")

!call import (v7d_dba,var=(/"B12001","B12003","B11001","B11002"/),varkind=(/"d","d","d","d"/))
call import (v7d_dba)

call l4f_category_log(category,L4F_INFO,"importato vol7d")

call vol7d_reform(v7d_dba%vol7d,sort=.true.)

!call vol7d_normalize_vcoord(v7d_dba%vol7d)

call display(v7d_dba%vol7d)

call init(plot,file=outfile,wstype=wstype,PSTYPE="PS", ORIENT="LANDSCAPE",COLOR="COLOR")

do network=1,size(v7d_dba%vol7d%network)
  do ana=1, size(v7d_dba%vol7d%ana)
    do time=1, size(v7d_dba%vol7d%time)
      do timerange=1, size(v7d_dba%vol7d%timerange)

        call init(v7d_profile)
        call plot_herlofson(plot,logo="Test : S.I.M.C. ARPA Emilia Romagna")
        call vol7d_normalize_vcoord(v7d_dba%vol7d,v7d_profile,ana,time,timerange,network)
        call plot_vertical_plofiles(plot,v7d_profile,1,1,1,1)
        call plot_vp_title (plot,v7d_profile,1,1,1,1)
        CALL FRAME
        call delete(v7d_profile)

      end do
    end do
  end do
end do

call delete(plot)

call delete (v7d_dba)

call l4f_category_log(category,L4F_INFO,"terminato")


!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

contains

subroutine help()

print*,"Plot herlofson diagram  from bufr/crex file."
print*,""
print*,""
print*,"v7d_plt_sound [-h] [-w wstype]  infile outfile"
print*,""
print*,"-h         this help message"
print*,"wstype     work station type (see ncar GKS manuals) "
print*,""
print*,""
print *,"default : wstype=8 X11 display"

end subroutine help


end program example_ncarg_sounding
