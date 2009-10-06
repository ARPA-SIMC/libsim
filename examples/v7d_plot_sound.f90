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
integer :: wstype=imiss,ic
character(len=512):: a_name,infile,outfile,PSTYPE="PS", ORIENT="LANDSCAPE", COLOR="COLOR"
character(len=100) :: nomogramma="herlofson",logo="Met Service"
TYPE(vol7d_dballe) :: v7d_dba
TYPE(vol7d) :: v7d_profile
type(ncar_plot) :: plot
integer   :: time,ana,timerange,network
logical ::  packtimerange=.false.,changepg=.false.,distinct=.false.
character(len=20) ::  tcolor(4)=(/'brown','red','black','tan'/)
character(len=20) ::  tdcolor(4)=(/'orange','forest Green','cyan','yellow'/)
character(len=20) ::  ucolor(4)=(/'sky blue','blue','blue magenta','magenta'/)
character(len=20) ::  wcolor(4)=(/'black','violet','light gray','dark gray'/)

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="readtemp")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

do
  select case( getopt( "w:p:o:c:n:l:htd"))

  case( char(0))
    exit

  case( 'w' )
    read(optarg,*,iostat=ier)wstype
    if (ier/= 0)then
      call l4f_category_log(category,L4F_ERROR,'w option argument error')
      call help()
      call exit(ier)
    end if

  case( 'p' )
    pstype=optarg
    if (pstype/='PS' .and. pstype/='EPS' .and. pstype/='EPSI')then
      call l4f_category_log(category,L4F_ERROR,'p option argument error')
      call help()
      call exit(ier)
    end if

  case( 'o' )
    orient=optarg
    if (orient/='PORTRAIT' .and. orient/='LANDSCAPE')then
      call l4f_category_log(category,L4F_ERROR,'o option argument error')
      call help()
      call exit(ier)
    end if

  case( 'c' )
    color=optarg
    if (color/='COLOR' .and. color/='MONOCHROME')then
      call l4f_category_log(category,L4F_ERROR,'c option argument error')
      call help()
      call exit(ier)
    end if

  case( 'n' )
    nomogramma=optarg

  case( 'l' )
    logo=optarg

  case( 'h' )
    call help()
    call exit(0)

  case( '?' )
    call l4f_category_log(category,L4F_ERROR,'unknown option '//optopt)
    call help()
    call exit(1)

  case( 't' )
    packtimerange=.true.

  case( 'd' )
    distinct=.true.

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

if ( c_e(wstype))then
  call init(plot,file=outfile,wstype=wstype)
else
  call init(plot,file=outfile,PSTYPE=pstype, ORIENT=orient,COLOR=color)
end if

do ana=1, size(v7d_dba%vol7d%ana)
  do time=1, size(v7d_dba%vol7d%time)
    ic=0
    do network=1,size(v7d_dba%vol7d%network)

      if (packtimerange) then
        ic=mod(ic,4)+1       ! cicla sui 4 colori

        if (changepg) then
          CALL FRAME
          ic=1
          changepg=.false.
        end if

        if (ic == 1)  then
          call plot_vp_title (plot,v7d_dba%vol7d,ana,time,1,network,color=tcolor(ic))  !solo primo titolo
          call plot_herlofson(plot,logo=logo,nomogramma=nomogramma)
        end if
      end if

      do timerange=1, size(v7d_dba%vol7d%timerange)

        if (.not. packtimerange) then
          ic=mod(ic,4)+1       ! cicla sui 4 colori

          if (changepg) then
            CALL FRAME
            ic=1
            changepg=.false.
          end if

          if (ic == 1)  then
            call plot_vp_title (plot,v7d_dba%vol7d,ana,time,timerange,network,color=tcolor(ic))  !solo primo titolo
            call plot_herlofson(plot,logo=logo,nomogramma=nomogramma)
          end if
        end if

        call init(v7d_profile)
        call vol7d_normalize_vcoord(v7d_dba%vol7d,v7d_profile,ana,time,timerange,network)


        call plot_vertical_plofiles(plot,v7d_profile,1,1,1,1,&
         tcolor=tcolor(ic),tdcolor=tdcolor(ic),&
         ucolor=ucolor(ic),wcolor=wcolor(ic))

        call plot_vp_legend (plot,v7d_profile,1,1,1,1,&
         tcolor=tcolor(ic),tdcolor=tdcolor(ic),ucolor=ucolor(ic),wcolor=wcolor(ic),position=ic) ! legenda
        call delete(v7d_profile)
        if (ic == 4 )changepg=.true.
        if (distinct) then
          changepg=.true.
        end if

      end do
    end do
    changepg=.true.
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
print*,"v7d_plt_sound [-h] [-w wstype] [-p PSTYPE] [-o ORIENT] [-c COLOR] [-n nomogramma] [-l logo] [-t] [-d] infile outfile"
print*,""
print*,"-h            this help message"
print*,"-w wstype     work station type (see ncar GKS manuals - wstype=8 X11 display)"
print*,""
print*,"    oppure se omesso wstype"
print*,""
print*,"-p pstype     'PS', 'EPS', or 'EPSI'"
print*,"-o orient     'PORTRAIT' or 'LANDSCAPE'"
print*,"-c color      'COLOR' or 'MONOCHROME'" 
print*,"-n nomogramma 'tipo di nomogramma aerologico: herlofson//herlofson-down//emagramma//emagramma-down'"
print*,"-l logo       'logo to print in footer'"
print*,"-t            'collapse timerange dimension writing the first in title and legend'"
print*,"-d            'distinct page each sounding'"
print*,""
print*,""
print *,"default :  pstype='PS' orient='LANDSCAPE' clor='COLOR' nomogramma='herlofson' logo='Met Service'"
end subroutine help


end program example_ncarg_sounding
