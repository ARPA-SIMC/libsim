! Plot temp bufr messages with  herlofson termodinamic rappresentation 

program example_ncarg_sounding

use log4fortran
USE vol7d_dballe_class
USE vol7d_class
USE ncar_plot_class

implicit none

integer :: category,ier
character(len=512):: a_name,filename="../data/example_temp.bufr"
TYPE(vol7d_dballe) :: v7d_dba
type(ncar_plot) :: plot
integer   :: time,ana,timerange,network

character(len=20) ::  tcolor(6)=(/'tan   ','brown ','orange','red   ','black ','black '/)
character(len=20) ::  tdcolor(6)=(/'yellow      ','green       ','forest Green','cyan        ','black       ','black       '/)
character(len=20) ::  ucolor(6)=(/'sky blue    ','blue        ','blue magenta','magenta     ','black       ','black       '/)
character(len=20) ::  wcolor(6)=(/'light gray','dark gray ','black     ','violet    ','black     ','black     '/)


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="readtemp")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d_dba,file=.true.,write=.false.,filename=filename,&
 categoryappend="importBUFR",format="BUFR")

!call import (v7d_dba,var=(/"B12001","B12003","B11001","B11002"/),varkind=(/"d","d","d","d"/))
call import (v7d_dba)

call l4f_category_log(category,L4F_INFO,"importato vol7d")

call vol7d_reform(v7d_dba%vol7d,sort=.true.)

call display(v7d_dba%vol7d)

!out on ps file
call init(plot,file="example_ncarg_sounding.ps",&
 PSTYPE="PS", ORIENT="PORTRAIT",COLOR="COLOR")

!out on X11 display
!call init(plot,wstype=8)

call plot_herlofson(plot,logo="Test : S.I.M.C. ARPA Emilia Romagna")

do network=1,size(v7d_dba%vol7d%network)
  do ana=1, size(v7d_dba%vol7d%ana)
    do time=1, size(v7d_dba%vol7d%time)
      do timerange=1, size(v7d_dba%vol7d%timerange)

        call plot_vertical_plofiles(plot,v7d_dba%vol7d,&
         ana,time,timerange,network,&
         tcolor=tcolor(time),tdcolor=tdcolor(time),&
         ucolor=ucolor(time),wcolor=wcolor(time))
!        call plot_vp_title (plot,v7d_dba%vol7d,ana,time,timerange,network)

!        CALL FRAME

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

end program example_ncarg_sounding
