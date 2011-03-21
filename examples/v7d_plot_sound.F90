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
! Plot temp bufr messages with  herlofson termodinamic rappresentation 
#include "config.h"
PROGRAM v7d_plot_sound

use log4fortran
USE vol7d_dballe_class
USE vol7d_class_compute
USE vol7d_class
USE ncar_plot_class
USE optionparser_class
USE err_handling

implicit none

type(optionparser) :: opt
INTEGER :: optind, optstatus, category, ier
logical :: version
integer :: wstype,ic
character(len=512):: a_name, infile, outfile, PSTYPE, ORIENT, COLOR
character(len=100) :: nomogram, logo
TYPE(vol7d_dballe) :: v7d_dba
TYPE(vol7d) :: v7d_profile
type(ncar_plot) :: plot
integer :: time,ana,timerange,network
logical ::  packtimerange,changepg=.false.,distinct
character(len=20) :: tcolor(4)=(/'brown','red  ','black','tan  '/)
character(len=20) :: tdcolor(4)=(/'orange      ','forest Green','cyan        ','yellow      '/)
character(len=20) :: ucolor(4)=(/'sky blue    ','blue        ','blue magenta','magenta     '/)
character(len=20) :: wcolor(4)=(/'black     ','violet    ','light gray','dark gray '/)

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="readtemp")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Program for plotting an Herlofson diagram from a BUFR/CREX file. Different &
 &output formats, either on file or on screen, are supported.', &
 usage_msg='Usage: v7d_plot_sound [options] inputfile outputfile')

wstype = imiss
CALL optionparser_add(opt, 'w', 'wstype', wstype, help= &
 'workstation type (see NCAR GKS manuals, e.g. 8=X11 display), if &
 &omitted (default), then postscript is chosen, see pstype option')
CALL optionparser_add(opt, 'p', 'pstype', pstype, 'PS', help= &
 'postscript type (''PS'', ''EPS'', or ''EPSI'') if wstype is provided &
 &this option is ininfluent')
CALL optionparser_add(opt, 'o', 'orient', orient, 'LANDSCAPE', help= &
 'postscript orientation (''PORTRAIT'' or ''LANDSCAPE'')')
CALL optionparser_add(opt, 'c', 'color', color, 'COLOR', help= &
 'postscript color mode (''COLOR'' or ''MONOCHROME'')')
CALL optionparser_add(opt, 'n', 'nomogram', nomogram, 'herlofson', help= &
 'nomogram type (''herlofson'', ''herlofson-down'', ''emagram'', ''emagram-down''')
CALL optionparser_add(opt, 'l', 'logo', logo, 'Met Service', help= &
 'logo to print in footer')
CALL optionparser_add(opt, 't', 'packtimerange', packtimerange, help= &
 'collapse all in a single timerange dimension, writing the first timerange &
 &in title and legend')
CALL optionparser_add(opt, 'd', 'distinct', distinct, help= &
 'put every plot on a distinct page')

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
  WRITE(*,'(A,1X,A)')'v7d_plot_sound',VERSION
  CALL exit(0)
ENDIF

IF (pstype/='PS' .AND. pstype/='EPS' .AND. pstype/='EPSI')THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'pstype '//TRIM(pstype)//' not valid')
  CALL raise_fatal_error()
ENDIF
IF (orient/='PORTRAIT' .AND. orient/='LANDSCAPE')THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'orient '//TRIM(orient)//' not valid')
  CALL raise_fatal_error()
ENDIF
IF (color/='COLOR' .AND. color/='MONOCHROME')THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'color '//TRIM(color)//' not valid')
  CALL raise_fatal_error()
ENDIF

IF (optind <= iargc()) THEN
  CALL getarg(optind, infile)
  optind=optind+1
ELSE
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'input file missing')
  CALL raise_fatal_error()
ENDIF

IF (optind <= iargc()) THEN
  CALL getarg(optind, outfile)
  optind=optind+1
ELSE
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'output file missing')
  CALL raise_fatal_error()
ENDIF

CALL delete(opt)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d_dba,file=.true.,write=.false.,filename=infile,&
 categoryappend="importBUFR",format="BUFR")

!call import (v7d_dba,var=(/"B12101","B12103","B11001","B11002"/),varkind=(/"d","d","d","d"/))
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
        ic=mod(ic,4)+1       ! cicle over 4 color

        if (changepg) then
          CALL FRAME
          ic=1
          changepg=.false.
        end if

        if (ic == 1)  then
          call plot_vp_title (plot,v7d_dba%vol7d,ana,time,1,network,color=tcolor(ic))  !solo primo titolo
          call plot_herlofson(plot,logo=logo,nomogramma=nomogram)
        end if
      end if

      do timerange=1, size(v7d_dba%vol7d%timerange)

        if (.not. packtimerange) then
          ic=mod(ic,4)+1       ! cicle over 4 color

          if (changepg) then
            CALL FRAME
            ic=1
            changepg=.false.
          end if

          if (ic == 1)  then
            call plot_vp_title (plot,v7d_dba%vol7d,ana,time,timerange,network,color=tcolor(ic))  !solo primo titolo
            call plot_herlofson(plot,logo=logo,nomogramma=nomogram)
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

call l4f_category_log(category,L4F_INFO,"terminated")


!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM v7d_plot_sound
