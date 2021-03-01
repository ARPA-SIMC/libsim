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
USE vol7d_class
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif
USE vol7d_class_compute
USE ncar_plot_class
USE optionparser_class
USE err_handling

implicit none

type(optionparser) :: opt
INTEGER :: iargc, optind, optstatus, category, ier
logical :: version
integer :: wstype,ic
CHARACTER(len=8) :: input_format
character(len=512):: a_name, input_file, outfile, PSTYPE, ORIENT, COLOR
character(len=100) :: nomogram, logo
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_dba
INTEGER :: time_definition
#endif
TYPE(vol7d) :: v7dtmp, v7d_profile
type(ncar_plot) :: plot
integer :: time,ana,timerange,network
LOGICAL :: file, ldisplay, packtimerange, changepg=.FALSE., distinct
character(len=20) :: tcolor(4)=(/'brown','red  ','black','tan  '/)
character(len=20) :: tdcolor(4)=(/'orange      ','forest Green','cyan        ','yellow      '/)
character(len=20) :: ucolor(4)=(/'sky blue    ','blue        ','blue magenta','magenta     '/)
character(len=20) :: wcolor(4)=(/'black     ','violet    ','light gray','dark gray '/)
character(len=20) :: acolor(4)=(/'black     ','violet    ','light gray','dark gray '/)
character(len=10) :: addonvar

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="readtemp")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Program for plotting an Herlofson diagram from one or more &
 &atmospheric vertical profiles imported from a vol7d native file'&
#ifdef HAVE_DBALLE
 //', from a dbAll.e database, from a BUFR/CREX file'&
#endif
 //'. Different output formats, either on file or on screen, are supported.', &
 usage_msg='Usage: v7d_plot_sound [options] inputfile outputfile')
wstype = imiss
CALL optionparser_add(opt, ' ', 'input-format', input_format, &
#ifdef HAVE_DBALLE
'BUFR', &
#else
'native', &
#endif
&  help='format of input, ''native'' for vol7d native binary file'&
#ifdef HAVE_DBALLE
 //', ''BUFR'' for BUFR file with generic template, ''CREX'' for CREX file&
 &, ''dba'' for dballe database'&
#endif
 )

#ifdef HAVE_DBALLE
CALL optionparser_add(opt, ' ', 'time-definition', time_definition, 1, help= &
 'time definition for vol7d volume, 0 for reference time (more suitable for &
 &presenting forecast data) and 1 for verification time (more suitable for &
 &comparing forecasts with observations)')
#endif

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
 'nomogram type (''herlofson'', ''herlofson-down'', ''emagramma'', ''emagramma-down''')
CALL optionparser_add(opt, 'l', 'logo', logo, 'Met Service', help= &
 'logo to print in footer')
CALL optionparser_add(opt, 't', 'packtimerange', packtimerange, help= &
 'collapse all in a single timerange dimension, writing the first timerange &
 &in title and legend')
CALL optionparser_add(opt, 'd', 'distinct', distinct, help= &
 'put every plot on a distinct page')
CALL optionparser_add(opt, ' ', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
addonvar=cmiss
CALL optionparser_add(opt, ' ', 'addon-var', addonvar, help= &
 'addition variable to plot with ortogonal vertical axes.')

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
  CALL getarg(optind, input_file)
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

IF (input_format == 'native') THEN
  IF (input_file == '-') THEN ! stdin_unit does not work with unformatted
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit.')
    input_file='/dev/stdin'
  ENDIF
  CALL import(v7dtmp, filename=input_file)

#ifdef HAVE_DBALLE
ELSE IF (input_format == 'BUFR' .OR. input_format == 'CREX' .OR. input_format == 'dba') THEN

!    IF (.NOT.ALLOCATED(nl)) THEN
!      allocate (nl(0))
!    ENDIF
!
!    IF( .NOT.ALLOCATED(vl)) THEN
!      allocate (vl(0))
!    ENDIF

  IF (input_format == 'BUFR' .OR. input_format == 'CREX') THEN

    IF (input_file == '-') THEN
      !CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit.')
      !input_file='/dev/stdin'
      input_file=''
    ENDIF
    file=.TRUE.

  ELSE IF (input_format == 'dba') THEN
    file=.FALSE.
  ENDIF
 
  CALL init(v7d_dba, filename=input_file, FORMAT=input_format, &
   dsn=input_file, file=file, &
   time_definition=time_definition, categoryappend="importBUFR")

!call import (v7d_dba,var=(/"B12101","B12103","B11001","B11002"/),varkind=(/"d","d","d","d"/))
  CALL import(v7d_dba)

  v7dtmp = v7d_dba%vol7d
  CALL init(v7d_dba%vol7d) ! nullify without deallocating
  CALL delete(v7d_dba)

#endif

ENDIF

call l4f_category_log(category,L4F_INFO,"importato vol7d")

!call vol7d_reform(v7dtmp,sort=.true.)

!call vol7d_normalize_vcoord(v7dtmp)

IF (ldisplay) CALL display(v7dtmp)

if ( c_e(wstype))then
  call init(plot,file=outfile,wstype=wstype)
else
  call init(plot,file=outfile,PSTYPE=pstype, ORIENT=orient,COLOR=color)
end if

do ana=1, size(v7dtmp%ana)
  do time=1, size(v7dtmp%time)
    ic=0
    do network=1,size(v7dtmp%network)

      if (packtimerange) then
        ic=mod(ic,4)+1       ! cicle over 4 color

        if (changepg) then
          CALL FRAME
          ic=1
          changepg=.false.
        end if

        if (ic == 1)  then
          call plot_vp_title (plot,v7dtmp,ana,time,1,network,color=tcolor(ic))  !solo primo titolo
          call plot_herlofson(plot,logo=logo,nomogramma=nomogram)
        end if
      end if

      do timerange=1, size(v7dtmp%timerange)

        if (.not. packtimerange) then
          ic=mod(ic,4)+1       ! cicle over 4 color

          if (changepg) then
            CALL FRAME
            ic=1
            changepg=.false.
          end if

          if (ic == 1)  then
            call plot_vp_title (plot,v7dtmp,ana,time,timerange,network,color=tcolor(ic))  !solo primo titolo
            call plot_herlofson(plot,logo=logo,nomogramma=nomogram)
          end if
        end if

        call init(v7d_profile)
        call vol7d_normalize_vcoord(v7dtmp,v7d_profile,ana,time,timerange,network)

        call plot_vertical_plofiles(plot,v7d_profile,1,1,1,1,&
         tcolor=tcolor(ic),tdcolor=tdcolor(ic),&
         ucolor=ucolor(ic),wcolor=wcolor(ic),acolor=acolor(ic),addonvar=addonvar)

        call plot_vp_legend (plot,v7d_profile,1,1,1,1,addonvar=addonvar,&
         tcolor=tcolor(ic),tdcolor=tdcolor(ic),ucolor=ucolor(ic),wcolor=wcolor(ic),acolor=acolor(ic),position=ic) ! legenda
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

call delete(v7dtmp)

call l4f_category_log(category,L4F_INFO,"terminated")


!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM v7d_plot_sound
