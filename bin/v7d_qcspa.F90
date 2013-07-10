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
! Example program to quality control data with climatological values

#include "config.h"

program esempio_qcspa

use log4fortran
use modqc
use modqcspa
use vol7d_dballe_class
USE optionparser_class
use array_utilities
#ifdef HAVE_LIBNCARG
USE ncar_plot_class
#endif

implicit none

integer :: category,io,ier,i
character(len=512):: a_name

                                !tipi derivati.
TYPE(optionparser) :: opt
TYPE(geo_coord)    :: coordmin, coordmax 
TYPE(datetime)     :: time,ti, tf, timei, timef, timeiqc, timefqc
type(qcspatype)    :: v7dqcspa
type(vol7d_dballe) :: v7ddballe
#ifdef HAVE_LIBNCARG
type(ncar_plot) :: plot
#endif

integer, parameter :: maxvar=10
character(len=6) :: var(maxvar)=cmiss   ! variables to elaborate
character(len=80) :: dsn='test1',user='test',password=''
character(len=80) :: dsne='test',usere='test',passworde=''
integer :: years=imiss,months=imiss,days=imiss,hours=imiss,yeare=imiss,monthe=imiss,daye=imiss,houre=imiss,nvar=0
doubleprecision :: lons=dmiss,lats=dmiss,lone=dmiss,late=dmiss,lon,lat
integer :: year, month, day, hour
logical :: height2level=.false.,doplot=.false.,version
CHARACTER(len=512) :: input_file, output_file
INTEGER :: optind, optstatus, ninput
CHARACTER(len=20) :: operation
TYPE(ARRAYOF_REAL):: grad
real :: val
integer :: iostat
REAL, DIMENSION(11)   :: perc_vals=(/(10.*i,i=0,10)/) !(/0.,10.,20.,30.,40.,50.,60.,70.,80.,90.,100./)  !<the percentiles values to be computed, between 0. and 100.
REAL, DIMENSION(size(perc_vals)-1)   :: ndi        !< normalized density index
REAL, DIMENSION(size(perc_vals))     :: nlimbins   !< the extreme values of data taken in account for ndi computation
double precision, dimension(2)       :: percentile

    
namelist /odbc/   dsn,user,password,dsne,usere,passworde       ! namelist to define DSN
namelist /switch/ height2level,doplot
namelist /minmax/ years,months,days,hours,lons,lats,yeare,monthe,daye,houre,lone,late
namelist /varlist/ var

!init log4fortran
ier=l4f_init()

! unique name from launcher
call l4f_launcher(a_name,a_name_force="esempio_qcspa")

! set a_name
category=l4f_category_get(a_name//".main")


! define the option parser
opt = optionparser_new(description_msg= &
 'Spatial quality control: compute gradient; compute NDI from gradient; apply quality control', &
 usage_msg='Usage: v7d_transform [options] [inputfile1] [inputfile2...] [outputfile]')

! options for defining input
CALL optionparser_add(opt, ' ', 'operation', operation, cmiss, help= &
 'operation to execute: ''gradient'' compute gradient and write on files; '&
  //'''ndi''  compute NDI from gradient;' &
  //'''run'' apply quality control ')


! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add_html(opt, ' ', 'html-form', help= &
 &'print the options as an html form')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0) ! generate a clean manpage
ELSE IF (optstatus == optionparser_html) THEN
  CALL exit(0) ! generate a clean form
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line parameters')
  CALL raise_fatal_error()
ENDIF
IF (version) THEN
  WRITE(*,'(A,1X,A)')'v7d_qcspa',VERSION
  CALL exit(0)
ENDIF


if (operation /= "gradient"  .and. operation /= "ndi" .and. operation /= "run") then
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category, L4F_ERROR, &
   'argument to --operation is wrong')
  CALL raise_fatal_error()
end if


if (operation == "ndi") then

                                ! check input/output files
  i = iargc() - optind
  IF (i < 0) THEN
    CALL optionparser_printhelp(opt)
    CALL l4f_category_log(category,L4F_ERROR,'input file missing')
    CALL raise_fatal_error()
  ELSE IF (i < 1) THEN
    CALL optionparser_printhelp(opt)
    CALL l4f_category_log(category,L4F_ERROR,'output file missing')
    CALL raise_fatal_error()
  ENDIF
  CALL getarg(iargc(), output_file)
  
  
  do ninput = optind, iargc()-1
    call getarg(ninput, input_file)

    print *,"open file:", t2c(input_file)

    open (10,file=input_file,status="old")


    do while (.true.)
      read (10,*,iostat=iostat) val
      if (iostat /= 0) exit
      if (val /= 0.) call insert(grad,val)
    end do

    close(10)

  end do
  
  print *,"calcolo percentile"
  percentile = stat_percentile(grad%array(:grad%arraysize), (/10.,90./))
  print *,percentile

  print *,"calcolo NDI"
  call NormalizedDensityIndex(pack(grad%array(:grad%arraysize),&
   mask=(percentile(1) < grad%array(:grad%arraysize) .and. &
   grad%array(:grad%arraysize) < percentile(2) )), perc_vals, ndi, nlimbins)

  print *,ndi
  print *,nlimbins


!  0.13021742      0.25837621      0.45376661      0.73488075      0.99223697       1.0000000       1.0000000      0.66323280      0.38160193      0.20700261    
! -2.21905229E-05 -1.18105772E-05 -6.57925284E-06 -3.60051945E-06 -1.76120716E-06 -3.99033837E-07  9.52641415E-07  2.22688618E-06  4.26485803E-06  7.80683240E-06  1.43365824E-05

  call delete(grad)

  stop

end if

!------------------------------------------------------------------------
! read the namelist to define DSN
!------------------------------------------------------------------------

open(10,file='qcspa.nml',status='old')
read(10,nml=odbc,iostat=io)
if ( io == 0 ) read(10,nml=switch,iostat=io)
if ( io == 0 ) read(10,nml=minmax,iostat=io)
if ( io == 0 ) read(10,nml=varlist,iostat=io)

if (io /= 0 )then
    call l4f_category_log(category,L4F_ERROR,"Error reading namelist qcspa.nml")
    call raise_error("Error reading namelist qcspa.nml")
end if
close(10)


!------------------------------------------------------------------------
! Define what you want to QC
!------------------------------------------------------------------------

nvar=count(c_e(var))

if (nvar == 0) then
    call l4f_category_log(category,L4F_ERROR,"0 variables defined")
    call raise_error()
end if
                                ! Definisco le date iniziale e finale
CALL init(ti, year=years, month=months, day=days, hour=hours)
CALL init(tf, year=yeare, month=monthe, day=daye, hour=houre)
print *,"time extreme"
call display(ti)
call display(tf)

                                ! Define coordinate box
CALL init(coordmin,lat=lats,lon=lons)
CALL init(coordmax,lat=late,lon=lone)

call getval(coordmin,lon=lon,lat=lat)
print*,"lon lat minumum",lon,lat
call getval(coordmax,lon=lon,lat=lat)
print*,"lon lat maximum",lon,lat

!------------------------------------------------------------------------
call l4f_category_log(category,L4F_INFO,"QC on "//t2c(nvar)//" variables")
do i=1,nvar
  call l4f_category_log(category,L4F_INFO,"QC on "//var(i)//" variable")
enddo
if (c_e(lons)) call l4f_category_log(category,L4F_INFO,"QC on "//t2c(lons)//" lon min value")
if (c_e(lone)) call l4f_category_log(category,L4F_INFO,"QC on "//t2c(lone)//" lon max value")
if (c_e(lats)) call l4f_category_log(category,L4F_INFO,"QC on "//t2c(lats)//" lat min value")
if (c_e(late)) call l4f_category_log(category,L4F_INFO,"QC on "//t2c(late)//" lat max value")
if (c_e(ti))   call l4f_category_log(category,L4F_INFO,"QC on "//t2c(ti)//" datetime min value")
if (c_e(tf))   call l4f_category_log(category,L4F_INFO,"QC on "//t2c(tf)//" datetime max value")
!------------------------------------------------------------------------


!timei=ti
time=ti+timedelta_new(minute=30)
CALL getval(time,year, month, day, hour)
call init(time,  year, month, day, hour, minute=00, msec=00)
!if (time < timei) time=time+timedelta_new(hour=1)
!timef=tf
!if (time > timef) time=timei

#ifdef HAVE_LIBNCARG
if (doplot) then
  call l4f_category_log(category,L4F_INFO,"start plot")
  call init(plot,PSTYPE='PS', ORIENT='LANDSCAPE',COLOR='COLOR',file="v7d_qcspa.ps")
end if
#endif
DO WHILE (time <= tf)
  timei = time - timedelta_new(minute=30)
  timef = time + timedelta_new(minute=30)
  timeiqc = time - timedelta_new(minute=15)
  timefqc = time + timedelta_new(minute=15)
  time  = time + timedelta_new(minute=30)
  call l4f_category_log(category,L4F_INFO,"elaborate from "//t2c(timeiqc)//" to "//t2c(timefqc))

                                ! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
  CALL init(v7ddballe,dsn=dsn,user=user,password=password,write=.true.,wipe=.false.,categoryappend="QCtarget-"//t2c(time))
  call l4f_category_log(category,L4F_INFO,"start data import")

  CALL import(v7ddballe,var=var(:nvar),varkind=(/("r",i=1,nvar)/),&
   anavar=(/"B07030"/),anavarkind=(/"r"/),&
   attr=(/qcattrvarsbtables(1),qcattrvarsbtables(2),qcattrvarsbtables(4)/),attrkind=(/"b","b","b"/)&
   ,timei=timei,timef=timef,coordmin=coordmin,coordmax=coordmax)
  
  !call display(v7ddballe%vol7d)
  call l4f_category_log(category,L4F_INFO,"end data import")
  call l4f_category_log(category,L4F_INFO, "input N staz="//t2c(size(v7ddballe%vol7d%ana)))

  call l4f_category_log(category,L4F_INFO,"start peeling")

  !remove data invalidated and gross error only
  !qcpar=qcpartype(0_int_b,0_int_b,0_int_b)
  qcpar%att=bmiss
  call vol7d_peeling(v7ddballe%vol7d,v7ddballe%data_id,keep_attr=(/qcattrvarsbtables(4)/),purgeana=.true.)
  !call display(v7ddballe%vol7d)

  call l4f_category_log(category,L4F_INFO, "filtered N staz="//t2c(size(v7ddballe%vol7d%ana)))

  call l4f_category_log(category,L4F_INFO,"start QC")

                                ! chiamiamo il "costruttore" per il Q.C.
  call init(v7dqcspa,v7ddballe%vol7d,var(:nvar),timei=ti,timef=tf, coordmin=coordmin, coordmax=coordmax,&
   data_id_in=v7ddballe%data_id, dsne=dsne, usere=usere, height2level=height2level, operation=operation,&
   categoryappend="space")
  !call display(v7dqcspa%clima)
  !call display(v7dqcspa%extreme)

  call alloc(v7dqcspa)

  ! spatial QC
  call l4f_category_log(category,L4F_INFO,"start spatial QC")
  call quaconspa(v7dqcspa,noborder=.true.,timemask= ( v7dqcspa%v7d%time >= timeiqc .and. v7dqcspa%v7d%time <= timefqc ))
  call l4f_category_log(category,L4F_INFO,"end spatial QC")

#ifdef HAVE_LIBNCARG
  if (doplot) then
    call l4f_category_log(category,L4F_INFO,"start plot")
    call plot_triangles(plot,v7dqcspa%co,v7dqcspa%tri,logo="Time: "//t2c(timeiqc)//" to "//t2c(timefqc))
    call frame()
  end if
#endif

  call l4f_category_log(category,L4F_INFO,"start export data")
  !call display(v7ddballe%vol7d)

  CALL export(v7ddballe,attr_only=.true.)

  call l4f_category_log(category,L4F_INFO,"end export data")

  call delete(v7ddballe)
  call delete(v7dqcspa)

end do

#ifdef HAVE_LIBNCARG
  if (doplot) then
    call delete(plot)
  end if
#endif

!close logger
call l4f_category_delete(category)
ier=l4f_fini()

end program esempio_qcspa
