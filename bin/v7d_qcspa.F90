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
! Program to quality control data with climatological values

#include "config.h"

program v7d_qcspa

use log4fortran
USE missing_values
USE simple_stat
USE geo_coord_class
USE datetime_class
use modqc
use modqcspa
!use vol7d_dballeold_class
use vol7d_dballe_class
USE vol7d_class
USE optionparser_class
use array_utilities
#ifdef HAVE_LIBNCARG
USE ncar_plot_class
#endif

implicit none

integer :: category,io,ier,i,iun,n,ind
character(len=512):: a_name,output_format, output_template

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
#ifdef HAVE_DBALLE
character(len=512) :: dsn='test1',user='test',password=''
character(len=512) :: dsne='test',usere='test',passworde=''
character(len=512) :: dsnspa='test',userspa='test',passwordspa=''
#endif
integer :: years=imiss,months=imiss,days=imiss,hours=imiss,yeare=imiss,monthe=imiss,daye=imiss,houre=imiss,nvar=0
doubleprecision :: lons=dmiss,lats=dmiss,lone=dmiss,late=dmiss
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
TYPE(cyclicdatetime) :: cyclicdt !< cyclic date and time
type(vol7d_level) :: level,levelo
type(vol7d_timerange) :: timerange,timerangeo
type(vol7d_var) :: varia,variao
CHARACTER(len=vol7d_ana_lenident) :: ident
integer :: indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork,indcattr
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d_dballe) :: v7d_dba_out
logical :: file
integer :: status,grunit
    
#ifdef HAVE_DBALLE
namelist /odbc/   dsn,user,password,dsne,usere,passworde
namelist /odbcspa/ dsnspa,userspa,passwordspa       ! namelist to define DSN
#endif
namelist /switchspa/ height2level,doplot
namelist /minmax/ years,months,days,hours,lons,lats,yeare,monthe,daye,houre,lone,late
namelist /varlist/ var

!init log4fortran
ier=l4f_init()

! unique name from launcher
call l4f_launcher(a_name,a_name_force="v7d_qcspa")

! set a_name
category=l4f_category_get(a_name//".main")


! define the option parser
opt = optionparser_new(description_msg= &
 'Spatial quality control: compute gradient; compute NDI from gradient; apply quality control', &
 usage_msg='Usage: v7d_qcspa [options] [inputfile1] [inputfile2...] [outputfile] \n&
 &If input-format is of file type, inputfile ''-'' indicates stdin,'&
#ifdef HAVE_DBALLE
 //'if  output-format is of database type, outputfile specifies &
 &database access info in the form user/password@dsn,' &
#endif
 //'if empty or ''-'', a suitable default is used. &
&')

! options for defining input
CALL optionparser_add(opt, ' ', 'operation', operation, "run", help= &
 'operation to execute: ''gradient'' compute gradient and write on files; '&
  //'''ndi''  compute NDI from gradient;' &
  //'''run'' apply quality control ')


! options for defining output
output_template = ''
CALL optionparser_add(opt, ' ', 'output-format', output_format, 'native', help= &
 'format of output file for "ndi" operation only, in the form ''name[:template]''; ''native'' for vol7d &
 &native binary format (no template to be specified)'&
#ifdef HAVE_DBALLE
 //'; ''BUFR'' and ''CREX'' for corresponding formats, with template as an alias like ''synop'', ''metar'', &
 &''temp'', ''generic'', empty for ''generic'''&
#endif
)

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
  WRITE(*,'(A,1X,A)')'v7d_qcspa',VERSION
  CALL exit(0)
ENDIF


if (operation /= "gradient"  .and. operation /= "ndi" .and. operation /= "run") then
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category, L4F_ERROR, &
   'argument to --operation is wrong')
  CALL raise_fatal_error()
end if


! check output format/template
n = word_split(output_format, w_s, w_e, ':')
IF (n >= 2) THEN ! set output template if present
  output_template = output_format(w_s(2):w_e(2))
  output_format(w_e(1)+1:) = ' '
ENDIF
DEALLOCATE(w_s, w_e)

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
  

                                !you can define level, timerange, var or get it from file
  call init(levelo)
  call init(timerangeo)
  call init (variao)

  do ninput = optind, iargc()-1
    call getarg(ninput, input_file)

    CALL l4f_category_log(category,L4F_INFO,"open file: "//t2c(input_file))
    grunit=getunit()
    open (grunit,file=input_file,status="old")
    read (grunit,*,iostat=status) level,timerange,varia
    if (status /= 0) then
      CALL l4f_category_log(category,L4F_WARN,"error reading: "//t2c(input_file))
      close(grunit)
      cycle
    endif

    if (c_e(levelo)) then

      !if (t2c(input_file) /= to_char(levelo)//"_"//to_char(timerangeo)//"_"//variao%btable//".grad")
      if ( level /= levelo .or. timerange /= timerangeo .or. varia /= variao ) then
        call l4f_category_log(category,L4F_ERROR,"Error reading grad files: file are incoerent")
        call raise_error("")
      end if
    else
      levelo = level
      timerangeo = timerange
      variao = varia
    end if

    do while (.true.)
      read (grunit,*,iostat=iostat) val
      if (iostat /= 0) exit
      if (val /= 0.) call insert(grad,val)
    end do

    close(grunit)
  end do
  
  CALL l4f_category_log(category,L4F_INFO,"compute percentile to remove the tails")
  percentile = stat_percentile(grad%array(:grad%arraysize),(/10.,90./))
  !print *,percentile

    CALL l4f_category_log(category,L4F_INFO,"compute NDI")
  call NormalizedDensityIndex(pack(grad%array(:grad%arraysize),&
   mask=(grad%array(:grad%arraysize) < percentile(2) )), perc_vals, ndi, nlimbins)

  call delete(grad)
  call delete(v7dqcspa%clima)
  CALL init(v7dqcspa%clima, time_definition=0)
  call vol7d_alloc(v7dqcspa%clima,nana=size(perc_vals)-1, &
   nlevel=1, ntimerange=1, &
   ndativarr=1, nnetwork=1,ntime=1,ndativarattrr=1,ndatiattrr=1)

  call vol7d_alloc_vol(v7dqcspa%clima,inivol=.true.)

  call init(v7dqcspa%clima%network(1),name="qcspa-ndi")
  v7dqcspa%clima%level=level
  v7dqcspa%clima%timerange=timerange
  v7dqcspa%clima%dativar%r(1)=varia
  v7dqcspa%clima%dativarattr%r(1)=varia
  call init(v7dqcspa%clima%datiattr%r(1), btable="*B33209")    ! NDI order number
  cyclicdt = cyclicdatetime_new(chardate="/////////") !TMMGGhhmm
  v7dqcspa%clima%time(1)=cyclicdatetime_to_conventional(cyclicdt)

  indctime=1
  indclevel=1
  indcnetwork=1
  indcattr=1
  indcdativarr=1
  indctimerange=1

  do indcana=1,size(perc_vals)-1
    write(ident,'("#",i2.2,2i3.3)')0,0,nint(perc_vals(indcana))
    call init(v7dqcspa%clima%ana(indcana),ident=ident,lat=0d0,lon=0d0)
    if (c_e(nlimbins(indcana)).and.c_e(ndi(indcana)))then
      ind=index_c(spa_btable,varia%btable)
      v7dqcspa%clima%voldatir(indcana, indctime, indclevel, indctimerange, indcdativarr, indcnetwork)=&
       nlimbins(indcana)*spa_a(ind) + spa_b(ind)
      v7dqcspa%clima%voldatiattrr(indcana, indctime, indclevel, indctimerange, indcdativarr, indcnetwork,indcattr)=&
       ndi(indcana)*100.
    end if
  end do

  print *,">>>>>> NDI Volume <<<<<<"
  call display (v7dqcspa%clima)

  IF (output_format == 'native') THEN
    IF (output_file == '-') THEN ! stdout_unit does not work with unformatted
      CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
      output_file='/dev/stdout'
    ENDIF
    iun = getunit()
    OPEN(iun, file=output_file, form='UNFORMATTED', access=stream_if_possible)
    CALL export(v7dqcspa%clima, unit=iun)
    CLOSE(iun)
    CALL delete(v7dqcspa%clima)

#ifdef HAVE_DBALLE
  ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX' .OR. output_format == 'dba') THEN
    IF (output_format == 'BUFR' .OR. output_format == 'CREX') THEN
      IF (output_file == '-') THEN
        CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
        output_file='/dev/stdout'
      ENDIF
      file=.TRUE.

    ELSE IF (output_format == 'dba') THEN
      CALL parse_dba_access_info(output_file, dsn, user, password)
      file=.FALSE.
    ENDIF

    IF (output_template == '') output_template = 'generic'
                                ! check whether wipe=file is reasonable
    CALL init(v7d_dba_out, filename=output_file, FORMAT=output_format, &
     dsn=dsn, user=user, password=password, file=file, WRITE=.TRUE., wipe=file)
    
    v7d_dba_out%vol7d = v7dqcspa%clima
    CALL init(v7dqcspa%clima) ! nullify without deallocating
    CALL export(v7d_dba_out, template=output_template)
    CALL delete(v7d_dba_out)
#endif
    
  end if

  stop

end if

!------------------------------------------------------------------------
! read the namelist to define DSN
!------------------------------------------------------------------------

open(10,file='qc.nml',status='old')
read(10,nml=odbc,iostat=io)
if ( io == 0 ) read(10,nml=odbcspa,iostat=io)
if ( io == 0 ) read(10,nml=switchspa,iostat=io)
if ( io == 0 ) read(10,nml=minmax,iostat=io)
if ( io == 0 ) read(10,nml=varlist,iostat=io)

if (io /= 0 )then
    call l4f_category_log(category,L4F_ERROR,"Error reading namelist qc.nml")
    call raise_error("Error reading namelist qc.nml")
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

!call getval(coordmin,lon=lon,lat=lat)
print*,"lon lat minimum -> ",to_char(coordmin)
!call getval(coordmax,lon=lon,lat=lat)
print*,"lon lat maximum -> ",to_char(coordmax)

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
  
!  call display(v7ddballe%vol7d)
  call l4f_category_log(category,L4F_INFO,"end data import")
  call l4f_category_log(category,L4F_INFO, "input N staz="//t2c(size(v7ddballe%vol7d%ana)))

  call l4f_category_log(category,L4F_INFO,"start peeling")

  !remove data invalidated and gross error only
  !qcpar=qcpartype(0_int_b,0_int_b,0_int_b)
  qcpar%att=bmiss
  !call vol7d_peeling(v7ddballe%vol7d,v7ddballe%data_id,keep_attr=(/qcattrvarsbtables(4)/),purgeana=.true.)
  call vol7d_peeling(v7ddballe%vol7d,keep_attr=(/qcattrvarsbtables(4)/),purgeana=.true.)
!  call display(v7ddballe%vol7d)

  call l4f_category_log(category,L4F_INFO, "filtered N staz="//t2c(size(v7ddballe%vol7d%ana)))

  call l4f_category_log(category,L4F_INFO,"initialize QC")

                                ! chiamiamo il "costruttore" per il Q.C.


  call init(v7dqcspa,v7ddballe%vol7d,var(:nvar),timei=timeiqc,timef=timefqc, coordmin=coordmin, coordmax=coordmax,&
   !data_id_in=v7ddballe%data_id, &
   dsne=dsne, usere=usere, passworde=passworde,&
   dsnspa=dsnspa, userspa=userspa, passwordspa=passwordspa,&
   height2level=height2level, operation=operation,&
   categoryappend="space")

!  print *,">>>>>> Clima Spatial Volume <<<<<<"
!  call display(v7dqcspa%clima)

  !print *,">>>>>> Pre Data Volume <<<<<<"
  !call display(v7dqcspa%v7d)

  call alloc(v7dqcspa)

  ! spatial QC
  !exclude the last time to do not check data two times
  call l4f_category_log(category,L4F_INFO,"start spatial QC")
  call quaconspa(v7dqcspa,noborder=.true.,timemask= ( v7dqcspa%v7d%time >= timeiqc .and. v7dqcspa%v7d%time < timefqc ))
  call l4f_category_log(category,L4F_INFO,"end spatial QC")

#ifdef HAVE_LIBNCARG
  if (doplot) then
    call l4f_category_log(category,L4F_INFO,"start plot")
    call plot_triangles(plot,v7dqcspa%co,v7dqcspa%tri,logo="Time: "//t2c(timeiqc)//" to "//t2c(timefqc))
    call frame()
  end if
#endif


  ! prepare data_id to be recreated
  !deallocate(v7ddballe%data_id)
  !nullify(v7ddballe%data_id)

  if (v7dqcspa%operation == "run") then
    call l4f_category_log(category,L4F_INFO,"start export data")
    !print *,">>>>>> Post Data Volume <<<<<<"
    !call display(v7ddballe%vol7d)

    ! data_id to use is the new one
    !v7ddballe%data_id => v7dqcspa%data_id_out
    CALL export(v7ddballe,attr_only=.true.)
    !CALL export(v7ddballe)
    call l4f_category_log(category,L4F_INFO,"end export data")
  end if

  call delete(v7dqcspa)
  ! data_id was allready deleted
  !nullify(v7ddballe%data_id)
  call delete(v7ddballe)

end do

#ifdef HAVE_LIBNCARG
  if (doplot) then
    call delete(plot)
  end if
#endif

!close logger
call l4f_category_delete(category)
ier=l4f_fini()

CONTAINS

SUBROUTINE parse_dba_access_info(string, dsn, user, password)
CHARACTER(len=*),INTENT(in) :: string
CHARACTER(len=*),INTENT(out) :: dsn
CHARACTER(len=*),INTENT(out) :: user
CHARACTER(len=*),INTENT(out) :: password

INTEGER :: bar, at

IF (string == '-' .OR. string == '') THEN
  dsn = cmiss
  user = cmiss
  password = cmiss
ELSE
  bar = INDEX(string, '/')
  at = INDEX(string, '@')
  IF (bar > 0 .AND. at > bar) THEN
    user = string(:bar-1)
    password = string(bar+1:at-1)
    dsn = string(at+1:)
  ELSE
    CALL optionparser_printhelp(opt)
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, database access info '// &
     TRIM(string)//' not valid.')
    CALL raise_fatal_error()
  ENDIF
ENDIF

END SUBROUTINE parse_dba_access_info


end program v7d_qcspa
