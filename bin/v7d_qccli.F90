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

program v7d_qccli

use log4fortran
USE missing_values
USE geo_coord_class
USE datetime_class
USE modqc
use modqccli
use vol7d_class
#ifdef OLDDBALLEAPI 
use vol7d_dballeold_class
#else
use vol7d_dballe_class
#endif
implicit none

integer :: category,io,ier,i
character(len=512):: a_name

                                !tipi derivati.
TYPE(geo_coord)    :: coordmin, coordmax 
TYPE(datetime)     :: ti, tf
type(qcclitype)    :: v7dqccli
type(vol7d_dballe) :: v7ddballe


integer, parameter :: maxvar=10
character(len=6) :: var(maxvar)=cmiss   ! variables to elaborate
character(len=80) :: dsn='test',user='test',password=''
character(len=80) :: dsnc='test1', dsne="test2", userce='test',passwordce='', macropath=cmiss
integer :: years=imiss,months=imiss,days=imiss,hours=imiss,yeare=imiss,monthe=imiss,daye=imiss,houre=imiss,nvar=0
doubleprecision :: lons=dmiss,lats=dmiss,lone=dmiss,late=dmiss
logical :: height2level=.false.

namelist /odbc/   dsn,user,password,dsnc,dsne,userce,passwordce,macropath,height2level       ! namelist to define DSN
namelist /minmax/ years,months,days,hours,lons,lats,yeare,monthe,daye,houre,lone,late
namelist /varlist/ var

!init log4fortran
ier=l4f_init()

! unique name from launcher
call l4f_launcher(a_name,a_name_force="v7d_qccli")

! set a_name
category=l4f_category_get(a_name//".main")

!------------------------------------------------------------------------
! read the namelist to define DSN
!------------------------------------------------------------------------

open(10,file='qccli.nml',status='old')
read(10,nml=odbc,iostat=io)
if ( io == 0 ) read(10,nml=minmax,iostat=io)
if ( io == 0 ) read(10,nml=varlist,iostat=io)

if (io /= 0 )then
    call l4f_category_log(category,L4F_ERROR,"Error reading namelist odbc.nml")
    call raise_error("Error reading namelist odbc.nml")
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
                                ! Define coordinate box
CALL init(coordmin,lat=lats,lon=lons)
CALL init(coordmax,lat=late,lon=lone)


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


                                ! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7ddballe,dsn=dsn,user=user,password=password,write=.true.,wipe=.false.,categoryappend="QCtarget")

call l4f_category_log(category,L4F_INFO,"start data import")

CALL import(v7ddballe,var=var(:nvar),varkind=(/("r",i=1,nvar)/),&
 anavar=(/"B07030"/),anavarkind=(/"r"/),&
 attr=qcattrvarsbtables(1:2),attrkind=(/"b","b"/)&
 ,timei=ti,timef=tf,coordmin=coordmin,coordmax=coordmax)

print *,"data input:"
call display(v7ddballe%vol7d)
call l4f_category_log(category,L4F_INFO,"end data import")

call l4f_category_log(category,L4F_INFO,"start QC")

                                ! chiamiamo il "costruttore" per il Q.C.
call init(v7dqccli,v7ddballe%vol7d,var(:nvar), &
 timei=ti,timef=tf, &
#ifdef OLDDBALLEAPI  
 data_id_in=v7ddballe%data_id,&
#endif
 dsncli=dsnc, dsnextreme=dsne, &
 user=userce, password=passwordce, macropath=macropath, height2level=height2level, categoryappend="clima")

print *,"data extreme:"
call display(v7dqccli%extreme)

print *,"data clima:"
call display(v7dqccli%clima)

call l4f_category_log(category,L4F_INFO,"start climat QC")
call quaconcli(v7dqccli)
call l4f_category_log(category,L4F_INFO,"end climat QC")

call l4f_category_log(category,L4F_INFO,"start export data")
print *,"data output:"
call display(v7ddballe%vol7d)


#ifdef OLDDBALLEAPI  
deallocate (v7ddballe%data_id)
v7ddballe%data_id => v7dqccli%data_id_out
#endif

CALL export(v7ddballe&
#ifdef OLDDBALLEAPI 
,attr_only=.true.
#endif
)

call l4f_category_log(category,L4F_INFO,"end export data")

call delete(v7dqccli)
call delete(v7ddballe)

!close logger
call l4f_category_delete(category)
ier=l4f_fini()

end program v7d_qccli
