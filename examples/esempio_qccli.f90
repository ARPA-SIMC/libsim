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

program esempio_qccli

use log4fortran
use modqccli
use vol7d_dballe_class

implicit none

integer :: category,io,ier,i
character(len=19) :: database,user,password
character(len=512):: a_name
character(len=6) :: var(1)      ! variable to elaborate

                                !tipi derivati.
TYPE(geo_coord)    :: coordmin, coordmax 
TYPE(datetime)     :: ti, tf
type(qcclitype)    :: v7dqccli
type(vol7d_dballe) :: v7ddballe

namelist  /odbc/database,user,password       ! namelist to define DSN

!init log4fortran
ier=l4f_init()

! unique name from launcher
call l4f_launcher(a_name,a_name_force="esempio_qccli")

! set a_name
category=l4f_category_get(a_name//".main")

!------------------------------------------------------------------------
! Define what you want to QC
!------------------------------------------------------------------------

var=(/"B12101"/)                ! variables to elaborate

                                ! Definisco le date iniziale e finale
CALL init(ti, year=2011, month=05, day=1, hour=00)
CALL init(tf, year=2011, month=05, day=30, hour=00)

!------------------------------------------------------------------------
! read the namelist to define DSN
!------------------------------------------------------------------------

open(10,file='odbc.nml',status='old')
read(10,nml=odbc,iostat=io)
if (io /= 0 )then
    call l4f_category_log(category,L4F_ERROR,"Error reading namelist odbc.nml")
    call raise_error("Error reading namelist odbc.nml")
end if
close(10)

!------------------------------------------------------------------------

                                ! Define coordinate box
CALL init(coordmin,lat=43.70_fp_geo,lon=9.16_fp_geo)
CALL init(coordmax,lat=45.2_fp_geo,lon=12.84_fp_geo)

                                ! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7ddballe,dsn=database,user=user,password=password,write=.true.,wipe=.false.,categoryappend="QCtarget")

call l4f_category_log(category,L4F_INFO,"start data import")

CALL import(v7ddballe,var=var,varkind=(/("r",i=1,size(var))/),&
 anavar=(/"B07030"/),anavarkind=(/"r"/),&
 attr=(/"*B33196","*B33192"/),attrkind=(/"b","b"/)&
 ,timei=ti,timef=tf,coordmin=coordmin,coordmax=coordmax)

call display(v7ddballe%vol7d)
call l4f_category_log(category,L4F_INFO,"end data import")

call l4f_category_log(category,L4F_INFO,"start QC")

                                ! chiamiamo il "costruttore" per il Q.C.

call init(v7dqccli,v7ddballe%vol7d,var,timei=ti,timef=tf,coordmin=coordmin,coordmax=coordmax,&
 data_id_in=v7ddballe%data_id, dsn="test", user="test", categoryappend="base")
! data_id_in=v7ddballe%data_id, dsn="qccli", user="qc", password="qc", categoryappend="base")

print *,"ecco"
call display(v7dqccli%clima)
print *,"fine"

call alloc(v7dqccli)

call l4f_category_log(category,L4F_INFO,"start climat QC")

call quaconcli(v7dqccli)

call l4f_category_log(category,L4F_INFO,"end QC")

call l4f_category_log(category,L4F_INFO,"start export data")

call display(v7ddballe%vol7d)

CALL export(v7ddballe,attr_only=.true.)

call l4f_category_log(category,L4F_INFO,"end export data")

call delete(v7dqccli)
call delete(v7ddballe)

!close logger
call l4f_category_delete(category)
ier=l4f_fini()

end program esempio_qccli
