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
PROGRAM v7ddballe_dba2v7d
! Programma di esempio di estrazione e scrittura dall'archivio DB-all.e
use dballe_class
USE datetime_class
USE log4fortran
USE vol7d_class
USE vol7d_dballe_class

IMPLICIT NONE

TYPE(vol7d_dballe) :: v7d_dba
TYPE(vol7d) :: v7d
type(dbametaanddata),allocatable :: metaanddatav(:)
type(dbametaanddatalist) :: metaanddatal
type(dbadcv) :: attrv

integer :: category,ier,i
character(len=512):: a_name,filename="/tmp/example.bufr",filenameout="/tmp/exampleout.bufr"
!"../data/example_temp.bufr"

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name)
!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
print*,"!!                           import/export from file"
print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_dba,filename=filename,file=.true.,categoryappend="dballenewapi_read")

call l4f_category_log(category,L4F_INFO,"End data reading")

call l4f_category_log(category,L4F_INFO,"start import")

!CALL import(v7d_dba)
CALL import(v7d_dba,var=["B12101","B12102"],varkind=["r","i"],&
                   attr=["*B33196","*B33192","*B33193"],attrkind=["b","c","b"],&
                   anavar=["B12101"],anaattr=["*B33192"])

call l4f_category_log(category,L4F_INFO,"end import")

call vol7d_copy(v7d_dba%vol7d,v7d)
CALL delete (v7d_dba) 

call display(v7d)


CALL init(v7d_dba,filename=filenameout,file=.true.,write=.true.,wipe=.true.,categoryappend="dballenewapi_write")

call vol7d_copy(v7d,v7d_dba%vol7d)

call l4f_category_log(category,L4F_INFO,"start export")
CALL export(v7d_dba)
call l4f_category_log(category,L4F_INFO,"end export")

CALL delete (v7d_dba) 
CALL delete (v7d) 


Print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
print*,"!!                           import from metaanddatav"
print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

allocate(metaanddatav(2))   ! one metadata for data and one for constant data

metaanddatav(1)%metadata=dbametadata( &
  level=dbalevel(level1=103, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! create  an etherogeneous ensamble of data
allocate (metaanddatav(1)%dataattrv%dataattr(2))

! first data
allocate (metaanddatav(1)%dataattrv%dataattr(1)%dat,source=dbadatai("B13003",85))

! create an etherogeneous ensamble of attr
allocate (attrv%dcv(3))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
allocate (attrv%dcv(3)%dat,source=dbadatar("*B33194",70.))
!assemble data and attribute
metaanddatav(1)%dataattrv%dataattr(1)%attrv=attrv

! second data
allocate (metaanddatav(1)%dataattrv%dataattr(2)%dat,source=dbadatai("B12101",27315))
! create an etherogeneous ensamble of attr
deallocate(attrv%dcv)
allocate (attrv%dcv(2))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
!assemble data and attribute
metaanddatav(1)%dataattrv%dataattr(2)%attrv=attrv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! station costant data
! copy the same metadata setting that here we have constant in time data
metaanddatav(2)%metadata=metaanddatav(1)%metadata%dbacontextana()
! create  an etherogeneous ensamble of data
allocate (metaanddatav(2)%dataattrv%dataattr(2))
allocate (metaanddatav(2)%dataattrv%dataattr(1)%dat,source=dbadatai("B07030",223))
allocate (metaanddatav(2)%dataattrv%dataattr(1)%attrv%dcv(0))          ! we do not want attributes
allocate (metaanddatav(2)%dataattrv%dataattr(2)%dat,source=dbadatac("B01019","My beautifull station"))
allocate (metaanddatav(2)%dataattrv%dataattr(2)%attrv%dcv(0))          ! we do not want attributes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! display and put everythings in vol7d
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
end do

call import (v7d,metaanddatav)

call display(v7d)

!call metaanddatav%delete()
call export (v7d,metaanddatal)
call metaanddatal%display()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM v7ddballe_dba2v7d
