#include "config.h"

program dballe_test

use dballe_class
use datetime_class
use log4fortran
use missing_values
implicit none

type(dbasession) :: session, sessionto
type(dbaconnection) :: connection
integer :: ier, category
CHARACTER(len=512) :: a_name

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="dballe_test")
!init di log4fortran
ier=l4f_init()
!imposta a_name
category=l4f_category_get(a_name//".main")

! connect to dsn type DBA
connection=dbaconnection(dsn="sqlite:dballe_test.sqlite")
session=dbasession(connection,wipe=.true.,write=.true.)
call write1()    ! write etherogeneous ensamble of data with attributes and constant data using macro object
call delete3()   ! delete some attributes from one var only where are some defined metadati
call writeattronly()   ! append some attributes to station 1


! connect to dsn type BUFR file for write
sessionto=dbasession(filename="dballe_test2.bufr",wipe=.true.,write=.true.,memdb=.false.,template="generic")
call export2bufr()

# ifndef F2003_FULL_FEATURES
call sessionto%delete()
#endif

! connect to dsn type BUFR file for write with memdb 
sessionto=dbasession(filename="dballe_test2_memdb.bufr",wipe=.true.,write=.true.,memdb=.true.,template="generic")
call export2bufr()


# ifndef F2003_FULL_FEATURES
!close everythings
call sessionto%delete()
call session%delete()
call connection%delete()
#endif

!chiudo il logger
CALL l4f_category_delete(category)
ier=l4f_fini()

contains

subroutine write1()

type(dbametaanddata),allocatable :: metaanddata(:)
type(dbadcv) :: attrv
integer :: i

print *,"----------------------------------------------"
print *,"--------------- write1 ------------------------"

allocate(metaanddata(5))   ! one metadata for data and one for constant data

metaanddata(1)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

metaanddata(2)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=11.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

metaanddata(3)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=12.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

metaanddata(4)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=13.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

metaanddata(5)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=14.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! create  an etherogeneous ensamble of data
allocate (metaanddata(1)%dataattrv%dataattr(1))

! first data
allocate (metaanddata(1)%dataattrv%dataattr(1)%dat,source=dbadatai("B13003",85))

! create an etherogeneous ensamble of attr
allocate (attrv%dcv(3))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
allocate (attrv%dcv(3)%dat,source=dbadatar("*B33194",70.))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(1)%attrv=attrv

! second station
allocate (metaanddata(2)%dataattrv%dataattr(1))

allocate (metaanddata(2)%dataattrv%dataattr(1)%dat,source=dbadatai("B12101",27315))
! create an etherogeneous ensamble of attr
deallocate(attrv%dcv)
allocate (attrv%dcv(2))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
!assemble data and attribute
metaanddata(2)%dataattrv%dataattr(1)%attrv=attrv


! 3d station with all data and attributes missing
allocate (metaanddata(3)%dataattrv%dataattr(1))

allocate (metaanddata(3)%dataattrv%dataattr(1)%dat,source=dbadatai("B12101",imiss))
! create an etherogeneous ensamble of attr
deallocate(attrv%dcv)
allocate (attrv%dcv(1))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",rmiss))
!assemble data and attribute
metaanddata(3)%dataattrv%dataattr(1)%attrv=attrv


! 4d station with  data present and attributes missing
allocate (metaanddata(4)%dataattrv%dataattr(1))

allocate (metaanddata(4)%dataattrv%dataattr(1)%dat,source=dbadatai("B12101",27315))
! create an etherogeneous ensamble of attr
deallocate(attrv%dcv)
allocate (attrv%dcv(1))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",rmiss))
!assemble data and attribute
metaanddata(4)%dataattrv%dataattr(1)%attrv=attrv



! ! 5d station with  data invalid (Value -5 is outside the range [0,126] for 013003 (RELATIVE HUMIDITY) ) and attribute
allocate (metaanddata(5)%dataattrv%dataattr(1))

! first data
allocate (metaanddata(5)%dataattrv%dataattr(1)%dat,source=dbadatai("B13003",-5))

! create an attr
deallocate (attrv%dcv)
allocate (attrv%dcv(1))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
!assemble data and attribute
metaanddata(5)%dataattrv%dataattr(1)%attrv=attrv


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! display and save everythings
do i=1,size(metaanddata)
  call metaanddata(i)%display()
  !call session%extrude(metaanddata=metaanddata(i))
  call metaanddata(i)%extrude(session)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine write1



subroutine delete3()

type(dbametadata),allocatable :: metadata(:)
type(dbafilter) :: filter

print *,"----------------------------------------------"
print *,"--------------- delete ----------------------"


allocate(metadata(1))

metadata(1)=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

filter=dbafilter(var="B13003",starvarlist="*B33193,*B33194")
call session%set(filter=filter)
call session%dissolveattr(metadata)

deallocate(metadata)

end subroutine delete3

subroutine writeattronly()

type(dbametaanddata),allocatable :: metaanddata(:)
type(dbadcv) :: attrv
integer :: i

print *,"----------------------------------------------"
print *,"--------------- writeattronly ------------------------"

allocate(metaanddata(1))   ! one metadata for data and one for constant data

metaanddata(1)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=11.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! create  an etherogeneous ensamble of data
allocate (metaanddata(1)%dataattrv%dataattr(1))

! first data
allocate (metaanddata(1)%dataattrv%dataattr(1)%dat,source=dbadatai("B12101",28315))

! create an etherogeneous ensamble of attr
allocate (attrv%dcv(2))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33193"))
allocate (attrv%dcv(2)%dat,source=dbadatar("*B33194",40.))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(1)%attrv=attrv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! display and save everythings
do i=1,size(metaanddata)
  call metaanddata(i)%display()
  !call session%extrude(metaanddata=metaanddata(i))
  call metaanddata(i)%extrude(session,attronly=.true.)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine writeattronly

subroutine export2bufr()
type(dbametaanddata), allocatable:: metaanddatav(:)
integer :: i

print *,"----------------------------------------------"
print *,"--------------- export2bufr ------------------------"

call session%ingest(metaanddatav)

do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
  !call sessionto%extrude(metaanddata=metaanddatav(i))
  call metaanddatav(i)%extrude(sessionto)
  if (sessionto%memdb) then
    call sessionto%messages_write_next("generic")
                                !clean memdb
    call sessionto%remove_all()
  endif
end do

end subroutine export2bufr


end program dballe_test
