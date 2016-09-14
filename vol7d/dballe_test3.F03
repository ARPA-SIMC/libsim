program dballe_test3

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
call write()    ! write etherogeneous ensamble of data with attributes using macro object

! connect to dsn type BUFR file for write
sessionto=dbasession(filename="dballe_test3.bufr",wipe=.true.,write=.true.,memdb=.false.,template="generic")
call export2bufrwithfilter()

call sessionto%delete()

!close everythings
call sessionto%delete()
call session%delete()
call connection%delete()


!chiudo il logger
CALL l4f_category_delete(category)
ier=l4f_fini()

contains

subroutine write()

type(dbametaanddata),allocatable :: metaanddata(:)
type(dbadcv) :: attrv
integer :: i

print *,"----------------------------------------------"
print *,"--------------- write ------------------------"

allocate(metaanddata(1))   ! one metadata for data and one for constant data

metaanddata(1)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! allocate  data
allocate (metaanddata(1)%dataattrv%dataattr(2))

! first data
allocate (metaanddata(1)%dataattrv%dataattr(1)%dat,source=dbadatai("B13003",85))
! second data
allocate (metaanddata(1)%dataattrv%dataattr(2)%dat,source=dbadatai("B12101",27315))

! create an etherogeneous ensamble of attr
allocate (attrv%dcv(3))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
allocate (attrv%dcv(3)%dat,source=dbadatar("*B33194",70.))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(1)%attrv=attrv
metaanddata(1)%dataattrv%dataattr(2)%attrv=attrv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! display and save everythings
do i=1,size(metaanddata)
  call metaanddata(i)%display()
  !call session%extrude(metaanddata=metaanddata(i))
  call metaanddata(i)%extrude(session)
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine write


subroutine export2bufrwithfilter
type(dbametaanddata), allocatable:: metaanddatav(:)
type(dbadcv) :: vars,starvars
type(dbafilter) filter

integer :: i

print *,"----------------------------------------------"
print *,"--------------- export2bufr ------------------------"

allocate (vars%dcv(1))
allocate (vars%dcv(1)%dat,source=dbadatai("B12101"))

allocate (starvars%dcv(2))
allocate (starvars%dcv(1)%dat,source=dbadatab("*B33193"))
allocate (starvars%dcv(2)%dat,source=dbadatab("*B33194"))

filter=dbafilter()
call filter%display()
call session%ingest(metaanddatav,filter=filter)
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
  call metaanddatav(i)%extrude(sessionto)
end do


filter=dbafilter(vars=vars)
call filter%display()
call session%ingest(metaanddatav,filter=filter)
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
  call metaanddatav(i)%extrude(sessionto)
end do


filter=dbafilter(vars=vars,starvars=starvars)
call filter%display()
call session%ingest(metaanddatav,filter=filter)
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
  call metaanddatav(i)%extrude(sessionto)
end do


filter=dbafilter(starvars=starvars)
call filter%display()
call session%ingest(metaanddatav,filter=filter)
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
  call metaanddatav(i)%extrude(sessionto)
end do

end subroutine export2bufrwithfilter

end program dballe_test3
