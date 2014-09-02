program dballe_test

use dballe_class
use datetime_class
implicit none

type(dbasession) :: session, sessionfrom,sessionto
type(dbaconnection) :: connection ,connectionfrom

! connect to dsn type DBA
connection=dbaconnection(dsn="sqlite:dballe_test.sqlite")
session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")
call write1()    ! write etherogeneous ensamble of data with attributes and constant data using macro object
call write2()    ! write an omogeneous vector of data
call write3()    ! write an etherogeneous ensamble of data
call write4()    ! write an etherogeneous ensamble of data and attributes
call delete1()   ! delete one var from the entire DB
call delete2()   ! delete one var only where are some defined metadati
call delete3()   ! delete some attributes from one var only where are some defined metadati
!close everythings
call session%delete()
call connection%delete()


! connect to dsn type BUFR file for write
session=dbasession(filename="dballe_test.bufr",wipe=.true.,write=.true.,memdb=.false.)
call write0()    ! write ana on file
call write1()    ! write etherogeneous ensamble of data with attributes and constant data using macro object
call write2()    ! write an omogeneous vector of data
call write3()    ! write an etherogeneous ensamble of data
call write4()    ! write an etherogeneous ensamble of data and attributes
!close everythings
call session%delete()


! connect to dsn type BUFR file for read
sessionfrom=dbasession(filename="dballe_test.bufr",memdb=.false.)
! connect to dsn type BUFR file for write
sessionto=dbasession(filename="dballe_test_copy1f.bufr",wipe=.true.,write=.true.,memdb=.false.)
call copy1f()     ! copy data and attributes of everythings to an other file
!close everythings
call sessionto%delete()
call sessionfrom%delete()

! use memdb
! connect to dsn type DBA
connectionfrom=dbaconnection(dsn="mem:")
sessionfrom=dbasession(connectionfrom,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")
call sessionfrom%messages_open_input(filename="dballe_test.bufr",mode="r",format="BUFR",simplified=.true.)
! connect to dsn type BUFR file for write
sessionto=dbasession(filename="dballe_test_copy1fmem.bufr",wipe=.true.,write=.true.,memdb=.false.)
call copy1fmem()     ! copy data and attributes of everythings to an other file
!close everythings
call sessionfrom%delete()
call connectionfrom%delete()
call sessionto%delete()

contains

subroutine write0()

type(dbasession) :: sessionana
type(dbaanalist) :: anal
type(dbaana) :: ana
logical :: status

! connect to dsn type BUFR file for write
sessionana=dbasession(filename="dballe_test_ana.bufr",wipe=.true.,write=.true.,memdb=.false.)


call anal%append(dbaana(lon=11.d0,lat=45.d0))
call anal%append(dbaana(lon=12.d0,lat=45.d0))
call anal%append(dbaana(lon=13.d0,lat=45.d0))

call anal%display()

call anal%rewind()
!extrude ana
do while (anal%element())
  !call sessionana%extrude(ana=anal%current())
  ana=anal%current()
  call ana%extrude(sessionana)
  call anal%next()
end do

call sessionana%delete()
status=anal%delete()

end subroutine write0


subroutine write1()

type(dbametaanddata),allocatable :: metaanddata(:)
type(dbadcv) :: attrv
integer :: i

print *,"----------------------------------------------"
print *,"--------------- write1 ------------------------"

allocate(metaanddata(2))   ! one metadata for data and one for constant data

metaanddata(1)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! create  an etherogeneous ensamble of data
allocate (metaanddata(1)%dataattrv%dataattr(2))

! first data
allocate (metaanddata(1)%dataattrv%dataattr(1)%dat,source=dbadatai("B13003",85))

! create an etherogeneous ensamble of attr
allocate (attrv%dcv(3))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
allocate (attrv%dcv(3)%dat,source=dbadatar("*B33194",70.))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(1)%attrv=attrv

! second data
allocate (metaanddata(1)%dataattrv%dataattr(2)%dat,source=dbadatai("B12101",27315))
! create an etherogeneous ensamble of attr
deallocate(attrv%dcv)
allocate (attrv%dcv(2))
allocate (attrv%dcv(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dcv(2)%dat,source=dbadatai("*B33193",50))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(2)%attrv=attrv
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! station costant data
! copy the same metadata setting that here we have constant in time data
metaanddata(2)%metadata=metaanddata(1)%metadata%dbacontextana()
! create  an etherogeneous ensamble of data
allocate (metaanddata(2)%dataattrv%dataattr(2))
allocate (metaanddata(2)%dataattrv%dataattr(1)%dat,source=dbadatai("B07030",223))
allocate (metaanddata(2)%dataattrv%dataattr(1)%attrv%dcv(0))          ! we do not want attributes
allocate (metaanddata(2)%dataattrv%dataattr(2)%dat,source=dbadatac("B01019","My beautifull station"))
allocate (metaanddata(2)%dataattrv%dataattr(2)%attrv%dcv(0))          ! we do not want attributes
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


subroutine write2()

type(dbadatar),allocatable :: data(:)

type(dbalevel) :: level
type(dbatimerange) :: timerange
type(dbaana) :: ana
type(dbanetwork) :: network
type(dbadatetime) :: datetime
integer :: i

print *,"----------------------------------------------"
print *,"--------------- write2 ------------------------"


!clear the dballe session
call session%unsetall()

! set and display metadata
level=dbalevel(level1=105, l1=2000)
call level%display()
timerange=dbatimerange(timerange=4, p1=3600,p2=7200)
call timerange%display()
!ana=dbaana(coord=dbacoord(ilon=1000000,ilat=4500000))
ana=dbaana(lon=11.d0,lat=45.d0)
call ana%display()
network=dbanetwork("generic")
call network%display()
datetime=dbadatetime(datetime_new(2014,01,06,18,00))
call datetime%display()

! can set metadata  step by step
call session%set(level=level)
call session%set(timerange=timerange)
call session%set(ana=ana)
call session%set(network=network)
call session%set(datetime=datetime)

! I can use the reverse vision  step by step
!call level%dbaset(session)
!call timerange%dbaset(session)
!call ana%dbaset(session)
!call network%dbaset(session)
!call datetime%dbaset(session)

! create an omogeneous vector of data
allocate (data(2),source=[dbadatar("B12102",265.33),dbadatar("B12101",273.15)])

!set and display omogeneous data
do i =1,size(data)
  call data(i)%display()
  call session%set(data=data(i))
  !call data(i)%dbaset(session)
end do

!write it in dsn or file
call session%prendilo()
!!$session%enq(metadata)

!close message if I am writing on file
call session%close_message()


end subroutine write2


subroutine write3()

type(dbametadata) :: metadata
type(dbadcv) :: datav
type(dbalevel) :: level
type(dbatimerange) :: timerange
type(dbaana) :: ana
type(dbanetwork) :: network
type(dbadatetime) :: datetime

print *,"----------------------------------------------"
print *,"--------------- write3 ------------------------"


!clear the dballe session
call session%unsetall()

! set metadata
level=dbalevel(level1=105, l1=2000)
timerange=dbatimerange(timerange=4, p1=3600,p2=7200)
ana=dbaana(lon=12.d0,lat=45.d0)
network=dbanetwork("generic")
datetime=dbadatetime(datetime_new(2014,01,06,18,00))

!assemble metadata
metadata=dbametadata(level=level,timerange=timerange,ana=ana,network=network,datetime=datetime)
call metadata%display()

! I can set metadata one shot
call session%set(metadata)
! or in the reverse vision
!call metadata%dbaset(session)

call metadata%display()

! create and display an etherogeneous ensamble of data
allocate (datav%dcv(2))
allocate (datav%dcv(1)%dat,source=dbadatai("B12102",26312))
allocate (datav%dcv(2)%dat,source=dbadatar("B12101",273.15))
call datav%display()
!set data
call session%set(datav=datav)

! or in the reverse vision
!call datav%dbaset(session)

!write it in dsn
call session%prendilo()
!!$session%enq(metadata)

!close message if I am writing on file
call session%close_message()

end subroutine write3


subroutine write4()

type(dbametadata) :: metadata
type(dbadataattrv) :: dataattrv
type(dbadcv) :: attrv
type(dbadcv) :: datav

print *,"----------------------------------------------"
print *,"--------------- write4 ------------------------"

! clear the dballe session
call session%unsetall()

! define metadata
metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=13.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

call session%set(metadata)

! create and display an etherogeneous ensamble of data
allocate (datav%dcv(2))
allocate (datav%dcv(1)%dat,source=dbadatai("B12102",26312))
allocate (datav%dcv(2)%dat,source=dbadatar("B12101",273.15))

! create and display an etherogeneous ensamble of attr
allocate (attrv%dcv(3))
allocate (attrv%dcv(1)%dat,source=dbadatai("*B33192",30))
allocate (attrv%dcv(2)%dat,source=dbadatac("*B33193","70"))
allocate (attrv%dcv(3)%dat,source=dbadatad("*B33194",50.d0))
call attrv%display()

! assemble data and attribute
allocate (dataattrv%dataattr(2))
! first with attribute
allocate (dataattrv%dataattr(1)%dat,source=datav%dcv(1)%dat)
dataattrv%dataattr(1)%attrv=attrv
! second without attribute
allocate (dataattrv%dataattr(2)%dat,source=datav%dcv(2)%dat)
allocate (dataattrv%dataattr(2)%attrv%dcv(0))

call dataattrv%display()

! write data and attribute
!call session%extrude(dataattrv=dataattrv)
call dataattrv%extrude(session)

! work on constant station data
call session%set(metadata%dbacontextana())

! write the same data and attribute as constant station data
!call session%extrude(dataattrv=dataattrv)
call dataattrv%extrude(session)

end subroutine write4


subroutine delete1()

type(dbafilter) :: filter

print *,"----------------------------------------------"
print *,"--------------- delete1 ----------------------"

filter=dbafilter(var="B12101")
call session%set(filter=filter)
call session%dissolve()

end subroutine delete1


subroutine delete2()

type(dbametadata),allocatable :: metadata(:)
type(dbafilter) :: filter

print *,"----------------------------------------------"
print *,"--------------- delete2 ----------------------"


allocate(metadata(1))

metadata(1)=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=11.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))


filter=dbafilter(var="B12102")
call session%set(filter=filter)
call session%dissolve(metadata)

deallocate(metadata)

end subroutine delete2



subroutine delete3()

type(dbametadata),allocatable :: metadata(:)
type(dbafilter) :: filter

print *,"----------------------------------------------"
print *,"--------------- delete3 ----------------------"


allocate(metadata(1))

metadata(1)=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=13.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

filter=dbafilter(var="B12102",starvarlist="*B33194,*B33193")
call session%set(filter=filter)
call session%dissolveattr(metadata)

deallocate(metadata)

end subroutine delete3


subroutine copy1f()
type(dbametaanddata), allocatable:: metaanddatav(:)
integer :: i

print *,"----------------------------------------------"
print *,"--------------- copy1f ----------------------"

! data
call sessionfrom%filerewind()

call sessionfrom%set(filter=dbafilter())
call sessionfrom%ingest(metaanddatav)
do while (size(metaanddatav) >0)
  print*,"read/write data; count: ",sessionfrom%count
  do i =1,size(metaanddatav)
    print *, "display metaanddatav index: ", i
!    call metaanddatav(i)%display()
    !call sessionto%extrude(metaanddata=metaanddatav(i))
    call metaanddatav(i)%extrude(sessionto)
  end do
!  call sessionto%close_message()
  call sessionfrom%ingest(metaanddatav)
end do
deallocate (metaanddatav)

! constant data
call sessionfrom%filerewind()

call sessionfrom%set(filter=dbafilter(contextana=.true.))
call sessionfrom%ingest(metaanddatav)
do while (size(metaanddatav) >0)
  print*,"read/write data; count: ",sessionfrom%count
  do i =1,size(metaanddatav)
    print *, "display metaanddatav index: ", i
!    call metaanddatav(i)%display()
    !call sessionto%extrude(metaanddata=metaanddatav(i))
    call metaanddatav(i)%extrude(sessionto)
  end do
!  call sessionto%close_message()
  call sessionfrom%ingest(metaanddatav)
end do

end subroutine copy1f


subroutine copy1fmem()
type(dbametaanddata),allocatable :: metaanddatav(:)

print *,"----------------------------------------------"
print *,"--------------- copy1fmem ----------------------"


do while (sessionfrom%messages_read_next())
!!$  print*,"read/write message"
!!$  call sessionfrom%set(filter=dbafilter())
!!$  call sessionfrom%ingest(metaanddatav)
!!$  do i =1,size(metaanddatav)
!!$    call metaanddatav(i)%display()
!!$    call sessionto%extrude(metaanddata=metaanddatav(i))
!!$  end do
!!$  call sessionto%prendilo()

  print *,"contextana"
  call sessionfrom%set(filter=dbafilter(contextana=.true.))
  call sessionfrom%ingest(metaanddatav)
!!$  do i =1,size(metaanddatav)
!!$    call metaanddatav(i)%display()
!!$    call sessionto%extrude(metaanddata=metaanddatav(i))
!!$  end do
!!$  call sessionto%close_message()
  call sessionfrom%remove_all()
end do

end subroutine copy1fmem

end program dballe_test
