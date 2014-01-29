program example_dballe

use dballe_class
use datetime_class
implicit none

integer :: i
type(dbasession) :: session
type(dbaconnection) :: connection

! connect to dsn
connection=dbaconnection(dsn="sqlite:/tmp/dballe.sqlite")
session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

call write1()    ! write etherogeneous ensamble of data with attributes and constant data using macro object
call write2()    ! write an omogeneous vector of data
call write3()    ! write an etherogeneous ensamble of data
call write4()    ! write an etherogeneous ensamble of data and attributes
call delete1()   ! delete one var from the entire DB
call delete2()   ! delete one var only where are some defined metadati
call delete3()   ! delete some attributes from one var only where are some defined metadati
call read1()     ! read ana, data and attributes for constant data, data and attributes for data
call read2()     ! read an omogeneous vector of data
call copy1()     ! copy data and attributes of everythings to an other dsn
call copy2()     ! copy data and constant data to an other dsn

!close everythings
call session%delete()
call connection%delete()

contains

subroutine write1()

type(dbametaanddata),allocatable :: metaanddata(:)
type(dbadatav) :: attrv

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
allocate (attrv%dat(3))
allocate (attrv%dat(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dat(2)%dat,source=dbadatai("*B33193",50))
allocate (attrv%dat(3)%dat,source=dbadatar("*B33194",70.))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(1)%attrv=attrv

! second data
allocate (metaanddata(1)%dataattrv%dataattr(2)%dat,source=dbadatai("B12101",27315))
! create an etherogeneous ensamble of attr
deallocate(attrv%dat)
allocate (attrv%dat(2))
allocate (attrv%dat(1)%dat,source=dbadatar("*B33192",30.))
allocate (attrv%dat(2)%dat,source=dbadatai("*B33193",50))
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
allocate (metaanddata(2)%dataattrv%dataattr(1)%attrv%dat(0))          ! we do not want attributes
allocate (metaanddata(2)%dataattrv%dataattr(2)%dat,source=dbadatac("B01019","My beautifull station"))
allocate (metaanddata(2)%dataattrv%dataattr(2)%attrv%dat(0))          ! we do not want attributes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! display and save everythings
do i=1,size(metaanddata)
  call metaanddata(i)%display()
  call session%ingest(metaanddata=metaanddata(i))
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

!write it in dsn
call session%prendilo()
!!$session%enq(metadata)

end subroutine write2


subroutine write3()

type(dbametadata) :: metadata
type(dbadatav) :: datav
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
allocate (datav%dat(2))
allocate (datav%dat(1)%dat,source=dbadatai("B12102",26312))
allocate (datav%dat(2)%dat,source=dbadatar("B12101",273.15))
call datav%display()
!set data
call session%set(datav=datav)

! or in the reverse vision
!call datav%dbaset(session)

!write it in dsn
call session%prendilo()
!!$session%enq(metadata)


end subroutine write3


subroutine write4()

type(dbametadata) :: metadata
type(dbadataattrv) :: dataattrv
type(dbadatav) :: attrv
type(dbadatav) :: datav

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
allocate (datav%dat(2))
allocate (datav%dat(1)%dat,source=dbadatai("B12102",26312))
allocate (datav%dat(2)%dat,source=dbadatar("B12101",273.15))

! create and display an etherogeneous ensamble of attr
allocate (attrv%dat(3))
allocate (attrv%dat(1)%dat,source=dbadatai("*B33192",30))
allocate (attrv%dat(2)%dat,source=dbadatac("*B33193","70"))
allocate (attrv%dat(3)%dat,source=dbadatad("*B33194",50.d0))
call attrv%display()

! assemble data and attribute
allocate (dataattrv%dataattr(2))
! first with attribute
allocate (dataattrv%dataattr(1)%dat,source=datav%dat(1)%dat)
dataattrv%dataattr(1)%attrv=attrv
! second without attribute
allocate (dataattrv%dataattr(2)%dat,source=datav%dat(2)%dat)
allocate (dataattrv%dataattr(2)%attrv%dat(0))

call dataattrv%display()

! write data and attribute
call session%ingest(dataattrv=dataattrv)


! work on constant station data
call session%set(metadata%dbacontextana())

! write the same data and attribute as constant station data
call session%ingest(dataattrv=dataattrv)

end subroutine write4

subroutine read1()

type(dbametaanddata),allocatable :: metaanddatav(:)
type(dbaana),allocatable :: ana(:)
type(dbafilter) :: filter
integer :: i

print *,"----------------------------------------------"
print *,"--------------- read1 ------------------------"

call session%set(filter=dbafilter())

print *, "anagrafica:"
filter=dbafilter(coordmin=dbacoord(lon= 9.d0,lat=44.d0),&
                 coordmax=dbacoord(lon=11.d0,lat=46.d0))
call session%set(filter=filter)
call session%extrude(ana)
do i=1,size(ana)
  call ana(i)%display()
end do
deallocate(ana)

print *, "dati di anagrafica:"
call session%set(filter=dbafilter(contextana=.true.))
call session%extrude(metaanddatav)
do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
end do
deallocate(metaanddatav)

print *, "dati dati:"
filter=dbafilter(var="B12101")
call filter%display()
call session%set(filter=filter)
call session%extrude(metaanddatav)

do i=1,size(metaanddatav)
  call metaanddatav(i)%display()
end do

deallocate(metaanddatav)

end subroutine read1

subroutine read2()

type(dbametaanddatar),allocatable :: metaanddatarv(:)
type(dbafilter) :: filter
integer :: i

print *,"----------------------------------------------"
print *,"--------------- read2 ------------------------"

print *, "dati dati:"
filter=dbafilter(var="B12102")
call filter%display()
call session%set(filter=filter)
call session%extrude(metaanddatarv)

do i=1,size(metaanddatarv)
  call metaanddatarv(i)%display()
end do

print *,"max=",maxval(metaanddatarv(:)%dbadatar%value)
print *,"livelli", metaanddatarv(:)%metadata%level

deallocate(metaanddatarv)

end subroutine read2


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

subroutine copy1()
type(dbasession)    :: sessioncp
type(dbaconnection) :: connectioncp
type(dbametaanddata):: metaanddata

print *,"----------------------------------------------"
print *,"--------------- copy1 ----------------------"

! connect to dsn
connectioncp=dbaconnection(dsn="sqlite:/tmp/dballecopy1.sqlite")
sessioncp=dbasession(connectioncp,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

! data
call session%set(filter=dbafilter())
call session%extrude_metaanddata()
do while (session%count >0)
  call session%extrude_metaanddata (metaanddata)
  call sessioncp%ingest(metaanddata=metaanddata)
end do

! constant data
call session%set(filter=dbafilter(contextana=.true.))
call session%extrude_metaanddata()
do while (session%count >0)
  call session%extrude_metaanddata (metaanddata)
  call sessioncp%ingest(metaanddata=metaanddata)
end do


!close everythings
call sessioncp%delete()
call connectioncp%delete()

end subroutine copy1


subroutine copy2()
type(dbasession)    :: sessioncp
type(dbaconnection) :: connectioncp
type(dbametaanddatac):: metaanddatac

print *,"----------------------------------------------"
print *,"--------------- copy2 ----------------------"

! connect to dsn
connectioncp=dbaconnection(dsn="sqlite:/tmp/dballecopy2.sqlite")
sessioncp=dbasession(connectioncp,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

! use character type

! data
call session%set(filter=dbafilter())
call session%extrude_metaanddatac()
do while (session%count >0)
  call session%extrude_metaanddatac (metaanddatac)
  call sessioncp%ingest(metaanddatac=metaanddatac)
end do

! constant data
call session%set(filter=dbafilter(contextana=.true.))
call session%extrude_metaanddatac()
do while (session%count >0)
  call session%extrude_metaanddatac (metaanddatac)
  call sessioncp%ingest(metaanddatac=metaanddatac)
end do


!close everythings
call sessioncp%delete()
call connectioncp%delete()

end subroutine copy2

end program example_dballe
