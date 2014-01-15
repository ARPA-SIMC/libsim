program example_dballe

use dballe_class
use datetime_class
implicit none

integer, parameter :: ndata=2,nattr=3,nmetaanddata=2
integer :: i
type(dbasession) :: session
type(dbaconnection) :: connection

! connect to dsn
connection=dbaconnection(dsn="sqlite:/tmp/dballe.sqlite")
session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

call example1()
call example2()

!close everythings
call session%delete()
call connection%delete()

contains

subroutine example1()

type(dbametaanddata),allocatable :: metaanddata(:)
type(dbadatav) :: attrv


allocate(metaanddata(nmetaanddata))

metaanddata(1)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))


! create an etherogeneous ensamble of attr
allocate (attrv%dat(nattr))
allocate (attrv%dat(1)%data,source=dbadatai("*B33192",30))
allocate (attrv%dat(2)%data,source=dbadatai("*B33193",70))
allocate (attrv%dat(3)%data,source=dbadatar("*B33194",50.))

! create  an etherogeneous ensamble of data
allocate (metaanddata(1)%dataattrv%dataattr(ndata))
allocate (metaanddata(1)%dataattrv%dataattr(1)%data,source=dbadatai("B12102",85))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(1)%attrv=attrv

allocate (metaanddata(1)%dataattrv%dataattr(2)%data,source=dbadatai("B12101",27315))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(2)%attrv=attrv


! station costant data
metaanddata(2)%metadata=metaanddata(1)%metadata%dbacontextana()
! create  an etherogeneous ensamble of data
allocate (metaanddata(2)%dataattrv%dataattr(ndata))
allocate (metaanddata(2)%dataattrv%dataattr(1)%data,source=dbadatai("B12102",85))
allocate (metaanddata(2)%dataattrv%dataattr(1)%attrv%dat(0))
allocate (metaanddata(2)%dataattrv%dataattr(2)%data,source=dbadatar("B12101",273.15))
allocate (metaanddata(2)%dataattrv%dataattr(2)%attrv%dat(0))

do i=1,nmetaanddata
  call metaanddata(i)%display()
  call session%ingest(metaanddata=metaanddata(i))
end do


end subroutine example1


subroutine example2()

!type(dbalevel) :: level
!type(dbatimerange) :: timerange
!type(dbaana) :: ana
!type(dbanetwork) :: network
!type(dbadatetime) :: dt

type(dbametadata) :: metadata
type(dbadatav) :: datav
type(dbadatar),allocatable :: data(:)
type(dbadataattrv) :: dataattrv
type(dbadatav) :: attrv

!!$! set and display metadata
!!$level=dbalevel(level1=105, l1=2000)
!!$call level%display()
!!$timerange=dbatimerange(timerange=4, p1=3600,p2=7200)
!!$call timerange%display()
!!$!ana=dbaana(coord=dbacoord(ilon=1000000,ilat=4500000))
!!$ana=dbaana(lon=10.d0,lat=45.d0)
!!$call ana%display()
!!$network=dbanetwork("generic")
!!$call network%display()
!!$dt=dbadatetime(datetime_new(2014,01,06,18,00))
!!$call dt%display()


!assemble metadata
!metadata=dbametadata(level=level,timerange=timerange,ana=ana,network=network,datetime=dt)

metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=11.d0,lat=46.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

call metadata%display()

! create and display an etherogeneous ensamble of data
allocate (datav%dat(ndata))
allocate (datav%dat(1)%data,source=dbadatai("B12102",85))
allocate (datav%dat(2)%data,source=dbadatar("B12101",273.15))
call datav%display()

! create and display an omogeneous vector of data
allocate (data(ndata),source=[dbadatar("B12102",85.),dbadatar("B12101",273.15)])
do i =1,size(data)
  call data(i)%display()
end do

!clear the dballe session
call session%unsetall()

! can set metadata  step by step
!call session%set(level=level)
!call session%set(timerange=timerange)

! I can use the reverse vision  step by step
!call level%dbaset(session)
!call timerange%dbaset(session)


!or I can set metadata one shot
call session%set(metadata)

! or in the reverse vision
!call metadata%dbaset(session)

!set data
call session%set(datav=datav)

! or in the reverse vision
!call datav%dbaset(session)


!set omogeneous data
do i =1,size(data)
  call session%set(data=data(i))
  !call data(i)%dbaset(session)
end do

!write it in dsn
call session%prendilo()
!!$session%enq(metadata)

! create and display an etherogeneous ensamble of attr
allocate (attrv%dat(nattr))
allocate (attrv%dat(1)%data,source=dbadatai("*B33192",30))
allocate (attrv%dat(2)%data,source=dbadatac("*B33193","70"))
allocate (attrv%dat(3)%data,source=dbadatad("*B33194",50.d0))
call attrv%display()

!assemble data and attribute
allocate (dataattrv%dataattr(ndata))
do i=1,ndata
  allocate (dataattrv%dataattr(i)%data,source=data(i))
  dataattrv%dataattr(i)%attrv=attrv
end do
call dataattrv%display()

!write data and attribute
call session%ingest(dataattrv=dataattrv)

call session%set(metadata%dbacontextana())

!write ana data and attribute
call session%ingest(dataattrv=dataattrv)

end subroutine example2

end program example_dballe
