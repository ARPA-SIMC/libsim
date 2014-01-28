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

call write1()
call write2()
call read1()
call read2()
call delete1()
call delete2()

!close everythings
call session%delete()
call connection%delete()

contains

subroutine write1()

type(dbametaanddata),allocatable :: metaanddata(:)
type(dbadatav) :: attrv

print *,"----------------------------------------------"
print *,"--------------- write1 ------------------------"

allocate(metaanddata(nmetaanddata))

metaanddata(1)%metadata=dbametadata( &
  level=dbalevel(level1=105, l1=2000) &
 ,timerange=dbatimerange(timerange=4, p1=3600,p2=7200) &
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))


! create an etherogeneous ensamble of attr
allocate (attrv%dat(nattr))
allocate (attrv%dat(1)%dat,source=dbadatai("*B33192",30))
allocate (attrv%dat(2)%dat,source=dbadatai("*B33193",70))
allocate (attrv%dat(3)%dat,source=dbadatar("*B33194",50.))

! create  an etherogeneous ensamble of data
allocate (metaanddata(1)%dataattrv%dataattr(ndata))
allocate (metaanddata(1)%dataattrv%dataattr(1)%dat,source=dbadatai("B12102",85))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(1)%attrv=attrv

allocate (metaanddata(1)%dataattrv%dataattr(2)%dat,source=dbadatai("B12101",27315))
!assemble data and attribute
metaanddata(1)%dataattrv%dataattr(2)%attrv=attrv


! station costant data
metaanddata(2)%metadata=metaanddata(1)%metadata%dbacontextana()
! create  an etherogeneous ensamble of data
allocate (metaanddata(2)%dataattrv%dataattr(ndata))
allocate (metaanddata(2)%dataattrv%dataattr(1)%dat,source=dbadatai("B12102",85))
allocate (metaanddata(2)%dataattrv%dataattr(1)%attrv%dat(0))
allocate (metaanddata(2)%dataattrv%dataattr(2)%dat,source=dbadatar("B12101",273.15))
allocate (metaanddata(2)%dataattrv%dataattr(2)%attrv%dat(0))

do i=1,nmetaanddata
  call metaanddata(i)%display()
  call session%ingest(metaanddata=metaanddata(i))
end do


end subroutine write1


subroutine write2()

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

print *,"----------------------------------------------"
print *,"--------------- write2 ------------------------"

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
allocate (datav%dat(1)%dat,source=dbadatai("B12102",85))
allocate (datav%dat(2)%dat,source=dbadatar("B12101",273.15))
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
allocate (attrv%dat(1)%dat,source=dbadatai("*B33192",30))
allocate (attrv%dat(2)%dat,source=dbadatac("*B33193","70"))
allocate (attrv%dat(3)%dat,source=dbadatad("*B33194",50.d0))
call attrv%display()

!assemble data and attribute
allocate (dataattrv%dataattr(ndata))
do i=1,ndata
  allocate (dataattrv%dataattr(i)%dat,source=data(i))
  dataattrv%dataattr(i)%attrv=attrv
end do
call dataattrv%display()

!write data and attribute
call session%ingest(dataattrv=dataattrv)

call session%set(metadata%dbacontextana())

!write ana data and attribute
call session%ingest(dataattrv=dataattrv)

end subroutine write2

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

print *,"livelli", metaanddatav(:)%metadata%level

deallocate(metaanddatav)

end subroutine read1

subroutine read2()

type(dbametaanddatar),allocatable :: metaanddatarv(:)
type(dbafilter) :: filter
integer :: i

print *,"----------------------------------------------"
print *,"--------------- read2 ------------------------"

print *, "dati dati:"
filter=dbafilter(var="B12101")
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
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))


filter=dbafilter(var="B12101")
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
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))


filter=dbafilter(var="B12101",starvarlist="*B33194,*B33193")
call session%set(filter=filter)
call session%dissolveattr(metadata)

deallocate(metadata)

end subroutine delete3


end program example_dballe
