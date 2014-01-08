program example_dballe

use dballe_class
use datetime_class
implicit none

type(dbasession) :: session
type(dbaconnection) :: connection
!type(dbalevel) :: level
!type(dbatimerange) :: timerange
!type(dbaana) :: ana
!type(dbanetwork) :: network
!type(dbadatetime) :: dt

type(dbametadata) :: metadata
type(dbadatav) :: datav
type(dbadatar),allocatable :: data(:)
integer :: i

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
 ,ana=dbaana(lon=10.d0,lat=45.d0) &
 ,network=dbanetwork("generic") &
 ,datetime=dbadatetime(datetime_new(2014,01,06,18,00)))

call metadata%display()

! create and display an etherogeneous ensamble of data
allocate (datav%datac(2))
allocate (datav%datac(1)%var,source=dbadatai("B12102",85))
allocate (datav%datac(2)%var,source=dbadatar("B12101",273.15))
call datav%display()

! create and display an omogeneous vector of data
allocate (data(2),source=[dbadatar("B12102",85.),dbadatar("B12101",273.15)])
do i =1,size(data)
  call data(i)%display()
end do

! connect to dsn
connection=dbaconnection(dsn="sqlite:/tmp/dballe.sqlite")
session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

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

!close everythings
call session%delete()
call connection%delete()

end program example_dballe
