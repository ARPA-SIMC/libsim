program example_dballe

use dballe_class
implicit none

type(dbasession) :: session
type(dbaconnection) :: connection
type(dbalevel) :: level
type(dbatimerange) :: timerange
type(dbametadata) :: metadata
type(dbadatav) :: datav
type(dbadatar),allocatable :: data(:)
integer :: i

! set and display metadata
level=dbalevel(level1=105, l1=2000)
call level%display()
timerange=dbatimerange(timerange=4, p1=3600,p2=7200)
call timerange%display()

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

!assemble metadata
metadata=dbametadata(level,timerange)
call metadata%display()

! connect to dsn
connection=dbaconnection(dsn="sqlite:/tmp/dballe.sqlite")
session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")

!clear the dballe session
call session%unsetall()

! can set metadata step by step
call level%set(session)
call timerange%set(session)
! I can use the reverse vision
call session%set(level=level)
call session%set(timerange=timerange)

!or i can set metadata one shot
call session%set(metadata)
! or in the reverse vision
call metadata%set(session)

!set data
call datav%set(session)
! or in the reverse vision
call session%set(datav=datav)

!set omogeneous data
do i =1,size(data)
  call data(i)%set(session)
  call session%set(data=data(i))
end do

!write it in dsn
call session%prendilo()
!!$session%enq(metadata)

!close everythings
call session%delete()
call connection%delete()

end program example_dballe
