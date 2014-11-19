program dbatransform

use dballe_class
use vol7d_ana_class
use datetime_class

implicit none

type(dbasession) :: session
type(dbametaanddatar),allocatable :: metaanddatar(:)
type(dbafilter) :: filter
type(dbadcv) :: vars
integer :: i
double precision :: lat,lon
integer :: year, month, day, hour, minute, msec
real :: val

! create mem db and import bufr from file
session=dbasession(anaflag="write", dataflag="write", attrflag="write",&
filename="/home/ppatruno/tmp/rmap.bufr")

! create filter for real data
allocate (vars%dcv(2))
allocate (vars%dcv(1)%dat,source=dbadatar("B12101"))
allocate (vars%dcv(2)%dat,source=dbadatar("B13003"))
filter=dbafilter(vars=vars)

call filter%dbaset(session)
! get data
call session%ingest(metaanddatar)

do i =1, size(metaanddatar)
!  call metaanddatav(i)%metadata%datetime%display()
!  call metaanddatav(i)%metadata%level%display()
!  call metaanddatav(i)%dataattrv%dataattr(j)%dat%display()
  if(metaanddatar(i)%dbadatar%btable == "B13003" .and. metaanddatar(i)%dbadatar%c_e()) then
    call metaanddatar(i)%dbadatar%get(val)
    call getval(metaanddatar(i)%metadata%datetime%datetime,year=year, month=month, day=day, hour=hour, minute=minute, msec=msec)
    print *,"date:",year, month, day, hour, minute, msec/1000.
    print *,"level 1:",metaanddatar(i)%metadata%level%vol7d_level%l1
    lon=getlon(metaanddatar(i)%metadata%ana%coord)
    lat=getlat(metaanddatar(i)%metadata%ana%coord)
    print *,"coord:",lat,lon
    print *,"humidity", val
  else
    print *,"btable:",metaanddatar(i)%dbadatar%btable
  end if
end do

deallocate (metaanddatar)

!close everythings
call session%delete()

end program dbatransform
