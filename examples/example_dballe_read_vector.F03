program dbatransform

use dballe_class
use vol7d_ana_class
use datetime_class

implicit none

type(dbasession) :: session
type(dbametaanddata),allocatable :: metaanddatav(:)
type(dbafilter) :: filter
type(dbadcv) :: vars
logical :: read_next
integer :: i,j
double precision :: lat,lon
integer :: year, month, day, hour, minute, msec
real :: val

! connect to dsn
session=dbasession(anaflag="write", dataflag="write", attrflag="write",&
 filename="/home/ppatruno/tmp/rmap.bufr")

! create filter for real data
allocate (vars%dcv(2))
allocate (vars%dcv(1)%dat,source=dbadatar("B12101"))
allocate (vars%dcv(2)%dat,source=dbadatar("B13003"))
filter=dbafilter(vars=vars)

! get data
call session%ingest(metaanddatav,filter=filter)


do i =1, size(metaanddatav)
!  call metaanddatav(i)%metadata%datetime%display()
!  call metaanddatav(i)%metadata%level%display()
  do j=1,size(metaanddatav(i)%dataattrv%dataattr)
!    call metaanddatav(i)%dataattrv%dataattr(j)%dat%display()
    if(metaanddatav(i)%dataattrv%dataattr(j)%dat%btable == "B13003" .and. metaanddatav(i)%dataattrv%dataattr(j)%dat%c_e()) then
      call metaanddatav(i)%dataattrv%dataattr(j)%dat%get(val)
      call getval(metaanddatav(i)%metadata%datetime%datetime,year=year, month=month, day=day, hour=hour, minute=minute, msec=msec)
      print *,"date:",year, month, day, hour, minute, msec/1000.
      print *,"level 1:",metaanddatav(i)%metadata%level%vol7d_level%l1
      lon=getlon(metaanddatav(i)%metadata%ana%coord)
      lat=getlat(metaanddatav(i)%metadata%ana%coord)
      print *,"coord:",lat,lon
      print *,"humidity", val
    end if
  end do
end do

deallocate (metaanddatav)

!close everythings
call session%delete()

end program dbatransform
