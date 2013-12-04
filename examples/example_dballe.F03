program example_dballe

use dballe_class
implicit none

type(dba) :: dballe
type(dbalevel) :: level
type(dbatimerange) :: timerange
type(dbadatai),allocatable :: idata(:)
type(dbarecord) :: record

level=dbalevel(level1=105, l1=2000)
timerange=dbatimerange(timerange=4, p1=3600,p2=7200)
call level%display()
call timerange%display()


idata=[dbadatai("temperatura",27315)]

print *,"record"
record=dbarecord(level,timerange,idata)

call record%display()


dballe=dba()
call dballe%delete()


end program example_dballe
