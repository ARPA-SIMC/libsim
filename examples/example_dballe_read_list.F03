program dbatransform

use dballe_class
implicit none

type(dbasession) :: session,sessionfrom,sessionto
type(dbaconnection) :: connection,connectionfrom
type(dbametaanddatalist) :: metaanddatal
type(dbafilter) :: filter
logical :: status


session=dbasession(anaflag="write", dataflag="write", attrflag="write",&
 filename="/home/ppatruno/tmp/rmap.bufr",loadfile=.false.)

filter=dbafilter(var="B12101")

! in this case we have to pass filter and do not set it
call session%ingest(metaanddatal,filter=filter)

print *, "dati dati:"
call metaanddatal%display()

status = metaanddatal%delete()

!close everythings
call session%delete()
!call connection%delete()


end program dbatransform
