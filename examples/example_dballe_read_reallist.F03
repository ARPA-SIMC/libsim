program dbatransform

use dballe_class
implicit none

type(dbasession) :: session,sessionfrom,sessionto
type(dbametaanddatarlist) :: metaanddatarl
type(dbametaanddatar),allocatable :: metaanddatarv(:)
type(dbafilter) :: filter
logical :: status


! create mem db and import bufr from file
session=dbasession(anaflag="write", dataflag="write", attrflag="write",&
filename="/home/ppatruno/tmp/rmap.bufr",mode="r",format="BUFR",loadfile=.false.)

filter=dbafilter(var="B12101")

print *,"ingest"
call session%ingest(metaanddatarl,filter=filter)

!print *, "dati dati:"
!call metaanddatarl%display()

metaanddatarv=metaanddatarl%toarray()
status = metaanddatarl%delete()

print *,"max=",maxval(metaanddatarv(:)%dbadatar%value)

deallocate(metaanddatarv)

!close everythings
call session%delete()

end program dbatransform
