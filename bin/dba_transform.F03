program dbatransform

use dballe_class
implicit none

type(dbasession) :: session,sessionfrom,sessionto
type(dbaconnection) :: connection,connectionfrom
type(dbametaanddatalist) :: metaanddatal
type(dbafilter) :: filter
logical :: status


! connect to dsn
!connection=dbaconnection(dsn="mem:")
!session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write")


! create mem db where put bufr from file
!session=dbasession(connection,wipe=.true.,anaflag="write", dataflag="write", attrflag="write",&
session=dbasession(anaflag="write", dataflag="write", attrflag="write",&
 filename="/home/ppatruno/tmp/rmap.bufr",mode="r",format="BUFR")

!filter=dbafilter(var="B12101")
! in this case we have to pass filter and do not set it
!call session%ingest(metaanddatal,filter=filter)
!metaanddatal=dbametaanddatalist()
call session%ingest(metaanddatal)

print *, "dati dati:"
call metaanddatal%display()

status = metaanddatal%delete()

!close everythings
call session%delete()
!call connection%delete()


end program dbatransform
