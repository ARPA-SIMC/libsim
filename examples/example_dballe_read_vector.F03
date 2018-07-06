PROGRAM dbaread_db
USE dballe_class
USE vol7d_ana_class
USE datetime_class
IMPLICIT NONE

TYPE(dbaconnection) :: dbhandle
TYPE(dbasession) :: session
TYPE(dbametaanddata),ALLOCATABLE :: metaanddatav(:)
TYPE(dbafilter) :: filter
CHARACTER(len=512) :: input_uri, query


CALL getarg(1, input_uri)
CALL getarg(2, query)

dbhandle=dbaconnection(input_uri)
! connect to dsn
session=dbasession(dbhandle)

! get ana
filter = dbafilter(contextana=.TRUE.,query=TRIM(query))
CALL session%ingest(metaanddatav,filter=filter)
IF (ALLOCATED(metaanddatav)) THEN
  PRINT*,'ana',SIZE(metaanddatav)
  DEALLOCATE (metaanddatav)
ENDIF

! get data
filter=dbafilter(contextana=.FALSE.,query=TRIM(query))
CALL session%ingest(metaanddatav,filter=filter)
IF (ALLOCATED(metaanddatav)) THEN
  PRINT*,'data',SIZE(metaanddatav)
  DEALLOCATE (metaanddatav)
ENDIF

!close everything
CALL session%delete()

END PROGRAM dbaread_db
