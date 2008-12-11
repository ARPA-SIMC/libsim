PROGRAM datetime_test
!USE kinds
USE datetime_class
IMPLICIT NONE

TYPE(datetime) :: dt1, dt2, dt3
TYPE(timedelta) :: td1, td2
INTEGER :: dtim, i, ii, y, m ,d, h, mm
!INTEGER(kind=int_ll) :: un, ir1, ir2
INTEGER :: un, ir1, ir2
CHARACTER(len=12) :: dtc
CHARACTER(len=24) :: dtlong
LOGICAL :: err

PRINT*,'=== Testing datetime_class module ==='

PRINT*,'Testing datetime_init and datetime_getval with simpledate'
dt1 = datetime_new(simpledate='000412300000')
CALL getval(dt1, simpledate=dtlong)
IF (dtlong(1:12) /= '000412300000') CALL EXIT(1)
dt1 = datetime_new(simpledate='000412310000')
CALL getval(dt1, simpledate=dtlong)
IF (dtlong(1:12) /= '000412310000') CALL EXIT(1)

PRINT*,'Testing datetime_init and datetime_getval with isodate'
dt1 = datetime_new(simpledate='000412300000')
CALL getval(dt1, isodate=dtlong)
IF (dtlong(1:23) /= '0004-12-30 00:00:00.000') CALL EXIT(1)
dt1 = datetime_new(simpledate='000412310000')
CALL getval(dt1, isodate=dtlong)
IF (dtlong(1:23) /= '0004-12-31 00:00:00.000') CALL EXIT(1)

PRINT*,'Testing leap year, 2000'
dt1 = datetime_new(simpledate='199912010000')
td1 = timedelta_new(minute=60*24)
DO i=1,366
  dt1 = dt1+td1
ENDDO
CALL getval(dt1, simpledate=dtlong)
IF (dtlong(1:17) /= '20001201000000000') CALL EXIT(1)

PRINT*,'Testing leap year, 1900'
dt1 = datetime_new(simpledate='189912010000')
td1 = timedelta_new(minute=60*24)
DO i=1,365
  dt1 = dt1+td1
ENDDO
CALL getval(dt1, simpledate=dtlong)
IF (dtlong(1:17) /= '19001201000000000') CALL EXIT(1)

PRINT*,'Testing human timedelta intervals'
dt1 = datetime_new(simpledate='189912010000')
td1 = timedelta_new(month=3)
DO i=1,101
  dt1 = dt1+4*td1
ENDDO
CALL getval(dt1, simpledate=dtlong)
IF (dtlong(1:17) /= '20001201000000000') CALL EXIT(1)

PRINT*,'Testing now'
dt1 = datetime_new(now=datetime_utc)
dt2 = datetime_new(now=datetime_local)
CALL getval(dt1, isodate=dtlong)
WRITE(*,'(A,A)')'UTC time is: ',dtlong
CALL getval(dt2, isodate=dtlong)
WRITE(*,'(A,A)')'Local time is: ',dtlong

END PROGRAM datetime_test
