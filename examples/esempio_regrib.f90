PROGRAM regrib
USE grib_io_class
IMPLICIT NONE

TYPE(grib_io) :: gribt, gribtd
TYPE(matchgrib) :: mgt
INTEGER :: tunit, tdunit, ounit, ier, i
CHARACTER(len=512) :: nome
LOGICAL :: first = .TRUE.

CALL getarg(1, nome)
CALL pbopen(tunit, nome, 'r', ier)
IF (ier /= 0) THEN
  STOP
ENDIF
CALL pbopen(tdunit, nome, 'r', ier)
IF (ier /= 0) THEN
  STOP
ENDIF
CALL pbopen(ounit, nome, 'a', ier)
IF (ier /= 0) THEN
  STOP
ENDIF
CALL init(gribt)
CALL init(gribtd)

DO WHILE (.TRUE.)
  CALL setval(gribt%mg, var=(/-1, 2, 11/), lev=(/105,2,-1/))
  IF (.NOT. findgribdata(gribt, tunit, ier)) EXIT
!  IF (ier /= 0) EXIT
  IF (first) THEN
    mgt = gribt%gg
    first = .FALSE.
  ELSE
    IF (mgt == gribt%gg) EXIT
  ENDIF

  gribtd%mg = gribt%gg

  CALL setval(gribtd%mg, var=(/-1, 2, 17/), lev=(/105,2,-1/))
  IF (.NOT. findgribdata(gribtd, tdunit, ier)) EXIT
!  IF (ier /= 0) EXIT

  DO i = 1, MIN(SIZE(gribt%zsec4), SIZE(gribtd%zsec4))
    gribt%zsec4(i) = 100.*esat(gribtd%zsec4(i))/esat(gribt%zsec4(i))
  ENDDO
  gribt%isec1(6) = 52 ! %

  CALL putgribdata(gribt, ounit, ier)
  IF (ier /= 0) EXIT
ENDDO

CALL pbclose(tunit, ier)
IF (ier /= 0) THEN
  STOP
ENDIF
CALL pbclose(tdunit, ier)
IF (ier /= 0) THEN
  STOP
ENDIF
CALL pbclose(ounit, ier)
IF (ier /= 0) THEN
  STOP
ENDIF

CALL delete(gribt)
CALL delete(gribtd)


CONTAINS

FUNCTION esat(t)
REAL :: t, esat
! t in K, esat in Pa
REAL, PARAMETER ::zk=273.15, es0=611., aw=7.567*2.3025851, bw=239.7-zk, &
 awn=7.744*2.3025851, bwn=245.2-zk

IF (t > zk) THEN
  esat = es0*EXP(aw*(t-zk)/(bw+t))
ELSE
  esat = es0*EXP(awn*(t-zk)/(bwn+t))
ENDIF
END FUNCTION esat

END PROGRAM regrib


