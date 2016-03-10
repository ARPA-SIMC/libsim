PROGRAM mathstat_func_test
USE mathstat_func_class
IMPLICIT NONE

TYPE(mathstat_func_1d) :: userfunc
DOUBLE PRECISION :: val1, val2

PRINT*,'Checking predefined function, ''gauss'''
CALL userfunc%set_func('gauss', (/8.0D0/))
val1 = userfunc%compute(1.047D0)
val2 = EXP(-0.5D0*8.0D0*1.047D0*1.047D0)
PRINT*,'gauss: ',val1,val2

IF (val1 /= val2) THEN
  IF (ABS(val1-val2)/MAX(ABS(val1), ABS(val2)) > 1.0D-12) CALL EXIT(1)
ENDIF

CALL userfunc%delete()

PRINT*,'Checking user-defined function'
CALL userfunc%set_func(user_defined, (/7.0D0,3.0D0,2.0D0/))
val1 = userfunc%compute(7.72D0)
val2 = 7.0D0 + 3.0D0*7.72D0 + 2.0D0*7.72D0**2
PRINT*,'user-defined function: ',val1,val2

IF (val1 /= val2) THEN
  IF (ABS(val1-val2)/MAX(ABS(val1), ABS(val2)) > 1.0D-12) CALL EXIT(1)
ENDIF


CONTAINS

FUNCTION user_defined(x, params) RESULT(func)
DOUBLE PRECISION,INTENT(in) :: x
DOUBLE PRECISION,INTENT(in) :: params(:)

DOUBLE PRECISION :: func
INTEGER :: i

func = 0

DO i = 1, SIZE(params)
  func = func + params(i)*x**(i-1)
ENDDO

END FUNCTION user_defined

END PROGRAM mathstat_func_test


    
  
