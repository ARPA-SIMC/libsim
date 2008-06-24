! Programma di test per il module char_utilities
! migliorare a piacimento
PROGRAM char_test
USE kinds
USE char_utilities
IMPLICIT NONE

CHARACTER(len=64) :: charbuf
INTEGER :: icheck
INTEGER(kind=int_b) :: bcheck
REAL :: rcheck
DOUBLE PRECISION :: dcheck

PRINT*,'=== Testing char_utilities module ==='

PRINT*,'Checking int_to_char'
charbuf = to_char(HUGE(1))
READ(charbuf, '(I10)') icheck
IF (icheck /= HUGE(1)) CALL EXIT(1)
charbuf = to_char(-1000000)
READ(charbuf, '(I10)') icheck
IF (icheck /= -1000000) CALL EXIT(1)

PRINT*,'Checking byte_to_char'
charbuf = to_char(127_int_b)
READ(charbuf, '(I4)') bcheck
IF (bcheck /= 127_int_b) CALL EXIT(1)
charbuf = to_char(-127_int_b)
READ(charbuf, '(I4)') bcheck
IF (bcheck /= -127_int_b) CALL EXIT(1)

PRINT*,'Checking real_to_char'
charbuf = to_char(1.0E+20)
READ(charbuf, '(F15.0)') rcheck
IF (ABS((rcheck-1.0E+20)/1.0E+20) > 1.0E-30 ) CALL EXIT(1)
charbuf = to_char(-1.0E-20)
READ(charbuf, '(F15.0)') rcheck
IF (ABS((rcheck+1.0E-20)/1.0E+20) > 1.0E-30 ) CALL EXIT(1)

PRINT*,'Checking double_to_char'
charbuf = to_char(1.0D+120)
READ(charbuf, '(D24.0)') dcheck
IF (ABS((dcheck-1.0D+120)/1.0D+120) > 1.0D-200 ) CALL EXIT(1)
charbuf = to_char(-1.0D-120)
READ(charbuf, '(D24.0)') dcheck
IF (ABS((dcheck+1.0D-120)/1.0D+120) > 1.0D-200 ) CALL EXIT(1)

END PROGRAM char_test
