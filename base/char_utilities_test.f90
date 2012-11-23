! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
INTEGER, POINTER :: ws(:), we(:)
TYPE(line_split) :: longline

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

PRINT*,'Checking l_nblnk'
IF (l_nblnk('1234') /= 4 .OR. l_nblnk('12345 ') /= 5) CALL EXIT(1)
PRINT*,'Checking l_nblnk degenerated'
IF (l_nblnk('    ') /= 0 .OR. l_nblnk('') /= 0) CALL EXIT(1)
PRINT*,'Checking f_nblnk'
IF (f_nblnk('1234', ' ') /= 1 .OR. f_nblnk(' 12345',' ') /= 2) CALL EXIT(1)
PRINT*,'Checking f_nblnk degenerated'
IF (f_nblnk('    ') /= 5 .OR. f_nblnk('') /= 1) CALL EXIT(1)

!PRINT*,'Checking align_left'
!IF (align_left('  ciao') /= 'ciao  ' .OR. align_left('ciao  ') /= 'ciao  ') CALL EXIT(1)
!PRINT*,'Checking align_left degenerated'
!IF (align_left('') /= '' .OR. align_left('  ') /= '  ') CALL EXIT(1)
!PRINT*,'Checking align_right'
!IF (align_right('  ciao') /= '  ciao' .OR. align_right('ciao  ') /= '  ciao') CALL EXIT(1)
!PRINT*,'Checking align_right degenerated'
!IF (align_right('') /= '' .OR. align_right('  ') /= '  ') CALL EXIT(1)
PRINT*,'Checking align_center even'
IF (align_center('  ciao') /= ' ciao ' .OR. align_center('ciao  ') /= ' ciao ') CALL EXIT(1)
PRINT*,'Checking align_center odd'
IF (align_center('  ciao ') /= '  ciao ' .AND. align_center('  ciao ') /= ' ciao  ') CALL EXIT(1)
PRINT*,'Checking align_center degenerated'
IF (align_center('') /= '' .OR. align_center('  ') /= '  ') CALL EXIT(1)

PRINT*,'Checking word_split - 3 words'
IF (word_split('  prima secunda  tertia   ') /= 3 .OR. &
 word_split('prima secunda  tertia   ') /= 3 .OR. &
 word_split('  prima secunda  tertia') /= 3 .OR. &
 word_split('prima secunda  tertia') /= 3) CALL EXIT(1)
PRINT*,'Checking word_split degenerated - 1 word'
IF (word_split('prima') /= 1 .OR. word_split(' prima') /= 1 &
 .OR. word_split('prima ') /= 1) CALL EXIT(1)
PRINT*,'Checking word_split degenerated - 0 words'
IF (word_split('') /= 0 .OR. word_split(' ') /= 0) CALL EXIT(1)

PRINT*,'Checking word_split with indices - 3 words'
IF (word_split('  prima secunda  tertia   ', ws, we) /= 3) CALL EXIT(1)
PRINT*,'Checking word_split with indices - 3 words - checking indices'
IF (ANY(ws(:) /= (/3,9,18/)) .OR. ANY(we(:) /= (/7,15,23/))) CALL EXIT(1)
DEALLOCATE(ws, we)

PRINT*,'Checking line_split'
longline=line_split_new('che bella cosa ''na jurna` de sole, l''aria serena dopo la tempesta', 20)
IF (line_split_get_nlines(longline) /= 4 .OR. &
 line_split_get_line(longline, 1) /= 'che bella cosa ''na' .OR. &
 line_split_get_line(longline, 2) /= 'jurna` de sole,' .OR. &
 line_split_get_line(longline, 3) /= 'l''aria serena dopo' .OR. &
 line_split_get_line(longline, 4) /= 'la tempesta') CALL EXIT(1)
CALL delete(longline)

END PROGRAM char_test
