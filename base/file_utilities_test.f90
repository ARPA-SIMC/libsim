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
! Programma di test per il module file_utilities
! migliorare a piacimento
PROGRAM file_test
USE kinds
USE file_utilities
IMPLICIT NONE

TYPE(csv_record) :: csv_writer, csv_reader
CHARACTER(len=80) :: charbuf, ccheck
INTEGER :: nfield, ier, icheck, clen
REAL :: rcheck
DOUBLE PRECISION :: dcheck

PRINT*,'=== Testing file_utilities module ==='

PRINT*,'Checking csv_record writing'
CALL init(csv_writer)
CALL csv_record_addfield(csv_writer, 45)
CALL csv_record_addfield_miss(csv_writer, imiss)
CALL csv_record_addfield(csv_writer, 'Berlusconi mafioso')
CALL csv_record_addfield(csv_writer, ' a''nvedi "er pecora"')
CALL csv_record_addfield_miss(csv_writer, rmiss)
PRINT*,'Checking csv_record_getrecord'
charbuf = csv_record_getrecord(csv_writer)
IF (charbuf /= '45,,Berlusconi mafioso," a''nvedi ""er pecora""",') THEN
  PRINT*,'charbuf:',TRIM(charbuf)
  CALL EXIT(1)
ENDIF
CALL delete(csv_writer)

PRINT*,'Checking csv_record reading'
charbuf = '45, 897.4,903.1 ,,Berlusconi mafioso," a''nvedi ""er pecora""",'
CALL init(csv_reader, charbuf, nfield=nfield)
PRINT*,'Checking nfield with a missing at the end'
IF (nfield /= 7) THEN
  PRINT*,'nfield:',nfield
  CALL EXIT(1)
ENDIF

PRINT*,'Checking csv_record_getfield integer'
CALL csv_record_getfield(csv_reader, icheck, ier)
IF (ier /= 0) THEN
  PRINT*,'Error code:',ier
  CALL EXIT(1)
ENDIF
IF (icheck /= 45) THEN
  PRINT*,'icheck:',icheck
  CALL EXIT(1)
ENDIF

PRINT*,'Checking csv_record_getfield real'
CALL csv_record_getfield(csv_reader, rcheck, ier)
IF (ier /= 0) THEN
  PRINT*,'Error code:',ier
  CALL EXIT(1)
ENDIF
IF (ABS(rcheck-897.4) > .1) THEN
  PRINT*,'rcheck:',rcheck
  CALL EXIT(1)
ENDIF
CALL csv_record_getfield(csv_reader, dcheck, ier)
IF (ier /= 0) THEN
  PRINT*,'Error code:',ier
  CALL EXIT(1)
ENDIF
IF (ABS(dcheck-903.1) > .1) THEN
  PRINT*,'dcheck:',dcheck
  CALL EXIT(1)
ENDIF

PRINT*,'Checking csv_record_getfield missing integer'
CALL csv_record_getfield(csv_reader, icheck, ier)
IF (ier /= 0) THEN
  PRINT*,'Error code:',ier
  CALL EXIT(1)
ENDIF
IF (c_e(icheck)) THEN
  PRINT*,'icheck:',icheck
  CALL EXIT(1)
ENDIF

PRINT*,'Checking csv_record_getfield simple character'
CALL csv_record_getfield(csv_reader, ccheck, clen, ier)
IF (ier /= 0) THEN
  PRINT*,'Error code:',ier
  CALL EXIT(1)
ENDIF
IF (ccheck(1:clen) /= 'Berlusconi mafioso') THEN
  PRINT*,'ccheck:',ccheck(1:clen)
  CALL EXIT(1)
ENDIF

PRINT*,'Checking csv_record_getfield quoted character'
CALL csv_record_getfield(csv_reader, ccheck, clen, ier)
IF (ier /= 0) THEN
  PRINT*,'Error code:',ier
  CALL EXIT(1)
ENDIF
IF (ccheck(1:clen) /= ' a''nvedi "er pecora"') THEN
  PRINT*,'ccheck:',ccheck(1:clen)
  CALL EXIT(1)
ENDIF

PRINT*,'Checking csv_record_getfield empty character'
CALL csv_record_getfield(csv_reader, ccheck, clen, ier)
IF (ier /= 0) THEN
  PRINT*,'Error code:',ier
  CALL EXIT(1)
ENDIF
IF (ccheck /= '') THEN
  PRINT*,'ccheck:',ccheck(1:clen)
  CALL EXIT(1)
ENDIF

CALL delete(csv_reader)

charbuf(LEN_TRIM(charbuf)-1:) = ' '
CALL init(csv_reader, charbuf, nfield=nfield)
PRINT*,'Checking nfield'
IF (nfield /= 6) THEN
  PRINT*,'nfield:',nfield
  CALL EXIT(1)
ENDIF

CALL delete(csv_reader)

END PROGRAM file_test
