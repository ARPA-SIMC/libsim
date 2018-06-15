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
! Programma di test per il module simple_stat
! migliorare a piacimento
PROGRAM grid_transform_test
USE missing_values
USE grid_transform_class
IMPLICIT NONE


INTEGER,PARAMETER :: ninterv=5, nval=7
REAL :: interv_lower(ninterv)=(/20.,20.,rmiss,20.,20./), &
        interv_upper(ninterv)=(/40.,rmiss,40.,20.,10./), &
 interv_testval(nval)=(/5.,10.,15.,20.,30.,40.,50./)
REAL :: interv_ffresult(nval,ninterv)=RESHAPE((/ &
 0,0,0,0,1,0,0, &
 0,0,0,0,1,1,1, &
 1,1,1,1,1,0,0, &
 0,0,0,0,0,0,0, &
 0,0,0,0,0,0,0 &
 /), (/nval,ninterv/))
REAL :: &
interv_ftresult(nval,ninterv)=RESHAPE((/ &
 0,0,0,0,1,1,0, &
 0,0,0,0,1,1,1, &
 1,1,1,1,1,1,0, &
 0,0,0,0,0,0,0, &
 0,0,0,0,0,0,0 &
 /), (/nval,ninterv/)), &
interv_tfresult(nval,ninterv)=RESHAPE((/ &
 0,0,0,1,1,0,0, &
 0,0,0,1,1,1,1, &
 1,1,1,1,1,0,0, &
 0,0,0,0,0,0,0, &
 0,0,0,0,0,0,0 &
 /), (/nval,ninterv/)), &
interv_ttresult(nval,ninterv)=RESHAPE((/ &
 0,0,0,1,1,1,0, &
 0,0,0,1,1,1,1, &
 1,1,1,1,1,1,0, &
 0,0,0,1,0,0,0, &
 0,0,0,0,0,0,0 &
 /), (/nval,ninterv/)), &
 tmpresult(nval,ninterv)

INTEGER :: i, j
TYPE(interval_info) :: interv, interv_init

PRINT*,'=== Testing grid_transform module ==='

PRINT*,'Checking intervals a<x<b'
DO j = 1, ninterv
  interv = interval_info_new(interv_gt=interv_lower(j), interv_lt=interv_upper(j))
  DO i = 1, nval
    tmpresult(i,j) = interval_info_valid(interv, interv_testval(i))
  ENDDO
ENDDO
CALL interv_print_result(tmpresult, interv_ffresult)

PRINT*,'Checking intervals a<x<=b'
DO j = 1, ninterv
  interv = interval_info_new(interv_gt=interv_lower(j), interv_le=interv_upper(j))
  DO i = 1, nval
    tmpresult(i,j) = interval_info_valid(interv, interv_testval(i))
  ENDDO
ENDDO
CALL interv_print_result(tmpresult, interv_ftresult)

PRINT*,'Checking intervals a<=x<b'
DO j = 1, ninterv
  interv = interval_info_new(interv_ge=interv_lower(j), interv_lt=interv_upper(j))
  DO i = 1, nval
    tmpresult(i,j) = interval_info_valid(interv, interv_testval(i))
  ENDDO
ENDDO
CALL interv_print_result(tmpresult, interv_tfresult)

PRINT*,'Checking intervals a<=x<=b'
DO j = 1, ninterv
  interv = interval_info_new(interv_ge=interv_lower(j), interv_le=interv_upper(j))
  DO i = 1, nval
    tmpresult(i,j) = interval_info_valid(interv, interv_testval(i))
  ENDDO
ENDDO
CALL interv_print_result(tmpresult, interv_ttresult)

CONTAINS

SUBROUTINE interv_print_result(res, ref)
REAL,INTENT(in) :: res(nval,ninterv), ref(nval,ninterv)

IF (COUNT(res == ref) == nval*ninterv) THEN
ELSE
  
  DO j = 1, ninterv
    DO i = 1, nval
      IF (res(i,j) /= ref(i,j)) THEN
        PRINT*,'Failing: ',interv_lower(j),'<(=)',interv_testval(i),'<(=)',interv_upper(j)
        PRINT*,'Result: ',res(i,j),' Reference: ',ref(i,j)
      ENDIF
    ENDDO
  ENDDO
  CALL EXIT(1)
ENDIF

END SUBROUTINE interv_print_result



END PROGRAM grid_transform_test
