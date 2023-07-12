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
PROGRAM optionparser_test
USE optionparser_class
USE missing_values
USE file_utilities
!USE err_handling
!USE char_utilities
USE array_utilities
!USE log4fortran
IMPLICIT NONE

! option parsing
TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
INTEGER :: iargc
! option variables
CHARACTER(len=80) :: name
INTEGER :: nx
REAL :: xval, yval
DOUBLE PRECISION :: dval
TYPE(arrayof_integer) :: count_list
TYPE(arrayof_real) :: value_list
LOGICAL :: force, version
INTEGER :: verbose
! for checking
CHARACTER(len=1024) :: exp_res
TYPE(csv_record) :: csv_reader
CHARACTER(len=80) :: ccheck
INTEGER :: icheck, ier
REAL :: rcheck
DOUBLE PRECISION :: dcheck

! define the option parser, for help2man usage_msg should start with
! "Usage:"
opt = optionparser_new(description_msg= &
 'Test program for the optionparser class, &
 &it just tests the arguments.', &
 usage_msg='Usage: optionparser_test [options] expected_result')

! add various options
CALL optionparser_add(opt, 'n', 'name', name, 'defaultname', help= &
 'short and long character option with default value')
CALL optionparser_add(opt, '', 'nx', nx, 100, help= &
 'long integer option with default value')
CALL optionparser_add(opt, 'x', 'xval', xval, 712., help= &
 'short and long real option with default value')
yval = rmiss ! preset yval to recognize whether it has been set
CALL optionparser_add(opt, '', 'yval', yval, help= &
 'long real option without default value')
CALL optionparser_add(opt, 'd', 'dval', dval, 489.0D0, help=&
 'short and long double precision option with default value, &
 &this should be a positive number')
CALL optionparser_add(opt, '', 'count-list', count_list, (/1,2,3/), help= &
 'integer array option, a comma-separated list of values can be provided, &
 &the default is partially displayed, in this case it is 1,2,3')
CALL optionparser_add(opt, '', 'value-list', value_list, help= &
 'real array option, a comma-separated list of values can be provided, &
 &the default in this case does not exist')
CALL optionparser_add(opt, 'f', 'force', force, help= &
 'logical option, it cannot have a default value because it is .FALSE. by design')
CALL optionparser_add_count(opt, 'v', 'verbose', verbose, help= &
 'count option without start value, it will be incremented at every appearence &
 &of the option')
verbose = 0

! help options, useful for help2man
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN ! for help2man
  CALL exit(0)
ELSE IF (optstatus == optionparser_err) THEN
  WRITE(*,'(A)')'Error in command-line arguments'
  CALL exit(1)
ENDIF
IF (version) THEN ! for help2man
  WRITE(*,'(A,1X,A)')'optionparser_test 1.0'
  CALL exit(0)
ENDIF
IF (iargc() > optind) THEN
  WRITE(*,'(A)')'Error, zero or one non-option argument required'
  CALL exit(1)
ENDIF

! check for specific errors in the options
!IF (dval <= 0.0D0) THEN
!  CALL optionparser_printhelp(opt)
!  CALL l4f_log(L4F_ERROR,'dval must be positive!')
!  CALL raise_fatal_error()
!ENDIF

! release all the option data structure, the variables set by options will remain
CALL delete(opt)

IF (iargc() < optind) THEN ! nothing to do
  CALL exit(0)
ENDIF
! now format the options for comparing with expected result
CALL getarg(optind, exp_res)
CALL init(csv_reader, exp_res)

CALL csv_record_getfield(csv_reader, ccheck, icheck, ier)
IF (ier /= 0 .OR. TRIM(ccheck) /= TRIM(name)) THEN
  WRITE(*,'(A)')'Error in command-line argument --name'
  CALL exit(1)
ENDIF

CALL csv_record_getfield(csv_reader, icheck, ier)
IF (ier /= 0 .OR. icheck /= nx) THEN
  WRITE(*,'(A)')'Error in command-line argument --nx'
  CALL exit(1)
ENDIF

CALL csv_record_getfield(csv_reader, rcheck, ier)
IF (ier /= 0 .OR. rcheck /= xval) THEN
  WRITE(*,'(A)')'Error in command-line argument --xval'
  CALL exit(1)
ENDIF

CALL csv_record_getfield(csv_reader, rcheck, ier)
IF (ier /= 0 .OR. rcheck /= yval) THEN
  WRITE(*,'(A)')'Error in command-line argument --yval'
  CALL exit(1)
ENDIF

CALL csv_record_getfield(csv_reader, dcheck, ier)
IF (ier /= 0 .OR. dcheck /= dval) THEN
  WRITE(*,'(A)')'Error in command-line argument --dval'
  CALL exit(1)
ENDIF

! add list and logical check here

CALL csv_record_getfield(csv_reader, icheck, ier)
IF (ier /= 0 .OR. icheck /= verbose) THEN
  WRITE(*,'(A)')'Error in command-line argument --verbose'
  CALL exit(1)
ENDIF

CALL delete(csv_reader)
CALL delete(count_list)
CALL delete(value_list)

END PROGRAM optionparser_test
