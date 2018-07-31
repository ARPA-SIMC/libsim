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
#include "config.h"
PROGRAM example_optionparser
USE optionparser_class
USE missing_values
USE err_handling
USE char_utilities
USE array_utilities
USE log4fortran
IMPLICIT NONE

! option parsing
TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
INTEGER :: i, iargc
! option variables
CHARACTER(len=80) :: name, extraopt
INTEGER :: nx
REAL :: xval, yval
DOUBLE PRECISION :: dval
TYPE(arrayof_integer) :: count_list
TYPE(arrayof_real) :: value_list
LOGICAL :: force, version
INTEGER :: verbose
! other variables
!INTEGER :: i
! define the option parser, for help2man usage_msg should start with
! "Usage:"
opt = optionparser_new(description_msg= &
 'Example program for the optionparser class, &
 &it does not do anything useful.', &
 usage_msg='Usage: example_getopt [options] inputfile outputfile')

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

! help options, useful for help2man
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN ! for help2man
  CALL exit(0)
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_log(L4F_ERROR,'in command-line parameters')
  CALL raise_fatal_error()
ENDIF
IF (version) THEN ! for help2man
  WRITE(*,'(A,1X,A)')'example_optionparser',VERSION
  CALL exit(0)
ENDIF

! check for specific errors in the options
IF (dval <= 0.0D0) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_log(L4F_ERROR,'dval must be positive!')
  CALL raise_fatal_error()
ENDIF

! release all the option data structure, the variables set by options will remain
CALL delete(opt)

! print a report of the options received
CALL l4f_log(L4F_INFO,'options report:')
CALL l4f_log(L4F_INFO,'name: '//TRIM(name))
CALL l4f_log(L4F_INFO,'nx: '//t2c(nx))
CALL l4f_log(L4F_INFO,'xval: '//t2c(xval))
IF (c_e(yval)) THEN
  CALL l4f_log(L4F_INFO,'yval: '//t2c(yval))
ELSE
  CALL l4f_log(L4F_INFO,'yval has not been specified')
ENDIF
CALL l4f_log(L4F_INFO,'dval: '//t2c(dval))
CALL l4f_log(L4F_INFO,'count-list: '//t2c(count_list%arraysize))
DO i = 1, count_list%arraysize
  CALL l4f_log(L4F_INFO,t2c(i)//': '//t2c(count_list%array(i)))
ENDDO
CALL l4f_log(L4F_INFO,'value-list: '//t2c(value_list%arraysize))
DO i = 1, value_list%arraysize
  CALL l4f_log(L4F_INFO,t2c(i)//': '//t2c(value_list%array(i)))
ENDDO
CALL l4f_log(L4F_INFO,'force: '//t2c(force))
CALL l4f_log(L4F_INFO,'verbose: '//t2c(verbose))

! parse the remaining optional arguments
IF (optind <= iargc()) THEN
  CALL l4f_log(L4F_INFO, 'extra arguments provided:')
  DO i = optind, iargc()
    CALL getarg(i, extraopt)
    CALL l4f_log(L4F_INFO, TRIM(extraopt))
  ENDDO
ENDIF

CALL delete(count_list)
CALL delete(value_list)

END PROGRAM example_optionparser
