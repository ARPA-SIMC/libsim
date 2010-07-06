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
PROGRAM example_getopt
USE getopt_m
USE log4fortran
IMPLICIT NONE

CHARACTER(len=1) :: ch
CHARACTER(len=512) :: extraopt
TYPE(option_s) :: opts(2)
INTEGER :: i, iargc

! Define two long options corresponding to short options a and b,
! the first without, the second with optional argument
opts(1) = option_s( 'alpha', .false., 'a' )
opts(2) = option_s( 'beta',  .true.,  'b' )

! Loop over options
DO
  SELECT CASE( getopt( 'ab:c', opts ))
  CASE( CHAR(0)) ! end of options
    EXIT
  CASE( 'a' ) ! long/short option without argument
    CALL l4f_log(L4F_INFO, 'option alpha/a')
  CASE( 'b' ) ! long/short option with argument
    CALL l4f_log(L4F_INFO, 'option beta/b='//TRIM(optarg))
  CASE( 'c' ) ! no long option here
    CALL l4f_log(L4F_INFO, 'option c')
  CASE( '?' )
    CALL l4f_log(L4F_FATAL, 'unknown option '//TRIM(optopt))
    call exit(1)
  CASE default
    CALL l4f_log(L4F_FATAL, 'unhandled option '//TRIM(optopt)//' this should not happen')
    call exit(2)
  END SELECT
END DO

IF (optind <= iargc()) THEN
  CALL l4f_log(L4F_INFO, 'extra arguments provided:')
  DO i = optind, iargc()
    CALL getarg(i, extraopt)
    CALL l4f_log(L4F_INFO, TRIM(extraopt))
  ENDDO
ENDIF

END PROGRAM example_getopt
