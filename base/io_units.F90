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
!> Definition of constants related to I/O units.
!! This modules defines some integer constants associating the UNIX
!! Input/Output units to Fortran units, which can then used in
!! commands such as \c READ, \c WRITE and \c INQUIRE.
!!
!! Example of typical use:
!! \code
!! USE io_units
!! ...
!! WRITE(stout_unit,*)'Dimmi qualcosa di carino'
!! READ(stdin_unit,*)mesg
!! IF (mesg == 'oca') THEN
!!   WRITE(stderr_unit,*)'Mascalzone!'
!!   STOP
!! ENDIF
!! ...
!! \endcode
!! \ingroup base
MODULE io_units
IMPLICIT NONE

! These should be set by autoconf and included
INTEGER, PARAMETER :: stdin_unit = 5 !< standard input
INTEGER, PARAMETER :: stdout_unit = 6 !< standard output
INTEGER, PARAMETER :: stderr_unit = 0 !< standard error

END MODULE io_units
