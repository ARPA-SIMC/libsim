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
!> \defgroup base Libsim package, base library.
!! The libsim base library defines modules and classes of general
!! utility for scientifical applications in Fortran 90. In order to
!! compile and link programs using this library, you have to insert
!! the required \c USE statements in the program units involved,
!! specify the location of module files when compiling (tipically \c
!! -I/usr/lib/gfortran/modules or \c -I/usr/lib64/gfortran/modules or
!! \c -I/usr/include) and indicate the library name \c -lsim_base when
!! linking, assuming that the library has been installed in a default
!! location.

!> Definition of constants to be used for declaring variables of a
!! desired type. This module defines constants that can be portably
!! used when declaring variables (through the \c KIND attribute) and
!! when defining constants (through the underscore character \c _ ) in
!! order to be sure that the desired type is used.
!!
!! There is a subtle difference between platform-default single and
!! double precision real, obtained by declaring a variable of type \c
!! REAL or \c DOUBLE \c PRECISION respectively, and single and double
!! precision IEEE (4 and 8 bytes respectively) which are the standard
!! IEEE data types and which are declared through \c REAL(kind=fp_s)
!! and \c REAL(kind=fp_d) respectively: these two pairs of types
!! usually coincide, but it may be not the case on some platforms, so
!! you should choose one or the other approach depending on situation.
!!
!! Example of typical use:
!! \code
!! USE kinds
!! ...
!! INTEGER(kind=int_b) :: ab, bb
!! REAL(kind=fp_d) :: dd
!! 
!! ab = 13_int_b
!! dd = REAL(ab, kind=fp_d)
!! ...
!! \endcode
!! \ingroup base
MODULE kinds
IMPLICIT NONE

INTEGER, PARAMETER :: int_b    = SELECTED_INT_KIND(1) !< 1-byte integer (byte)
INTEGER, PARAMETER :: int_s    = SELECTED_INT_KIND(4) !< 2-byte integer (short)
INTEGER, PARAMETER :: int_l    = SELECTED_INT_KIND(8) !< 4-byte integer (long)
INTEGER, PARAMETER, PRIVATE :: &
 int_ll_t = SELECTED_INT_KIND(16)
!> 8-byte integer (long long) if supported, otherwise 4-byte integer
INTEGER, PARAMETER :: int_ll = &
 ( ( ( 1 + SIGN( 1, int_ll_t ) ) / 2 ) * int_ll_t ) + &
 ( ( ( 1 - SIGN( 1, int_ll_t ) ) / 2 ) * int_l    )

INTEGER, PARAMETER :: fp_s = SELECTED_REAL_KIND(6) !< single precision floating point (4 byte IEEE)
INTEGER, PARAMETER :: fp_d = SELECTED_REAL_KIND(15) !< double precision floating point (8 byte IEEE)
INTEGER, PARAMETER, PRIVATE :: fp_q_t = SELECTED_REAL_KIND(20)
!> quad precision floating point (16 byte IEEE) if supported, otherwise double precision floating point
INTEGER, PARAMETER :: fp_q = &
 ( ( ( 1 + SIGN( 1, fp_q_t ) ) / 2 ) * fp_q_t ) + &
 ( ( ( 1 - SIGN( 1, fp_q_t ) ) / 2 ) * fp_d )

INTEGER, PARAMETER :: ptr_c = SIZEOF_PTR_C !< kind for an integer having the same size of a C pointer

END MODULE kinds
