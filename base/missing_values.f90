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

!> Definitions of constants and functions for working with missing values.
!! This modules provides tools for handling missing values in various
!! data types. Users should use the various \a *miss constants for
!! setting a variable to a missing value, and use the interfaced
!! function \a c_e or an equality test with the corresponding \a *miss
!! constant for checking the validity of a value.
!!
!! When using the \a *miss constants, it is important to choose the
!! constant of the right type in order to avoid implicit conversions
!! that would impair the results.
!!
!! Example of typical use:
!! \code
!! USE missing_values
!! ...
!! INTEGER :: i
!! INTEGER(kind=int_b) :: ib
!! REAL :: r
!! 
!! i = imiss
!! ib = ibmiss
!! r = rmiss
!! IF (c_e(i) .OR. c_e(ib)) THEN
!!   PRINT*,"this is not executed"
!! ENDIF
!! IF (.NOT.c_e(r)) THEN
!!   PRINT*,"this is executed"
!! ENDIF
!! ...
!! \endcode
!! \ingroup base
MODULE missing_values
USE kinds
IMPLICIT NONE

REAL, PARAMETER :: rmiss = HUGE(1.0) !< default single precision real
DOUBLE PRECISION, PARAMETER :: dmiss = HUGE(1.0D0) !< default double precision real
REAL(kind=fp_s), PARAMETER :: rsmiss = HUGE(1.0_fp_s) !< single precision IEEE real \a (kind=fp_s)
REAL(kind=fp_d), PARAMETER :: rdmiss = HUGE(1.0_fp_d) !< double precision IEEE real \a (kind=fp_d)
INTEGER, PARAMETER :: imiss = HUGE(0) !< default integer
INTEGER(kind=int_b), PARAMETER :: ibmiss = HUGE(0_int_b) !< 1-byte integer \a (kind=int_b)
INTEGER(kind=int_b), PARAMETER :: bmiss = ibmiss
INTEGER(kind=int_s), PARAMETER :: ismiss = HUGE(0_int_s) !< 2-byte integer \a (kind=int_s)
INTEGER(kind=int_l), PARAMETER :: ilmiss = HUGE(0_int_l) !< 4-byte integer \a (kind=int_l)
INTEGER(kind=int_ll), PARAMETER :: illmiss = HUGE(0_int_ll) !< 8-byte integer if supported \a (kind=int_ll)
CHARACTER(len=1), PARAMETER :: cmiss = char(0) !< character (any length)


!> Function to check whether a value is missing or not.
!! It works with all the basic types supported and returns a logical
!! value \a .TRUE. if the argument is a valid value and \a .FALSE. if
!! not. It is elemental, so it works also for arrays of any size and
!! shape.
INTERFACE c_e
  MODULE PROCEDURE c_e_b, c_e_s, c_e_l,c_e_ll, c_e_r, c_e_d, c_e_c
END INTERFACE

PRIVATE c_e_b, c_e_s, c_e_l,c_e_ll, c_e_r, c_e_d, c_e_c

CONTAINS

!> Check whether the byte argument is valid.
ELEMENTAL LOGICAL FUNCTION c_e_b(var)
INTEGER(kind=int_b),INTENT(in)  :: var !< value to be checked

c_e_b = var /= ibmiss

END FUNCTION c_e_b


!> Check whether the short integer argument is valid.
ELEMENTAL LOGICAL FUNCTION c_e_s(var)
INTEGER(kind=int_s),INTENT(in)  :: var !< value to be checked

c_e_s = var /= ismiss

END FUNCTION c_e_s


!> Check whether the long integer argument is valid.
ELEMENTAL LOGICAL FUNCTION c_e_l(var)
INTEGER(kind=int_l),INTENT(in)  :: var !< value to be checked

c_e_l = var /= ilmiss

END FUNCTION c_e_l


! This may not compile if long long is as long as long
!> Check whether the long long integer argument is valid.
ELEMENTAL LOGICAL FUNCTION c_e_ll(var)
INTEGER(kind=int_ll),INTENT(in)  :: var !< value to be checked

c_e_ll = var /= illmiss

END FUNCTION c_e_ll


!> Check whether the real argument is valid.
ELEMENTAL LOGICAL FUNCTION c_e_r(var)
REAL,INTENT(in)  :: var !< value to be checked

c_e_r = var /= rmiss

END FUNCTION c_e_r


!> Check whether the double precision argument is valid.
ELEMENTAL LOGICAL FUNCTION c_e_d(var)
DOUBLE PRECISION,INTENT(in)  :: var !< value to be checked

c_e_d = var /= dmiss

END FUNCTION c_e_d
! cannot implement quad precision otherwise it may not compile if missing

!> Check whether the character argument is valid.
ELEMENTAL LOGICAL FUNCTION c_e_c(var)
CHARACTER(len=*),INTENT(in)  :: var !< value to be checked

c_e_c = var /= cmiss

END FUNCTION c_e_c


END MODULE missing_values
