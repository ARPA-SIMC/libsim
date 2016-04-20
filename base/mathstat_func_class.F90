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

!> Module for encapsulating an arbitrary mathematical function into a
!!class.
!! This module defines a class that encapsulates a mathematical
!! function, either chosen from a predefined set or user-provided, to
!! be passed to other computing modules requiring the user to provide
!! a function.
!!
!! \ingroup base
MODULE mathstat_func_class
USE missing_values
!USE optional_values
!USE array_utilities
IMPLICIT NONE

! Per l'optimum interpolation a basso livello serve:
!  - (o)bs, (b)ackground interpolato su punti obs, entrambi (no)
!  - O covar. err. di O, B covar. err. di b sui punti o, entrambe (no,no)
!  - Brhs covar. err. di b tra i punti b e i punti o (no,nb)

! Per l'optimum interpolation ad alto livello con B e O calcolate come
! funzioni analitiche rispetto alla distanza serve:
!  - (o)bs, (b)ackground interpolato su punti obs, entrambi (no)
!  - funzione di autocorrelazione dell'errore di O e di B
!  - coordinate di o e b (no) (nb)
!  - funzione distanza tra le coordinate


TYPE :: mathstat_func_1d
  PROCEDURE(mathstat_func_1d_gauss),NOPASS,POINTER :: func => NULL()
  DOUBLE PRECISION,ALLOCATABLE :: params(:)
  CONTAINS
  PROCEDURE,PRIVATE :: mathstat_func_1d_set_custom
  PROCEDURE,PRIVATE :: mathstat_func_1d_set_pre
  PROCEDURE,PRIVATE :: mathstat_func_1d_delete
  PROCEDURE,PRIVATE :: mathstat_func_1d_compute
  PROCEDURE,PRIVATE :: mathstat_func_1d_safecompute
  GENERIC :: set_func => mathstat_func_1d_set_custom, mathstat_func_1d_set_pre
  GENERIC :: delete => mathstat_func_1d_delete
!  FINAL :: mathstat_func_1d_delete ! FINAL not implemented in gfortran
  GENERIC :: compute => mathstat_func_1d_compute
  GENERIC :: safecompute => mathstat_func_1d_safecompute
END TYPE mathstat_func_1d

PRIVATE
PUBLIC mathstat_func_1d

CONTAINS


SUBROUTINE mathstat_func_1d_set_custom(this, func, params)
CLASS(mathstat_func_1d),INTENT(inout) :: this
PROCEDURE(mathstat_func_1d_gauss) :: func
DOUBLE PRECISION,INTENT(in) :: params(:)

this%func => func
this%params = params

END SUBROUTINE mathstat_func_1d_set_custom


SUBROUTINE mathstat_func_1d_set_pre(this, func, params)
CLASS(mathstat_func_1d),INTENT(inout) :: this
CHARACTER(len=*),INTENT(in) :: func
DOUBLE PRECISION,INTENT(in) :: params(:)

SELECT CASE(func)
CASE('gauss')
  IF (SIZE(params) >= 2) THEN
    this%func => mathstat_func_1d_gauss
  ELSE
    this%func => mathstat_func_1d_gauss_nonorm
  ENDIF
CASE('exp')
  IF (SIZE(params) >= 2) THEN
    this%func => mathstat_func_1d_exp
  ELSE
    this%func => mathstat_func_1d_exp_nonorm
  ENDIF
CASE('delta')
  this%func => mathstat_func_1d_delta
CASE default
  this%func => NULL()
END SELECT
! strongly tailored to the functions defined
IF (SIZE(params) > 0) THEN
  this%params = params
ELSE
  this%params = (/1.0D0/)
ENDIF

END SUBROUTINE mathstat_func_1d_set_pre


SUBROUTINE mathstat_func_1d_delete(this)
CLASS(mathstat_func_1d),INTENT(out) :: this

! intent(out) should be enough to reinitialise
!this%func => NULL()
!IF (ALLOCATED(this%params)) DEALLOCATE(this%params)

END SUBROUTINE mathstat_func_1d_delete


FUNCTION mathstat_func_1d_compute(this, x) RESULT(compute)
CLASS(mathstat_func_1d),INTENT(inout) :: this
DOUBLE PRECISION,INTENT(in) :: x

DOUBLE PRECISION :: compute

compute = this%func(x, this%params)

END FUNCTION mathstat_func_1d_compute


FUNCTION mathstat_func_1d_safecompute(this, x) RESULT(compute)
CLASS(mathstat_func_1d),INTENT(inout) :: this
DOUBLE PRECISION,INTENT(in) :: x

DOUBLE PRECISION :: compute

IF (ASSOCIATED(this%func)) THEN
  compute = this%func(x, this%params)
ELSE
  compute = dmiss
ENDIF

END FUNCTION mathstat_func_1d_safecompute


! predefined functions
FUNCTION mathstat_func_1d_gauss(x, params) RESULT(func)
DOUBLE PRECISION,INTENT(in) :: x
DOUBLE PRECISION,INTENT(in) :: params(:)

DOUBLE PRECISION :: func

func = params(2)*EXP(-0.5D0*params(1)*x*x)

END FUNCTION mathstat_func_1d_gauss

FUNCTION mathstat_func_1d_gauss_nonorm(x, params) RESULT(func)
DOUBLE PRECISION,INTENT(in) :: x
DOUBLE PRECISION,INTENT(in) :: params(:)

DOUBLE PRECISION :: func

func = EXP(-0.5D0*params(1)*x*x)

END FUNCTION mathstat_func_1d_gauss_nonorm

FUNCTION mathstat_func_1d_exp(x, params) RESULT(func)
DOUBLE PRECISION,INTENT(in) :: x
DOUBLE PRECISION,INTENT(in) :: params(:)

DOUBLE PRECISION :: func

func = params(2)*EXP(-params(1)*x)

END FUNCTION mathstat_func_1d_exp

FUNCTION mathstat_func_1d_exp_nonorm(x, params) RESULT(func)
DOUBLE PRECISION,INTENT(in) :: x
DOUBLE PRECISION,INTENT(in) :: params(:)

DOUBLE PRECISION :: func

func = EXP(-params(1)*x)

END FUNCTION mathstat_func_1d_exp_nonorm

FUNCTION mathstat_func_1d_delta(x, params) RESULT(func)
DOUBLE PRECISION,INTENT(in) :: x
DOUBLE PRECISION,INTENT(in) :: params(:)

DOUBLE PRECISION :: func

IF (x == 0.0D0) THEN
  func = params(1)
ELSE
  func = 0.0D0
ENDIF

END FUNCTION mathstat_func_1d_delta


END MODULE mathstat_func_class
