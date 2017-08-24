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

!> Module for quickly interpreting the \c OPTIONAL parameters passed
!! to a subprogram.
!! This module defines functions and subroutines that handle in a
!! quick way \c OPTIONAL parameters in a subprogram by returning a
!! useable missing value if a given parameter has not been provided.
!! The module provides a generic subroutine, valid for almost all
!! intrinsic types, and specific functions.
!!
!! \ingroup base
MODULE optional_values
USE missing_values
IMPLICIT NONE

!> Generic subroutine for checking \c OPTIONAL parameters.
!! The generic interface has to be used instead of the specific
!! one. It sets the output value to input, if input is present,
!! otherwise it sets it to missing value.
INTERFACE optio
  MODULE PROCEDURE soptio_b, soptio_s, soptio_l, soptio_r, soptio_d, &
   soptio_c, soptio_log
END INTERFACE

PRIVATE
PUBLIC optio, &
 optio_b, optio_s, optio_i, optio_l, optio_r, optio_d, optio_c, optio_log, &
 soptio_b, soptio_s, soptio_l, soptio_r, soptio_d, soptio_c, soptio_log

CONTAINS

! Functions

!> Return the optional value if present, otherwise return missing value.
ELEMENTAL INTEGER(kind=int_b) FUNCTION optio_b(var)
INTEGER(kind=int_b),INTENT(in),OPTIONAL  :: var !< variable to be checked

if (present(var))then
  optio_b=var
else
  optio_b=ibmiss
end if

END FUNCTION optio_b

!> Return the optional value if present, otherwise return missing value.
ELEMENTAL INTEGER(kind=int_s) FUNCTION optio_s(var)
INTEGER(kind=int_s),INTENT(in),OPTIONAL  :: var !< variable to be checked

if (present(var))then
  optio_s=var
else
  optio_s=ismiss
end if

END FUNCTION optio_s

!> Return the optional value if present, otherwise return missing value.
ELEMENTAL INTEGER(kind=int_l) FUNCTION optio_i(var)
INTEGER(kind=int_l),INTENT(in),OPTIONAL  :: var !< variable to be checked

if (present(var))then
  optio_i=var
else
  optio_i=imiss
end if

END FUNCTION optio_i

!> Return the optional value if present, otherwise return missing value.
ELEMENTAL INTEGER(kind=int_l) FUNCTION optio_l(var)
INTEGER(kind=int_l),INTENT(in),OPTIONAL  :: var !< variable to be checked

if (present(var))then
  optio_l=var
else
  optio_l=ilmiss
end if

END FUNCTION optio_l

!> Return the optional value if present, otherwise return missing value.
ELEMENTAL REAL FUNCTION optio_r(var)
REAL,INTENT(in),OPTIONAL  :: var !< variable to be checked

if (present(var))then
  optio_r=var
else
  optio_r=rmiss
end if

END FUNCTION optio_r

!> Return the optional value if present, otherwise return missing value.
ELEMENTAL DOUBLE PRECISION FUNCTION optio_d(var)
DOUBLE PRECISION,INTENT(in),OPTIONAL  :: var !< variable to be checked

if (present(var))then
  optio_d=var
else
  optio_d=rdmiss
end if

END FUNCTION optio_d

!> Return the optional value if present, otherwise return missing value.
!! Unfortunately elemental is not possible here.
FUNCTION optio_c(var,len) RESULT(char)
CHARACTER (len=*),INTENT(in),OPTIONAL  :: var !< variable to be checked
INTEGER,INTENT(in) :: len !< length of the result

CHARACTER(len=len) :: char

if (present(var))then
  char=var
else
  char=cmiss
end if

END FUNCTION optio_c

!> Return the optional value if present, otherwise return \c .FALSE.
ELEMENTAL LOGICAL FUNCTION optio_log(var)
LOGICAL,INTENT(in),OPTIONAL  :: var !< variable to be checked

if (present(var))then
  optio_log=var
else
  optio_log=.false.
end if

END FUNCTION optio_log


! Subroutines

!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
ELEMENTAL SUBROUTINE soptio_b(var,optio_b)
INTEGER(kind=int_b),INTENT(in),OPTIONAL  :: var !< variable to be checked
INTEGER(kind=int_b),INTENT(out) :: optio_b !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_b=var
else
  optio_b=ibmiss
end if

END SUBROUTINE soptio_b

!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
ELEMENTAL SUBROUTINE soptio_s(var,optio_s)
INTEGER(kind=int_s),INTENT(in),OPTIONAL  :: var !< variable to be checked
INTEGER(kind=int_s),INTENT(out) :: optio_s !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_s=var
else
  optio_s=ismiss
end if

END SUBROUTINE soptio_s

!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
ELEMENTAL SUBROUTINE soptio_l(var,optio_l)
INTEGER(kind=int_l),INTENT(in),OPTIONAL  :: var !< variable to be checked
INTEGER(kind=int_l),INTENT(out) :: optio_l !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_l=var
else
  optio_l=ilmiss
end if

END SUBROUTINE soptio_l

!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
ELEMENTAL SUBROUTINE soptio_r(var,optio_r)
REAL,INTENT(in),OPTIONAL  :: var !< variable to be checked
REAL,INTENT(out) :: optio_r !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_r=var
else
  optio_r=rmiss
end if

END SUBROUTINE soptio_r

!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
ELEMENTAL SUBROUTINE soptio_d(var,optio_d)
DOUBLE PRECISION,INTENT(in),OPTIONAL  :: var !< variable to be checked
DOUBLE PRECISION,INTENT(out) :: optio_d !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_d=var
else
  optio_d=rdmiss
end if

END SUBROUTINE soptio_d


!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
ELEMENTAL SUBROUTINE soptio_c(var,optio_c)
CHARACTER (len=*),INTENT(in),OPTIONAL  :: var !< variable to be checked
CHARACTER (len=*),INTENT(out) :: optio_c !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_c=var
else
  optio_c=cmiss
end if

END SUBROUTINE soptio_c

!> Set the output value to input, if input is present, otherwise set it
!! to \c .FALSE.
ELEMENTAL SUBROUTINE soptio_log(var,optio_log)
LOGICAL,INTENT(in),OPTIONAL  :: var !< variable to be checked
LOGICAL,INTENT(out) :: optio_log !< equal to \a var if present, otherwise equal to .false.

if (present(var))then
  optio_log=var
else
  optio_log=.false.
end if

END SUBROUTINE soptio_log


END MODULE optional_values

