!> Module for quickly interprert the \c OPTIONAL parameters passed
!! to a subprogram.
!! This module defines functions and subroutines that handle in a
!! quick way \c OPTIONAL parameters in a subprogram by returning
!! a useable missing value if a given parameter has not been provided.
!! The module provide a generic subroutine, valid for almost all
!! intrinsic types, and specific functions.
!!
!! \ingroup base
MODULE optional_values
USE missing_values
IMPLICIT NONE

!> Generic subroutine for checking \c OPTIONAL parameterw.
INTERFACE optio
  MODULE PROCEDURE soptio_b, soptio_s, soptio_l, soptio_r, soptio_d, soptio_c, soptio_log
END INTERFACE

PUBLIC

CONTAINS


!FUNCTION

!> Return the optional value if present, otherwise return missing value.
elemental integer(kind=int_b) function optio_b(var)

integer(kind=int_b),intent(in),optional  :: var !< variable to be checked

if (present(var))then
  optio_b=var
else
  optio_b=ibmiss
end if

return
end function optio_b


!> Return the optional value if present, otherwise return missing value.
elemental integer(kind=int_s) function optio_s(var)

integer(kind=int_s),intent(in),optional  :: var !< variable to be checked

if (present(var))then
  optio_s=var
else
  optio_s=ismiss
end if

return
end function optio_s


!> Return the optional value if present, otherwise return missing value.
elemental integer(kind=int_l) function optio_l(var)

integer(kind=int_l),intent(in),optional  :: var !< variable to be checked

if (present(var))then
  optio_l=var
else
  optio_l=ilmiss
end if

return
end function optio_l


!> Return the optional value if present, otherwise return missing value.
elemental real function optio_r(var)

real,intent(in),optional  :: var !< variable to be checked

if (present(var))then
  optio_r=var
else
  optio_r=rmiss
end if

return
end function optio_r


!> Return the optional value if present, otherwise return missing value.
elemental doubleprecision function optio_d(var)

doubleprecision,intent(in),optional  :: var !< variable to be checked

if (present(var))then
  optio_d=var
else
  optio_d=rdmiss
end if

return
end function optio_d


!> Return the optional value if present, otherwise return missing value.
!! N.B. elemental is not possible here.
function optio_c(var,len) result(char)

character (len=*),intent(in),optional  :: var !< variable to be checked
integer , intent(in) :: len
CHARACTER(len=LEN) :: char

if (present(var))then
  char=var
else
  char=cmiss
end if

return
end function optio_c


!> Return the optional value if present, otherwise return \c .FALSE. .
elemental logical function optio_log(var)

logical,intent(in),optional  :: var !< variable to be checked

if (present(var))then
  optio_log=var
else
  optio_log=.false.
end if

return
end function optio_log


!SUBROUTINE

!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
elemental subroutine soptio_b(var,optio_b)

integer(kind=int_b),intent(in),optional  :: var !< variable to be checked
integer(kind=int_b),intent(out) :: optio_b !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_b=var
else
  optio_b=ibmiss
end if

return
end subroutine soptio_b


!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
elemental subroutine soptio_s(var,optio_s)

integer(kind=int_s),intent(in),optional  :: var !< variable to be checked
integer(kind=int_s),intent(out) :: optio_s !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_s=var
else
  optio_s=ismiss
end if

return
end subroutine soptio_s


!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
elemental subroutine soptio_l(var,optio_l)

integer(kind=int_l),intent(in),optional  :: var !< variable to be checked
integer(kind=int_l),intent(out) :: optio_l !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_l=var
else
  optio_l=ilmiss
end if

return
end subroutine soptio_l



!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
elemental subroutine soptio_r(var,optio_r)

real,intent(in),optional  :: var !< variable to be checked
real,intent(out) :: optio_r !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_r=var
else
  optio_r=rmiss
end if

return
end subroutine soptio_r


!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
elemental subroutine soptio_d(var,optio_d)

doubleprecision,intent(in),optional  :: var !< variable to be checked
doubleprecision,intent(out) :: optio_d !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_d=var
else
  optio_d=rdmiss
end if

return
end subroutine soptio_d


!> Set the output value to input, if input is present, otherwise set it
!! to missing value.
elemental subroutine soptio_c(var,optio_c)

character (len=*),intent(in),optional  :: var !< variable to be checked
CHARACTER (len=*),intent(out) :: optio_c !< equal to \a var if present, otherwise equal to the corresponding missing value


if (present(var))then
  optio_c=var
else
  optio_c=cmiss
end if

return
end subroutine soptio_c


!> Set the output value to input, if input is present, otherwise set it
!! to \c .FALSE. .
elemental subroutine soptio_log(var,optio_log)

logical,intent(in),optional  :: var !< variable to be checked
logical,intent(out) :: optio_log !< equal to \a var if present, otherwise equal to the corresponding missing value

if (present(var))then
  optio_log=var
else
  optio_log=.false.
end if

return
end subroutine soptio_log


end module optional_values

