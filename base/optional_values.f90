module optional_values

use missing_values
IMPLICIT NONE


INTERFACE get_opt
  MODULE PROCEDURE get_opt_b, get_opt_s, get_opt_l, get_opt_r, get_opt_d, get_opt_c
END INTERFACE

PUBLIC

CONTAINS

!> Return the optional values if present or missing
integer(kind=int_b) function get_opt_b(var)

integer(kind=int_b),intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  get_opt_b=var
else
  get_opt_b=ibmiss
end if

return
end function get_opt_b

!> Return the optional values if present or missing
integer(kind=int_s) function get_opt_s(var)

integer(kind=int_s),intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  get_opt_s=var
else
  get_opt_s=ismiss
end if

return
end function get_opt_s




!> Return the optional values if present or missing
integer(kind=int_l) function get_opt_l(var)

integer(kind=int_l),intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  get_opt_l=var
else
  get_opt_l=ilmiss
end if

return
end function get_opt_l



!> Return the optional values if present or missing
real function get_opt_r(var)

real,intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  get_opt_r=var
else
  get_opt_r=rmiss
end if

return
end function get_opt_r



!> Return the optional values if present or missing
real (kind=fp_d)  function get_opt_d(var)

real (kind=fp_d),intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  get_opt_d=var
else
  get_opt_d=rdmiss
end if

return
end function get_opt_d


!> Return the optional values if present or missing
character function get_opt_c(var,len) result(char)

character (len=*),intent(in),optional  :: var !< variabile da controllare
integer , intent(in) :: len
CHARACTER(len=LEN) :: char

if (present(var))then
  char=var
else
  char=cmiss
end if

return
end function get_opt_c


end module optional_values

