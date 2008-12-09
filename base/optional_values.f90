module optional_values

use missing_values
IMPLICIT NONE


INTERFACE get_opt
  MODULE PROCEDURE sget_opt_b, sget_opt_s, sget_opt_l, sget_opt_r, sget_opt_d, sget_opt_c
END INTERFACE

PUBLIC

CONTAINS


!FUNCTION

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



!SUBROUTINE

!> Return the optional values if present or missing
subroutine sget_opt_b(var,get_opt_b)

integer(kind=int_b),intent(in),optional  :: var !< variabile da controllare
integer(kind=int_b),intent(out) :: get_opt_b

if (present(var))then
  get_opt_b=var
else
  get_opt_b=ibmiss
end if

return
end subroutine sget_opt_b

!> Return the optional values if present or missing
subroutine sget_opt_s(var,get_opt_s)

integer(kind=int_s),intent(in),optional  :: var !< variabile da controllare
integer(kind=int_s),intent(out) :: get_opt_s

if (present(var))then
  get_opt_s=var
else
  get_opt_s=ismiss
end if

return
end subroutine sget_opt_s




!> Return the optional values if present or missing
subroutine sget_opt_l(var,get_opt_l)

integer(kind=int_l),intent(in),optional  :: var !< variabile da controllare
integer(kind=int_l),intent(out) :: get_opt_l

if (present(var))then
  get_opt_l=var
else
  get_opt_l=ilmiss
end if

return
end subroutine sget_opt_l



!> Return the optional values if present or missing
subroutine sget_opt_r(var,get_opt_r)

real,intent(in),optional  :: var !< variabile da controllare
real,intent(out) :: get_opt_r

if (present(var))then
  get_opt_r=var
else
  get_opt_r=rmiss
end if

return
end subroutine sget_opt_r



!> Return the optional values if present or missing
subroutine sget_opt_d(var,get_opt_d)

real (kind=fp_d),intent(in),optional  :: var !< variabile da controllare
real (kind=fp_d),intent(out) :: get_opt_d

if (present(var))then
  get_opt_d=var
else
  get_opt_d=rdmiss
end if

return
end subroutine sget_opt_d


!> Return the optional values if present or missing
subroutine sget_opt_c(var,get_opt_c)

character (len=*),intent(in),optional  :: var !< variabile da controllare
CHARACTER (len=*),intent(out) :: get_opt_c


if (present(var))then
  get_opt_c=var
else
  get_opt_c=cmiss
end if

return
end subroutine sget_opt_c


end module optional_values

