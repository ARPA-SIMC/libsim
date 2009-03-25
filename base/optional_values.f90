!> \brief  parse command options (enhanced)
!!
!! Questo modulo definisce delle funzioni e delle subroutine per
!! gestire comodamente i parametri \c OPTIONAL all'interno di
!! programmi.
!!
!! \ingroup base
module optional_values

use missing_values
IMPLICIT NONE

!> Generica subroutine per controllare i parametri OPTIONAL.
INTERFACE optio
  MODULE PROCEDURE soptio_b, soptio_s, soptio_l, soptio_r, soptio_d, soptio_c, soptio_log
END INTERFACE

PUBLIC

CONTAINS


!FUNCTION

!> Return the optional values if present or missing
elemental integer(kind=int_b) function optio_b(var)

integer(kind=int_b),intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  optio_b=var
else
  optio_b=ibmiss
end if

return
end function optio_b

!> Return the optional values if present or missing
elemental integer(kind=int_s) function optio_s(var)

integer(kind=int_s),intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  optio_s=var
else
  optio_s=ismiss
end if

return
end function optio_s




!> Return the optional values if present or missing
elemental integer(kind=int_l) function optio_l(var)

integer(kind=int_l),intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  optio_l=var
else
  optio_l=ilmiss
end if

return
end function optio_l



!> Return the optional values if present or missing
elemental real function optio_r(var)

real,intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  optio_r=var
else
  optio_r=rmiss
end if

return
end function optio_r



!> Return the optional values if present or missing
elemental real (kind=fp_d)  function optio_d(var)

real (kind=fp_d),intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  optio_d=var
else
  optio_d=rdmiss
end if

return
end function optio_d


!> Return the optional values if present or missing
!! N.B. elemental is not possible here
function optio_c(var,len) result(char)

character (len=*),intent(in),optional  :: var !< variabile da controllare
integer , intent(in) :: len
CHARACTER(len=LEN) :: char

if (present(var))then
  char=var
else
  char=cmiss
end if

return
end function optio_c



!> Return the optional values if present or missing
elemental logical function optio_log(var)

logical,intent(in),optional  :: var !< variabile da controllare

if (present(var))then
  optio_log=var
else
  optio_log=.false.
end if

return
end function optio_log




!SUBROUTINE

!> Return the optional values if present or missing
elemental subroutine soptio_b(var,optio_b)

integer(kind=int_b),intent(in),optional  :: var !< variabile da controllare
integer(kind=int_b),intent(out) :: optio_b !< variabile pari a var se presente altrimenti missing

if (present(var))then
  optio_b=var
else
  optio_b=ibmiss
end if

return
end subroutine soptio_b

!> Return the optional values if present or missing
elemental subroutine soptio_s(var,optio_s)

integer(kind=int_s),intent(in),optional  :: var !< variabile da controllare
integer(kind=int_s),intent(out) :: optio_s !< variabile pari a var se presente altrimenti missing

if (present(var))then
  optio_s=var
else
  optio_s=ismiss
end if

return
end subroutine soptio_s




!> Return the optional values if present or missing
elemental subroutine soptio_l(var,optio_l)

integer(kind=int_l),intent(in),optional  :: var !< variabile da controllare
integer(kind=int_l),intent(out) :: optio_l !< variabile pari a var se presente altrimenti missing

if (present(var))then
  optio_l=var
else
  optio_l=ilmiss
end if

return
end subroutine soptio_l



!> Return the optional values if present or missing
elemental subroutine soptio_r(var,optio_r)

real,intent(in),optional  :: var !< variabile da controllare
real,intent(out) :: optio_r !< variabile pari a var se presente altrimenti missing

if (present(var))then
  optio_r=var
else
  optio_r=rmiss
end if

return
end subroutine soptio_r



!> Return the optional values if present or missing
elemental subroutine soptio_d(var,optio_d)

real (kind=fp_d),intent(in),optional  :: var !< variabile da controllare
real (kind=fp_d),intent(out) :: optio_d !< variabile pari a var se presente altrimenti missing

if (present(var))then
  optio_d=var
else
  optio_d=rdmiss
end if

return
end subroutine soptio_d


!> Return the optional values if present or missing
elemental subroutine soptio_c(var,optio_c)

character (len=*),intent(in),optional  :: var !< variabile da controllare
CHARACTER (len=*),intent(out) :: optio_c !< variabile pari a var se presente altrimenti missing


if (present(var))then
  optio_c=var
else
  optio_c=cmiss
end if

return
end subroutine soptio_c


!> Return the optional values if present or missing
!!assume .false. for default
elemental subroutine soptio_log(var,optio_log)

logical,intent(in),optional  :: var !< variabile da controllare
logical,intent(out) :: optio_log !< variabile pari a var se presente altrimenti missing

if (present(var))then
  optio_log=var
else
  optio_log=.false.
end if

return
end subroutine soptio_log



end module optional_values

