
module list_mix
use list_abstract
use kinds
private
public :: mixList
type, extends(list) :: mixList
  contains
  procedure :: current => currentmix ! get integer pointed by iterator
  procedure :: display => displaymix ! print the integer list
end type mixList

contains

subroutine displaymix(this)
class(mixList) :: this
class(*), pointer :: curr
logical :: found

call this%start()
do while(this%more())
  curr => this%Currentpoli()
  found=.false.
  select type(curr)
  type is (integer)
    print *,curr
    found=.true.
  type is (real)
    print *,curr
    found=.true.
  type is (doubleprecision)
    print *,curr
    found=.true.
  type is (integer(kind=int_b))
    print *,curr
    found=.true.
  type is (character(*))
    print *,curr
    found=.true.
  end select

  if (.not. found) then
    print *, "not supported type for display"
  end if

  call this%next()
end do
call this%start()
end subroutine displaymix


function currentmix(this)
class(mixlist) :: this
class(*), pointer :: Currentmix
Currentmix => this%currentpoli()
end function Currentmix


end module list_mix
