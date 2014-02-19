
module list_integer
  use list_abstract
  private
  public :: integerList
  type, extends(list) :: integerList
   contains
!     procedure :: addInteger                    ! add integer in list
     procedure :: current => currentInteger ! get integer pointed by iterator
     procedure :: display => displayInteger ! print the integer list
!     generic :: add => addInteger
  end type integerList

contains

subroutine displayInteger(this)
class(integerList) :: this
class(*), pointer :: curr

call this%start()
do while(this%more())
  curr => this%Currentpoli()
  select type(curr)
  type is (integer)
    print *, curr
  end select
  call this%next()
end do
call this%start()
end subroutine displayInteger

!!$  subroutine addInteger(this, value)
!!$    class(integerList) :: this
!!$    integer value
!!$    class(*), allocatable :: v
!!$
!!$    allocate(v,source=value)
!!$    call this%addvalue(v)
!!$
!!$  end subroutine addInteger


integer function currentInteger(this)
class(integerList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (integer)
  currentInteger = v
end select
end function currentInteger

end module list_integer
