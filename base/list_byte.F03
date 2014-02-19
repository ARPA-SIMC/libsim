
module list_byte
  use list_abstract
  use kinds
  private
  public :: byteList
  type, extends(list) :: byteList
   contains
!     procedure :: addByte                    ! add byte in list
     procedure :: current => currentByte ! get byte pointed by iterator
     procedure :: display => displayByte ! print the byte list
!     generic :: add => addByte
  end type byteList

contains

subroutine displayByte(this)
class(byteList) :: this
class(*), pointer :: curr

call this%start()
do while(this%more())
  curr => this%Currentpoli()
  select type(curr)
  type is (integer(kind=int_b))
    print *, curr
  end select
  call this%next()
end do
call this%start()
end subroutine displayByte

!!$  subroutine addByte(this, value)
!!$    class(byteList) :: this
!!$    byte value
!!$    class(*), allocatable :: v
!!$
!!$    allocate(v,source=value)
!!$    call this%addvalue(v)
!!$
!!$  end subroutine addByte


integer(kind=int_b) function currentByte(this)
class(byteList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (integer(kind=int_b))
  currentByte = v
end select
end function currentByte

end module list_byte
