
module list_real
  use list_abstract
  private
  public :: realList
  type, extends(list) :: realList
   contains
!     procedure :: addReal                    ! add real in list
     procedure :: current => currentReal ! get real pointed by iterator
     procedure :: display => displayReal ! print the real list
!     generic :: add => addReal
  end type realList

contains

subroutine displayReal(this)
class(realList) :: this

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:", this%current()
  call this%next()
end do
end subroutine displayReal

!!$  subroutine addReal(this, value)
!!$    class(realList) :: this
!!$    real value
!!$    class(*), allocatable :: v
!!$
!!$    allocate(v,source=value)
!!$    call this%addvalue(v)
!!$
!!$  end subroutine addReal


real function currentReal(this)
class(realList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (real)
  currentReal = v
end select
end function currentReal

end module list_real
