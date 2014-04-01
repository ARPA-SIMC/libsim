
module list_doubleprecision
  use list_abstract
  private
  public :: doubleprecisionList
  type, extends(list) :: doubleprecisionList
   contains
!     procedure :: addDoubleprecision                    ! add doubleprecision in list
     procedure :: current => currentDoubleprecision ! get doubleprecision pointed by iterator
     procedure :: display => displayDoubleprecision ! print the doubleprecision list
!     generic :: add => addDoubleprecision
  end type doubleprecisionList

contains

subroutine displayDoubleprecision(this)
class(doubleprecisionList) :: this

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:", this%current()
  call this%next()
end do
end subroutine displayDoubleprecision

!!$  subroutine addDoubleprecision(this, value)
!!$    class(doubleprecisionList) :: this
!!$    doubleprecision value
!!$    class(*), allocatable :: v
!!$
!!$    allocate(v,source=value)
!!$    call this%addvalue(v)
!!$
!!$  end subroutine addDoubleprecision


doubleprecision function currentDoubleprecision(this)
class(doubleprecisionList) :: this
class(*), pointer :: v

v => this%currentpoli()
select type(v)
type is (doubleprecision)
  currentDoubleprecision = v
end select
end function currentDoubleprecision

end module list_doubleprecision