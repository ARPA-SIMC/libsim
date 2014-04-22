!> \brief  class to use lists in fortran 2003.
!!
!! Linked data structure is a data structure which
!! consists of a set of data records (nodes) linked together and organized by references .
!!
!! A doubly-linked list is a linked data structure that consists of a set
!! of sequentially linked records called nodes. Each node contains two
!! fields, called links, that are references to the previous and to the
!! next node in the sequence of nodes. The beginning and ending nodes'
!! previous and next links, respectively, point to some kind of
!! terminator.
!!
!! The program example is the better starting point:
!!\include example_list.F03
!!\ingroup base
!!
module list_integer
  use list_abstract
  private
  public :: integerList

!> Integer specific implementation of doubly-linked list
!!
!! extend list_abstract::list
  type, extends(list) :: integerList
#ifdef DOXYGEN
  integer::none  ! doxigen workaround: if missed do not show procedure
#endif
   contains
!     procedure :: addInteger               !< add integer in list
     procedure :: current => currentInteger !< get integer pointed by iterator
     procedure :: display => displayInteger !< print the integer list
!     generic :: add => addInteger
  end type integerList

contains

!> Print the integer list
subroutine displayInteger(this)
class(integerList) :: this

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:", this%current()
  call this%next()
end do
end subroutine displayInteger

!!$  subroutine addInteger(this, value)
!!$    class(integerList) :: thisb
!!$    integer value
!!$    class(*), allocatable :: v
!!$
!!$    allocate(v,source=value)
!!$    call this%addvalue(v)
!!$
!!$  end subroutine addInteger

!> get integer pointed by iterator
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
