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
module list_byte
  use list_abstract
  use kinds
  private
  public :: byteList
!> Byte specific implementation of doubly-linked list
!!
!! extend list_abstract::list
  type, extends(list) :: byteList
#ifdef DOXYGEN
  integer::none  ! doxigen workaround: if missed do not show procedure
#endif
   contains
!     procedure :: addByte               !< add byte in list
     procedure :: current => currentByte !< get byte pointed by iterator
     procedure :: display => displayByte !< print the byte list
!     generic :: add => addByte
  end type byteList

contains

!> Print the byte list
subroutine displayByte(this)
class(byteList),intent(inout) :: this

call this%rewind()
do while(this%element())
  print *,"index:",this%currentindex()," value:", this%current()
  call this%next()
end do
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


!> get byte pointed by iterator
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
