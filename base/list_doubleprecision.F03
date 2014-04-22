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
module list_doubleprecision
  use list_abstract
  private
  public :: doubleprecisionList
!> Double precision specific implementation of doubly-linked list
!!
!! extend list_abstract::list
  type, extends(list) :: doubleprecisionList
#ifdef DOXYGEN
  integer::none  ! doxigen workaround: if missed do not show procedure
#endif
   contains
!     procedure :: addDoubleprecision               !< add doubleprecision in list
     procedure :: current => currentDoubleprecision !< get doubleprecision pointed by iterator
     procedure :: display => displayDoubleprecision !< print the doubleprecision list
!     generic :: add => addDoubleprecision
  end type doubleprecisionList

contains

!> Print the double precision list
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


!> get double precision pointed by iterator
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
