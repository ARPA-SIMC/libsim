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
module list_mix
use list_abstract
use kinds
private
!> Non type specific implementation of doubly-linked list
!!
!! extend list_abstract::list
type,public,extends(list) :: mixList
#ifdef DOXYGEN
  integer::none  ! doxigen workaround: if missed do not show procedure
#endif
  contains
  procedure :: current => currentmix !< get integer pointed by iterator
  procedure :: display => displaymix !< print the integer list
end type mixList

contains

!> Print the list
subroutine displaymix(this)
class(mixList) :: this
class(*), pointer :: curr
logical :: found

call this%rewind()
do while(this%element())
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
call this%rewind()
end subroutine displaymix


!> get class(*) pointed by iterator
function currentmix(this)
class(mixlist) :: this
class(*), pointer :: Currentmix
Currentmix => this%currentpoli()
end function Currentmix


end module list_mix
