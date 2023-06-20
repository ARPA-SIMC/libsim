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
module list_real
  use list_abstract
  private
  public :: realList
!> Real specific implementation of doubly-linked list
!!
!! extend list_abstract::list
  type, extends(list) :: realList
#ifdef DOXYGEN
  integer::none  ! doxigen workaround: if missed do not show procedure
#endif
   contains
!     procedure :: addReal               !< add real in list
     procedure :: current => currentReal !< get real pointed by iterator
     procedure :: display => displayReal !< print the real list
!     generic :: add => addReal
  end type realList

contains

!> Print the real list
subroutine displayReal(this)
class(realList),intent(inout) :: this

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


!> get real pointed by iterator
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
