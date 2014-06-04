!> \brief  class to use character lists in fortran 2003 WARNING !!!! CHAR LEN IS FIXED TO listcharmaxlen.
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
module list_character
  use list_abstractforchar
  use list_linkchar
  private
  public :: characterList, toarray_charl
!> Character specific implementation of doubly-linked list
!!
!! extend list_abstract::list
  type, extends(list) :: characterList
#ifdef DOXYGEN
  integer::none  ! doxigen workaround: if missed do not show procedure
#endif
   contains
!     procedure :: addCharacter               !< add character in list
     procedure :: current => currentCharacter !< get character pointed by iterator
     procedure :: display => displayCharacter !< print the character list
!     generic :: add => addCharacter
  end type characterList

contains

!> Print the character list
subroutine displayCharacter(this)
class(characterList) :: this

call this%rewind()
do while(this%element())

  print *,"index:",this%currentindex()," value:", trim(this%current())
  call this%next()
end do
end subroutine displayCharacter

!!$  subroutine addCharacter(this, value)
!!$    class(characterList) :: this
!!$    character value
!!$    class(*), allocatable :: v
!!$
!!$    allocate(v,source=value)
!!$    call this%addvalue(v)
!!$
!!$  end subroutine addCharacter


!> get character pointed by iterator
function currentCharacter(this)
character(len=listcharmaxlen) :: currentCharacter
class(characterList) :: this

currentCharacter = this%currentpoli()

end function currentCharacter


!> /brief Return an array of char from 
function toarray_charl(this)
character(len=listcharmaxlen),allocatable :: toarray_charl(:) !< array
type(characterlist) :: this !< list of char

integer :: i

allocate (toarray_charl(this%countelements()))

call this%rewind()
i=0
do while(this%element())
  i=i+1
  toarray_charl(i) =this%current()
  call this%next()
end do
end function toarray_charl



end module list_character
