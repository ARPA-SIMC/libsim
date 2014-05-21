!> \brief  class to manage links for lists in fortran 2003.
!!
!! Linked data structure is a data structure which
!! consists of a set of data records (nodes) linked together and organized by references.
!! This module is used by other lists modules only for character.
!!\ingroup base
module list_linkchar
  parameter (listcharmaxlen=10)

  private
  public :: link, listcharmaxlen
!> Base type to manage links for lists
  type link
     private
     character(len=listcharmaxlen) :: value = ""       !< value stored in link
     type(link), pointer :: next => null()!< next link in list
     type(link), pointer :: prev => null()!< next link in list
     contains
     procedure :: getValue    !< return value pointer
     procedure :: nextLink    !< return next pointer
     procedure :: prevLink    !< return next pointer
     procedure :: setNextLink !< set next pointer
     procedure :: setPrevLink !< set next pointer
  end type link

!> User-defined constructors => list_link::constructor
  interface link
   procedure constructor !< construct/initialize a link
  end interface

contains

function nextLink(this)
class(link) :: this
class(link), pointer :: nextLink
nextLink => this%next
end function nextLink

function prevLink(this)
class(link) :: this
class(link), pointer :: prevLink
prevLink => this%prev
end function prevLink
    
subroutine setNextLink(this,next)
class(link) :: this
type(link), pointer :: next
this%next => next
end subroutine setNextLink

subroutine setPrevLink(this,prev)
class(link) :: this
type(link), pointer :: prev
this%prev => prev
end subroutine setPrevLink

function getValue(this)
class(link) :: this
character(len=listcharmaxlen) :: getValue
getValue = this%value
end function getValue

!> Constructor
function constructor(value)
type(link),pointer :: constructor
character (len=*)  :: value !< value for list

allocate(constructor)
constructor%prev => null()
constructor%next => null()
constructor%value=value

end function constructor

end module list_linkchar
