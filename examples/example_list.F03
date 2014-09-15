program example_list

use list_mix
use list_integer
use list_real
use list_character
use kinds

implicit none
type(integerlist) :: list_int
type(reallist) :: list_real
type(characterlist) :: list_char
type(mixlist) :: list_mix

class(*),pointer :: val
integer values(10)
integer i,n

type mytype
  integer :: i
  real    :: r
end type mytype
type(mytype) :: ir=mytype(3,6.)

type charmylen
  character(len=10) :: char10
end type charmylen
type(charmylen) :: mychar


                                ! real list
call list_real%append(3.14)
call list_real%append(6.28)
print *,"display real list"
call list_real%display()

!!$! do not work !!!
!!$! compiler bug ?
!!$                                ! char fixed len (10) list
call list_char%append("hello world")
call list_char%append("bye bye baby")
print *,"display char list"
call list_char%display()


                                ! test integer list
do i=1, 10
  call list_int%append(i)
enddo
print *,"display integer list"
call list_int%display()
print *
call list_int%rewind()
i = 1
do while(list_int%element())
  values(i) = list_int%current()
  call list_int%next()
  i = i + 1
end do
print *,"display integer vector from list"
print *, values

!more easy
values =list_int%toarray()
print *,"display integer vector from list with toarray"
print *, values

! reverse
call list_int%forward()
i = 1
do while(list_int%element())
  values(i) = list_int%current()
  call list_int%prev()
  i = i + 1
end do
print *,"display inverse integer vector from list"
print *, values

print *,"seek return status =", list_int%seek(3)
print *,"list index 3 =", list_int%current()

                                ! test remove
print *,"delete(5) return status =", list_int%delete(5)
print *,"display integer vector with 5 removed"
call list_int%display()
! reverse
print *,"display reverse integer vector with 5 removed"
call list_int%forward()
do while(list_int%element())
  print*, "index:",list_int%currentindex()," value:",list_int%current()
  call list_int%prev()
end do


print *,"delete(1) return status =", list_int%delete(1)
print *,"display integer vector with 1 removed"
call list_int%display()

n=list_int%countelements()
print *,"number of list elements=",n

print *,"delete(",n,")"
print *,"return status =", list_int%delete(n)
print *,"display integer vector with last removed"
call list_int%display()

print *,"delete return status =", list_int%delete()
print *,"display integer vector with everithings removed"
call list_int%display()

                                ! test mix list
do i=1, 10
  call list_mix%append(i)
enddo
call list_mix%append(1.23)
call list_mix%append(4d0)
call list_mix%append(ir)
                                ! this do not work !
                                ! compiler bug ???
                                !  call list_mix%append("test1")
                                !  call list_mix%append("test2")

                                ! but this should work

mychar%char10="ciao ciao"
call list_mix%append(mychar)

print *,"display mix list"
call list_mix%display()

print *,"print mix list with cast"
call list_mix%rewind()
do while (list_mix%element())
  
  val => list_mix%current()
  select type (x => val)
  type is (integer)
    print *,x
  type is (real)
    print *,x
  type is (doubleprecision)
    print *,x
  type is (integer(kind=int_b))
    print *,x
  type is (character(*))
    print *,x
  type is (mytype)
    print *,x%i,x%r
  type is (charmylen)
    print *,x%char10
  end select
  
  call list_mix%next()
  
end do

end program example_list
