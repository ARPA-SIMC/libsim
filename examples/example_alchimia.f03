program cucina

USE alchimia
USE forno
USE pentolone
USE missing_values
USE log4fortran

IMPLICIT NONE
integer :: i
integer, parameter :: ndat=100
type(fndsv) :: vfn,myvfn
character(len=10), allocatable:: mybin(:),mybout(:)
real,allocatable :: myin(:,:),myout(:,:)
integer :: category,ier
CHARACTER(len=512):: a_name

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="volgrid6dtransform")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")


call register_pentolone(vfn)
call register_forno(vfn)

!mybin  = [character(len=10)::"acqua","olio","patate","sale","pollo","mais","gommosa"]
!mybout = [character (len=10) :: "pole.pata.","lesso"]

mybin  = [character(len=10)::"acqua","olio","patate","sale","pollo","mais","gommosa"]
mybout = [character (len=10) :: "pole.pata.","pane"]

print *,"I have:  ",mybin
print *,"I have to prepare:     ",mybout

if (.not. oracle(mybin,mybout,vfn,myvfn)) then
  print*, "I cannot make ",mybout

  if (.not. shoppinglist(mybout,vfn,myvfn)) then
    print*, " error shoppinglist"
    stop 2
  else
        call display(myvfn)
        call display(compile_sl(myvfn))
    stop 3
  end if
end if

call display(myvfn)
print *,"I need ",myvfn%nout," more variables"


allocate(myin(ndat,size(mybin)))
myin=1.5
allocate(myout(ndat,myvfn%nout))
myout=rmiss
call make(myvfn,mybin,mybout,myin,myout)

                                !chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program cucina
