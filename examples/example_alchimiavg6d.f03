program alchimiavg6d

USE alchimia
USE termo
USE volgrid6d_class
USE volgrid6d_alchimia_class
USE volgrid6d_var_class
USE vol7d_var_class

IMPLICIT NONE
integer :: i,nvar,ivar
type(fndsv) :: vfn,myvfn
character(len=10), allocatable:: mybin(:),mybout(:)
type(volgrid6d),pointer :: myin(:),myout(:)
TYPE(vol7d_var),allocatable :: varbufr(:)
TYPE(conv_func), pointer :: c_func(:)

call register_termo(vfn)


CALL import(myin,filename="../data/in.grb",decode=.true., time_definition=0, categoryappend="input")

allocate(myout(size(myin)))

do i=1,size(myin)

  nvar=size(myin(i)%var)
  allocate(varbufr(nvar))
  CALL vargrib2varbufr(myin(i)%var, varbufr, c_func)

  IF (ASSOCIATED(c_func)) THEN

    DO ivar = 1, nvar
      myin(i)%voldati(:,:,:,:,:,ivar) = convert(c_func(ivar),myin(i)%voldati(:,:,:,:,:,ivar))
    ENDDO
    
  ENDIF

!  mybin  = [character(len=10) :: "B12002","B10004"]
  mybin  = [character(len=10) :: "B12002"]
  mybout = [character(len=10) :: "B12103"]

  print *,"Ho a disposizione:  ",mybin
  print *,"Devo preparare:     ",mybout

  if (.not. oracle(mybin,mybout,vfn,myvfn)) then
    print*, " non riesco a fare ",mybout
    stop 3
  end if

  call display(myvfn)
  print *,"mi occorrono ",myvfn%nout," variabili in piu"

  call make(myvfn,mybin,mybout,myin(i),myout(i))

  call export(myout(i))
  call delete(myout(i))
    
end do

call delete(myin)
DEALLOCATE(c_func)
deallocate(myout)


end program alchimiavg6d
