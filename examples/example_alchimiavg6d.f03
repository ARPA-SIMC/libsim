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
TYPE(vol7d_var),allocatable :: varv7d(:)
TYPE(volgrid6d_var),allocatable :: varvg6d(:)
TYPE(conv_func), pointer :: c_func(:)
character(len=255) :: filenamein,filenameout

mybout = [character(len=10) :: "B12192"]
filenamein="../data/t_p.grb"
filenameout="../data/tp.grb"

call register_termo(vfn)

CALL import(myin,filename=filenamein,decode=.true., time_definition=0, categoryappend="input")

allocate(myout(size(myin)))

do i=1,size(myin)

  nvar=size(myin(i)%var)
  allocate(varv7d(nvar))
  CALL vargrib2varbufr(myin(i)%var, varv7d, c_func)

  print *,"varv7d"
  print *,varv7d

  IF (ASSOCIATED(c_func)) THEN
    DO ivar = 1, nvar
      myin(i)%voldati(:,:,:,:,:,ivar) = convert(c_func(ivar),myin(i)%voldati(:,:,:,:,:,ivar))
    ENDDO
  ENDIF
  DEALLOCATE(c_func)

  mybin=varv7d(:)%btable
  deallocate(varv7d)

  print *,"Ho a disposizione:  ",mybin
  print *,"Devo preparare:     ",mybout

  if (.not. oracle(mybin,mybout,vfn,myvfn)) then
    print*, " non riesco a fare ",mybout
    stop 3
  end if

  call display(myvfn)
  print *,"mi occorrono ",myvfn%nout," variabili in piu"

  call make(myvfn,mybin,mybout,myin(i),myout(i))

  nvar=size(mybout)
  allocate (varv7d(nvar))
  allocate(varvg6d(nvar))
  
  do ivar = 1, nvar
    call init(varv7d(ivar),mybout(ivar))
  end DO

  CALL varbufr2vargrib(varv7d, varvg6d, c_func)

  IF (ASSOCIATED(c_func)) THEN
    DO ivar = 1, nvar
      myout(i)%voldati(:,:,:,:,:,ivar) = convert(c_func(ivar),myout(i)%voldati(:,:,:,:,:,ivar))
    ENDDO
  ENDIF
  myout(i)%var=varvg6d
  DEALLOCATE(c_func)
  deallocate (varv7d)

  print *,"varvg6d"
  print *,varvg6d

  
end do

call export(myout,filenameout)

call delete(myout)
call delete(myin)

end program alchimiavg6d
