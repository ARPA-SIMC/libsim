module forno

USE alchimia

IMPLICIT NONE

private
public register_forno

contains

subroutine register_forno(vfn)

  type(fndsv),intent(inout) :: vfn

  call fnregister(vfn,inforna_def())
  call fnregister(vfn,rosola_def())
  call fnregister(vfn,ripassa_def())
  call fnregister(vfn,brucia_def())

end subroutine register_forno


subroutine  inforna(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out=5.
end subroutine inforna

type(fnds) function inforna_def()
  call init(inforna_def,"inforna",&
       [character(len=10) :: "farina","lievito"],&
       [character(len=10) :: "pane"],0,func=inforna)
end function inforna_def



subroutine rosola(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out=6.

end subroutine rosola

type(fnds) function rosola_def()
  call init(rosola_def,"rosola",&
       [character(len=10) :: "patate","sale"],&
       [character(len=10) :: "patatine"],0,func=rosola)
end function rosola_def


subroutine ripassa(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out=7.

end subroutine ripassa

type(fnds) function ripassa_def()
  call init(ripassa_def,"ripassa",&
       [character(len=10) :: "patatine","pole.frit."],&
       [character(len=10) :: "pole.pata."],0,func=ripassa)
end function ripassa_def


subroutine brucia(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out=8.

end subroutine brucia

type(fnds) function brucia_def()
  call init(brucia_def,"brucia",&
       [character(len=10) :: "patate","sale"],&
       [character(len=10) :: "carboncini"],0,func=brucia)
end function brucia_def



end module forno
