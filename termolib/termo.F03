module termo

USE alchimia
USE termolib

IMPLICIT NONE

private
public register_termo

contains

subroutine register_termo(vfn)

  type(fndsv),intent(inout) :: vfn

  call fnregister(vfn,t_p2pt_def())

end subroutine register_termo


subroutine  t_p2pt(mybin,mybout,bin,bout,in,out)
  CHARACTER(len=10),intent(in) :: mybin(:)  !< vector table B  WMO input  in user's data
  CHARACTER(len=10),intent(in) :: mybout(:) !< vector table B  WMO output in user's data
  CHARACTER(len=10),intent(in) :: bin(:)    !< vector table B  WMO input used by function
  CHARACTER(len=10),intent(in) :: bout(:)   !< vector table B  WMO output used by function
  real, intent(in) :: in(:,:)
  real, intent(out) :: out(:,:)

  out(:,1)=o(in(:,1),in(:,2))

end subroutine t_p2pt

type(fnds) function t_p2pt_def()
  call init(t_p2pt_def,"t_p2pt",&
       [character(len=10) :: "B12002","B10004"],&
       [character(len=10) :: "B12103"],0,func=t_p2pt)
end function t_p2pt_def

end module termo
