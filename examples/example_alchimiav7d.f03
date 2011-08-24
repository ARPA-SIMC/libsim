program alchimiav7d

USE alchimia
USE termo
USE vol7d_class
USE vol7d_dballe_class
USE vol7d_alchimia_class
USE vol7d_var_class
use log4fortran


IMPLICIT NONE
integer :: i,ivar,nbin,nbinn
type(fndsv) :: vfn,myvfn
character(len=10), allocatable:: mybin(:),mybout(:)
type(vol7d_dballe) :: myin,myout
character(len=255) :: filenamein,filenameout

integer :: category,ier
character(len=512):: a_name

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name)

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name)

call l4f_category_log(category,L4F_INFO,"Start")

mybout = [character(len=10) :: "B12192"]
filenamein="../data/example_temp.bufr"
filenameout="../data/tp.bufr"

call register_termo(vfn)

call init(myin,filename=filenamein, file=.true., categoryappend="input")
call init(myout,filename=filenameout, file=.true., write=.true., wipe=.true., categoryappend="output")

!CALL import(myin,var=(/"B12101","B10004"/),varkind=(/"r","r"/))
CALL import(myin)

nbin=0

if (associated(myin%vol7d%dativar%r)) nbin = nbin + size(myin%vol7d%dativar%r)
if (associated(myin%vol7d%dativar%i)) nbin = nbin + size(myin%vol7d%dativar%i)
if (associated(myin%vol7d%dativar%d)) nbin = nbin + size(myin%vol7d%dativar%d)
if (associated(myin%vol7d%dativar%b)) nbin = nbin + size(myin%vol7d%dativar%b)
if (associated(myin%vol7d%dativar%c)) nbin = nbin + size(myin%vol7d%dativar%c)

allocate (mybin(nbin))

print*, nbin

nbin=0
if (associated(myin%vol7d%dativar%r)) then
  nbinn=nbin+size(myin%vol7d%dativar%r)
  mybin(nbin+1:nbinn) = myin%vol7d%dativar%r(:)%btable
  nbin=nbinn
end if

if (associated(myin%vol7d%dativar%i)) then
  nbinn=nbin+size(myin%vol7d%dativar%i)
  mybin(nbin+1:nbinn) = myin%vol7d%dativar%i(:)%btable
  nbin=nbinn
end if

if (associated(myin%vol7d%dativar%d)) then
  nbinn=nbin+size(myin%vol7d%dativar%d)
  mybin(nbin+1:nbinn) = myin%vol7d%dativar%d(:)%btable
  nbin=nbinn
end if

if (associated(myin%vol7d%dativar%b)) then
  nbinn=nbin+size(myin%vol7d%dativar%b)
  mybin(nbin+1:nbinn) = myin%vol7d%dativar%b(:)%btable
  nbin=nbinn
end if

if (associated(myin%vol7d%dativar%c)) then
  nbinn=nbin+size(myin%vol7d%dativar%c)
  mybin(nbin+1:nbinn) = myin%vol7d%dativar%c(:)%btable
end if

print *,"Ho a disposizione:  ",mybin
print *,"Devo preparare:     ",mybout

if (.not. oracle(mybin,mybout,vfn,myvfn)) then
  print*, " non riesco a fare ",mybout
  stop 3
end if

call display(myvfn)
print *,"mi occorrono ",myvfn%nout," variabili in piu"

call make(myvfn,mybin,mybout,myin%vol7d,myout%vol7d)

myout%vol7d%dativar%r(:)%btable=mybout

call export(myout,template="generic")

call delete(myout)
call delete(myin)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program alchimiav7d
