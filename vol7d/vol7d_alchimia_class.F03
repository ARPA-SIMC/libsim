module vol7d_alchimia_class
#include "config.h"

USE vol7d_class
USE alchimia
USE log4fortran
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif
implicit NONE

interface make
   module procedure  make_v7d
end interface

interface alchemy
   module procedure  alchemy_v7d
end interface

private
public make, alchemy, v7d_all_var, sl_display_pretty

contains


function pretty_var(var)
CHARACTER(len=80) :: pretty_var
character(len=*) :: var

integer :: ind

#ifdef HAVE_DBALLE
type(vol7d_var),pointer :: dballevar(:)

call vol7d_dballe_import_dballevar(dballevar)
ind=index_c(dballevar(:)%btable,var)

if (ind > 0 )then
  pretty_var=dballevar(ind)%description
else
  pretty_var=var
end if
#else

pretty_var=var

#endif
end function pretty_var


subroutine make_v7d(mayvfn,mybin,mybout,v7din,v7dout)
type(fndsv),intent(inout) :: mayvfn
character(len=*),intent(in) :: mybin(:),mybout(:)
type(vol7d),intent(inout) :: v7din
type(vol7d),intent(out) :: v7dout
integer :: i,j,nana,nlevel,ntime,ntimerange,nvarin,nvarout,nnetwork
integer :: ilevel,itime,itimerange,inetwork,ivar,ind
type(vol7d_var) :: var
real,allocatable :: myin(:,:)
character(len=1) :: type
character(len=10) :: newbout(mayvfn%nout)

nana=size(v7din%ana)
ntime=size(v7din%time)
nlevel=size(v7din%level)
ntimerange=size(v7din%timerange)
nnetwork=size(v7din%network)
nvarin=size(mybin)
nvarout=mayvfn%nout

call copy (v7din,v7dout,&
 ldativarr=(/.false./),&
 ldativari=(/.false./),&
 ldativard=(/.false./),&
 ldativarb=(/.false./),&
 ldativarc=(/.false./))

call vol7d_alloc(v7dout, ndativarr=nvarout)

newbout=cmiss
do i=1, size(mayvfn%fnds)
  if (c_e(mayvfn%fnds(i))) then
    do j=1, size(mayvfn%fnds(i)%bout)
      if (c_e(mayvfn%fnds(i)%bout(j))) then
        if (index_c(newbout,mayvfn%fnds(i)%bout(j)) <= 0) then
          newbout(index_c(newbout,cmiss)) = mayvfn%fnds(i)%bout(j)
        end if
      end if
    end do
  end if
end do

do ivar=1,nvarout
  call init(v7dout%dativar%r(ivar),btable=newbout(ivar))
end do

call vol7d_alloc_vol(v7dout,inivol=.true.)

allocate (myin(nana,nvarin))

do i=size(mayvfn%fnds),1,-1
  if (c_e(mayvfn%fnds(i))) then
    do ilevel=1,nlevel
      do itime=1,ntime
        do itimerange=1,ntimerange
          do inetwork=1,nnetwork
            v7dout%voldatir(:,itime,ilevel,itimerange,:,inetwork)=rmiss

            do ivar=1,nvarin
              call init(var, btable=mybin(ivar))

              type=cmiss
              ind = index(v7din%dativar, var, type=type)

              select case (type)

              case("d")
                myin(:,ivar)=realdat(v7din%voldatid(:,itime,ilevel,itimerange,ind,inetwork),v7din%dativar%d(ind))
                                                           
              case("r")                                    
                myin(:,ivar)=realdat(v7din%voldatir(:,itime,ilevel,itimerange,ind,inetwork),v7din%dativar%r(ind))
                                                           
              case("i")                                    
                myin(:,ivar)=realdat(v7din%voldatii(:,itime,ilevel,itimerange,ind,inetwork),v7din%dativar%i(ind))
                                                           
              case("b")                                    
                myin(:,ivar)=realdat(v7din%voldatib(:,itime,ilevel,itimerange,ind,inetwork),v7din%dativar%b(ind))
                                                           
              case("c")                                    
                myin(:,ivar)=realdat(v7din%voldatic(:,itime,ilevel,itimerange,ind,inetwork),v7din%dativar%c(ind))
                
              case default
                
                myin(:,ivar)=rmiss

              end select

            end do

            call mayvfn%fnds(i)%fn(mybin,newbout,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout,&
             myin,&
             v7dout%voldatir(:,ilevel,itime,itimerange,:,inetwork))

          end do
        end do
      end do
    end do
  end if
end do

deallocate (myin)

end subroutine make_v7d


subroutine v7d_all_var(myin,mybin)

type(vol7d),intent(in) :: myin
character(len=10), allocatable:: mybin(:)
integer :: nbin,nbinn

nbin=0

if (associated(myin%dativar%r)) nbin = nbin + size(myin%dativar%r)
if (associated(myin%dativar%i)) nbin = nbin + size(myin%dativar%i)
if (associated(myin%dativar%d)) nbin = nbin + size(myin%dativar%d)
if (associated(myin%dativar%b)) nbin = nbin + size(myin%dativar%b)
if (associated(myin%dativar%c)) nbin = nbin + size(myin%dativar%c)

allocate (mybin(nbin))

nbin=0
if (associated(myin%dativar%r)) then
  nbinn=nbin+size(myin%dativar%r)
  mybin(nbin+1:nbinn) = myin%dativar%r(:)%btable
  nbin=nbinn
end if

if (associated(myin%dativar%i)) then
  nbinn=nbin+size(myin%dativar%i)
  mybin(nbin+1:nbinn) = myin%dativar%i(:)%btable
  nbin=nbinn
end if

if (associated(myin%dativar%d)) then
  nbinn=nbin+size(myin%dativar%d)
  mybin(nbin+1:nbinn) = myin%dativar%d(:)%btable
  nbin=nbinn
end if

if (associated(myin%dativar%b)) then
  nbinn=nbin+size(myin%dativar%b)
  mybin(nbin+1:nbinn) = myin%dativar%b(:)%btable
  nbin=nbinn
end if

if (associated(myin%dativar%c)) then
  nbinn=nbin+size(myin%dativar%c)
  mybin(nbin+1:nbinn) = myin%dativar%c(:)%btable
end if


end subroutine v7d_all_var


integer function  alchemy_v7d(myin,vfn,mybout,myout,copy,vfnoracle)

character(len=10),intent(in) :: mybout(:)
type(fndsv),intent(in) :: vfn
type(vol7d),intent(inout) :: myin
type(vol7d),intent(out) :: myout
logical,intent(in),optional :: copy !< if .true. the copy functions are localy added to vfn (you can have input variable copyed to output)
type(fndsv),intent(out),optional :: vfnoracle

integer :: i
type(fndsv) :: vfntmp, myvfn
character(len=10), allocatable:: mybin(:)
character(len=255) :: filenamein,filenameout

alchemy_v7d = 0

call v7d_all_var(myin,mybin)

vfntmp=vfn
if (optio_log(copy)) call register_copy(vfntmp,mybin)

do i=1,size(mybin)
  call l4f_log(L4F_INFO,"alchemy_v7d: I have:   "//mybin(i))
end do

do i=1,size(mybout)
  call l4f_log(L4F_INFO,"alchemy_v7d: To make:  "//mybout(i))
end do

if (.not. oracle(mybin,mybout,vfntmp,myvfn)) then
  call l4f_log(L4F_WARN,"alchemy_v7d: I cannot make your request")
  alchemy_v7d = 1
  if(.not. shoppinglist(mybout,vfntmp,myvfn,copy=optio_log(copy))) then
    call l4f_log(L4F_WARN,"shoppinglist: return error status")
    alchemy_v7d = 2
  end if
  if (present(vfnoracle)) vfnoracle=myvfn
  return
end if

if (present(vfnoracle)) vfnoracle=myvfn

!call display(myvfn)
call l4f_log(L4F_INFO,"alchemy_v7d: I need "//t2c(myvfn%nout)//" variables")

call make(myvfn,mybin,mybout,myin,myout)

call delete(myvfn)
call delete(vfntmp)

end function alchemy_v7d


!> Display shopping lists on screen more pretty than simple display
subroutine sl_display_pretty(sl)
type(shoplists),intent(in) :: sl !< shopping lists to display

integer :: i,j

do i = 1, size(sl%shoplist)
  print *,"shopping list : ",i
  do j=1,size(sl%shoplist(i)%bvar)
    print *,"required var  : ",sl%shoplist(i)%bvar(j)," -> ",pretty_var(sl%shoplist(i)%bvar(j))
  end do
  print *,""
end do

end subroutine sl_display_pretty


end module vol7d_alchimia_class
