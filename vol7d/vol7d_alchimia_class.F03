module vol7d_alchimia_class

USE vol7d_class
USE alchimia

implicit NONE

interface make
   module procedure  make_v7d
end interface

interface alchemy
   module procedure  alchemy_v7d
end interface

private
public make, alchemy

contains

subroutine make_v7d(mayvfn,mybin,mybout,v7din,v7dout)
type(fndsv),intent(inout) :: mayvfn
character(len=*),intent(in) :: mybin(:),mybout(:)
type(vol7d),intent(inout) :: v7din
type(vol7d),intent(out) :: v7dout
integer :: i,nana,nlevel,ntime,ntimerange,nvarin,nvarout,nnetwork
integer :: ilevel,itime,itimerange,inetwork,ivar,ind
type(vol7d_var) :: var
real,allocatable :: myin(:,:)
character(len=1) :: type

nana=size(v7din%ana)
ntime=size(v7din%time)
nlevel=size(v7din%level)
ntimerange=size(v7din%timerange)
nnetwork=size(v7din%network)
nvarin=size(mybin)
nvarout=size(mybout)

call copy (v7din,v7dout,&
 ldativarr=(/.false./),&
 ldativari=(/.false./),&
 ldativard=(/.false./),&
 ldativarb=(/.false./),&
 ldativarc=(/.false./))

call vol7d_alloc(v7dout, ndativarr=nvarout)

do ivar=1,nvarout
  call init(v7dout%dativar%r(ivar),btable=mybout(ivar))
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

            call mayvfn%fnds(i)%fn(mybin,mybout,mayvfn%fnds(i)%bin,mayvfn%fnds(i)%bout,&
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



subroutine alchemy_v7d(myin,vfn,mybout,myout)

character(len=10),intent(in) :: mybout(:)
type(fndsv),intent(in) :: vfn
type(vol7d),intent(inout) :: myin
type(vol7d),intent(out) :: myout

integer :: i,ivar,nbin,nbinn
type(fndsv) :: myvfn
character(len=10), allocatable:: mybin(:)
character(len=255) :: filenamein,filenameout

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

print *,"Ho a disposizione:  ",mybin
print *,"Devo preparare:     ",mybout

if (.not. oracle(mybin,mybout,vfn,myvfn)) then
  print*, " non riesco a fare ",mybout
  stop 3
end if

call display(myvfn)
print *,"mi occorrono ",myvfn%nout," variabili in piu"

call make(myvfn,mybin,mybout,myin,myout)

end subroutine alchemy_v7d


end module vol7d_alchimia_class
