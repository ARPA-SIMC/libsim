module vol7d_alchimia_class

USE vol7d_class
USE alchimia

implicit NONE

interface make
   module procedure  make_v7d
end interface

private
public make

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

end module vol7d_alchimia_class
