                                !ciclo sul tipo attributo
                                !#define __strunz(s) #s
                                !#define stringa(s) __strunz(s)
                     
                                !print *,"type var e attr ",  stringa(VOL7D_POLY_TYPES_V)  ,  stringa(VOL7D_POLY_TYPES)
                                !print *,ind,ndatiattr/**/VOL7D_POLY_TYPES


!! (indana,indanavar,indnetwork )

ind = this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V/**/(ii)%/**/VOL7D_POLY_TYPES

if (ind > 0) then
  do indanaattr=1,nanaattr/**/VOL7D_POLY_TYPES
    if (lanaattr/**/VOL7D_POLY_TYPES (indanaattr))then
      if (c_e(this%vol7d%volanaattr/**/VOL7D_POLY_TYPES/**/(i,ind,iii,indanaattr)))then
                                !print*,"attr ",this%vol7d%datiattr%/**/VOL7D_POLY_TYPES/**/(indanaattr)%btable,&
                                !this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES/**/(i,ind,iii,indanaattr)
        call idba_set (this%handle, this%vol7d%anaattr%/**/VOL7D_POLY_TYPES/**/(indanaattr)%btable,&
         this%vol7d%volanaattr/**/VOL7D_POLY_TYPES/**/(i,ind,iii,indanaattr))
        writeattr=.true.
      end if
    end if
  end do
end if
                     
                                !fine ciclo sul tipo attributo
