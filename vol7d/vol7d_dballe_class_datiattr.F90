                                !ciclo sul tipo attributo
                                !#define __strunz(s) #s
                                !#define stringa(s) __strunz(s)
                     
                                !print *,"type var e attr ",  stringa(VOL7D_POLY_TYPES_V)  ,  stringa(VOL7D_POLY_TYPES)
                                !print *,ind,ndatiattr/**/VOL7D_POLY_TYPES


ind = this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V/**/(iiiii)%/**/VOL7D_POLY_TYPES

if (ind > 0) then
  do inddatiattr=1,ndatiattr/**/VOL7D_POLY_TYPES
    if (lattr/**/VOL7D_POLY_TYPES (inddatiattr))then
      if (c_e(this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES/**/(i,ii,iii,iiii,ind,iiiiii,inddatiattr)))then
                                !print*,"attr ",this%vol7d%datiattr%/**/VOL7D_POLY_TYPES/**/(inddatiattr)%btable,&
                                !this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES/**/(i,ii,iii,iiii,ind,iiiiii,inddatiattr)
        call idba_set (this%handle, this%vol7d%datiattr%/**/VOL7D_POLY_TYPES/**/(inddatiattr)%btable,&
         this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES/**/(i,ii,iii,iiii,ind,iiiiii,inddatiattr))
        writeattr=.true.
      end if
    end if
  end do
end if
                     
                                !fine ciclo sul tipo attributo
