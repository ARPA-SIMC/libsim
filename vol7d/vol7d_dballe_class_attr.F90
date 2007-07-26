!ciclo sul tipo attributo
                     ind = this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V/**/(iiiii)%/**/VOL7D_POLY_TYPES
                     
                     writeattr=.false.

                     if (ind > 0) then
                        do inddatiattr=1,ndatiattr/**/VOL7D_POLY_TYPES
                           if (c_e(this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES (i,ii,iii,iiii,ind,iiiiii,inddatiattr)))then
                              if (lattr/**/VOL7D_POLY_TYPES (inddatiattr))then
                                 call idba_prendilo (this%handle)
                                 write=.false.
                                 call idba_set (this%handle, this%vol7d%datiattr%/**/VOL7D_POLY_TYPES/**/(inddatiattr)%btable,&
                                      this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES/**/(i,ii,iii,iiii,ind,iiiiii,inddatiattr))
                                 writeattr=.true.
                              end if
                           end if
                        end do

                        if (writeattr) call idba_critica (this%handle)

                     end if

!fine ciclo sul tipo attributo
