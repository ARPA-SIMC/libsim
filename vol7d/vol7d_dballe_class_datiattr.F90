! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
                                !ciclo sul tipo attributo
                                !#define __strunz(s) #s
                                !#define stringa(s) __strunz(s)
                     
                                !print *,"type var e attr ",  stringa(VOL7D_POLY_TYPES_V)  ,  stringa(VOL7D_POLY_TYPES)
                                !print *,ind,ndatiattr/**/VOL7D_POLY_TYPES


ind = this%vol7d%dativar% VOL7D_POLY_TYPES_V/**/(iiiii)% VOL7D_POLY_TYPES
#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,&
     "attr " // to_char(iiiii)//"  "//to_char(ind))
#endif

if (ind > 0) then
  do inddatiattr=1,ndatiattr/**/VOL7D_POLY_TYPES
    if (lattr/**/VOL7D_POLY_TYPES (inddatiattr))then
      if (c_e(this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES/**/(i,ii,iii,iiii,ind,iiiiii,inddatiattr)))then


#ifdef DEBUG
         CALL l4f_category_log(this%category,L4F_DEBUG,&
              "attr "//this%vol7d%datiattr%/**/VOL7D_POLY_TYPES/**/(inddatiattr)%btable//" : "//&
              t2c(this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES/**/(i,ii,iii,iiii,ind,iiiiii,inddatiattr)))
#endif


        call idba_set (this%handle, this%vol7d%datiattr% VOL7D_POLY_TYPES/**/(inddatiattr)%btable,&
         this%vol7d%voldatiattr/**/VOL7D_POLY_TYPES/**/(i,ii,iii,iiii,ind,iiiiii,inddatiattr))
        writeattr=.true.
      end if
    end if
  end do
end if
                     
                                !fine ciclo sul tipo attributo
