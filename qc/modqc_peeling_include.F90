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




if (associated(this%datiattr%/**/VOL7D_POLY_TYPES)) then
  inddatiattrinv = firsttrue(attrvars%vars(1) == this%datiattr%/**/VOL7D_POLY_TYPES) !indice attributo
  inddatiattrcli = firsttrue(attrvars%vars(2) == this%datiattr%/**/VOL7D_POLY_TYPES) !indice attributo
  inddatiattrtem = firsttrue(attrvars%vars(3) == this%datiattr%/**/VOL7D_POLY_TYPES) !indice attributo
  inddatiattrspa = firsttrue(attrvars%vars(4) == this%datiattr%/**/VOL7D_POLY_TYPES) !indice attributo

  if (inddatiattrinv > 0 .or. inddatiattrcli > 0 .or. inddatiattrtem > 0 .or. inddatiattrspa > 0 ) then  ! solo se c'è l'attributo

    if (associated(this%dativarattr%/**/VOL7D_POLY_TYPES)) then
                                !print *, "ELABORO this%dativarattr%&
                                ! & VOL7D_POLY_TYPES &
                                ! &",inddatiattrinv, inddatiattrcli, inddatiattrtem, inddatiattrspa 
                                !print *, "dimensione dativarattr",size(this%dativarattr%/**/VOL7D_POLY_TYPES)
      
      if (associated(this%dativar%/**/VOL7D_POLY_SUBTYPES)) then
                                !print *, "ELABORO this%dativar%&
                                ! & VOL7D_POLY_SUBTYPES &
                                ! &"
                                !print *, "dimensione dativar ",size(this%dativar%/**/VOL7D_POLY_SUBTYPES)
        
        do inddativar=1,size(this%dativar%/**/VOL7D_POLY_SUBTYPES)   ! per tutte le variabili /**/VOL7D_POLY_SUBTYPES
          
          inddativarattr  = this%dativar%/**/VOL7D_POLY_SUBTYPES(inddativar)%/**/VOL7D_POLY_TYPES
                                !call display (this%dativar%/**/VOL7D_POLY_SUBTYPES(inddativar))
                                !print *, "this%dativar%"//' #VOL7D_POLY_SUBTYPES ',inddativarattr

          if (inddativarattr > 0) then         ! se la variabile ha quell'attributo /**/VOL7D_POLY_TYPES
            nullify(invb/**/VOL7D_POLY_TYPES)
            nullify(clib/**/VOL7D_POLY_TYPES)
            nullify(temb/**/VOL7D_POLY_TYPES)
            nullify(spab/**/VOL7D_POLY_TYPES)
            
            if (inddatiattrinv > 0) invb/**/VOL7D_POLY_TYPES => this%voldatiattr/**/VOL7D_POLY_TYPES(:,:,:,:,inddativarattr,:,inddatiattrinv)
            if (inddatiattrcli > 0) clib/**/VOL7D_POLY_TYPES => this%voldatiattr/**/VOL7D_POLY_TYPES(:,:,:,:,inddativarattr,:,inddatiattrcli)
            if (inddatiattrtem > 0) temb/**/VOL7D_POLY_TYPES => this%voldatiattr/**/VOL7D_POLY_TYPES(:,:,:,:,inddativarattr,:,inddatiattrtem)
            if (inddatiattrspa > 0) spab/**/VOL7D_POLY_TYPES => this%voldatiattr/**/VOL7D_POLY_TYPES(:,:,:,:,inddativarattr,:,inddatiattrspa)
            
            this%voldati/**/VOL7D_POLY_SUBTYPES(:,:,:,:,inddativar,:) = peeled(this%voldati/**/VOL7D_POLY_SUBTYPES(:,:,:,:,inddativar,:), &
             invb/**/VOL7D_POLY_TYPES,clib/**/VOL7D_POLY_TYPES,temb/**/VOL7D_POLY_TYPES,spab/**/VOL7D_POLY_TYPES)
          end if
        end do
      endif
    endif
  end if
end if
