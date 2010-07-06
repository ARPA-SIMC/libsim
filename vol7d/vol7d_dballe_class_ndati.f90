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
!type VOL7D_POLY_TYPES_V

if (associated(this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V))then
   ndativar/**/VOL7D_POLY_TYPES_V=size(this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(:))
   allocate (lvar/**/VOL7D_POLY_TYPES_V(ndativar/**/VOL7D_POLY_TYPES_V))

   call l4f_category_log(this%category,L4F_DEBUG,"ndativar*: "//&
    to_char(ndativar/**/VOL7D_POLY_TYPES_V ))

   lvar/**/VOL7D_POLY_TYPES_V(ndativar/**/VOL7D_POLY_TYPES_V)=.false.
   if (present(var))then
      lvar/**/VOL7D_POLY_TYPES_V(:)=.false.
      do  i=1,size(var)
         where (var(i) == this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(:)%btable)
            lvar/**/VOL7D_POLY_TYPES_V(:)=.true.
         end where
      end do
   else
     lvar/**/VOL7D_POLY_TYPES_V(:)=c_e(this%vol7d%dativar%/**/VOL7D_POLY_TYPES_V(:)%btable)
   end if
else
   allocate (lvar/**/VOL7D_POLY_TYPES_V(0))
end if

if (associated(this%vol7d%dativarattr%/**/VOL7D_POLY_TYPES_V))then
   ndatiattr/**/VOL7D_POLY_TYPES_V=size(this%vol7d%datiattr%/**/VOL7D_POLY_TYPES_V(:))
   allocate (lattr/**/VOL7D_POLY_TYPES_V(ndatiattr/**/VOL7D_POLY_TYPES_V))
   lattr/**/VOL7D_POLY_TYPES_V(ndatiattr/**/VOL7D_POLY_TYPES_V)=.false.
   if (present(attr))then
      lattr/**/VOL7D_POLY_TYPES_V(:)=.false.
      do  i=1,size(attr)
         where (attr(i) == this%vol7d%datiattr%/**/VOL7D_POLY_TYPES_V(:)%btable)
            lattr/**/VOL7D_POLY_TYPES_V(:)=.true.
         end where
      end do
   else
      lattr/**/VOL7D_POLY_TYPES_V(:)=c_e(this%vol7d%datiattr%/**/VOL7D_POLY_TYPES_V(:)%btable)
   end if
else
   allocate (lattr/**/VOL7D_POLY_TYPES_V(0))
end if

!# end type  /**/VOL7D_POLY_TYPES_V

