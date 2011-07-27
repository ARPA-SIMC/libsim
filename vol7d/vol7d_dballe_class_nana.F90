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
!type /**/VOL7D_POLY_TYPES_V

if (associated(this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V))then
   nanavar/**/VOL7D_POLY_TYPES_V=size(this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(:))
   allocate (lanavar/**/VOL7D_POLY_TYPES_V(nanavar/**/VOL7D_POLY_TYPES_V))
   lanavar/**/VOL7D_POLY_TYPES_V(nanavar/**/VOL7D_POLY_TYPES_V)=.false.
   if (present(anavar))then
      lanavar/**/VOL7D_POLY_TYPES_V(:)=.false.
      do  i=1,size(anavar)
         where (anavar(i) == this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(:)%btable)
            lanavar/**/VOL7D_POLY_TYPES_V(:)=.true.
         end where
      end do
   else
      lanavar/**/VOL7D_POLY_TYPES_V(:)=c_e(this%vol7d%anavar%/**/VOL7D_POLY_TYPES_V(:)%btable)
   end if
else
   allocate (lanavar/**/VOL7D_POLY_TYPES_V(0))
end if

if (associated(this%vol7d%anavarattr%/**/VOL7D_POLY_TYPES_V))then
   nanaattr/**/VOL7D_POLY_TYPES_V=size(this%vol7d%anaattr%/**/VOL7D_POLY_TYPES_V(:))
   allocate (lanaattr/**/VOL7D_POLY_TYPES_V(nanaattr/**/VOL7D_POLY_TYPES_V))
   lanaattr/**/VOL7D_POLY_TYPES_V(nanaattr/**/VOL7D_POLY_TYPES_V)=.false.
   if (present(anaattr))then
      lanaattr/**/VOL7D_POLY_TYPES_V(:)=.false.
      do  i=1,size(anaattr)
         where (anaattr(i) == this%vol7d%anaattr%/**/VOL7D_POLY_TYPES_V(:)%btable)
            lanaattr/**/VOL7D_POLY_TYPES_V(:)=.true.
         end where
      end do
   else
      lanaattr/**/VOL7D_POLY_TYPES_V(:)=c_e(this%vol7d%anaattr%/**/VOL7D_POLY_TYPES_V(:)%btable)
   end if
else
   allocate (lanaattr/**/VOL7D_POLY_TYPES_V(0))
end if

!# end type /**/VOL7D_POLY_TYPES_V

