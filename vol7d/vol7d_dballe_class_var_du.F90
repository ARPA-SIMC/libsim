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
if (associated(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V)) then
do i =1,size(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V)

if (this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%btable(:1) == "*") then
  j=firsttrue(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%btable(2:) == dballevar(:)%btable)
else
  j=firsttrue(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%btable == dballevar(:)%btable)
end if

  if ( j > 0 )then
    if(.not.c_e(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%description))this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%description =  dballevar(j)%description
    if(.not.c_e(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%unit))this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%unit =  dballevar(j)%unit
    if(.not.c_e(this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%scalefactor))this%/**/VOL7D_POLY_NAME%/**/VOL7D_POLY_TYPES_V(i)%scalefactor =  dballevar(j)%scalefactor
  end if
end do
endif

