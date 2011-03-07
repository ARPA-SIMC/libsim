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

if (associated(this%VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V) .and. associated(that%VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V))then
  where (this%VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V == that%VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V)
    that%VOL7D_POLY_ARRAY/**/VOL7D_POLY_TYPES_V = VOL7D_POLY_TYPES_MISS
  end where
end if

