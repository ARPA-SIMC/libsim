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
PROGRAM esempio_geo_coordvect
USE file_utilities
USE geo_coord_with_utm_class
IMPLICIT NONE

TYPE(geo_coordvect),POINTER :: macroa(:)
INTEGER :: un, i
INTEGER(kind=ptr_c) :: shphandle
CHARACTER(len=512) :: filesim

!filesim=get_package_filepath('polipciv4.dat', filetype_data)
CALL IMPORT(macroa, shpfile='sottobacini_adb', proj=proj_utm, fuso=32, &
 elliss=elliss_intl)
DO i = 1, SIZE(macroa)
  CALL to_geo(macroa(i))
ENDDO

CALL export(macroa, shpfile='sottobacini_adb_geo', proj=proj_geo)

END PROGRAM esempio_geo_coordvect
