! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>
! Enrico Minguzzi <eminguzzi@arpa.emr.it>

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
#include "config.h"
MODULE grib_api_csv
USE grib_api
USE vol7d_class
USE volgrid6d_class
USE char_utilities
USE grid_id_class
USE log4fortran
IMPLICIT NONE

CHARACTER(len=6),PARAMETER :: lnmspc='gacsv:'


CONTAINS


SUBROUTINE grib_api_csv_export(v7d, vg6d, iun, keys)
TYPE(vol7d) :: v7d
TYPE(volgrid6d) :: vg6d
INTEGER :: iun
CHARACTER(len=*) :: keys

TYPE(csv_record) :: csvline
TYPE(datetime) :: veriftime
INTEGER,POINTER :: w_s(:), w_e(:)
INTEGER :: n, i, j, k, l, np, nv, ncol, gaid, status
INTEGER :: csv_igaid
CHARACTER(len=24) :: csv_time, csv_level, csv_gaid
CHARACTER(len=12) :: csv_simpletime, csv_simplevertime
LOGICAL,ALLOCATABLE :: key_mask(:)
DOUBLE PRECISION :: coord


#ifdef DEBUG
! expensive checks
CALL l4f_category_log(vg6d%category,L4F_DEBUG, &
 "grib_api_csv, checking data volumes")

IF (SIZE(v7d%time) /= SIZE(vg6d%time)) THEN
  CALL l4f_category_log(vg6d%category,L4F_ERROR, &
   "grib_api_csv, v7d and vg6d objects non conformal, time dimension")
  RETURN
ENDIF
IF (ANY(v7d%time(:) /= vg6d%time(:))) THEN
  CALL l4f_category_log(vg6d%category,L4F_ERROR, &
   "grib_api_csv, v7d and vg6d objects different, time dimension")
  RETURN
ENDIF

IF (SIZE(v7d%timerange) /= SIZE(vg6d%timerange)) THEN
  CALL l4f_category_log(vg6d%category,L4F_ERROR, &
   "grib_api_csv, v7d and vg6d objects non conformal, timerange dimension")
  RETURN
ENDIF
IF (ANY(v7d%timerange(:) /= vg6d%timerange(:))) THEN
  CALL l4f_category_log(vg6d%category,L4F_ERROR, &
   "grib_api_csv, v7d and vg6d objects different, timerange dimension")
  RETURN
ENDIF

IF (SIZE(v7d%level) /= SIZE(vg6d%level)) THEN
  CALL l4f_category_log(vg6d%category,L4F_ERROR, &
   "grib_api_csv, v7d and vg6d objects non conformal, level dimension")
  RETURN
ENDIF
IF (ANY(v7d%level(:) /= vg6d%level(:))) THEN
  CALL l4f_category_log(vg6d%category,L4F_ERROR, &
   "grib_api_csv, v7d and vg6d objects different, level dimension")
  RETURN
ENDIF

IF (SIZE(v7d%dativar%r) /= SIZE(vg6d%var)) THEN
  CALL l4f_category_log(vg6d%category,L4F_ERROR, &
   "grib_api_csv, v7d and vg6d objects non conformal, var dimension")
  RETURN
ENDIF
#endif

ncol = word_split(keys, w_s, w_e, ',')
ALLOCATE(key_mask(ncol))
key_mask(:) = .FALSE.

! write csv header
CALL init(csvline)
DO i = 1, ncol
  IF (keys(w_s(i):MIN(w_e(i),w_s(i)+LEN(lnmspc)-1)) == lnmspc) THEN
    key_mask(i) = .TRUE.    ! it is a local namespace key, ie. gacsv:*
    w_s(i) = w_s(i) + LEN(lnmspc) ! shift start pointer to skip prefix
  ENDIF
  CALL csv_record_addfield(csvline, TRIM(keys(w_s(i):w_e(i))))
ENDDO

WRITE(iun,'(A)')csv_record_getrecord(csvline)
CALL delete(csvline)

! write csv body
DO l = 1, SIZE(vg6d%time)
  CALL getval(vg6d%time(l), isodate=csv_time, simpledate=csv_simpletime)
  DO k = 1, SIZE(vg6d%timerange)
    veriftime = vg6d%time(l) + timedelta_new(msec=vg6d%timerange(k)%p1*1000)
    CALL getval(veriftime, simpledate=csv_simplevertime)
    DO j = 1, SIZE(vg6d%level)
! TODO handle coupling of variables on a single csv record (e.g. U/V)
      DO nv = 1, SIZE(vg6d%var)
! check the presence of input grid
        gaid = grid_id_get_gaid(vg6d%gaid(j,l,k,nv))
        IF (.NOT.c_e(gaid)) CYCLE ! grid not present
! loop on interpolated points
        DO np = 1, SIZE(v7d%ana)
          CALL init(csvline)
! loop on columns
          DO n = 1, ncol
            IF (key_mask(n)) THEN ! the truth is inside me
! TODO add keys: iindex, jindex, kindex, var_description
              SELECT CASE(keys(w_s(n):w_e(n)))
              CASE('lon')
                CALL getval(v7d%ana(np)%coord, lon=coord)
                CALL csv_record_addfield_miss(csvline, coord)
              CASE('lat')
                CALL getval(v7d%ana(np)%coord, lat=coord)
                CALL csv_record_addfield_miss(csvline, coord)
              CASE('npoint')
                CALL csv_record_addfield(csvline, TRIM(to_char(np)))
              CASE('isodate')
                CALL csv_record_addfield(csvline, csv_time(1:16))
              CASE('value')
                CALL csv_record_addfield_miss(csvline, &
                 v7d%voldatir(np,l,j,k,nv,1))
              CASE('timerange')
                CALL csv_record_addfield(csvline, vg6d%timerange(k)%timerange)
              CASE('p1')
                CALL csv_record_addfield(csvline, vg6d%timerange(k)%p1)
              CASE('p2')
                CALL csv_record_addfield(csvline, vg6d%timerange(k)%p2)
              CASE('level1')
                CALL csv_record_addfield(csvline, vg6d%level(j)%level1)
              CASE('l1')
                CALL csv_record_addfield(csvline, vg6d%level(j)%l1)
              CASE('level2')
                CALL csv_record_addfield(csvline, vg6d%level(j)%level2)
              CASE('l2')
                CALL csv_record_addfield(csvline, vg6d%level(j)%l2)
              CASE('centre')
                CALL csv_record_addfield(csvline, vg6d%var(nv)%centre)
              CASE('category')
                CALL csv_record_addfield(csvline, vg6d%var(nv)%category)
              CASE('number')
                CALL csv_record_addfield(csvline, vg6d%var(nv)%number)
              CASE('discipline')
                CALL csv_record_addfield(csvline, vg6d%var(nv)%discipline)
              CASE('simpledate')
                CALL csv_record_addfield(csvline, csv_simpletime)
              CASE('simpleverdate')
                CALL csv_record_addfield(csvline, csv_simplevertime)
              CASE default
                CALL l4f_category_log(vg6d%category,L4F_WARN, &
                 "requested gacsv key "//TRIM(keys(w_s(n):w_e(n)))//" undefined")
                CALL csv_record_addfield(csvline, '')
              END SELECT

            ELSE ! the truth is in grib_api
! TODO handle non integer type in grib_api
              CALL grib_get(gaid, keys(w_s(n):w_e(n)), csv_igaid, status)
              IF (status /= GRIB_SUCCESS) THEN
                csv_igaid = imiss
                CALL l4f_category_log(vg6d%category,L4F_WARN, &
                 "requested grib_api key "//TRIM(keys(w_s(n):w_e(n)))// &
                 " not found in message")
              ENDIF
              CALL csv_record_addfield_miss(csvline, csv_igaid)

            ENDIF

          ENDDO
          WRITE(iun,'(A)')csv_record_getrecord(csvline)
          CALL delete(csvline)
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

DEALLOCATE(key_mask, w_s, w_e)

END SUBROUTINE grib_api_csv_export

END MODULE grib_api_csv
