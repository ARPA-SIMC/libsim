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
USE optionparser_class
IMPLICIT NONE

CHARACTER(len=6),PARAMETER :: lnmspc='gacsv:'
CHARACTER(len=512) :: output_keys


CONTAINS


! add command-line options specific to this module
SUBROUTINE grib_api_csv_add_options(opt)
TYPE(optionparser),INTENT(inout) :: opt

CALL optionparser_add(opt, ' ', 'output-keys', output_keys, '', help= &
 'keys that have to appear in the output grib_api_csv file, any grib_api key &
 &or ''gacsv:xxx''; xxx can be any of: lon, lat, npoint, isodate, value, &
 &timerange, p1, p1h, p2, p2h, level1, l1, level2, l2, centre, category, &
 &number, discipline, simpledate, simpleverdate')
! other local command-line options can be added here

END SUBROUTINE grib_api_csv_add_options


! if needed, create a routine for checking local options and call it
!SUBROUTINE grib_api_csv_check_options()
!
!END SUBROUTINE grib_api_csv_check_options


SUBROUTINE grib_api_csv_export(v7d, vg6d, iun, header)
TYPE(vol7d),INTENT(in) :: v7d
TYPE(volgrid6d),INTENT(in) :: vg6d
INTEGER,INTENT(in) :: iun
LOGICAL,INTENT(in) :: header

TYPE(csv_record) :: csvline
TYPE(datetime) :: veriftime
INTEGER,POINTER :: w_s(:), w_e(:)
INTEGER :: n, i, j, k, l, np, nv, ncol, gaid, status
INTEGER :: csv_igaid
INTEGER :: p1h,p2h
CHARACTER(len=24) :: csv_time
CHARACTER(len=12) :: csv_simpletime, csv_simplevertime
CHARACTER(len=3) :: ch3
LOGICAL,ALLOCATABLE :: key_mask(:)

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

if (len_trim(output_keys) > 0) then 
  ncol = word_split(output_keys, w_s, w_e, ',')
else
  ncol=0
end if
ALLOCATE(key_mask(ncol))
key_mask(:) = .FALSE.

CALL init(csvline)
DO i = 1, ncol
  IF (output_keys(w_s(i):MIN(w_e(i),w_s(i)+LEN(lnmspc)-1)) == lnmspc) THEN
    key_mask(i) = .TRUE.    ! it is a local namespace key, ie. gacsv:*
    w_s(i) = w_s(i) + LEN(lnmspc) ! shift start pointer to skip prefix
  ENDIF
  CALL csv_record_addfield(csvline, TRIM(output_keys(w_s(i):w_e(i))))
ENDDO

IF (header) THEN ! write csv header
  WRITE(iun,'(A)')csv_record_getrecord(csvline)
ENDIF
CALL delete(csvline)

! write csv body
DO l = 1, SIZE(vg6d%time)
  CALL getval(vg6d%time(l), isodate=csv_time, simpledate=csv_simpletime)
  DO k = 1, SIZE(vg6d%timerange)
    veriftime = vg6d%time(l) + timedelta_new(sec=vg6d%timerange(k)%p1)
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
              SELECT CASE(output_keys(w_s(n):w_e(n)))
              CASE('lon')
                CALL csv_record_addfield_miss(csvline, trim(ADJUSTL(to_char(getlon(v7d%ana(np)%coord),miss="",form="(f10.5)"))))
              CASE('lat')
                CALL csv_record_addfield_miss(csvline, trim(ADJUSTL(to_char(getlat(v7d%ana(np)%coord),miss="",form="(f10.5)"))))
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
              CASE('p1h')
                p1h = NINT(vg6d%timerange(k)%p1/3600.)
                IF (p1h >= 0 .AND. p1h < 1000) THEN
                  WRITE (ch3,'(i3.3)') p1h
                  CALL csv_record_addfield(csvline,ch3)
                ELSE
                  CALL csv_record_addfield(csvline,p1h)
                ENDIF
              CASE('p2')
                CALL csv_record_addfield(csvline, vg6d%timerange(k)%p2)
              CASE('p2h')
                p2h = NINT(vg6d%timerange(k)%p2/3600.)
                IF (p2h >= 0 .AND. p2h < 1000) THEN
                  WRITE (ch3,'(i3.3)') p2h
                  CALL csv_record_addfield(csvline,ch3)
                ELSE
                  CALL csv_record_addfield(csvline,p2h)
                ENDIF
              CASE('level1')
                CALL csv_record_addfield_miss(csvline, vg6d%level(j)%level1)
              CASE('l1')
                CALL csv_record_addfield_miss(csvline, vg6d%level(j)%l1)
              CASE('level2')
                CALL csv_record_addfield_miss(csvline, vg6d%level(j)%level2)
              CASE('l2')
                CALL csv_record_addfield_miss(csvline, vg6d%level(j)%l2)
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
                 "requested gacsv key "//TRIM(output_keys(w_s(n):w_e(n)))//" undefined")
                CALL csv_record_addfield(csvline, '')
              END SELECT

            ELSE ! the truth is in grib_api
! TODO handle non integer type in grib_api
              CALL grib_get(gaid, output_keys(w_s(n):w_e(n)), csv_igaid, status)
              IF (status /= GRIB_SUCCESS) THEN
                csv_igaid = imiss
                CALL l4f_category_log(vg6d%category,L4F_WARN, &
                 "requested grib_api key "//TRIM(output_keys(w_s(n):w_e(n)))// &
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

if (ncol > 0) DEALLOCATE(key_mask, w_s, w_e)

END SUBROUTINE grib_api_csv_export

END MODULE grib_api_csv
