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
MODULE vol7d_csv
USE vol7d_class
USE array_utilities
USE file_utilities
IMPLICIT NONE

! csv output configuration
CHARACTER(len=8) :: csv_volume
CHARACTER(len=512) :: csv_column, csv_columnorder, csv_variable
LOGICAL :: csv_keep_miss, csv_no_rescale
INTEGER :: csv_header, icsv_column(7), icsv_columnorder(6), icsv_colinvorder(6), &
 icsv_colstart(6), icsv_colend(6), icsv_colind(6)

TYPE vol7d_var_mapper
  CHARACTER(len=2) :: cat
  CHARACTER(len=1) :: typ
  INTEGER :: i5, i7
END TYPE vol7d_var_mapper

CONTAINS

SUBROUTINE csv_export(v7d, iun)
TYPE(vol7d),INTENT(inout) :: v7d
INTEGER,INTENT(in) :: iun

INTEGER :: licsv_column(SIZE(icsv_column))
LOGICAL :: no_miss, no_missa, anaonly, different
CHARACTER(len=50) :: desdata(7)
TYPE(csv_record) :: csvline, csv_desdata(7), csv_anadesdata(7)
INTEGER :: i, i1, i2, i3, i4, i5, i6, i7, nv, ndvar, nav, ndv
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d_var_mapper),POINTER :: mapper(:)
LOGICAL :: analine

NULLIFY(mapper)
CALL vol7d_alloc_vol(v7d) ! be safe

! Filter requested variables
IF (csv_variable /= 'all') THEN
  nv = word_split(csv_variable, w_s, w_e, ',')
  CALL checkvarvect(v7d%anavar)
  CALL checkvarvect(v7d%anaattr)
  CALL checkvarvect(v7d%anavarattr)
  CALL checkvarvect(v7d%dativar)
  CALL checkvarvect(v7d%datiattr)
  CALL checkvarvect(v7d%dativarattr)
  CALL vol7d_reform(v7d, miss=.TRUE.) ! sort?
  DEALLOCATE(w_s, w_e)
ENDIF
CALL var_mapper(v7d, mapper)

licsv_column(:) = icsv_column(:)

! If only ana volume, skip data-only dimensions
IF (SIZE(v7d%time) == 0) THEN
  WHERE (licsv_column(:) == vol7d_time_d)
    licsv_column(:) = -1
  END WHERE
ENDIF
IF (SIZE(v7d%level) == 0) THEN
  WHERE (licsv_column(:) == vol7d_level_d)
    licsv_column(:) = -1
  END WHERE
ENDIF
IF (SIZE(v7d%timerange) == 0) THEN
  WHERE (licsv_column(:) == vol7d_timerange_d)
    licsv_column(:) = -1
  END WHERE
ENDIF
IF (SIZE(v7d%time) == 0 .AND. SIZE(v7d%level) == 0 .AND. &
 SIZE(v7d%timerange) == 0) THEN
  anaonly = .TRUE.
ELSE
  anaonly = .FALSE.
ENDIF

nav = COUNT(mapper(:)%cat == 'av')
ndv = COUNT(mapper(:)%cat == 'dv')
IF ((ndv > 0 .AND. &
 (SIZE(v7d%ana) == 0 .OR. SIZE(v7d%network) == 0 .OR. &
 SIZE(v7d%time) == 0 .OR. SIZE(v7d%level) == 0 .OR. &
 SIZE(v7d%timerange) == 0)) .OR. &
 (nav > 0 .AND. &
 (SIZE(v7d%ana) == 0 .OR. SIZE(v7d%network) == 0))) THEN
  IF (ASSOCIATED(mapper)) DEALLOCATE(mapper)
  RETURN ! unexpectedly empty volume
ENDIF
  

! For column reordering
icsv_colstart(:) = 1
icsv_colend(:) = 0
WHERE (icsv_columnorder(:) == vol7d_ana_d)
  icsv_colend(:) = SIZE(v7d%ana)
END WHERE
WHERE (icsv_columnorder(:) == vol7d_time_d)
  icsv_colend(:) = SIZE(v7d%time)
END WHERE
WHERE (icsv_columnorder(:) == vol7d_level_d)
  icsv_colend(:) = SIZE(v7d%level)
END WHERE
WHERE (icsv_columnorder(:) == vol7d_timerange_d)
  icsv_colend(:) = SIZE(v7d%timerange)
END WHERE
WHERE (icsv_columnorder(:) == vol7d_var_d)
  icsv_colend(:) = SIZE(mapper)
END WHERE
WHERE (icsv_columnorder(:) == vol7d_network_d)
  icsv_colend(:) = SIZE(v7d%network)
END WHERE

! invert icsv_columnorder
icsv_colinvorder(vol7d_ana_d) = firsttrue(icsv_columnorder(:) == vol7d_ana_d)
icsv_colinvorder(vol7d_time_d) = firsttrue(icsv_columnorder(:) == vol7d_time_d)
icsv_colinvorder(vol7d_level_d) = firsttrue(icsv_columnorder(:) == vol7d_level_d)
icsv_colinvorder(vol7d_timerange_d) = firsttrue(icsv_columnorder(:) == vol7d_timerange_d)
icsv_colinvorder(vol7d_var_d) = firsttrue(icsv_columnorder(:) == vol7d_var_d)
icsv_colinvorder(vol7d_network_d) = firsttrue(icsv_columnorder(:) == vol7d_network_d)
! there should not be missing columns here except
! icsv_colinvorder(vol7d_var_d) thanks to the check in
! parse_v7d_column
IF (icsv_colinvorder(vol7d_var_d) <= 0) THEN
  ndvar = 5
ELSE
  ndvar = 6
ENDIF

IF (csv_header > 1) THEN ! Dummy header line, for compatibility
  CALL init(csvline)
  CALL csv_record_addfield(csvline, 'written by v7d_transform')
  WRITE(iun,'(A)')csv_record_getrecord(csvline)
  CALL delete(csvline)
ENDIF

IF (csv_header > 0) THEN ! Main header line

  DO i = 1, SIZE(csv_desdata)
    CALL init(csv_desdata(i))
  ENDDO

! Create header entries for all the v7d dimensions
  CALL csv_record_addfield(csv_desdata(vol7d_ana_d), 'Longitude')
  CALL csv_record_addfield(csv_desdata(vol7d_ana_d), 'Latitude')
  CALL csv_record_addfield(csv_desdata(vol7d_time_d), 'Date')
  CALL csv_record_addfield(csv_desdata(vol7d_level_d), 'Level1')
  CALL csv_record_addfield(csv_desdata(vol7d_level_d), 'L1')
  CALL csv_record_addfield(csv_desdata(vol7d_level_d), 'Level2')
  CALL csv_record_addfield(csv_desdata(vol7d_level_d), 'L2')
  CALL csv_record_addfield(csv_desdata(vol7d_timerange_d), 'Time range')
  CALL csv_record_addfield(csv_desdata(vol7d_timerange_d), 'P1')
  CALL csv_record_addfield(csv_desdata(vol7d_timerange_d), 'P2')
  CALL csv_record_addfield(csv_desdata(vol7d_var_d), 'Variable')
  CALL csv_record_addfield(csv_desdata(vol7d_network_d), 'Report')
  CALL csv_record_addfield(csv_desdata(7), 'Value')

  CALL init(csvline)
  DO i = 1, SIZE(licsv_column) ! add the required header entries in the desired order
    IF (licsv_column(i) > 0) &
     CALL csv_record_addfield(csvline,csv_desdata(licsv_column(i)))
  ENDDO
! and now add the header entries for the variables
  IF (ndvar == 5) THEN
    DO i5 = 1, SIZE(mapper)
      CALL add_var(csvline, v7d, mapper(i5))
    ENDDO
  ENDIF
! write the header line
  WRITE(iun,'(A)')csv_record_getrecord(csvline)
  CALL delete(csvline)

  DO i = 1, SIZE(csv_desdata)
    CALL delete(csv_desdata(i))
  ENDDO

ENDIF ! csv_header > 0

DO i = 1, SIZE(csv_desdata)
  CALL init(csv_desdata(i))
  CALL init(csv_anadesdata(i))
ENDDO

! Create data entries for all the v7d non-variables dimensions
icsv_colind(:) = icsv_colstart(:)
loop7d: DO WHILE(.TRUE.)

! initial part of the loop over columns
  DO i = 1, ndvar
    IF (icsv_colind(i) == icsv_colstart(i)) THEN
      IF (icsv_colind(i) <= icsv_colend(i)) THEN ! skip empty dimensions (anaonly)
! prepare the dimension descriptor data
        CALL make_csv_desdata(v7d, icsv_columnorder(i), icsv_colind(i), &
         csv_desdata(icsv_columnorder(i)), mapper, .FALSE.)
        CALL make_csv_desdata(v7d, icsv_columnorder(i), icsv_colind(i), &
         csv_anadesdata(icsv_columnorder(i)), mapper, .TRUE.)
      ENDIF
    ENDIF
  ENDDO

! set indices, use pointers?
  i1 = icsv_colind(icsv_colinvorder(vol7d_ana_d))
  i2 = icsv_colind(icsv_colinvorder(vol7d_time_d))
  i3 = icsv_colind(icsv_colinvorder(vol7d_level_d))
  i4 = icsv_colind(icsv_colinvorder(vol7d_timerange_d))
  i6 = icsv_colind(icsv_colinvorder(vol7d_network_d))

! body of the loop
  CALL init(csvline)
  no_miss = .FALSE.; no_missa = .FALSE. ! keep track of line with all missing data
  different = .TRUE.

  IF (ndvar == 5) THEN
    DO i = 1, SIZE(licsv_column) ! add the required data entries in the desired order
      IF (licsv_column(i) > 0) &
       CALL csv_record_addfield(csvline, csv_desdata(licsv_column(i)))
    ENDDO

! and now add the data entries for the variables
    DO i5 = 1, SIZE(mapper)
      CALL add_val(csvline, v7d, mapper(i5), i1, i2, i3, i4, i6, no_missa, no_miss)
    ENDDO

  ELSE

    i5 = icsv_colind(icsv_colinvorder(vol7d_var_d)) ! set var index, use pointer?

    IF (mapper(i5)%cat(1:1) == 'a') THEN ! ana line
      different = (i2 == 1 .AND. i3 == 1 .AND. i4 == 1)
      IF (different) THEN
        DO i = 1, SIZE(licsv_column) ! add the required data entries in the desired order
          IF (licsv_column(i) > 0 .AND. licsv_column(i) <= 6) THEN
            CALL csv_record_addfield(csvline, csv_anadesdata(licsv_column(i)))
          ELSE IF (licsv_column(i) > 6) THEN
            CALL add_val(csvline, v7d, mapper(i5), i1, i2, i3, i4, i6, no_miss, no_miss)
          ENDIF
        ENDDO
      ENDIF
    ELSE

      DO i = 1, SIZE(licsv_column) ! add the required data entries in the desired order
        IF (licsv_column(i) > 0 .AND. licsv_column(i) <= 6) THEN
          CALL csv_record_addfield(csvline, csv_desdata(licsv_column(i)))
        ELSE IF (licsv_column(i) > 6) THEN
          CALL add_val(csvline, v7d, mapper(i5), i1, i2, i3, i4, i6, no_miss, no_miss)
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  IF ((csv_keep_miss .OR. no_miss .OR. (no_missa .AND. anaonly)) .AND. different) THEN
    WRITE(iun,'(A)')csv_record_getrecord(csvline)
  ENDIF
  CALL delete(csvline)

! final part of the loop over columns
  DO i = ndvar, 1, -1
    IF (icsv_colind(i) < icsv_colend(i)) THEN ! increment loop index
      icsv_colind(i) = icsv_colind(i) + 1
! prepare the dimension descriptor data
      CALL make_csv_desdata(v7d, icsv_columnorder(i), icsv_colind(i), &
       csv_desdata(icsv_columnorder(i)), mapper, .FALSE.)
      CALL make_csv_desdata(v7d, icsv_columnorder(i), icsv_colind(i), &
       csv_anadesdata(icsv_columnorder(i)), mapper, .TRUE.)
      EXIT
    ELSE ! end of loop for this index, reset and increment next index
      icsv_colind(i) = icsv_colstart(i)
    ENDIF
  ENDDO
  IF (i == 0) EXIT loop7d ! all counters have reached the end

END DO loop7d

DO i = 1, SIZE(csv_desdata)
  CALL delete(csv_desdata(i))
  CALL delete(csv_anadesdata(i))
ENDDO
IF (ASSOCIATED(mapper)) DEALLOCATE(mapper)

CONTAINS

SUBROUTINE checkvarvect(varvect)
TYPE(vol7d_varvect),INTENT(inout) :: varvect

CALL checkvar(varvect%r)
CALL checkvar(varvect%d)
CALL checkvar(varvect%i)
CALL checkvar(varvect%b)
CALL checkvar(varvect%c)

END SUBROUTINE checkvarvect

SUBROUTINE checkvar(var)
TYPE(vol7d_var),POINTER :: var(:)

INTEGER :: i, j

IF (.NOT.ASSOCIATED(var)) RETURN

v7dvarloop: DO i = 1, SIZE(var)
  csvvarloop:  DO j = 1, nv
    IF (var(i)%btable == csv_variable(w_s(j):w_e(j))) THEN
      CYCLE v7dvarloop
    ENDIF
  ENDDO csvvarloop
  var(i) = vol7d_var_miss ! var not found, nullify
ENDDO v7dvarloop

END SUBROUTINE checkvar

SUBROUTINE addfieldc(var, val, no_miss)
TYPE(vol7d_var),INTENT(in) :: var
CHARACTER(len=*),INTENT(in) :: val
LOGICAL,INTENT(inout),OPTIONAL :: no_miss

IF (c_e(val)) THEN
  IF (.NOT.csv_no_rescale .AND. c_e(var%scalefactor) .AND. var%unit /= 'CCITTIA5' .AND. &
   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
    CALL csv_record_addfield(csvline, realdat(val, var))
  ELSE
    CALL csv_record_addfield(csvline, TRIM(val))
  ENDIF
  IF (PRESENT(no_miss)) no_miss = .TRUE.
ELSE
  CALL csv_record_addfield(csvline,'')
ENDIF

END SUBROUTINE addfieldc

SUBROUTINE addfieldi(var, val, no_miss)
TYPE(vol7d_var),INTENT(in) :: var
INTEGER,INTENT(in) :: val
LOGICAL,INTENT(inout),OPTIONAL :: no_miss

IF (c_e(val)) THEN
  IF (.NOT.csv_no_rescale .AND. c_e(var%scalefactor) .AND. &
   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
    CALL csv_record_addfield(csvline, realdat(val, var))
  ELSE
    CALL csv_record_addfield(csvline, val)
  ENDIF
  IF (PRESENT(no_miss)) no_miss = .TRUE.
ELSE
  CALL csv_record_addfield(csvline,'')
ENDIF

END SUBROUTINE addfieldi

SUBROUTINE addfieldb(var, val, no_miss)
TYPE(vol7d_var),INTENT(in) :: var
INTEGER(kind=int_b),INTENT(in) :: val
LOGICAL,INTENT(inout),OPTIONAL :: no_miss

IF (c_e(val)) THEN
  CALL addfieldi(var, INT(val), no_miss)
ELSE
  CALL csv_record_addfield(csvline,'')
ENDIF

END SUBROUTINE addfieldb

END SUBROUTINE csv_export


SUBROUTINE make_csv_desdata(v7d, icol, ind, csv_desdata, mapper, analine)
TYPE(vol7d),INTENT(in) :: v7d
INTEGER,INTENT(in) :: icol
INTEGER,INTENT(in) :: ind
TYPE(csv_record),INTENT(inout) :: csv_desdata
TYPE(vol7d_var_mapper),INTENT(in) :: mapper(:)
LOGICAL,INTENT(in) :: analine

REAL(kind=fp_geo) :: l1, l2
CHARACTER(len=128) :: charbuffer
CHARACTER(len=20) :: tmpbuf

CALL csv_record_rewind(csv_desdata)

SELECT CASE(icol)

CASE(vol7d_ana_d)
  CALL getval(v7d%ana(ind)%coord, lon=l1, lat=l2)
  CALL csv_record_addfield_miss(csv_desdata, l1)
  CALL csv_record_addfield_miss(csv_desdata, l2)

CASE(vol7d_time_d)
  IF (v7d%time(ind) /= datetime_miss .AND. .NOT.analine) THEN
    CALL getval(v7d%time(ind), isodate=charbuffer(1:19))
    CALL csv_record_addfield(csv_desdata, charbuffer(1:19))
  ELSE
    CALL csv_record_addfield(csv_desdata, '')
  ENDIF

CASE(vol7d_level_d)
  IF (analine) THEN
    CALL csv_record_addfield(csv_desdata, '')
    CALL csv_record_addfield(csv_desdata, '')
    CALL csv_record_addfield(csv_desdata, '')
    CALL csv_record_addfield(csv_desdata, '')
  ELSE
    CALL csv_record_addfield_miss(csv_desdata, v7d%level(ind)%level1)
    CALL csv_record_addfield_miss(csv_desdata, v7d%level(ind)%l1)
    CALL csv_record_addfield_miss(csv_desdata, v7d%level(ind)%level2)
    CALL csv_record_addfield_miss(csv_desdata, v7d%level(ind)%l2)
  ENDIF

CASE(vol7d_timerange_d)
  IF (analine) THEN
    CALL csv_record_addfield(csv_desdata, '')
    CALL csv_record_addfield(csv_desdata, '')
    CALL csv_record_addfield(csv_desdata, '')
  ELSE
    CALL csv_record_addfield_miss(csv_desdata, v7d%timerange(ind)%timerange)
    CALL csv_record_addfield_miss(csv_desdata, v7d%timerange(ind)%p1)
    CALL csv_record_addfield_miss(csv_desdata, v7d%timerange(ind)%p2)
  ENDIF

CASE(vol7d_var_d)
  CALL add_var(csv_desdata, v7d, mapper(ind))

CASE(vol7d_network_d)
  CALL csv_record_addfield_miss(csv_desdata, TRIM(v7d%network(ind)%name))

END SELECT

END SUBROUTINE make_csv_desdata


SUBROUTINE var_mapper(v7d, mapper)
TYPE(vol7d),INTENT(in) :: v7d
TYPE(vol7d_var_mapper),POINTER :: mapper(:)

INTEGER :: n

n = 0

IF (ASSOCIATED(v7d%anavar%r)) n = n + SIZE(v7d%anavar%r)
IF (ASSOCIATED(v7d%anavar%d)) n = n + SIZE(v7d%anavar%d)
IF (ASSOCIATED(v7d%anavar%i)) n = n + SIZE(v7d%anavar%i)
IF (ASSOCIATED(v7d%anavar%b)) n = n + SIZE(v7d%anavar%b)
IF (ASSOCIATED(v7d%anavar%c)) n = n + SIZE(v7d%anavar%c)

IF (ASSOCIATED(v7d%anaattr%r) .AND. ASSOCIATED(v7d%anavarattr%r)) n = n + &
 SIZE(v7d%anaattr%r) * SIZE(v7d%anavarattr%r)
IF (ASSOCIATED(v7d%anaattr%d) .AND. ASSOCIATED(v7d%anavarattr%d)) n = n + &
 SIZE(v7d%anaattr%d) * SIZE(v7d%anavarattr%d)
IF (ASSOCIATED(v7d%anaattr%i) .AND. ASSOCIATED(v7d%anavarattr%i)) n = n + &
 SIZE(v7d%anaattr%i) * SIZE(v7d%anavarattr%i)
IF (ASSOCIATED(v7d%anaattr%b) .AND. ASSOCIATED(v7d%anavarattr%b)) n = n + &
 SIZE(v7d%anaattr%b) * SIZE(v7d%anavarattr%b)
IF (ASSOCIATED(v7d%anaattr%c) .AND. ASSOCIATED(v7d%anavarattr%c)) n = n + &
 SIZE(v7d%anaattr%c) * SIZE(v7d%anavarattr%c)

IF (ASSOCIATED(v7d%dativar%r)) n = n + SIZE(v7d%dativar%r)
IF (ASSOCIATED(v7d%dativar%d)) n = n + SIZE(v7d%dativar%d)
IF (ASSOCIATED(v7d%dativar%i)) n = n + SIZE(v7d%dativar%i)
IF (ASSOCIATED(v7d%dativar%b)) n = n + SIZE(v7d%dativar%b)
IF (ASSOCIATED(v7d%dativar%c)) n = n + SIZE(v7d%dativar%c)

IF (ASSOCIATED(v7d%datiattr%r) .AND. ASSOCIATED(v7d%dativarattr%r)) n = n + &
 SIZE(v7d%datiattr%r) * SIZE(v7d%dativarattr%r)
IF (ASSOCIATED(v7d%datiattr%d) .AND. ASSOCIATED(v7d%dativarattr%d)) n = n + &
 SIZE(v7d%datiattr%d) * SIZE(v7d%dativarattr%d)
IF (ASSOCIATED(v7d%datiattr%i) .AND. ASSOCIATED(v7d%dativarattr%i)) n = n + &
 SIZE(v7d%datiattr%i) * SIZE(v7d%dativarattr%i)
IF (ASSOCIATED(v7d%datiattr%b) .AND. ASSOCIATED(v7d%dativarattr%b)) n = n + &
 SIZE(v7d%datiattr%b) * SIZE(v7d%dativarattr%b)
IF (ASSOCIATED(v7d%datiattr%c) .AND. ASSOCIATED(v7d%dativarattr%c)) n = n + &
 SIZE(v7d%datiattr%c) * SIZE(v7d%dativarattr%c)

ALLOCATE(mapper(n))

n = 0

IF (ASSOCIATED(v7d%anavar%r)) THEN
  CALL set_mapper('av', 'r', 1, SIZE(v7d%anavar%r))
ENDIF
IF (ASSOCIATED(v7d%anavar%d)) THEN
  CALL set_mapper('av', 'd', 1, SIZE(v7d%anavar%d))
ENDIF
IF (ASSOCIATED(v7d%anavar%i)) THEN
  CALL set_mapper('av', 'i', 1, SIZE(v7d%anavar%i))
ENDIF
IF (ASSOCIATED(v7d%anavar%b)) THEN
  CALL set_mapper('av', 'b', 1, SIZE(v7d%anavar%b))
ENDIF
IF (ASSOCIATED(v7d%anavar%c)) THEN
  CALL set_mapper('av', 'c', 1, SIZE(v7d%anavar%c))
ENDIF

IF (ASSOCIATED(v7d%anaattr%r) .AND. ASSOCIATED(v7d%anavarattr%r)) THEN
  CALL set_mapper('aa', 'r', SIZE(v7d%anaattr%r), SIZE(v7d%anavarattr%r))
ENDIF
IF (ASSOCIATED(v7d%anaattr%d) .AND. ASSOCIATED(v7d%anavarattr%d)) THEN
  CALL set_mapper('aa', 'd', SIZE(v7d%anaattr%d), SIZE(v7d%anavarattr%d))
ENDIF
IF (ASSOCIATED(v7d%anaattr%i) .AND. ASSOCIATED(v7d%anavarattr%i)) THEN
  CALL set_mapper('aa', 'i', SIZE(v7d%anaattr%i), SIZE(v7d%anavarattr%i))
ENDIF
IF (ASSOCIATED(v7d%anaattr%b) .AND. ASSOCIATED(v7d%anavarattr%b)) THEN
  CALL set_mapper('aa', 'b', SIZE(v7d%anaattr%b), SIZE(v7d%anavarattr%b))
ENDIF
IF (ASSOCIATED(v7d%anaattr%c) .AND. ASSOCIATED(v7d%anavarattr%c)) THEN
  CALL set_mapper('aa', 'c', SIZE(v7d%anaattr%c), SIZE(v7d%anavarattr%c))
ENDIF

IF (ASSOCIATED(v7d%dativar%r)) THEN
  CALL set_mapper('dv', 'r', 1, SIZE(v7d%dativar%r))
ENDIF
IF (ASSOCIATED(v7d%dativar%d)) THEN
  CALL set_mapper('dv', 'd', 1, SIZE(v7d%dativar%d))
ENDIF
IF (ASSOCIATED(v7d%dativar%i)) THEN
  CALL set_mapper('dv', 'i', 1, SIZE(v7d%dativar%i))
ENDIF
IF (ASSOCIATED(v7d%dativar%b)) THEN
  CALL set_mapper('dv', 'b', 1, SIZE(v7d%dativar%b))
ENDIF
IF (ASSOCIATED(v7d%dativar%c)) THEN
  CALL set_mapper('dv', 'c', 1, SIZE(v7d%dativar%c))
ENDIF

IF (ASSOCIATED(v7d%datiattr%r) .AND. ASSOCIATED(v7d%dativarattr%r)) THEN
  CALL set_mapper('da', 'r', SIZE(v7d%datiattr%r), SIZE(v7d%dativarattr%r))
ENDIF
IF (ASSOCIATED(v7d%datiattr%d) .AND. ASSOCIATED(v7d%dativarattr%d)) THEN
  CALL set_mapper('da', 'd', SIZE(v7d%datiattr%d), SIZE(v7d%dativarattr%d))
ENDIF
IF (ASSOCIATED(v7d%datiattr%i) .AND. ASSOCIATED(v7d%dativarattr%i)) THEN
  CALL set_mapper('da', 'i', SIZE(v7d%datiattr%i), SIZE(v7d%dativarattr%i))
ENDIF
IF (ASSOCIATED(v7d%datiattr%b) .AND. ASSOCIATED(v7d%dativarattr%b)) THEN
  CALL set_mapper('da', 'b', SIZE(v7d%datiattr%b), SIZE(v7d%dativarattr%b))
ENDIF
IF (ASSOCIATED(v7d%datiattr%c) .AND. ASSOCIATED(v7d%dativarattr%c)) THEN
  CALL set_mapper('da', 'c', SIZE(v7d%datiattr%c), SIZE(v7d%dativarattr%c))
ENDIF

CONTAINS

SUBROUTINE set_mapper(cat, typ, s1, s2)
CHARACTER(len=2),INTENT(in) :: cat
CHARACTER(len=1),INTENT(in) :: typ
INTEGER,INTENT(in) :: s1, s2

INTEGER :: i, j, n1

n1 = n + s1*s2
mapper(n+1:n1)%cat = cat
mapper(n+1:n1)%typ = typ
mapper(n+1:n1)%i5 = (/((i,i=1,s2),j=1,s1)/)
mapper(n+1:n1)%i7 = (/((j,i=1,s2),j=1,s1)/)
n = n1

END SUBROUTINE set_mapper

END SUBROUTINE var_mapper


SUBROUTINE add_var(csvline, v7d, mapper)
TYPE(csv_record),INTENT(inout) :: csvline
TYPE(vol7d),INTENT(in) :: v7d
TYPE(vol7d_var_mapper),INTENT(in) :: mapper

SELECT CASE(mapper%cat)
CASE('av')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavar%r(mapper%i5)%btable))
  CASE('d')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavar%d(mapper%i5)%btable))
  CASE('i')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavar%i(mapper%i5)%btable))
  CASE('b')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavar%b(mapper%i5)%btable))
  CASE('c')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavar%c(mapper%i5)%btable))
  END SELECT
CASE('aa')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavarattr%r(mapper%i5)%btable) &
     //'.'//TRIM(v7d%anaattr%r(mapper%i7)%btable))
  CASE('d')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavarattr%d(mapper%i5)%btable) &
     //'.'//TRIM(v7d%anaattr%d(mapper%i7)%btable))
  CASE('i')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavarattr%i(mapper%i5)%btable) &
     //'.'//TRIM(v7d%anaattr%i(mapper%i7)%btable))
  CASE('b')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavarattr%b(mapper%i5)%btable) &
     //'.'//TRIM(v7d%anaattr%b(mapper%i7)%btable))
  CASE('c')
    CALL csv_record_addfield(csvline,TRIM(v7d%anavarattr%c(mapper%i5)%btable) &
     //'.'//TRIM(v7d%anaattr%c(mapper%i7)%btable))
  END SELECT
CASE('dv')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativar%r(mapper%i5)%btable))
  CASE('d')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativar%d(mapper%i5)%btable))
  CASE('i')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativar%i(mapper%i5)%btable))
  CASE('b')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativar%b(mapper%i5)%btable))
  CASE('c')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativar%c(mapper%i5)%btable))
  END SELECT
CASE('da')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativarattr%r(mapper%i5)%btable) &
     //'.'//TRIM(v7d%datiattr%r(mapper%i7)%btable))
  CASE('d')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativarattr%d(mapper%i5)%btable) &
     //'.'//TRIM(v7d%datiattr%d(mapper%i7)%btable))
  CASE('i')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativarattr%i(mapper%i5)%btable) &
     //'.'//TRIM(v7d%datiattr%i(mapper%i7)%btable))
  CASE('b')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativarattr%b(mapper%i5)%btable) &
     //'.'//TRIM(v7d%datiattr%b(mapper%i7)%btable))
  CASE('c')
    CALL csv_record_addfield(csvline,TRIM(v7d%dativarattr%c(mapper%i5)%btable) &
     //'.'//TRIM(v7d%datiattr%c(mapper%i7)%btable))
  END SELECT
END SELECT

END SUBROUTINE add_var


SUBROUTINE add_val(csvline, v7d, mapper, i1, i2, i3, i4, i6, no_missa, no_missd)
TYPE(csv_record),INTENT(inout) :: csvline
TYPE(vol7d),INTENT(in) :: v7d
TYPE(vol7d_var_mapper),INTENT(in) :: mapper
INTEGER,INTENT(in) :: i1, i2, i3, i4, i6
LOGICAL,INTENT(out) :: no_missa
LOGICAL,INTENT(out) :: no_missd

SELECT CASE(mapper%cat)
CASE('av')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL addfieldr(v7d%volanar(i1,mapper%i5,i6), no_missa)
  CASE('d')
    CALL addfieldd(v7d%volanad(i1,mapper%i5,i6), no_missa)
  CASE('i')
    CALL addfieldi(v7d%anavar%i(mapper%i5), v7d%volanai(i1,mapper%i5,i6), no_missa)
  CASE('b')
    CALL addfieldb(v7d%anavar%b(mapper%i5), v7d%volanab(i1,mapper%i5,i6), no_missa)
  CASE('c')
    CALL addfieldc(v7d%anavar%c(mapper%i5), v7d%volanac(i1,mapper%i5,i6), no_missa)
  END SELECT
CASE('aa')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL addfieldr(v7d%volanaattrr(i1,mapper%i5,i6,mapper%i7), no_missa)
  CASE('d')
    CALL addfieldd(v7d%volanaattrd(i1,mapper%i5,i6,mapper%i7), no_missa)
  CASE('i')
    CALL addfieldi(v7d%anaattr%i(mapper%i7), v7d%volanaattri(i1,mapper%i5,i6,mapper%i7), &
     no_missa)
  CASE('b')
    CALL addfieldb(v7d%anaattr%b(mapper%i7), v7d%volanaattrb(i1,mapper%i5,i6,mapper%i7), &
     no_missa)
  CASE('c')
    CALL addfieldc(v7d%anaattr%c(mapper%i7), v7d%volanaattrc(i1,mapper%i5,i6,mapper%i7), &
     no_missa)
  END SELECT
CASE('dv')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL addfieldr(v7d%voldatir(i1,i2,i3,i4,mapper%i5,i6), no_missd)
  CASE('d')
    CALL addfieldd(v7d%voldatid(i1,i2,i3,i4,mapper%i5,i6), no_missd)
  CASE('i')
    CALL addfieldi(v7d%dativar%i(mapper%i5), v7d%voldatii(i1,i2,i3,i4,mapper%i5,i6), &
     no_missd)
  CASE('b')
    CALL addfieldb(v7d%dativar%b(mapper%i5), v7d%voldatib(i1,i2,i3,i4,mapper%i5,i6), &
     no_missd)
  CASE('c')
    CALL addfieldc(v7d%dativar%c(mapper%i5), v7d%voldatic(i1,i2,i3,i4,mapper%i5,i6), &
     no_missd)
  END SELECT
CASE('da')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL addfieldr(v7d%voldatiattrr(i1,i2,i3,i4,mapper%i5,i6,mapper%i7), no_missd)
  CASE('d')
    CALL addfieldd(v7d%voldatiattrd(i1,i2,i3,i4,mapper%i5,i6,mapper%i7), no_missd)
  CASE('i')
    CALL addfieldi(v7d%datiattr%i(mapper%i7), &
     v7d%voldatiattri(i1,i2,i3,i4,mapper%i5,i6,mapper%i7), no_missd)
  CASE('b')
    CALL addfieldb(v7d%datiattr%b(mapper%i7), &
     v7d%voldatiattrb(i1,i2,i3,i4,mapper%i5,i6,mapper%i7), no_missd)
  CASE('c')
    CALL addfieldc(v7d%datiattr%c(mapper%i7), &
     v7d%voldatiattrc(i1,i2,i3,i4,mapper%i5,i6,mapper%i7), no_missd)
  END SELECT
END SELECT

CONTAINS

SUBROUTINE addfieldr(val, no_miss)
REAL,INTENT(in) :: val
LOGICAL,INTENT(inout) :: no_miss

CALL csv_record_addfield_miss(csvline, val)
no_miss = no_miss .OR. c_e(val)

END SUBROUTINE addfieldr

SUBROUTINE addfieldd(val, no_miss)
DOUBLE PRECISION,INTENT(in) :: val
LOGICAL,INTENT(inout) :: no_miss

CALL csv_record_addfield_miss(csvline, val)
no_miss = no_miss .OR. c_e(val)

END SUBROUTINE addfieldd

SUBROUTINE addfieldc(var, val, no_miss)
TYPE(vol7d_var),INTENT(in) :: var
CHARACTER(len=*),INTENT(in) :: val
LOGICAL,INTENT(inout) :: no_miss

IF (c_e(val)) THEN
  IF (.NOT.csv_no_rescale .AND. c_e(var%scalefactor) .AND. var%unit /= 'CCITTIA5' .AND. &
   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
    CALL csv_record_addfield(csvline, realdat(val, var))
  ELSE
    CALL csv_record_addfield(csvline, TRIM(val))
  ENDIF
  no_miss = .TRUE.
ELSE
  CALL csv_record_addfield(csvline,'')
ENDIF

END SUBROUTINE addfieldc

SUBROUTINE addfieldi(var, val, no_miss)
TYPE(vol7d_var),INTENT(in) :: var
INTEGER,INTENT(in) :: val
LOGICAL,INTENT(inout) :: no_miss

IF (c_e(val)) THEN
  IF (.NOT.csv_no_rescale .AND. c_e(var%scalefactor) .AND. &
   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
    CALL csv_record_addfield(csvline, realdat(val, var))
  ELSE
    CALL csv_record_addfield(csvline, val)
  ENDIF
  no_miss = .TRUE.
ELSE
  CALL csv_record_addfield(csvline,'')
ENDIF

END SUBROUTINE addfieldi

SUBROUTINE addfieldb(var, val, no_miss)
TYPE(vol7d_var),INTENT(in) :: var
INTEGER(kind=int_b),INTENT(in) :: val
LOGICAL,INTENT(inout) :: no_miss

IF (c_e(val)) THEN
  CALL addfieldi(var, INT(val), no_miss)
ELSE
  CALL csv_record_addfield(csvline,'')
ENDIF

END SUBROUTINE addfieldb

END SUBROUTINE add_val

END MODULE vol7d_csv
