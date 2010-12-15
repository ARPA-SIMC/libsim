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
USE vol7d_utilities
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
LOGICAL :: no_miss, no_missa, anaonly
CHARACTER(len=50) :: desdata(7)
TYPE(csv_record) :: csvline, csv_desdata(7)
INTEGER :: i, i1, i2, i3, i4, i5, i6, i7, nv, ndvar
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d_var_mapper),POINTER :: mapper(:)

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
ENDIF ! csv_header > 0

DO i = 1, SIZE(csv_desdata)
  CALL init(csv_desdata(i))
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
         csv_desdata(icsv_columnorder(i)), mapper)
      ENDIF
    ENDIF
  ENDDO

! set indices
  i1 = icsv_colind(icsv_colinvorder(vol7d_ana_d))
  i2 = icsv_colind(icsv_colinvorder(vol7d_time_d))
  i3 = icsv_colind(icsv_colinvorder(vol7d_level_d))
  i4 = icsv_colind(icsv_colinvorder(vol7d_timerange_d))
  i6 = icsv_colind(icsv_colinvorder(vol7d_network_d))

! body of the loop
  CALL init(csvline)
  no_miss = .FALSE.; no_missa = .FALSE. ! keep track of line with all missing data

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
    i5 = icsv_colind(icsv_colinvorder(vol7d_var_d))

    DO i = 1, SIZE(licsv_column) ! add the required data entries in the desired order
      IF (licsv_column(i) > 0 .AND. licsv_column(i) <= 6) THEN
        CALL csv_record_addfield(csvline, csv_desdata(licsv_column(i)))
      ELSE IF (licsv_column(i) > 6) THEN
        CALL add_val(csvline, v7d, mapper(i5), i1, i2, i3, i4, i6, no_miss, no_miss)
      ENDIF
    ENDDO

  ENDIF

  IF (csv_keep_miss .OR. no_miss .OR. (no_missa .AND. anaonly)) THEN
    WRITE(iun,'(A)')csv_record_getrecord(csvline)
  ENDIF
  CALL delete(csvline)

! final part of the loop over columns
  DO i = ndvar, 1, -1
    IF (icsv_colind(i) < icsv_colend(i)) THEN ! increment loop index
      icsv_colind(i) = icsv_colind(i) + 1
! prepare the dimension descriptor data
      CALL make_csv_desdata(v7d, icsv_columnorder(i), icsv_colind(i), &
       csv_desdata(icsv_columnorder(i)), mapper)
      EXIT
    ELSE ! end of loop for this index, reset and increment next index
      icsv_colind(i) = icsv_colstart(i)
    ENDIF
  ENDDO
  IF (i == 0) EXIT loop7d ! all counters have reached the end

END DO loop7d

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


SUBROUTINE make_csv_desdata(v7d, icol, ind, csv_desdata, mapper)
TYPE(vol7d),INTENT(inout) :: v7d
INTEGER,INTENT(in) :: icol
INTEGER,INTENT(in) :: ind
TYPE(csv_record),INTENT(inout) :: csv_desdata
TYPE(vol7d_var_mapper) :: mapper(:)

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
  IF (v7d%time(ind) /= datetime_miss) THEN
    CALL getval(v7d%time(ind), isodate=charbuffer(1:19))
    CALL csv_record_addfield(csv_desdata, charbuffer(1:19))
  ELSE
    CALL csv_record_addfield(csv_desdata, '')
  ENDIF

CASE(vol7d_level_d)
  CALL csv_record_addfield_miss(csv_desdata, v7d%level(ind)%level1)
  CALL csv_record_addfield_miss(csv_desdata, v7d%level(ind)%l1)
  CALL csv_record_addfield_miss(csv_desdata, v7d%level(ind)%level2)
  CALL csv_record_addfield_miss(csv_desdata, v7d%level(ind)%l2)

CASE(vol7d_timerange_d)
  CALL csv_record_addfield_miss(csv_desdata, v7d%timerange(ind)%timerange)
  CALL csv_record_addfield_miss(csv_desdata, v7d%timerange(ind)%p1)
  CALL csv_record_addfield_miss(csv_desdata, v7d%timerange(ind)%p2)

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
    CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%r(mapper%i5)%btable))
  CASE('d')
    CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%d(mapper%i5)%btable))
  CASE('i')
    CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%i(mapper%i5)%btable))
  CASE('b')
    CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%b(mapper%i5)%btable))
  CASE('c')
    CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%c(mapper%i5)%btable))
  END SELECT
CASE('aa')
  SELECT CASE(mapper%typ)
  CASE('r')
    CALL csv_record_addfield(csvline,'(Ana '// &
     TRIM(v7d%anavarattr%r(mapper%i5)%btable) &
     //','//TRIM(v7d%anaattr%r(mapper%i7)%btable)//')')
  CASE('d')
    CALL csv_record_addfield(csvline,'(Ana '// &
     TRIM(v7d%anavarattr%d(mapper%i5)%btable) &
     //','//TRIM(v7d%anaattr%d(mapper%i7)%btable)//')')
  CASE('i')
    CALL csv_record_addfield(csvline,'(Ana '// &
     TRIM(v7d%anavarattr%i(mapper%i5)%btable) &
     //','//TRIM(v7d%anaattr%i(mapper%i7)%btable)//')')
  CASE('b')
    CALL csv_record_addfield(csvline,'(Ana '// &
     TRIM(v7d%anavarattr%b(mapper%i5)%btable) &
     //','//TRIM(v7d%anaattr%b(mapper%i7)%btable)//')')
  CASE('c')
    CALL csv_record_addfield(csvline,'(Ana '// &
     TRIM(v7d%anavarattr%c(mapper%i5)%btable) &
     //','//TRIM(v7d%anaattr%c(mapper%i7)%btable)//')')
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
    CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%r(mapper%i5)%btable) &
     //','//TRIM(v7d%datiattr%r(mapper%i7)%btable)//')')
  CASE('d')
    CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%d(mapper%i5)%btable) &
     //','//TRIM(v7d%datiattr%d(mapper%i7)%btable)//')')
  CASE('i')
    CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%i(mapper%i5)%btable) &
     //','//TRIM(v7d%datiattr%i(mapper%i7)%btable)//')')
  CASE('b')
    CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%b(mapper%i5)%btable) &
     //','//TRIM(v7d%datiattr%b(mapper%i7)%btable)//')')
  CASE('c')
    CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%c(mapper%i5)%btable) &
     //','//TRIM(v7d%datiattr%c(mapper%i7)%btable)//')')
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


PROGRAM v7d_transform
#include "config.h"
USE log4fortran
USE char_utilities
USE file_utilities
USE getopt_m
USE io_units
USE vol7d_class
USE vol7d_class_compute
USE datetime_class
#ifdef HAVE_ORSIM
USE vol7d_oraclesim_class
#endif
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif
#ifdef HAVE_LIBGRIBAPI
USE grid_id_class
USE grid_class
USE gridinfo_class
#endif
USE grid_transform_class
use volgrid6d_class
USE geo_coord_class
USE vol7d_csv
!USE ISO_FORTRAN_ENV
IMPLICIT NONE

TYPE(op_option) :: options(50) ! remember to update dimension when adding options
TYPE(optionparser) :: opt
TYPE(csv_record) :: argparse
CHARACTER(len=8) :: input_format, coord_format

CHARACTER(len=512) :: input_file, output_file, output_format, output_template, &
 network_list, variable_list, anavariable_list, attribute_list, coord_file
CHARACTER(len=160) :: pre_trans_type
TYPE(vol7d_network), ALLOCATABLE :: nl(:)
CHARACTER(len=10), ALLOCATABLE :: vl(:), avl(:), al(:)
CHARACTER(len=23) :: start_date, end_date
CHARACTER(len=19) :: start_date_default, end_date_default
TYPE(datetime) :: now, s_d, e_d
INTEGER :: iun, ier, i, j, n, ninput, yy, mm, dd, iargc
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d) :: v7d, v7d_coord, v7dtmp, v7d_comp1, v7d_comp2, v7d_comp3
TYPE(geo_coordvect),POINTER :: poly(:)
DOUBLE PRECISION ::  ilon, ilat, flon, flat
TYPE(transform_def) :: trans
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_dba, v7d_dba_out
#endif
#ifdef HAVE_ORSIM
TYPE(vol7d_oraclesim) :: v7d_osim
#endif
TYPE(vol7d_network) :: set_network_obj
CHARACTER(len=network_name_len) :: set_network
CHARACTER(len=32) :: dsn, user, password
LOGICAL :: version, ldisplay
CHARACTER(len=512):: a_name
INTEGER :: category

! for computing
LOGICAL :: comp_regularize, comp_average, comp_cumulate, comp_keep, comp_sort, obso
CHARACTER(len=13) :: comp_stat_proc
CHARACTER(len=23) :: comp_step, comp_start
INTEGER :: istat_proc, ostat_proc
TYPE(timedelta) :: c_i, comp_max_step
TYPE(datetime) :: c_s
REAL :: comp_frac_valid

! for grib output
#ifdef HAVE_LIBGRIBAPI
TYPE(grid_file_id) :: ifile, ofile
TYPE(grid_id) :: gaid
TYPE(griddim_def) :: grid_out
TYPE(volgrid6d) :: vg6d
TYPE(gridinfo_def),POINTER :: gridinfo(:)
character(len=160) :: post_trans_type
#endif

NULLIFY(poly)
!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name,a_name_force="v7d_transform")
!init di log4fortran
ier=l4f_init()
!imposta a_name
category=l4f_category_get(a_name//".main")

now = datetime_new(now=datetime_utc)
CALL getval(now, year=yy, month=mm, day=dd)
CALL getval(datetime_new(year=yy, month=mm, day=dd)-timedelta_new(day=1), &
 isodate=start_date_default)
CALL getval(datetime_new(year=yy, month=mm, day=dd), isodate=end_date_default)

! define command-line options
CALL op_option_nullify(options)

! options for defining input
options(1) = op_option_new(' ', 'input-format', input_format, 'native', help= &
 'format of input, ''native'' for vol7d native binary file&
#ifdef HAVE_DBALLE
 &, ''BUFR'' for BUFR file with generic template, ''CREX'' for CREX file&
 &, ''dba'' for dballe database&
#endif
#ifdef HAVE_ORSIM
 &, ''orsim'' for SIM Oracle database&
#endif
 &')
options(2) = op_option_new('c', 'coord-file', coord_file, help= &
 'file with coordinates of interpolation points, required if a geographical &
 &transformation is requested')
coord_file=cmiss
options(3) = op_option_new(' ', 'coord-format', coord_format, &
#ifdef HAVE_DBALLE
 'BUFR', &
#else
 'native', &
#endif 
 & help='format of input file with coordinates, ''native'' for vol7d native binary file &
#ifdef HAVE_DBALLE
 &, ''BUFR'' for BUFR file, ''CREX'' for CREX file&
#endif
#ifdef HAVE_LIBSHP_FORTRAN
 &, ''shp'' for shapefile (interpolation on polygons)&
#endif
 &')

! input database options
options(4) = op_option_new('s', 'start-date', start_date, start_date_default, help= &
 'if input-format is of database type, initial date for extracting data')
options(5) = op_option_new('e', 'end-date', end_date, end_date_default, help= &
 'if input-format is of database type, final date for extracting data')
options(6) = op_option_new('n', 'network-list', network_list, '', help= &
 'if input-format is of database type, list of station networks to be extracted &
 &in the form of a comma-separated list of alphanumeric network identifiers')
options(7) = op_option_new('v', 'variable-list', variable_list, '', help= &
 'if input-format is of database type, list of data variables to be extracted &
 &in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B13011,B12101''')
options(8) = op_option_new(' ', 'anavariable-list', anavariable_list, '', help= &
 'if input-format is of database type, list of station variables to be extracted &
 &in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B01192,B01193,B07001''')
options(9) = op_option_new(' ', 'attribute-list', attribute_list, '', help= &
 'if input-format is of database type, list of data attributes to be extracted &
 &in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B33196,B33197''')
options(10) = op_option_new(' ', 'set-network', set_network, '', help= &
 'if input-format is of database type, collapse all the input data into a single &
 &pseudo-network with the given name, empty for keeping the original networks')

! option for displaying/processing
options(12) = op_option_new('d', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
options(13) = op_option_new(' ', 'comp-regularize', comp_regularize, help= &
 'regularize the time series keeping only the data at regular time steps')

options(14) = op_option_new(' ', 'comp-stat-proc', comp_stat_proc, '', help= &
 'statistically process data with an operator specified in the form [isp:]osp &
 &where isp is the statistical process of input data which has to be processed &
 &and osp is the statistical process to apply and which will appear in output &
 &timerange; possible values for isp and osp are 0=average, 1=accumulated, &
 &2=maximum, 3=minimum, 254=instantaneous, but not all the combinations &
 &make sense; if isp is not provided it is assumed to be equal to osp')

options(15) = op_option_new(' ', 'comp-average', obso, help= &
 'recompute average of averaged fields on a different time step, &
 &obsolete, use --comp-stat-proc 0 instead')
options(16) = op_option_new(' ', 'comp-cumulate', obso, help= &
 'recompute cumulation of accumulated fields on a different time step, &
 &obsolete, use --comp-stat-proc 1 instead')
options(17) = op_option_new(' ', 'comp-step', comp_step, '0000000001 00:00:00.000', help= &
 'length of regularization or statistical processing step in the format &
 &''YYYYMMDDDD hh:mm:ss.msc'', it can be simplified up to the form ''D hh''')
options(18) = op_option_new(' ', 'comp-start', comp_start, '', help= &
 'start of regularization, or statistical processing interval, an empty value means &
 &take the initial time step of the available data; the format is the same as for &
 &--start-date parameter')
options(19) = op_option_new(' ', 'comp-keep', comp_keep, help= &
 'keep the data that are not the result of the requested statistical processing, &
 &merging them with the result of the processing')
options(20) = op_option_new(' ', 'comp-frac-valid', comp_frac_valid, 1., help= &
 'specify the fraction of input data that has to be valid in order to consider a &
 &statistically processed value acceptable')
options(21) = op_option_new(' ', 'comp-sort', comp_sort, help= &
 'sort all sortable dimensions of the volume after the computations')

! option for interpolation processing
options(22) = op_option_new(' ', 'pre-trans-type', pre_trans_type, '', help= &
 'transformation type (sparse points to sparse points) to be applied before &
 &other computations, in the form ''trans-type:subtype''; &
 &''inter'' for interpolation, with subtypes ''near'', ''linear'', ''bilin''&
#ifdef HAVE_LIBSHP_FORTRAN
 &; ''polyinter'' for statistical processing within given polygons, &
 &with subtype ''average'', ''max'', ''min''&
#endif
 &; ''metamorphosis'' with subtype ''coordbb'' for selecting only data &
 &within a given bounding box&
 &; empty for no transformation')
#ifdef HAVE_LIBGRIBAPI
options(23) = op_option_new(' ', 'post-trans-type', post_trans_type, '', help= &
 'transformation type (sparse points to grid) to be applied after &
 &other computations, in the form ''trans-type:subtype''; &
 &''inter'' for interpolation, with subtype ''linear''; &
 &''boxinter'' for statistical processing within output grid box, &
 &with subtype ''average''; &
 &empty for no transformation; this option is compatible with output &
 &on gridded format only (see output-format)')
#endif

options(24) = op_option_new(' ', 'ilon', ilon, 0.0D0, help= &
 'longitude of the southwestern bounding box corner')
options(25) = op_option_new(' ', 'ilat', ilat, 30.D0, help= &
 'latitude of the southwestern bounding box corner')
options(26) = op_option_new(' ', 'flon', flon, 30.D0, help= &
 'longitude of the northeastern bounding box corner')
options(27) = op_option_new(' ', 'flat', flat, 60.D0, help= &
 'latitude of the northeastern bounding box corner')

! options for defining output
output_template = ''
options(29) = op_option_new(' ', 'output-format', output_format, 'native', help= &
 'format of output file, in the form ''name[:template]''; ''native'' for vol7d &
 &native binary format (no template to be specified)&
#ifdef HAVE_DBALLE
 &; ''BUFR'' and ''CREX'' for corresponding formats, with template in the form &
 &''category.subcategory.localcategory'' or as an alias like ''synop'', ''metar'', &
 &''temp'', ''generic'', empty for ''generic''&
#endif
#ifdef HAVE_LIBGRIBAPI
 &; ''grib_api'' for gridded output in grib format, template (required) is the &
 &path name of a grib file in which the first message defines the output grid and &
 &is used as a template for the output grib messages, (see also post-trans-type)&
#endif
 &; csv for formatted csv format (no template to be specified)')

! options for configuring csv output
options(30) = op_option_new(' ', 'csv-volume', csv_volume, 'all', help= &
 'vol7d volumes to be output to csv: ''all'' for all volumes, &
 &''ana'' for station volumes only or ''data'' for data volumes only')
options(31) = op_option_new(' ', 'csv-column', csv_column, &
 'time,timerange,ana,level,network', help= &
 'list of columns (excluding variables) that have to appear in csv output: &
 &a comma-separated combination of ''time,timerange,level,ana,network'' &
 &in the desired order')
options(32) = op_option_new(' ', 'csv-columnorder', csv_columnorder, &
 'time,timerange,ana,level,network', help= &
 'order of looping on columns (excluding variables) that have to appear in &
 &csv output, the format is the same as for the --csv-column parameter &
 &but here all the column identifiers have to be present')
options(33) = op_option_new(' ', 'csv-variable', csv_variable, 'all', help= &
 'list of variables that have to appear in the data columns of csv output: &
 &''all'' or a comma-separated list of B-table alphanumeric codes, e.g. &
 &''B10004,B12101'' in the desired order')
options(34) = op_option_new(' ', 'csv-header', csv_header, 2, help= &
 'write 0 to 2 header lines at the beginning of csv output')
options(35) = op_option_new(' ', 'csv-keep-miss', csv_keep_miss, help= &
 'keep records containing only missing values in csv output')
options(36) = op_option_new(' ', 'csv-norescale', csv_no_rescale, help= &
 'do not rescale in output integer variables according to their scale factor')

! obsolete options
options(45) = op_option_new(' ', 'comp-discard', obso, help= &
 'obsolete option, use --comp-keep with opposite meaning')
options(46) = op_option_new(' ', 'csv-skip-miss', obso, help= &
 'obsolete option, use --csv-keep-miss with opposite meaning')

! help options
options(49) = op_option_help_new('h', 'help', help= &
 'show an help message and exit')
options(50) = op_option_new(' ', 'version', version, help= &
 'show version and exit')


! define the option parser
opt = optionparser_new(options, description_msg= &
 'Vol7d transformation application, it imports a vol7d volume of sparse point data &
 &from a native vol7d file&
#ifdef HAVE_DBALLE
 &, from a dbAll.e database, from a BUFR/CREX file&
#endif
#ifdef HAVE_ORSIM
 &, from SIM Oracle database&
#endif
 & and exports it into a native v7d file&
#ifdef HAVE_DBALLE
 &, into a BUFR/CREX file&
#endif
#ifdef HAVE_LIBGRIBAPI
 &, into a GRIB file&
#endif
 &, or into a configurable formatted csv file. &
 &If input-format is of file type, inputfile ''-'' indicates stdin, &
 &if input-format is of database type, inputfile specifies &
 &database access info in the form user/password@dsn, &
 &if empty or ''-'', a suitable default is used. &
 &If output-format is of file type, outputfile ''-'' indicates stdout.', &
 usage_msg='v7d_transform [options] inputfile1 [inputfile2...] outputfile')

! parse options and check for errors
optind = optionparser_parseoptions(opt)
IF (optind <= 0) THEN
  CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters')
  CALL EXIT(1)
ENDIF

IF (version) THEN
  WRITE(*,'(A,1X,A)')'v7d_transform',VERSION
  CALL exit(0)
ENDIF

! check input/output files
i = iargc() - optind
IF (i < 0) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'input file missing')
  CALL EXIT(1)
ELSE IF (i < 1) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'output file missing')
  CALL EXIT(1)
ENDIF
CALL getarg(iargc(), output_file)

! check obsolete arguments
IF (obso) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category, L4F_ERROR, &
   'arguments --comp-average --comp-cumulate --csv-skip-miss --comp-discard')
  CALL l4f_category_log(category, L4F_ERROR, &
   'are obsolete, please read help')
  CALL EXIT(1)
ENDIF

! generate network
IF (LEN_TRIM(network_list) > 0) THEN
  n = word_split(network_list, w_s, w_e, ',')
  ALLOCATE(nl(n))
  DO i = 1, n
    CALL init(nl(i), name=network_list(w_s(i):w_e(i)))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF
! generate variable lists
IF (LEN_TRIM(variable_list) > 0) THEN
  n = word_split(variable_list, w_s, w_e, ',')
  ALLOCATE(vl(n))
  DO i = 1, n
    vl(i) = variable_list(w_s(i):w_e(i))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF
IF (LEN_TRIM(anavariable_list) > 0) THEN
  n = word_split(anavariable_list, w_s, w_e, ',')
  ALLOCATE(avl(n))
  DO i = 1, n
    avl(i) = anavariable_list(w_s(i):w_e(i))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF
IF (LEN_TRIM(attribute_list) > 0) THEN
  n = word_split(attribute_list, w_s, w_e, ',')
  ALLOCATE(al(n))
  DO i = 1, n
    al(i) = attribute_list(w_s(i):w_e(i))
  ENDDO
  DEALLOCATE(w_s, w_e)
ENDIF
! time-related arguments
s_d = datetime_new(isodate=start_date)
e_d = datetime_new(isodate=end_date)
c_i = timedelta_new(isodate=comp_step)
IF (comp_start /= '') THEN
  c_s = datetime_new(isodate=comp_start)
ELSE
  c_s = datetime_miss
ENDIF

! check comp_stat_proc
istat_proc = imiss
ostat_proc = imiss
IF (comp_stat_proc /= '') THEN
  CALL init(argparse, comp_stat_proc, ':', nfield=n)
  IF (n == 1) THEN
    CALL csv_record_getfield(argparse, ostat_proc)
    istat_proc = ostat_proc
  ELSE  IF (n == 2) THEN
    CALL csv_record_getfield(argparse, istat_proc)
    CALL csv_record_getfield(argparse, ostat_proc)
  ENDIF
  CALL delete(argparse)
  IF (.NOT.c_e(istat_proc) .OR. .NOT.c_e(ostat_proc)) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, wrong syntax for --comp-stat-proc: ' &
     //comp_stat_proc)
    CALL EXIT(1)
  ENDIF
ENDIF


CALL init(v7d_coord)
! import coord_file
IF (c_e(coord_file)) THEN
  IF (coord_format == 'native') THEN
    CALL import(v7d_coord, filename=input_file)

#ifdef HAVE_DBALLE
  ELSE IF (coord_format == 'BUFR' .OR. coord_format == 'CREX') THEN
    CALL init(v7d_dba, filename=coord_file, format=coord_format, file=.TRUE., &
     write=.FALSE., categoryappend="anagrafica")
    CALL import(v7d_dba, anaonly=.TRUE.)
    v7d_coord = v7d_dba%vol7d
! destroy v7d_ana without deallocating the contents passed to v7d
    CALL init(v7d_dba%vol7d)
    CALL delete(v7d_dba)

#endif
#ifdef HAVE_LIBSHP_FORTRAN
  ELSE IF (coord_format == 'shp') THEN
    NULLIFY(poly)
    CALL import(poly, shpfile=coord_file)
    IF (.NOT.ASSOCIATED(poly)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error importing shapefile '//TRIM(coord_file))
      CALL EXIT(1)
    ENDIF

#endif
  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(coord_format)//' in --coord-format not valid or not supported.')
    CALL EXIT(1)
  ENDIF

ENDIF

! check csv-column
CALL parse_v7d_column(csv_column, icsv_column, '--csv-column', .FALSE.)
CALL parse_v7d_column(csv_columnorder, icsv_columnorder, '--csv-columnorder', .TRUE.)

! check output format/template
n = word_split(output_format, w_s, w_e, ':')
IF (n >= 2) THEN ! set output template if present
  output_template = output_format(w_s(2):w_e(2))
  output_format(w_e(1)+1:) = ' '
ENDIF
DEALLOCATE(w_s, w_e)


! import data looping on input files
CALL init(v7d)
DO ninput = optind, iargc()-1
  CALL getarg(ninput, input_file)

  IF (input_format == 'native') THEN
    IF (input_file == '-') THEN ! stdin_unit does not work with unformatted
      CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit.')
      input_file='/dev/stdin'
    ENDIF
    CALL import(v7dtmp, filename=input_file)

#ifdef HAVE_DBALLE
  ELSE IF (input_format == 'BUFR' .OR. input_format == 'CREX') THEN
    IF (input_file == '-') THEN
      CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit.')
      input_file='/dev/stdin'
    ENDIF
    CALL init(v7d_dba, filename=input_file, FORMAT=input_format, file=.TRUE.)
    CALL IMPORT(v7d_dba)
    v7dtmp = v7d_dba%vol7d
    CALL init(v7d_dba%vol7d) ! nullify without deallocating

  ELSE IF (input_format == 'dba') THEN
    IF (.NOT.ALLOCATED(nl) .OR. .NOT.ALLOCATED(vl)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error in command-line parameters, it is necessary to provide --network-list &
       &and --variable-list with dbAll.e source.')
      CALL EXIT(1)
    ENDIF
    CALL parse_dba_access_info(input_file, dsn, user, password)
    CALL init(v7d_dba, dsn=dsn, user=user, password=password, file=.FALSE.)
    CALL import(v7d_dba, vl, nl, timei=s_d, timef=e_d)
    v7dtmp = v7d_dba%vol7d
    CALL init(v7d_dba%vol7d) ! nullify without deallocating
#endif

#ifdef HAVE_ORSIM
  ELSE IF (input_format == 'orsim') THEN
    IF (.NOT.ALLOCATED(nl) .OR. (.NOT.ALLOCATED(vl) .AND. .NOT.ALLOCATED(avl))) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error in command-line parameters, it is necessary to provide --network-list')
      CALL l4f_category_log(category, L4F_ERROR, &
       'and either --variable-list or --anavariable-list with SIM Oracle source')
      CALL EXIT(1)
    ENDIF
    CALL parse_dba_access_info(input_file, dsn, user, password)
    CALL init(v7d_osim, dsn=dsn, user=user, password=password, time_definition=0)
    IF (.NOT.ALLOCATED(vl)) ALLOCATE(vl(0)) ! allocate if missing
    IF (.NOT.ALLOCATED(avl)) ALLOCATE(avl(0)) ! allocate if missing
    IF (.NOT.ALLOCATED(al)) ALLOCATE(al(0)) ! allocate if missing
    IF (set_network /= '') THEN
      CALL init(set_network_obj, name=set_network)
    ELSE
      set_network_obj = vol7d_network_miss
    ENDIF
    IF (SIZE(vl) > 0) THEN
      CALL import(v7d_osim, vl, nl, timei=s_d, timef=e_d, anavar=avl, attr=al, &
       set_network=set_network_obj)
    ELSE
      CALL import(v7d_osim, nl, anavar=avl, &
       set_network=set_network_obj)
    ENDIF
    v7dtmp = v7d_osim%vol7d
    CALL init(v7d_osim%vol7d) ! nullify without deallocating
#endif

  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(input_format)//' in --input-format not valid or not supported.')
    CALL EXIT(1)
  ENDIF

  CALL vol7d_merge(v7d, v7dtmp) ! smart merge in v7d
  CALL delete(v7dtmp)

ENDDO

! displaying/processing
#ifdef HAVE_DBALLE
CALL vol7d_dballe_set_var_du(v7d)
#endif

IF (ldisplay) CALL display(v7d)

IF (pre_trans_type /= '') THEN
  n = word_split(pre_trans_type, w_s, w_e, ':')
  IF (n >= 2) THEN ! syntax is correct
    CALL init(trans, trans_type=pre_trans_type(w_s(1):w_e(1)), &
     ilon=ilon, ilat=ilat, flon=flon, flat=flat, &
     sub_type=pre_trans_type(w_s(2):w_e(2)), categoryappend="transformation1")
    IF (ASSOCIATED(poly)) THEN ! improve
      CALL transform(trans, vol7d_in=v7d, vol7d_out=v7d_comp1, v7d=v7d_coord, &
       poly=poly, categoryappend="transform1")
    ELSE
      CALL transform(trans, vol7d_in=v7d, vol7d_out=v7d_comp1, v7d=v7d_coord, &
       categoryappend="transform1")
    ENDIF
    CALL delete(trans)
  ELSE ! syntax is wrong
    CALL init(v7d_comp1)
    CALL l4f_category_log(category, L4F_ERROR, &
     'pre-transformation syntax '//TRIM(pre_trans_type)//' non valid')
  ENDIF
  DEALLOCATE(w_s, w_e)

  IF (c_e(v7d_comp1)) THEN ! transformation successful, use the new volume
    v7d = v7d_comp1
    CALL init(v7d_comp1) ! detach it
  ELSE ! otherwise continue with original volume
    CALL l4f_category_log(category, L4F_ERROR, &
     'pre-transformation '//TRIM(pre_trans_type)//' failed')
    CALL l4f_category_log(category, L4F_ERROR, &
     'continuing with untransformed data')
  ENDIF
ENDIF

IF (comp_regularize) THEN
  CALL init(v7d_comp1)
  CALL vol7d_regularize_time(v7d, v7d_comp1, c_i, c_s)
  CALL delete(v7d)
  v7d = v7d_comp1
ENDIF

IF (c_e(istat_proc) .AND. c_e(ostat_proc)) THEN
  CALL init(v7d_comp1, time_definition=v7d%time_definition)
  CALL init(v7d_comp2, time_definition=v7d%time_definition)

  IF (input_format == 'BUFR' .OR. input_format == 'CREX') THEN
    call vol7d_convr(v7d,v7dtmp)
    call delete(v7d)
    v7d=v7dtmp
  end if

  CALL vol7d_compute_stat_proc(v7d, v7d_comp1, istat_proc, ostat_proc, c_i, c_s, &
   full_steps=.TRUE., frac_valid=comp_frac_valid, &
   max_step=timedelta_depop(c_i)/10, weighted=.TRUE., other=v7d_comp3)

  CALL delete(v7d)
  v7d = v7d_comp3

! merge the tho computed fields
  IF (.NOT. comp_keep) THEN ! the user is not interested in the other volume
    CALL delete(v7d)
    v7d = v7d_comp1
  ELSE
    CALL vol7d_merge(v7d, v7d_comp1, sort=.TRUE.)
  ENDIF
ENDIF

! sort
IF (comp_sort) THEN
  CALL vol7d_smart_sort(v7d, lsort_time=.TRUE., lsort_timerange=.TRUE., lsort_level=.TRUE.)
ENDIF

! output
IF (output_format == 'native') THEN
  IF (output_file == '-') THEN ! stdout_unit does not work with unformatted
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  iun = getunit()
  OPEN(iun, file=output_file, form='UNFORMATTED', access=stream_if_possible)
  CALL export(v7d, unit=iun)
  CLOSE(iun)
  CALL delete(v7d)

ELSE IF (output_format == 'csv') THEN
  IF (output_file == '-') THEN
    iun = stdout_unit
  ELSE
    iun = getunit()
    OPEN(iun, file=output_file, form='FORMATTED', access='SEQUENTIAL')
  ENDIF
  CALL csv_export(v7d, iun)
  IF (output_file /= '-') CLOSE(iun)
  CALL delete(v7d)

#ifdef HAVE_DBALLE
ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX') THEN
  IF (output_file == '-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  IF (output_template == '') output_template = 'generic'
  CALL init(v7d_dba_out, filename=output_file, FORMAT=output_format, file=.TRUE., &
   WRITE=.TRUE., wipe=.TRUE.)
  v7d_dba_out%vol7d = v7d
  CALL init(v7d) ! nullify without deallocating
  CALL export(v7d_dba_out, template=output_template)
  CALL delete(v7d_dba_out)
#endif

#ifdef HAVE_LIBGRIBAPI
ELSE IF (output_format == 'grib_api') THEN

  IF (post_trans_type /= '' .AND. output_template /= '') THEN
    n = word_split(post_trans_type, w_s, w_e, ':')
    IF (n >= 2) THEN ! syntax is correct

! initialize transform
      CALL init(trans, trans_type=post_trans_type(w_s(1):w_e(1)), &
       sub_type=post_trans_type(w_s(2):w_e(2)), categoryappend="transformation2")
! open grib template file and import first message, format:template is
! reconstructed here, improve
      ifile = grid_file_id_new(TRIM(output_format)//':'//TRIM(output_template), 'r')
      gaid = grid_id_new(ifile)
      IF (c_e(gaid)) THEN

! use the message  as a template for defining the grid
        CALL import(grid_out, gaid)
! interpolate sparse data over the requested grid
        CALL transform(trans, grid_out, v7d, vg6d, categoryappend="transform2")
! TODO check here whether the transformation succeeded
! serialize the interpolated volume into a gridinfo object keeping the
! same grib template used for the grid
        CALL export((/vg6d/), gridinfo, gaid_template=gaid)

        IF (ASSOCIATED(gridinfo)) THEN
! export to output grib file
          ofile = grid_file_id_new(output_file, 'w')
          DO i = 1, SIZE(gridinfo)
            CALL export(gridinfo(i))
            CALL export(gridinfo(i)%gaid, ofile)
            CALL delete(gridinfo(i))
          ENDDO
          CALL delete(ofile)

        ELSE ! export to gridinfo failed
          CALL l4f_category_log(category,L4F_ERROR, &
           'export of transformed volume to grib failed')
        ENDIF
        CALL delete(ifile)

      ELSE
        CALL l4f_category_log(category,L4F_ERROR, &
         'cannot read any grib message from template file '//TRIM(output_template))
      ENDIF
      CALL delete(trans)

    ELSE ! syntax is wrong
      CALL l4f_category_log(category, L4F_ERROR, &
       'post-transformation syntax '//TRIM(post_trans_type)//' non valid')
    ENDIF
    DEALLOCATE(w_s, w_e)

  ELSE
    CALL l4f_category_log(category,L4F_ERROR, &
     'output format '//TRIM(output_format)// &
     ' requires post-trans-type and output-template to be defined')
    CALL l4f_category_log(category,L4F_ERROR, &
     'post-trans-type: '//TRIM(post_trans_type)// &
     '; output template '//TRIM(output_template))
  ENDIF

#endif

ELSE IF (output_format /= '') THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, format '// &
   TRIM(output_format)//' in --output-format not valid or not supported.')
  CALL EXIT(1)
ENDIF

! cleanly close the databases
IF (input_format == 'native') THEN
  CALL delete(v7d) ! controllare? input native output bufr
#ifdef HAVE_DBALLE
ELSE IF (input_format == 'BUFR' .OR. input_format == 'CREX' &
 .OR. input_format == 'dba') THEN
  CALL delete(v7d_dba)
#endif
#ifdef HAVE_ORSIM
ELSE IF (input_format == 'orsim') THEN
  CALL delete(v7d_osim)
#endif
ENDIF

CONTAINS

SUBROUTINE parse_dba_access_info(string, dsn, user, password)
CHARACTER(len=*),INTENT(in) :: string
CHARACTER(len=*),INTENT(out) :: dsn
CHARACTER(len=*),INTENT(out) :: user
CHARACTER(len=*),INTENT(out) :: password

INTEGER :: bar, at

IF (string == '-' .OR. string == '') THEN
  dsn = cmiss
  user = cmiss
  password = cmiss
ELSE
  bar = INDEX(string, '/')
  at = INDEX(string, '@')
  IF (bar > 0 .AND. at > bar) THEN
    user = string(:bar-1)
    password = string(bar+1:at-1)
    dsn = string(at+1:)
  ELSE
    CALL optionparser_printhelp(opt)
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, database access info '// &
     TRIM(string)//' not valid.')
    CALL EXIT(1)
  ENDIF
ENDIF

END SUBROUTINE parse_dba_access_info


SUBROUTINE parse_v7d_column(ccol, icol, par_name, check_all)
CHARACTER(len=*),INTENT(in) :: ccol
INTEGER,INTENT(out) :: icol(:)
CHARACTER(len=*),INTENT(in) :: par_name
LOGICAL,INTENT(in) :: check_all

INTEGER :: i, j, nc
INTEGER,POINTER :: w_s(:), w_e(:)

nc = word_split(ccol, w_s, w_e, ',')
j = 0
icol(:) = -1
DO i = 1, MIN(nc, SIZE(icol))
  SELECT CASE(ccol(w_s(i):w_e(i)))
  CASE('time')
    j = j + 1
    icol(j) = vol7d_time_d
  CASE('timerange')
    j = j + 1
    icol(j) = vol7d_timerange_d
  CASE('level')
    j = j + 1
    icol(j) = vol7d_level_d
  CASE('ana')
    j = j + 1
    icol(j) = vol7d_ana_d
  CASE('var')
    j = j + 1
    icol(j) = vol7d_var_d
  CASE('network')
    j = j + 1
    icol(j) = vol7d_network_d
  CASE('value')
    j = j + 1
    icol(j) = 7
  CASE default
    CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters, column '// &
     ccol(w_s(i):w_e(i))//' in '//TRIM(par_name)//' not valid.')
    CALL EXIT(1)
  END SELECT
ENDDO
nc = j
DEALLOCATE(w_s, w_e)

IF (check_all) THEN
  IF (ALL(icol /= vol7d_time_d) .OR. ALL(icol /= vol7d_timerange_d) .OR. &
   ALL(icol /= vol7d_level_d) .OR. ALL(icol /= vol7d_ana_d) .OR. &
   ALL(icol /= vol7d_network_d)) THEN
    CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters, some columns missing in '//TRIM(par_name)//' .')
    CALL raise_fatal_error()
  ENDIF
  IF (ANY(icol == 7)) THEN
    CALL l4f_category_log(category,L4F_ERROR,"column 'value' not valid in "// &
     TRIM(par_name)//' .')
    CALL raise_fatal_error()
  ENDIF
ENDIF

END SUBROUTINE parse_v7d_column

END PROGRAM v7d_transform

