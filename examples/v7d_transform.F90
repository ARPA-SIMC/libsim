MODULE vol7d_csv
USE vol7d_class
USE file_utilities
IMPLICIT NONE

! csv output configuration
CHARACTER(len=8) :: csv_volume
CHARACTER(len=512) :: csv_column, csv_variable
LOGICAL :: csv_skip_miss, csv_no_rescale
INTEGER :: csv_header, icsv_column(7)

CONTAINS

SUBROUTINE csv_export(v7d, iun)
TYPE(vol7d),INTENT(inout) :: v7d
INTEGER,INTENT(in) :: iun

INTEGER :: licsv_column(SIZE(icsv_column))
LOGICAL :: no_miss, anaonly
CHARACTER(len=50) :: desdata(7)
CHARACTER(len=128) :: charbuffer
TYPE(csv_record) :: csvline, csv_desdata(7)
INTEGER :: i, i1, i2, i3, i4, i5, i6, i7, nv, datastart
REAL(kind=fp_geo) :: l1, l2
INTEGER,POINTER :: w_s(:), w_e(:)

licsv_column(:) = icsv_column(:)

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

! Create header entries for all the v7d non-variables dimensions
  CALL csv_record_addfield(csv_desdata(vol7d_ana_a), 'Longitude')
  CALL csv_record_addfield(csv_desdata(vol7d_ana_a), 'Latitude')
  CALL csv_record_addfield(csv_desdata(vol7d_time_d), 'Date')
  CALL csv_record_addfield(csv_desdata(vol7d_level_d), 'Level1')
  CALL csv_record_addfield(csv_desdata(vol7d_level_d), 'L1')
  CALL csv_record_addfield(csv_desdata(vol7d_level_d), 'Level2')
  CALL csv_record_addfield(csv_desdata(vol7d_level_d), 'L2')
  CALL csv_record_addfield(csv_desdata(vol7d_timerange_d), 'Time range')
  CALL csv_record_addfield(csv_desdata(vol7d_timerange_d), 'P1')
  CALL csv_record_addfield(csv_desdata(vol7d_timerange_d), 'P2')
  CALL csv_record_addfield(csv_desdata(vol7d_network_d), 'Report')

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
  anaonly = SIZE(v7d%time) == 0 .AND. SIZE(v7d%level) == 0 .AND. &
   SIZE(v7d%timerange) == 0
  IF (anaonly) THEN
    datastart = 0
  ELSE
    datastart = 1
  ENDIF

  CALL init(csvline)
  DO i = 1, SIZE(licsv_column) ! add the required header entries in the desirded order
    IF (licsv_column(i) > 0) &
     CALL csv_record_addfield(csvline,csv_desdata(licsv_column(i)))
  ENDDO
! and now add the header entries for the variables
! ana variables
  IF (ASSOCIATED(v7d%anavar%r)) THEN
    DO i5 = 1, SIZE(v7d%anavar%r)
      CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%r(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%anavar%d)) THEN
    DO i5 = 1, SIZE(v7d%anavar%d)
      CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%d(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%anavar%i)) THEN
    DO i5 = 1, SIZE(v7d%anavar%i)
      CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%i(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%anavar%b)) THEN
    DO i5 = 1, SIZE(v7d%anavar%b)
      CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%b(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%anavar%c)) THEN
    DO i5 = 1, SIZE(v7d%anavar%c)
      CALL csv_record_addfield(csvline,'Ana '//TRIM(v7d%anavar%c(i5)%btable))
    ENDDO
  ENDIF
! ana attr variables
  IF (ASSOCIATED(v7d%anaattr%r) .AND. ASSOCIATED(v7d%anavarattr%r)) THEN
    DO i7 = 1, SIZE(v7d%anaattr%r)
      DO i5 = 1, SIZE(v7d%anavarattr%r)
        CALL csv_record_addfield(csvline,'(Ana '// &
         TRIM(v7d%anavarattr%r(i5)%btable) &
         //','//TRIM(v7d%anaattr%r(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%anaattr%d) .AND. ASSOCIATED(v7d%anavarattr%d)) THEN
    DO i7 = 1, SIZE(v7d%anaattr%d)
      DO i5 = 1, SIZE(v7d%anavarattr%d)
        CALL csv_record_addfield(csvline,'(Ana '// &
         TRIM(v7d%anavarattr%d(i5)%btable) &
         //','//TRIM(v7d%anaattr%d(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%anaattr%i) .AND. ASSOCIATED(v7d%anavarattr%i)) THEN
    DO i7 = 1, SIZE(v7d%anaattr%i)
      DO i5 = 1, SIZE(v7d%anavarattr%i)
        CALL csv_record_addfield(csvline,'(Ana '// &
         TRIM(v7d%anavarattr%i(i5)%btable) &
         //','//TRIM(v7d%anaattr%i(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%anaattr%b) .AND. ASSOCIATED(v7d%anavarattr%b)) THEN
    DO i7 = 1, SIZE(v7d%anaattr%b)
      DO i5 = 1, SIZE(v7d%anavarattr%b)
        CALL csv_record_addfield(csvline,'(Ana '// &
         TRIM(v7d%anavarattr%b(i5)%btable) &
         //','//TRIM(v7d%anaattr%b(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%anaattr%c) .AND. ASSOCIATED(v7d%anavarattr%c)) THEN
    DO i7 = 1, SIZE(v7d%anaattr%c)
      DO i5 = 1, SIZE(v7d%anavarattr%c)
        CALL csv_record_addfield(csvline,'(Ana '// &
         TRIM(v7d%anavarattr%c(i5)%btable) &
         //','//TRIM(v7d%anaattr%c(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
! data variables
  IF (ASSOCIATED(v7d%dativar%r)) THEN
    DO i5 = 1, SIZE(v7d%dativar%r)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%r(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%dativar%d)) THEN
    DO i5 = 1, SIZE(v7d%dativar%d)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%d(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%dativar%i)) THEN
    DO i5 = 1, SIZE(v7d%dativar%i)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%i(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%dativar%b)) THEN
    DO i5 = 1, SIZE(v7d%dativar%b)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%b(i5)%btable))
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%dativar%c)) THEN
    DO i5 = 1, SIZE(v7d%dativar%c)
      CALL csv_record_addfield(csvline,TRIM(v7d%dativar%c(i5)%btable))
    ENDDO
  ENDIF
! data attr variables
  IF (ASSOCIATED(v7d%datiattr%r) .AND. ASSOCIATED(v7d%dativarattr%r)) THEN
    DO i7 = 1, SIZE(v7d%datiattr%r)
      DO i5 = 1, SIZE(v7d%dativarattr%r)
        CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%r(i5)%btable) &
         //','//TRIM(v7d%datiattr%r(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%datiattr%d) .AND. ASSOCIATED(v7d%dativarattr%d)) THEN
    DO i7 = 1, SIZE(v7d%datiattr%d)
      DO i5 = 1, SIZE(v7d%dativarattr%d)
        CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%d(i5)%btable) &
         //','//TRIM(v7d%datiattr%d(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%datiattr%i) .AND. ASSOCIATED(v7d%dativarattr%i)) THEN
    DO i7 = 1, SIZE(v7d%datiattr%i)
      DO i5 = 1, SIZE(v7d%dativarattr%i)
        CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%i(i5)%btable) &
         //','//TRIM(v7d%datiattr%i(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%datiattr%b) .AND. ASSOCIATED(v7d%dativarattr%b)) THEN
    DO i7 = 1, SIZE(v7d%datiattr%b)
      DO i5 = 1, SIZE(v7d%dativarattr%b)
        CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%b(i5)%btable) &
         //','//TRIM(v7d%datiattr%b(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF
  IF (ASSOCIATED(v7d%datiattr%c) .AND. ASSOCIATED(v7d%dativarattr%c)) THEN
    DO i7 = 1, SIZE(v7d%datiattr%c)
      DO i5 = 1, SIZE(v7d%dativarattr%c)
        CALL csv_record_addfield(csvline,'('//TRIM(v7d%dativarattr%c(i5)%btable) &
         //','//TRIM(v7d%datiattr%c(i7)%btable)//')')
      ENDDO
    ENDDO
  ENDIF

  WRITE(iun,'(A)')csv_record_getrecord(csvline)
  CALL delete(csvline)
ENDIF

DO i = 1, SIZE(csv_desdata)
  CALL init(csv_desdata(i))
ENDDO

! Create data entries for all the v7d non-variables dimensions
DO i2 = datastart, size(v7d%time)
  IF (i2 > 0) THEN
    CALL csv_record_rewind(csv_desdata(2))
    CALL getval(v7d%time(i2), isodate=charbuffer(1:19))
    CALL csv_record_addfield(csv_desdata(2), charbuffer(1:19))
  ENDIF
  DO i4 = datastart, SIZE(v7d%timerange)
    IF (i4 > 0) THEN
      CALL csv_record_rewind(csv_desdata(4))
      CALL csv_record_addfield_miss(csv_desdata(4), v7d%timerange(i4)%timerange)
      CALL csv_record_addfield_miss(csv_desdata(4), v7d%timerange(i4)%p1)
      CALL csv_record_addfield_miss(csv_desdata(4), v7d%timerange(i4)%p2)
    ENDIF
    DO i3 = datastart, SIZE(v7d%level)
      IF (i3 > 0) THEN
        CALL csv_record_rewind(csv_desdata(3))
        CALL csv_record_addfield_miss(csv_desdata(3), v7d%level(i3)%level1)
        CALL csv_record_addfield_miss(csv_desdata(3), v7d%level(i3)%l1)
        CALL csv_record_addfield_miss(csv_desdata(3), v7d%level(i3)%level2)
        CALL csv_record_addfield_miss(csv_desdata(3), v7d%level(i3)%l2)
      ENDIF
      DO i6 = 1, SIZE(v7d%network)
        CALL csv_record_rewind(csv_desdata(6))
        CALL csv_record_addfield_miss(csv_desdata(6), TRIM(v7d%network(i6)%name))
        DO i1 = 1, SIZE(v7d%ana)
          CALL csv_record_rewind(csv_desdata(1))
          CALL getval(v7d%ana(i1)%coord, lon=l1, lat=l2)
          CALL csv_record_addfield_miss(csv_desdata(1), l1)
          CALL csv_record_addfield_miss(csv_desdata(1), l2)

          CALL init(csvline)
          DO i = 1, SIZE(licsv_column) ! add the required data entries in the desirded order
            IF (licsv_column(i) > 0) &
             CALL csv_record_addfield(csvline,csv_desdata(licsv_column(i)))
          ENDDO
          no_miss = .FALSE. ! keep track of line with all missing data
! and now add the data entries for the variables
! ana variables
          IF (ASSOCIATED(v7d%volanar)) THEN
            DO i5 = 1, SIZE(v7d%volanar(i1,:,i6))
              CALL csv_record_addfield_miss(csvline, v7d%volanar(i1,i5,i6))
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%volanad)) THEN
            DO i5 = 1, SIZE(v7d%volanad(i1,:,i6))
              CALL csv_record_addfield_miss(csvline, v7d%volanad(i1,i5,i6))
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%volanai)) THEN
            DO i5 = 1, SIZE(v7d%volanai(i1,:,i6))
              CALL addfieldi(v7d%anavar%i(i5), v7d%volanai(i1,i5,i6))
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%volanab)) THEN
            DO i5 = 1, SIZE(v7d%volanab(i1,:,i6))
              CALL addfieldb(v7d%anavar%b(i5), v7d%volanab(i1,i5,i6))
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%volanac)) THEN
            DO i5 = 1, SIZE(v7d%volanac(i1,:,i6))
              CALL addfieldc(v7d%anavar%c(i5), v7d%volanac(i1,i5,i6))
            ENDDO
          ENDIF
! ana attr variables
          IF (ASSOCIATED(v7d%volanaattrr)) THEN
            DO i7 = 1, SIZE(v7d%anaattr%r)
              DO i5 = 1, SIZE(v7d%anavarattr%r)
                CALL csv_record_addfield_miss(csvline, v7d%volanaattrr(i1,i5,i6,i7))
              ENDDO
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%volanaattrd)) THEN
            DO i7 = 1, SIZE(v7d%anaattr%d)
              DO i5 = 1, SIZE(v7d%anavarattr%d)
                CALL csv_record_addfield_miss(csvline, v7d%volanaattrd(i1,i5,i6,i7))
              ENDDO
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%volanaattri)) THEN
            DO i7 = 1, SIZE(v7d%anaattr%i)
              DO i5 = 1, SIZE(v7d%anavarattr%i)
                CALL addfieldi(v7d%anaattr%i(i7), v7d%volanaattri(i1,i5,i6,i7))
              ENDDO
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%volanaattrb)) THEN
            DO i7 = 1, SIZE(v7d%anaattr%b)
              DO i5 = 1, SIZE(v7d%anavarattr%b)
                CALL addfieldb(v7d%anaattr%b(i7), v7d%volanaattrb(i1,i5,i6,i7))
              ENDDO
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%volanaattrc)) THEN
            DO i7 = 1, SIZE(v7d%anaattr%c)
              DO i5 = 1, SIZE(v7d%anavarattr%c)
                CALL addfieldc(v7d%anaattr%c(i7), v7d%volanaattrc(i1,i5,i6,i7))
              ENDDO
            ENDDO
          ENDIF
! data variables
          IF (ASSOCIATED(v7d%voldatir)) THEN
            DO i5 = 1, SIZE(v7d%voldatir(i1,i2,i3,i4,:,i6))
              IF (c_e(v7d%voldatir(i1,i2,i3,i4,i5,i6))) THEN
                CALL csv_record_addfield(csvline, v7d%voldatir(i1,i2,i3,i4,i5,i6))
                no_miss = .TRUE.
              ELSE
                CALL csv_record_addfield(csvline,'')
              ENDIF
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatid)) THEN
            DO i5 = 1, SIZE(v7d%voldatid(i1,i2,i3,i4,:,i6))
              IF (c_e(v7d%voldatid(i1,i2,i3,i4,i5,i6))) THEN
                CALL csv_record_addfield(csvline, v7d%voldatid(i1,i2,i3,i4,i5,i6))
                no_miss = .TRUE.
              ELSE
                CALL csv_record_addfield(csvline,'')
              ENDIF
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatii)) THEN
            DO i5 = 1, SIZE(v7d%voldatii(i1,i2,i3,i4,:,i6))
              CALL addfieldi(v7d%dativar%i(i5), v7d%voldatii(i1,i2,i3,i4,i5,i6), &
               no_miss)
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatib)) THEN
            DO i5 = 1, SIZE(v7d%voldatib(i1,i2,i3,i4,:,i6))
              CALL addfieldb(v7d%dativar%b(i5), v7d%voldatib(i1,i2,i3,i4,i5,i6), &
               no_miss)
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatic)) THEN
            DO i5 = 1, SIZE(v7d%voldatic(i1,i2,i3,i4,:,i6))
              CALL addfieldc(v7d%dativar%c(i5), v7d%voldatic(i1,i2,i3,i4,i5,i6), &
               no_miss)
            ENDDO
          ENDIF
! data attr variables
          IF (ASSOCIATED(v7d%voldatiattrr)) THEN
            DO i7 = 1, SIZE(v7d%datiattr%r)
              DO i5 = 1, SIZE(v7d%dativarattr%r)
                IF (c_e(v7d%voldatiattrr(i1,i2,i3,i4,i5,i6,i7))) THEN
                  CALL csv_record_addfield(csvline, &
                   v7d%voldatiattrr(i1,i2,i3,i4,i5,i6,i7))
                  no_miss = .TRUE.
                ELSE
                  CALL csv_record_addfield(csvline,'')
                ENDIF
              ENDDO
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatiattrd)) THEN
            DO i7 = 1, SIZE(v7d%datiattr%d)
              DO i5 = 1, SIZE(v7d%dativarattr%d)
                IF (c_e(v7d%voldatiattrd(i1,i2,i3,i4,i5,i6,i7))) THEN
                  CALL csv_record_addfield(csvline, &
                   v7d%voldatiattrd(i1,i2,i3,i4,i5,i6,i7))
                  no_miss = .TRUE.
                ELSE
                  CALL csv_record_addfield(csvline,'')
                ENDIF
              ENDDO
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatiattri)) THEN
            DO i7 = 1, SIZE(v7d%datiattr%i)
              DO i5 = 1, SIZE(v7d%dativarattr%i)
                CALL addfieldi(v7d%datiattr%i(i7), &
                 v7d%voldatiattri(i1,i2,i3,i4,i5,i6,i7), no_miss)
              ENDDO
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatiattrb)) THEN
            DO i7 = 1, SIZE(v7d%datiattr%b)
              DO i5 = 1, SIZE(v7d%dativarattr%b)
                CALL addfieldb(v7d%datiattr%b(i7), &
                 v7d%voldatiattrb(i1,i2,i3,i4,i5,i6,i7), no_miss)
              ENDDO
            ENDDO
          ENDIF
          IF (ASSOCIATED(v7d%voldatiattrc)) THEN
            DO i7 = 1, SIZE(v7d%datiattr%c)
              DO i5 = 1, SIZE(v7d%dativarattr%c)
                CALL addfieldc(v7d%datiattr%c(i7), &
                 v7d%voldatiattrc(i1,i2,i3,i4,i5,i6,i7), no_miss)
              ENDDO
            ENDDO
          ENDIF

          IF (.NOT.csv_skip_miss .OR. no_miss) THEN
            WRITE(iun,'(A)')csv_record_getrecord(csvline)
          ENDIF
          CALL delete(csvline)
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

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

END MODULE vol7d_csv


PROGRAM v7d_transform
#include "config.h"
USE log4fortran
USE char_utilities
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
USE grid_transform_class
use volgrid6d_class
USE geo_coord_class
USE vol7d_csv
!USE ISO_FORTRAN_ENV
IMPLICIT NONE

TYPE(op_option) :: options(50) ! remember to update dimension when adding options
TYPE(optionparser) :: opt
CHARACTER(len=8) :: input_format, coord_format, output_format
CHARACTER(len=512) :: input_file, output_file, network_list, variable_list, &
 anavariable_list, attribute_list, coord_file
character(len=80) :: output_template,trans_type,sub_type
TYPE(vol7d_network), ALLOCATABLE :: nl(:)
CHARACTER(len=10), ALLOCATABLE :: vl(:), avl(:), al(:)
CHARACTER(len=23) :: start_date, end_date
CHARACTER(len=19) :: start_date_default, end_date_default
TYPE(datetime) :: now, s_d, e_d
INTEGER :: iun, ier, i, j, n, nc, ninput, yy, mm, dd, iargc
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d) :: v7d, v7d_coord, v7dtmp, v7d_comp1, v7d_comp2, v7d_comp3
TYPE(geo_coordvect),POINTER :: poly(:)
type(transform_def) :: trans
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_dba, v7d_dba_out
#endif
#ifdef HAVE_ORSIM
TYPE(vol7d_oraclesim) :: v7d_osim
#endif
CHARACTER(len=32) :: dsn, user, password
LOGICAL :: version, ldisplay
CHARACTER(len=512):: a_name
INTEGER :: category

! for computing
LOGICAL :: comp_regularize, comp_average, comp_cumulate, comp_discard
CHARACTER(len=23) :: comp_step, comp_start
TYPE(timedelta) :: c_i
TYPE(datetime) :: c_s
REAL :: comp_frac_valid


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
 &e.g. ''B13011,B12001''')
options(8) = op_option_new(' ', 'anavariable-list', anavariable_list, '', help= &
 'if input-format is of database type, list of station variables to be extracted &
 &in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B01192,B01193,B07001''')
options(9) = op_option_new(' ', 'attribute-list', attribute_list, '', help= &
 'if input-format is of database type, list of data attributes to be extracted &
 &in the form of a comma-separated list of B-table alphanumeric codes, &
 &e.g. ''B33196,B33197''')

! option for displaying/processing
options(10) = op_option_new('d', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
options(11) = op_option_new(' ', 'comp-regularize', comp_regularize, help= &
 'regularize the time series keeping only the data at regular time steps')
options(12) = op_option_new(' ', 'comp-average', comp_average, help= &
 'recompute average of averaged fields on a different time step')
options(13) = op_option_new(' ', 'comp-cumulate', comp_cumulate, help= &
 'recompute cumulation of accumulated fields on a different time step')
options(14) = op_option_new(' ', 'comp-step', comp_step, '0000000001 00:00:00.000', help= &
 'length of regularization, average or cumulation step in the format &
 &''YYYYMMDDDD hh:mm:ss.msc'', it can be simplified up to the form ''D hh''')
options(15) = op_option_new(' ', 'comp-start', comp_start, '', help= &
 'start of regularization, average or cumulation interval, an empty value means &
 &take the initial time step of the available data; the format is the same as for &
 &--start-date parameter')
options(16) = op_option_new(' ', 'comp-discard', comp_discard, help= &
 'discard the data that are not the result of the cumulation and/or averaging &
 &processes and keep only the result of the computations')
options(17) = op_option_new(' ', 'comp-frac-valid', comp_frac_valid, 1., help= &
 'specify the fraction of data that has to be valid in order to consider an &
 &accumulated or averaged value acceptable')
! option for interpolation processing
options(18) = op_option_new(' ', 'trans-type', trans_type, ' ', help= &
 'transformation type, ''inter'' for interpolation&
#ifdef HAVE_LIBSHP_FORTRAN
 & or ''polyinter'' for statistical processing within given polygons&
#endif
 &, empty for no transformation')
options(19) = op_option_new(' ', 'sub-type', sub_type, ' ', help= &
 'transformation subtype, for inter: ''near'', ''bilin''&
#ifdef HAVE_LIBSHP_FORTRAN
 &, for ''polyinter'': ''average''&
#endif
 &')

! options for defining output
!options(20) = op_option_new('o', 'output-file', output_file, '-', help= &
! 'output file, ''-'' for stdout')
options(21) = op_option_new(' ', 'output-format', output_format, 'native', help= &
 'format of output file, ''native'' for vol7d native binary format&
#ifdef HAVE_DBALLE
 &, ''BUFR'' for BUFR with generic template, ''CREX'' for CREX format&
#endif
 &, csv for formatted csv output')
#ifdef HAVE_DBALLE
options(22) = op_option_new('t', 'output-template', output_template, 'generic', help= &
 'output TEMPLATE for BUFR/CREX, in the form ''category.subcategory.localcategory'', or &
& an alias like ''synop'', ''metar'',''temp'',''generic''')
#endif

! options for configuring csv output
options(30) = op_option_new(' ', 'csv-volume', csv_volume, 'all', help= &
 'vol7d volumes to be output to csv: ''all'' for all volumes, &
 &''ana'' for station volumes only or ''data'' for data volumes only')
options(31) = op_option_new(' ', 'csv-column', csv_column, 'time,timerange,ana,level,network', help= &
 'list of columns (excluding variables) that have to appear in csv output: &
 &a comma-separated combination of ''time,timerange,level,ana,network'' &
 &in the desired order')
options(32) = op_option_new(' ', 'csv-variable', csv_variable, 'all', help= &
 'list of variables that have to appear in the data columns of csv output: &
 &''all'' or a comma-separated list of B-table alphanumeric codes, e.g. &
 &''B10004,B12001'' in the desired order')
options(33) = op_option_new(' ', 'csv-header', csv_header, 2, help= &
 'write 0 to 2 header lines at the beginning of csv output')
options(34) = op_option_new(' ', 'csv-skip-miss', csv_skip_miss, help= &
 'skip records containing only missing values in csv output')
options(35) = op_option_new(' ', 'csv-norescale', csv_no_rescale, help= &
 'do not rescale in output integer variables according to their scale factor')

! help options
options(49) = op_option_help_new('h', 'help', help= &
 'show an help message and exit')
options(50) = op_option_new(' ', 'version', version, help= &
 'show version and exit')


! define the option parser
opt = optionparser_new(options, description_msg= &
 'Vol7d transformation application, it imports a vol7d volume from a &
 &native vol7d file&
#ifdef HAVE_DBALLE
 &, from a dbAll.e database, from a BUFR/CREX file&
#endif
#ifdef HAVE_ORSIM
 &, from SIM Oracle database&
#endif
 & and exports it into a native v7d file&
#ifdef HAVE_DBALLE
 &, or into a BUFR/CREX file&
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
  CALL l4f_category_log(category,L4F_ERROR,'input file missing')
  CALL optionparser_printhelp(opt)
  CALL EXIT(1)
ELSE IF (i < 1) THEN
  CALL l4f_category_log(category,L4F_ERROR,'output file missing')
  CALL optionparser_printhelp(opt)
  CALL EXIT(1)
ENDIF
CALL getarg(iargc(), output_file)

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
CALL init(s_d, isodate=start_date)
CALL init(e_d, isodate=end_date)
c_i = timedelta_new(isodate=comp_step)
IF (comp_start /= '') THEN
  c_s = datetime_new(isodate=comp_start)
ELSE
  c_s = datetime_miss
ENDIF

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
nc = word_split(csv_column, w_s, w_e, ',')
j = 0
icsv_column(:) = -1
DO i = 1, MIN(nc, SIZE(icsv_column))
  SELECT CASE(csv_column(w_s(i):w_e(i)))
  CASE('time')
    j = j + 1
    icsv_column(j) = vol7d_time_d
  CASE('timerange')
    j = j + 1
    icsv_column(j) = vol7d_timerange_d
  CASE('level')
    j = j + 1
    icsv_column(j) = vol7d_level_d
  CASE('ana')
    j = j + 1
    icsv_column(j) = vol7d_ana_d
  CASE('network')
    j = j + 1
    icsv_column(j) = vol7d_network_d
  CASE default
    CALL l4f_category_log(category,L4F_ERROR,'error in command-line parameters, column '// &
     csv_column(w_s(i):w_e(i))//' in --csv-column not valid.')
    CALL EXIT(1)
  END SELECT
END DO
nc = j
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
    IF (.NOT.ALLOCATED(nl) .OR. .NOT.ALLOCATED(vl)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error in command-line parameters, it is necessary to provide --network-list &
       &and --variable-list with SIM Oracle source.')
      CALL EXIT(1)
    ENDIF
    CALL parse_dba_access_info(input_file, dsn, user, password)
    CALL init(v7d_osim, dsn=dsn, user=user, password=password, time_definition=0)
    IF (.NOT.ALLOCATED(avl)) ALLOCATE(avl(0)) ! allocate if missing
    IF (.NOT.ALLOCATED(al)) ALLOCATE(al(0)) ! allocate if missing
    CALL IMPORT(v7d_osim, vl, nl, timei=s_d, timef=e_d, anavar=avl, attr=al)
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
ENDDO

! displaying/processing
#ifdef HAVE_DBALLE
CALL vol7d_dballe_set_var_du(v7d)
#endif

IF (ldisplay) CALL display(v7d)

IF (trans_type /= '') THEN
  CALL init(trans, trans_type=trans_type, sub_type=sub_type, &
   categoryappend="transformation") !, time_definition=output_td)
  CALL transform(trans, vol7d_in=v7d, vol7d_out=v7d_comp1, v7d=v7d_coord, &
   poly=poly, categoryappend="transform")

  v7d = v7d_comp1
  CALL init(v7d_comp1)

ENDIF

IF (comp_regularize) THEN
  CALL init(v7d_comp1)
  CALL vol7d_regularize_time(v7d, v7d_comp1, c_i, c_s)
  CALL delete(v7d)
  v7d = v7d_comp1
ENDIF

IF (comp_average .OR. comp_cumulate) THEN
  CALL init(v7d_comp1, time_definition=v7d%time_definition)
  CALL init(v7d_comp2, time_definition=v7d%time_definition)
  IF (comp_average) THEN
    CALL vol7d_average(v7d, v7d_comp1, c_i, c_s, full_steps=.TRUE., &
     frac_valid=comp_frac_valid, other=v7d_comp3)
    CALL delete(v7d)
    v7d = v7d_comp3
  ENDIF
  IF (comp_cumulate) THEN
    CALL vol7d_cumulate(v7d, v7d_comp2, c_i, c_s, full_steps=.TRUE., &
     frac_valid=comp_frac_valid, other=v7d_comp3)
    CALL delete(v7d)
    v7d = v7d_comp3
  ENDIF
! merge the tho computed fields
  IF (comp_discard) THEN ! the user is not interested in the other volume
    CALL delete(v7d)
    v7d = v7d_comp1
    CALL vol7d_merge(v7d, v7d_comp2, sort=.TRUE.)
  ELSE
    CALL vol7d_merge(v7d, v7d_comp1, sort=.TRUE.)
    CALL vol7d_merge(v7d, v7d_comp2, sort=.TRUE.)
  ENDIF
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
  CALL init(v7d_dba_out, filename=output_file, FORMAT=output_format, file=.TRUE., &
   WRITE=.TRUE., wipe=.TRUE.)
  v7d_dba_out%vol7d = v7d
  CALL export(v7d_dba_out, template=output_template)
  CALL delete(v7d_dba_out)
#endif

ELSE IF (output_format /= '') THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, format '// &
   TRIM(output_format)//' in --output-format not valid or not supported.')
  CALL EXIT(1)
ENDIF

! cleanly close the databases
IF (input_format == 'native') THEN
  CALL delete(v7d)
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
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, database access info '// &
     TRIM(string)//' not valid.')
    CALL optionparser_printhelp(opt)
    CALL EXIT(1)
  ENDIF
ENDIF

END SUBROUTINE parse_dba_access_info

END PROGRAM v7d_transform

