MODULE grib_io_class
USE missing_values
USE err_handling
USE char_utilities
USE matchgrib_class
IMPLICIT NONE

INTEGER, PARAMETER :: grio_imiss = imiss
REAL, PARAMETER :: grio_rmiss = rmiss
INTEGER, PARAMETER :: lev_n_dbl(9)=(/100,103,105,107,109,111,113,115,119/)
INTEGER, PARAMETER, PRIVATE :: &
 nb=4, & ! Integer word size, to be improved
 maxsize=16000000, startsize=1000000 ! Initial and max size of raw grib buffer

TYPE grib_io
  INTEGER :: cursize
  INTEGER (kind=nb), POINTER :: rawgrib(:)
  INTEGER :: isec0(2), isec1(1024), isec2(1024), isec3(2), isec4(512)
  REAL :: zsec2(512), zsec3(2)
  REAL, POINTER :: zsec4(:)
  INTEGER :: misec1(1024), misec2(1024), misec4(512)
  LOGICAL :: lmisec1(1024), lmisec2(1024), lmisec4(512)
  LOGICAL :: smartmlev, smartmtr
  TYPE(matchgrib) :: mg, gg
END TYPE grib_io

INTERFACE init
  MODULE PROCEDURE grio_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE grio_delete
END INTERFACE

INTERFACE findgribinfo
  MODULE PROCEDURE grio_findgribinfo
END INTERFACE

INTERFACE findgribdata
  MODULE PROCEDURE grio_findgribdata
END INTERFACE

INTERFACE getgribinfo
  MODULE PROCEDURE grio_getgribinfo
END INTERFACE

INTERFACE getgribdata
  MODULE PROCEDURE grio_getgribdata
END INTERFACE

INTERFACE putgribdata
  MODULE PROCEDURE grio_putgribdata
END INTERFACE

INTERFACE putrawgrib
  MODULE PROCEDURE grio_putrawgrib
END INTERFACE

PRIVATE :: rawgrib_allocate, zsec4_allocate, getgrib

CONTAINS

SUBROUTINE grio_init(this)
TYPE (grib_io) :: this

CALL init(this%mg)
CALL init(this%gg)
NULLIFY(this%zsec4)
CALL rawgrib_allocate(this)
this%lmisec1(:) = .FALSE.
this%lmisec2(:) = .FALSE.
this%lmisec4(:) = .FALSE.
this%smartmlev = .TRUE.
this%smartmtr = .TRUE.

END SUBROUTINE grio_init


SUBROUTINE grio_delete(this)
TYPE (grib_io) :: this

DEALLOCATE(this%rawgrib)
CALL delete(this%mg)
CALL delete(this%gg)

END SUBROUTINE grio_delete


RECURSIVE SUBROUTINE rawgrib_allocate(this, size, ier)
TYPE (grib_io), INTENT(inout) :: this
INTEGER, OPTIONAL, INTENT(in) :: size
INTEGER, OPTIONAL, INTENT(out) :: ier

INTEGER :: ierval

IF (PRESENT(size)) THEN
  this%cursize = size
ELSE
  this%cursize = startsize
ENDIF
ALLOCATE(this%rawgrib(this%cursize), STAT=ierval)
! In case of error while allocating a larger buffer,
! recur to reallocate a basic size buffer and exit
IF (ierval /= 0) THEN
  CALL raise_error('cannot allocate '//to_char(this%cursize)//' words',ierval,ier)
  IF (PRESENT(size)) THEN
    CALL rawgrib_allocate(this)
  ENDIF
ENDIF
IF (PRESENT(ier)) ier = ierval   

END SUBROUTINE rawgrib_allocate


SUBROUTINE zsec4_allocate(this, ier)
TYPE (grib_io), INTENT(inout) :: this
INTEGER, OPTIONAL, INTENT(out) :: ier

INTEGER :: actsize, ierval
REAL, POINTER :: pzsec4(:)
REAL, TARGET :: zzsec4(1)

IF (PRESENT(ier)) ier = 0
pzsec4 => zzsec4 ! Gribex requires a valid section 4 also for 'I/J', satisfy it!
ierval = 1 ! Do not abort in case of error
CALL gribex(this%isec0, this%isec1, this%isec2, this%zsec2, &
 this%isec3, this%zsec3, this%isec4, pzsec4, SIZE(pzsec4), &
 this%rawgrib, this%cursize, actsize, 'J', ierval)
IF (ierval > 0) THEN
  CALL raise_error('from routine gribex',ierval, ier)
  RETURN
ENDIF
ALLOCATE(this%zsec4(MAX(1,this%isec4(1))), STAT=ierval)
IF (ierval /= 0) THEN
  CALL raise_error('cannot allocate '//to_char(this%isec4(1))//' words',ierval,ier)
  RETURN
ENDIF

END SUBROUTINE zsec4_allocate


SUBROUTINE setkey(this, date, time, tr, lev, var, ext, grid)
TYPE (grib_io) :: this
INTEGER, OPTIONAL :: date(3), time(2), tr(5), lev(4), var(3), ext(3), grid(4)

IF (PRESENT(date)) THEN
  IF (date(1) == -1) THEN
    this%lmisec1(12) = .FALSE.
  ELSE! IF (date(1) >= 0) THEN ...
    this%misec1(12) = date(1)
    this%lmisec1(12) = .TRUE.
  ENDIF
  IF (date(2) == -1) THEN
    this%lmisec1(11) = .FALSE.
  ELSE
    this%misec1(11) = date(2)
    this%lmisec1(11) = .TRUE.
  ENDIF
  IF (date(3) == -1) THEN
    this%lmisec1(10) = .FALSE.
    this%lmisec1(21) = .FALSE.
  ELSE
    this%misec1(10) = MOD(date(3), 100)
    this%misec1(21) = date(3)/100 + 1
    this%lmisec1(10) = .TRUE.
    this%lmisec1(21) = .TRUE.
  ENDIF
ENDIF

IF (PRESENT(time)) THEN
  IF (time(1) == -1) THEN
    this%lmisec1(13) = .FALSE.
  ELSE
    this%misec1(13) = time(1)
    this%lmisec1(13) = .TRUE.
  ENDIF
  IF (time(2) == -1) THEN
    this%lmisec1(14) = .FALSE.
  ELSE
    this%misec1(14) = time(2)
    this%lmisec1(14) = .TRUE.
  ENDIF
ENDIF

IF (PRESENT(tr)) THEN
  IF (tr(1) == -1) THEN
    this%lmisec1(15) = .FALSE.
  ELSE
    this%misec1(15) = tr(1)
    this%lmisec1(15) = .TRUE.
  ENDIF
  IF (tr(2) == -1) THEN
    this%lmisec1(16) = .FALSE.
  ELSE
    this%misec1(16) = tr(2)
    this%lmisec1(16) = .TRUE.
  ENDIF
  IF (tr(3) == -1) THEN
    this%lmisec1(17) = .FALSE.
  ELSE
    this%misec1(17) = tr(3)
    this%lmisec1(17) = .TRUE.
  ENDIF
  IF (tr(4) == -1) THEN
    this%lmisec1(18) = .FALSE.
  ELSE
    this%misec1(18) = tr(4)
    this%lmisec1(18) = .TRUE.
  ENDIF
ENDIF

IF (PRESENT(lev)) THEN
  IF (lev(1) == -1) THEN
    this%lmisec1(7) = .FALSE.
  ELSE
    this%misec1(7) = lev(1)
    this%lmisec1(7) = .TRUE.
  ENDIF
  IF (lev(2) == -1) THEN
    this%lmisec1(8) = .FALSE.
  ELSE
    this%misec1(8) = lev(2)
    this%lmisec1(8) = .TRUE.
  ENDIF
  IF (lev(3) == -1) THEN
    this%lmisec1(9) = .FALSE.
  ELSE
    this%misec1(9) = lev(3)
    this%lmisec1(9) = .TRUE.
  ENDIF
ENDIF

IF (PRESENT(var)) THEN
  IF (var(1) == -1) THEN
    this%lmisec1(2) = .FALSE.
  ELSE
    this%misec1(2) = var(1)
    this%lmisec1(2) = .TRUE.
  ENDIF
  IF (var(2) == -1) THEN
    this%lmisec1(1) = .FALSE.
  ELSE
    this%misec1(1) = var(2)
    this%lmisec1(1) = .TRUE.
  ENDIF
  IF (var(3) == -1) THEN
    this%lmisec1(6) = .FALSE.
  ELSE
    this%misec1(6) = var(3)
    this%lmisec1(6) = .TRUE.
  ENDIF
ENDIF

IF (PRESENT(ext)) THEN
  IF (ext(1) == -1) THEN
    this%lmisec1(37) = .FALSE.
  ELSE
    this%misec1(37) = ext(1)
    this%lmisec1(37) = .TRUE.
  ENDIF
  IF (ext(2) == -1) THEN
    this%lmisec1(39) = .FALSE.
  ELSE
    this%misec1(39) = ext(2)
    this%lmisec1(39) = .TRUE.
  ENDIF
  IF (ext(3) == -1) THEN
    this%lmisec1(42) = .FALSE.
  ELSE
    this%misec1(42) = ext(3)
    this%lmisec1(42) = .TRUE.
  ENDIF
ENDIF

IF (PRESENT(grid)) THEN
  IF (grid(1) == -1) THEN
    this%lmisec2(1) = .FALSE.
  ELSE
    this%misec2(1) = grid(1)
    this%lmisec2(1) = .TRUE.
  ENDIF
  IF (grid(2) == -1) THEN
    this%lmisec2(2) = .FALSE.
  ELSE
    this%misec2(2) = grid(2)
    this%lmisec2(2) = .TRUE.
  ENDIF
  IF (grid(3) == -1) THEN
    this%lmisec2(3) = .FALSE.
  ELSE
    this%misec2(3) = grid(3)
    this%lmisec2(3) = .TRUE.
  ENDIF
  IF (grid(4) == -1) THEN
    this%lmisec4(1) = .FALSE.
  ELSE
    this%misec4(1) = grid(4)
    this%lmisec4(1) = .TRUE.
  ENDIF
ENDIF

END SUBROUTINE setkey


SUBROUTINE grib_to_match(this)
TYPE (grib_io) :: this

this%misec1 = this%isec1
this%misec2 = this%isec2
this%misec4 = this%isec4

END SUBROUTINE grib_to_match


LOGICAL FUNCTION matchtime(this) RESULT(match)
TYPE (grib_io) :: this

match = &
 ALL(this%isec1(10:14) == this%misec1(10:14) .OR. .NOT.this%lmisec1(10:14)) &
 .AND. (this%isec1(21) == this%misec1(21) .OR. .NOT.this%lmisec1(21))

END FUNCTION matchtime


LOGICAL FUNCTION matchtr(this) RESULT(match)
TYPE (grib_io) :: this

IF (this%smartmtr .AND. (this%misec1(18) >= 2 .AND. this%misec1(18) <= 5)) THEN
  match = ALL(this%isec1(15:18:3) == this%misec1(15:18:3) .OR. &
   .NOT.this%lmisec1(15:18:3)) .AND. &
   (MAXVAL(this%isec1(16:17)) == this%misec1(16) .OR. &
   .NOT.this%lmisec1(16))
ELSE
  match = ALL(this%isec1(15:18) == this%misec1(15:18) .OR. &
   .NOT.this%lmisec1(15:18))
ENDIF

END FUNCTION matchtr


LOGICAL FUNCTION matchgrid(this) RESULT(match)
TYPE (grib_io) :: this

match = ALL(this%isec2(1:3) == this%misec2(1:3) .OR. &
 .NOT.this%lmisec2(1:3)) .AND. &
 (this%isec4(1) == this%misec4(1) .OR. .NOT.this%lmisec4(1))

END FUNCTION matchgrid


LOGICAL FUNCTION matchvar(this) RESULT(match)
TYPE (grib_io) :: this

match = ALL(this%isec1(1:2) == this%misec1(1:2) .OR. &
 .NOT.this%lmisec1(1:2)) .AND. &
 (this%isec1(6) == this%misec1(6) .OR. .NOT.this%lmisec1(6))

END FUNCTION matchvar


LOGICAL FUNCTION matchlev(this) RESULT(match)
TYPE (grib_io) :: this

IF (this%smartmlev .AND. ANY(this%misec1(7) == lev_n_dbl)) THEN
  match = .NOT.this%lmisec1(7) .OR. &
  (this%isec1(7) == this%misec1(7) .AND. &
  (this%isec1(8) == this%misec1(8) .OR. .NOT.this%lmisec1(8))) .OR. &
  (this%isec1(7) == this%misec1(7)+1 .AND. &
  (this%isec1(8) == this%misec1(9) .OR. .NOT.this%lmisec1(9)))
ELSE
  match = ALL(this%isec1(7:9) == this%misec1(7:9) .OR. &
   .NOT.this%lmisec1(7:9))
ENDIF

END FUNCTION matchlev


FUNCTION grio_findgribinfo(this, unit, ier)
! Find a grib matching this%mg key and store its info in this
TYPE (grib_io), INTENT(inout) :: this
INTEGER, INTENT(in) :: unit
INTEGER, OPTIONAL, INTENT(out) :: ier
LOGICAL :: grio_findgribinfo

INTEGER :: ntry, fstart, fcurr, ierval

grio_findgribinfo = .FALSE.
IF (PRESENT(ier)) ier = 0

IF (unit < 0) THEN
  CALL raise_error('findgrib must be called with a valid unit number', 1, ier)
  RETURN
ENDIF

CALL pbtell(unit, fstart)
IF (fstart < 0) THEN
  CALL raise_error('in routine pbtell', fstart, ier)
  RETURN
ENDIF
ntry = 0
DO WHILE(.TRUE.)
  CALL getgrib(this, unit, 'J', ierval)
  IF (ierval == -1) THEN ! End of file
    IF (ntry == 0) THEN ! first time rewind
      CALL pbseek(unit, 0, 0, ierval)
      IF (ierval /= 0) THEN
        CALL raise_error('in routine pbseek, code', ierval, ier)
        RETURN
      ENDIF
      ntry = 1
    ELSE ! then abort - not found (should not pass here)
      RETURN
    ENDIF
  ELSE IF (ierval /= 0) THEN ! Other error
    IF (PRESENT(ier)) ier = ierval
    RETURN
  ELSE ! ierval == 0
    IF (this%gg == this%mg) THEN
      grio_findgribinfo = .TRUE.
      RETURN ! found
    ENDIF
  ENDIF
  IF (ntry > 0) THEN
    CALL pbtell(unit, fcurr)
    IF (fcurr >= fstart) THEN ! Cycled through all the file
      RETURN ! not found
    ELSE IF (fcurr < 0) THEN
      CALL raise_error('in routine pbtell', fcurr, ier)
      RETURN
    ENDIF
  ENDIF
ENDDO

END FUNCTION grio_findgribinfo


FUNCTION grio_findgribdata(this, unit, ier)
! Find a grib matching this%mg key and store its info and data in this
TYPE (grib_io), INTENT(inout) :: this
INTEGER, INTENT(in) :: unit
INTEGER, OPTIONAL, INTENT(out) :: ier
LOGICAL :: grio_findgribdata

INTEGER :: ierval

grio_findgribdata = grio_findgribinfo(this, unit, ierval)
IF (grio_findgribdata .AND. ierval == 0) CALL getgrib(this, -1, 'D', ierval)
IF (PRESENT(ier)) ier = ierval

END FUNCTION grio_findgribdata


SUBROUTINE grio_getgribinfo(this, unit, ier)
! Read next grib in unit and store its info in this
TYPE (grib_io), INTENT(inout) :: this
INTEGER, INTENT(in) :: unit
INTEGER, OPTIONAL, INTENT(out) :: ier

INTEGER :: ierval

CALL getgrib(this, unit, 'J', ierval)
IF (PRESENT(ier)) ier = ierval

END SUBROUTINE grio_getgribinfo


SUBROUTINE grio_getgribdata(this, unit, ier)
! Read next grib in unit and store its info and data in this
TYPE (grib_io), INTENT(inout) :: this
INTEGER, INTENT(in) :: unit
INTEGER, OPTIONAL, INTENT(out) :: ier

INTEGER :: ierval

CALL getgrib(this, unit, 'D', ierval)
IF (PRESENT(ier)) ier = ierval

END SUBROUTINE grio_getgribdata


SUBROUTINE grio_putgribdata(this, unit, ier)
TYPE (grib_io), INTENT(inout) :: this
INTEGER, INTENT(in) :: unit
INTEGER, OPTIONAL, INTENT(out) :: ier

INTEGER :: actsize, ierval

ierval = 1 ! Do not abort in case of error
CALL gribex(this%isec0, this%isec1, this%isec2, this%zsec2, &
 this%isec3, this%zsec3, this%isec4, this%zsec4, SIZE(this%zsec4), &
 this%rawgrib, this%cursize, actsize, 'C', ierval)
IF (ierval > 0) THEN
  CALL raise_error('from routine gribex', ierval, ier)
  RETURN
ENDIF
CALL pbwrite(unit, this%rawgrib, this%isec0(1), ierval) !actsize*nb
IF (ierval < 0) THEN
  CALL raise_error('from routine pbwrite', ierval, ier)
  RETURN
ENDIF

IF (PRESENT(ier)) ier = 0

END SUBROUTINE grio_putgribdata


SUBROUTINE grio_putrawgrib(this, unit, ier)
TYPE (grib_io), INTENT(inout) :: this
INTEGER, INTENT(in) :: unit
INTEGER, OPTIONAL, INTENT(out) :: ier

INTEGER :: actsize, ierval

CALL getgrib(this, -1, 'L', ierval) ! Just decode grib edition and length
IF (ierval > 0) THEN
  CALL raise_error('from routine gribex', ierval, ier)
  RETURN
ENDIF
CALL pbwrite(unit, this%rawgrib, this%isec0(1), ierval) !actsize*nb
IF (ierval < 0) THEN
  CALL raise_error('from routine pbwrite', ierval, ier)
  RETURN
ENDIF

IF (PRESENT(ier)) ier = 0

END SUBROUTINE grio_putrawgrib


SUBROUTINE getgrib(this, unit, op, ier)
TYPE (grib_io), INTENT(inout) :: this
INTEGER, INTENT(in) :: unit
CHARACTER(LEN=1), INTENT(in) :: op
INTEGER, INTENT(out) :: ier

INTEGER :: actsize, fstart
REAL, POINTER :: pzsec4(:)
REAL, TARGET :: zzsec4(1)

IF (unit >= 0) THEN ! Read the grib, otherwise use the last one read
  ier = 1
  DO WHILE(ier /= 0)
    CALL pbtell(unit, fstart)
    IF (fstart < 0) THEN
      CALL raise_error('in pbtell, code', fstart, ier)
      RETURN
    ENDIF
    CALL pbgrib(unit, this%rawgrib, this%cursize*nb, actsize, ier)
    IF (ier == -3 .AND. this%cursize < maxsize) THEN ! rawgrib too small
      DEALLOCATE(this%rawgrib)
      CALL rawgrib_allocate(this, this%cursize*2, ier) ! Double the size
      IF (ier /= 0) THEN
        CALL raise_error('in allocation', ier, ier)
        RETURN
      ENDIF
      CALL pbseek(unit, fstart, 0, ier)
      IF (ier /= 0) THEN
        CALL raise_error('in pbseek, code', ier, ier)
        RETURN
      ENDIF
    ELSE IF (ier == -1) THEN ! End of file
      RETURN
    ELSE IF (ier /= 0) THEN
      CALL raise_error('from routine pbgrib',ier, ier)
      RETURN
    ENDIF
  ENDDO
ENDIF

IF (.NOT. ASSOCIATED(this%zsec4)) THEN ! Provide gribex correct size of section 4
  IF (op == 'D') THEN
    CALL zsec4_allocate(this, ier)
    IF (ier /= 0) RETURN
    pzsec4 => this%zsec4
  ELSE
    pzsec4 => zzsec4 ! Gribex requires a valid section 4 also for 'I/J', satisfy it!
  ENDIF
ELSE
  pzsec4 => this%zsec4
ENDIF

this%isec3(2) = grio_imiss
this%zsec3(2) = grio_rmiss
ier = 1 ! Do not abort in case of error
CALL gribex(this%isec0, this%isec1, this%isec2, this%zsec2, &
 this%isec3, this%zsec3, this%isec4, pzsec4, SIZE(pzsec4), &
 this%rawgrib, this%cursize, actsize, op, ier)

IF (ier == 710 .AND. op == 'D') THEN ! If failure due to small zsec4, retry
  DEALLOCATE(this%zsec4)
  CALL zsec4_allocate(this, ier)
  IF (ier /= 0) RETURN
  pzsec4 => this%zsec4
  ier = 1 ! Do not abort in case of error
  CALL gribex(this%isec0, this%isec1, this%isec2, this%zsec2, &
   this%isec3, this%zsec3, this%isec4, pzsec4, SIZE(pzsec4), &
   this%rawgrib, this%cursize, actsize, op, ier)
ENDIF
IF (ier > 0) THEN
  CALL raise_error('from routine gribex', ier, ier)
  RETURN
ELSE IF (ier == -3) THEN
  WRITE(*,'(A,I4)')'Warning, from routine gribex, code ',ier
!ELSE IF (ierval < 0) THEN ! The other warnings are not worth
ENDIF

IF (op /= 'L') THEN
  IF (this%isec1(5) < 128) THEN ! Section 2 missing
    CALL raise_error('after routine gribex, found a grib with section 2 missing', &
     8000, ier)
    RETURN
  ENDIF

  CALL mgkey_from_gribex(this%gg, this%isec1, this%isec2, this%isec4)
ENDIF

ier = 0

END SUBROUTINE getgrib

END MODULE grib_io_class

