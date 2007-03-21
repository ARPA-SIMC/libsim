MODULE matchgrib_class

IMPLICIT NONE

TYPE matchgrib
  INTEGER :: date(3), time(2), tr(5), lev(4), var(3), ext(3), grid(4)
END TYPE matchgrib

INTERFACE init
  MODULE PROCEDURE mg_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE mg_delete
END INTERFACE

INTERFACE setval
  MODULE PROCEDURE mg_setval
END INTERFACE

INTERFACE getval
  MODULE PROCEDURE mg_getval
END INTERFACE

INTERFACE OPERATOR (==)
  MODULE PROCEDURE mg_fullmatch
END INTERFACE

CONTAINS

SUBROUTINE mg_init(this)
TYPE (matchgrib) :: this

this%date = -1
this%time = -1
this%tr = -1
this%lev = -1
this%var = -1
this%ext = -1
this%grid = -1

END SUBROUTINE mg_init


SUBROUTINE mg_delete(this)
TYPE (matchgrib) :: this

END SUBROUTINE mg_delete


SUBROUTINE mg_setval(this, date, time, tr, lev, var, ext, grid)
TYPE (matchgrib) :: this
INTEGER, OPTIONAL :: date(3), time(2), tr(5), lev(4), var(3), ext(3), grid(4)

IF (PRESENT(date)) this%date = date
IF (PRESENT(time)) this%time = time
IF (PRESENT(tr))   this%tr   = tr
IF (PRESENT(lev))  this%lev  = lev
IF (PRESENT(var))  this%var  = var
IF (PRESENT(ext))  this%ext  = ext
IF (PRESENT(grid)) this%grid = grid

END SUBROUTINE mg_setval


SUBROUTINE mg_getval(this, date, time, tr, lev, var, ext, grid)
TYPE (matchgrib) :: this
INTEGER, OPTIONAL :: date(3), time(2), tr(5), lev(4), var(3), ext(3), grid(4)

IF (PRESENT(date)) date = this%date
IF (PRESENT(time)) time = this%time
IF (PRESENT(tr))   tr   = this%tr
IF (PRESENT(lev))  lev  = this%lev
IF (PRESENT(var))  var  = this%var
IF (PRESENT(ext))  ext  = this%ext
IF (PRESENT(grid)) grid = this%grid

END SUBROUTINE mg_getval


LOGICAL FUNCTION mg_fullmatch(m1, m2) RESULT(mg_match)
TYPE (matchgrib), INTENT(in) :: m1, m2

CHARACTER(LEN=7) :: why

mg_match = &
 ALL(m1%date == m2%date .OR. m1%date == -1 .OR. m2%date == -1) .AND. &
 ALL(m1%time == m2%time .OR. m1%time == -1 .OR. m2%time == -1) .AND. &
 ALL(m1%tr == m2%tr .OR. m1%tr == -1 .OR. m2%tr == -1) .AND. &
 ALL(m1%lev == m2%lev .OR. m1%lev == -1 .OR. m2%lev == -1) .AND. &
 ALL(m1%var == m2%var .OR. m1%var == -1 .OR. m2%var == -1) .AND. &
 ALL(m1%ext == m2%ext .OR. m1%ext == -1 .OR. m2%ext == -1) .AND. &
 ALL(m1%grid == m2%grid .OR. m1%grid == -1 .OR. m2%grid == -1)

!!$IF (.NOT. mg_match) THEN
!!$  why='FFFFFFF'
!!$  IF (ALL(m1%date == m2%date .OR. m1%date == -1 .OR. m2%date == -1)) &
!!$   why(1:1)='M'
!!$  IF (ALL(m1%time == m2%time .OR. m1%time == -1 .OR. m2%time == -1)) &
!!$   why(2:2)='M'
!!$  IF (ALL(m1%tr == m2%tr .OR. m1%tr == -1 .OR. m2%tr == -1)) &
!!$   why(3:3)='M'
!!$  IF (ALL(m1%lev == m2%lev .OR. m1%lev == -1 .OR. m2%lev == -1)) &
!!$   why(4:4)='M'
!!$  IF (ALL(m1%var == m2%var .OR. m1%var == -1 .OR. m2%var == -1)) &
!!$   why(5:5)='M'
!!$  IF (ALL(m1%ext == m2%ext .OR. m1%ext == -1 .OR. m2%ext == -1)) &
!!$   why(6:6)='M'
!!$  IF (ALL(m1%grid == m2%grid .OR. m1%grid == -1 .OR. m2%grid == -1)) &
!!$   why(7:7)='M'
!!$  PRINT'(A)',why
!!$  PRINT*,m1%grid,m2%grid
!!$ENDIF
END FUNCTION mg_fullmatch


LOGICAL FUNCTION mg_matchconst(m1, m2) RESULT(mg_match)
TYPE (matchgrib) :: m1, m2

mg_match = &
 ALL(m1%lev == m2%lev .OR. m1%lev == -1 .OR. m2%lev == -1) .AND. &
 ALL(m1%var == m2%var .OR. m1%var == -1 .OR. m2%var == -1) .AND. &
 ALL(m1%ext == m2%ext .OR. m1%ext == -1 .OR. m2%ext == -1) .AND. &
 ALL(m1%grid == m2%grid .OR. m1%grid == -1 .OR. m2%grid == -1)

END FUNCTION mg_matchconst


LOGICAL FUNCTION mg_matchvar(m1, m2) RESULT(mg_match)
TYPE (matchgrib) :: m1, m2

mg_match = &
 ALL(m1%var == m2%var .OR. m1%var == -1 .OR. m2%var == -1)

END FUNCTION mg_matchvar


LOGICAL FUNCTION mg_matchtime(m1, m2) RESULT(mg_match)
TYPE (matchgrib) :: m1, m2

mg_match = &
 ALL(m1%tr == m2%tr .OR. m1%tr == -1 .OR. m2%tr == -1) .AND. &
 ALL(m1%date == m2%date .OR. m1%date == -1 .OR. m2%date == -1) .AND. &
 ALL(m1%time == m2%time .OR. m1%time == -1 .OR. m2%time == -1)

END FUNCTION mg_matchtime


LOGICAL FUNCTION mg_matchvertime(m1, m2) RESULT(mg_match)
TYPE (matchgrib) :: m1, m2

mg_match = & ! Check with date and time functions instead!
 ALL(m1%tr == m2%tr .OR. m1%tr == -1 .OR. m2%tr == -1) .AND. &
 ALL(m1%date == m2%date .OR. m1%date == -1 .OR. m2%date == -1) .AND. &
 ALL(m1%time == m2%time .OR. m1%time == -1 .OR. m2%time == -1)

END FUNCTION mg_matchvertime


LOGICAL FUNCTION mg_fullmatchvertime(m1, m2) RESULT(mg_match)
TYPE (matchgrib) :: m1, m2

mg_match = &
 ALL(m1%lev == m2%lev .OR. m1%lev == -1 .OR. m2%lev == -1) .AND. &
 ALL(m1%var == m2%var .OR. m1%var == -1 .OR. m2%var == -1) .AND. &
 ALL(m1%ext == m2%ext .OR. m1%ext == -1 .OR. m2%ext == -1) .AND. &
 ALL(m1%grid == m2%grid .OR. m1%grid == -1 .OR. m2%grid == -1)

IF (.NOT. mg_match) RETURN ! Useless to go on
mg_match = mg_matchvertime(m1, m2)

END FUNCTION mg_fullmatchvertime


LOGICAL FUNCTION mg_matchlev(m1, m2) RESULT(mg_match)
TYPE (matchgrib) :: m1, m2

mg_match = &
 ALL(m1%lev == m2%lev .OR. m1%lev == -1 .OR. m2%lev == -1)

END FUNCTION mg_matchlev


LOGICAL FUNCTION mg_matchgrid(m1, m2) RESULT(mg_match)
TYPE (matchgrib) :: m1, m2

mg_match = &
 ALL(m1%grid == m2%grid .OR. m1%grid == -1 .OR. m2%grid == -1)

END FUNCTION mg_matchgrid


SUBROUTINE mgkey_from_gribex(this, isec1, isec2, isec4)
TYPE (matchgrib) :: this
INTEGER :: &
 isec1(:), &
 isec2(:)

INTEGER, OPTIONAL :: &
 isec4(:)

this%date(1) = isec1(12)
this%date(2) = isec1(11)
this%date(3) = isec1(10)+(isec1(21)-1)*100

this%time(1) = isec1(13)
this%time(2) = isec1(14)

this%tr(1) = isec1(15)
this%tr(2) = isec1(16)
this%tr(3) = isec1(17)
this%tr(4) = isec1(18)
this%tr(5) = MAX(isec1(16), isec1(17))

this%var(1) = isec1(2)
this%var(2) = isec1(1)
this%var(3) = isec1(6)

this%lev(1) = isec1(7)
this%lev(2) = isec1(8)
this%lev(3) = isec1(9)
this%lev(4) = MAX(isec1(8), isec1(9))

IF (SIZE(isec1) >= 42) THEN
  this%ext(1) = isec1(37) ! ECMWF eps extensions
  this%ext(2) = isec1(39)
  this%ext(3) = isec1(42)
ELSE
  this%ext = -1
ENDIF

this%grid(1) = isec2(1) ! Projection
IF (isec2(1) == 0 .OR. isec2(1) == 10 .OR. isec2(1) == 20 .OR. isec2(1) == 30 &
 .OR. isec2(1) == 4 .OR. isec2(1) == 14 .OR. isec2(1) == 24 .OR. isec2(1) == 34) &
 THEN ! Lat/lon or Gaussian
  this%grid(2) = isec2(2)
  this%grid(3) = isec2(3)
ELSE ! Add other projections!!
  IF (PRESENT(isec4)) THEN
    this%grid(2) = isec4(1)
    this%grid(3) = 1
  ELSE
    this%grid(2) = -1
    this%grid(3) = -1
  ENDIF
ENDIF

this%grid(2) = MAX(this%grid(2), 1)
this%grid(3) = MAX(this%grid(3), 1)
IF (PRESENT(isec4)) THEN
  this%grid(4) = isec4(1)
ELSE
  this%grid(4) = -1
ENDIF

END SUBROUTINE mgkey_from_gribex

END MODULE matchgrib_class

