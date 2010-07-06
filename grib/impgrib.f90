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
MODULE impgrib_class

USE grib_grid_class
IMPLICIT NONE

INTEGER, PARAMETER :: cum_sum=1, cum_diff=2, cum_rate=3, cum_diffavg=4

TYPE impgrib
  LOGICAL :: verbose, fatal
  INTEGER :: lastrtim, nvar, nlevmax, ntim
  REAL, POINTER :: cumbuff(:,:,:)
  LOGICAL, POINTER :: found4d(:,:,:)
  CHARACTER (LEN=10), POINTER :: varname(:) ! Variable names 
  INTEGER, POINTER :: gblevv3d(:) ! Level values for upper-air fields
  INTEGER, POINTER :: gblevv(:)   ! Level values for "surface" fields
  TYPE (vartab), POINTER :: vt(:)
  TYPE (timtab), POINTER :: tt(:)
  TYPE (grib_grid) :: rg
END TYPE impgrib

! Local types
TYPE vartab
  INTEGER :: &
   gbpar,     & ! Grib parameter indicator
   gbt2v,     & ! Grib table 2 version
   gblevt,    & ! Level type
   gridtype,  & ! Type of C-grid point (0=H, +-1=U, +-2=V)
   cumulate,  & ! >0 for cumulating, <0 for subtracting in time
   filter       ! How many time a spatial filter has to be applied

  INTEGER, POINTER :: &
   gblevv(:)    ! Level values

  REAL :: &
   convm,     & ! Conversion factor
   convs,     & ! Conversion sum
   ulim,      & ! Upper limit for the field
   llim         ! Lower limit for the field

  LOGICAL :: &
   const,     & ! Which variables are to be kept constant in time
   input,     & ! Which variables are to be read
   output       ! Which variables are to be written
END TYPE vartab

TYPE timtab
  INTEGER :: &
   gbtrd,     & ! Forecast times in days
   gbtrh,     & ! Forecast times in hours
   gbtrm        ! Forecast times in minutes
END TYPE timtab

! Private variables
INTEGER, PARAMETER, PRIVATE :: nvarm=40, nlevm=120, ntimm=120

INTERFACE init
  MODULE PROCEDURE ig_init
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE ig_delete
END INTERFACE

PRIVATE:: ig_reallocate, ig_deallocate, lastne

CONTAINS

SUBROUTINE ig_init(this)
TYPE (impgrib) :: this

CALL init(this%rg)
this%verbose = .FALSE.
this%fatal = .TRUE.
NULLIFY(this%cumbuff, this%found4d, this%gblevv, this%gblevv3d, this%vt, this%tt)
this%lastrtim = 0

END SUBROUTINE ig_init


SUBROUTINE ig_delete(this)
TYPE (impgrib) :: this

IF (ASSOCIATED(this%gblevv3d)) DEALLOCATE(this%gblevv3d)
IF (ASSOCIATED(this%gblevv)) DEALLOCATE(this%gblevv)
IF (ASSOCIATED(this%varname)) DEALLOCATE(this%varname)
IF (ASSOCIATED(this%vt)) DEALLOCATE(this%vt)
IF (ASSOCIATED(this%tt)) DEALLOCATE(this%tt)
IF (ASSOCIATED(this%cumbuff)) DEALLOCATE(this%cumbuff)
IF (ASSOCIATED(this%found4d)) DEALLOCATE(this%found4d)
CALL delete(this%rg)

END SUBROUTINE ig_delete


SUBROUTINE vt_init(this, n)
TYPE (impgrib) :: this
INTEGER :: n

INTEGER :: i

ALLOCATE(this%vt(n))
DO i = 1, n
  NULLIFY(this%vt(i)%gblevv)
ENDDO

END SUBROUTINE vt_init


SUBROUTINE tt_init(this, n)
TYPE (impgrib) :: this
INTEGER :: n

ALLOCATE(this%tt(n))

END SUBROUTINE tt_init


SUBROUTINE readnaml(this, filename, ier)
TYPE (impgrib) :: this
CHARACTER (LEN=*) :: filename
INTEGER :: ier

LOGICAL :: newgrid

CHARACTER (LEN=1024) :: file
INTEGER :: i, j, k

! Namelist variables
INTEGER :: &
 nx=-1, &
 ny=-1, &
 nxny=-1, &
 ntim=-1

LOGICAL :: &
 fcmode=.TRUE.          ! Run in variable forecast time mode

CHARACTER (LEN=10) :: &
 varname(nvarm) = ''    ! Variable names 

INTEGER :: &
 gbpar(nvarm)=-1,     & ! Grib parameter indicator
 gbt2v(nvarm)=-1,     & ! Grib table 2 version
 gblevt(nvarm)=-1,    & ! Level type for "surface" fields
 gblevv(nvarm)=-1,    & ! Level value for "surface" fields
 gblevt3d=-1,         & ! Level type for upper-air fields
 gblevv3d(nlevm)=-1,  & ! Level value for upper-air fields
 gbtrd(ntimm)=-1,     & ! Forecast times in days
 gbtrh(ntimm)=-1,     & ! Forecast times in hours
 gbtrm(ntimm)=-1,     & ! Forecast times in minutes
 gridtype(nvarm)=0,   & ! Type of C-grid point (0=H, 1=U, 2=V)
 cumulate(nvarm)=0,   & ! >0 for cumulating, <0 for subtracting in time
 filter(nvarm)=0        ! How many time a spatial filter has to be applied

REAL :: &
 convm(nvarm)=1.,     & ! Conversion factor
 convs(nvarm)=0.,     & ! Conversion sum
 ulim(nvarm)=-1.,     & ! Upper limit for the field
 llim(nvarm)=1.         ! Lower limit for the field

LOGICAL :: &
 const(nvarm) = .FALSE.,    & ! Which variables are constant in time
 input(nvarm) = .TRUE.,     & ! Which variables are to be read
 output(nvarm) = .TRUE.       ! Which variables are to be written

NAMELIST / impgrib / gbpar, gbt2v, gblevt, gblevv, gblevt3d, gblevv3d, &
 gbtrd, gbtrh, gbtrm, gridtype, cumulate, filter, convm, convs, ulim, llim, &
 const, input, output, fcmode, ntim

ier = 0

OPEN(10,FILE=filename,STATUS='old')
READ(10,NML=impgrib)
CLOSE(10)

!!$! Inizializzazioni statiche
!!$CALL ig_deallocate(this)

! Count var, lev, tim and do sanity checks
this%nvar = lastne(gbpar, -1)
IF (ANY(gblevt(1:this%nvar) == -1)) THEN ! Upper-air fields requested
  this%nlevmax = lastne(gblevv3d, -1)
  IF (this%nlevmax <= 0) THEN
    ier = 1
    WRITE(*,'(A,A)') &
     'Error, list of upper-air levels (gblevv3d) missing in namelist file ', &
     TRIM(filename)
  ENDIF
  IF (gblevt3d == -1) THEN
    ier = 1
    WRITE(*,'(A,A)') &
     'Error, type of upper-air (gblevt3d) levels missing in namelist file ', &
     TRIM(filename)
  ENDIF
  IF (ier /= 0) THEN
    IF (this%fatal) STOP 1
    RETURN
  ENDIF
ELSE ! Only single-level fields requested
  this%nlevmax = 0
ENDIF

this%ntim = MAX(lastne(gbtrd, -1), lastne(gbtrh, -1), lastne(gbtrm, -1))
IF (this%ntim > 0 .AND. ntim /= -1 .AND. this%ntim /= ntim) THEN
! non e` meglio IF (ntim /= -1 .AND. this%ntim /= ntim) THEN ?
  ier = 1
  WRITE(*,'(A,2I4,A,A)') &
   'Error, list of time levels (gbtrh) has a different length w/r to ntim:', &
   this%ntim, ntim,' in ',TRIM(filename)
  IF (this%fatal) STOP 1
  RETURN
ELSE IF (ntim > 0) THEN
  this%ntim = ntim
ENDIF
IF (this%ntim > 0) THEN ! Set to 0 unset time values to correctly compute time
  WHERE (gbtrd(1:this%ntim) == -1)
    gbtrd(1:this%ntim) = 0
  END WHERE
  WHERE (gbtrh(1:this%ntim) == -1)
    gbtrh(1:this%ntim) = 0
  END WHERE
  WHERE (gbtrm(1:this%ntim) == -1)
    gbtrm(1:this%ntim) = 0
  END WHERE
ENDIF
! this%ntim == 0 => nothing specified => autodetect later

IF (this%nvar <= 0) THEN
  ier = 1
  WRITE(*,'(A,A)')'Error, list of variables (gbpar) missing in namelist file ', &
   TRIM(filename)
  IF (this%fatal) STOP 1
  RETURN
ENDIF

IF (this%verbose) WRITE(*,'(A,I4,A,I4,A,I4,A)') &
 'You have requested',this%nvar,' variables at',this%nlevmax, &
 ' upper-air vertical levels and',this%ntim,' time levels.'

CALL vt_init(this, this%nvar)
CALL tt_init(this, MAX(this%ntim,1))
ALLOCATE(this%gblevv(this%nvar))
this%gblevv = gblevv(1:this%nvar)
IF (ANY(varname /= '')) THEN
  ALLOCATE(this%varname(this%nvar))
  this%varname = varname(1:this%nvar)
ENDIF
IF (this%nlevmax > 0) THEN
  ALLOCATE(this%gblevv3d(this%nlevmax))
  this%gblevv3d = gblevv3d(1:this%nlevmax)
ELSE
  this%nlevmax = 1
ENDIF

DO i = 1, this%nvar
  this%vt(i)%gbpar = gbpar(i)
  this%vt(i)%gbt2v = gbt2v(i)
  IF (gblevt(i) > -1) THEN
    this%vt(i)%gblevt = gblevt(i)
  ELSE
    this%vt(i)%gblevt = gblevt3d
  ENDIF
  this%vt(i)%gridtype = gridtype(i)
  this%vt(i)%cumulate = cumulate(i)
  this%vt(i)%filter = filter(i)

  IF (gblevt(i) == -1) THEN ! Upper air variable
    this%vt(i)%gblevv => this%gblevv3d
  ELSE ! Single level variable
    this%vt(i)%gblevv => this%gblevv(i:i)
  ENDIF

  this%vt(i)%convm = convm(i)
  this%vt(i)%convs = convs(i)
  this%vt(i)%ulim = ulim(i)
  this%vt(i)%llim = llim(i)

  this%vt(i)%const = const(i)
  this%vt(i)%input = input(i)
  this%vt(i)%output = output(i)
ENDDO

DO i = 1, this%ntim
  this%tt(i)%gbtrd = gbtrd(i)
  this%tt(i)%gbtrh = gbtrh(i)
  this%tt(i)%gbtrm = gbtrm(i)
ENDDO

IF (nx > 0 .AND. ny > 0) THEN
!!$  ALLOCATE(this%rg%grid%field5d(nx,ny,this%nlevmax,this%nvar,MAX(this%ntim,1)))
!!$  ALLOCATE(this%found4d(this%nlevmax,this%nvar,MAX(this%ntim,1)))
!!$  this%found4d = .FALSE.
  nxny=nx*ny
ENDIF

CALL setkey(this%rg%grib, grid=(/-1, nx, ny, nxny/))
!!$this%rg%grib%mg%grid = (/-1, nx, ny, nxny/)

END SUBROUTINE readnaml


SUBROUTINE readtl(this, filename, ier, const, mode)

TYPE(impgrib), TARGET :: this
CHARACTER(LEN=*), INTENT(IN) :: filename
INTEGER, INTENT(OUT) :: ier
LOGICAL, OPTIONAL :: const
INTEGER, OPTIONAL :: mode

INTEGER :: l, unit, nctim, nctimdata, timdim, mtim, mvar, mlev, nx, ny, lmode
LOGICAL :: checktr, detecttr
TYPE(matchgrib), POINTER :: match

IF (PRESENT(mode)) THEN
  lmode = mode
ELSE
  lmode = 1
ENDIF
IF (lmode == 4) THEN ! for mode == 3 can cumulate only all at once at the end
  DO l = 1, this%ntim
    CALL ig_cumulate(this, 3, l, l)
  ENDDO
  RETURN
ENDIF
! Open grib file
CALL pbopen (unit, filename, 'r', ier)
IF (ier /= 0) THEN
  WRITE(*,'(A,I3,A,A)')'Error in pbopen, code',ier,' while opening ',TRIM(filename)
  IF (this%fatal) STOP 1
  RETURN
ENDIF

!!$match => this%rg%grib%mg
!CALL init(match)
IF (lmode == 3) THEN ! No memory in multi-time-level mode
  this%lastrtim = 0
ENDIF
IF (PRESENT(const)) THEN ! Update time level only if not reading only constant
  IF (const) THEN
    nctim = MAX(this%lastrtim, 1)
  ELSE
    nctim = this%lastrtim + 1
    this%lastrtim = nctim
  ENDIF
  checktr = .NOT. const
ELSE
  nctim = this%lastrtim + 1
  this%lastrtim = nctim
  checktr = .TRUE.
ENDIF
IF (lmode > 1) THEN
  nctimdata = nctim
  timdim = MAX(this%ntim,1)
ELSE
  nctimdata = 1
  timdim = 1
ENDIF

IF (this%ntim > 0) THEN
  IF (nctim > this%ntim) THEN
    WRITE(*,'(A,I3,A,I3)')'Error, time level ',nctim,' out of range 1:',this%ntim
    IF (this%fatal) STOP 1
    CALL pbclose(unit)
    RETURN
  ENDIF
  itr = MAX(-1,this%tt(nctim)%gbtrh+24*this%tt(nctim)%gbtrd)
  CALL setkey(this%rg%grib, tr=(/-1, itr, -1, -1/))
  detecttr = (itr == -1)
ELSE ! Realloc to have a new time range
  CALL setkey(this%rg%grib, tr=(/-1, -1, -1, -1/))
  detecttr = .TRUE.
ENDIF

CALL ig_reallocate(this, nctim, nctimdata)
fileloop: DO WHILE (.TRUE.)
  CALL getgribinfo(this%rg, unit, ier)
  IF (ier == -1) EXIT fileloop ! End of file
  IF (ier /= 0) THEN
    IF (this%fatal) STOP 1
    CALL pbclose(unit)
    RETURN
  ENDIF
  ! Variable matching
  varmatch: DO mvar = 1, this%nvar
    IF (PRESENT(const)) THEN ! Check the "constance"
      IF (this%vt(mvar)%const .NEQV. const) CYCLE varmatch
    ENDIF
    IF (this%vt(mvar)%gbpar == -1) CYCLE varmatch ! Skip dummy variables
    CALL setkey(this%rg%grib, var=(/-1, this%vt(mvar)%gbt2v, this%vt(mvar)%gbpar/))
    IF (mg_matchvar(this%rg%grib%gg, match)) THEN
      ! Level matching
      match%lev(1) = this%vt(mvar)%gblevt
      levmatch: DO mlev = 1, SIZE(this%vt(mvar)%gblevv)
        match%lev(4) = this%vt(mvar)%gblevv(mlev)
        IF (mg_matchlev(this%rg%grib%gg, match)) EXIT varmatch
      ENDDO levmatch
      IF (mlev > SIZE(this%vt(mvar)%gblevv)) CYCLE varmatch ! Level does not match
    ENDIF
  ENDDO varmatch
  IF (mvar > this%nvar) CYCLE fileloop ! Variable does not match
  ! Time range detection and matching
  IF (checktr) THEN
    IF (detecttr) THEN ! Time ranges not fully specified in naml, autodetect
      this%tt(nctim)%gbtrh = MAX(this%rg%grib%isec1(16), this%rg%grib%isec1(17))
      this%tt(nctim)%gbtrd = 0
      this%tt(nctim)%gbtrm = 0
      ??? come fare ???
      IF (ANY(match%date == -1)) match%date = this%rg%grib%gg%date
      IF (ANY(match%time == -1)) match%time = this%rg%grib%gg%time
      detecttr = .FALSE.
    ENDIF
    IF (lmode == 3) THEN ! If multiple time range in one file, find which one matches
      DO mtim = 1, this%ntim
        CALL setkey(this%rg%grib, tr=(/-1, &
         MAX(-1, this%tt(mtim)%gbtrh+24*this%tt(mtim)%gbtrd), -1, -1/))
        IF (mg_matchtime(this%rg%grib%gg, match)) EXIT
      ENDDO
      IF (mtim > this%ntim) CYCLE fileloop ! Time range does not match
    ELSE
      IF (.NOT. mg_matchtime(this%rg%grib%gg, match)) CYCLE fileloop
      mtim = nctimdata
    ENDIF
  ELSE
    mtim = nctimdata
  ENDIF
  ! Grid detection and matching move right after varmatch?
  IF (ALL(match%grid == -1)) THEN
    match%grid = this%rg%grib%gg%grid
  ELSE
    IF (.NOT. mg_matchgrid(this%rg%grib%gg, match)) CYCLE fileloop ! No match
  ENDIF
  ! Field now matches request!
  IF (ASSOCIATED(this%found4d)) THEN ! Avoid duplicate reads
    IF (this%found4d(mlev,mvar,mtim)) CYCLE fileloop
  ENDIF
  IF (.NOT. ASSOCIATED(this%found4d)) THEN ! First time allocate space
    CALL setsize(this%rg, this%nlevmax, this%nvar, timdim)
    ALLOCATE(this%found4d(this%nlevmax,this%nvar,timdim))
    this%found4d = .FALSE.
  ENDIF
  CALL setelement(this%rg, mlev, mvar, mtim)
  CALL getgribdata(this%rg, -1, ier) ! Get the data
  IF (ier /= 0) THEN
    IF (this%fatal) STOP 1
    CALL pbclose(unit)
    RETURN
  ENDIF
  CALL rescale(this%rg, this%vt(mvar)%convm, this%vt(mvar)%convs)
  CALL filter5p(this%rg, this%vt(mvar)%filter)
  this%found4d(mlev,mvar,mtim) = .TRUE.
  ! Copy ahead constant field
  IF (this%ntim > 0 .AND. this%vt(mvar)%const .AND. mode > 1) THEN
    DO l = nctim+1, this%ntim
      this%rg%grid%field5d(:,:,mlev,mvar,l) = &
       this%rg%grid%field5d(:,:,mlev,mvar,mtim)
    ENDDO
    this%found4d(mlev,mvar,mtim+1:) = .TRUE.
  ENDIF
  IF (this%verbose) WRITE(*,'(A,I3,A,I3,A,I3)')'Found variable ',mvar, &
   ' at level ',mlev,' and time level ',mtim

ENDDO fileloop

CALL pbclose(unit)
IF (lmode < 3) THEN
  IF (PRESENT(const)) THEN
    IF (const) CALL ig_cumulate(this, lmode, nctim, nctimdata)
  ELSE
    CALL ig_cumulate(this, lmode, nctim, nctimdata)
  ENDIF
ENDIF

ier = 0

END SUBROUTINE readtl


SUBROUTINE ig_cumulate(this, mode, nctim, nctimdata)
TYPE (impgrib) :: this
INTEGER, INTENT(in) :: mode, nctim, nctimdata

REAL :: tmpbuff(UBOUND(this%rg%grid%field5d,1)-LBOUND(this%rg%grid%field5d,1)+1, &
 UBOUND(this%rg%grid%field5d,2)-LBOUND(this%rg%grid%field5d,2)+1)
REAL :: hd, hd1
INTEGER :: i, k, nlcbuff, nl

IF (ALL(this%vt(:)%cumulate == 0)) RETURN
IF (.NOT. ASSOCIATED(this%cumbuff)) THEN
  nlcbuff = 0
  DO i = 1, SIZE(this%vt(:))
    IF (this%vt(i)%cumulate == cum_diff .OR. this%vt(i)%cumulate == cum_rate .OR. &
     this%vt(i)%cumulate == cum_diffavg .OR. &
     (this%vt(i)%cumulate == cum_sum .AND. mode == 1)) THEN
      nlcbuff = nlcbuff+SIZE(this%vt(i)%gblevv)
    ENDIF
  ENDDO
  ALLOCATE(this%cumbuff( &
   UBOUND(this%rg%grid%field5d,1)-LBOUND(this%rg%grid%field5d,1)+1, &
   UBOUND(this%rg%grid%field5d,2)-LBOUND(this%rg%grid%field5d,2)+1, nlcbuff))
  this%cumbuff = 0.
ENDIF

nl = 0 ! 3rd index in cumbuff
DO i = 1, SIZE(this%vt(:))
  IF (this%vt(i)%cumulate == 0) CYCLE
  DO k = 1, SIZE(this%vt(i)%gblevv)
    SELECT CASE(this%vt(i)%cumulate)
    CASE(0)
      CYCLE
    CASE(cum_sum)
      IF (mode == 1) THEN
        nl = nl + 1
        this%rg%grid%field5d(:,:,k,i,nctimdata) = &
         this%rg%grid%field5d(:,:,k,i,nctimdata) + this%cumbuff(:,:,nl)
        this%cumbuff(:,:,nl) = this%rg%grid%field5d(:,:,k,i,nctimdata)
      ELSE
        IF (nctimdata > 1) THEN
          this%rg%grid%field5d(:,:,k,i,nctimdata) = &
           this%rg%grid%field5d(:,:,k,i,nctimdata) + &
          this%rg%grid%field5d(:,:,k,i,nctimdata-1)
        ENDIF
      ENDIF
    CASE(cum_diff)
      nl = nl + 1
      PRINT*, nctim, nctimdata, MAXVAL(this%cumbuff(:,:,nl)), MAXVAL(this%rg%grid%field5d(:,:,k,i,nctimdata))
      tmpbuff(:,:) = this%rg%grid%field5d(:,:,k,i,nctimdata)
      this%rg%grid%field5d(:,:,k,i,nctimdata) = &
       this%rg%grid%field5d(:,:,k,i,nctimdata) - this%cumbuff(:,:,nl)
      this%cumbuff(:,:,nl) = tmpbuff(:,:)
      PRINT*, MAXVAL(this%rg%grid%field5d(:,:,k,i,nctimdata))
    CASE(cum_rate)
      nl = nl + 1
      tmpbuff(:,:) = this%rg%grid%field5d(:,:,k,i,nctimdata)
      this%rg%grid%field5d(:,:,k,i,nctimdata) = &
       this%rg%grid%field5d(:,:,k,i,nctimdata) - this%cumbuff(:,:,nl)
      this%cumbuff(:,:,nl) = tmpbuff(:,:)
      IF (nctim > 1) THEN
        hd = this%tt(nctim)%gbtrh+24*this%tt(nctim)%gbtrd - &
         this%tt(nctim-1)%gbtrh+24*this%tt(nctim-1)%gbtrd
        IF (hd > 0.) THEN
          this%rg%grid%field5d(:,:,k,i,nctimdata) = &
           this%rg%grid%field5d(:,:,k,i,nctimdata)/hd
        ENDIF
      ENDIF
    CASE(cum_diffavg)
      nl = nl + 1
      hd = this%tt(nctim)%gbtrh+24*this%tt(nctim)%gbtrd
      tmpbuff(:,:) = this%rg%grid%field5d(:,:,k,i,nctimdata)*hd
      IF (nctim > 1) THEN
        hd1 = this%tt(nctim-1)%gbtrh+24*this%tt(nctim-1)%gbtrd
      ELSE
        hd1 = 0.
      ENDIF
      this%rg%grid%field5d(:,:,k,i,nctimdata) = tmpbuff(:,:) - this%cumbuff(:,:,nl)
      this%cumbuff(:,:,nl) = tmpbuff(:,:)
      hd = hd - hd1
      IF (hd > 0.) THEN
        this%rg%grid%field5d(:,:,k,i,nctimdata) = &
         this%rg%grid%field5d(:,:,k,i,nctimdata)/hd
      ENDIF
    END SELECT
  ENDDO
ENDDO

END SUBROUTINE ig_cumulate


SUBROUTINE ig_deallocate(this)
TYPE (impgrib) :: this

IF (ASSOCIATED(this%gblevv3d)) DEALLOCATE(this%gblevv3d)
IF (ASSOCIATED(this%gblevv)) DEALLOCATE(this%gblevv)
IF (ASSOCIATED(this%varname)) DEALLOCATE(this%varname)
IF (ASSOCIATED(this%vt)) DEALLOCATE(this%vt)
IF (ASSOCIATED(this%tt)) DEALLOCATE(this%tt)

IF (ASSOCIATED(this%cumbuff)) DEALLOCATE(this%cumbuff)
IF (ASSOCIATED(this%found4d)) DEALLOCATE(this%found4d)

END SUBROUTINE ig_deallocate


SUBROUTINE ig_reallocate(this, nctim, nctimdata)
TYPE (impgrib) :: this
INTEGER :: nctim, nctimdata
INTEGER :: i, k, ntm1
INTEGER :: sh1(1), sh2(5), sh3(3)
TYPE (impgrib) :: tmpig

IF (.NOT. ASSOCIATED(this%tt) .OR. .NOT. ASSOCIATED(this%rg%grid%field5d) &
 .OR. .NOT. ASSOCIATED(this%found4d)) THEN
  RETURN ! Not allocated yet
ENDIF
sh1 = SHAPE(this%tt)
sh2 = SHAPE(this%rg%grid%field5d)
sh3 = SHAPE(this%found4d)
IF (((nctim == nctimdata) .AND. (sh1(1) /= sh2(5) .OR. sh1(1) /= sh3(3))) &
 .OR. ((nctim > nctimdata) .AND. (sh2(5) /= 1 .OR. sh3(3) /= 1)) &
 .OR. ((nctim > nctimdata) .AND. nctimdata /= 1) &
  .OR. nctim < nctimdata) THEN ! Sanity check
  WRITE(*,'(A,5I3)')'Internal error, allocated time dimensions do not match: ', &
   sh1(1), sh2(5), sh3(3), nctim, nctimdata
  STOP 1
ENDIF
ntm1 = sh1(1)
IF (nctim > nctimdata) THEN ! Overwriting time levels
  DO i = 1, SIZE(this%vt(:)) ! Keep only existing constant fields
    IF (.NOT. this%vt(i)%const) THEN
      DO k = 1, SIZE(this%vt(i)%gblevv)
        IF (this%found4d(k,i,1)) THEN
          this%rg%grid%field5d(:,:,k,i,1) = gg_rmd
          this%found4d(k,i,1) = .FALSE.
        ENDIF
      ENDDO
    ENDIF
  ENDDO
ENDIF

IF (ntm1 >= nctim) RETURN ! Nothing to allocate
IF (ntm1+1 < nctim) THEN
  WRITE(*,'(A,4I3)')'Internal error, can reallocate only one time level at a time, not ', &
   nctim - ntm1
  STOP 1
ENDIF

IF (nctim == nctimdata) THEN ! Reading all time levels separately
!!$  ALLOCATE(tmpig%rg%grid%field5d(sh2(1),sh2(2),sh2(3),sh2(4),nctim))
  CALL setsize(this%rg, ntim=nctim)
  ALLOCATE(tmpig%found4d(sh3(1),sh3(2),nctim))
!!$  tmpig%rg%grid%field5d(:,:,:,:,1:ntm1) = this%rg%grid%field5d(:,:,:,:,1:ntm1)
  tmpig%found4d(:,:,1:ntm1) = this%found4d(:,:,1:ntm1)
!!$  tmpig%rg%grid%field5d(:,:,:,:,nctim) = gg_rmd
  tmpig%found4d(:,:,nctim) = .FALSE.
  DO i = 1, SIZE(this%vt(:)) ! Copy existing constant fields
    IF (this%vt(i)%const) THEN
      DO k = 1, SIZE(this%vt(i)%gblevv)
        IF (this%found4d(k,i,ntm1)) THEN
          this%rg%grid%field5d(:,:,k,i,nctim) = this%rg%grid%field5d(:,:,k,i,ntm1)
          tmpig%found4d(k,i,nctim) = .TRUE.
        ENDIF
      ENDDO
    ENDIF
  ENDDO
!!$  DEALLOCATE(this%rg%grid%field5d, this%found4d)
!!$  this%rg%grid%field5d => tmpig%rg%grid%field5d
  DEALLOCATE(this%found4d)
  this%found4d => tmpig%found4d
ENDIF

ALLOCATE(tmpig%tt(nctim))
tmpig%tt(1:ntm1) = this%tt(1:ntm1) ! Copy old values
tmpig%tt(nctim) = timtab(0, 0, 0) ! Set new values
DEALLOCATE(this%tt)
this%tt => tmpig%tt

END SUBROUTINE ig_reallocate


FUNCTION lastne(vect, val)
INTEGER :: lastne, vect(:), val

INTEGER :: i

DO i = SIZE(vect), 1, -1
  IF (vect(i) /= val) THEN
    lastne = i
    RETURN
  ENDIF
ENDDO
lastne = 0
END FUNCTION lastne

END MODULE impgrib_class
