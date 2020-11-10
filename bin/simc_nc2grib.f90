PROGRAM simc_nc2grib
!-------------------------------------------------------------------------------
!
! Reads the NetCDF output of ROMS or Chimere, wirtes a subset of parameters in
! GRIB2 format
!
! Note that in NetCDF files, the most rapidly changing dimension is last (time
! is the slowest varying dimension), while in fortran it is the first.
! The order of indexes in arrays is:
! - ROMS NetCDF: (time, z, x, y)
! - Chimere NetCDF: (time, z, y, x)
! - values4: (x,y,z,vtimes)
! - values3: (x,y,vtimes)
! - values2: (x,y)
! - volgrid6d%voldati: x, y, z, reftime, timerange, var
!
! Notes
! - The %voldati component of a volgrid6d LibSIM object must be allocated
!   separately: CALL volgrid6d_alloc(...); CALL volgrid6d_alloc_vol(...)
! - If a "volgrid6d" object is a scalar, it can only be exported as unformatted; to
!   export as GRIB, it must be POINTER(:)
! - to enable full logging form LibSIM routines, before launching the program
!   execute the command: export LOG4C_PRIORITY=debug
!
! Todo
! *** gestione campi 3d/2d
! *** gestione dati ROMS; vettori "map"
!
! Possible errors to encode constant 3D fields (Eg. heights of model layers)
! Par. 5.2: check if the reference date for ROMS NetCDF dates is actually equal
! to roms_refdate (1968-05-23 00:00:00)
!
!                                      Version 1.0,0, Enrico & Lidia, 06/12/2019
!-------------------------------------------------------------------------------

USE netcdf
USE eccodes

USE grid_id_class
USE grid_class
USE vol7d_level_class
USE vol7d_timerange_class
USE volgrid6d_class
USE volgrid6d_var_class
USE grid_transform_class

USE, INTRINSIC :: iso_fortran_env, ONLY : iostat_end
USE datetime_class
USE char_utilities
USE optionparser_class
USE err_handling
USE missing_values
USE log4fortran

IMPLICIT NONE

INTEGER, PARAMETER :: dstrl = 19       ! length of date strings in Chimere output
INTEGER, PARAMETER :: maxvars = 200    ! max number of variables in NetCDF input

! Definition of "volgrid6d" object
TYPE(volgrid6d), POINTER :: volgrid_out(:)
TYPE(grid_id) :: gaid_tpl                          ! module grid_id_class
TYPE(griddim_def) :: griddim_out   
TYPE (vol7d_level), POINTER :: vg6_levs(:)         ! module vol7d_level_class
TYPE (vol7d_timerange), POINTER :: vg6_tranges(:)  ! module vol7d_timerange_class
TYPE (volgrid6d_var), POINTER :: vg6_vars(:)       ! module volgrid6d_var_class
TYPE (datetime), POINTER :: vg6_times(:)           ! module datetime_class

! Grid parameters
INTEGER :: nx,ny,component_flag,utm_zone
DOUBLE PRECISION :: xmin,xmax,ymin,ymax,xoff,yoff
DOUBLE PRECISION :: lov,latin1,latin2,lad,dx,dy
DOUBLE PRECISION :: latitude_south_pole,longitude_south_pole,angle_rotation
CHARACTER (LEN=80) :: proj_type
INTEGER :: nx_nc,ny_nc,nz_nc,nt_nc,nrtime,ntrange,nvar,nz

!
TYPE (datetime), ALLOCATABLE :: vtimes(:)
DOUBLE PRECISION, ALLOCATABLE :: vtimes_double(:)
REAL, ALLOCATABLE :: values2(:,:), values3(:,:,:), values4(:,:,:,:)
CHARACTER (LEN=dstrl), ALLOCATABLE :: vtimes_char(:)
TYPE (datetime) :: roms_refdate
TYPE (timedelta) :: tdelta

!
INTEGER :: disc(maxvars),pc(maxvars),pn(maxvars)
CHARACTER (LEN=20) :: ncstring(maxvars),varname_t

! Miscellanea
TYPE (optionparser) :: opt
TYPE (datetime) :: reftime
REAL :: fillvalue,rdum
INTEGER :: gribid_tpl,ncid,dimid_x,dimid_y,dimid_z,dimid_t,dimid_dstrl
INTEGER :: varid_t,dstrl_nc,varid,ndims,xt
INTEGER :: pdtn,togp,centre,sc,sm,gpi,bpv
INTEGER :: nextarg,optstatus,idp,kp,pp,nw,kt,kl,kv
INTEGER :: ios,ios1,ios2,ios3,ier,iret(10),ncstat,ncstat1,ncstat2
INTEGER :: yy,mm,dd,hh,scad
INTEGER, POINTER :: p1(:),p2(:)
CHARACTER (LEN=512) :: ncfile,gribfile,nmlfile,chdum,chdum2,chrt,grib_template
CHARACTER (LEN=512) :: a_name,log_name
CHARACTER (LEN=20) :: version,model,levels,dimname_x,dimname_y,dimname_z,dimname_t
CHARACTER (LEN=2) :: trange_type
LOGICAL :: optversion, opttemplate

!-------------------------------------------------------------------------------
! 0) Constant parameters

CALL eh_setval(verbose=3)                ! set debug level
version = "1.0.0"                        ! version of this program
grib_template = "regular_ll_sfc_grib2"   ! template for grib2
roms_refdate = datetime_new(YEAR=1968, MONTH=5, DAY=23, HOUR=0) ! "ROMS time"
log_name = "simc_nc2grib"                 ! rott name of LibSIM logfiles

! Initialize log4fortran (to enable logging messages from LibSIM routines
CALL l4f_launcher(a_name, a_name_force=log_name)
ier = l4f_init()

!-------------------------------------------------------------------------------
! 1) Parsing command line arguments

! define the option parser; help on positional parametersa
opt = optionparser_new( &
 usage_msg = "Usage: simc_nc2grib [--template] [--help] [--version] {grid-specifications}" // &
 " ncfile gribfile nmlfile [reftime]", &
 description_msg= &
"Reads the NetCDF output of ROMS or Chimere, writes a subset of parameters in GRIB2 format. " // &
"Grid specifications  must be provided, with the same syntax as vg6d_transform. " // &
"""ncfile"" is the input file (NetCDF). ""gribfile"" is the output file (GRIB2). " // &
"""nmlfile"" is the namelist file (use option --template to create a template). " // &
"If ""reftime"" is provided, it is the starting time of the forecast (YYYYMMDDHH), " // &
"otherwise it is assumed that input data are analyis, and time staps are taken from ncfile")

! General options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', optversion, help='show version and exit')
CALL optionparser_add(opt, ' ', 'template', opttemplate, &
  help='write a template for nmlfile (simc_nc2grib.nml) and exit')

! grid specification
CALL optionparser_add(opt, ' ', 'type', proj_type, 'regular_ll', help= &
 "type of projection desired, possible values: " // &
 "regular_ll, rotated_ll, lambert, UTM")
CALL optionparser_add(opt, ' ', 'nx', nx, 31, help= &
 'number of nodes along x axis on target grid')
CALL optionparser_add(opt, ' ', 'ny', ny, 31, help= &
 'number of nodes along y axis on target grid')
CALL optionparser_add(opt, ' ', 'x-min', xmin, 0.0D0, help= &
 'x coordinate of the lower left corner of target grid (degrees or meters)')
CALL optionparser_add(opt, ' ', 'y-min', ymin, 30.0D0, help= &
 'y coordinate of the lower left corner of target grid (degrees or meters)')
CALL optionparser_add(opt, ' ', 'x-max', xmax, 30.0D0, help= &
 'x coordinate of the upper right corner of target grid (degrees or meters)')
CALL optionparser_add(opt, ' ', 'y-max', ymax, 60.0D0, help= &
 'y coordinate of the upper right corner of target grid (degrees or meters)')
CALL optionparser_add(opt, ' ', 'component-flag', component_flag, &
 0, help='wind component flag in target grid (0/1)')

CALL optionparser_add(opt, ' ', 'utm-zone', utm_zone, 32, help= &
 '[UTM] zone number')
CALL optionparser_add(opt, ' ', 'x-off', xoff, 0.0D0, help= &
 '[UTM] x coordinate offset (also known as false easting) in target grid')
CALL optionparser_add(opt, ' ', 'y-off', yoff, 0.0D0, help= &
 '[UTM] y coordinate offset (also known as false northing) in target grid')

CALL optionparser_add(opt, ' ', 'lad', lad, 30.0D0, &
 help='[LAMB] latitudine at which dx and dy (in m) are specified')
CALL optionparser_add(opt, ' ', 'lov', lov, 80.0D0, help= &
 '[LAMB] line of view, also known as reference longitude or orientation of the grid')
CALL optionparser_add(opt, ' ', 'latin1', latin1, 30.0D0, &
 help='[LAMB] first latitude at which the projection plane intesects the sphere')
CALL optionparser_add(opt, ' ', 'latin2', latin2, 60.0D0, &
 help='[LAMB] second latitude at which the projection plane intesects the sphere')
CALL optionparser_add(opt, ' ', 'dx', dx, 3000.0D0, &
 help='[LAMB] X direction grid step')
CALL optionparser_add(opt, ' ', 'dy', dy, 3000.0D0, &
 help='[LAMB] Y direction grid step')

CALL optionparser_add(opt, ' ', 'latitude-south-pole', latitude_south_pole, -90.0D0, &
 help='[ROT] latitude of south pole')
CALL optionparser_add(opt, ' ', 'longitude-south-pole', longitude_south_pole, 0.0D0, &
 help='[ROT] longitude of south pole')
CALL optionparser_add(opt, ' ', 'angle-rotation', angle_rotation, &
 0.0D0, help='[RTO] angle of rotation')

! Parse options; check for errors and for options that require program termination
CALL optionparser_parse(opt, nextarg, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0)
ELSE IF (optstatus == optionparser_err) THEN
  CALL raise_fatal_error(msg="Error parsing command line options", ierval=1)
ELSE IF (optversion) THEN
  WRITE(*,'(2a)')'simc_nc2grib, version ',TRIM(version)
  CALL exit(0)
ELSE IF (opttemplate) THEN
  CALL write_template
  CALL exit(0)
ENDIF

! Positional parameters
idp = 0
reftime = datetime_miss
trange_type = "an"
DO kp = nextarg,HUGE(0)-1
  CALL getarg(kp,chdum)
  IF (TRIM(chdum) == "") THEN
    EXIT
  ELSE
    idp = idp + 1
    SELECT CASE (idp)
    CASE (1)
      ncfile = chdum
    CASE (2)
      gribfile = chdum
    CASE (3)
      nmlfile = chdum
    CASE (4)
      chrt = chdum
      reftime = datetime_new(SIMPLEDATE=chrt)
      trange_type = "fc"
    CASE DEFAULT
      EXIT
    END SELECT
  ENDIF
ENDDO

IF ((trange_type == "fc" .AND. idp /= 4) .OR. (trange_type == "an" .AND. idp /= 3)) THEN
  CALL optionparser_printhelp(opt)
  CALL raise_fatal_error(msg="simc_nc2grib, error in positional parameters", ierval=1)
ENDIF
IF (.NOT. c_e(reftime)) THEN
  CALL raise_fatal_error(msg="simc_nc2grib, """ //TRIM(chrt) // """ is not a valid reftime", ierval=1)
ENDIF

!-------------------------------------------------------------------------------
! 2) Read namelist

! defaults
model = "chimere"
levels = "surface"
centre = 200       ! section 1
sc = 1             ! section 1
sm = 64            ! scanningMode, flag table 3.4
pdtn = 0           ! code table 4.0
togp = 0           ! template 4.0
gpi = 1            ! template 4.0
bpv = 24           ! template 5.0

OPEN (21, FILE=nmlfile, STATUS="OLD", FORM="FORMATTED", IOSTAT=ios)
IF (ios /= 0) CALL raise_fatal_error( &
  msg="simc_nc2grib, file " // TRIM(nmlfile) // " not found", ierval=2)

nvar = 0
DO
  READ (21,'(a)',IOSTAT=ios) chdum2
  chdum = ADJUSTL(chdum2)
  IF (ios == iostat_end) EXIT
  IF (ios /= 0) CALL raise_fatal_error( &
    msg="simc_nc2grib, error reading " // TRIM(nmlfile), ierval=2)
  IF (TRIM(chdum) == "" .OR. chdum(1:1) == "!") CYCLE

  pp = INDEX(chdum,"=")
  IF (pp /= 0) THEN
    SELECT CASE(chdum(1:pp-1))
    CASE ("model")
      READ (chdum(pp+1:),*) model
    CASE ("levels")
      READ (chdum(pp+1:),*) levels

    CASE ("centre")
      READ (chdum(pp+1:),*) centre
    CASE ("subCentre")
      READ (chdum(pp+1:),*) sc
    CASE ("scanningMode")
      READ (chdum(pp+1:),*) sm
    CASE ("productDefinitionTemplateNumber")
      READ (chdum(pp+1:),*) pdtn
    CASE ("typeOfGeneratingProcess")
      READ (chdum(pp+1:),*) togp
    CASE ("generatingProcessIdentifier")
      READ (chdum(pp+1:),*) gpi
    CASE ("bitsPerValue")
      READ (chdum(pp+1:),*) bpv
    CASE DEFAULT
      CALL raise_fatal_error(msg="simc_nc2grib, unknown keyword in " // &
        TRIM(nmlfile) // ": " // chdum(1:pp-1), ierval=2)
    END SELECT

  ELSE
    nw = word_split(chdum, word_start=p1, word_end=p2, sep=",")
    IF (nw /= 4) CALL raise_fatal_error(msg="simc_nc2grib, error parsing line " // &
      TRIM(chdum) // " in namelist " // TRIM(ncfile), ierval=2)
    nvar = nvar + 1
    ncstring(nvar) = chdum(p1(1):p2(1))
    READ (chdum(p1(2):p2(2)),*,IOSTAT=ios1) disc(nvar)
    READ (chdum(p1(3):p2(3)),*,IOSTAT=ios2) pc(nvar)
    READ (chdum(p1(4):p2(4)),*,IOSTAT=ios3) pn(nvar)
    IF (ios1 /= 0 .OR. ios2 /= 0 .OR. ios3 /= 0) CALL raise_fatal_error( &
     msg="simc_nc2grib, error parsing line " // TRIM(chdum) // " in namelist " // &
     TRIM(nmlfile), ierval=2)

  ENDIF
ENDDO
CLOSE(21)

IF (TRIM(model) == "chimere") THEN
  dimname_x = "west_east"
  dimname_y = "south_north"
  dimname_z = "bottom_top"
  dimname_t = "Time"
  varname_t = "Times"
ELSE IF (TRIM(model) == "roms") THEN
  dimname_x = "eta_rho"
  dimname_y = "xi_rho"
  dimname_z = "s_rho"
  dimname_t = "ocean_time"
  varname_t = "ocean_time"
ELSE
  CALL raise_fatal_error(msg="simc_nc2grib, error reading " // TRIM(nmlfile), ierval=2)
ENDIF

!-------------------------------------------------------------------------------
! 3) Open NetCDF files, read dimensions and attributes

ncstat = nf90_open(ncfile, NF90_NOWRITE, ncid)
IF (ncstat /= 0) CALL raise_fatal_error( &
  msg="simc_nc2grib, file " // TRIM(ncfile) // "not found", ierval=3)

ncstat1 = nf90_inq_dimid(ncid, dimname_x, dimid_x)
ncstat2 = nf90_inquire_dimension(ncid, dimid_x, len=nx_nc)
IF (ncstat1 /= 0 .OR. ncstat2 /= 0) CALL raise_fatal_error( &
  msg="simc_nc2grib, file " // TRIM(ncfile) // ": x dim " // TRIM(dimname_x) // " not found", ierval=3)

ncstat1 = nf90_inq_dimid(ncid, dimname_y, dimid_y)
ncstat2 = nf90_inquire_dimension(ncid, dimid_y, len=ny_nc)
IF (ncstat1 /= 0 .OR. ncstat2 /= 0) CALL raise_fatal_error( &
  msg="simc_nc2grib, file " // TRIM(ncfile) // ": y dim " // TRIM(dimname_y) // " not found", ierval=3)

ncstat1 = nf90_inq_dimid(ncid, dimname_z, dimid_z)
ncstat2 = nf90_inquire_dimension(ncid, dimid_z, len=nz_nc)
IF (ncstat1 /= 0 .OR. ncstat2 /= 0) CALL raise_fatal_error( &
  msg="simc_nc2grib, file " // TRIM(ncfile) // ": z dim " // TRIM(dimname_z) // " not found", ierval=3)

ncstat1 = nf90_inq_dimid(ncid, dimname_t, dimid_t)
ncstat2 = nf90_inquire_dimension(ncid, dimid_t, len=nt_nc)
IF (ncstat1 /= 0 .OR. ncstat2 /= 0) CALL raise_fatal_error( &
  msg="simc_nc2grib, file " // TRIM(ncfile) // ": x dim " // TRIM(dimname_t) // " not found", ierval=3)
 
IF (nx_nc /= nx) THEN
  WRITE (chdum,'(3a,2i5)') "simc_nc2grib, specifed X dimension is different from that in ", &
    TRIM(ncfile),": ",nx_nc,nx
  CALL raise_fatal_error(msg=TRIM(chdum), ierval=4)
ENDIF
IF (ny_nc /= ny) THEN
  WRITE (chdum,'(3a,2i5)') "simc_nc2grib, specifed Y dimension is different from that in ", &
    TRIM(ncfile),": ",ny_nc,ny
  CALL raise_fatal_error(msg=TRIM(chdum), ierval=4)
ENDIF

IF (levels == "all") THEN
  nz = nz_nc
ELSE
  nz = 1
ENDIF

IF (trange_type == "an") THEN
  nrtime = nt_nc
  ntrange = 1
ELSE IF (trange_type == "fc") THEN
  nrtime = 1
  ntrange = nt_nc
ENDIF

ALLOCATE (values2(nx,ny))
ALLOCATE (values3(nx,ny,nt_nc))
ALLOCATE (values4(nx,ny,nz,nt_nc))

! Log to stdout the list of input and output data
CALL getval(reftime, isodate=chdum)
dx = (xmax - xmin) / DBLE(nx-1)
dy = (ymax - ymin) / DBLE(ny-1)
WRITE (*,*) "File NetCDF: nx,ny,nz,nt ",nx_nc,ny_nc,nz_nc,nt_nc
WRITE (*,*) "Grid step: ",dx,dy
WRITE (*,*) "Required data: nz ",nz," nvar ",nvar,": ",ncstring(1:nvar)
WRITE (*,*) "GRIB encoding: (pdtn,centre,sc,gpi,bpv)",pdtn,centre,sc,gpi,bpv
WRITE (*,*) "Reftime: ",TRIM(chdum)

!-------------------------------------------------------------------------------
! 4) Create a "volgrid6d" LibSIM object that will contain the entire data volume

ALLOCATE(volgrid_out(1))

! Define the "griddim" LibSIM object corresponding to ouput grid
CALL init(griddim_out, nx=nx, ny=ny, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, &
  component_flag=component_flag, proj_type=proj_type, &
  zone=utm_zone, xoff=xoff, yoff=yoff, &
  lad=lad, lov=lov, latin1=latin1, latin2=latin2, dx=dx, dy=dy, &
  longitude_south_pole=longitude_south_pole, latitude_south_pole=latitude_south_pole, angle_rotation=angle_rotation)

!deb
CALL display(griddim_out) 

! Get ecCodes "id" for grib2 template
CALL codes_grib_new_from_samples(gribid_tpl, grib_template, status=ier)

! Modify the grib template according to namelist options
CALL codes_set (gribid_tpl, "centre", centre, status=iret(1))
CALL codes_set (gribid_tpl, "subCentre", sc, status=iret(2))
CALL codes_set (gribid_tpl, "scanningMode", sm, status=iret(3))
CALL codes_set (gribid_tpl, "productDefinitionTemplateNumber", pdtn, status=iret(4))
CALL codes_set (gribid_tpl, "typeOfGeneratingProcess", togp, status=iret(5))
CALL codes_set (gribid_tpl, "generatingProcessIdentifier", gpi, status=iret(6))
CALL codes_set (gribid_tpl, "bitsPerValue", bpv, status=iret(7))
IF (ANY(iret(1:7) /= CODES_SUCCESS)) &
  WRITE (*,*) "Warning: error setting user-defind key in grib template"

! Define the "grid_id" LibSIM object corresponding to the id of grib2 template
gaid_tpl = grid_id_new(grib_api_id=gribid_tpl)

! Define the "volgrid6d" LibSIM object 
CALL init (volgrid_out(1), griddim=griddim_out, time_definition=0) 
CALL volgrid6d_alloc(volgrid_out(1), nlevel=nz, ntime=nrtime, ntimerange=ntrange, nvar=nvar)
CALL volgrid6d_alloc_vol(volgrid_out(1), decode=.TRUE.)

!-------------------------------------------------------------------------------
! 5) Read verification times from NetCDF data and store them in the "volgrid6d"
!    LibSIM object

ALLOCATE (vtimes(nt_nc))
vtimes(:) = datetime_miss

! 5.1) Chimere input
IF (TRIM(model) == "chimere") THEN

! Make sure that dates strings have the usual length (dimension "DateStrLen")
  ncstat1 = nf90_inq_dimid(ncid, 'DateStrLen', dimid_dstrl)
  ncstat2 = nf90_inquire_dimension(ncid, dimid_dstrl, len=dstrl_nc)
  IF (dstrl_nc /= dstrl .OR. ncstat1 /= NF90_NOERR .OR. ncstat2 /= NF90_NOERR) &
    CALL raise_fatal_error(msg="Error checking the value of ""DateStrLen""", ierval=5)

! Read all timestmaps (NetCDF variable "Times"), and store them in array "vtimes"
  ALLOCATE (vtimes_char(nt_nc))
  ncstat1 = nf90_inq_varid(ncid,varname_t,varid_t)
  ncstat2 = nf90_get_var(ncid, varid_t, vtimes_char(1:nt_nc))
  DO kt = 1, nt_nc
    READ(vtimes_char(kt),'(i4,3(1x,i2))') yy,mm,dd,hh
    vtimes(kt) = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
  ENDDO

! 5.2) ROMS input
ELSE IF (TRIM(model) == "roms") THEN

! Read all timestmaps (NetCDF variable "ocean_time"), and store them in array "vtimes"  
  ALLOCATE(vtimes_double(nt_nc))
  ncstat1 = nf90_inq_varid(ncid, varname_t, varid_t)
  ncstat2 = nf90_get_var(ncid, varid_t, vtimes_double(1:nt_nc))
  DO kt = 1, nt_nc
    tdelta = timedelta_new(sec=INT(vtimes_double(kt)))
    vtimes(kt) = roms_refdate + tdelta
  ENDDO

ENDIF

! 5.3 Check and log
IF (ncstat1 /= NF90_NOERR .OR. ncstat2 /= NF90_NOERR .OR. ANY(vtimes == datetime_miss)) &
  CALL raise_fatal_error(msg="Error reading dates form NetCDF file", ierval=5)
CALL getval(vtimes(1), isodate=chdum)
CALL getval(vtimes(nt_nc), isodate=chdum2)
WRITE (*,*) "Verification times from ",TRIM(chdum)," to ",TRIM(chdum2)

!-------------------------------------------------------------------------------
! 6) Define the coordinate elements of the "volgrid6d" LibSIM object
! The "griddim" element was already defined in section 4

! 6.1) Allocate arrays
ALLOCATE (vg6_levs(nz), vg6_vars(nvar), vg6_tranges(ntrange), vg6_times(nrtime))

! 6.2) Assign times / timeranges
IF (trange_type == "an") THEN
  DO kt = 1, nt_nc
    vg6_times(kt) = vtimes(kt)
  ENDDO
  CALL init(vg6_tranges(1), timerange=254, p1=0, p2=0)

ELSE IF (trange_type == "fc") THEN
  vg6_times(1) = reftime
  DO kt = 1, nt_nc
    CALL getval (vtimes(kt) - reftime, AHOUR=scad)
    CALL init(vg6_tranges(kt), timerange=254, p1=scad*3600, p2=0)
  ENDDO

ENDIF

! 6.3) Assign levels
DO kl = 1, nz
  CALL init(vg6_levs(kl), level1=105, l1=kl)
ENDDO
  
! 6.4) Assign variables
DO kv = 1, nvar
  CALL init(vg6_vars(kv), centre=centre , category=pc(nvar), number=pn(kv), discipline=disc(kv))
ENDDO

! 6.5) Write coordinate elements in "volgrid6d" LibSIM object
volgrid_out(1)%time = vg6_times
volgrid_out(1)%timerange = vg6_tranges
volgrid_out(1)%level = vg6_levs
volgrid_out(1)%var = vg6_vars

!-------------------------------------------------------------------------------
! 7) Read NetCDF data and store them in the "volgrid6d" LibSIM object

DO kv = 1, nvar
  
! 7.1) Get information on the required variable
  ier = nf90_inq_varid(ncid, ncstring(kv), varid)
  IF (ier /= nf90_noerr) THEN
    CALL raise_error(msg="Error getting varid for var " // TRIM(ncstring(kv)), ierval=10)
    CYCLE
  ENDIF
    
  ier = nf90_inquire_variable(ncid, varid, ndims=ndims, xtype=xt)
  IF (ier /= nf90_noerr) THEN
    CALL raise_error(msg="Error getting ndims for var " // TRIM(ncstring(kv)), ierval=10)
    CYCLE
  ELSE IF (ndims < 2 .OR. ndims > 4) THEN
    WRITE (chdum,'(3a,i5)') "Bad number of dimensions for var ", TRIM(ncstring(kv)),": ", ndims
    CALL raise_error(msg=TRIM(chdum), ierval=10)
    CYCLE
  ELSE IF (xt /= NF90_FLOAT .AND. xt /= NF90_DOUBLE) THEN
    CALL raise_error(msg="Invalid tpye for var " // TRIM(ncstring(kv)), ierval=10)
    CYCLE
  ENDIF
    
  ier= nf90_get_att(ncid, varid, '_FillValue', fillvalue)
  IF (ier /= nf90_noerr) THEN
    WRITE (*,*) "Warning: No fillValue for var ", TRIM(ncstring(kv))
    fillvalue = rmiss
  ENDIF

! 7.2) Read data and store them in "volgrid6d" LibSIM object
  WRITE (*,*) "Processing var ",TRIM(ncstring(kv))," ndims ",ndims

! 7.2.1) Single field (eg. latitude)
  IF (ndims == 2) THEN          ! 
    ier = nf90_get_var(ncid, varid, values2)
    IF (ier /= nf90_noerr) THEN
      CALL raise_error(msg="Error getting values for var " // TRIM(ncstring(kv)), ierval=11)
      CYCLE
    ENDIF

    WHERE (values2(:,:) /= fillvalue)
      volgrid_out(1)%voldati(:,:,1,1,1,kv) = values2(:,:)
    ELSEWHERE
      volgrid_out(1)%voldati(:,:,1,1,1,kv) = rmiss
    ENDWHERE
    volgrid_out(1)%gaid(1,1,1,kv) = gaid_tpl
    
! 7.2.1) surface field (eg. 2m temperature)
  ELSE IF (ndims == 3) THEN
    ier = nf90_get_var(ncid, varid, values3)
    IF (ier /= nf90_noerr) THEN
      CALL raise_error(msg="Error getting values for var " // TRIM(ncstring(kv)), ierval=11)
      CYCLE
    ENDIF

    IF (trange_type == "an") THEN
      WHERE (values3(:,:,:) /= fillvalue)
        volgrid_out(1)%voldati(:,:,1,:,1,kv) = values3(:,:,:)
      ELSEWHERE
        volgrid_out(1)%voldati(:,:,1,:,1,kv) = rmiss
      ENDWHERE
      volgrid_out(1)%gaid(1,:,1,kv) = gaid_tpl

    ELSE IF (trange_type == "fc") THEN
      WHERE (values3(:,:,:) /= fillvalue)
        volgrid_out(1)%voldati(:,:,1,1,:,kv) = values3(:,:,:)
      ELSEWHERE
        volgrid_out(1)%voldati(:,:,1,1,:,kv) = rmiss
      ENDWHERE
      volgrid_out(1)%gaid(1,1,:,kv) = gaid_tpl
    ENDIF

!-------------------------------------------------------------------------------
! 7.2.3) 3D field (eg. PM10)
! Use of the "map" optional argument of nf90_get_var: "The elements of the index
! mapping vector correspond, in order, to the netCDF variable's dimensions;
! map(1) gives the distance between elements of the internal array corresponding
! to the most rapidly varying dimension of the netCDF variable"

  ELSE IF (ndims == 4) THEN
print *,nx,ny,nz,nt_nc
print *,ncid,varid
!    ier = nf90_get_var(ncid, varid, rdum)
!    ier = nf90_get_var(ncid, varid, values4(1:nx,1:ny,1:nz,1:nt_nc), map=(/1, nx, nx*ny, nx*ny*nz/))
    ier = nf90_get_var(ncid, varid, values4(1:nx,1:ny,1:nz,1:nt_nc))
    print *,ier
!    IF (ier /= nf90_noerr) THEN
!      CALL raise_error(msg="Error getting values for var " // TRIM(ncstring(kv)), ierval=11)
!      CYCLE
!    ENDIF

    IF (trange_type == "an") THEN
      WHERE (values4(:,:,:,:) /= fillvalue)
        volgrid_out(1)%voldati(:,:,:,:,1,kv) = values4(:,:,:,:)
      ELSEWHERE
        volgrid_out(1)%voldati(:,:,:,:,1,kv) = rmiss
      ENDWHERE
      volgrid_out(1)%gaid(:,:,1,kv) = gaid_tpl

    ELSE IF (trange_type == "fc") THEN

!deb!
! print *,nx,ny,nz,nt_nc
! print *, "vgo%dati"
! print *,SHAPE(volgrid_out(1)%voldati)
! print *, "vgo"
! print *,SHAPE(volgrid_out(:))
!deb!

      WHERE (values4(:,:,:,:) /= fillvalue)
        volgrid_out(1)%voldati(1:nx,1:ny,1:nz,1,1:nt_nc,kv) = values4(1:nx,1:ny,1:nz,1:nt_nc) !deb!
!deb!   volgrid_out(1)%voldati(:,:,:,1,:,kv) = values4(:,:,:,:)
      ELSEWHERE
        volgrid_out(1)%voldati(:,:,:,1,:,kv) = rmiss
      ENDWHERE
      volgrid_out(1)%gaid(:,1,:,kv) = gaid_tpl
    ENDIF

  ENDIF

!deb!
IF (ndims == 4) THEN
  do kt = 1, nt_nc
  do kl = 1, nz
    write (*,*) kt,kl,"cdf: (ave, pct zeri, max, min) ",sum(values4(1:nx,1:ny,kl,kt))/real(nx*ny), &
     100.*real(count(values4(1:nx,1:ny,kl,kt)==0))/real(nx*ny), &
     maxval(values4(1:nx,1:ny,kl,kt)),minval(values4(1:nx,1:ny,kl,kt))
  enddo
  enddo
ELSE IF (ndims == 3) THEN
  do kt = 1, nt_nc
    write (*,*) kt,"cdf: (ave, pct zeri, max, min) ",sum(values3(1:nx,1:ny,kt))/real(nx*ny), &
     100.*real(count(values3(1:nx,1:ny,kt)==0))/real(nx*ny), &
     maxval(values3(1:nx,1:ny,kt)),minval(values3(1:nx,1:ny,kt))
  enddo
ENDIF
!deb!

ENDDO
  
!-------------------------------------------------------------------------------
! 8) Export data to GRIB2 output file

CALL display(volgrid_out(1))
write(*,*) MINVAL(volgrid_out(1)%voldati(:,:,1,1,1,1)),maxval(volgrid_out(1)%voldati(:,:,1,1,1,1))
!deb! CALL export(volgrid_out(1:1), filename=gribfile, gaid_template=gaid_tpl, categoryappend="output_volume")

CALL export(volgrid_out(:), filename=gribfile, gaid_template=gaid_tpl)

! CALL delete ...
ier = nf90_close(ncid)

STOP
END PROGRAM simc_nc2grib

!-------------------------------------------------------------------------------

SUBROUTINE write_template
!
! Write a template for namelist file
!
OPEN (20, FILE="simc_nc2grib.nml", STATUS="REPLACE")
WRITE (20,'(a)') "! Namelist file for simc_nc2grib"
WRITE (20,'(a)') "! Empty lines and lines beginning with """ // CHAR(33) // """ are ignored"
WRITE (20,'(a)')
WRITE (20,'(a)') "! 1) General namelist"
WRITE (20,'(a)') "! - model: ""chimere"" or ""roms"": this flag define the names of the coordinate"
WRITE (20,'(a)') "!   dimensions, and the name and format of timestamps"
WRITE (20,'(a)') "! - levels: ""all"" or ""surface"": this flag define which levels will be included in"
WRITE (20,'(a)') "!   output file"
WRITE (20,'(a)') "! - productDefinitionTemplateNumber: usually set to 0;"
WRITE (20,'(a)') "!   set to 40 to encode air quality parameters following C3S conventions;"
WRITE (20,'(a)') "!   in this case, the 4th field in section 2 of this namelist is the constituentType"
WRITE (20,'(a)') "! - centre, subCentre, generatingProcessIdentifier, bitsPerValue: set the"
WRITE (20,'(a)') "!   corresponding keys in sections 1, 1, 4, 5."
WRITE (20,'(a)') 
WRITE (20,'(a)') "model=""chimere"""
WRITE (20,'(a)') "levels=""surface"""
WRITE (20,'(a)') "centre=200"
WRITE (20,'(a)') "subCentre=1"
WRITE (20,'(a)') "scanningMode=64"
WRITE (20,'(a)') "productDefinitionTemplateNumber=0"
WRITE (20,'(a)') "typeOfGeneratingProcess=0"
WRITE (20,'(a)') "generatingProcessIdentifier=1"
WRITE (20,'(a)') "bitsPerValue=24"                                  
WRITE (20,'(a)') ""
WRITE (20,'(a)') "! 2) Required parameters"
WRITE (20,'(a)') "! one line for each required parameter:"
WRITE (20,'(a)') "! NetCDF-string,discipline,parameterCategory,parameterNumber/constituentType"
WRITE (20,'(a)') "O3,0,200,151"
CLOSE (20)

RETURN

END SUBROUTINE write_template
