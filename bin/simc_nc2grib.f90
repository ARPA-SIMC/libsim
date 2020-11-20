module simc_netcdf
 USE datetime_class
 USE err_handling
 implicit none
 integer,parameter:: centre=80
 integer,parameter:: subCentre=255
 CHARACTER (LEN=20) :: dimname_x,dimname_y,dimname_z,dimname_t, varname_t
 !! Chimere
 INTEGER, PARAMETER :: dstrl = 19       ! length of date strings in Chimere output
 !! ROMS
 TYPE (datetime) :: roms_refdate 
 
contains

 SUBROUTINE set_roms_reftime()
   roms_refdate = datetime_new(YEAR=1968, MONTH=5, DAY=23, HOUR=0) 
 END SUBROUTINE
 
 
 SUBROUTINE read_time_nc (ncid, model, it, vtime)
  USE datetime_class
  USE netcdf
  USE err_handling
  implicit none
  integer,intent(in):: ncid, it
  CHARACTER(len=20),intent(in) :: model
  TYPE (datetime),intent(out) :: vtime
  CHARACTER (LEN=dstrl) :: vtimes_char
  integer :: dimid_dstrl, dstrl_nc
  integer :: varid_t,yy,mm,dd,hh, ncstat1, ncstat2
  TYPE (timedelta) :: tdelta
  DOUBLE PRECISION:: vtimes_double
  !
  ! 5.1) Chimere input
  IF (TRIM(model)=="chimere") THEN
    varname_t = "Times"
  ! Make sure that dates strings have the usual length (dimension "DateStrLen")
    ncstat1 = nf90_inq_dimid(ncid, 'DateStrLen', dimid_dstrl)
    ncstat2 = nf90_inquire_dimension(ncid, dimid_dstrl, len=dstrl_nc)
    IF (dstrl_nc /= dstrl .OR. ncstat1 /= NF90_NOERR .OR. ncstat2 /= NF90_NOERR) &
      CALL raise_fatal_error(msg="Error checking the value of ""DateStrLen""", ierval=5)
  ! Read all timestmaps (NetCDF variable "Times"), and store them in array "vtime"
    ncstat1 = nf90_inq_varid(ncid,varname_t,varid_t)
    ncstat2 = nf90_get_var(ncid, varid_t, vtimes_char, start=(/it/))
    IF (ncstat1 /= NF90_NOERR .OR. ncstat2 /= NF90_NOERR) &
      CALL raise_fatal_error(msg="Error reading dates form NetCDF file", ierval=5)
    READ(vtimes_char,'(i4,3(1x,i2))') yy,mm,dd,hh
    vtime = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
  ! 5.2) ROMS input
  ELSE IF (TRIM(model)=="roms") THEN
    call set_roms_reftime()
    varname_t = "ocean_time"
  ! Read all timestmaps (NetCDF variable "ocean_time"), and store them in array "vtime"
    ncstat1 = nf90_inq_varid(ncid, varname_t, varid_t)
    ncstat2 = nf90_get_var(ncid, varid_t, vtimes_double, start=(/it/))
    IF (ncstat1 /= NF90_NOERR .OR. ncstat2 /= NF90_NOERR) &
      CALL raise_fatal_error(msg="Error reading dates form NetCDF file", ierval=5)
    tdelta = timedelta_new(sec=INT(vtimes_double))
    vtime = roms_refdate + tdelta
  ENDIF
 END SUBROUTINE read_time_nc


 SUBROUTINE set_dimension_names(model)
  implicit none
  CHARACTER(len=20),intent(in) :: model
   IF (TRIM(model)=="chimere") THEN
     dimname_x = "west_east"
     dimname_y = "south_north"
     dimname_z = "bottom_top"
     dimname_t = "Time"
     varname_t = "Times"
   ELSE IF (TRIM(model)=="roms") THEN
     dimname_y = "eta_rho"
     dimname_x = "xi_rho"
     dimname_z = "s_rho"
     dimname_t = "ocean_time"
     varname_t = "ocean_time"
   ELSE  
     CALL raise_fatal_error(msg="simc_nc2grib, error setting model dimension names " // TRIM(model), ierval=2)
   END IF 
 END SUBROUTINE
 
end module




PROGRAM simc_nc2grib
!-------------------------------------------------------------------------------
! Reads the NetCDF output of ROMS or Chimere, writes a subset of parameters in
! GRIB2 format
!
! Note that in NetCDF files, the most rapidly changing dimension is last (time
! is the slowest varying dimension), while in fortran it is the first.
! The order of indexes in arrays is:
! - ROMS NetCDF: (time, z, x, y)
! - Chimere NetCDF: (time, z, y, x)
! - values4: (x,y,z,valtime)
! - values3: (x,y,valtime)
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
! Par. 5.2: the reference date for ROMS NetCDF dates is actually equal
! to roms_refdate (1968-05-23 00:00:00)? yes!
!
!                                      Version 1.0,0, Enrico & Lidia, 11/11/2020
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
! USE char_utilities
USE optionparser_class
USE err_handling
USE missing_values
USE log4fortran
USE datetime_class
USE simc_netcdf
IMPLICIT NONE
! integer, parameter :: r8 = selected_real_kind(12,300)  ! 64-bit

! INTEGER, PARAMETER :: dstrl = 19       ! length of date strings in Chimere output
INTEGER, PARAMETER :: maxvars = 200    ! max number of variables in NetCDF input

! Definition of "volgrid6d" object
TYPE(volgrid6d), POINTER :: volgrid_out(:) , volgrid_tpl(:)
TYPE(grid_id) :: gaid_tpl                          ! module grid_id_class
TYPE(griddim_def) :: griddim_out
TYPE (vol7d_level), POINTER :: vg6_levs(:)         ! module vol7d_level_class
TYPE (vol7d_timerange), POINTER :: vg6_tranges(:)  ! module vol7d_timerange_class
TYPE (volgrid6d_var), POINTER :: vg6_vars(:)       ! module volgrid6d_var_class
TYPE(transform_def) :: trans                       ! module grid_transform_class
TYPE (datetime), POINTER :: vg6_times(:)           ! module datetime_class

! Grid parameters
INTEGER :: nx,ny,component_flag,utm_zone,projection_center_flag=0
DOUBLE PRECISION :: xmin,xmax,ymin,ymax,xoff,yoff
DOUBLE PRECISION :: lov,latin1,latin2,lad,dx,dy
DOUBLE PRECISION :: latitude_south_pole,longitude_south_pole,angle_rotation
CHARACTER (LEN=80) :: proj_type
INTEGER :: nx_nc,ny_nc,nz_nc,nt_nc,nrtime,ntrange,nvar,ngvar,nz,dimid,ldim

!
TYPE (datetime) :: valtime
REAL, ALLOCATABLE :: values2(:,:), values3(:,:,:) , values4(:,:,:,:)
! TYPE (datetime) :: roms_refdate
INTEGER, ALLOCATABLE :: dimids(:)
!
INTEGER :: disc(maxvars),pc(maxvars),pn(maxvars),typelevel(maxvars)
CHARACTER (LEN=20) :: ncstring(maxvars)  !!,varname_t
CHARACTER (LEN=52),parameter :: grib_tpl="/usr/share/eccodes/samples/regular_ll_sfc_grib2.tmpl"

! Miscellanea
TYPE (optionparser) :: opt
TYPE (datetime) :: reftime
REAL :: fillvalue
INTEGER :: gribid_tpl,ncid,dimid_x,dimid_y,dimid_z,dimid_t
INTEGER :: varid,ndims,xt
INTEGER :: pdtn,sc,gpi,bpv
INTEGER :: optstatus,pp,nw,kt,kl,kv
INTEGER :: ios,ios1,ios2,ios3,ios4,ier,iret,ncstat,ncstat1,ncstat2 !,irett(10)
INTEGER :: scad
INTEGER, POINTER :: p1(:),p2(:)
INTEGER:: grib_id, infile
CHARACTER (LEN=512) :: ncfile,gribfile,gribfileout,nmlfile,chdum,chdum2,chrt,grib_template, grib_model_tmpl
CHARACTER (LEN=512) :: a_name,log_name
CHARACTER (LEN=20) :: version,model,levels !!,dimname_x,dimname_y,dimname_z,dimname_t
CHARACTER (LEN=2) :: trange_type
CHARACTER (LEN=80) :: ch80
LOGICAL :: optversion, opttemplatenml, unfilesolo

!! disordinati Lidia
INTEGER :: category
INTEGER :: optind
INTEGER :: value_i, igrib_out, it
CHARACTER (LEN=80) :: grbparameters_s
CHARACTER (LEN=23) :: data_s, datetime_s

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
category=l4f_category_get(a_name//".main")

!-------------------------------------------------------------------------------
! 1) Parsing command line arguments

! define the option parser; help on positional parametersa
opt = optionparser_new( &
 usage_msg = "Usage: simc_nc2grib [--templatenml] [--template] [--help] [--version] {grid-specifications}" // &
 " ncfile gribfile nmlfile [reftime]", &
 description_msg= &
"Reads the NetCDF output of ROMS or Chimere, writes a subset of parameters in GRIB2 format. " // &
"Grid specifications  must be provided, with the same syntax as vg6d_transform. " // &
"""ncfile"" is the input file (NetCDF). ""gribfile"" is the output file (GRIB2). " // &
"""nmlfile"" is the namelist file (use option --template to create a template). " // &
"If ""reftime"" is provided, it is the starting time of the forecast (YYYYMMDDHH), " // &
"otherwise it is assumed that input data are analyis, and time steps are taken from ncfile")

! General options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', optversion, help='show version and exit')
CALL optionparser_add(opt, ' ', 'template_nml', opttemplatenml, &
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
CALL optionparser_add(opt, ' ', 'x-off', xoff, 0.0D0, help= &
 '[UTM] x coordinate offset (also known as false easting) in target grid')
CALL optionparser_add(opt, ' ', 'y-off', yoff, 0.0D0, help= &
 '[UTM] y coordinate offset (also known as false northing) in target grid')
!
 CALL optionparser_add(opt, ' ', 'component-flag', component_flag, &
 0, help='wind component flag in target grid (0/1)')
!
utm_zone = imiss
CALL optionparser_add(opt, ' ', 'utm-zone', utm_zone, help= &
 '[UTM] zone number')
lad = dmiss
CALL optionparser_add(opt, ' ', 'lad', lad, &
 help='[LAMB] latitudine at which dx and dy (in m) are specified')
lov = dmiss
CALL optionparser_add(opt, ' ', 'lov', lov, help= &
 '[LAMB] line of view, also known as reference longitude or orientation of the grid')
latin1 = dmiss
CALL optionparser_add(opt, ' ', 'latin1', latin1, &
 help='[LAMB] first latitude at which the projection plane intesects the sphere')
latin2 = dmiss
CALL optionparser_add(opt, ' ', 'latin2', latin2, &
 help='[LAMB] second latitude at which the projection plane intesects the sphere')
! dx=dmiss
! dy=dmiss
! CALL optionparser_add(opt, ' ', 'dx', dx, &
! help='[LAMB] X direction grid step')
! CALL optionparser_add(opt, ' ', 'dy', dy, &
!  help='[LAMB] Y direction grid step')
! latitude_south_pole = dmiss
CALL optionparser_add(opt, ' ', 'latitude-south-pole', latitude_south_pole, -90.0D0, &
 help='[ROT] latitude of south pole')
! longitude_south_pole = dmiss
CALL optionparser_add(opt, ' ', 'longitude-south-pole', longitude_south_pole, 0.0D0, &
 help='[ROT] longitude of south pole')
CALL optionparser_add(opt, ' ', 'angle-rotation', angle_rotation, &
 0.0D0, help='[RTO] angle of rotation')
CALL optionparser_add(opt, ' ', 'component-flag', component_flag, &
 0, help='wind component flag in target grid (0/1)') 
!! read template 
CALL optionparser_add(opt, ' ', 'template', grib_model_tmpl, '', help= &
 "grib template to read")

CALL optionparser_add(opt, ' ', 'component-flag', component_flag, &
 0, help='wind component flag in target grid (0/1)')

! Parse options; check for errors and for options that require program termination
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0)
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line arguments')
  CALL raise_fatal_error(msg="Error parsing command line options", ierval=1)
ELSE IF (optversion) THEN
  WRITE(*,'(2a)')'simc_nc2grib, version ',TRIM(version)
  CALL exit(0)
ELSE IF (opttemplatenml) THEN
  CALL write_template
  CALL exit(0)
ENDIF

!
! Positional parameters
!
reftime = datetime_miss
trange_type = "an"
!
IF (optind + 3 <= iargc()) THEN
  CALL getarg(optind, ncfile)
  IF (ncfile=='-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit')
    ncfile = '/dev/stdin'
  ENDIF

  optind = optind+1
  CALL getarg(optind, gribfile)
  IF (gribfile=='-') THEN
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit')
    gribfile = '/dev/stdout'
  ENDIF

  optind = optind+1
  CALL getarg(optind, nmlfile)

  optind = optind+1
  IF(optind==iargc())THEN
    CALL getarg(optind, chrt)
    reftime = datetime_new(SIMPLEDATE=chrt)
    trange_type = "fc"
  END IF
ELSE
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category, L4F_FATAL, 'input or output file missing')
  CALL raise_fatal_error()
ENDIF

IF(grib_model_tmpl=='')then
    !-------------------------------------------------------------------------------
    ! 2) Create a new template with the output grid and the desired options (name=gribfile)

    ! Define the "griddim" LibSIM object corresponding to ouput grid
    CALL init(griddim_out, nx=nx, ny=ny, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, &
      component_flag=component_flag, proj_type=proj_type, &
      zone=utm_zone, xoff=xoff, yoff=yoff, &
      lad=lad, lov=lov, latin1=latin1, latin2=latin2,  &
      longitude_south_pole=longitude_south_pole, latitude_south_pole=latitude_south_pole, &
      angle_rotation=angle_rotation, &
      projection_center_flag=projection_center_flag)

    !-------------------------------------------------------------------------------
    ! 3) Read namelist and change the template accordingly
    !
    ! open the new template with eccodes and change grib parameters as in nmlfile
    !
    CALL codes_open_file(infile, grib_tpl, 'r', iret)
    IF (iret /= CODES_SUCCESS) &
         WRITE (*,*) "Warning: error opening eccodes template grib file."
    call codes_grib_new_from_file(infile, gribid_tpl)
    !   IF (iret /= CODES_SUCCESS) &
    !      WRITE (*,*) "Warning: error opening eccodes template grib file."
    !
    ! write it in the output file
    !
    CALL codes_open_file(grib_id, 'tmp.tmpl', 'w', iret)
    IF (iret /= CODES_SUCCESS) &
         WRITE (*,*) "Warning: error opening just created grid.tmpl"
    call codes_clone(gribid_tpl, igrib_out)
    OPEN (21, FILE=nmlfile, STATUS="OLD", FORM="FORMATTED", IOSTAT=ios)
    IF (ios /= 0) CALL raise_fatal_error( &
      msg="simc_nc2grib, file " // TRIM(nmlfile) // " not found", ierval=2)
    
    DO
      value_i = imiss
      READ (21,'(a)',IOSTAT=ios) chdum2
      chdum = ADJUSTL(chdum2)
      IF (ios==iostat_end) EXIT
      IF (ios /= 0) CALL raise_fatal_error( &
        msg="simc_nc2grib, error reading " // TRIM(nmlfile), ierval=2)
      IF (TRIM(chdum)=="" .OR. chdum(1:1)=="!") CYCLE
      pp = INDEX(chdum,"=")
      IF (pp /= 0) THEN
          grbparameters_s=chdum(1:pp-1)
          !!! write(*,*) TRIM(grbparameters_s), TRIM(chdum(pp+1:)), TRIM(grbparameters_s)=="model"
          IF(TRIM(grbparameters_s)=="centre"   .or. &
            &  TRIM(grbparameters_s)=="subCentre" .or. &
            &  TRIM(grbparameters_s)=="scanningMode" .or. &
            &  TRIM(grbparameters_s)=="productDefinitionTemplateNumber" .or. &
            &  TRIM(grbparameters_s)=="typeOfGeneratingProcess" .or. &
            &  TRIM(grbparameters_s)=="generatingProcessIdentifier" .or. &
            &  TRIM(grbparameters_s)=="bitsPerValue"  .or. &
            &  TRIM(grbparameters_s)=="indicatorOfUnitOfTimeRange")THEN
             READ (chdum(pp+1:),*) value_i
      ! Modify the grib template according to namelist options
             CALL codes_set (igrib_out, TRIM(grbparameters_s), value_i, status=iret)
             IF (iret /= CODES_SUCCESS) &
               WRITE (*,*) "Warning: error setting "//TRIM(grbparameters_s)//", user-defind key in grib template"
          ELSEIF(TRIM(grbparameters_s)=="model" .or. TRIM(grbparameters_s)=="levels")THEN
              CYCLE
          ELSE 
            CALL raise_fatal_error(msg="simc_nc2grib, unknown keyword in " // &
               TRIM(nmlfile) // ": " // chdum(1:pp-1), ierval=2)
          END IF
      ENDIF
    ENDDO
    CLOSE(21)
    IF(trange_type=="an")THEN
      CALL codes_set (igrib_out, "typeOfGeneratingProcess", 0, status=iret)
    ELSEIF(trange_type=="fc")THEN
      CALL codes_set (igrib_out, "typeOfGeneratingProcess", 2, status=iret)
    END IF
    CALL codes_close_file(infile, iret)
       IF (iret /= CODES_SUCCESS) &
         WRITE (*,*) "Warning: error closing eccodes template file."
    call codes_write(igrib_out, grib_id)
    CALL codes_release(igrib_out, iret)
    CALL codes_close_file(grib_id, iret)
       IF (iret /= CODES_SUCCESS) &
         WRITE (*,*) "Warning: error closing tmp.tmpl"
    
    ! import a template, change the grid and write it
    CALL IMPORT(volgrid_tpl, filename='tmp.tmpl', decode=.TRUE., &
     categoryappend="grib_tmpl")
    CALL init(trans, trans_type='inter', sub_type='near', &
     categoryappend="transformation")
    CALL transform(trans, griddim_out, volgrid6d_in=volgrid_tpl, &
     volgrid6d_out=volgrid_out, &
      clone=.TRUE., categoryappend="transform")
    !! IMPORTANTE!! calcola i parametri tipo Dx e Dy  
    CALL unproj(volgrid_out(1)%griddim)
    !! set griddim_out per volgrid_out
    griddim_out=volgrid_out(1)%griddim
    !!
    CALL export(volgrid_out, filename=trim(gribfile), categoryappend="output_volume")
    CALL delete(volgrid_tpl)
    CALL delete(volgrid_out)
    CALL delete(trans)
    ! updated template generated
    grib_model_tmpl=trim(gribfile)
ELSE
  CALL IMPORT(volgrid_out, filename=grib_model_tmpl, decode=.TRUE., &
      categoryappend="grib_model_tmpl")
  nx=size(volgrid_out(1)%voldati(:,1,1,1,1,1))
  ny=size(volgrid_out(1)%voldati(1,:,1,1,1,1))
  write(*,*) nx,ny
  !! IMPORTANTE!! calcola i parametri tipo Dx e Dy  
  CALL unproj(volgrid_out(1)%griddim)
  !! set griddim_out per volgrid_out
  griddim_out=volgrid_out(1)%griddim
  !!
  CALL delete(volgrid_out)
END IF


!!
!! read variables to write in grib from nmlfile
!! set model and levels
!!
model=""
! defaults
levels = "all"
!!
OPEN (21, FILE=TRIM(nmlfile), STATUS="OLD", FORM="FORMATTED", IOSTAT=ios)
IF (ios /= 0) CALL raise_fatal_error( &
  msg="simc_nc2grib, file " // TRIM(nmlfile) // " not found", ierval=2)
nvar = 0
DO
  READ (21,'(a)',IOSTAT=ios) chdum2
  chdum = ADJUSTL(chdum2)
  IF (ios==iostat_end) EXIT
  IF (ios /= 0) CALL raise_fatal_error( &
    msg="simc_nc2grib, error reading " // TRIM(nmlfile), ierval=2)
  IF (TRIM(chdum)=="" .OR. chdum(1:1)=="!") CYCLE
  pp = INDEX(chdum,"=")
  
  IF (pp /= 0) THEN
      grbparameters_s=chdum(1:pp-1)
      IF(TRIM(grbparameters_s)=="model")THEN
         READ (chdum(pp+1:),*) model
      ELSEIF(TRIM(grbparameters_s)=="levels")THEN
         READ (chdum(pp+1:),*) levels
      endif
  ELSE
      write(*,*) TRIM(chdum)
      nw = word_split(chdum, word_start=p1, word_end=p2, sep=",")
      IF (nw /= 5) CALL raise_fatal_error(msg="simc_nc2grib, error parsing line " // &
        TRIM(chdum) // " in namelist " // TRIM(ncfile), ierval=2)
      nvar = nvar + 1
      ncstring(nvar) = chdum(p1(1):p2(1))
      READ (chdum(p1(2):p2(2)),*,IOSTAT=ios1) disc(nvar)
      READ (chdum(p1(3):p2(3)),*,IOSTAT=ios2) pc(nvar)
      READ (chdum(p1(4):p2(4)),*,IOSTAT=ios3) pn(nvar)
      READ (chdum(p1(5):p2(5)),*,IOSTAT=ios4) typelevel(nvar)
      write(*,*) nvar, nw, ios1,ios2,ios3, ios4
      write(*,*) ncstring(nvar), disc(nvar),pc(nvar),pn(nvar),typelevel(nvar)
      IF (ios1 /= 0 .OR. ios2 /= 0 .OR. ios3 /= 0 .OR. ios4 /= 0) CALL raise_fatal_error( &
       msg="simc_nc2grib, error parsing line " // TRIM(chdum) // " in namelist " // &
       TRIM(nmlfile), ierval=2)
  ENDIF
ENDDO
CLOSE(21)
IF(model=="")  CALL raise_fatal_error(msg="simc_nc2grib, " // &
   "error reading model in namelist " // TRIM(nmlfile), ierval=2)

!!! set the dimension names correctly
call set_dimension_names(model)

!-------------------------------------------------------------------------------
! Open NetCDF files, read dimensions and attributes

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
IF (ncstat1==NF90_NOERR)THEN
  ncstat2 = nf90_inquire_dimension(ncid, dimid_z, len=nz_nc)
  IF (ncstat2 /= 0)  CALL raise_fatal_error( &
  msg="simc_nc2grib, file " // TRIM(ncfile) // ": z dim " // TRIM(dimname_z) // " not found", ierval=3)
ELSE
  levels="msl"
END IF

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

IF (levels=="all") THEN
  nz = nz_nc
ELSE
  nz = 1
ENDIF



WRITE (*,*) "File NetCDF: nx,ny,nz,nt ",nx_nc,ny_nc,nz_nc,nt_nc

!! se fai tutti i tempi contemporaneamente
unfilesolo=.false.
if(unfilesolo)then
    IF (trange_type=="an") THEN
    nrtime = nt_nc
    ntrange = 1
    ELSE IF (trange_type=="fc") THEN
    nrtime = 1
    ntrange = nt_nc
    ENDIF
    ALLOCATE (values4(nx,ny,nz,nt_nc))
    ngvar = nvar
ELSE
    !! divido per tempo
    nrtime = 1
    ntrange = 1
    ngvar = 1
END IF
ALLOCATE (values2(nx,ny))
if(nz>1)then
!   ALLOCATE (values3(nz,nx,ny))
  ALLOCATE (values3(nx,ny,nz))
end if
!
! Log to stdout the list of input and output data
!
IF(model=='chimere')THEN
  dx = (xmax - xmin) / DBLE(nx-1)
  dy = (ymax - ymin) / DBLE(ny-1)
  WRITE (*,*) "Grid step: ",dx,dy
  WRITE (*,*) "GRIB encoding: (pdtn,centre,sc,gpi,bpv)",pdtn,centre,sc,gpi,bpv
END IF
WRITE (*,*) "Required data: nz ",nz," nvar ",nvar,": ",ncstring(1:nvar)
IF(reftime /= datetime_miss)THEN
  data_s=to_char(reftime)
  WRITE (*,*) "Reftime: ",TRIM(data_s)
END IF


!-------------------------------------------------------------------------------
! 6) Create a "volgrid6d" LibSIM object that will contain the entire data volume
ALLOCATE(volgrid_out(1))
! Get ecCodes "id" for grib2 template
CALL codes_open_file(infile, trim(grib_model_tmpl), mode='r', status=ier)
CALL codes_grib_new_from_file(infile, grib_id, status=ier)
! Define the "grid_id" LibSIM object corresponding to the id of grib2 template
gaid_tpl = grid_id_new(grib_api_id=grib_id)
! Define the "volgrid6d" LibSIM object
CALL init (volgrid_out(1), griddim=griddim_out, time_definition=0)
CALL volgrid6d_alloc(volgrid_out(1), nlevel=nz, ntime=nrtime, ntimerange=ntrange, nvar=ngvar)
CALL volgrid6d_alloc_vol(volgrid_out(1), decode=.TRUE.)

write(*,*) nx,ny,nz,nrtime,ntrange,ngvar

!-------------------------------------------------------------------------------
! Define the coordinate elements of the "volgrid6d" LibSIM object
! The "griddim" element was already defined in section 4

! 6.1) Allocate arrays
ALLOCATE (vg6_levs(nz), vg6_vars(1), vg6_tranges(ntrange), vg6_times(nrtime))

!! cycle variables

if(.not.unfilesolo)then
  DO kv = 1, nvar
    ! 6.3) Assign variable
    CALL init(vg6_vars(1), centre=centre, category=pc(kv), number=pn(kv), discipline=disc(kv))
    ! 6.4) Write coordinate elements in "volgrid6d" LibSIM object
    volgrid_out(1)%var = vg6_vars

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
    if (allocated(dimids)) deallocate(dimids)
    allocate(dimids(ndims))
    ier = nf90_inquire_variable(ncid, varid, dimids=dimids, xtype=xt)
! !     do it=1,ndims
! !       ncstat2 = nf90_inquire_dimension(ncid, dimids(it), len=ldim)
! !       write(*,*) ldim
! !     end do
    
   
    ier= nf90_get_att(ncid, varid, '_FillValue', fillvalue)
    IF (ier /= nf90_noerr) THEN
      WRITE (*,*) "Warning: No fillValue for var ", TRIM(ncstring(kv))
      fillvalue = rmiss
    ENDIF
    
    ! 7.2) Read data and store them in "volgrid6d" LibSIM object
    WRITE (*,*) "Processing var ",TRIM(ncstring(kv))," ndims ",ndims
    
    !! cycle time/timeranges
    DO it=1,nt_nc
        !-------------------------------------------------------------------------------
        ! 5) Read verification times from NetCDF data
        call read_time_nc (ncid, model, it, valtime)
        data_s=to_char(valtime)
        if (it==1)then
          WRITE (*,*) "Verification times from ",TRIM(data_s)
        elseif(it==nt_nc)then
          WRITE (*,*) "Verification times to ",TRIM(data_s)
        endif
    
        ! 6.2) Assign times / timeranges
        IF (trange_type=="an") THEN
            vg6_times(1) = valtime
            CALL init(vg6_tranges(1), timerange=254, p1=0, p2=0)
        ELSE IF (trange_type=="fc") THEN
            vg6_times(1) = reftime
            CALL getval (valtime - reftime, AMINUTE=scad)
            CALL init(vg6_tranges(1), timerange=254, p1=scad*60, p2=0)
            IF (scad < 0) THEN
              WRITE (ch80,'(a,i5,a,i4)') "simc_nc2grib: forecast with negative timerange: ",scad," at timestamp ",kt
              CALL raise_fatal_error(msg=ch80, ierval=10)
            ENDIF
        ENDIF
        volgrid_out(1)%time = vg6_times
        volgrid_out(1)%timerange = vg6_tranges
    
        !-------------------------------------------------------------------------------
        ! 7) Read NetCDF data and store them in the "volgrid6d" LibSIM object
        ! 7.2.1) Single field (eg. latitude)
        IF (ndims==2) THEN
            !! volgrd6d dimensions: ix,iy,il,it,itr,iv
            ier = nf90_get_var(ncid, varid, values2)
            IF (ier /= nf90_noerr) THEN
                CALL raise_error(msg="Error getting values for var " // TRIM(ncstring(kv)), ierval=11)
                CYCLE
            ENDIF
            WHERE (values2(:,:) /= fillvalue)
                volgrid_out(1)%voldati(:,:,1,1,1,1) = values2(:,:)
            ELSEWHERE
                volgrid_out(1)%voldati(:,:,1,1,1,1) = rmiss
            ENDWHERE
            volgrid_out(1)%gaid(1,1,1,1) = gaid_tpl
    
        ! 7.2.1) surface field (eg. 2m temperature)
        ELSE IF (ndims==3) THEN
            !! volgrd6d dimensions: ix,iy,il,it,itr,iv
            !! Assign levels
            CALL init(vg6_levs(1), level1=typelevel(kv))
            volgrid_out(1)%level = vg6_levs(1)
            !! variabile
            ier = nf90_get_var(ncid, varid, values2, start=(/1,1,it/), count=(/nx,ny,1/))
            IF (ier /= nf90_noerr) THEN
                CALL raise_error(msg="Error getting values for var " // TRIM(ncstring(kv)), ierval=11)
                CYCLE
            ENDIF
            WHERE (values2 /= fillvalue)
                volgrid_out(1)%voldati(:,:,1,1,1,1) = values2(:,:)
            ELSEWHERE
                volgrid_out(1)%voldati(:,:,1,1,1,1) = rmiss
            ENDWHERE
            volgrid_out(1)%gaid(1,:,1,1) = gaid_tpl
    
        !-------------------------------------------------------------------------------
        ! 7.2.3) 3D field (eg. PM10)
        ! Use of the "map" optional argument of nf90_get_var: "The elements of the index
        ! mapping vector correspond, in order, to the netCDF variable's dimensions;
        ! map(1) gives the distance between elements of the internal array corresponding
        ! to the most rapidly varying dimension of the netCDF variable"
        ELSE IF (ndims==4) THEN
            ! Assign levels
            DO kl = 1, nz
                CALL init(vg6_levs(kl), level1=typelevel(kv), l1=kl)
            END DO
            volgrid_out(1)%level = vg6_levs
            
            ier = nf90_get_var(ncid, varid, values3, start=(/1,1,1,it/), count=(/nx,ny,nz,1/))
            IF (ier /= nf90_noerr) THEN
                CALL raise_error(msg="Error getting values for var " // TRIM(ncstring(kv)), ierval=11)
                CYCLE
            ENDIF
            WHERE (values3 /= fillvalue)
                volgrid_out(1)%voldati(:,:,:,1,1,1) = values3
            ELSEWHERE
                volgrid_out(1)%voldati(:,:,:,1,1,1) = rmiss
            ENDWHERE
            volgrid_out(1)%gaid(:,:,1,1) = gaid_tpl
        ENDIF
    
        !-------------------------------------------------------------------------------
        ! 8) Export data to GRIB2 output file
        datetime_s=data_s(1:4)//data_s(6:7)//data_s(9:10)//data_s(12:13)//data_s(15:16)
        gribfileout = trim(datetime_s)//'_'//trim(ncstring(kv))//'_'//trim(gribfile)
        write(*,*)  MINVAL(volgrid_out(1)%voldati(:,:,1:nz,1:nrtime,1:ntrange,1:ngvar)), &
                  & maxval(volgrid_out(1)%voldati(:,:,1,1,1,1)), trim(gribfileout)
!         CALL display(volgrid_out(1))
        
        CALL export(volgrid_out, filename=trim(gribfileout), categoryappend="output_volume")
    ENDDO
  ENDDO
END IF  

CALL delete(volgrid_out)
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
WRITE (20,'(a)') "centre=80"
WRITE (20,'(a)') "subCentre=255"
WRITE (20,'(a)') "scanningMode=64"
WRITE (20,'(a)') "! productDefinitionTemplateNumber=0"
WRITE (20,'(a)') "! typeOfGeneratingProcess=0"
WRITE (20,'(a)') "! generatingProcessIdentifier=1"
WRITE (20,'(a)') "! bitsPerValue=24"
WRITE (20,'(a)') ""
WRITE (20,'(a)') "! 2) Required parameters"
WRITE (20,'(a)') "! one line for each required parameter:"
WRITE (20,'(a)') "! NetCDF-string,discipline,parameterCategory,parameterNumber/constituentType,typeOfFirstFixedSurface"
WRITE (20,'(a)') "O3,0,200,151,105"
WRITE (20,'(a)') "sea_level,10,3,1,101"
CLOSE (20)

RETURN
END SUBROUTINE write_template
