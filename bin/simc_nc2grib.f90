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

 !=========================
 ! CHECK errori call netcdf (da Virginia)
 !=========================
 subroutine checknc(status,category,message)
  use grid_id_class
  USE log4fortran
  USE netcdf
  USE err_handling
  implicit none
  integer, intent(in) :: status,category
  character(len=*),optional :: message    
  
  if (status /= nf90_noerr) then
    call l4f_category_log(category,L4F_ERROR,&
     nf90_strerror(status)//optio_c(message,40))
    call raise_error()
  end if
 end subroutine checknc

 SUBROUTINE read_time_nc (ncid, model, it, vtime, category)
  USE datetime_class
  USE netcdf
  USE err_handling
  implicit none
  integer,intent(in):: ncid, it, category
  CHARACTER(len=20),intent(in) :: model
  TYPE (datetime),intent(out) :: vtime
  CHARACTER (LEN=dstrl) :: vtimes_char
  integer :: dimid_dstrl, dstrl_nc
  integer :: varid_t,yy,mm,dd,hh
  TYPE (timedelta) :: tdelta
  DOUBLE PRECISION:: vtimes_double
  CHARACTER (LEN=512) :: msg
  !
  ! Chimere input
  IF (TRIM(model)=="chimere") THEN
    varname_t = "Times"
  ! Make sure that dates strings have the usual length (dimension "DateStrLen")
    msg="Error checking the value of ""DateStrLen"""
    call checknc( nf90_inq_dimid(ncid, 'DateStrLen', dimid_dstrl), category, trim(msg))
    msg="Error reading the value of ""DateStrLen"""
    call checknc( nf90_inquire_dimension(ncid, dimid_dstrl, len=dstrl_nc), category, trim(msg))
  ! Read all timestmaps (NetCDF variable "Times"), and store them in array "vtime"
    msg="Error reading dates form NetCDF file: "//trim(varname_t)
    call checknc( nf90_inq_varid(ncid,varname_t,varid_t), category, trim(msg))
    call checknc( nf90_get_var(ncid, varid_t, vtimes_char, start=(/it/)), category, trim(msg))
    READ(vtimes_char,'(i4,3(1x,i2))') yy,mm,dd,hh
    vtime = datetime_new(YEAR=yy, MONTH=mm, DAY=dd, HOUR=hh)
  ! ROMS input
  ELSE IF (TRIM(model)=="roms") THEN
    call set_roms_reftime()
    varname_t = "ocean_time"
  ! Read all timestmaps (NetCDF variable "ocean_time"), and store them in array "vtime"
    msg="Error reading dates form NetCDF file: "//trim(varname_t)
    call checknc( nf90_inq_varid(ncid, varname_t, varid_t), category, trim(msg))
    call checknc( nf90_get_var(ncid, varid_t, vtimes_double, start=(/it/)), category, trim(msg))
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

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!  ROMS
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 SUBROUTINE set_roms_reftime()
   roms_refdate = datetime_new(YEAR=1968, MONTH=5, DAY=23, HOUR=0)
 END SUBROUTINE

!!
!!
!! per il calcolo delle coordinate verticali di ROMS
!!
!!
 SUBROUTINE get_nlevels_roms(romsgridnc, N, category)
 use netcdf
  implicit none
  CHARACTER (LEN=512), intent(in) :: romsgridnc
  CHARACTER (LEN=512) :: msg
  integer, intent(in) :: category
  integer, intent(out)  :: N
  integer::nc_id, dimid
! Open NetCDF files, read dimensions and attributes
  msg="Error reading file "//trim(romsgridnc)//"."
  call checknc( nf90_open(trim(romsgridnc), NF90_NOWRITE, nc_id), category, trim(msg))
  call checknc( nf90_inq_dimid(nc_id, 's_rho', dimid), category, trim(msg))
  call checknc( nf90_inquire_dimension(nc_id, dimid, len=N), category, trim(msg))
  call checknc( nf90_close(nc_id), category)
 END SUBROUTINE get_nlevels_roms


 SUBROUTINE get_infovgrid_roms(romsgridnc, nx,ny,N, Vtransform, sc_r, sc_w, Cs_r, Cs_w, h, hc, category)
  use netcdf
  implicit none
  CHARACTER (LEN=512), intent(in) :: romsgridnc
  integer,intent(in):: nx,ny,N, category
  integer, intent(out)  :: Vtransform
  double precision, intent(out) :: sc_r(N), sc_w(0:N), Cs_r(N), Cs_w(0:N), h(nx,ny), hc
  CHARACTER (LEN=512) :: msg
  integer::nc_id, varid
! Open NetCDF grid file, read variables for zeta
  msg="Error reading file "//trim(romsgridnc)//"."
  call checknc( nf90_open(trim(romsgridnc), NF90_NOWRITE, nc_id), category, trim(msg))
  call checknc( nf90_inq_varid(nc_id, "Vtransform", varid), category, trim(msg))
  call checknc( nf90_get_var(nc_id, varid, Vtransform), category, trim(msg))
  call checknc( nf90_inq_varid(nc_id, "hc", varid), category, trim(msg))
  call checknc( nf90_get_var(nc_id, varid, hc), category, trim(msg))
  call checknc( nf90_inq_varid(nc_id, "h", varid), category, trim(msg))
  call checknc( nf90_get_var(nc_id, varid, h), category, trim(msg))
  call checknc( nf90_inq_varid(nc_id, "s_rho", varid), category, trim(msg))
  call checknc( nf90_get_var(nc_id, varid, sc_r), category, trim(msg))
  call checknc( nf90_inq_varid(nc_id, "s_w", varid), category, trim(msg))
  call checknc( nf90_get_var(nc_id, varid, sc_w), category, trim(msg))
  call checknc( nf90_inq_varid(nc_id, "Cs_r", varid), category, trim(msg))
  call checknc( nf90_get_var(nc_id, varid, Cs_r), category, trim(msg))
  call checknc( nf90_inq_varid(nc_id, "Cs_w", varid), category, trim(msg))
  call checknc( nf90_get_var(nc_id, varid, Cs_w), category, trim(msg))
  call checknc( nf90_close(nc_id), category, trim(msg))
 END SUBROUTINE get_infovgrid_roms


 SUBROUTINE set_depth(Vtransform, N, sc_r, sc_w, Cs_r, Cs_w, h, hc, Zt_avg1, z_r, z_w) !,Hz)
  implicit none
  integer, intent(in)  :: Vtransform, N
  double precision, intent(in) :: sc_r(N), sc_w(0:N), Cs_r(N), Cs_w(0:N)
  double precision, intent(in) :: hc
  real, intent(in) :: Zt_avg1(:,:)
  double precision, intent(in) :: h(:,:)
  !!! double precision, intent(out) :: Hz(:,:,:)
  double precision, intent(out) :: z_r(:,:,:), z_w(:,:,0:) !, Hz(:,:,:)
  integer :: i, j, k, nx, ny
  double precision :: cff_r, cff1_r, cff2_r, cff_w, cff1_w, cff2_w
  double precision :: hinv, hwater, z_r0, z_w0
  double precision :: C2_r, C2_w, hh2, vert_n1, vert_a, vert_h0, vert_s0
  nx=size(h,1)
  ny=size(h,2)

  IF (Vtransform.eq.3) THEN
    vert_s0 = 90./120.
    vert_n1 = 2./3. !3./5.
    vert_h0 = 60./(vert_s0)**(vert_n1)
    vert_a = 1./(vert_s0-1)
  END IF
  IF (Vtransform.eq.1) THEN
    DO j=1,ny
!      DO i=1,nx
!        z_w(i,j,0)=-h(i,j)
!      END DO
      DO k=1,N
        cff_r=hc*(sc_r(k)-Cs_r(k))
        cff_w=hc*(sc_w(k)-Cs_w(k))
        cff1_r=Cs_r(k)
        cff1_w=Cs_w(k)
        DO i=1,nx
          hwater=h(i,j)
          hinv=1.0/hwater
          z_w0=cff_w+cff1_w*hwater
!!          z_w(i,j,k)=z_w0+Zt_avg1(i,j)*(1.0+z_w0*hinv)
          z_r0=cff_r+cff1_r*hwater
          z_r(i,j,k)=z_r0+Zt_avg1(i,j)*(1.0+z_r0*hinv)
!!           Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
        END DO
      END DO
    END DO
  ELSE IF (Vtransform.eq.3) THEN
    DO j=1,ny
      DO k=1,N
        DO i=1,nx
          hwater=h(i,j)
          hinv=1.0/hwater
          hh2=(min(vert_h0,hwater))*hinv
          IF (sc_w(k).gt.-vert_s0) THEN
            C2_w=-hh2*(-sc_w(k))**(vert_n1)
            C2_r=-hh2*(-sc_r(k))**(vert_n1)
            cff_w=hc*(sc_w(k)-C2_w)
            cff1_w=C2_w
            cff_r=hc*(sc_r(k)-C2_r)
            cff1_r=C2_r
            z_w0=cff_w+cff1_w*hwater
!!            z_w(i,j,k)=z_w0+Zt_avg1(i,j)*(1.0+z_w0*hinv)
            z_r0=cff_r+cff1_r*hwater
            z_r(i,j,k)=z_r0+Zt_avg1(i,j)*(1.0+z_r0*hinv)
          ELSE
            C2_w=-hh2*(-sc_w(k))**(vert_n1)             &
 &               -(1-hh2)*(vert_a*(sc_w(k)+vert_s0))**2
            C2_r=-hh2*(-sc_r(k))**(vert_n1)             &
 &               -(1-hh2)*(vert_a*(sc_r(k)+vert_s0))**2
            cff_w=hc*(sc_w(k)-C2_w)
            cff1_w=C2_w
            cff_r=hc*(sc_r(k)-C2_r)
            cff1_r=C2_r
            z_w0=cff_w+cff1_w*hwater
!!            z_w(i,j,k)=z_w0+Zt_avg1(i,j)*(1.0+z_w0*hinv)
            z_r0=cff_r+cff1_r*hwater
            z_r(i,j,k)=z_r0+Zt_avg1(i,j)*(1.0+z_r0*hinv)
          END IF
!!           Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
        END DO
      END DO
    END DO
!-----------------------------------------------------------------------
!  New formulation: Compute vertical depths (meters, negative) at
!                   RHO- and W-points, and vertical grid thicknesses.
!  Various stretching functions are possible.
!
!         z_w(x,y,s,t) = zeta(x,y,t) + [zeta(x,y,t)+ h(x,y)] * Zo_w
!
!                 Zo_w = [hc * s(k) + C(k) * h(x,y)] / [hc + h(x,y)]
!
!-----------------------------------------------------------------------
  ELSE IF (Vtransform.eq.2) THEN
    DO j=1,ny
      DO i=1,nx
        z_w(i,j,0)=-h(i,j)
      END DO
      DO k=1,N
        cff_r=hc*sc_r(k)
        cff_w=hc*sc_w(k)
        cff1_r=Cs_r(k)
        cff1_w=Cs_w(k)
        DO i=1,nx
          hwater=h(i,j)
          hinv=1.0/(hc+hwater)
          cff2_r=(cff_r+cff1_r*hwater)*hinv
          cff2_w=(cff_w+cff1_w*hwater)*hinv
          z_w(i,j,k)=Zt_avg1(i,j)+(Zt_avg1(i,j)+hwater)*cff2_w
          z_r(i,j,k)=Zt_avg1(i,j)+(Zt_avg1(i,j)+hwater)*cff2_r
!!           Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
        END DO
      END DO
    END DO
  END IF
END SUBROUTINE set_depth

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

USE, INTRINSIC :: iso_fortran_env, ONLY : iostat_end
USE netcdf
! USE eccodes
USE grid_id_class
USE grid_class
USE vol7d_level_class
USE vol7d_timerange_class
USE volgrid6d_class
USE volgrid6d_var_class
USE grid_transform_class
! USE char_utilities
USE optionparser_class
USE err_handling
USE missing_values
USE log4fortran
USE datetime_class
USE simc_netcdf
use vol7d_var_class
use array_utilities
IMPLICIT NONE
! INTEGER, PARAMETER :: dstrl = 19       ! length of date strings in Chimere output
INTEGER, PARAMETER :: maxvars = 200    ! max number of variables in NetCDF input

! Definition of "volgrid6d" object
TYPE(volgrid6d), POINTER :: volgrid_out(:), volgrid_tpl(:)
TYPE(volgrid6d), POINTER :: volgrid_int(:), volgrid_coord(:)
TYPE(grid_id) :: gaid_tpl                          ! module grid_id_class
TYPE(griddim_def) :: griddim_out
TYPE (vol7d_level), allocatable :: int_levs(:)         ! module vol7d_level_class
TYPE(transform_def) :: trans                           ! module grid_transform_class
TYPE(grid_file_id) :: ifile

! Grid parameters
INTEGER :: nx,ny,component_flag,utm_zone,projection_center_flag=0
DOUBLE PRECISION :: xmin,xmax,ymin,ymax,xoff,yoff
DOUBLE PRECISION :: lov,latin1,latin2,lad,dx,dy
DOUBLE PRECISION :: latitude_south_pole,longitude_south_pole,angle_rotation
CHARACTER (LEN=80) :: proj_type
INTEGER :: nx_nc,ny_nc,nz_nc,nt_nc,nrtime,ntrange,nvar,ngvar,nz
!
TYPE (datetime) :: valtime
REAL, ALLOCATABLE :: values2(:,:), values3(:,:,:) 
!
INTEGER :: disc(maxvars),pc(maxvars),pn(maxvars),typelevel(maxvars)
CHARACTER (LEN=20) :: ncstring(maxvars)
CHARACTER (LEN=52),parameter :: grib_tpl="/usr/share/eccodes/samples/regular_ll_sfc_grib2.tmpl"

! Miscellanea
TYPE (optionparser) :: opt
TYPE (datetime) :: reftime
REAL :: fillvalue
INTEGER :: ncid,dimid_x,dimid_y,dimid_z,dimid_t
INTEGER :: varid,ndims,xt,varid2
INTEGER :: optstatus,pp,nw,kt,kl,kv
INTEGER :: ios,ios1,ios2,ios3,ios4,ier,ncstat
INTEGER :: scad
INTEGER, pointer :: p1(:),p2(:)
CHARACTER (LEN=512) :: ncfile,gribfile,gribfileout,gribfilez,nmlfile,chdum,chdum2,chrt, grib_model_tmpl
CHARACTER (LEN=512) :: a_name,log_name
CHARACTER (LEN=20) :: version,model,levels !!,dimname_x,dimname_y,dimname_z,dimname_t
CHARACTER (LEN=2) :: trange_type
LOGICAL :: optversion, opttemplatenml

!! disordinati Lidia
INTEGER :: category
INTEGER :: optind
INTEGER :: value_i, it, nzvar
CHARACTER (LEN=80) :: grbparameters_s
CHARACTER (LEN=23) :: data_s, datetime_s
CHARACTER (LEN=512) :: msg

!! ROMS ZETA
CHARACTER (LEN=512) :: romsgridnc
integer:: N, Vtransform,nlevels_out
double precision,allocatable :: sc_r(:), sc_w(:), Cs_r(:), Cs_w(:), h(:,:)
double precision:: hc
double precision,allocatable:: z_r(:,:,:), z_w(:,:,:) !, Hz(:,:,:)
real,allocatable:: zeta(:,:)
TYPE(arrayof_real)::interplevels
!! DEBUG
! ! INTEGER,allocatable::dimids(:)
! ! integer::ldim
!-------------------------------------------------------------------------------
! Constant parameters

CALL eh_setval(verbose=3)                ! set debug level
version = "1.0.0"                        ! version of this program
log_name = "simc_nc2grib"                ! rott name of LibSIM logfiles

! Initialize log4fortran (to enable logging messages from LibSIM routines
CALL l4f_launcher(a_name, a_name_force=log_name)
ier = l4f_init()
category=l4f_category_get(a_name//".main")

!-------------------------------------------------------------------------------
! Parsing command line arguments

! define the option parser; help on positional parametersa
opt = optionparser_new( &
 usage_msg = "Usage: simc_nc2grib [--templatenml] [--template] [--help] [--version] {grid-specifications}" // &
 "[--romsgridnc romsgridnc] ncfile gribfile nmlfile [reftime]", &
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
CALL optionparser_add(opt, ' ', 'templatenml', opttemplatenml, &
  help='WRITE a template for nmlfile (simc_nc2grib.nml) and exit')

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
!!
CALL optionparser_add(opt, ' ', 'component-flag', component_flag, &
 0, help='wind component flag in target grid (0/1)')
!! roms zeta
CALL optionparser_add(opt, ' ', 'romsgridnc', romsgridnc, '', &
 help='ROMS grid file with vertical grid information')
CALL optionparser_add(opt, ' ', 'interplevels', interplevels, &
 help='Levels (depth below sea surface) to interpolate roms fields in.')

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
IF (optind + 2 <= iargc()) THEN
  CALL getarg(optind, ncfile)

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

!-------------------------------------------------------------------------------
! Create a template with the output grid and the desired options (name=gribfile)
IF(grib_model_tmpl=='')then
    ! Define the "griddim" LibSIM object corresponding to ouput grid
    CALL init(griddim_out, nx=nx, ny=ny, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, &
      component_flag=component_flag, proj_type=proj_type, &
      zone=utm_zone, xoff=xoff, yoff=yoff, &
      lad=lad, lov=lov, latin1=latin1, latin2=latin2,  &
      longitude_south_pole=longitude_south_pole, latitude_south_pole=latitude_south_pole, &
      angle_rotation=angle_rotation, &
      projection_center_flag=projection_center_flag)

    ! import a template, change the grid
    ! 
    CALL IMPORT(volgrid_tpl, filename=grib_tpl, decode=.TRUE., &
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
    
    ! read gaid from template
    ! 
    ifile = grid_file_id_new(trim(grib_tpl), 'r')
    gaid_tpl = grid_id_new(ifile)

    !-------------------------------------------------------------------------------
    ! Read namelist and change the template accordingly as in nmlfile
    !
    OPEN (21, FILE=nmlfile, STATUS="OLD", FORM="FORMATTED", IOSTAT=ios)
    IF (ios /= 0) CALL raise_fatal_error( &
      msg="simc_nc2grib, file " // TRIM(nmlfile) // " not found", ierval=2)
      
    ios=0
    DO WHILE(.true.)
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
          !!! WRITE(*,*) TRIM(grbparameters_s), TRIM(chdum(pp+1:)), TRIM(grbparameters_s)=="model"
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
             call grib_set(grid_id_get_gaid(gaid_tpl),TRIM(grbparameters_s), value_i)
!              IF (iret /= CODES_SUCCESS) &
!                WRITE(*,*) "Warning: error setting "//TRIM(grbparameters_s)//", user-defind key in grib template"
          ELSEIF(TRIM(grbparameters_s)=="model" .or. &
               & TRIM(grbparameters_s)=="levels")THEN
              CYCLE
          ELSE
            CALL raise_fatal_error(msg="simc_nc2grib, unknown keyword in " // &
               TRIM(nmlfile) // ": " // chdum(1:pp-1), ierval=2)
          END IF
      ENDIF
    ENDDO
    CLOSE(21)
    IF(trange_type=="an")THEN
      call grib_set(grid_id_get_gaid(gaid_tpl), "typeOfGeneratingProcess", 0)
    ELSEIF(trange_type=="fc")THEN
      call grib_set(grid_id_get_gaid(gaid_tpl), "typeOfGeneratingProcess", 2)
    END IF
    !-------------------------------------------------------------------------------
    ! Write template in gribfile
    !
    volgrid_out(1)%gaid = gaid_tpl
    CALL export(volgrid_out, filename=trim(gribfile), categoryappend="output_volume")
    CALL delete(volgrid_tpl)
    CALL delete(volgrid_out)
    CALL delete(trans)
    ! updated template generated
    grib_model_tmpl=trim(gribfile)
ELSE
  !-------------------------------------------------------------------------------
  ! Read a template 
  ! with the output grid and the desired options (name=gribfile)
  CALL IMPORT(volgrid_out, filename=grib_model_tmpl, decode=.TRUE., &
      categoryappend="grib_model_tmpl")
  nx=size(volgrid_out(1)%voldati(:,1,1,1,1,1))
  ny=size(volgrid_out(1)%voldati(1,:,1,1,1,1))
  !! IMPORTANTE!! calcola i parametri tipo Dx e Dy
  CALL unproj(volgrid_out(1)%griddim)
  !! set griddim_out per volgrid_out
  griddim_out=volgrid_out(1)%griddim
  !!
  CALL delete(volgrid_out)
END IF


!-------------------------------------------------------------------------------
! default values
!
!! set model and levels
! 
model=""
levels = "all"
! default values if you need to interpolate vertical levels
nlevels_out=interplevels%arraysize
IF(nlevels_out>0)THEN
  allocate (int_levs(nlevels_out))
END IF

!-------------------------------------------------------------------------------
! Read nmlfile to get variables and parameters to export in grib
!
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
      !!! WRITE(*,*) TRIM(chdum)
      nw = word_split(chdum, word_start=p1, word_end=p2, sep=",")
      IF (nw /= 5) CALL raise_fatal_error(msg="simc_nc2grib, error parsing line " // &
        TRIM(chdum) // " in namelist " // TRIM(ncfile), ierval=2)
      nvar = nvar + 1
      ncstring(nvar) = chdum(p1(1):p2(1))
      READ (chdum(p1(2):p2(2)),*,IOSTAT=ios1) disc(nvar)
      READ (chdum(p1(3):p2(3)),*,IOSTAT=ios2) pc(nvar)
      READ (chdum(p1(4):p2(4)),*,IOSTAT=ios3) pn(nvar)
      READ (chdum(p1(5):p2(5)),*,IOSTAT=ios4) typelevel(nvar)
      !!! WRITE(*,*) ncstring(nvar), disc(nvar),pc(nvar),pn(nvar),typelevel(nvar)
      IF (ios1 /= 0 .OR. ios2 /= 0 .OR. ios3 /= 0 .OR. ios4 /= 0) CALL raise_fatal_error( &
       msg="simc_nc2grib, error parsing line " // TRIM(chdum) // " in namelist " // &
       TRIM(nmlfile), ierval=2)
  ENDIF
ENDDO
CLOSE(21)

!!-------------------------------------------------------------------------------
!! checks for model and levels -- ROMS
!!
IF(model=="")THEN
  msg="error reading model in namelist "//TRIM(nmlfile)
  call l4f_category_log(category,L4F_ERROR, trim(msg))
  call raise_error()
ELSE IF(model=="roms")THEN
  do kv=1, nvar
    IF(trim(ncstring(nvar))=="salt" .or. &
     & trim(ncstring(nvar))=="temp" .or. &
     & trim(ncstring(nvar))=="u_eastward" )then
        levels="roms"
        exit
    END IF
  end do
END IF

!!! set the dimension names correctly
call set_dimension_names(model)

!-------------------------------------------------------------------------------
! Open NetCDF files, read dimensions and attributes

msg="simc_nc2grib, file " // TRIM(ncfile) // "not found"
call checknc( nf90_open(ncfile, NF90_NOWRITE, ncid), category, trim(msg))

msg="simc_nc2grib, file " // TRIM(ncfile) // ": x dim " // TRIM(dimname_x) // " not found"
call checknc( nf90_inq_dimid(ncid, dimname_x, dimid_x), category, trim(msg))
call checknc( nf90_inquire_dimension(ncid, dimid_x, len=nx_nc), category, trim(msg))
  
msg="simc_nc2grib, file " // TRIM(ncfile) // ": y dim " // TRIM(dimname_y) // " not found"
call checknc( nf90_inq_dimid(ncid, dimname_y, dimid_y), category, trim(msg))
call checknc( nf90_inquire_dimension(ncid, dimid_y, len=ny_nc), category, trim(msg))

ncstat = nf90_inq_dimid(ncid, trim(dimname_z), dimid_z)
IF(ncstat==nf90_noerr)THEN
  call checknc( nf90_inquire_dimension(ncid, dimid_z, len=nz_nc), category, trim(msg))
ELSE
  msg="simc_nc2grib, file " //TRIM(ncfile)// ": z dim " // TRIM(dimname_z) // " not found"
  call l4f_category_log(category,L4F_INFO, trim(msg))
  levels="msl"
  nz_nc=1
END IF

msg="simc_nc2grib, file " // TRIM(ncfile) // ": t dim " // TRIM(dimname_t) // " not found"
call checknc( nf90_inq_dimid(ncid, dimname_t, dimid_t), category, trim(msg))
msg="simc_nc2grib, file " // TRIM(ncfile) // ": x dim " // TRIM(dimname_t) // " not found"
call checknc( nf90_inquire_dimension(ncid, dimid_t, len=nt_nc), category, trim(msg))
  
IF (nx_nc /= nx) THEN
  WRITE(msg,'(3a,2i5)') "simc_nc2grib, specifed X dimension is different from that in ", &
    TRIM(ncfile),": ",nx_nc,nx
  call l4f_category_log(category,L4F_FATAL,trim(msg))
  CALL raise_fatal_error(ierval=4)
ENDIF
IF (ny_nc /= ny) THEN
  WRITE(msg,'(3a,2i5)') "simc_nc2grib, specifed Y dimension is different from that in ", &
    TRIM(ncfile),": ",ny_nc,ny
  call l4f_category_log(category,L4F_FATAL,trim(msg))
  CALL raise_fatal_error(ierval=4)
ENDIF

!!
!! dimensiona il volume
!!
!! divido per tempo e variabile
!!
nrtime = 1
ntrange = 1
ngvar = 1

!!
!! livelli verticali
!!
IF (levels=="all" .or. levels=="roms") THEN
  nz = nz_nc
ELSE
  nz = 1
ENDIF
!!
!! numero delle varibili
!! nel caso di roms, mi devo ricostruire i livelli verticali
!!
IF(model=="roms" .and. levels=="roms")THEN
  IF(romsgridnc=='')then
    CALL l4f_category_log(category, L4F_FATAL, 'inline input arguments')
    msg="romsgridnc needed to compute roms levels"
    CALL l4f_category_log(category, L4F_FATAL, trim(msg))
    CALL raise_fatal_error(ierval=1)
  ELSE
    msg="Roms file for vertical grid information: "//trim(romsgridnc)
    call l4f_category_log(category,L4F_INFO,trim(msg))
  END IF
  nzvar = ngvar+1
ELSE
  nzvar = ngvar
END IF

!
! Log to stdout the list of input and output data
!
WRITE(msg,'(a,4i5)') "File NetCDF: nx,ny,nz,nt ",nx_nc,ny_nc,nz_nc,nt_nc
call l4f_category_log(category,L4F_INFO,trim(msg))
IF(model=='chimere')THEN
  dx = (xmax - xmin) / DBLE(nx-1)
  dy = (ymax - ymin) / DBLE(ny-1)
  WRITE(msg,'(a,4i5)') "Grid step: ",dx,dy
  call l4f_category_log(category,L4F_INFO,trim(msg))
END IF
WRITE(msg,*) "Required data: nz ",nz," nvar ",nvar,": ",ncstring(1:nvar)
call l4f_category_log(category,L4F_INFO,trim(msg))
IF(reftime /= datetime_miss)THEN
  data_s=to_char(reftime)
  msg="Reftime: "//TRIM(data_s)
  call l4f_category_log(category,L4F_INFO,trim(msg))
END IF


!-------------------------------------------------------------------------------
!! allocate arrays
ALLOCATE (values2(nx,ny))
if(nz>1)then
  ALLOCATE (values3(nx,ny,nz))
end if

!-------------------------------------------------------------------------------
! Create a "volgrid6d" LibSIM object that will contain the entire data volume
ALLOCATE(volgrid_out(1))

! Define the "volgrid6d" LibSIM object
CALL init (volgrid_out(1), griddim=griddim_out, time_definition=0)
CALL volgrid6d_alloc(volgrid_out(1), nlevel=nz, ntime=nrtime, ntimerange=ntrange, nvar=nzvar)
CALL volgrid6d_alloc_vol(volgrid_out(1), decode=.TRUE.)
!!! WRITE(*,*) nx,ny,nz,nrtime,ntrange,ngvar,nzvar


! Get "id" for grib2 template
ifile = grid_file_id_new(trim(grib_model_tmpl), 'r')
gaid_tpl=grid_id_new(ifile)
!! assign gaid
volgrid_out(1)%gaid = gaid_tpl

IF(levels=="roms")THEN
!! costruisci le zeta
!! definisci l'ultima variabile
!! depth below sea surface
  CALL init(volgrid_out(1)%var(nzvar), centre=centre, category=4, number=195, discipline=10)
  call get_nlevels_roms(romsgridnc, N, category)
  allocate(sc_r(N), sc_w(0:N), Cs_r(N), Cs_w(0:N), h(nx,ny), z_r(nx,ny,N), zeta(nx,ny), z_w(nx,ny,0:N))
  call get_infovgrid_roms(romsgridnc, nx,ny, N, Vtransform, sc_r, sc_w, Cs_r, Cs_w, h, hc, category)
  !!! 
  IF(nlevels_out>0)THEN
    ALLOCATE(volgrid_coord(1))
    CALL init (volgrid_coord(1), griddim=griddim_out, time_definition=0)
    CALL volgrid6d_alloc(volgrid_coord(1), nlevel=nz, ntime=nrtime, ntimerange=ntrange, nvar=1)
    CALL volgrid6d_alloc_vol(volgrid_coord(1), decode=.TRUE.)
  END IF  
END IF



!-------------------------------------------------------------------------------
! cycle variables
!
DO kv = 1,nvar
    ! Assign variable
    CALL init(volgrid_out(1)%var(1), centre=centre, category=pc(kv), number=pn(kv), discipline=disc(kv))

    ! Get information on the required variable
    !! msg="Error getting varid for var " // TRIM(ncstring(kv))
    ncstat = nf90_inq_varid(ncid, ncstring(kv), varid)
    IF(ncstat/=nf90_noerr)THEN
      !!! write(*,*) "cycle for "// TRIM(ncstring(kv))   
      CYCLE
    END IF
    msg="Error getting ndims for var " // TRIM(ncstring(kv))
    call checknc( nf90_inquire_variable(ncid, varid, ndims=ndims, xtype=xt), category, trim(msg))
    IF (ndims < 2 .OR. ndims > 4) THEN
      WRITE(msg,'(3a,i5)') "Bad number of dimensions for var ", TRIM(ncstring(kv)),": ", ndims
      call l4f_category_log(category,L4F_ERROR,trim(msg))
      CALL raise_error(ierval=10)
      CYCLE
    ELSE IF (xt /= NF90_FLOAT .AND. xt /= NF90_DOUBLE) THEN
      msg="Invalid type for var " // TRIM(ncstring(kv))
      call l4f_category_log(category,L4F_ERROR,trim(msg))
      CALL raise_error(ierval=10)
      CYCLE
    ENDIF
! ! debug    
! !     if (allocated(dimids)) deallocate(dimids)
! !     allocate(dimids(ndims))
! !     call checknc(nf90_inquire_variable(ncid, varid, dimids=dimids, xtype=xt), category)
! !     do it=1,ndims
! !       call checknc( nf90_inquire_dimension(ncid, dimids(it), len=ldim), category)
! !       WRITE(*,*) ldim
! !     end do
! !     write(*,*) "------------"
! ! debug    

    fillvalue = rmiss
    msg="Warning: No fillValue for var "//TRIM(ncstring(kv))
    call checknc( nf90_get_att(ncid, varid, '_FillValue', fillvalue), category, trim(msg))
    WRITE(msg,'(a,i2)') "Processing var "//TRIM(ncstring(kv))//" ndims ",ndims
    call l4f_category_log(category,L4F_INFO,trim(msg))
 
    !-------------------------------------------------------------------------------
    ! cycle time/timeranges
    ! Read data and store them in "volgrid6d" LibSIM object
    DO it=1,nt_nc
        !-------------------------------------------------------------------------------
        ! Read verification times from NetCDF data
        call read_time_nc (ncid, model, it, valtime, category)
        data_s=to_char(valtime)
        if (it==1)then
          msg="Verification times from "//TRIM(data_s)
          call l4f_category_log(category,L4F_INFO,trim(msg))
        elseif(it==nt_nc)then
          msg="Verification times to "//TRIM(data_s)
          call l4f_category_log(category,L4F_INFO,trim(msg))
        endif
        datetime_s=data_s(1:4)//data_s(6:7)//data_s(9:10)//data_s(12:13)//data_s(15:16)

        ! Assign times / timeranges
        IF (trange_type=="an") THEN
            volgrid_out(1)%time = valtime
            CALL init(volgrid_out(1)%timerange(1), timerange=254, p1=0, p2=0)
          gribfileout = trim(datetime_s)//'_'//trim(ncstring(kv))//'_an_'//trim(gribfile)
          gribfilez = trim(datetime_s)//'_'//trim(ncstring(kv))//'_z_an_'//trim(gribfile)
        ELSE IF (trange_type=="fc") THEN
            volgrid_out(1)%time = reftime
            CALL getval (valtime - reftime, AMINUTE=scad)
            CALL init(volgrid_out(1)%timerange(1), timerange=254, p1=scad*60, p2=0)
            IF (scad < 0) THEN
              WRITE(msg,'(a,i5,a,i4)') "simc_nc2grib: forecast with negative timerange: ",scad," at timestamp ",kt
              call l4f_category_log(category,L4F_ERROR,trim(msg))
              CALL raise_fatal_error(msg=msg, ierval=10)
            ENDIF
          gribfileout = trim(datetime_s)//'_'//trim(ncstring(kv))//'_fc_'//trim(gribfile)
          gribfilez = trim(datetime_s)//'_'//trim(ncstring(kv))//'_z_fc_'//trim(gribfile)
        ENDIF

        !-------------------------------------------------------------------------------
        ! Read NetCDF data and store them in the "volgrid6d" LibSIM object
        ! Single field (eg. latitude)
        IF (ndims==2) THEN
            !! volgrd6d dimensions: ix,iy,il,it,itr,iv
            msg="Error getting values for var " // TRIM(ncstring(kv))
            call checknc( nf90_get_var(ncid, varid, values2), category, trim(msg))
            WHERE (values2(:,:) /= fillvalue)
                volgrid_out(1)%voldati(:,:,1,1,1,1) = values2(:,:)
            ELSEWHERE
                volgrid_out(1)%voldati(:,:,1,1,1,1) = rmiss
            ENDWHERE

        ! surface field (eg. 2m temperature)
        ELSE IF (ndims==3) THEN
            !! volgrd6d dimensions: ix,iy,il,it,itr,iv
            !! Assign levels
            CALL init(volgrid_out(1)%level(1), level1=typelevel(kv))
            !! variabile
            msg="Error getting values for var " // TRIM(ncstring(kv))
            call checknc( nf90_get_var(ncid, varid, values2, start=(/1,1,it/), count=(/nx,ny,1/)), category, trim(msg))
            WHERE (values2 /= fillvalue)
                volgrid_out(1)%voldati(:,:,1,1,1,1) = values2(:,:)
            ELSEWHERE
                volgrid_out(1)%voldati(:,:,1,1,1,1) = rmiss
            ENDWHERE

        !-------------------------------------------------------------------------------
        ! 3D field (eg. PM10)
        ! Use of the "map" optional argument of nf90_get_var: "The elements of the index
        ! mapping vector correspond, in order, to the netCDF variable's dimensions;
        ! map(1) gives the distance between elements of the internal array corresponding
        ! to the most rapidly varying dimension of the netCDF variable"
        ELSE IF (ndims==4 .and. (levels=="all" .or. levels=="roms")) THEN
            ! Assign levels
            DO kl = 1, nz
                CALL init(volgrid_out(1)%level(kl), level1=typelevel(kv), l1=kl)
            END DO
            !! get values
            msg="Error getting values for var " //TRIM(ncstring(kv))
            call checknc( nf90_get_var(ncid, varid, values3, start=(/1,1,1,it/), count=(/nx,ny,nz,1/)), category, trim(msg))
            IF("model"=="roms" .and. trim(ncstring(kv))=="temp")THEN
              !! from Celsius to Kelvin degrees
              values3 = values3 +273.15
            END IF
            WHERE (values3 /= fillvalue)
                volgrid_out(1)%voldati(:,:,:,1,1,1) = values3
            ELSEWHERE
                volgrid_out(1)%voldati(:,:,:,1,1,1) = rmiss
            ENDWHERE

            IF(levels=="roms")then
              !!!
              !!! conmpute and write ROMS levels
              !!!
              msg="Error getting values for var zeta (ROMS)"
              call checknc( nf90_inq_varid(ncid, "zeta", varid2), category, trim(msg))
              call checknc( nf90_get_var(ncid, varid2, zeta, start=(/1,1,it/), count=(/nx,ny,1/)), category, trim(msg))
              !! compute roms levels
              call set_depth(Vtransform, N, sc_r, sc_w, Cs_r, Cs_w, h, hc, zeta, z_r, z_w)
              !!!
              !!!  change from geiod to sea surface and write levels in volume
              !!!
              do kl=1,N
               WHERE (values3(:,:,kl) /= fillvalue)
                  volgrid_out(1)%voldati(:,:,kl,1,1,nzvar) = REAL(-(z_r(:,:,kl) - zeta(:,:)))
                ELSEWHERE
                  volgrid_out(1)%voldati(:,:,kl,1,1,nzvar) = rmiss
                ENDWHERE
              end do
              
              !!!
              !!! ROMS vertical interpolation
              !!!
              IF(nlevels_out>0 .and. levels=="roms")THEN
                !! in millimetri
                !! set vg6_levs to desired output levels
                do kl=1,interplevels%arraysize
                   CALL init(int_levs(kl), level1=161, l1=int(abs(interplevels%array(kl)*1000)))
                end do 
                volgrid_coord(1)%var = volgrid_out(1)%var(nzvar)
                volgrid_coord(1)%time = volgrid_out(1)%time
                volgrid_coord(1)%timerange = volgrid_out(1)%timerange
                volgrid_coord(1)%level = volgrid_out(1)%level
                volgrid_coord(1)%voldati(:,:,:,:,:,1) = volgrid_out(1)%voldati(:,:,:,:,:,nzvar)
                !! init vertical transformation
                CALL init(trans, trans_type='vertint', sub_type='linear', &
                         & output_levtype=int_levs(1),            &
                         & input_levtype=volgrid_out(1)%level(1), &
                         & categoryappend="transformation")
                !! do interpolation
                CALL transform(trans, volgrid6d_in=volgrid_out, &
                             & volgrid6d_out=volgrid_int, &
                             & lev_out=int_levs, &
                             & volgrid6d_coord_in=volgrid_coord(1), &
                             & clone=.TRUE., decode=.TRUE., &
                             & categoryappend="transform")
                 call export(volgrid_int, filename=trim(gribfileout), & 
                   & categoryappend="output_volume")
                 gribfileout=gribfilez
              END IF
            end if
        ENDIF
        !-------------------------------------------------------------------------------
        ! Export data to GRIB2 output file
        WRITE(*,*)  MINVAL(volgrid_out(1)%voldati(:,:,1:nz,1:nrtime,1:ntrange,1:ngvar)), &
                  & maxval(volgrid_out(1)%voldati(:,:,1,1,1,1)), trim(gribfileout)
        CALL export(volgrid_out, filename=trim(gribfileout), & 
                   & categoryappend="output_volume")
    ENDDO
ENDDO


CALL delete(volgrid_out)
call checknc( nf90_close(ncid), category)

STOP
END PROGRAM simc_nc2grib

!-------------------------------------------------------------------------------

SUBROUTINE write_template
!
! Write a template for namelist file
!
OPEN (20, FILE="simc_nc2grib.nml", STATUS="REPLACE")
WRITE(20,'(a)') "! Namelist file for simc_nc2grib"
WRITE(20,'(a)') "! Empty lines and lines beginning with """ // CHAR(33) // """ are ignored"
WRITE(20,'(a)')
WRITE(20,'(a)') "! 1) General namelist"
WRITE(20,'(a)') "! - model: ""chimere"" or ""roms"": this flag define the names of the coordinate"
WRITE(20,'(a)') "!   dimensions, and the name and format of timestamps"
WRITE(20,'(a)') "! - levels: ""all"" or ""surface"": this flag define which levels will be included in"
WRITE(20,'(a)') "!   output file"
WRITE(20,'(a)') "! - productDefinitionTemplateNumber: usually set to 0;"
WRITE(20,'(a)') "!   set to 40 to encode air quality parameters following C3S conventions;"
WRITE(20,'(a)') "!   in this case, the 4th field in section 2 of this namelist is the constituentType"
WRITE(20,'(a)') "! - centre, subCentre, generatingProcessIdentifier, bitsPerValue: set the"
WRITE(20,'(a)') "!   corresponding keys in sections 1, 1, 4, 5."
WRITE(20,'(a)')
WRITE(20,'(a)') "model=""chimere"""
WRITE(20,'(a)') "levels=""surface"""
WRITE(20,'(a)') "centre=80"
WRITE(20,'(a)') "subCentre=255"
WRITE(20,'(a)') "scanningMode=64"
WRITE(20,'(a)') "! productDefinitionTemplateNumber=0"
WRITE(20,'(a)') "! typeOfGeneratingProcess=0"
WRITE(20,'(a)') "! generatingProcessIdentifier=1"
WRITE(20,'(a)') "! bitsPerValue=24"
WRITE(20,'(a)') ""
WRITE(20,'(a)') "! 2) Required parameters"
WRITE(20,'(a)') "! one line for each required parameter:"
WRITE(20,'(a)') "! NetCDF-string,discipline,parameterCategory,parameterNumber/constituentType,typeOfFirstFixedSurface"
WRITE(20,'(a)') "O3,0,200,151,105"
WRITE(20,'(a)') "sea_level,10,3,1,101"
CLOSE (20)

RETURN
END SUBROUTINE write_template
