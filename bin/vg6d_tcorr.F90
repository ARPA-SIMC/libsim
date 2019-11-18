PROGRAM vg6d_tcorr
USE log4fortran
USE err_handling
USE missing_values
USE char_utilities
USE phys_const
USE optionparser_class
USE vol7d_var_class
USE grid_class
USE volgrid6d_var_class
USE vol7d_level_class
USE volgrid6d_class
USE vol7d_class
USE vol7d_dballe_class
USE grid_transform_class
IMPLICIT NONE

INTEGER :: category, ier, i, j, k, l, gridsize, tindex, hindex
CHARACTER(len=512) :: a_name, input_orography, output_orography, &
 input_file, output_file
CHARACTER(len=12) :: tcorr_method, output_orography_format
CHARACTER(len=160) :: trans_type
REAL :: tgrad, orography_thresh
TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
LOGICAL :: version, ldisplay, gridded
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d_var) :: varbufr
TYPE(volgrid6d),POINTER  :: volgrid(:),  volgrid_tmp(:), volgrid_io(:), volgrid_oo(:)
TYPE(vol7d) :: v7d_io, v7d_oo, v7d_t, v7dtmp
TYPE(vol7d_dballe) :: v7d_dba
TYPE(transform_def) :: trans

! innitialise logging
CALL l4f_launcher(a_name,a_name_force='prodsim_vg6d_tcorr')
ier=l4f_init()
! set a_name
category=l4f_category_get(TRIM(a_name)//'.main')

! define the option parser
opt = optionparser_new(description_msg= &
 'Tool for correcting near-surface temperature according to height difference &
 &between model orography and detailed orography.', &
 usage_msg='Usage: prodsim_vg6d_tcorr [options] inputfile outputfile')

CALL optionparser_add(opt, ' ', 'tcorr-method', tcorr_method, default='dry', &
 help='method for determining the vertical temperature gradient, &
 &''dry'' for dry adiabatic gradient, &
 &''user'' for constant gradient &
 &provided by the user with the --tgrad argument')
tgrad = rmiss
CALL optionparser_add(opt, ' ', 'tgrad', tgrad, &
 help='constant vertical temperature gradient in K/m, to be used with &
 &--tcorr-method=user, it should be <0 for temperature decreasing with height')
input_orography = ''
CALL optionparser_add(opt, ' ', 'input-orography', input_orography, &
 help='name of file containing the orography associated to the input &
 &temperature data, it should be on the same grid as the input data')
output_orography = ''
CALL optionparser_add(opt, ' ', 'output-orography', output_orography, &
 help='name of file containing the target orography to which temperature &
 &should be corrected in output, if gridded, it should be on the same grid &
 &as the input data, otherwise it should be a set of sparse points with height data')
CALL optionparser_add(opt, ' ', 'output-orography-format', output_orography_format, &
 default='grib_api', &
 help='format of the file defining output orography, grib_api or BUFR')
CALL optionparser_add(opt, ' ', 'trans-type', trans_type, &
 default='inter:bilin', help= &
 'transformation type (grid to sparse points), in the case of output on &
 &sparse points, in the form ''trans-type:subtype''')
orography_thresh = rmiss
CALL optionparser_add(opt, ' ', 'orography-thresh', orography_thresh, &
 help='threshold for orography difference in m, if provided, points having &
 &an orography difference higher than the value requested, are eliminated from output')

! display option
CALL optionparser_add(opt, 'd', 'display', ldisplay, help= &
 'briefly display the data volumes imported')

! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0) ! generate a clean manpage
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line arguments')
  CALL raise_fatal_error()
ENDIF
IF (version) THEN
  WRITE(*,'(A,1X,A)')'prodsim_vg6d_tcorr','0.1'
  CALL exit(0)
ENDIF

IF (tcorr_method == 'dry') THEN
  tgrad = - gearth/cpd
ELSE IF (tcorr_method == 'user') THEN
  IF (.NOT.c_e(tgrad)) THEN
    CALL l4f_category_log(category,L4F_ERROR, &
     'argument --tcorr-method=user requires specification of --tgrad')
    CALL raise_fatal_error()
  ENDIF
ELSE
  CALL l4f_category_log(category,L4F_ERROR, &
   'value '//TRIM(tcorr_method)//' not valid for argument --tcorr-method')
  CALL raise_fatal_error()
ENDIF

IF (optind+1 /= iargc()) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'input and/or output file missing')
  CALL raise_fatal_error()
  CALL EXIT(1)
ENDIF

! last argument is output file
CALL getarg(iargc(), output_file)

CALL l4f_category_log(category,L4F_INFO,'output file: '//TRIM(output_file))

IF (input_orography /= '') THEN
  CALL IMPORT(volgrid_io, filename=input_orography, decode=.TRUE., dup_mode=0, &
   time_definition=0, categoryappend='input_oro')
  IF (SIZE(volgrid_io) > 1) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error '//t2c(SIZE(volgrid_io))//' grids found in '//TRIM(input_orography))
    CALL raise_fatal_error()
  ENDIF
ELSE
  CALL l4f_category_log(category, L4F_ERROR, &
   'error input orography not provided')
  CALL raise_fatal_error()
ENDIF

IF (output_orography /= '') THEN
  IF (output_orography_format == 'grib_api') THEN
    CALL IMPORT(volgrid_oo, filename=output_orography, decode=.TRUE., dup_mode=0, &
     time_definition=0, categoryappend='output_oro')
    IF (SIZE(volgrid_oo) > 1) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error '//t2c(SIZE(volgrid_oo))//' grids found in '//TRIM(output_orography))
      CALL raise_fatal_error()
    ENDIF
    gridded = .TRUE.

  ELSE IF (output_orography_format == 'BUFR' .OR. output_orography_format == 'CREX') THEN
    CALL init(v7d_dba, filename=output_orography, FORMAT=output_orography_format, file=.TRUE., &
     WRITE=.FALSE., categoryappend="output_orography")
!    CALL IMPORT(v7d_dba, anavar=(/'B07030','B07031','B07002','B07007'/), &
!     anavarkind=(/'r','r','r','r'/), anaonly=.TRUE.)
! temporary, improve by importing all and selecting only present variable
!    CALL IMPORT(v7d_dba, anavar=(/'B07030'/), &
!     anavarkind=(/'r'/), anaonly=.TRUE.)
    CALL IMPORT(v7d_dba, anaonly=.TRUE.)
    v7d_oo = v7d_dba%vol7d

! destroy v7d_dba without deallocating the contents passed to v7d
    CALL init(v7d_dba%vol7d)
    CALL delete(v7d_dba)
    gridded = .FALSE.

  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error output orography format '//TRIM(output_orography_format)//' unknown')
    CALL raise_fatal_error()
  ENDIF
ELSE
  CALL l4f_category_log(category, L4F_ERROR, &
   'error output orography not provided')
  CALL raise_fatal_error()
ENDIF
! check for orography variables, etc.?

! loop on input file(s)
!DO WHILE(optind <= iargc()-1)
CALL getarg(optind, input_file)
CALL l4f_category_log(category,L4F_INFO,'importing file: '//TRIM(input_file))
CALL IMPORT(volgrid, filename=input_file, decode=.TRUE., dup_mode=0, &
 time_definition=0, categoryappend='input_volume')
IF (.NOT.ASSOCIATED(volgrid)) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error importing input volume from file '//TRIM(input_file))
  CALL raise_fatal_error()
ENDIF

IF (SIZE(volgrid) > 1) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error '//t2c(SIZE(volgrid))//' grids found in '//TRIM(input_file))
  CALL raise_fatal_error()
ENDIF

! round the volume to flatten similar level and timeranges
!CALL rounding(volgrid, volgrid_tmp, level=almost_equal_levels, &
! nostatproc=.TRUE.)
!CALL delete(volgrid)
!volgrid => volgrid_tmp
!NULLIFY(volgrid_tmp)

IF (ldisplay) THEN
  PRINT*,'input volume >>>>>>>>>>>>>>>>>>>>'
  CALL display(volgrid)
ENDIF

! check for consistency
IF (SIZE(volgrid(1)%level) /= 1) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error '//t2c(SIZE(volgrid(1)%level))//' levels found in '//TRIM(input_file))
  CALL raise_fatal_error()
ENDIF

j = volgrid(1)%griddim%dim%nx*volgrid(1)%griddim%dim%ny
gridsize = SIZE(volgrid(1)%voldati,1)*SIZE(volgrid(1)%voldati,2)
IF (j /= gridsize .OR. gridsize == 0) THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error inconsistent grid sizes: '//t2c(j)//','//t2c(gridsize)//' in '//TRIM(input_file))
  CALL raise_fatal_error()
ENDIF

!PRINT*,ASSOCIATED(volgrid(1)%griddim%dim%lon),ASSOCIATED(volgrid(1)%griddim%dim%lat)
! if coordinates of input grid are needed, do the following, then
! coordinates will be allocated in arrays volgrid(1)%griddim%dim%lon
! volgrid(1)%griddim%dim%lat
!  CALL unproj(volgrid(1)%griddim)
!  PRINT*,ASSOCIATED(volgrid(1)%griddim%dim%lon),ASSOCIATED(volgrid(1)%griddim%dim%lat)

IF (gridded) THEN

  tindex = imiss
  DO j = 1, SIZE(volgrid(1)%var)
    varbufr = convert(volgrid(1)%var(j))
    IF (varbufr%btable == 'B12101') THEN
      tindex = j
      EXIT
    ENDIF
  ENDDO

  IF (c_e(tindex)) THEN
    DO k = 1, SIZE(volgrid(1)%timerange)
      DO j = 1, SIZE(volgrid(1)%time)
        DO i = 1, SIZE(volgrid(1)%level)
          IF (volgrid(1)%level(i)%level1 == 1 .OR. &
           volgrid(1)%level(i)%level1 == 103) THEN ! only fixed height over surface
! (x,y,level,time,timerange,var)
            WHERE(c_e(volgrid(1)%voldati(:,:,i,j,k,tindex)))
              volgrid(1)%voldati(:,:,i,j,k,tindex) = &
               volgrid(1)%voldati(:,:,i,j,k,tindex) + &
               tgrad*(volgrid_oo(1)%voldati(:,:,1,1,1,1) - &
               volgrid_io(1)%voldati(:,:,1,1,1,1))
            END WHERE
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDIF

  CALL export(volgrid, output_file)
  CALL delete(volgrid)

ELSE

  IF (trans_type == '') THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error, --trans-type must be specified for output on sparse points')
    CALL raise_fatal_error()
  ENDIF
  i = word_split(trans_type, w_s, w_e, ':')
  IF (i /= 2) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error, syntax '//TRIM(trans_type)//' for --trans-type not correct')
    CALL raise_fatal_error()
  ENDIF

  CALL init(trans, trans_type=trans_type(w_s(1):w_e(1)), &
   sub_type=trans_type(w_s(2):w_e(2)), categoryappend="transformation")

! convert to real data
  CALL vol7d_convr(v7d_oo, v7dtmp, anaconv=.TRUE.)
  CALL delete(v7d_oo)
  v7d_oo = v7dtmp
  CALL init(v7dtmp) ! detach it
  
! try different variables for station height:
! height of ground, height of barometer, height or altitude, height
  hindex = firsttrue(v7d_oo%anavar%r(:)%btable == 'B07030')
  IF (hindex < 1) hindex = firsttrue(v7d_oo%anavar%r(:)%btable == 'B07031')
  IF (hindex < 1) hindex = firsttrue(v7d_oo%anavar%r(:)%btable == 'B07002')
  IF (hindex < 1) hindex = firsttrue(v7d_oo%anavar%r(:)%btable == 'B07007')

  IF (hindex < 1) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error station height not found in '//TRIM(output_orography))
    CALL raise_fatal_error()
  ENDIF
  CALL l4f_category_log(category, L4F_INFO, 'output orography found in variable '// &
   TRIM(v7d_oo%anavar%r(hindex)%btable)//' at position '//t2c(hindex))

  CALL transform(trans, volgrid(1), v7d_t, v7d_oo, &
   categoryappend="transform t")

  CALL transform(trans, volgrid_io(1), v7d_io, v7d_oo, &
   categoryappend="transform oro")

! look for temperature in input file interpolated
  tindex = firsttrue(v7d_t%dativar%r(:)%btable == 'B12101')
  IF (tindex < 1) THEN
    CALL l4f_category_log(category, L4F_ERROR, &
     'error temperature not found in '//TRIM(input_file)//' after interpolation')
    CALL raise_fatal_error()
  ENDIF
! input orography is in dati, interpolated from grib, variable still unchecked
! output orography is in ana (station data)
  IF (ASSOCIATED(v7d_io%voldatir) .AND. ASSOCIATED(v7d_oo%volanar) .AND. &
   c_e(tindex)) THEN

    DO l = 1, SIZE(v7d_t%network)
      DO k = 1, SIZE(v7d_t%timerange)
        DO j = 1, SIZE(v7d_t%level)
          DO i = 1, SIZE(v7d_t%time)
            WHERE(c_e(v7d_oo%volanar(:,hindex,1)) .AND. &
             c_e(v7d_io%voldatir(:,1,1,1,1,1)) .AND. &
             c_e(v7d_t%voldatir(:,i,j,k,tindex,l)))
              v7d_t%voldatir(:,i,j,k,tindex,l) = v7d_t%voldatir(:,i,j,k,tindex,l) + &
               tgrad*(v7d_oo%volanar(:,hindex,1)-v7d_io%voldatir(:,1,1,1,1,1))
            ELSEWHERE
              v7d_t%voldatir(:,i,j,k,tindex,l) = rmiss
            END WHERE
            IF (c_e(orography_thresh)) THEN ! reset to missing if threshold provided
              WHERE(c_e(v7d_oo%volanar(:,hindex,1)) .AND. &
               c_e(v7d_io%voldatir(:,1,1,1,1,1)) .AND. &
               c_e(v7d_t%voldatir(:,i,j,k,tindex,l)) .AND. &
               v7d_oo%volanar(:,hindex,1)-v7d_io%voldatir(:,1,1,1,1,1) > orography_thresh)
                
                v7d_t%voldatir(:,i,j,k,tindex,l) = rmiss
              END WHERE
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDIF

  CALL init(v7d_dba, filename=output_file, FORMAT=output_orography_format, &
   file=.TRUE., WRITE=.TRUE., wipe=.TRUE., categoryappend="export", &
   TEMPLATE='generic')
  v7d_dba%vol7d = v7d_t
  CALL export(v7d_dba)
  CALL delete(v7d_dba)

ENDIF
!  optind = optind + 1
!ENDDO

END PROGRAM vg6d_tcorr
