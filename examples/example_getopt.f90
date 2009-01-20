PROGRAM example_getopt
USE getopt_m
USE err_handling
IMPLICIT NONE

CHARACTER(len=1) :: ch
CHARACTER(len=512) :: extraopt
TYPE(option_s) :: opts(2)
INTEGER :: i

! Define two long options corresponding to short options a and b,
! the first without, the second with optional argument
opts(1) = option_s( 'alpha', .false., 'a' )
opts(2) = option_s( 'beta',  .true.,  'b' )

! Loop over options
DO
  SELECT CASE( getopt( 'ab:c', opts ))
  CASE( CHAR(0)) ! end of options
    EXIT
  CASE( 'a' ) ! long/short option without argument
    CALL print_info('option alpha/a')
  CASE( 'b' ) ! long/short option with argument
    CALL print_info('option beta/b='//TRIM(optarg))
  CASE( 'c' ) ! no long option here
    CALL print_info('option c')
  CASE( '?' )
    CALL raise_error('unknown option '//TRIM(optopt))
  CASE default
    CALL raise_error('unhandled option '//TRIM(optopt)//' this should not happen')
  END SELECT
END DO

IF (optind <= iargc()) THEN
  CALL print_info('extra arguments provided:')
  DO i = optind, iargc()
    CALL getarg(i, extraopt)
    CALL print_info(TRIM(extraopt))
  ENDDO
ENDIF

END PROGRAM example_getopt
