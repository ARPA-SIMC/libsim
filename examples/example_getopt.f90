PROGRAM example_getopt
USE getopt_m
USE log4fortran
IMPLICIT NONE

CHARACTER(len=1) :: ch
CHARACTER(len=512) :: extraopt
TYPE(option_s) :: opts(2)
INTEGER :: i, iargc

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
    CALL l4f_log(L4F_INFO, 'option alpha/a')
  CASE( 'b' ) ! long/short option with argument
    CALL l4f_log(L4F_INFO, 'option beta/b='//TRIM(optarg))
  CASE( 'c' ) ! no long option here
    CALL l4f_log(L4F_INFO, 'option c')
  CASE( '?' )
    CALL l4f_log(L4F_FATAL, 'unknown option '//TRIM(optopt))
    call exit(1)
  CASE default
    CALL l4f_log(L4F_FATAL, 'unhandled option '//TRIM(optopt)//' this should not happen')
    call exit(2)
  END SELECT
END DO

IF (optind <= iargc()) THEN
  CALL l4f_log(L4F_INFO, 'extra arguments provided:')
  DO i = optind, iargc()
    CALL getarg(i, extraopt)
    CALL l4f_log(L4F_INFO, TRIM(extraopt))
  ENDDO
ENDIF

END PROGRAM example_getopt
