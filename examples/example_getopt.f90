program example_getopt
use getopt_m
implicit none
character:: ch
type(option_s):: opts(2)
opts(1) = option_s( "alpha", .false., 'a' )
opts(2) = option_s( "beta",  .true.,  'b' )
do
  select case( getopt( "ab:c", opts ))
  case( char(0))
    exit
  case( 'a' )
    print *, 'option alpha/a'
  case( 'b' )
    print *, 'option beta/b=', optarg
  case( '?' )
    print *, 'unknown option ', optopt
    stop
  case default
    print *, 'unhandled option ', optopt, ' (this is a bug)'
  end select
end do
end program example_getopt
