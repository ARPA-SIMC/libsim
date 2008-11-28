! ------------------------------------------------------------
! Copyright 2008 by Mark Gates
!
! This program is free software; you can redistribute or modify it under
! the terms of the GNU general public license (GPL), version 2 or later.
!
! This program is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! merchantability or fitness for a particular purpose.
!
! If you wish to incorporate this into non-GPL software, please contact
! me regarding licensing terms.
!
! ------------------------------------------------------------

!> \brief Fortran 95 getopt() and getopt_long(),similar to those in standard C library.
!! \include example_getopt.f90
!! \ingroup base

module getopt_m
!	use util_m
implicit none
character(len=80):: optarg
character:: optopt
integer:: optind=1
logical:: opterr=.true.


!> \brief describes one long option
!! The name field is the option name, without the leading -- double dash.
!! Set the has_arg field to true if it requires an argument, false if not.
!! The val field is returned. Typically this is set to the corresponding short
!! option, so short and long options can be processed together. (But there
!! is no requirement that every long option has a short option, or vice-versa.)
type option_s
  character(len=80) :: name    !< option name, without the leading -- double dash
  logical           :: has_arg !< true if it requires an argument, false if not
  character         :: val     !< the corresponding short option
end type option_s
	
! grpind is index of next option within group; always >= 2
integer, private:: grpind=2

contains

! ----------------------------------------


!> \brief Fortran 95 getopt() and getopt_long(), similar to those in standard C library.
!!
!! ch = getopt( optstring, [longopts] )
!! Returns next option character from command line arguments.
!! If an option is not recognized, it returns '?'.
!! If no options are left, it returns a null character, char(0).
!!
!! optstring contains characters that are recognized as options.
!! If a character is followed by a colon, then it takes a required argument.
!! For example, "x" recognizes "-x", while "x:" recognizes "-x arg" or "-xarg".
!!
!! optopt is set to the option character, even if it isn't recognized.
!! optarg is set to the option's argument.
!! optind has the index of the next argument to process. Initially optind=1.
!! Errors are printed by default. Set opterr=.false. to suppress them.
!!
!! Grouped options are allowed, so "-abc" is the same as "-a -b -c".
!!
!! If longopts is present, it is an array of type(option_s), where each entry
!! describes one long option.
!!
!! The name field is the option name, without the leading -- double dash.
!! Set the has_arg field to true if it requires an argument, false if not.
!! The val field is returned. Typically this is set to the corresponding short
!! option, so short and long options can be processed together. (But there
!! is no requirement that every long option has a short option, or vice-versa.)
!!
!! Differences from C version:
!! - when options are finished, C version returns -1 instead of char(0),
!!   and thus stupidly requires an int instead of a char.
!! - does not support optreset
!! - does not support "--" as last argument
!! - if no argument, optarg is blank, not NULL
!! - argc and argv are implicit
!!
!! Differences for long options:
!! - optional argument to getopt(), rather than separate function getopt_long()
!! - has_arg is logical, and does not support optional_argument
!! - does not support flag field (and thus always returns val)
!! - does not support longindex
!! - does not support "--opt=value" syntax, only "--opt value"
!! - knows the length of longopts, so does not need an empty last record
!!
!! Copyright 2008 by Mark Gates

character function getopt( optstring, longopts )
	! arguments
type(option_s) ,   intent(in), optional :: longopts(:) !< If longopts is present, it is an array where each entry describes one long option.
character(len=*) , intent(in) :: optstring !< optstring contains characters that are recognized as option

	! local variables
character(len=80):: arg
	
optarg = ''
if ( optind > iargc()) then
  getopt = char(0)
endif

call getarg( optind, arg )
if ( present( longopts ) .and. arg(1:2) == '--' ) then
  getopt = process_long( longopts, arg )
elseif ( arg(1:1) == '-' ) then
  getopt = process_short( optstring, arg )
else
  getopt = char(0)
endif

end function getopt


! ----------------------------------------
character function process_long( longopts, arg )
	! arguments
        type(option_s),   intent(in):: longopts(:)
        character(len=*), intent(in):: arg
	
	! local variables
	integer:: i
	
	! search for matching long option
	optind = optind + 1
	do i = 1, size(longopts)
		if ( arg(3:) == longopts(i)%name ) then
			optopt = longopts(i)%val
			process_long = optopt
			if ( longopts(i)%has_arg ) then
				if ( optind <= iargc()) then
					call getarg( optind, optarg )
					optind = optind + 1
				elseif ( opterr ) then
					 print '(a,a,a)', "Error: option '", trim(arg), "' requires an argument"
				endif
			endif
			return
		endif
	end do
	! else not found
	process_long = '?'
	if ( opterr ) then
		print '(a,a,a)', "Error: unrecognized option '", trim(arg), "'"
	endif
end function process_long


! ----------------------------------------
character function process_short( optstring, arg )
	! arguments
	character(len=*), intent(in):: optstring, arg
	
	! local variables
	integer:: i, arglen
	
	arglen = len( trim( arg ))
	optopt = arg(grpind:grpind)
	process_short = optopt
	
	i = index( optstring, optopt )
	if ( i == 0 ) then
		! unrecognized option
		process_short = '?'
		if ( opterr ) then
			print '(a,a,a)', "Error: unrecognized option '-", optopt, "'"
		endif
	endif
!	if ( i > 0 .and. substr( optstring, i+1, i+1 ) == ':' ) then
	if ( i > 0 .and. optstring(i+1:i+1) == ':' ) then
		! required argument
		optind = optind + 1
		if ( arglen > grpind ) then
			! -xarg, return remainder of arg
			optarg = arg(grpind+1:arglen)
		elseif ( optind <= iargc()) then
			! -x arg, return next arg
			call getarg( optind, optarg )
			optind = optind + 1
		elseif ( opterr ) then
			print '(a,a,a)', "Error: option '-", optopt, "' requires an argument"
		endif
		grpind = 2
	elseif ( arglen > grpind ) then
		! no argument (or unrecognized), go to next option in argument (-xyz)
		grpind = grpind + 1
	else
		! no argument (or unrecognized), go to next argument
		grpind = 2
		optind = optind + 1
	endif
end function process_short


!> \example example_getopt.f90
!! \brief example of use of getopt routine
!! Fortran 95 getopt() and getopt_long(), similar to those in standard C library.
end module getopt_m

