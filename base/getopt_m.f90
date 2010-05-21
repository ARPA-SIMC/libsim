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
!> Module for parsing command-line optons.
!! This module defines 2 alternative approaches for parsing
!! command-line arguments: the Fortran 95 getopt() and getopt_long()
!! functions, similar to those in standard C library and the optparser
!! class, similar to the one found in the Python library.
!!
!! The getopt function scans the command line and returns the options
!! and possible optional arguments matching those requested.  The
!! scanning stops when a non option argument (i.e. not starting with
!! \c -) or the special argument \c -- is found, so that the remaining
!! arguments (through \a optind) can be treated as a list of files or
!! operands for the program.
!!
!! \include example_getopt.f90
!!
!! The optparse class provides a more object-oriented approach and a
!! simple way to build a help message from the command-line options
!! definition.
!!
!! \ingroup base
MODULE getopt_m
USE log4fortran
USE err_handling
USE kinds
USE char_utilities
IMPLICIT NONE

CHARACTER(len=80):: optarg !< the argument to the current option, if applicable
CHARACTER:: optopt !< the current option character (for long options it is the corresponding short option)
INTEGER:: optind=1 !< the index of current option

!> This derived type describes a long option.
!! The \a name field is the option name, without the leading \c -- double dash.
!! Set the \a has_arg field to \c .TRUE. if it requires an argument,
!! to \c .FALSE. if not.
!! The \a val field is the value that has to be returned by getopt when such a
!! long option is encountered.
!! Typically this is set to the corresponding short
!! option, so short and long options can be processed together, but there
!! is no requirement that every long option has a short option, or vice-versa.
type option_s
  character(len=80) :: name    !< option name, without the leading \c -- double dash
  logical           :: has_arg !< \c .TRUE. if option requires an argument, \c .FALSE. if not
  character         :: val     !< the corresponding short option
end type option_s

! grpind is index of next option within group; always >= 2
integer, private:: grpind=2

INTERFACE op_option_new
  MODULE PROCEDURE op_optionc_new, op_optioni_new, op_optionr_new, op_optiond_new, &
   op_optionl_new !, op_option_newcount
END INTERFACE

INTERFACE c_e
  MODULE PROCEDURE op_option_c_e
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE optionparser_delete, op_option_delete
END INTERFACE


!> This class allows to describe how to parse the command-line options
!! of a program in an object-oriented way, similarly to the optparse
!! class found in Python library. The command-line options are
!! described by an array of getopt_m::op_option objects, and the
!! effect of command line parsing is to set some desired variables
!! according to the information provided on the command line.
!!
!! The class handles both GNU-style long options, introduced by a
!! double dash \c -- and containing any character except the equal
!! sign \c == , and the traditional Unix short options, introduced by
!! a single dash \c - and containing a single character which can be
!! any ASCII character except the dash itself.
!!
!! Options may require an argument, which can be integer, real, double
!! precision or character, in that case the argument may be given in
!! the following way (long and short options):
!!
!!  - <tt>--lon=34.5</tt>
!!  - <tt>--lon 34.5</tt>
!!  - <tt>-l34.5</tt>
!!  - <tt>-l 34.5</tt>
!!
!! Grouping of short options, like \c -xvf is not allowed.  When a
!! double dash \c -- or an argument (which is not an argument to an
!! option) not starting by a dash \c - is encountered, the parsing of
!! optons stops and the management of the remaining arguments
!! (typically a list of files) is left to the calling program.
TYPE optionparser
  PRIVATE
  INTEGER(kind=int_b),POINTER :: usage_msg(:), description_msg(:)
  TYPE(op_option), POINTER :: option(:)
  LOGICAL :: option_allocated, error_cond
END TYPE optionparser

!> This class, to be used in association with the getopt_m::optionparser class,
!! describes a single command-line option.
!! Options can be of the following kinds:
!!
!!  - character (with additional argument)
!!  - integer (with additional argument)
!!  - real (with additional argument)
!!  - double precision (with additional argument)
!!  - logical (without additional argument)
!!  - count (without additional argument)
!!  - help (without additional argument)
!!
!! Each option can be introduced by a short and/or a long option
!! string, see the description of getopt_m::optionparser . Each object
!! of the class has to be instantiated through the generic constructor
!! getopt_m::op_option_new , which is an interface to the specific
!! constructors for the different option types, or with
!! getopt_m::op_option_count_new or getopt_m::op_option_help_new for
!! count or help options respectively.
TYPE op_option
  PRIVATE
  CHARACTER(len=1) :: short_opt
  CHARACTER(len=80) :: long_opt
  INTEGER :: opttype
  LOGICAL :: need_arg
  CHARACTER(len=1),POINTER :: destc
  INTEGER :: destclen
  INTEGER,POINTER :: desti
  REAL,POINTER :: destr
  DOUBLE PRECISION, POINTER :: destd
  LOGICAL,POINTER :: destl
  INTEGER,POINTER :: destcount
  INTEGER(kind=int_b),POINTER :: help_msg(:)
END TYPE op_option

INTEGER, PARAMETER, PRIVATE :: opttype_c = 1, opttype_i = 2, opttype_r = 3, &
 opttype_d = 4, opttype_l = 5, opttype_count = 6, opttype_help = 7
PRIVATE op_option_new_common, op_option_found, &
 op_optionc_new, op_optioni_new, op_optionr_new, op_optiond_new, op_optionl_new, &
 op_option_c_e

contains

!> Fortran 95 getopt similar to getopt() and getopt_long() in standard C library.
!! ch = getopt( optstring, [longopts] )
!! Returns next option character from command line arguments.
!! If an option is not recognized, it returns '?'.
!! If no options are left, it returns a null character, char(0).
!!
!! The \a optstring argument contains characters that are recognized as options.
!! If a character is followed by a colon, then it takes a required argument.
!! For example, "x" recognizes "-x", while "x:" recognizes "-x arg" or "-xarg".
!!
!! Global variable \a optopt is set to the option character, even if it isn't
!! recognized. Global variable \a optarg is set to the option's argument.
!! Global variable \a optind has the index of the next argument to process,
!! initially optind=1.
!!
!! Grouped options are allowed, so "-abc" is the same as "-a -b -c".
!!
!! If the optional argument \a longopts is present, it is an array of
!! type(option_s), where each entry describes a long option accepted.
!!
!! Differences from C version:
!! - when options are finished, C version returns -1 instead of char(0),
!!   and thus stupidly requires an int instead of a char.
!! - does not support optreset
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
character(len=*) , intent(in) :: optstring !< characters that are recognized as option
type(option_s) ,   intent(in), optional :: longopts(:) !< if present, it is an array where each entry describes one long option.

! local variables
character(len=80):: arg
integer :: iargc

optarg = ''
if ( optind > iargc()) then
  getopt = char(0)
endif

call getarg( optind, arg )
if (arg == '--') then ! end of options
  optind = optind + 1
  getopt = char(0)
elseif ( present( longopts ) .and. arg(1:2) == '--' ) then
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
INTEGER:: i, iargc

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
      else
        CALL l4f_log(L4F_ERROR, &
         'in getopt, option '''//TRIM(arg)//''' requires an argument')
        CALL raise_error()
      endif
    endif
    return
  endif
end do
! else not found
process_long = '?'
CALL l4f_log(L4F_ERROR, 'in getopt, unrecognized option '''//TRIM(arg)//'''')
CALL raise_error()

end function process_long


! ----------------------------------------
character function process_short( optstring, arg )
! arguments
character(len=*), intent(in):: optstring, arg

! local variables
INTEGER:: i, arglen, iargc

arglen = len_trim( arg )
optopt = arg(grpind:grpind)
process_short = optopt

i = index( optstring, optopt )
if ( i == 0 ) then
! unrecognized option
  process_short = '?'
  CALL l4f_log(L4F_ERROR, 'in getopt, unrecognized option ''-'//optopt//'''')
  CALL raise_error()
endif

IF ( i > 0 .AND. optstring(MIN(i+1,LEN(optstring)):MIN(i+1,LEN(optstring))) == ':' ) THEN
! required argument
  optind = optind + 1
  if ( arglen > grpind ) then
! -xarg, return remainder of arg
    optarg = arg(grpind+1:arglen)
  elseif ( optind <= iargc()) then
! -x arg, return next arg
    call getarg( optind, optarg )
    optind = optind + 1
  else
    CALL l4f_log(L4F_ERROR, 'in getopt, option ''-'//optopt//''' requires an argument')
    CALL raise_error()
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


!> Create a new option with a character type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination, truncating it if it is too long. An
!! optional default value can be provided for the destination. Please
!! use the generic \a op_option_new constructor rather than this
!! particular function.
FUNCTION op_optionc_new(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
CHARACTER(len=*),TARGET :: dest !< the destination of the option parse result
CHARACTER(len=*),OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this
CHARACTER(LEN=40) :: cdefault

IF (PRESENT(default)) THEN
  cdefault = ' [default='//TRIM(default)//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
this = op_option_new_common(short_opt, long_opt, cdefault, help)

! this is needed in order to circumvent a bug in gfortran 4.1.2
! in future replace with following line and erase dirty_char_pointer_set
CALL dirty_char_pointer_set(this%destc, dest(1:1))
!this%destc => dest!(1:1)
this%destclen = LEN(dest) ! needed to avoid exceeding the length of dest
IF (PRESENT(default)) &
 CALL dirty_char_assignment(this%destc, this%destclen, default, LEN(default))
!IF (PRESENT(default)) this%destc(1:this%destclen) = default
this%opttype = opttype_c
this%need_arg = .TRUE.

END FUNCTION op_optionc_new


!> Create a new option with an integer type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. An optional default value can be
!! provided for the destination. Please use the generic \a
!! op_option_new constructor rather than this particular function.
FUNCTION op_optioni_new(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
INTEGER,TARGET :: dest !< the destination of the option parse result
INTEGER,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this
CHARACTER(LEN=40) :: cdefault

IF (PRESENT(default)) THEN
  cdefault = ' [default='//TRIM(to_char(default))//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
this = op_option_new_common(short_opt, long_opt, cdefault, help)

this%desti => dest
IF (PRESENT(default)) this%desti = default
this%opttype = opttype_i
this%need_arg = .TRUE.

END FUNCTION op_optioni_new


!> Create a new option with a real type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. An optional value default can be
!! provided for the destination. Please use the generic \a
!! op_option_new constructor rather than this particular function.
FUNCTION op_optionr_new(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
REAL,TARGET :: dest !< the destination of the option parse result
REAL,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this
CHARACTER(LEN=40) :: cdefault

IF (PRESENT(default)) THEN
  cdefault = ' [default='//TRIM(to_char(default))//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
this = op_option_new_common(short_opt, long_opt, cdefault, help)

this%destr => dest
IF (PRESENT(default)) this%destr = default
this%opttype = opttype_r
this%need_arg = .TRUE.

END FUNCTION op_optionr_new


!> Create a new option with a double precision type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. An optional default value can be
!! provided for the destination. Please use the generic \a
!! op_option_new constructor rather than this particular function.
FUNCTION op_optiond_new(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
DOUBLE PRECISION,TARGET :: dest !< the destination of the option parse result
DOUBLE PRECISION,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this
CHARACTER(LEN=40) :: cdefault

IF (PRESENT(default)) THEN
  cdefault = ' [default='//TRIM(align_left(to_char(default,'(G15.9)')))//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
this = op_option_new_common(short_opt, long_opt, cdefault, help)

this%destd => dest
IF (PRESENT(default)) this%destd = default
this%opttype = opttype_d
this%need_arg = .TRUE.

END FUNCTION op_optiond_new


!> Create a new logical option, without optional argument.
!! When parsing will be performed, if the requested option is
!! encountered, the provided destination will be set to \a
!! .TRUE. . The provided destination is initially set to \a
!! .FALSE. . Please use the generic \a op_option_new constructor
!! rather than this particular function.
FUNCTION op_optionl_new(short_opt, long_opt, dest, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
LOGICAL,TARGET :: dest !< the destination of the option parse result
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this

! common initialisation
this = op_option_new_common(short_opt, long_opt, '', help)

this%destl => dest
this%destl = .FALSE. ! unconditionally set to false, option can only set it to true
this%opttype = opttype_l
this%need_arg = .FALSE.

END FUNCTION op_optionl_new


!> Create a new counter option, without optional argument.
!! When parsing will be performed, the provided destination will be
!! incremented by one, starting from \a start, each time the
!! requested option is encountered.
FUNCTION op_option_count_new(short_opt, long_opt, dest, start, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
INTEGER,TARGET :: dest !< the destination of the option parse result
INTEGER,OPTIONAL :: start !< initial value for \a dest
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this

! common initialisation
this = op_option_new_common(short_opt, long_opt, '', help)

this%destcount => dest
IF (PRESENT(start)) this%destcount = start
this%opttype = opttype_count
this%need_arg = .FALSE.

END FUNCTION op_option_count_new


!> Create a new help option, without optional argument.
!! When parsing will be performed, the full help message will be
!! printed if this option is encountered.
FUNCTION op_option_help_new(short_opt, long_opt, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this

! common initialisation
this = op_option_new_common(short_opt, long_opt, '', help)

this%opttype = opttype_help
this%need_arg = .FALSE.

END FUNCTION op_option_help_new


! private function
FUNCTION op_option_new_common(short_opt, long_opt, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt
CHARACTER(len=*),INTENT(in) :: long_opt
CHARACTER(len=*) :: default
CHARACTER(len=*),OPTIONAL :: help

TYPE(op_option) :: this

INTEGER(kind=int_b) :: dummy(1)

IF (short_opt == '' .AND. long_opt == '') THEN
! programmer error condition, option empty
  CALL l4f_log(L4F_ERROR, 'in op_option, both short and log options empty')
  CALL raise_error()
ENDIF
this%short_opt = short_opt
this%long_opt = long_opt
this%opttype = -1
this%need_arg = .FALSE.
IF (PRESENT(help)) THEN
  ALLOCATE(this%help_msg(LEN_TRIM(help) + LEN_TRIM(default) + 1))
  this%help_msg = fchar_to_cstr(TRIM(help)//TRIM(default))
ELSE
  NULLIFY(this%help_msg)
ENDIF
NULLIFY(this%destc)
NULLIFY(this%desti)
NULLIFY(this%destr)
NULLIFY(this%destd)
NULLIFY(this%destl)
NULLIFY(this%destcount)

END FUNCTION op_option_new_common


!> Destructor for the \a op_option class, the memory associated with
!! the object is freed.
SUBROUTINE op_option_delete(this)
TYPE(op_option),INTENT(out) :: this !< object to destroy

IF (ASSOCIATED(this%help_msg)) DEALLOCATE(this%help_msg)
NULLIFY(this%destc)
NULLIFY(this%desti)
NULLIFY(this%destr)
NULLIFY(this%destd)
NULLIFY(this%destl)
NULLIFY(this%destcount)

END SUBROUTINE op_option_delete


FUNCTION op_option_found(this, optarg) RESULT(err)
TYPE(op_option),INTENT(inout) :: this
CHARACTER(len=*),INTENT(in),OPTIONAL :: optarg

LOGICAL :: err

err = .FALSE.

SELECT CASE(this%opttype)
CASE(opttype_c)
  CALL dirty_char_assignment(this%destc, this%destclen, optarg, LEN_TRIM(optarg))
!  this%destc(1:this%destclen) = optarg
  IF (LEN_TRIM(optarg) > this%destclen) THEN
    CALL l4f_log(L4F_WARN, &
     'in op_option, argument '''//TRIM(optarg)//''' too long, truncated')
  ENDIF
CASE(opttype_i)
  READ(optarg,'(I12)',ERR=100)this%desti
CASE(opttype_r)
  READ(optarg,'(F20.0)',ERR=102)this%destr
CASE(opttype_d)
  READ(optarg,'(F20.0)',ERR=102)this%destd
CASE(opttype_l)
  this%destl = .TRUE.
CASE(opttype_count)
  this%destcount = this%destcount + 1
CASE(opttype_help)
  err = .TRUE.
END SELECT

RETURN

100 err = .TRUE.
CALL l4f_log(L4F_ERROR, &
 'in op_option, argument '''//TRIM(optarg)//''' not valid as integer')
RETURN
102 err = .TRUE.
CALL l4f_log(L4F_ERROR, &
 'in op_option, argument '''//TRIM(optarg)//''' not valid as real')
RETURN

END FUNCTION op_option_found


!> Return a string which gives a short representation of the
!! option \a this, without help message. The resulting string is quite
!! long and it should be trimmed with the \a TRIM() intrinsic
!! function.
FUNCTION op_option_format_opt(this) RESULT(format_opt)
TYPE(op_option),INTENT(inout) :: this

CHARACTER(len=100) :: format_opt

CHARACTER(len=10) :: argname

SELECT CASE(this%opttype)
CASE(opttype_c)
  argname = 'STRING'
CASE(opttype_i)
  argname = 'INT'
CASE(opttype_r, opttype_d)
  argname = 'REAL'
CASE default
  argname = ''
END SELECT

format_opt = ''
IF (this%short_opt /= '') THEN
  format_opt(LEN_TRIM(format_opt)+1:) = ' -'//this%short_opt
  IF (argname /= '') THEN
    format_opt(LEN_TRIM(format_opt)+1:) = ' '//argname
  ENDIF
ENDIF
IF (this%short_opt /= '' .AND. this%long_opt /= '') THEN
  format_opt(LEN_TRIM(format_opt)+1:) = ','
ENDIF
IF (this%long_opt /= '') THEN
  format_opt(LEN_TRIM(format_opt)+1:) = ' --'//this%long_opt
  IF (argname /= '') THEN
    format_opt(LEN_TRIM(format_opt)+1:) = '='//argname
  ENDIF
ENDIF  

END FUNCTION op_option_format_opt


FUNCTION op_option_c_e(this) RESULT(c_e)
TYPE(op_option),INTENT(in) :: this

LOGICAL :: c_e

c_e = this%long_opt /= ' ' .OR. this%short_opt /= ' '

END FUNCTION op_option_c_e


ELEMENTAL SUBROUTINE op_option_nullify(this)
TYPE(op_option),INTENT(inout) :: this

this%long_opt = ''
this%short_opt = ''
NULLIFY(this%help_msg)

END SUBROUTINE op_option_nullify


!> Create a new instance of an optionparser object. An array of \a op_option
!! objects, must be provided, describing the set of command-line
!! options recognized by the program. Additional help messages can be
!! provided.
!!
!! The \a option array must be allocated, either statically or
!! dinamically, by the calling program with the correct size (the
!! number of different options recognized), and each of its elements
!! has to be initialised, either before or after calling
!! ::optionparser_new, using one of the op_option*_new functions.
FUNCTION optionparser_new(option, usage_msg, description_msg) RESULT(this)
TYPE(op_option),TARGET :: option(:)
CHARACTER(len=*), INTENT(in), OPTIONAL :: usage_msg !< short help message which describes the program usage, if not provided, a standard message will be printed
CHARACTER(len=*), INTENT(in), OPTIONAL :: description_msg !< long help message which describes the program purpose, if not provided, nothing will be printed

TYPE(optionparser) :: this

IF (PRESENT(usage_msg)) THEN
  CALL fchar_to_cstr_alloc(TRIM(usage_msg), this%usage_msg)
ELSE
  NULLIFY(this%usage_msg)
ENDIF
IF (PRESENT(description_msg)) THEN
  CALL fchar_to_cstr_alloc(TRIM(description_msg), this%description_msg)
ELSE
  NULLIFY(this%description_msg)
ENDIF
!IF (PRESENT(option)) this%option => option
this%option => option
this%option_allocated = .FALSE.
this%error_cond = .FALSE.

END FUNCTION optionparser_new


!> Destroy the optionparser object freeing all the associated memory.
!! The destructor for each \a op_option object associated with the
!! optionparser object \a this is called as well, while the allocation
!! status of the option array is not modified.
SUBROUTINE optionparser_delete(this)
TYPE(optionparser),INTENT(inout) :: this !< object to destroy

INTEGER :: i

IF (ASSOCIATED(this%option)) THEN
  DO i = 1, SIZE(this%option)
    CALL delete(this%option(i))
  ENDDO
ENDIF
IF (ASSOCIATED(this%usage_msg)) DEALLOCATE(this%usage_msg)
IF (ASSOCIATED(this%description_msg)) DEALLOCATE(this%description_msg)
IF (this%option_allocated) DEALLOCATE(this%option)

END SUBROUTINE optionparser_delete


!> This method performs the parsing of the command-line options
!! which have been previously described when instantiating the
!! optionparser object \a this. The destination variables are assigned
!! according to the options encountered on the command line.  The
!! return value is the index of the first optional argument after
!! interpretation of all command-line options.
FUNCTION optionparser_parseoptions(this) RESULT(nextarg)
TYPE(optionparser),INTENT(inout) :: this !< optionparser object with correctly initialised options

INTEGER :: nextarg

INTEGER :: i, j, endopt, indeq, iargc
CHARACTER(len=1024) :: arg, optarg

i = 1
DO WHILE(i <= iargc())
  CALL getarg(i, arg)
  IF (arg == '--') THEN ! end of options
    nextarg = i + 1
    RETURN
  ELSE IF (arg(1:2) == '--') THEN ! long option
    indeq = INDEX(arg, '=')
    IF (indeq /= 0) THEN ! = present
      endopt = indeq - 1
    ELSE ! no =
      endopt = LEN_TRIM(arg)
    ENDIF
    find_longopt: DO j = 1, SIZE(this%option)
      IF (.NOT. c_e(this%option(j))) CYCLE find_longopt
      IF (this%option(j)%long_opt == arg(3:endopt)) THEN ! found option
        IF (this%option(j)%need_arg) THEN
          IF (indeq /= 0) THEN
            optarg = arg(indeq+1:)
          ELSE
            i=i+1
            CALL getarg(i, optarg)
          ENDIF
          this%error_cond = op_option_found(this%option(j), optarg)
        ELSE
          this%error_cond = op_option_found(this%option(j))
        ENDIF
        EXIT find_longopt
      ENDIF
    ENDDO find_longopt
    IF (j > SIZE(this%option)) THEN
      this%error_cond = .TRUE.
      CALL l4f_log(L4F_ERROR, &
       'in optionparser, long option '''//TRIM(arg)//''' not valid')
    ENDIF
  ELSE IF (arg(1:1) == '-') THEN ! short option
    find_shortopt: DO j = 1, SIZE(this%option)
      IF (.NOT. c_e(this%option(j))) CYCLE find_shortopt
      IF (this%option(j)%short_opt == arg(2:2)) THEN ! found option
        IF (this%option(j)%need_arg) THEN
          IF (LEN_TRIM(arg) > 2) THEN
            optarg = arg(3:)
          ELSE
            i=i+1
            CALL getarg(i, optarg)
          ENDIF
          this%error_cond = op_option_found(this%option(j), optarg)
        ELSE
          this%error_cond = op_option_found(this%option(j))
        ENDIF
        EXIT find_shortopt
      ENDIF
    ENDDO find_shortopt
    IF (j > SIZE(this%option)) THEN
      this%error_cond = .TRUE.
      CALL l4f_log(L4F_ERROR, &
       'in optionparser, long option '''//TRIM(arg)//''' not valid')
    ENDIF
  ELSE ! end of options
    nextarg = i
    RETURN
  ENDIF
  i = i + 1
  IF (this%error_cond) THEN
    CALL optionparser_printhelp(this)
    nextarg = -1
    RETURN
  ENDIF
  
ENDDO
nextarg = i

END FUNCTION optionparser_parseoptions


!> Print the help message well formatted on stdout. It can be called
!! by the user program and it is called anyway in case of error in the
!! interpretation of the command line.
SUBROUTINE optionparser_printhelp(this)
TYPE(optionparser),INTENT(inout) :: this !< optionparser object with correctly initialised options

INTEGER :: i, j, n, ncols
INTEGER, PARAMETER :: indent = 10
CHARACTER(len=80) :: buf
CHARACTER(len=10) :: argname
TYPE(line_split) :: help_line

ncols = default_columns()

! print usage message
IF (ASSOCIATED(this%usage_msg)) THEN
  help_line = line_split_new(cstr_to_fchar(this%usage_msg), ncols)
  DO j = 1, line_split_get_nlines(help_line)
    WRITE(*,'(A)')line_split_get_line(help_line,j)
  ENDDO
  CALL delete(help_line)
ELSE
  CALL getarg(0, buf)
  WRITE(*,'(A)')'Usage: '//TRIM(buf)//' [options] [arguments]'
ENDIF

! print description message
IF (ASSOCIATED(this%description_msg)) THEN
  WRITE(*,'()')
  help_line = line_split_new(cstr_to_fchar(this%description_msg), ncols)
  DO j = 1, line_split_get_nlines(help_line)
    WRITE(*,'(A)')line_split_get_line(help_line,j)
  ENDDO
  CALL delete(help_line)
ENDIF

WRITE(*,'(/,A)')'Options:'

DO i = 1, SIZE(this%option) ! loop over options
  IF (.NOT. c_e(this%option(i))) CYCLE
! print option brief representation
  WRITE(*,'(A)')TRIM(op_option_format_opt(this%option(i)))
! print option help
  IF (ASSOCIATED(this%option(i)%help_msg)) THEN
    help_line = line_split_new(cstr_to_fchar(this%option(i)%help_msg), ncols-indent)
    DO j = 1, line_split_get_nlines(help_line)
      WRITE(*,'(T10,A)')line_split_get_line(help_line,j)
    ENDDO
    CALL delete(help_line)
  ENDIF
ENDDO

END SUBROUTINE optionparser_printhelp

SUBROUTINE dirty_char_pointer_set(from, to)
CHARACTER(len=1),POINTER :: from
CHARACTER(len=1),TARGET :: to

from => to

END SUBROUTINE dirty_char_pointer_set

end module getopt_m


SUBROUTINE dirty_char_assignment(destc, destclen, src, srclen)
USE kinds
IMPLICIT NONE

INTEGER(kind=int_b) :: destc(*), src(*)
INTEGER :: destclen, srclen

INTEGER :: i

DO i = 1, MIN(destclen, srclen)
  destc(i) = src(i)
ENDDO
DO i = srclen+1, destclen
  destc(i) = ICHAR(' ')
ENDDO

END SUBROUTINE dirty_char_assignment


