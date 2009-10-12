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

!> Fortran 95 getopt() and getopt_long(), similar to those in standard C library.
!! This module provides the getopt function, scans the command line
!! and returns the options and possible optional arguments matching
!! those requested.
!! The scanning stops when a non option argument (i.e. not starting with \c -)
!! or the special argument \c -- is found, so that the remaining arguments
!! (through \a optind) can be treated as a list of files or operands for the program.
!! \include example_getopt.f90
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

!INTEGER :: iargc

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
  MODULE PROCEDURE op_option_newc, op_option_newi, op_option_newr, op_option_newd, &
   op_option_newl !, op_option_newcount
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE optionparser_delete, op_option_delete
END INTERFACE

TYPE optionparser
  PRIVATE
  INTEGER(kind=int_b),POINTER :: usage_msg(:), description_msg(:)
  TYPE(op_option), POINTER :: option(:)
  LOGICAL :: option_allocated
END TYPE optionparser

TYPE op_option
  PRIVATE
  CHARACTER(len=1) :: short_opt
  CHARACTER(len=80) :: long_opt
  LOGICAL :: need_arg
  CHARACTER(len=1024),POINTER :: destc
  INTEGER :: destclen
  INTEGER,POINTER :: desti
  REAL,POINTER :: destr
  DOUBLE PRECISION, POINTER :: destd
  LOGICAL,POINTER :: destl
  INTEGER,POINTER :: destcount
  INTEGER(kind=int_b),POINTER :: help_msg(:)
END TYPE op_option

PRIVATE op_option_new_common, op_option_found

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

FUNCTION op_option_newc(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
CHARACTER(len=*),TARGET :: dest !< the destination of the option parse result
CHARACTER(len=*),OPTIONAL :: default !< the default value to give to dest if no option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this

! common initialisation
this = op_option_new_common(short_opt, long_opt, help)

this%destc => dest
this%destclen = LEN(dest) ! needed to avoid exceeding the length of dest
IF (PRESENT(default)) this%destc(1:this%destclen) = default
this%need_arg = .TRUE.

END FUNCTION op_option_newc


FUNCTION op_option_newi(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
INTEGER,TARGET :: dest !< the destination of the option parse result
INTEGER,OPTIONAL :: default !< the default value to give to dest if no option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this

! common initialisation
this = op_option_new_common(short_opt, long_opt, help)

this%desti => dest
IF (PRESENT(default)) this%desti = default
this%need_arg = .TRUE.

END FUNCTION op_option_newi


FUNCTION op_option_newr(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
REAL,TARGET :: dest !< the destination of the option parse result
REAL,OPTIONAL :: default !< the default value to give to dest if no option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this

! common initialisation
this = op_option_new_common(short_opt, long_opt, help)

this%destr => dest
IF (PRESENT(default)) this%destr = default
this%need_arg = .TRUE.

END FUNCTION op_option_newr


FUNCTION op_option_newd(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
DOUBLE PRECISION,TARGET :: dest !< the destination of the option parse result
DOUBLE PRECISION,OPTIONAL :: default !< the default value to give to dest if no option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this

! common initialisation
this = op_option_new_common(short_opt, long_opt, help)

this%destd => dest
IF (PRESENT(default)) this%destd = default
this%need_arg = .TRUE.

END FUNCTION op_option_newd


FUNCTION op_option_newl(short_opt, long_opt, dest, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
LOGICAL,TARGET :: dest !< the destination of the option parse result
LOGICAL,OPTIONAL :: default !< the default value to give to dest if no option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

TYPE(op_option) :: this

! common initialisation
this = op_option_new_common(short_opt, long_opt, help)

this%destl => dest
IF (PRESENT(default)) this%destl = default
this%need_arg = .FALSE.

END FUNCTION op_option_newl


!FUNCTION op_option_newcount(short_opt, long_opt, dest, default, help) RESULT(this)
!CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
!CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
!INTEGER,TARGET :: dest !< the destination of the option parse result
!INTEGER,OPTIONAL :: default !< the default value to give to dest if no option is not found
!CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen
!
!TYPE(op_option) :: this
!
!! common initialisation
!this = op_option_new_common(short_opt, long_opt, help)
!
!this%destcount => dest
!IF (PRESENT(default)) this%destcount = default
!this%need_arg = .FALSE.
!NULLIFY(this%desti)
!NULLIFY(this%destr)
!NULLIFY(this%destd)
!NULLIFY(this%destl)
!
!END FUNCTION op_option_newcount


FUNCTION op_option_new_common(short_opt, long_opt, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt
CHARACTER(len=*),INTENT(in) :: long_opt
CHARACTER(len=*),OPTIONAL :: help

TYPE(op_option) :: this

INTEGER(kind=int_b) :: dummy(1)

IF (short_opt == '' .AND. long_opt == '') THEN
  !errore
ENDIF
this%short_opt = short_opt
this%long_opt = long_opt
IF (PRESENT(help)) THEN
  ALLOCATE(this%help_msg(LEN_TRIM(help) + 1))
  this%help_msg = fchar_to_cstr(TRIM(help))
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


SUBROUTINE op_option_delete(this)
TYPE(op_option),INTENT(out) :: this

IF (ASSOCIATED(this%help_msg)) DEALLOCATE(this%help_msg)
NULLIFY(this%destc)
NULLIFY(this%desti)
NULLIFY(this%destr)
NULLIFY(this%destd)
NULLIFY(this%destl)
NULLIFY(this%destcount)

END SUBROUTINE op_option_delete


SUBROUTINE op_option_found(this, optarg)
TYPE(op_option),INTENT(inout) :: this
CHARACTER(len=*),INTENT(in),OPTIONAL :: optarg


IF (this%need_arg .AND. PRESENT(optarg)) THEN
  IF (ASSOCIATED(this%destc)) THEN
    this%destc(1:this%destclen) = optarg
  ELSE IF (ASSOCIATED(this%desti)) THEN
    READ(optarg,'(I12)',ERR=100)this%desti
  ELSE IF (ASSOCIATED(this%destr)) THEN
    READ(optarg,'(F20.0)',ERR=100)this%destr
  ELSE IF (ASSOCIATED(this%destd)) THEN
    READ(optarg,'(F20.0)',ERR=100)this%destd
  ENDIF
ELSE IF (ASSOCIATED(this%destl)) THEN
  this%destl = .TRUE.
ELSE IF (ASSOCIATED(this%destcount)) THEN
  this%destcount = this%destcount + 1
ENDIF
RETURN

100 CONTINUE ! error condition 

END SUBROUTINE op_option_found


FUNCTION optionparser_new(option) RESULT(this)
TYPE(op_option),TARGET,OPTIONAL :: option(:)

TYPE(optionparser) :: this

NULLIFY(this%usage_msg, this%description_msg)
IF (PRESENT(option)) this%option => option
this%option_allocated = .FALSE.

END FUNCTION optionparser_new


SUBROUTINE optionparser_delete(this)
TYPE(optionparser),INTENT(inout) :: this

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


FUNCTION optionparser_parseoptions(this) RESULT(nextarg)
TYPE(optionparser),INTENT(inout) :: this

INTEGER :: nextarg

INTEGER :: i, j, endopt, indeq
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
      IF (this%option(j)%long_opt == arg(3:endopt)) THEN ! found option
        IF (this%option(j)%need_arg) THEN
          IF (indeq /= 0) THEN
            optarg = arg(indeq+1:)
          ELSE
            i=i+1
            CALL getarg(i, optarg)
          ENDIF
          CALL op_option_found(this%option(j), optarg)
        ELSE
          CALL op_option_found(this%option(j))
        ENDIF
        EXIT find_longopt
      ENDIF
    ENDDO find_longopt
  ELSE IF (arg(1:1) == '-') THEN ! short option
    find_shortopt: DO j = 1, SIZE(this%option)
      IF (this%option(j)%short_opt == arg(2:2)) THEN ! found option
        IF (this%option(j)%need_arg) THEN
          IF (LEN_TRIM(arg) > 2) THEN
            optarg = arg(3:)
          ELSE
            i=i+1
            CALL getarg(i, optarg)
          ENDIF
          CALL op_option_found(this%option(j), optarg)
        ELSE
          CALL op_option_found(this%option(j))
        ENDIF
        EXIT find_shortopt
      ENDIF
    ENDDO find_shortopt
  ELSE ! end of options
    nextarg = i
    RETURN
  ENDIF
  i = i + 1
ENDDO
nextarg = i

END FUNCTION optionparser_parseoptions


SUBROUTINE optionparser_printhelp(this)
TYPE(optionparser),INTENT(inout) :: this

INTEGER :: i, j, n, ncols
CHARACTER(len=80) :: buf
character(len=10) :: argname
TYPE(line_split) :: help_line

ncols = default_columns()
CALL getarg(0, buf)

WRITE(*,'(A)')'Usage: '//TRIM(buf)//' [options] [arguments]'
WRITE(*,'(A)')'Where [options] can be any of:'

DO i = 1, SIZE(this%option)
  WRITE(*,'()')
  IF (this%option(i)%need_arg) THEN
    IF (ASSOCIATED(this%option(i)%destc)) THEN
      argname = 'STRING'
    ELSE IF (ASSOCIATED(this%option(i)%desti)) THEN
      argname = 'INT'
    ELSE IF (ASSOCIATED(this%option(i)%destr) .OR. &
     ASSOCIATED(this%option(i)%destd)) THEN
      argname = 'REAL'
    ELSE
      argname = 'ARG'
    ENDIF
    WRITE(*,'(''-'',A,'' '',A,'', --'',A,''='',A)') &
     this%option(i)%short_opt,TRIM(argname), &
     TRIM(this%option(i)%long_opt),TRIM(argname)
  ELSE
    WRITE(*,'(''-'',A,'', --'',A)') &
     this%option(i)%short_opt,TRIM(this%option(i)%long_opt)
  ENDIF
  IF (ASSOCIATED(this%option(i)%help_msg)) THEN
    help_line = line_split_new(cstr_to_fchar(this%option(i)%help_msg), ncols-10)
    DO j = 1, line_split_get_nlines(help_line)
      WRITE(*,'(T10,A)')line_split_get_line(help_line,j)
    ENDDO
  ENDIF
ENDDO

END SUBROUTINE optionparser_printhelp

end module getopt_m

