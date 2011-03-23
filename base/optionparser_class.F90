! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!> Module for parsing command-line optons.
!! This module defines a class for parsing command-line arguments and
!! generating help messages similar to the one found in the Python
!! library.
!!
!! This is an example of use:
!! \include example_optionparser.F90
!!
!! \ingroup base
MODULE optionparser_class
USE log4fortran
USE err_handling
USE kinds
USE char_utilities
IMPLICIT NONE


! private class
TYPE option
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
END TYPE option

#define ARRAYOF_ORIGTYPE TYPE(option)
#define ARRAYOF_TYPE arrayof_option
#define ARRAYOF_ORIGDESTRUCTOR(x) CALL option_delete(x)
#include "arrayof_pre_nodoc.F90"
! from arrayof
!PUBLIC insert, append, remove, packarray
!PUBLIC insert_unique, append_unique

!> This class allows to parse the command-line options of a program in
!! an object-oriented way, similarly to the optparse class found in
!! Python library.
!!
!! The class handles both GNU-style long options, introduced by a
!! double dash \c -- and containing any character except the equal
!! sign \c = , and the traditional Unix short options, introduced by
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
!! options stops and the management of the remaining arguments
!! (typically a list of files) is left to the calling program.
!!
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
!! Options are added through the generic optionparser_add method (for
!! character, integer, floating point or logical options) or through
!! the specific methods optionparser_add_count, optionparser_add_help
!! (for count and help options).
!!
!! The effect of command line parsing is to set some desired variables
!! according to the information provided on the command line.
TYPE optionparser
  PRIVATE
  INTEGER(kind=int_b),POINTER :: usage_msg(:), description_msg(:)
  TYPE(arrayof_option) :: options
END TYPE optionparser


!> Add a new option of a specific type. Although the generic name
!! optionparser_add should be used, refer to the members of the interface
!! for a detailed documentation.
INTERFACE optionparser_add
  MODULE PROCEDURE optionparser_add_c, optionparser_add_i, optionparser_add_r, &
   optionparser_add_d, optionparser_add_l!?, optionparser_add_count
END INTERFACE

INTERFACE c_e
  MODULE PROCEDURE option_c_e
END INTERFACE

!> Destructor for the optionparser class.
!! It destroys the \a optionparser object freeing all the associated
!! memory.  The values assigned through command line parsing are
!! conserved after deleting the \a optionparser object, but it is not
!! possible to show the help with the optionparser_printhelp method.
!!
!! \param this TYPE(optionparser) object to be destroyed
INTERFACE delete
  MODULE PROCEDURE optionparser_delete!?, option_delete
END INTERFACE


INTEGER,PARAMETER :: opttype_c = 1, opttype_i = 2, opttype_r = 3, &
 opttype_d = 4, opttype_l = 5, opttype_count = 6, opttype_help = 7

INTEGER,PARAMETER :: optionparser_ok = 0 !< constants indicating the status returned by optionparser_parse, status of parsing: OK
INTEGER,PARAMETER :: optionparser_help = 1 !< status of parsing: help has been requested
INTEGER,PARAMETER :: optionparser_err = 2 !< status of parsing: an error was encountered

PRIVATE
PUBLIC optionparser, optionparser_new, delete, optionparser_add, &
 optionparser_add_count, optionparser_add_help, &
 optionparser_parse, optionparser_printhelp, &
 optionparser_ok, optionparser_err, optionparser_help


CONTAINS

#include "arrayof_post_nodoc.F90"

! Constructor for the option class
FUNCTION option_new(short_opt, long_opt, default, help) RESULT(this)
CHARACTER(len=1),INTENT(in) :: short_opt
CHARACTER(len=*),INTENT(in) :: long_opt
CHARACTER(len=*) :: default
CHARACTER(len=*),OPTIONAL :: help
TYPE(option) :: this

this%short_opt = short_opt
this%long_opt = long_opt
this%opttype = -1
this%need_arg = .FALSE.
NULLIFY(this%help_msg)
NULLIFY(this%destc)
NULLIFY(this%desti)
NULLIFY(this%destr)
NULLIFY(this%destd)
NULLIFY(this%destl)
NULLIFY(this%destcount)

IF (short_opt == '' .AND. long_opt == '') THEN
#ifdef DEBUG
! programmer error condition, option empty
  CALL l4f_log(L4F_ERROR, 'in optionparser, both short and long options empty')
  CALL raise_fatal_error()
#else
  CALL l4f_log(L4F_WARN, 'in optionparser, both short and long options empty')
#endif
  RETURN
ENDIF

IF (PRESENT(help)) THEN
  ALLOCATE(this%help_msg(LEN_TRIM(help) + LEN_TRIM(default) + 1))
  this%help_msg = fchar_to_cstr(TRIM(help)//TRIM(default))
ENDIF

END FUNCTION option_new


! Destructor for the \a option class, the memory associated with
! the object is freed.
SUBROUTINE option_delete(this)
TYPE(option),INTENT(out) :: this ! object to destroy

IF (ASSOCIATED(this%help_msg)) DEALLOCATE(this%help_msg)
NULLIFY(this%destc)
NULLIFY(this%desti)
NULLIFY(this%destr)
NULLIFY(this%destd)
NULLIFY(this%destl)
NULLIFY(this%destcount)

END SUBROUTINE option_delete


FUNCTION option_found(this, optarg) RESULT(status)
TYPE(option),INTENT(inout) :: this
CHARACTER(len=*),INTENT(in),OPTIONAL :: optarg

INTEGER :: status

status = optionparser_ok

SELECT CASE(this%opttype)
CASE(opttype_c)
  CALL dirty_char_assignment(this%destc, this%destclen, optarg, LEN_TRIM(optarg))
!  this%destc(1:this%destclen) = optarg
  IF (LEN_TRIM(optarg) > this%destclen) THEN
    CALL l4f_log(L4F_WARN, &
     'in optionparser, argument '''//TRIM(optarg)//''' too long, truncated')
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
  status = optionparser_help
END SELECT

RETURN

100 status = optionparser_err
CALL l4f_log(L4F_ERROR, &
 'in optionparser, argument '''//TRIM(optarg)//''' not valid as integer')
RETURN
102 status = optionparser_err
CALL l4f_log(L4F_ERROR, &
 'in optionparser, argument '''//TRIM(optarg)//''' not valid as real')
RETURN

END FUNCTION option_found


! Return a string which gives a short representation of the
! option \a this, without help message. The resulting string is quite
! long and it should be trimmed with the \a TRIM() intrinsic
! function.
FUNCTION option_format_opt(this) RESULT(format_opt)
TYPE(option),INTENT(inout) :: this

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

END FUNCTION option_format_opt


FUNCTION option_c_e(this) RESULT(c_e)
TYPE(option),INTENT(in) :: this

LOGICAL :: c_e

c_e = this%long_opt /= ' ' .OR. this%short_opt /= ' '

END FUNCTION option_c_e


!> Create a new instance of an optionparser object. 
!! General usage and description messages can be optionally provided,
!! the options will be added later.
FUNCTION optionparser_new(usage_msg, description_msg) RESULT(this)
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
this%options = arrayof_option_new()

END FUNCTION optionparser_new


SUBROUTINE optionparser_delete(this)
TYPE(optionparser),INTENT(inout) :: this

IF (ASSOCIATED(this%usage_msg)) DEALLOCATE(this%usage_msg)
IF (ASSOCIATED(this%description_msg)) DEALLOCATE(this%description_msg)
CALL delete(this%options)

END SUBROUTINE optionparser_delete


!> Add a new option with a character type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination, truncating it if it is too long. An
!! optional default value can be provided for the destination. Please
!! use the generic \a optionparser_add method rather than this
!! particular method.
SUBROUTINE optionparser_add_c(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
CHARACTER(len=*),TARGET :: dest !< the destination of the option parse result
CHARACTER(len=*),OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=60) :: cdefault
INTEGER :: i
TYPE(option) :: myoption


IF (PRESENT(default)) THEN
  cdefault = ' [default='//TRIM(default)//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

! this is needed in order to circumvent a bug in gfortran 4.1.2
! in future replace with following line and erase dirty_char_pointer_set
CALL dirty_char_pointer_set(myoption%destc, dest(1:1))
!this%destc => dest!(1:1)
myoption%destclen = LEN(dest) ! needed to avoid exceeding the length of dest
IF (PRESENT(default)) &
 CALL dirty_char_assignment(myoption%destc, myoption%destclen, default, LEN(default))
!IF (PRESENT(default)) myoption%destc(1:myoption%destclen) = default
myoption%opttype = opttype_c
myoption%need_arg = .TRUE.

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_c


!> Add a new option with an integer type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. An optional default value can be
!! provided for the destination. Please use the generic \a
!! optionparser_add method rather than this particular method.
SUBROUTINE optionparser_add_i(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
INTEGER,TARGET :: dest !< the destination of the option parse result
INTEGER,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

IF (PRESENT(default)) THEN
  cdefault = ' [default='//TRIM(to_char(default))//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%desti => dest
IF (PRESENT(default)) myoption%desti = default
myoption%opttype = opttype_i
myoption%need_arg = .TRUE.

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_i


!> Add a new option with a real type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. An optional value default can be
!! provided for the destination. Please use the generic \a
!! optionparser_add method rather than this particular method.
SUBROUTINE optionparser_add_r(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
REAL,TARGET :: dest !< the destination of the option parse result
REAL,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

IF (PRESENT(default)) THEN
  cdefault = ' [default='//TRIM(to_char(default))//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destr => dest
IF (PRESENT(default)) myoption%destr = default
myoption%opttype = opttype_r
myoption%need_arg = .TRUE.

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_r


!> Add a new option with a double precision type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. An optional default value can be
!! provided for the destination. Please use the generic \a
!! optionparser_add method rather than this particular method.
SUBROUTINE optionparser_add_d(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
DOUBLE PRECISION,TARGET :: dest !< the destination of the option parse result
DOUBLE PRECISION,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

IF (PRESENT(default)) THEN
  cdefault = ' [default='//TRIM(align_left(to_char(default,'(G15.9)')))//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destd => dest
IF (PRESENT(default)) myoption%destd = default
myoption%opttype = opttype_d
myoption%need_arg = .TRUE.

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_d


!> Add a new logical option, without optional argument.
!! When parsing will be performed, if the requested option is
!! encountered, the provided destination will be set to \a
!! .TRUE. . The provided destination is initially set to \a
!! .FALSE. . Please use the generic \a optionparser_add method
!! rather than this particular method.
SUBROUTINE optionparser_add_l(this, short_opt, long_opt, dest, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
LOGICAL,TARGET :: dest !< the destination of the option parse result
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

INTEGER :: i
TYPE(option) :: myoption

! common initialisation
myoption = option_new(short_opt, long_opt, '', help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destl => dest
myoption%destl = .FALSE. ! unconditionally set to false, option can only set it to true
myoption%opttype = opttype_l
myoption%need_arg = .FALSE.

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_l


!> Add a new counter option, without optional argument.
!! When parsing will be performed, the provided destination will be
!! incremented by one, starting from \a start, each time the requested
!! option is encountered.
SUBROUTINE optionparser_add_count(this, short_opt, long_opt, dest, start, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
INTEGER,TARGET :: dest !< the destination of the option parse result
INTEGER,OPTIONAL :: start !< initial value for \a dest 
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

INTEGER :: i
TYPE(option) :: myoption

! common initialisation
myoption = option_new(short_opt, long_opt, '', help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destcount => dest
IF (PRESENT(start)) myoption%destcount = start
myoption%opttype = opttype_count
myoption%need_arg = .FALSE.

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_count


!> Add a new help option, without optional argument.
!! When parsing will be performed, the full help message will be
!! printed if this option is encountered. The message can be directly
!! printed as well by calling the optparser_printhelp method.
SUBROUTINE optionparser_add_help(this, short_opt, long_opt, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=1),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

INTEGER :: i
TYPE(option) :: myoption

! common initialisation
myoption = option_new(short_opt, long_opt, '', help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%opttype = opttype_help
myoption%need_arg = .FALSE.

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_help


!> This method performs the parsing of the command-line options
!! which have been previously added using the optionparser_add family
!! of methods. The destination variables set through the
!! optionparser_add methods are assigned according to the options
!! encountered on the command line.  If any optional argument remains
!! after interpretation of all command-line options, the index of the
!! first of them is returned in \a nextarg, otherwise \a nextarg is
!! equal to \a iargc() \a + \a 1. The status of the parsing process
!! should be checked via the \a status argument.
SUBROUTINE optionparser_parse(this, nextarg, status)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object with correctly initialised options
INTEGER,INTENT(out) :: nextarg !< index of the first optional argument after interpretation of all command-line options
INTEGER,INTENT(out) :: status !< status of the parsing process, to be compared with the constants \a optionparser_ok, ecc.

INTEGER :: i, j, endopt, indeq, iargc
CHARACTER(len=1024) :: arg, optarg

status = optionparser_ok
i = 1
DO WHILE(i <= iargc())
  CALL getarg(i, arg)
  IF (arg == '--') THEN ! explicit end of options
    i = i + 1 ! skip present option (--)
    EXIT
  ELSE IF (arg == '-') THEN ! a single - is not an option
    EXIT
  ELSE IF (arg(1:2) == '--') THEN ! long option
    indeq = INDEX(arg, '=')
    IF (indeq /= 0) THEN ! = present
      endopt = indeq - 1
    ELSE ! no =
      endopt = LEN_TRIM(arg)
    ENDIF
    find_longopt: DO j = 1, this%options%arraysize
      IF (this%options%array(j)%long_opt == arg(3:endopt)) THEN ! found option
        IF (this%options%array(j)%need_arg) THEN
          IF (indeq /= 0) THEN
            optarg = arg(indeq+1:)
          ELSE
            i=i+1
            CALL getarg(i, optarg)
          ENDIF
          status = MAX(option_found(this%options%array(j), optarg), &
           status)
        ELSE
          status = MAX(option_found(this%options%array(j)), &
           status)
        ENDIF
        EXIT find_longopt
      ENDIF
    ENDDO find_longopt
    IF (j > this%options%arraysize) THEN
      status = optionparser_err
      CALL l4f_log(L4F_ERROR, &
       'in optionparser, option '''//TRIM(arg)//''' not valid')
    ENDIF
  ELSE IF (arg(1:1) == '-') THEN ! short option
    find_shortopt: DO j = 1, this%options%arraysize
      IF (this%options%array(j)%short_opt == arg(2:2)) THEN ! found option
        IF (this%options%array(j)%need_arg) THEN
          IF (LEN_TRIM(arg) > 2) THEN
            optarg = arg(3:)
          ELSE
            i=i+1
            CALL getarg(i, optarg)
          ENDIF
          status = MAX(option_found(this%options%array(j), optarg), &
           status)
        ELSE
          status = MAX(option_found(this%options%array(j)), &
           status)
        ENDIF
        EXIT find_shortopt
      ENDIF
    ENDDO find_shortopt
    IF (j > this%options%arraysize) THEN
      status = optionparser_err
      CALL l4f_log(L4F_ERROR, &
       'in optionparser, option '''//TRIM(arg)//''' not valid')
    ENDIF
  ELSE ! unrecognized = end of options
    EXIT
  ENDIF
  i = i + 1
ENDDO

nextarg = i
IF (status == optionparser_err .OR. status == optionparser_help) THEN
  CALL optionparser_printhelp(this)
ENDIF

END SUBROUTINE optionparser_parse


!> Print the help message well formatted on stdout. It can be called
!! by the user program and it is called anyway in case of error in the
!! interpretation of the command line.
SUBROUTINE optionparser_printhelp(this)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object with correctly initialised options

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

DO i = 1, this%options%arraysize ! loop over options
! print option brief representation
  WRITE(*,'(A)')TRIM(option_format_opt(this%options%array(i)))
! print option help
  IF (ASSOCIATED(this%options%array(i)%help_msg)) THEN
    help_line = line_split_new(cstr_to_fchar(this%options%array(i)%help_msg), ncols-indent)
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

END MODULE optionparser_class


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


