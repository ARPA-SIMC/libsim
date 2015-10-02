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
#include "config.h"

MODULE optionparser_class
USE log4fortran
USE err_handling
USE kinds
USE char_utilities
USE file_utilities
USE array_utilities
IMPLICIT NONE


! private class
TYPE option
  CHARACTER(len=1) :: short_opt=''
  CHARACTER(len=80) :: long_opt=''
  INTEGER :: opttype=-1
  INTEGER :: need_arg=0 ! 0=no, 1=optional, 2=yes, was .FALSE.
  LOGICAL :: has_default=.FALSE.
  CHARACTER(len=1),POINTER :: destc=>NULL()
  INTEGER :: destclen=0
  INTEGER :: helpformat=0 ! 0=txt, 1=markdown, 2=htmlform, improve!
  INTEGER,POINTER :: desti=>NULL()
  TYPE(arrayof_integer),POINTER :: destiarr=>NULL()
  REAL,POINTER :: destr=>NULL()
  TYPE(arrayof_real),POINTER :: destrarr=>NULL()
  DOUBLE PRECISION, POINTER :: destd=>NULL()
  TYPE(arrayof_doubleprecision),POINTER :: destdarr=>NULL()
  LOGICAL,POINTER :: destl=>NULL()
  TYPE(arrayof_logical),POINTER :: destlarr=>NULL()
  INTEGER,POINTER :: destcount=>NULL()
  INTEGER(kind=int_b),POINTER :: help_msg(:)=>NULL()
END TYPE option

#define ARRAYOF_ORIGTYPE TYPE(option)
#define ARRAYOF_TYPE arrayof_option
#define ARRAYOF_ORIGDESTRUCTOR(x) CALL option_delete(x)
#define ARRAYOF_PRIVATE 1
#include "arrayof_pre_nodoc.F90"
! from arrayof
!PUBLIC insert, append, remove, packarray
!PUBLIC insert_unique, append_unique

!> This class allows to parse the command-line options of a program in
!! an object-oriented way, similarly to the optparse class found in
!! Python library.
!!
!! The class handles both GNU-style long options, introduced by a
!! double dash \c -- and containing any printable ASCII character
!! except the equal sign \c = , and the traditional Unix short
!! options, introduced by a single dash \c - and containing a single
!! character which can be any printable ASCII character except the
!! dash itself.
!!
!! Options may require an argument, which can be integer, real, double
!! precision or character, in that case the argument may be given in
!! any of the following ways (long and short options):
!!
!!  - <tt>--lon=34.5</tt>
!!  - <tt>--lon 34.5</tt>
!!  - <tt>-l34.5</tt>
!!  - <tt>-l 34.5</tt>
!!
!! By default, the argument to an option is compulsory, so any
!! following string, even empty or starting with a dash \c - , is
!! interpreted as the argument to the option, while its absence
!! (i.e. end of command line) determines an error condition in the
!! parsing phase. However the argument toi character options can be
!! declared as optional in the corresponding definition method; in
!! those cases the following argument, if any, is interpreted as the
!! argument to the option only if it does not start with a dash \c -
!! (no chance to quote a dash in these cases); if no optional argument
!! is found, then the variable associated to the option is set to the
!! missing value of the corresponding type, without raising an error
!! condition.
!!
!! Array options (only for integer, real and double precision) must be
!! provided as comma-separated values, similarly to a record of a csv
!! file, an empty field generates a missing value of the proper type
!! in the resulting array, the length of the array is not a priori
!! limited.
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
!!  - array of integer (with additional argument)
!!  - array of real (with additional argument)
!!  - array of double precision (with additional argument)
!!  - logical (without additional argument)
!!  - count (without additional argument)
!!  - help (with additional optional argument)
!!
!! If the same option is encountered multiple times on the command
!! line, the value set in the last occurrence takes precedence, the
!! only exception is count options where every repetition increments
!! the corresponding counter by one.
!!
!! Options are added through the generic \a optionparser_add method
!! (for character, integer, floating point or logical options,
!! including array variants) or through the specific methods \a
!! optionparser_add_count, \a optionparser_add_help (for count and
!! help options).
!!
!! The effect of command line parsing is to set some desired variables
!! according to the information provided on the command line.
!!
!! Array options set the values of objects of derived types \a
!! arrayof_integer, \a arrayof_real and \a arrayof_doubleprecision
!! which are dynamically growable 1-d arrays defined in the \a
!! array_utilities module.
TYPE optionparser
  PRIVATE
  INTEGER(kind=int_b),POINTER :: usage_msg(:), description_msg(:)
  TYPE(arrayof_option) :: options
  LOGICAL :: httpmode=.FALSE.
END TYPE optionparser


!> Add a new option of a specific type. Although the generic name
!! optionparser_add should be used, refer to the members of the interface
!! for a detailed documentation.
INTERFACE optionparser_add
  MODULE PROCEDURE optionparser_add_c, optionparser_add_i, optionparser_add_r, &
   optionparser_add_d, optionparser_add_l, &
   optionparser_add_iarray, optionparser_add_rarray, optionparser_add_darray
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
 opttype_d = 4, opttype_l = 5, opttype_count = 6, opttype_help = 7, &
 opttype_carr = 11, opttype_iarr = 12, opttype_rarr = 13, &
 opttype_darr = 14, opttype_larr = 15

INTEGER,PARAMETER :: optionparser_ok = 0 !< constants indicating the status returned by optionparser_parse, status of parsing: OK
INTEGER,PARAMETER :: optionparser_help = 1 !< status of parsing: help has been requested
INTEGER,PARAMETER :: optionparser_err = 2 !< status of parsing: an error was encountered


PRIVATE
PUBLIC optionparser, optionparser_new, delete, optionparser_add, &
 optionparser_add_count, optionparser_add_help, &
 optionparser_parse, optionparser_printhelp, &
 optionparser_ok, optionparser_help, optionparser_err


CONTAINS

#include "arrayof_post_nodoc.F90"

! Constructor for the option class
FUNCTION option_new(short_opt, long_opt, default, help) RESULT(this)
CHARACTER(len=*),INTENT(in) :: short_opt
CHARACTER(len=*),INTENT(in) :: long_opt
CHARACTER(len=*),INTENT(in) :: default
CHARACTER(len=*),OPTIONAL :: help
TYPE(option) :: this

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

this%short_opt = short_opt
this%long_opt = long_opt
IF (PRESENT(help)) THEN
  CALL fchar_to_cstr_alloc(TRIM(help)//TRIM(default), this%help_msg)
ENDIF
this%has_default = (LEN_TRIM(default) > 0)

END FUNCTION option_new


! Destructor for the \a option class, the memory associated with
! the object is freed.
SUBROUTINE option_delete(this)
TYPE(option),INTENT(inout) :: this ! object to destroy

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

TYPE(csv_record) :: arrparser
INTEGER :: ibuff
REAL :: rbuff
DOUBLE PRECISION :: dbuff

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
CASE(opttype_iarr)
  CALL delete(this%destiarr) ! delete default values
  CALL init(arrparser, optarg)
  DO WHILE(.NOT.csv_record_end(arrparser))
    CALL csv_record_getfield(arrparser, ibuff)
    CALL insert(this%destiarr, ibuff)
  ENDDO
  CALL packarray(this%destiarr)
  CALL delete(arrparser)
CASE(opttype_r)
  READ(optarg,'(F20.0)',ERR=102)this%destr
CASE(opttype_rarr)
  CALL delete(this%destrarr) ! delete default values
  CALL init(arrparser, optarg)
  DO WHILE(.NOT.csv_record_end(arrparser))
    CALL csv_record_getfield(arrparser, rbuff)
    CALL insert(this%destrarr, rbuff)
  ENDDO
  CALL packarray(this%destrarr)
  CALL delete(arrparser)
CASE(opttype_d)
  READ(optarg,'(F20.0)',ERR=102)this%destd
CASE(opttype_darr)
  CALL delete(this%destdarr) ! delete default values
  CALL init(arrparser, optarg)
  DO WHILE(.NOT.csv_record_end(arrparser))
    CALL csv_record_getfield(arrparser, dbuff)
    CALL insert(this%destdarr, dbuff)
  ENDDO
  CALL packarray(this%destdarr)
  CALL delete(arrparser)
CASE(opttype_l)
  this%destl = .TRUE.
CASE(opttype_count)
  this%destcount = this%destcount + 1
CASE(opttype_help)
  status = optionparser_help
  SELECT CASE(optarg) ! set help format
  CASE('md', 'markdown')
    this%helpformat = 1
  CASE('htmlform')
    this%helpformat = 2
  END SELECT
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
TYPE(option),INTENT(in) :: this

CHARACTER(len=100) :: format_opt

CHARACTER(len=20) :: argname

SELECT CASE(this%opttype)
CASE(opttype_c)
  argname = 'STRING'
CASE(opttype_i)
  argname = 'INT'
CASE(opttype_iarr)
  argname = 'INT[,INT...]'
CASE(opttype_r, opttype_d)
  argname = 'REAL'
CASE(opttype_rarr, opttype_darr)
  argname = 'REAL[,REAL...]'
CASE default
  argname = ''
END SELECT

format_opt = ''
IF (this%short_opt /= '') THEN
  format_opt(LEN_TRIM(format_opt)+1:) = ' -'//this%short_opt
  IF (argname /= '') THEN
    format_opt(LEN_TRIM(format_opt)+1:) = ' '//TRIM(argname)
  ENDIF
ENDIF
IF (this%short_opt /= '' .AND. this%long_opt /= '') THEN
  format_opt(LEN_TRIM(format_opt)+1:) = ','
ENDIF
IF (this%long_opt /= '') THEN
  format_opt(LEN_TRIM(format_opt)+1:) = ' --'//this%long_opt
  IF (argname /= '') THEN
    format_opt(LEN_TRIM(format_opt)+1:) = '='//TRIM(argname)
  ENDIF
ENDIF

END FUNCTION option_format_opt


! print on stdout a human-readable text representation of a single option
SUBROUTINE option_format_help(this, ncols)
TYPE(option),INTENT(in) :: this
INTEGER,INTENT(in) :: ncols

INTEGER :: j
INTEGER, PARAMETER :: indent = 10
TYPE(line_split) :: help_line

! print option brief representation
WRITE(*,'(A)')TRIM(option_format_opt(this))
! print option help
IF (ASSOCIATED(this%help_msg)) THEN
  help_line = line_split_new(cstr_to_fchar(this%help_msg), ncols-indent)
  DO j = 1, line_split_get_nlines(help_line)
    WRITE(*,'(T10,A)')TRIM(line_split_get_line(help_line,j))
  ENDDO
  CALL delete(help_line)
ENDIF

END SUBROUTINE option_format_help


! print on stdout a markdown representation of a single option
SUBROUTINE option_format_md(this)
TYPE(option),INTENT(in) :: this

! print option brief representation
WRITE(*,'(''`'',A,''`'')')TRIM(option_format_opt(this))
! print option help
IF (ASSOCIATED(this%help_msg)) THEN
  WRITE(*,'(''> '',A,/)')TRIM(cstr_to_fchar(this%help_msg))
ENDIF

END SUBROUTINE option_format_md


! print on stdout an html form representation of a single option
SUBROUTINE option_format_htmlform(this)
TYPE(option),INTENT(in) :: this

CHARACTER(len=80) :: opt_name, opt_id, opt_default ! check len of default

IF (.NOT.c_e(this)) RETURN
IF (this%long_opt == '') THEN
  opt_name = this%short_opt
  opt_id = 'short_opt_'//this%short_opt
ELSE
  opt_name = this%long_opt
  opt_id = this%long_opt
ENDIF

SELECT CASE(this%opttype)
CASE(opttype_c)
  CALL option_format_html_openspan('text')

  IF (this%has_default .AND. ASSOCIATED(this%destc) .AND. this%destclen > 0) THEN
!    opt_default = TRANSFER(this%destc(1:MIN(LEN(opt_default),this%destclen)), &
!     opt_default) ! improve
    opt_default = ''
    WRITE(*,'(A)')' value="'//TRIM(opt_default)//'"'
  ENDIF
  CALL option_format_html_help()
  CALL option_format_html_closespan()

CASE(opttype_i,opttype_r,opttype_d)
  CALL option_format_html_openspan('text')
  IF (this%has_default) THEN
    SELECT CASE(this%opttype)
    CASE(opttype_i)
      WRITE(*,'(3A)')' value="',t2c(this%desti),'"'
! todo    CASE(opttype_iarr)
    CASE(opttype_r)
      WRITE(*,'(3A)')' value="',t2c(this%destr),'"'
    CASE(opttype_d)
      WRITE(*,'(3A)')' value="',t2c(this%destd),'"'
    END SELECT
  ENDIF
  CALL option_format_html_help()
  CALL option_format_html_closespan()

! todo CASE(opttype_iarr)

CASE(opttype_l)
  CALL option_format_html_openspan('checkbox')
  CALL option_format_html_help()
  CALL option_format_html_closespan()

CASE(opttype_count)
END SELECT


CONTAINS

SUBROUTINE option_format_html_openspan(formtype)
CHARACTER(len=*),INTENT(in) :: formtype

WRITE(*,'(A)')'<span id="span_'//TRIM(opt_id)//'">'//TRIM(opt_name)//':'
! size=? maxlen=?
WRITE(*,'(A)')'<input id="'//TRIM(opt_id)//'" type="'//formtype// &
 '" name="'//TRIM(opt_id)//'" '

END SUBROUTINE option_format_html_openspan

SUBROUTINE option_format_html_closespan()

WRITE(*,'(A)')'/></span>'

END SUBROUTINE option_format_html_closespan

SUBROUTINE option_format_html_help()
INTEGER :: j
TYPE(line_split) :: help_line
CHARACTER(len=20) :: form

IF (ASSOCIATED(this%help_msg)) THEN
  WRITE(*,'(A,$)')' title="'

  help_line = line_split_new(cstr_to_fchar(this%help_msg), 80)
  form = '(A,'' '')'
  DO j = 1, line_split_get_nlines(help_line)
    IF (j == line_split_get_nlines(help_line)) form = '(A,''"'',$)'
    WRITE(*,form)TRIM(line_split_get_line(help_line,j)) ! lines should be properly quoted here
  ENDDO

ENDIF

END SUBROUTINE option_format_html_help

END SUBROUTINE option_format_htmlform


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
SUBROUTINE optionparser_add_c(this, short_opt, long_opt, dest, default, help, isopt)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
CHARACTER(len=*),TARGET :: dest !< the destination of the option parse result
CHARACTER(len=*),OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen
LOGICAL,INTENT(in),OPTIONAL :: isopt !< if provided and \c .TRUE. the argument is considered optional

CHARACTER(LEN=60) :: cdefault
INTEGER :: i
TYPE(option) :: myoption


IF (PRESENT(default)) THEN
  cdefault = ' [default='//t2c(default, 'MISSING')//']'
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
IF (optio_log(isopt)) THEN
  myoption%need_arg = 1
ELSE
  myoption%need_arg = 2
ENDIF

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
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
INTEGER,TARGET :: dest !< the destination of the option parse result
INTEGER,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

IF (PRESENT(default)) THEN
  cdefault = ' [default='//t2c(default, 'MISSING')//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%desti => dest
IF (PRESENT(default)) myoption%desti = default
myoption%opttype = opttype_i
myoption%need_arg = 2

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_i


!> Add a new option with an integer type array argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. The argument must be provided in the
!! form of comma-separated list of values and is stored in an object
!! of type arrayof_integer (module \a array_utilities). An optional
!! default value can be provided for the destination. Please use the
!! generic \a optionparser_add method rather than this particular
!! method.
SUBROUTINE optionparser_add_iarray(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
TYPE(arrayof_integer),TARGET :: dest !< the destination of the option parse result
INTEGER,OPTIONAL :: default(:) !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

!IF (PRESENT(default)) THEN
!  cdefault = ' [default='//TRIM(to_char(default))//']'
!ELSE
  cdefault = ''
!ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destiarr => dest
IF (PRESENT(default)) THEN
  CALL insert(myoption%destiarr, default)
  CALL packarray(myoption%destiarr)
ENDIF
myoption%opttype = opttype_iarr
myoption%need_arg = 2

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_iarray


!> Add a new option with a real type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. An optional value default can be
!! provided for the destination. Please use the generic \a
!! optionparser_add method rather than this particular method.
SUBROUTINE optionparser_add_r(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
REAL,TARGET :: dest !< the destination of the option parse result
REAL,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

IF (PRESENT(default)) THEN
  cdefault = ' [default='//t2c(default, 'MISSING')//']'
ELSE
  cdefault = ''
ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destr => dest
IF (PRESENT(default)) myoption%destr = default
myoption%opttype = opttype_r
myoption%need_arg = 2

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_r


!> Add a new option with a real type array argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. The argument must be provided in the
!! form of comma-separated list of values and is stored in an object
!! of type arrayof_real (module \a array_utilities). An optional
!! default value can be provided for the destination. Please use the
!! generic \a optionparser_add method rather than this particular
!! method.
SUBROUTINE optionparser_add_rarray(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
TYPE(arrayof_real),TARGET :: dest !< the destination of the option parse result
REAL,OPTIONAL :: default(:) !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

!IF (PRESENT(default)) THEN
!  cdefault = ' [default='//TRIM(to_char(default))//']'
!ELSE
  cdefault = ''
!ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destrarr => dest
IF (PRESENT(default)) THEN
  CALL insert(myoption%destrarr, default)
  CALL packarray(myoption%destrarr)
ENDIF
myoption%opttype = opttype_rarr
myoption%need_arg = 2

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_rarray


!> Add a new option with a double precision type argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. An optional default value can be
!! provided for the destination. Please use the generic \a
!! optionparser_add method rather than this particular method.
SUBROUTINE optionparser_add_d(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
DOUBLE PRECISION,TARGET :: dest !< the destination of the option parse result
DOUBLE PRECISION,OPTIONAL :: default !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

IF (PRESENT(default)) THEN
  IF (c_e(default)) THEN
    cdefault = ' [default='//TRIM(ADJUSTL(to_char(default,form='(G15.9)')))//']'
  ELSE
    cdefault = ' [default='//t2c(default, 'MISSING')//']'
  ENDIF
ELSE
  cdefault = ''
ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destd => dest
IF (PRESENT(default)) myoption%destd = default
myoption%opttype = opttype_d
myoption%need_arg = 2

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_d


!> Add a new option with a double precision type array argument.
!! When parsing will be performed, if the requested option is
!! encountered, its corresponding compulsory argument will be copied
!! into the provided destination. The argument must be provided in the
!! form of comma-separated list of values and is stored in an object
!! of type arrayof_doubleprecision (module \a array_utilities). An optional
!! default value can be provided for the destination. Please use the
!! generic \a optionparser_add method rather than this particular
!! method.
SUBROUTINE optionparser_add_darray(this, short_opt, long_opt, dest, default, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
TYPE(arrayof_doubleprecision),TARGET :: dest !< the destination of the option parse result
DOUBLE PRECISION,OPTIONAL :: default(:) !< the default value to give to dest if option is not found
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

CHARACTER(LEN=40) :: cdefault
INTEGER :: i
TYPE(option) :: myoption

!IF (PRESENT(default)) THEN
!  cdefault = ' [default='//TRIM(to_char(default))//']'
!ELSE
  cdefault = ''
!ENDIF

! common initialisation
myoption = option_new(short_opt, long_opt, cdefault, help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%destdarr => dest
IF (PRESENT(default)) THEN
  CALL insert(myoption%destdarr, default)
  CALL packarray(myoption%destdarr)
ENDIF
myoption%opttype = opttype_darr
myoption%need_arg = 2

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_darray


!> Add a new logical option, without optional argument.
!! When parsing will be performed, if the requested option is
!! encountered, the provided destination will be set to \a
!! .TRUE. . The provided destination is initially set to \a
!! .FALSE. . Please use the generic \a optionparser_add method
!! rather than this particular method.
SUBROUTINE optionparser_add_l(this, short_opt, long_opt, dest, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
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
myoption%need_arg = 0

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_l


!> Add a new counter option, without optional argument.
!! When parsing will be performed, the provided destination will be
!! incremented by one, starting from \a start, each time the requested
!! option is encountered.
SUBROUTINE optionparser_add_count(this, short_opt, long_opt, dest, start, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
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
myoption%need_arg = 0

i = arrayof_option_append(this%options, myoption)

END SUBROUTINE optionparser_add_count


!> Add a new help option, with an optional argument.
!! When parsing will be performed, the full help message will be
!! printed if this option is encountered. The message can be directly
!! printed as well by calling the optparser_printhelp method.  The
!! optional argument given by the user to the option specifies the
!! format of the help message, it can be one fo the following:
!!
!!  - \c txt or no extra argument: generic plain-text format suitable
!!    for printing to screen and to be fed to the \c help2man command
!!    for generating man pages
!!  - <tt>md</tt> or <tt>markdown</tt>: print help in markdown format,
!!    suitable for wiki/github/doxygen etc. pages
!!  - <tt>htmlform</tt>: print help as an html form suitable for
!!    providing the options through a web interface (experimental)
SUBROUTINE optionparser_add_help(this, short_opt, long_opt, help)
TYPE(optionparser),INTENT(inout) :: this !< \a optionparser object
CHARACTER(len=*),INTENT(in) :: short_opt !< the short option (may be empty)
CHARACTER(len=*),INTENT(in) :: long_opt !< the long option (may be empty)
CHARACTER(len=*),OPTIONAL :: help !< the help message that will be formatted and pretty-printed on screen

INTEGER :: i
TYPE(option) :: myoption

! common initialisation
myoption = option_new(short_opt, long_opt, '', help)
IF (.NOT.c_e(myoption)) RETURN ! error in creating option, ignore it

myoption%opttype = opttype_help
myoption%need_arg = 1

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
CHARACTER(len=16384) :: arg, optarg

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
        SELECT CASE(this%options%array(j)%need_arg)
        CASE(2) ! compulsory
          IF (indeq /= 0) THEN
            optarg = arg(indeq+1:)
            status = MAX(option_found(this%options%array(j), optarg), &
             status)
          ELSE
            IF (i < iargc()) THEN
              i=i+1
              CALL getarg(i, optarg)
              status = MAX(option_found(this%options%array(j), optarg), &
               status)
            ELSE
              status = optionparser_err
              CALL l4f_log(L4F_ERROR, &
               'in optionparser, option '''//TRIM(arg)//''' requires an argument')
            ENDIF
          ENDIF
        CASE(1) ! optional
          IF (indeq /= 0) THEN
            optarg = arg(indeq+1:)
          ELSE
            IF (i < iargc()) THEN
              CALL getarg(i+1, optarg)
              IF (optarg(1:1) == '-') THEN
                optarg = cmiss ! refused
              ELSE            
                i=i+1 ! accepted
              ENDIF
            ELSE
              optarg = cmiss ! refused
            ENDIF
          ENDIF
          status = MAX(option_found(this%options%array(j), optarg), &
           status)
        CASE(0)
          status = MAX(option_found(this%options%array(j)), &
           status)
        END SELECT
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
        SELECT CASE(this%options%array(j)%need_arg)
        CASE(2) ! compulsory
          IF (LEN_TRIM(arg) > 2) THEN
            optarg = arg(3:)
            status = MAX(option_found(this%options%array(j), optarg), &
             status)
          ELSE
            IF (i < iargc()) THEN
              i=i+1
              CALL getarg(i, optarg)
              status = MAX(option_found(this%options%array(j), optarg), &
               status)
            ELSE
              status = optionparser_err
              CALL l4f_log(L4F_ERROR, &
               'in optionparser, option '''//TRIM(arg)//''' requires an argument')
            ENDIF
          ENDIF
        CASE(1) ! optional
          IF (LEN_TRIM(arg) > 2) THEN
            optarg = arg(3:)
          ELSE
            IF (i < iargc()) THEN
              CALL getarg(i+1, optarg)
              IF (optarg(1:1) == '-') THEN
                optarg = cmiss ! refused
              ELSE            
                i=i+1 ! accepted
              ENDIF
            ELSE
              optarg = cmiss ! refused
            ENDIF
          ENDIF
          status = MAX(option_found(this%options%array(j), optarg), &
           status)
        CASE(0)
          status = MAX(option_found(this%options%array(j)), &
           status)
        END SELECT
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
SELECT CASE(status)
CASE(optionparser_err, optionparser_help)
  CALL optionparser_printhelp(this)
END SELECT

END SUBROUTINE optionparser_parse


!> Print on stdout a human-readable text representation of the help message.
!! It can be called by the user program and it is called anyway in
!! case of error in the interpretation of the command line.
SUBROUTINE optionparser_printhelp(this)
TYPE(optionparser),INTENT(in) :: this !< \a optionparser object with correctly initialised options

INTEGER :: i, form

form = 0
DO i = 1, this%options%arraysize ! loop over options
  IF (this%options%array(i)%opttype == opttype_help) THEN
    form = this%options%array(i)%helpformat
  ENDIF
ENDDO

SELECT CASE(form)
CASE(0)
  CALL optionparser_printhelptxt(this)
CASE(1)
  CALL optionparser_printhelpmd(this)
CASE(2)
  CALL optionparser_printhelphtmlform(this)
END SELECT

END SUBROUTINE optionparser_printhelp


!> Print on stdout a human-readable text representation of the help message.
!! It can be called by the user program and it is called anyway in
!! case of error in the interpretation of the command line.
SUBROUTINE optionparser_printhelptxt(this)
TYPE(optionparser),INTENT(in) :: this !< \a optionparser object with correctly initialised options

INTEGER :: i, j, ncols
CHARACTER(len=80) :: buf
TYPE(line_split) :: help_line

ncols = default_columns()

! print usage message
IF (ASSOCIATED(this%usage_msg)) THEN
  help_line = line_split_new(cstr_to_fchar(this%usage_msg), ncols)
  DO j = 1, line_split_get_nlines(help_line)
    WRITE(*,'(A)')TRIM(line_split_get_line(help_line,j))
  ENDDO
  CALL delete(help_line)
ELSE
  CALL getarg(0, buf)
  i = INDEX(buf, '/', back=.TRUE.) ! remove directory part
  IF (buf(i+1:i+3) == 'lt-') i = i + 3 ! remove automake prefix
  WRITE(*,'(A)')'Usage: '//TRIM(buf(i+1:))//' [options] [arguments]'
ENDIF

! print description message
IF (ASSOCIATED(this%description_msg)) THEN
  WRITE(*,'()')
  help_line = line_split_new(cstr_to_fchar(this%description_msg), ncols)
  DO j = 1, line_split_get_nlines(help_line)
    WRITE(*,'(A)')TRIM(line_split_get_line(help_line,j))
  ENDDO
  CALL delete(help_line)
ENDIF

WRITE(*,'(/,A)')'Options:'

DO i = 1, this%options%arraysize ! loop over options
  CALL option_format_help(this%options%array(i), ncols)
ENDDO

END SUBROUTINE optionparser_printhelptxt


!> Print on stdout a markdown representation of the help message.
!! It can be called by the user program and it is called anyway if the
!! program has been called with the `--help md` option.
SUBROUTINE optionparser_printhelpmd(this)
TYPE(optionparser),INTENT(in) :: this !< \a optionparser object with correctly initialised options

INTEGER :: i, j
CHARACTER(len=80) :: buf
TYPE(line_split) :: help_line

! print usage message
WRITE(*,'(A)')'### Synopsis ###'

IF (ASSOCIATED(this%usage_msg)) THEN
  WRITE(*,'(A,/)')TRIM(mdquote_usage_msg(cstr_to_fchar(this%usage_msg)))
ELSE
  CALL getarg(0, buf)
  i = INDEX(buf, '/', back=.TRUE.) ! remove directory part
  IF (buf(i+1:i+3) == 'lt-') i = i + 3 ! remove automake prefix
  WRITE(*,'(A,/)')'Usage: `'//TRIM(buf(i+1:))//' [options] [arguments]`'
ENDIF

! print description message
IF (ASSOCIATED(this%description_msg)) THEN
  WRITE(*,'(A)')'### Description ###'
  WRITE(*,'(A,/)')cstr_to_fchar(this%description_msg)
ENDIF

WRITE(*,'(A)')'### Options ###'

DO i = 1, this%options%arraysize ! loop over options
  CALL option_format_md(this%options%array(i))
ENDDO

CONTAINS

FUNCTION mdquote_usage_msg(usage_msg)
CHARACTER(len=*),INTENT(in) :: usage_msg

CHARACTER(len=LEN(usage_msg)+2) :: mdquote_usage_msg
INTEGER :: colon

colon = INDEX(usage_msg, ':') ! typically 'Usage: cp [options] origin destination'
IF (colon > 0 .AND. colon < LEN(usage_msg)-1) THEN
  mdquote_usage_msg = usage_msg(:colon+1)//'`'//usage_msg(colon+2:)//'`'
ELSE
  mdquote_usage_msg = usage_msg
ENDIF

END FUNCTION mdquote_usage_msg

END SUBROUTINE optionparser_printhelpmd

!> Print on stdout an html form reflecting the command line options set up.
!! It can be called by the user program and it is called anyway if the
!! program has been called with the `--help htmlform` option.
SUBROUTINE optionparser_printhelphtmlform(this)
TYPE(optionparser),INTENT(in) :: this !< \a optionparser object with correctly initialised options

INTEGER :: i

DO i = 1, this%options%arraysize ! loop over options
  CALL option_format_htmlform(this%options%array(i))
ENDDO

WRITE(*,'(A)')'<input TYPE="submit" VALUE="runprogram" />'

END SUBROUTINE optionparser_printhelphtmlform


SUBROUTINE optionparser_make_completion(this)
TYPE(optionparser),INTENT(in) :: this !< \a optionparser object with correctly initialised options

INTEGER :: i
CHARACTER(len=512) :: buf

CALL getarg(0, buf)

WRITE(*,'(A/A/A)')'_'//TRIM(buf)//'()','{','local cur'

WRITE(*,'(A/A/A/A)')'COMPREPLY=()','cur=${COMP_WORDS[COMP_CWORD]}', &
 'case "$cur" in','-*)'

!-*)
!    COMPREPLY=( $( compgen -W 
DO i = 1, this%options%arraysize ! loop over options
  IF (this%options%array(i)%need_arg == 2) THEN
  ENDIF
ENDDO

WRITE(*,'(A/A/A)')'esac','return 0','}'

END SUBROUTINE optionparser_make_completion



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

