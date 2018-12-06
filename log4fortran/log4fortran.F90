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

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
#include "config.h"

!> \defgroup log4fortran Libsim package, log4fortran library.
!! Fortran interface to a basic set of log4c library for performing
!! logging within a program.

!>\brief classe per la gestione del logging
!!
!!Questo modulo permette una semplice, ma potente gestione della messagistica.
!!E' utile sia in fase di debug che di monitoraggio utente.
!!
!!Questo modulo fornisce funzionalità simili, ma non identiche a
!!seconda che siano disponibili in fase di compilazione le librerie
!!log4c e cnf.
!!
!!There are three fundamental types of object in Log4C: categories,
!!appenders and layouts. You can think of these objects as
!!corresponding to the what, where and how of the logging system:
!!categories describe what sub-system the message relates to,
!!appenders determine where the message goes and layouts determine how
!!the message is formatted.  
!!
!!First, you have to figure out what kind of categories you
!!want. Maybe you want one logger for GUI code and another one for
!!memory management and a third one for user access
!!logging. Okay. That's fine. Me, I like to have a separate logger for
!!each class or data structure. I've already gone to the trouble of
!!breaking my code down into such categories. Why not just use those?
!!Feel free to set it up any way you like. Just don't make the mistake
!!of using message severity as your categories. That's what the
!!priority is all about.
!!
!! La gestione di appenders e layouts viene demandata in toto al file
!! di configurazione di log4c (vedere apposita documentazione
!! http://log4c.cvs.sourceforge.net/\*checkout\*/log4c/log4c/doc/Log4C-DevelopersGuide.odt )
!!
!!log4fortran by default can log messages with some standard priority levels:
!!
!!Use debug to write debugging messages which should not be printed
!!when the application is in production.
!!
!!Use info for messages similar to the "verbose" mode of many
!!applications.
!!
!!Use warn for warning messages which are logged to some log but the
!!application is able to carry on without a problem.
!!
!!Use error for application error messages which are also logged to
!!some log but, still, the application can hobble along. Such as when
!!some administrator-supplied configuration parameter is incorrect and
!!you fall back to using some hard-coded default value.
!!
!!Use fatal for critical messages, after logging of which the
!!application quits abnormally.
!!
!!Configuration syntax:
!!
!!The log4crc configuration file uses an XML syntax. The root element
!!is &lt;log4c&gt; and it can be used to control the configuration file
!!version interface with the attribute "version". The following 4
!!elements are supported: &lt;config&gt;, &lt;category&gt;, &lt;appender&gt; and
!!&lt;layout&gt;.
!!
!!     The &lt;config&gt; element controls the global log4c
!!     configuration. It has 3 sub elements. The &lt;nocleanup&gt; flag
!!     inhibits the log4c destructors routines. The &lt;bufsize&gt; element
!!     sets the buffer size used to format log4c_logging_event_t
!!     objects. If is set to 0, the allocation is dynamic (the &lt;debug&gt;
!!     element is currently unused).
!!
!!     The &lt;category&gt; element has 3 possible attributes: the category
!!     "name", the category "priority" and the category
!!     "appender". Future versions will handle multple appenders per
!!     category.
!!
!!     The &lt;appender&gt; element has 3 possible attributes: the appender
!!     "name", the appender "type", and the appender "layout".
!!
!!     The &lt;layout&gt; element has 2 possible attributes: the layout
!!     "name" and the layout "type".
!!
!!
!!This initial version of the log4c configuration file syntax is quite
!!different from log4j. XML seemed the best choice to keep the log4j
!!configuration power in a C API.  Environment variables
!!
!!     LOG4C_RCPATH holds the path to the main log4crc configuration file
!!     LOG4C_PRIORITY holds the "root" category priority
!!     LOG4C_APPENDER holds the "root" category appender
!!
!!
!!Programma esempio \include log4fortran.f90
!!Here's one sample log4crc configuration file \include log4crc
!!
!!\ingroup log4fortran
MODULE log4fortran
USE iso_c_binding
IMPLICIT NONE

INTEGER(kind=c_int),PARAMETER :: L4F_FATAL    = 000  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_ALERT    = 100  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_CRIT     = 200  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_ERROR    = 300  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_WARN     = 400  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_NOTICE   = 500  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_INFO     = 600  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_DEBUG    = 700  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_TRACE    = 800  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_NOTSET   = 900  !< standard priority
INTEGER(kind=c_int),PARAMETER :: L4F_UNKNOWN  = 1000 !< standard priority

!> Default priority value. It is used only when compiled without log4c
!! since the configuration file is ignored, but it is better to define
!! it all the time.
INTEGER(kind=c_int),PUBLIC :: l4f_priority=L4F_NOTICE

!> l4f handle. This type defines an opaque handle
!! to a l4f category (mapped to a log4c category),
!! it has to be initialised with the l4f_category_get method.
TYPE,BIND(C) :: l4f_handle
  PRIVATE
  TYPE(c_ptr) :: ptr = C_NULL_PTR
END TYPE l4f_handle

#ifdef HAVE_LIBLOG4C

TYPE(l4f_handle),SAVE :: l4f_global_default

! emulation of old cnf behavior returning integer instead of pointer
#undef ARRAYOF_ORIGEQ
#undef ARRAYOF_ORIGTYPE
#undef ARRAYOF_TYPE
#define ARRAYOF_ORIGTYPE TYPE(l4f_handle)
#define ARRAYOF_TYPE arrayof_l4f_handle
#include "arrayof_pre_nodoc.F90"

TYPE(arrayof_l4f_handle) :: l4f_global_ptr

!> Global log4fortran constructor.
INTERFACE 
  FUNCTION l4f_init() BIND(C,name='log4c_init')
  IMPORT
  INTEGER(kind=c_int) :: l4f_init
  END FUNCTION l4f_init
END INTERFACE

!> Initialize a logging category. This is the C version, please use the
!! Fortran version l4f_category_get that receives a Fortran character.
INTERFACE
  FUNCTION l4f_category_get_c(a_name) BIND(C,name='log4c_category_get')
  IMPORT
  CHARACTER(kind=c_char),INTENT(in) :: a_name(*) !< category name
  TYPE(l4f_handle) :: l4f_category_get_c
  END FUNCTION l4f_category_get_c
END INTERFACE

!! Delete a logging category. It can receive a C pointer or a
!! legacy integer value.
INTERFACE l4f_category_delete
!  SUBROUTINE l4f_category_delete_c(a_category) BIND(C,name='log4c_category_delete')
!  IMPORT
!  TYPE(l4f_handle),VALUE :: a_category !< category as C native pointer
!  END SUBROUTINE l4f_category_delete_c
  MODULE PROCEDURE l4f_category_delete_legacy, l4f_category_delete_f
END INTERFACE
! this function has been disabled because aftere deleting a category
! the following log4c_fini fails with a double free, we must
! understand the log4c docs

INTERFACE
  SUBROUTINE l4f_category_log_c(a_category, a_priority, a_format) BIND(C,name='log4c_category_log_c')
  IMPORT
  TYPE(l4f_handle),VALUE :: a_category !< category
  INTEGER(kind=c_int),VALUE :: a_priority !< priority level
!  TYPE(c_ptr),VALUE :: locinfo !< not used
  CHARACTER(kind=c_char),INTENT(in) :: a_format(*) !< message to emit
 ! TYPE(c_ptr),VALUE :: a_args
  END SUBROUTINE l4f_category_log_c
END INTERFACE

!> Emit log message for a category with specific priority.
!! It can receive a C pointer or a legacy integer value.
INTERFACE l4f_category_log
  MODULE PROCEDURE l4f_category_log_f, l4f_category_log_legacy
END INTERFACE l4f_category_log

!> Return true if the corresponding category handle exists.
INTERFACE l4f_category_exist
  MODULE PROCEDURE l4f_category_exist_f, l4f_category_exist_legacy
END INTERFACE l4f_category_exist

!> log4fortran destructor
INTERFACE
  FUNCTION l4f_fini() BIND(C,name='log4c_fini')
  IMPORT
  INTEGER(kind=c_int) :: l4f_fini
  END FUNCTION l4f_fini
END INTERFACE

!>Ritorna un messaggio caratteristico delle priorità standard
!interface
!CHARACTER(len=12) FUNCTION l4f_msg(a_priority)
!integer,intent(in):: a_priority !< category name
!end function l4f_msg
!end interface

#else

CHARACTER(len=510),PRIVATE:: dummy_a_name

#endif

PRIVATE
PUBLIC L4F_FATAL, L4F_ALERT, L4F_CRIT, L4F_ERROR, L4F_WARN, L4F_NOTICE, &
 L4F_INFO, L4F_DEBUG, L4F_TRACE, L4F_NOTSET, L4F_UNKNOWN
PUBLIC l4f_init, l4f_category_get, l4f_category_delete, l4f_category_log, &
 l4f_log, l4f_category_exist, l4f_fini
PUBLIC l4f_launcher

CONTAINS

!> Routine specifica per il SIM. Cattura le variabili di ambiente
!! LOG4_APPLICATION_NAME,LOG4_APPLICATION_ID e compone il nome univoco
!! per il logging.  Se le variabili di ambiente non sono impostate
!! ritorna un nome definito dal nome del processo e da un timestamp.
SUBROUTINE l4f_launcher(a_name, a_name_force, a_name_append)
CHARACTER(len=*),INTENT(out) :: a_name !< nome univoco per logging
CHARACTER(len=*),INTENT(in),OPTIONAL :: a_name_force !< forza il valore di a_name
CHARACTER(len=*),INTENT(in),OPTIONAL :: a_name_append !< valore da appendere a a_name

INTEGER :: tarray(8)
CHARACTER(len=255) :: LOG4_APPLICATION_NAME,LOG4_APPLICATION_ID,arg
CHARACTER(len=255),SAVE :: a_name_save=""

IF (PRESENT(a_name_force))THEN
  a_name=a_name_force
ELSE IF (a_name_save /= "")THEN
  a_name=a_name_save
ELSE
  
  CALL date_and_TIME(values=tarray)
  CALL getarg(0, arg)
  CALL getenv("LOG4_APPLICATION_NAME", LOG4_APPLICATION_NAME)
  CALL getenv("LOG4_APPLICATION_ID", LOG4_APPLICATION_ID)
  
  IF (LOG4_APPLICATION_NAME == "" .AND. LOG4_APPLICATION_ID == "") THEN
    WRITE(a_name,"(a,a,8i5,a)")TRIM(arg),"[",tarray,"]"
  ELSE
    a_name = TRIM(LOG4_APPLICATION_NAME)//"["//TRIM(LOG4_APPLICATION_ID)//"]"
  END IF

END IF

a_name_save=a_name

IF (PRESENT(a_name_append)) THEN
  a_name=TRIM(a_name)//"."//TRIM(a_name_append)
END IF
  
END SUBROUTINE l4f_launcher

#ifndef HAVE_LIBLOG4C
! definisce delle dummy routine

!> log4fortran constructor
integer function l4f_init()

character(len=10)::priority
integer :: iostat

call getenv("LOG4C_PRIORITY",priority)
if (priority=="") then
  l4f_priority = L4F_NOTICE
else
  read(priority,*,iostat=iostat)l4f_priority
end if

if (iostat /= 0) then
  l4f_priority = L4F_NOTICE
end if

l4f_init = 0

end function l4f_init


!>Initialize a logging category.
integer function l4f_category_get (a_name)
character (len=*),intent(in) :: a_name !< category name

dummy_a_name = a_name
l4f_category_get = 1

end function l4f_category_get


!>Delete a logging category.
subroutine l4f_category_delete(a_category)
integer,intent(in):: a_category !< category name

if (a_category == 1) dummy_a_name = ""

end subroutine l4f_category_delete


!>Emit log message for a category with specific priority
subroutine l4f_category_log (a_category,a_priority,a_format)
integer,intent(in):: a_category !< category name
integer,intent(in):: a_priority !< priority level
character(len=*),intent(in):: a_format !< message to emit

if (a_category == 1 .and. a_priority <= l4f_priority) then
  write(*,*)"[dummy] ",l4f_msg(a_priority),trim(dummy_a_name)," - ",trim(a_format)
end if

end subroutine l4f_category_log


!>Emit log message without category with specific priority
subroutine l4f_log (a_priority,a_format)
integer,intent(in):: a_priority !< priority level
character(len=*),intent(in):: a_format !< message to emit

if ( a_priority <= l4f_priority) then
  write(*,*)"[_default] ",l4f_msg(a_priority),trim(dummy_a_name)," - ",trim(a_format)
end if

end subroutine l4f_log


!>Return True if category exist
logical function l4f_category_exist (a_category)
integer,intent(in):: a_category !< category name

if (a_category == 1) then
  l4f_category_exist= .TRUE.
else
  l4f_category_exist= .FALSE.  
end if

end function l4f_category_exist


!>log4fortran destructors
integer function l4f_fini()

l4f_fini= 0

end function l4f_fini

!>Ritorna un messaggio caratteristico delle priorità standard
character(len=12) function l4f_msg(a_priority)

integer,intent(in):: a_priority !< category name

write(l4f_msg,*)a_priority

if (a_priority == L4F_FATAL)   l4f_msg="FATAL"
if (a_priority == L4F_ALERT)   l4f_msg="ALERT"
if (a_priority == L4F_CRIT)    l4f_msg="CRIT"
if (a_priority == L4F_ERROR)   l4f_msg="ERROR"
if (a_priority == L4F_WARN)    l4f_msg="WARN"
if (a_priority == L4F_NOTICE)  l4f_msg="NOTICE"
if (a_priority == L4F_INFO)    l4f_msg="INFO"
if (a_priority == L4F_DEBUG)   l4f_msg="DEBUG"
if (a_priority == L4F_TRACE)   l4f_msg="TRACE"
if (a_priority == L4F_NOTSET)  l4f_msg="NOTSET"
if (a_priority == L4F_UNKNOWN) l4f_msg="UNKNOWN"

end function l4f_msg

#else

#include "arrayof_post_nodoc.F90"

!> Initialize a logging category. This is the 
!! Fortran legacy version that receives a Fortran character argument
!! and returns an integer.
FUNCTION l4f_category_get(a_name) RESULT(handle)
CHARACTER(kind=c_char,len=*),INTENT(in) :: a_name !< category name
INTEGER :: handle

INTEGER :: i

DO i = 1, l4f_global_ptr%arraysize ! look first for a hole
  IF (.NOT.l4f_category_exist(l4f_global_ptr%array(i))) THEN
    l4f_global_ptr%array(i) = l4f_category_get_c(TRIM(a_name)//CHAR(0))
    handle = i
    RETURN
  ENDIF
ENDDO

handle = append(l4f_global_ptr, l4f_category_get_c(TRIM(a_name)//CHAR(0)))

END FUNCTION l4f_category_get


!> Initialize a logging category. This is the 
!! Fortran version that receives a Fortran character argument
!! and returns a typed handle.
FUNCTION l4f_category_get_handle(a_name) RESULT(handle)
CHARACTER(kind=c_char,len=*),INTENT(in) :: a_name !< category name
TYPE(l4f_handle) :: handle

handle = l4f_category_get_c(TRIM(a_name)//CHAR(0))

END FUNCTION l4f_category_get_handle


!> Delete a logging category. Legacy version with an integer argument.
SUBROUTINE l4f_category_delete_legacy(a_category)
INTEGER,INTENT(in) :: a_category !< category as an integer

IF (a_category <= 0 .OR. a_category > l4f_global_ptr%arraysize) RETURN
IF (a_category == l4f_global_ptr%arraysize) THEN
  CALL remove(l4f_global_ptr, pos=a_category)
ELSE
  l4f_global_ptr%array(a_category)%ptr = C_NULL_PTR
ENDIF

END SUBROUTINE l4f_category_delete_legacy


!> Delete a logging category. No-op version with a typed handle.
SUBROUTINE l4f_category_delete_f(a_category)
TYPE(l4f_handle),INTENT(inout) :: a_category !< category as C native pointer

a_category%ptr = C_NULL_PTR ! is it necessary?

END SUBROUTINE l4f_category_delete_f


!> Emit log message for a category with specific priority.
!! Fortran version that receives a Fortran character argument.
SUBROUTINE l4f_category_log_f(a_category, a_priority, a_format)
TYPE(l4f_handle),INTENT(in) :: a_category !< category
INTEGER(kind=c_int),INTENT(in) :: a_priority !< priority level
CHARACTER(len=*),INTENT(in) :: a_format !< message to emit

CALL l4f_category_log_c(a_category, a_priority, TRIM(a_format)//CHAR(0))

END SUBROUTINE l4f_category_log_f


!> Emit log message for a category with specific priority.
!! Legacy Fortran version that receives an integer instead of a C
!! pointer and a Fortran character argument.
SUBROUTINE l4f_category_log_legacy(a_category, a_priority, a_format)
INTEGER(kind=c_int),INTENT(in) :: a_category !< category
INTEGER(kind=c_int),INTENT(in) :: a_priority !< priority level
CHARACTER(len=*),INTENT(in) :: a_format !< message to emit

CALL l4f_category_log_c(l4f_global_ptr%array(a_category), a_priority, TRIM(a_format)//CHAR(0))

END SUBROUTINE l4f_category_log_legacy


!> Emit log message without category with specific priority.
!! Fortran version that receives a Fortran character argument.
SUBROUTINE l4f_log(a_priority, a_format)
INTEGER(kind=c_int),INTENT(in) :: a_priority !< priority level
CHARACTER(len=*),INTENT(in) :: a_format !< message to emit

INTEGER :: i

IF (.NOT.l4f_category_exist(l4f_global_default)) THEN
  i = l4f_init()
  l4f_global_default = l4f_category_get_handle('_default')
ENDIF
CALL l4f_category_log(l4f_global_default, a_priority, a_format)

END SUBROUTINE l4f_log


!> Return true if the corresponding category handle exists
!! (is associated with a category).
FUNCTION l4f_category_exist_f(a_category) RESULT(exist)
TYPE(l4f_handle),INTENT(in) :: a_category !< category
LOGICAL :: exist

exist = C_ASSOCIATED(a_category%ptr)

END FUNCTION l4f_category_exist_f

!> Return true if the corresponding category handle exists
!! (is associated with a category).
!! Legacy Fortran version that receives an integer instead of a C
!! pointer.
FUNCTION l4f_category_exist_legacy(a_category) RESULT(exist)
INTEGER,INTENT(in):: a_category !< category
LOGICAL :: exist

IF (a_category <= 0 .OR. a_category > l4f_global_ptr%arraysize) THEN
  exist = .FALSE.
ELSE
  exist = l4f_category_exist(l4f_global_ptr%array(a_category))
ENDIF

END FUNCTION l4f_category_exist_legacy


#endif

end module log4fortran
