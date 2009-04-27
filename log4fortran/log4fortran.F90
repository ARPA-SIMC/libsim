#include "config.h"

!> \defgroup log4fortran Pacchetto log4fortran, libreria logforfortran 

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
!!is <log4c> and it can be used to control the configuration file
!!version interface with the attribute "version". The following 4
!!elements are supported: <config>, <category>, <appender> and
!!<layout>.
!!
!!     The <config> element controls the global log4c
!!     configuration. It has 3 sub elements. The <nocleanup> flag
!!     inhibits the log4c destructors routines. The <bufsize> element
!!     sets the buffer size used to format log4c_logging_event_t
!!     objects. If is set to 0, the allocation is dynamic (the <debug>
!!     element is currently unused).
!!
!!     The <category> element has 3 possible attributes: the category
!!     "name", the category "priority" and the category
!!     "appender". Future versions will handle multple appenders per
!!     category.
!!
!!     The <appender> element has 3 possible attributes: the appender
!!     "name", the appender "type", and the appender "layout".
!!
!!     The <layout> element has 2 possible attributes: the layout
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
use kinds
use missing_values
use char_utilities
use io_units
implicit none

INTEGER, PARAMETER :: L4F_FATAL    = 000  !< standard priority
INTEGER, PARAMETER :: L4F_ALERT    = 100  !< standard priority
INTEGER, PARAMETER :: L4F_CRIT     = 200  !< standard priority
INTEGER, PARAMETER :: L4F_ERROR    = 300  !< standard priority
INTEGER, PARAMETER :: L4F_WARN     = 400  !< standard priority
INTEGER, PARAMETER :: L4F_NOTICE   = 500  !< standard priority
INTEGER, PARAMETER :: L4F_INFO     = 600  !< standard priority
INTEGER, PARAMETER :: L4F_DEBUG    = 700  !< standard priority
INTEGER, PARAMETER :: L4F_TRACE    = 800  !< standard priority
INTEGER, PARAMETER :: L4F_NOTSET   = 900  !< standard priority
INTEGER, PARAMETER :: L4F_UNKNOWN  = 1000 !< standard priority

INTEGER, PRIVATE :: l4f_log_priority = L4F_INFO
INTEGER, PRIVATE :: l4f_fatal_priority = L4F_CRIT
INTEGER, PRIVATE :: l4f_unit = stdout_unit
LOGICAL, PRIVATE :: l4f_initialised = .FALSE.
LOGICAL, PRIVATE :: l4f_category_initialised = .FALSE.
INTEGER, PRIVATE :: l4f_default_category = imiss
CHARACTER(len=510), PRIVATE :: l4f_default_a_name = 'no category'


#ifdef HAVE_LIBLOG4C
!> Interfaces to C function wrappers, generated through CNF library,
!! which call the corresponding C functions of the log4c library.
interface 
! log4fortran constructor
integer function l4f_init_l4c()
end function l4f_init_l4c

! Initialize a logging category.
integer function l4f_category_get_l4c (a_name)
character (len=*),intent(in) :: a_name !< category name
end function l4f_category_get_l4c

!> Delete a logging category.
subroutine l4f_category_delete(a_category)
integer,intent(in):: a_category !< category name
end subroutine l4f_category_delete

! Emit log message for a category with specific priority
subroutine l4f_category_log_l4c(a_category,a_priority,a_format)
integer,intent(in):: a_category !< category name
integer,intent(in):: a_priority !< priority level
character(len=*),intent(in):: a_format !< message to emit
end subroutine l4f_category_log_l4c

! log4fortran destructor
integer function l4f_fini_l4c()
end function l4f_fini_l4c

!>Ritorna un messaggio caratteristico delle priorità standard
!character(len=12) function l4f_msg(a_priority)
!integer,intent(in):: a_priority !< category name
!end function l4f_msg
! serve questa interfaccia?
end interface

#endif

CONTAINS

!> Routine di inizializzazione specifica per il SIM.
!! Cattura le variabili di ambiente
!! LOG4_APPLICATION_NAME,LOG4_APPLICATION_ID e compone il nome univoco
!! per il logging.  Se le variabili di ambiente non sono impostate
!! ritorna un nome definito dal nome del processo e da un timestamp.
!! È utilizzata sia con che senza log4c.
SUBROUTINE l4f_launcher(a_name,a_name_force,a_name_append)

integer :: tarray(8)
character (len=255) :: LOG4_APPLICATION_NAME,LOG4_APPLICATION_ID,arg
character (len=*),intent(out) :: a_name !< nome univoco per logging
character (len=*),intent(in),optional :: a_name_force !< forza il valore di a_name
character (len=*),intent(in),optional :: a_name_append !< valore da appendere a a_name
character (len=255),save :: a_name_save=cmiss

if (present(a_name_force))then
  a_name=a_name_force
else if (c_e(a_name_save))then
  a_name=a_name_save
else

  call date_and_time(values=tarray)
  call getarg(0,arg)
  call getenv("LOG4_APPLICATION_NAME",LOG4_APPLICATION_NAME)
  call getenv("LOG4_APPLICATION_ID",LOG4_APPLICATION_ID)
  
  if (LOG4_APPLICATION_NAME=="" .and. LOG4_APPLICATION_ID=="") then
    
    write (a_name,"(a,a,8i5,a)")trim(arg),"[",tarray,"]"
    
  else
    
    a_name = trim(LOG4_APPLICATION_NAME)//"["//trim(LOG4_APPLICATION_ID)//"]"
    
  end if
end if

a_name_save=a_name

if (present(a_name_append))then
  a_name=to_char(a_name)//"."//to_char(a_name_append)
end if

end subroutine l4f_launcher


!> Initialise log4fortran environment.
INTEGER FUNCTION l4f_init()

#ifdef HAVE_LIBLOG4C
l4f_init = l4f_init_l4c()
#else
l4f_init = 0
#endif

l4f_log_priority = L4F_INFO
l4f_default_category = imiss
l4f_initialised = .TRUE.

END FUNCTION l4f_init


!> Initialise a logging category.
INTEGER FUNCTION l4f_category_get(a_name)
CHARACTER(len=*),INTENT(in) :: a_name !< category name

#ifdef HAVE_LIBLOG4C
l4f_category_get = l4f_category_get_l4c(a_name)

! Store the first category as the default
IF (.NOT. c_e(l4f_default_category)) THEN
  l4f_default_a_name = a_name
  l4f_default_category = l4f_category_get
  l4f_category_initialised = .TRUE.
ENDIF

#else
! Set the category as the default
l4f_default_a_name = a_name
l4f_default_category = 0
l4f_category_initialised = .TRUE.
l4f_category_get = 0
#endif

END FUNCTION l4f_category_get


#ifndef HAVE_LIBLOG4C
!> Delete a logging category.
subroutine l4f_category_delete(a_category)
integer,intent(in):: a_category !< category name

if (a_category == 0) l4f_default_a_name = ""

end subroutine l4f_category_delete
#endif


!> Cleans up the log4fortran environment.
INTEGER FUNCTION l4f_fini()

l4f_initialised = .FALSE.
l4f_category_initialised = .FALSE.
l4f_default_category = imiss
l4f_default_a_name = 'no category'

#ifdef HAVE_LIBLOG4C
l4f_fini = l4f_fini_l4c()
#else
l4f_fini = 0
#endif

END FUNCTION l4f_fini


!> Emit log message for a category with specific priority.
!! Calls the interface to the corresponding log4c function
!! or sends the output to the standard unit.
!! Program is terminated if necessary.
SUBROUTINE l4f_category_log(a_category, a_priority, a_format)
INTEGER,INTENT(in):: a_category !< category name
INTEGER,INTENT(in):: a_priority !< priority level
CHARACTER(len=*),INTENT(in):: a_format !< message to emit

#ifdef HAVE_LIBLOG4C
CALL l4f_category_log_l4c(a_category, a_priority, a_format)
#else
IF (a_category == 0 .AND. a_priority <= l4f_log_priority) THEN
  WRITE(l4f_unit,'(A)')'[dummy] '//l4f_msg(a_priority)//TRIM(l4f_default_a_name)//&
   ' - '//TRIM(a_format)
END IF
#endif

CALL check_terminate(a_priority)

END SUBROUTINE l4f_category_log


!> Emit log message for default category with specific priority.
!! It works also in case of uninitialised environment with an output
!! on the standard logging unit.
SUBROUTINE l4f_log(a_priority, a_format)
INTEGER,INTENT(in):: a_priority !< priority level
CHARACTER(len=*),INTENT(in):: a_format !< message to emit

#ifdef HAVE_LIBLOG4C
IF (l4f_initialised .AND. l4f_category_initialised) THEN ! use default category
  CALL l4f_category_log(l4f_default_category, a_priority, a_format)
ELSE IF (a_priority <= l4f_log_priority) THEN ! Fallback with dummy logging
  WRITE(l4f_unit,'(A)')'[no log4c] '//l4f_msg(a_priority)//&
   TRIM(l4f_default_a_name)//' - '//TRIM(a_format)
  CALL check_terminate(a_priority)
END IF
#else
CALL l4f_category_log(0, a_priority, a_format)
#endif

END SUBROUTINE l4f_log


!> Set the minimum priority level that generates a log message.
!! It is used only when logging without log4c or when the log4fortran
!! environment has not been initialised.  In the other cases the log4c
!! configuration file takes precedence.  At program startup
!! it is set to \a L4F_INFO .
SUBROUTINE l4f_set_log_priority(a_priority)
INTEGER,INTENT(in):: a_priority !< priority level

l4f_log_priority = a_priority
!MAX(a_priority, L4F_FATAL) ! L4F_FATAL always logs?

END SUBROUTINE l4f_set_log_priority


!> Set the minimum priority level that generates a fatal
!! error and terminates the program.  At program startup
!! it is set to \a L4F_CRIT .
SUBROUTINE l4f_set_fatal_priority(a_priority)
INTEGER,INTENT(in):: a_priority !< priority level

l4f_fatal_priority = MAX(a_priority, L4F_FATAL) ! L4F_FATAL is always fatal

END SUBROUTINE l4f_set_fatal_priority


!> Set the fortran I/O unit to which log messages are redirected.
!! It is used only when logging without log4c or when the log4fortran
!! environment has not been initialised.  At program startup
!! it is set to \a stdout .
SUBROUTINE l4f_set_unit(unit)
INTEGER,INTENT(in):: unit !< fortran unit number

l4f_unit = unit

END SUBROUTINE l4f_set_unit


!> Returns a text message describing the given priority.
CHARACTER(len=9) FUNCTION l4f_msg(a_priority)
INTEGER,INTENT(in):: a_priority !< category name

SELECT CASE(a_priority)
CASE(L4F_FATAL)
  l4f_msg = "FATAL"
CASE(L4F_ALERT)
  l4f_msg = "ALERT"
CASE(L4F_CRIT)
  l4f_msg = "CRIT"
CASE(L4F_ERROR)
  l4f_msg = "ERROR"
CASE(L4F_WARN)
  l4f_msg = "WARN"
CASE(L4F_NOTICE)
  l4f_msg = "NOTICE"
CASE(L4F_INFO)
  l4f_msg = "INFO"
CASE(L4F_DEBUG)
  l4f_msg = "DEBUG"
CASE(L4F_TRACE)
  l4f_msg = "TRACE"
CASE(L4F_NOTSET)
  l4f_msg = "NOTSET"
CASE(L4F_UNKNOWN)
  l4f_msg = "UNKNOWN"
CASE DEFAULT
  l4f_msg = to_char(a_priority)
END SELECT

END FUNCTION l4f_msg


! Internal subroutine for termination of program in case of error.
SUBROUTINE check_terminate(a_priority)
INTEGER,INTENT(in):: a_priority

IF (a_priority <= l4f_fatal_priority) THEN
  CALL EXIT(1)
ENDIF

END SUBROUTINE check_terminate


END MODULE log4fortran
