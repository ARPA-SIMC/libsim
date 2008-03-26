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
!! http://log4c.cvs.sourceforge.net/*checkout*/log4c/log4c/doc/Log4C-DevelopersGuide.odt )
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
module log4fortran

INTEGER ,PARAMETER :: L4F_FATAL    = 000  !< standard priority
INTEGER ,PARAMETER :: L4F_ALERT    = 100  !< standard priority
INTEGER ,PARAMETER :: L4F_CRIT     = 200  !< standard priority
INTEGER ,PARAMETER :: L4F_ERROR    = 300  !< standard priority
INTEGER ,PARAMETER :: L4F_WARN     = 400  !< standard priority
INTEGER ,PARAMETER :: L4F_NOTICE   = 500  !< standard priority
INTEGER ,PARAMETER :: L4F_INFO     = 600  !< standard priority
INTEGER ,PARAMETER :: L4F_DEBUG    = 700  !< standard priority
INTEGER ,PARAMETER :: L4F_TRACE    = 800  !< standard priority
INTEGER ,PARAMETER :: L4F_NOTSET   = 900  !< standard priority
INTEGER ,PARAMETER :: L4F_UNKNOWN  = 1000 !< standard priority

!> priority: default value used only when compiled without log4c or cnf
!!(the configuration file is ignored)  
integer :: log4fortran_priority=L4F_NOTICE 


character(len=510):: dummy_a_name

private dummy_a_name 

contains

!>Routine specifica per il SIM; cattura le variabili di ambiente
!!LOG4_APPLICATION_NAME,LOG4_APPLICATION_ID e compone il nome univoco
!!per il logging.  Se le variabili di ambiente non sono impostate
!!ritorna un nome definito dal nome del processo e da un timestamp
subroutine log4fortran_launcher(a_name)

integer :: tarray(8)
character (len=255) :: LOG4_APPLICATION_NAME,LOG4_APPLICATION_ID,arg
character (len=*),intent(out) :: a_name !<

call date_and_time(values=tarray)
call getarg(0,arg)
call getenv("LOG4_APPLICATION_NAME",LOG4_APPLICATION_NAME)
call getenv("LOG4_APPLICATION_ID",LOG4_APPLICATION_ID)

if (LOG4_APPLICATION_NAME=="" .or. LOG4_APPLICATION_ID=="") then

  write (a_name,"(a,a,8i5,a)")trim(arg),"[",tarray,"]"

else

  a_name = trim(LOG4_APPLICATION_NAME)//"["//trim(LOG4_APPLICATION_ID)//"]"

end if

end subroutine log4fortran_launcher

#ifndef LOG4FORTRAN

! definisce delle dummy routine

!>log4fortran constructors
integer function log4fortran_init()

log4fortran_priority=L4F_NOTICE

log4fortran_init= 1

end function log4fortran_init



!>Initialize a logging category.
integer function log4fortran_category_get (a_name)
character (len=*),intent(in) :: a_name !< category name

dummy_a_name=a_name

log4fortran_category_get= 0

end function log4fortran_category_get



!>Delete a logging category.
subroutine log4fortran_category_delete(a_category)
integer,intent(in):: a_category !< category name

if (a_category == 0 ) dummy_a_name=""

end subroutine log4fortran_category_delete


!>Emit log message for a category with specific priority
subroutine log4fortran_category_log (a_category,a_priority,&
 a_format)
integer,intent(in):: a_category !< category name
integer,intent(in):: a_priority !< priority level
character(len=*),intent(in):: a_format !< message to emit

if (a_category == 0 .and. a_priority <= log4fortran_priority  ) then
  write(*,*)"[dummy] ",log4fortran_msg(a_priority),trim(dummy_a_name)," - ",a_format
end if

end subroutine log4fortran_category_log


!>log4fortran destructors
integer function log4fortran_fini()

log4fortran_fini= 0

end function log4fortran_fini

!>Ritorna un messaggio caratteristico delle priorità standard
character(len=12) function  log4fortran_msg(a_priority)

integer,intent(in):: a_priority !< category name

  write(log4fortran_msg,*)a_priority

  if (a_priority == L4F_FATAL)   log4fortran_msg="FATAL"
  if (a_priority == L4F_ALERT)   log4fortran_msg="ALERT"
  if (a_priority == L4F_CRIT)    log4fortran_msg="CRIT"
  if (a_priority == L4F_ERROR)   log4fortran_msg="ERROR"
  if (a_priority == L4F_WARN)    log4fortran_msg="WARN"
  if (a_priority == L4F_NOTICE)  log4fortran_msg="NOTICE"
  if (a_priority == L4F_INFO)    log4fortran_msg="INFO"
  if (a_priority == L4F_DEBUG)   log4fortran_msg="DEBUG"
  if (a_priority == L4F_TRACE)   log4fortran_msg="TRACE"
  if (a_priority == L4F_NOTSET)  log4fortran_msg="NOTSET"
  if (a_priority == L4F_UNKNOWN) log4fortran_msg="UNKNOWN"


end function log4fortran_msg



#endif

end module log4fortran
