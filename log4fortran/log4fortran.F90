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
module log4fortran

implicit none

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
integer :: l4f_priority=L4F_NOTICE 


#ifdef HAVE_LIBLOG4C
!>Qui sono reperibili function e subroutine definite tramite questa interface
!!e che richiamano funzioni C tramite la libreria CNF. Le funzioni chiamabili da fortran
!!sono equivalenti a quelle messe a disposizione dalla libreria log4C
interface 
!>log4fortran constructors
integer function l4f_init()
end function l4f_init

!>Initialize a logging category.
integer function l4f_category_get (a_name)
character (len=*),intent(in) :: a_name !< category name
end function l4f_category_get

!>Delete a logging category.
subroutine l4f_category_delete(a_category)
integer,intent(in):: a_category !< category name
end subroutine l4f_category_delete

!>Emit log message for a category with specific priority
subroutine l4f_category_log (a_category,a_priority,&
 a_format)
integer,intent(in):: a_category !< category name
integer,intent(in):: a_priority !< priority level
character(len=*),intent(in):: a_format !< message to emit
end subroutine l4f_category_log

!>Emit log message without category with specific priority
subroutine l4f_log (a_priority,a_format)
integer,intent(in):: a_priority !< priority level
character(len=*),intent(in):: a_format !< message to emit
end subroutine l4f_log


!>Return True if category exist
logical function l4f_category_exist (a_category)
integer,intent(in):: a_category !< category name
end function l4f_category_exist

!>log4fortran destructors
integer function l4f_fini()
end function l4f_fini

!>Ritorna un messaggio caratteristico delle priorità standard
character(len=12) function l4f_msg(a_priority)
integer,intent(in):: a_priority !< category name
end function l4f_msg

end interface

#else

CHARACTER(len=510), PRIVATE:: dummy_a_name

#endif

contains

!>Routine specifica per il SIM; cattura le variabili di ambiente
!!LOG4_APPLICATION_NAME,LOG4_APPLICATION_ID e compone il nome univoco
!!per il logging.  Se le variabili di ambiente non sono impostate
!!ritorna un nome definito dal nome del processo e da un timestamp
subroutine l4f_launcher(a_name,a_name_force,a_name_append)

integer :: tarray(8)
character (len=255) :: LOG4_APPLICATION_NAME,LOG4_APPLICATION_ID,arg
character (len=*),intent(out) :: a_name !< nome univoco per logging
character (len=*),intent(in),optional :: a_name_force !< forza il valore di a_name
character (len=*),intent(in),optional :: a_name_append !< valore da appendere a a_name
character (len=255),save :: a_name_save=""

if (present(a_name_force))then
  a_name=a_name_force
else if (.not. a_name_save == "")then
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
  a_name=trim(a_name)//"."//trim(a_name_append)
end if
  

end subroutine l4f_launcher

#ifndef HAVE_LIBLOG4C
! definisce delle dummy routine

!>log4fortran constructors
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
l4f_category_get = 0

end function l4f_category_get


!>Delete a logging category.
subroutine l4f_category_delete(a_category)
integer,intent(in):: a_category !< category name

if (a_category == 0) dummy_a_name = ""

end subroutine l4f_category_delete


!>Emit log message for a category with specific priority
subroutine l4f_category_log (a_category,a_priority,a_format)
integer,intent(in):: a_category !< category name
integer,intent(in):: a_priority !< priority level
character(len=*),intent(in):: a_format !< message to emit

if (a_category == 0 .and. a_priority <= l4f_priority) then
  write(*,*)"[dummy] ",l4f_msg(a_priority),trim(dummy_a_name)," - ",a_format
end if

end subroutine l4f_category_log



!>Emit log message without category with specific priority
subroutine l4f_log (a_priority,a_format)
integer,intent(in):: a_priority !< priority level
character(len=*),intent(in):: a_format !< message to emit

if ( a_priority <= l4f_priority) then
  write(*,*)"[_default] ",l4f_msg(a_priority),trim(dummy_a_name)," - ",a_format
end if

end subroutine l4f_log


!>Return True if category exist
logical function l4f_category_exist (a_category)
integer,intent(in):: a_category !< category name

if (a_category == 0) then
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

#endif

end module log4fortran
