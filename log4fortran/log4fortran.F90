module log4fortran

INTEGER ,PARAMETER ::&
 L4F_FATAL    = 000,&
 L4F_ALERT    = 100,&
 L4F_CRIT     = 200,&
 L4F_ERROR    = 300,&
 L4F_WARN     = 400,&
 L4F_NOTICE   = 500,&
 L4F_INFO     = 600,&
 L4F_DEBUG    = 700,&
 L4F_TRACE    = 800,&
 L4F_NOTSET   = 900,&
 L4F_UNKNOWN  = 1000


#if !defined(LOG4FORTRAN)

! definisce delle dummy routine

contains

integer function log4fortran_init()

log4fortran_init= 1

end function log4fortran_init


integer function log4fortran_category_get (a_name)
character (len=*) :: a_name


log4fortran_category_get= 0

end function log4fortran_category_get


subroutine log4fortran_category_delete(a_category)
integer:: a_category


end subroutine log4fortran_category_delete


subroutine log4fortran_category_log (a_category,a_priority,&
 a_format)
integer:: a_category,a_priority
character(len=*):: a_format

end subroutine log4fortran_category_log

integer function log4fortran_fini()

log4fortran_fini= 0

end function log4fortran_fini


#endif

end module log4fortran
