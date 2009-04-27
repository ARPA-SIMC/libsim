#include "stdio.h"
#include "log4c.h"
#include "f77.h"



F77_INTEGER_FUNCTION(l4f_init_l4c)() {
  return log4c_init();
}

F77_POINTER_FUNCTION(l4f_category_get_l4c)(CHARACTER(a_name) TRAIL(a_name)) {

  void   *tmpptr;
  int tmpfptr;
  char ptr_a_name[101];

  GENPTR_CHARACTER(a_name)
  cnfImprt(a_name, a_name_length > 100 ? 100:a_name_length, ptr_a_name);

  tmpptr = log4c_category_get(ptr_a_name);

  // get if already registered
  tmpfptr=cnfFptr(tmpptr);
  
  if (tmpfptr == 0)
    // if not registered try to register
    if (cnfRegp(tmpptr) == 1)
      // convert to fortran
      tmpfptr= cnfFptr(tmpptr);

  return tmpfptr;

}


F77_SUBROUTINE(l4f_category_delete)(POINTER(a_category)){

  GENPTR_POINTER(a_category)
  
  cnfUregp(cnfCptr(*a_category));

}


F77_SUBROUTINE(l4f_category_log_l4c)(POINTER(a_category), 
				     INTEGER(a_priority),
				     CHARACTER(a_format) TRAIL(a_format)) {
  char ptr_a_format[101];

  GENPTR_POINTER(a_category)
  GENPTR_INTEGER(a_priority)
  GENPTR_CHARACTER(a_format)

  cnfImprt(a_format, a_format_length > 100 ? 100:a_format_length, ptr_a_format);
  log4c_category_log(cnfCptr(*a_category), *a_priority, ptr_a_format);

}


F77_INTEGER_FUNCTION(l4f_fini_l4c)() {
  return log4c_fini();
}
