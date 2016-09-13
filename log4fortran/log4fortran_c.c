#include "stdio.h"
#include "log4c.h"
#include "f77.h"

static void* default_cat=NULL;


F77_INTEGER_FUNCTION(l4f_init)() {
  return log4c_init();
}

F77_POINTER_FUNCTION(l4f_category_get)(CHARACTER(a_name) TRAIL(a_name)) {

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
    //  fini attemp to delete all category ! do not use this
    //  log4c_category_delete(cnfCptr(*a_category));
  cnfUregp(cnfCptr(*a_category));

}


F77_SUBROUTINE(l4f_category_log)(POINTER(a_category), 
				     INTEGER(a_priority),
				     CHARACTER(a_format) TRAIL(a_format)) {
  char ptr_a_format[101];

  GENPTR_POINTER(a_category)
  GENPTR_INTEGER(a_priority)
  GENPTR_CHARACTER(a_format)

  cnfImprt(a_format, a_format_length > 100 ? 100:a_format_length, ptr_a_format);
  log4c_category_log(cnfCptr(*a_category), *a_priority, "%s", ptr_a_format);

}


F77_SUBROUTINE(l4f_log)(INTEGER(a_priority),
			CHARACTER(a_format) TRAIL(a_format)) {
  char ptr_a_format[101];
  
  if (default_cat == NULL) {
    log4c_init();
    default_cat = log4c_category_get("_default");
  }
  
  GENPTR_INTEGER(a_priority)
  GENPTR_CHARACTER(a_format)

  cnfImprt(a_format, a_format_length > 100 ? 100:a_format_length, ptr_a_format);

  log4c_category_log(default_cat, *a_priority, "%s", ptr_a_format);

}


F77_LOGICAL_FUNCTION(l4f_category_exist) (POINTER(a_category)) {

  if ( cnfCptr(*a_category) == NULL ) return F77_FALSE ;else return F77_TRUE;

}



F77_INTEGER_FUNCTION(l4f_fini)() {

  if (default_cat != NULL) {
    //    log4c_category_delete(default_cat);
    default_cat = NULL;
  }

  return log4c_fini();
}
