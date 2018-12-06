#include "log4c.h"

/* a C layer is required because log4c_category_log is a symbol and
   supports variable arguments */
void log4c_category_log_c(const log4c_category_t* a_category, int a_priority, const char* a_format) {
  log4c_category_log(a_category, a_priority, "%s", a_format);
}
