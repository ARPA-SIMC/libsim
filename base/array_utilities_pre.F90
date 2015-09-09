#ifdef ENABLE_SORT
INTERFACE count_distinct_sorted
  MODULE PROCEDURE count_distinct_sorted/**/VOL7D_POLY_TYPES
END INTERFACE
#endif

INTERFACE count_distinct
  MODULE PROCEDURE count_distinct/**/VOL7D_POLY_TYPES
END INTERFACE

#ifndef VOL7D_NO_PACK

#ifdef ENABLE_SORT
INTERFACE pack_distinct_sorted
  MODULE PROCEDURE pack_distinct_sorted/**/VOL7D_POLY_TYPES
END INTERFACE
#endif

INTERFACE pack_distinct
  MODULE PROCEDURE pack_distinct/**/VOL7D_POLY_TYPES
END INTERFACE

INTERFACE count_and_pack_distinct
  MODULE PROCEDURE count_and_pack_distinct/**/VOL7D_POLY_TYPES
END INTERFACE
#endif

INTERFACE map_distinct
  MODULE PROCEDURE map_distinct/**/VOL7D_POLY_TYPES
END INTERFACE

INTERFACE map_inv_distinct
  MODULE PROCEDURE map_inv_distinct/**/VOL7D_POLY_TYPES
END INTERFACE

!> Index method.
INTERFACE index
  MODULE PROCEDURE index/**/VOL7D_POLY_TYPES
END INTERFACE

#ifdef ENABLE_SORT
!> Sort method.
INTERFACE sort
  MODULE PROCEDURE sort/**/VOL7D_POLY_TYPES
END INTERFACE
#endif
