! This universal template can be used to wrap any derived type into a
! derived type defining a 1-dimensional array of the original type;
! this array can be dynamically extended or shortened by adding or
! removing elements at an arbitrary position through the ::insert and
! ::remove methods.  All the allocations, deallocations, copy
! operations are taken care of in the present module; the user can
! also call the ::packarray method in order to reduce the memory
! occupation of the object or the ::delete method in order to delete
! all the data and release all the memory.  Before use, any object of
! the array type should be initialised through the constructor
! ARRAYOF_TYPE_new:: .
!
! The template requires the definition of the following preprocessor macros
! before being included:
!  - \c ARRAYOF_ORIGTYPE the type to be wrapped
!  - \c ARRAYOF_TYPE the name of the "arrayed" derived type, containing a 1-d array of ARRAYOF_ORIGTYPE, if undefined it will be \a arrayof_ARRAYOF_ORIGTYPE
!  - \c ARRAYOF_ORIGDESTRUCTOR(x) the instruction required in order to "destroy" an object of \c ARRAYOF_ORIGTYPE when the ::remove method is called, optional, if undefined no destructor is called
!  - \c ARRAYOF_ORIGEQ to be defined if ARRAYOF_ORIGTYPE supports the == operator, in that case the *_unique method are defined for the array
!  - \c ARRAYOF_PRIVATE to be defined if the array type is not going to be PUBLIC
!
! The template comes in 2 parts, one to be included in the
! declaration part of the module (before \c CONTAINS) and the second
! in the execution part of it (after \c CONTAINS).
#ifndef ARRAYOF_TYPE
#define ARRAYOF_TYPE arrayof_/**/ARRAYOF_ORIGTYPE
#endif

!> Derived type defining a dynamically extensible array of ARRAYOF_ORIGTYPE elements
TYPE ARRAYOF_TYPE
  ARRAYOF_ORIGTYPE, POINTER :: array(:) !< array of ARRAYOF_ORIGTYPE
  INTEGER :: arraysize !< current logical size of the array; it may be different from the physical size \c SIZE(this&perc;array), and it should be used instead of \c SIZE() intrinsic function in order to evaluate the number of elements assigned to \a array
  DOUBLE PRECISION :: overalloc !< overallocation factor, values close to 1 determine more calls to the system alloc function (decreased performances) at the advantage of less memory consumption, the default is 2; the results are not affected by the value of this member
END TYPE ARRAYOF_TYPE

INTERFACE insert
  MODULE PROCEDURE ARRAYOF_TYPE/**/_insert, ARRAYOF_TYPE/**/_insert_array
END INTERFACE

INTERFACE append
  MODULE PROCEDURE ARRAYOF_TYPE/**/_append
END INTERFACE

INTERFACE remove
  MODULE PROCEDURE ARRAYOF_TYPE/**/_remove
END INTERFACE

INTERFACE delete
  MODULE PROCEDURE ARRAYOF_TYPE/**/_delete
END INTERFACE

INTERFACE packarray
  MODULE PROCEDURE ARRAYOF_TYPE/**/_packarray
END INTERFACE

#ifndef ARRAYOF_PRIVATE
PUBLIC ARRAYOF_TYPE, ARRAYOF_TYPE/**/_new
#endif

PRIVATE array_of_alloc, &
 ARRAYOF_TYPE/**/_insert, ARRAYOF_TYPE/**/_insert_array, &
 ARRAYOF_TYPE/**/_append, ARRAYOF_TYPE/**/_remove, &
 ARRAYOF_TYPE/**/_delete, &
 ARRAYOF_TYPE/**/_packarray

!PUBLIC insert, append, remove, delete, packarray

#ifdef ARRAYOF_ORIGEQ
INTERFACE insert_unique
  MODULE PROCEDURE ARRAYOF_TYPE/**/_insert_unique
END INTERFACE

INTERFACE append_unique
  MODULE PROCEDURE ARRAYOF_TYPE/**/_append_unique
END INTERFACE

PRIVATE ARRAYOF_TYPE/**/_insert_unique, ARRAYOF_TYPE/**/_append_unique

!PUBLIC insert_unique, append_unique
#endif

