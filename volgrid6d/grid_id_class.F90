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
!> This module defines an abstract interface to different drivers for
!! access to files containing gridded information. It defines a class
!! associated to a file-like object \a grid_file_id, and a class
!! associated to a grid-like object \a grid_id, extracted from a
!! grid_file_id object. At the moment it has support for grib_api and
!! gdal.
!!
!! Example of use (driver independent):
!! \code
!! ...
!! USE grid_id_class
!!
!! TYPE(grid_file_id) :: ifile, ofile
!! TYPE(grid_id) :: gaid
!! CHARACTER(len=512) :: ifilename, ofilename
!!
!! ! open files
!! ifile = grid_file_id_new(trim(ifilename),'r')
!! ofile = grid_file_id_new(trim(ofilename),'w')
!! IF (.NOT.c_e(ifile) .OR. .NOT.c_e(ofile)) STOP
!!
!! ! loop on all the messages in a file
!! DO WHILE (.TRUE.)
!! ! import from input file
!!   gaid = grid_id_new(ifile)
!!   IF (.NOT.c_e(gaid)) EXIT
!!
!! ! work on gaid here
!! ...
!! ! export to output file
!!   CALL export(gaid, ofile)
!!   CALL delete(gaid)
!! ENDDO
!!
!! ! close the files
!! CALL delete(ifile)
!! CALL delete(ofile)
!! ...
!!
!! \endcode
!!
!! \ingroup volgrid6d
MODULE grid_id_class
#ifdef HAVE_LIBGRIBAPI
USE grib_api
#endif
#ifdef HAVE_LIBGDAL
USE gdal
#endif
USE missing_values
USE optional_values
USE char_utilities
USE file_utilities
USE log4fortran
USE err_handling
IMPLICIT NONE

INTEGER,PARAMETER :: grid_id_no_driver = 0 !< constants to be used for associating an object to a driver: no type specified
INTEGER,PARAMETER :: grid_id_grib_api = 1 !< type grib_api specified
INTEGER,PARAMETER :: grid_id_gdal = 2 !< type gdal specified

#if defined HAVE_LIBGRIBAPI
INTEGER,PARAMETER :: grid_id_default = grid_id_grib_api !< default driver if none specified in constructor
#elif defined HAVE_LIBGDAL
INTEGER,PARAMETER :: grid_id_default = grid_id_gdal !< default driver if none specified in constructor
#else
INTEGER,PARAMETER :: grid_id_default = grid_id_no_driver !< default driver if none specified in constructor
#endif

CHARACTER(len=12),PARAMETER :: driverlist(0:2) = &
 (/'no_driver   ','grib_api    ','gdal        '/)

#ifdef HAVE_LIBGDAL
!> Derived type containing driver-specific options for gdal.
!! It is initialised when opening the file/dataset with the
!! constructor grid_file_id_new with the information provided in the
!! driver string.
TYPE gdal_file_id_options
  DOUBLE PRECISION :: xmin=dmiss !< bounding box of the area to import from the dataset
  DOUBLE PRECISION :: ymin=dmiss !< bounding box of the area to import from the dataset
  DOUBLE PRECISION :: xmax=dmiss !< bounding box of the area to import from the dataset
  DOUBLE PRECISION :: ymax=dmiss !< bounding box of the area to import from the dataset
END TYPE gdal_file_id_options
#endif

!> Derived type associated to a file-like object containing many
!! blocks/messages/records/bands of gridded data.
TYPE grid_file_id
PRIVATE
#ifdef HAVE_LIBGRIBAPI
INTEGER :: gaid=imiss
#endif
#ifdef HAVE_LIBGDAL
TYPE(gdaldataseth) :: gdalid
INTEGER :: nlastband=0
TYPE(gdal_file_id_options) :: gdal_options
TYPE(grid_file_id),POINTER :: file_id_copy=>NULL()
#endif
INTEGER :: driver=grid_id_default
END TYPE grid_file_id


!> Derived type associated to a block/message/record/band of gridded
!! data coming from a file-like object.
TYPE grid_id
PRIVATE
INTEGER :: nodriverid=imiss
#ifdef HAVE_LIBGRIBAPI
INTEGER :: gaid=imiss
#endif
#ifdef HAVE_LIBGDAL
TYPE(gdalrasterbandh) :: gdalid
TYPE(grid_file_id),POINTER :: file_id=>NULL()
#endif
INTEGER :: driver=grid_id_default
END TYPE grid_id

!> Constructors for the corresponding classes in SUBROUTINE form.
!! It is alternative to the *_new function constructors.
INTERFACE init
  MODULE PROCEDURE grid_file_id_init, grid_id_init
END INTERFACE

!> Destructors for the corresponding classes.
INTERFACE delete
  MODULE PROCEDURE grid_file_id_delete, grid_id_delete
END INTERFACE

!> Make a deep copy, if possible, of the grid identifier.
INTERFACE copy
  MODULE PROCEDURE grid_id_copy
END INTERFACE

!> Export a grid to a file
INTERFACE export
  MODULE PROCEDURE grid_id_export
END INTERFACE

!INTERFACE ASSIGNMENT(=)
!  MODULE PROCEDURE &
!#ifdef HAVE_LIBGRIBAPI
!   grid_id_assign_int, &
!#endif
!#ifdef HAVE_LIBGDAL
!   grid_id_assign_dal, &
!this%gdalid = that%gdalid
!#endif
!  grid_id_assign_grid_id
!END INTERFACE

!> Check whether the corresponding object has been correctly associated.

!> Set of LOGICAL functions to check whether a \a grid_file_id or a \a
!! grid_id object have been correctly associated to a file or a grid.
!! For a \a grid_file_id object it returns \a .FALSE. if the file has
!! not been opened correctly or if the object has been initialized as
!! empty.  For a \a grid_id object it returns \a .FALSE. if the grid
!! has not been correctly obtained from the file or if the object has
!! been initialized as empty. They work both on scalars and 1-d array
!! objects.
!!
!! \param this (TYPE(grid_file_id) or TYPE(grid_id)) object to be checked.
INTERFACE c_e
  MODULE PROCEDURE grid_id_c_e, grid_id_c_e_v, grid_file_id_c_e, grid_file_id_c_e_v
END INTERFACE

!> Display on standard output a description of the \a grid_id object
!! provided.
INTERFACE display
  MODULE PROCEDURE grid_id_display
END INTERFACE

PRIVATE grid_file_id_delete, grid_id_delete, grid_id_copy, &
 grid_id_c_e, grid_file_id_c_e, grid_id_c_e_v, grid_file_id_c_e_v, grid_id_display

CONTAINS


SUBROUTINE grid_file_id_init(this, filename, mode, driver, from_grid_id)
TYPE(grid_file_id),INTENT(out) :: this ! object to initialise
CHARACTER(len=*),INTENT(in) :: filename ! name of file containing gridded data, in the format [driver:]pathname
CHARACTER(len=*),INTENT(in) :: mode ! access mode for file, 'r' or 'w'
INTEGER,INTENT(in),OPTIONAL :: driver ! select the driver that will be associated to the grid_file_id created, use the constants \a grid_id_notype, \a grid_id_grib_api, \a grid_id_gdal
TYPE(grid_id),INTENT(in),OPTIONAL :: from_grid_id ! select the driver as the one associated to the provided grid_id object

this = grid_file_id_new(filename, mode, driver, from_grid_id)

END SUBROUTINE grid_file_id_init


!> Constructor for the \a grid_file_id class. It opens the associated
!! file(s); the driver to be used for file access is selected
!! according to the \a filename argument, to the optional argument \a
!! driver, or to the optional argument \a from_grid_id, with
!! increasing priority. If \a driver and \a from_grid_id are not
!! provided and \a filename does not contain driver information, a
!! default is chosen. If filename is an empty string or missing value,
!! the object will be empty, the same will happen in case the file
!! cannot be successfully opened. This condition can be tested with
!! the function \a c_e() . The driver string provided with the
!! filename can also contain driver-specific options separated by
!! commas, e.g. \c 'gdal,8,44,10,46:globe.dat'.
FUNCTION grid_file_id_new(filename, mode, driver, from_grid_id) RESULT(this)
CHARACTER(len=*),INTENT(in) :: filename !< name of file containing gridded data, in the format [driver:]pathname
CHARACTER(len=*),INTENT(in) :: mode !< access mode for file, 'r' or 'w'
INTEGER,INTENT(in),OPTIONAL :: driver !< select the driver that will be associated to the grid_file_id created, use the constants \a grid_id_notype, \a grid_id_grib_api, \a grid_id_gdal
TYPE(grid_id),INTENT(in),OPTIONAL :: from_grid_id !< select the driver as the one associated to the provided grid_id object
TYPE(grid_file_id) :: this

INTEGER :: n, ier, nf
#ifdef HAVE_LIBGDAL
INTEGER :: imode
#endif
TYPE(csv_record) :: driveropts
CHARACTER(len=12) :: drivername

#ifdef HAVE_LIBGDAL
CALL gdalnullify(this%gdalid)
#endif

IF (filename == '' .OR. .NOT.c_e(filename)) RETURN

n = INDEX(filename,':')
IF (n > 1) THEN ! override with driver from filename
  CALL init(driveropts, filename(:n-1), nfield=nf)
  CALL csv_record_getfield(driveropts, drivername)
#ifdef HAVE_LIBGRIBAPI
  IF (drivername == 'grib_api') THEN
    this%driver = grid_id_grib_api
  ENDIF
#endif
#ifdef HAVE_LIBGDAL
  IF (drivername == 'gdal') THEN
    IF (nf > 4) THEN
      this%driver = grid_id_gdal
      CALL csv_record_getfield(driveropts, this%gdal_options%xmin)
      CALL csv_record_getfield(driveropts, this%gdal_options%ymin)
      CALL csv_record_getfield(driveropts, this%gdal_options%xmax)
      CALL csv_record_getfield(driveropts, this%gdal_options%ymax)
    ELSE
      CALL l4f_log(L4F_ERROR, 'gdal driver requires 4 extra arguments (bounding box)')
      CALL raise_error()
    ENDIF
  ENDIF
#endif
  CALL delete(driveropts)
ENDIF
IF (PRESENT(driver)) THEN ! override with driver
  this%driver = driver
ENDIF
IF (PRESENT(from_grid_id)) THEN ! override with from_grid_id
  this%driver = from_grid_id%driver
ENDIF

#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN
  CALL grib_open_file(this%gaid, filename(n+1:), TRIM(mode), ier)
  IF (ier /= GRIB_SUCCESS) this%gaid = imiss
ENDIF
#endif
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal) THEN
  IF (mode(1:1) == 'w') THEN
    imode = GA_Update
  ELSE
    imode = GA_ReadOnly
  ENDIF
  CALL gdalallregister()
  this%gdalid = gdalopen(TRIM(filename(n+1:))//C_NULL_CHAR, imode)
! dirty trick, with gdal I have to keep a copy of the file_id, memory leak
  ALLOCATE(this%file_id_copy)
  this%file_id_copy = this
ENDIF
#endif

END FUNCTION grid_file_id_new


!> Count the number of block/message/record/band of gridded
!! data in the file-like object provided. Returns 0 if the \a file_id
!! object is empty or not corrctly associated to a file.
FUNCTION grid_file_id_count(this) RESULT(count)
TYPE(grid_file_id),INTENT(in) :: this !< file object to count
INTEGER :: count

INTEGER :: ier

count = 0
#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN
  IF (c_e(this%gaid)) THEN
    CALL grib_count_in_file(this%gaid, count, ier)
    IF (ier /= GRIB_SUCCESS) count = 0
  ENDIF
ENDIF
#endif
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal) THEN
  IF (gdalassociated(this%gdalid)) THEN
    count = gdalgetrastercount(this%gdalid)
  ENDIF
ENDIF
#endif

END FUNCTION grid_file_id_count


!> Destructor for the \a grid_file_id class. It closes the associated
!! file(s) and releases the associated memory, but, in some drivers
!! like grib_api, it may not release the memory associated to the \a
!! grid_id objects read from that file, which continue their life in
!! memory.
SUBROUTINE grid_file_id_delete(this)
TYPE(grid_file_id),INTENT(inout) :: this !< object to be deleted

#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN
  IF (c_e(this%gaid)) CALL grib_close_file(this%gaid)
ENDIF
this%gaid = imiss
#endif
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal) THEN
! dirty trick, with gdal I have to keep the file open
!  IF (gdalassociated(this%gdalid)) CALL gdalclose(this%gdalid)
  this%nlastband = 0
ENDIF
CALL gdalnullify(this%gdalid)
#endif

this%driver = imiss

END SUBROUTINE grid_file_id_delete


! Function to check whether a \a grid_file_id object has been correctly associated
! to the requested file. It returns \a .FALSE. if the file has not
! been opened correctly or if the object has been initialized as empty.
FUNCTION grid_file_id_c_e(this)
TYPE(grid_file_id),INTENT(in) :: this ! object to be checked
LOGICAL :: grid_file_id_c_e

grid_file_id_c_e = .FALSE.

#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN
  grid_file_id_c_e = c_e(this%gaid)
ENDIF
#endif
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal) THEN
  grid_file_id_c_e = gdalassociated(this%gdalid)
ENDIF
#endif

END FUNCTION grid_file_id_c_e


! Function to check whether a \a grid_file_id object has been correctly associated
! to the requested file. It returns \a .FALSE. if the file has not
! been opened correctly or if the object has been initialized as empty.
FUNCTION grid_file_id_c_e_v(this)
TYPE(grid_file_id),INTENT(in) :: this(:) ! object to be checked
LOGICAL :: grid_file_id_c_e_v(SIZE(this))

INTEGER :: i

DO i = 1, SIZE(this)
  grid_file_id_c_e_v(i) = c_e(this(i))
ENDDO

END FUNCTION grid_file_id_c_e_v


SUBROUTINE grid_id_init(this, from_grid_file_id, grib_api_template, grib_api_id)
TYPE(grid_id),INTENT(out) :: this ! object to be initialized
TYPE(grid_file_id),INTENT(inout),OPTIONAL :: from_grid_file_id ! file object from which grid object has to be created
CHARACTER(len=*),INTENT(in),OPTIONAL :: grib_api_template ! grib_api template file from which grid_object has to be created
INTEGER,INTENT(in),OPTIONAL :: grib_api_id ! grib_api id obtained directly from a \a grib_get subroutine call

this = grid_id_new(from_grid_file_id, grib_api_template, grib_api_id)

END SUBROUTINE grid_id_init


!> Constructor for the \a grid_id class. It gets the next grid (grib
!! message or raster band) from the file_id provided. If the file
!! associated to the file_id provided contains no more grids, or if
!! the argument \a file_id is not provided, an empty object is
!! created; this condition can be tested with the function c_e().
!! Alternative ways to define the object (to be used in rare cases)
!! are through a grib_api template file name (\a grib_api_template
!! argument) or through a grib_api integer id obtained directly from
!! grib_api calls (\a grib_api_id argument).
FUNCTION grid_id_new(from_grid_file_id, grib_api_template, grib_api_id, &
 no_driver_id) RESULT(this)
TYPE(grid_file_id),INTENT(inout),OPTIONAL,TARGET :: from_grid_file_id !< file object from which grid object has to be created
CHARACTER(len=*),INTENT(in),OPTIONAL :: grib_api_template !< grib_api template file from which grid_object has to be created
INTEGER,INTENT(in),OPTIONAL :: grib_api_id !< grib_api id obtained directly from a \a grib_get subroutine call
INTEGER,INTENT(in),OPTIONAL :: no_driver_id
TYPE(grid_id) :: this

INTEGER :: ier

#ifdef HAVE_LIBGDAL
CALL gdalnullify(this%gdalid)
#endif

IF (PRESENT(from_grid_file_id)) THEN
  this%driver = from_grid_file_id%driver ! take driver from file_id

#ifdef HAVE_LIBGRIBAPI
  IF (this%driver == grid_id_grib_api) THEN
    IF (c_e(from_grid_file_id%gaid)) THEN
      CALL grib_new_from_file(from_grid_file_id%gaid, this%gaid, ier)
      IF (ier /= GRIB_SUCCESS) this%gaid = imiss
    ENDIF
  ENDIF
#endif
#ifdef HAVE_LIBGDAL
  IF (this%driver == grid_id_gdal) THEN
    IF (gdalassociated(from_grid_file_id%gdalid) .AND. &
     ASSOCIATED(from_grid_file_id%file_id_copy)) THEN
      IF (from_grid_file_id%nlastband < &
       gdalgetrastercount(from_grid_file_id%gdalid)) THEN ! anything to read?
        from_grid_file_id%nlastband = from_grid_file_id%nlastband + 1
        this%gdalid = &
         gdalgetrasterband(from_grid_file_id%gdalid, from_grid_file_id%nlastband)
        this%file_id => from_grid_file_id%file_id_copy ! for gdal remember copy of file_id

      ENDIF
    ENDIF
  ENDIF
#endif

#ifdef HAVE_LIBGRIBAPI
ELSE IF (PRESENT(grib_api_template)) THEN
  this%driver = grid_id_grib_api
  CALL grib_new_from_samples(this%gaid, grib_api_template, ier)
  IF (ier /= GRIB_SUCCESS) this%gaid = imiss
ELSE IF (PRESENT(grib_api_id)) THEN
  this%driver = grid_id_grib_api
  this%gaid = grib_api_id
#endif
ELSE IF (PRESENT(no_driver_id)) THEN
  this%driver = grid_id_no_driver
  this%nodriverid = no_driver_id
ENDIF

END FUNCTION grid_id_new


!> Destructor for the \a grid_id class. It releases the memory associated with
!! the grid descriptor identifier. In grib_api this is necessary and
!! can be made also after closing the corresponding \a grid_file_id
!! object; while for gdal this is a no-operation.
SUBROUTINE grid_id_delete(this)
TYPE(grid_id),INTENT(inout) :: this !< object to be deleted

this%nodriverid = imiss
#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN
  IF (c_e(this%gaid)) CALL grib_release(this%gaid)
ENDIF
this%gaid = imiss
#endif
#ifdef HAVE_LIBGDAL
CALL gdalnullify(this%gdalid)
NULLIFY(this%file_id)
#endif

this%driver = imiss

END SUBROUTINE grid_id_delete


!> Check whether the grid_id object is readonly (.TRUE.) or allows
!! writing bands (.FALSE.)
FUNCTION grid_id_readonly(this) RESULT(readonly)
TYPE(grid_id),INTENT(in) :: this !< object to test
LOGICAL :: readonly

readonly = this%driver /= grid_id_grib_api

END FUNCTION grid_id_readonly


!> Performs a "deep" copy of the \a grid_id object when possible.
!! For grib_api this clones the grid_id generating a new independent
!! object in memory, which can be manipulated without affecting the
!! original one. The \a grid_id object \a that does not need to be
!! initialized before the call.
SUBROUTINE grid_id_copy(this, that)
TYPE(grid_id),INTENT(in) :: this !< source object
TYPE(grid_id),INTENT(out) :: that !< destination object, it must not be initialized

that = this ! start with a shallow copy

#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN
  IF (c_e(this%gaid)) THEN
    that%gaid = -1
    CALL grib_clone(this%gaid, that%gaid)
  ENDIF
ENDIF
#endif
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal) THEN
  IF (c_e(this)) THEN
!   that = grid_id_new(no_driver_id=1)
!    that%gdalid = this%gdalid ! better idea?
!    that%file_id => this%file_id
  ENDIF
ENDIF
#endif

END SUBROUTINE grid_id_copy


!> Export a \a grid_id object \a this to the file indicated by a
!! \a grid_file_id object. Both grid_id and grid_file_id objects must
!! be related to the same driver (e.g. grib_api or gdal).
SUBROUTINE grid_id_export(this, file_id)
TYPE(grid_id),INTENT(inout) :: this
TYPE(grid_file_id),INTENT(in) :: file_id !< file object to which grid has to be exported

INTEGER :: ier

IF (c_e(this) .AND. c_e(file_id)) THEN
#ifdef HAVE_LIBGRIBAPI
  IF (this%driver == grid_id_grib_api .AND. file_id%driver == grid_id_grib_api) &
   CALL grib_write(this%gaid, file_id%gaid, ier) ! log ier?
#endif
ENDIF
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal .AND. file_id%driver == grid_id_gdal) THEN
  ! not implemented, log?
ENDIF
#endif

END SUBROUTINE grid_id_export


! Function to check whether a \a _file_id object has been correctly associated
! to a grid. It returns \a .FALSE. if the grid has not been correctly
! obtained from the file or if the object has been initialized as
! empty.
FUNCTION grid_id_c_e(this)
TYPE(grid_id),INTENT(in) :: this ! object to be checked
LOGICAL :: grid_id_c_e

grid_id_c_e = .FALSE.

#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN
  grid_id_c_e = c_e(this%gaid)
ENDIF
#endif
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal) THEN
  grid_id_c_e = gdalassociated(this%gdalid)
ENDIF
#endif
IF (this%driver == grid_id_no_driver) THEN
  grid_id_c_e = c_e(this%nodriverid)
ENDIF

END FUNCTION grid_id_c_e


! Function to check whether a \a _file_id object has been correctly associated
! to a grid. It returns \a .FALSE. if the grid has not been correctly
! obtained from the file or if the object has been initialized as
! empty.
FUNCTION grid_id_c_e_v(this)
TYPE(grid_id),INTENT(in) :: this(:) ! object to be checked
LOGICAL :: grid_id_c_e_v(SIZE(this))

INTEGER :: i

DO i = 1, SIZE(this)
  grid_id_c_e_v(i) = c_e(this(i))
ENDDO

END FUNCTION grid_id_c_e_v


!> Return a character representation of the driver associated
!! with the object \a this. The result is a string with constant
!! length and blank-padded at the end, representing the name of the
!! driver; 'none' is retured if the object is empty.
FUNCTION grid_file_id_get_driver(this) RESULT(driver)
TYPE(grid_file_id),INTENT(in) :: this
CHARACTER(len=LEN(driverlist)) :: driver

IF (this%driver > 0 .AND. this%driver <= SIZE(driverlist)) THEN
  driver = driverlist(this%driver)
ELSE
  driver = driverlist(0)
ENDIF

END FUNCTION grid_file_id_get_driver
  

!> Return a character representation of the driver associated
!! with the object \a this. The result is a string with constant
!! length and blank-padded at the end, representing the name of the
!! driver; 'none' is retured if the object is empty.
FUNCTION grid_id_get_driver(this) RESULT(driver)
TYPE(grid_id),INTENT(in) :: this
CHARACTER(len=LEN(driverlist)) :: driver

IF (this%driver > 0 .AND. this%driver <= SIZE(driverlist)) THEN
  driver = driverlist(this%driver)
ELSE
  driver = driverlist(0)
ENDIF

END FUNCTION grid_id_get_driver


!> Display on standard output a description of the \a grid_id object
!! provided. Also the grib key names and values are printed; the set
!! of keys returned can be controlled with the input variable
!! namespace. Available namespaces are \c ls, to get the same default
!! keys as the \a grib_ls command, and \c mars to get the keys used by
!! mars.
SUBROUTINE grid_id_display(this, namespace)
TYPE(grid_id),INTENT(in) :: this !< object to be displayed
CHARACTER(len=*),OPTIONAL :: namespace !< grib_api namespace of the keys to search for, all the keys if empty, default \c ls

INTEGER :: kiter, iret
CHARACTER(len=255) :: key, value, lnamespace


#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN

  lnamespace = optio_c(namespace,255)
  IF (.NOT.c_e(lnamespace))THEN
    lnamespace = "ls"
  ENDIF

  PRINT*,"GRIB_API namespace:",TRIM(lnamespace)

  CALL grib_keys_iterator_new(this%gaid, kiter, namespace=TRIM(lnamespace))

  iter: DO
    CALL grib_keys_iterator_next(kiter, iret) 
  
    IF (iret /= 1) THEN
      EXIT iter
    END IF
  
    CALL grib_keys_iterator_get_name(kiter, key)
!<\todo bug in grib_api, segmentation fault with computeStatistics key
    IF (key == 'computeStatistics') CYCLE

    CALL grib_get(this%gaid, TRIM(key), value, iret)
    IF (iret == 0)THEN
      PRINT*, TRIM(key)//' = '//TRIM(VALUE)
    ELSE
      PRINT*, TRIM(key)//' = '//"KEY NOT FOUND, namespace :"//TRIM(lnamespace)//" ( bug ? )"
    ENDIF
  ENDDO iter

  CALL grib_keys_iterator_delete(kiter)

ENDIF

#endif

END SUBROUTINE grid_id_display


#ifdef HAVE_LIBGRIBAPI
!> Returns the original grib_api id associated with the object
!! provided, i.e. the unit associated by grib_api to the file.
FUNCTION grid_file_id_get_gaid(this) RESULT(gaid)
TYPE(grid_file_id),INTENT(in) :: this !< object to query
INTEGER :: gaid
gaid = this%gaid
END FUNCTION grid_file_id_get_gaid

!> Returns the original grib_api id associated with the object
!! provided, i.e. the grib_api id associated to the grid.
FUNCTION grid_id_get_gaid(this) RESULT(gaid)
TYPE(grid_id),INTENT(in) :: this !< object to query
INTEGER :: gaid
gaid = this%gaid
END FUNCTION grid_id_get_gaid
#endif


#ifdef HAVE_LIBGDAL
!> Returns the original gdal Fortran object associated with the object
!! provided, i.e. the dataset pointer.
FUNCTION grid_file_id_get_gdalid(this) RESULT(gdalid)
TYPE(grid_file_id),INTENT(in) :: this !< object to query
TYPE(gdaldataseth) :: gdalid
gdalid = this%gdalid
END FUNCTION grid_file_id_get_gdalid

!> Returns the original gdal Fortran object associated with the object
!! provided, i.e. the raster band pointer.
FUNCTION grid_id_get_gdalid(this) RESULT(gdalid)
TYPE(grid_id),INTENT(in) :: this !< object to query
TYPE(gdalrasterbandh) :: gdalid
gdalid = this%gdalid
END FUNCTION grid_id_get_gdalid

!> Returns an object with driver-specific options associated to the grid_id
!! provided, available only for gdal.
FUNCTION grid_id_get_gdal_options(this) RESULT(gdal_options)
TYPE(grid_id),INTENT(in) :: this !< object to query
TYPE(gdal_file_id_options) :: gdal_options

TYPE(gdal_file_id_options) :: gdal_options_local

IF (ASSOCIATED(this%file_id)) THEN
  gdal_options = this%file_id%gdal_options
ELSE
  gdal_options = gdal_options_local ! empty object
ENDIF

END FUNCTION grid_id_get_gdal_options
#endif


!> Decode and return the data array from a grid_id object.
!! The output array \a field must have a size matching the size of the
!! encoded data.
SUBROUTINE grid_id_decode_data(this, field)
TYPE(grid_id),INTENT(in) :: this
REAL,INTENT(out) :: field(:,:)

LOGICAL :: done

done = .FALSE.
#ifdef HAVE_LIBGRIBAPI
IF (c_e(this%gaid)) THEN
  CALL grid_id_decode_data_gribapi(this%gaid, field)
  done = .TRUE.
ENDIF
#endif
#ifdef HAVE_LIBGDAL
! subarea?
IF (gdalassociated(this%gdalid)) THEN
  CALL grid_id_decode_data_gdal(this%gdalid, field, this%file_id%gdal_options)
  done = .TRUE.
ENDIF
#endif
IF (.NOT.done) field(:,:) = rmiss

END SUBROUTINE grid_id_decode_data


!> Encode a data array into a grid_id object.
!! The input array \a field must have a size matching the size of the
!! dataset.
SUBROUTINE grid_id_encode_data(this, field)
TYPE(grid_id),INTENT(inout) :: this !< gridinfo object
REAL,intent(in) :: field(:,:) !< data array to be encoded

#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN

!  call display(this,"parameter")
!  print *,minval(field,c_e(field)),maxval(field,c_e(field))
!  print *,"-----------------------"

  IF (c_e(this%gaid)) CALL grid_id_encode_data_gribapi(this%gaid, field)
ENDIF
#endif
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal) THEN
!gdalid = grid_id_get_gdalid(this%gaid)
  CALL l4f_log(L4F_WARN,"export to gdal not implemented" )
! subarea?
ENDIF
#endif

END SUBROUTINE grid_id_encode_data


#ifdef HAVE_LIBGRIBAPI
SUBROUTINE grid_id_decode_data_gribapi(gaid, field)
INTEGER,INTENT(in) :: gaid ! grib_api id
REAL,INTENT(out) :: field(:,:) ! array of decoded values

INTEGER :: EditionNumber
INTEGER :: alternativeRowScanning, &
 iScansNegatively, jScansPositively, jPointsAreConsecutive
INTEGER :: numberOfValues,numberOfPoints
REAL :: vector(SIZE(field))
INTEGER :: x1, x2, xs, y1, y2, ys, ord(2)


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 2) then

  call grib_get(gaid,'alternativeRowScanning',alternativeRowScanning)
  if (alternativeRowScanning /= 0)then
    call l4f_log(L4F_ERROR, "grib_api alternativeRowScanning not supported: " &
     //t2c(alternativeRowScanning))
    call raise_error()
    field(:,:) = rmiss
    RETURN
  end if

else if (EditionNumber /= 1) then

  CALL l4f_log(L4F_ERROR, &
   "grib_api GribEditionNumber not supported: "//t2c(EditionNumber))
  call raise_error()
  field(:,:) = rmiss
  RETURN

end if

call grib_get(gaid,'iScansNegatively',iScansNegatively)
call grib_get(gaid,'jScansPositively',jScansPositively)
call grib_get(gaid,'jPointsAreConsecutive',jPointsAreConsecutive)

call grib_get(gaid,'numberOfPoints',numberOfPoints)
call grib_get(gaid,'numberOfValues',numberOfValues)

IF (numberOfPoints /= SIZE(field)) THEN
  CALL l4f_log(L4F_ERROR, 'grid_id_decode_data_gribapi numberOfPoints and grid size different')
  CALL l4f_log(L4F_ERROR, 'grid_id_decode_data_gribapi numberOfPoints: ' &
   //t2c(numberOfPoints)//', nx,ny: '&
   //t2c(SIZE(field,1))//','//t2c(SIZE(field,2)))
  CALL raise_error()
  field(:,:) = rmiss
  RETURN
ENDIF

! get data values
#ifdef DEBUG
call l4f_log(L4F_INFO,'grib_api number of values: '//to_char(numberOfValues))
call l4f_log(L4F_INFO,'grib_api number of points: '//to_char(numberOfPoints))
#endif

CALL grib_set(gaid,'missingValue',rmiss)
CALL grib_get(gaid,'values',vector)
! suspect bug in grib_api, when all field is missing it is set to zero
IF (numberOfValues == 0) vector = rmiss

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, 'grib_api, decoded field in interval: '// &
 t2c(MINVAL(vector,mask=c_e(vector)))//' '//t2c(MAXVAL(vector,mask=c_e(vector))))
CALL l4f_log(L4F_DEBUG, 'grib_api, decoded field with number of missing: '// &
 t2c(COUNT(.NOT.c_e(vector))))
#endif

IF (numberOfValues /= COUNT(c_e(vector))) THEN
  CALL l4f_log(L4F_WARN, 'grid_id_decode_data_gribapi numberOfValues and valid data count different')
  CALL l4f_log(L4F_WARN, 'grid_id_decode_data_gribapi numberOfValues: ' &
   //t2c(numberOfValues)//', valid data: '//t2c(COUNT(c_e(vector))))
!  CALL raise_warning()
ENDIF

! Transfer data field changing scanning mode to 64
IF (iScansNegatively  == 0) THEN
  x1 = 1
  x2 = SIZE(field,1)
  xs = 1
ELSE
  x1 = SIZE(field,1)
  x2 = 1
  xs = -1
ENDIF
IF (jScansPositively == 0) THEN
  y1 = SIZE(field,2)
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = SIZE(field,2)
  ys = 1
ENDIF

IF ( jPointsAreConsecutive == 0) THEN
  ord = (/1,2/)
ELSE
  ord = (/2,1/)
ENDIF

field(x1:x2:xs,y1:y2:ys) = RESHAPE(vector, &
 (/SIZE(field,1),SIZE(field,2)/), ORDER=ord)

END SUBROUTINE grid_id_decode_data_gribapi


SUBROUTINE grid_id_encode_data_gribapi(gaid, field)
INTEGER,INTENT(in) :: gaid ! grib_api id
REAL,intent(in) :: field(:,:) ! data array to be encoded

INTEGER :: EditionNumber
INTEGER :: alternativeRowScanning, iScansNegatively, &
 jScansPositively, jPointsAreConsecutive
INTEGER :: x1, x2, xs, y1, y2, ys


call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 2) then

  call grib_get(gaid,'alternativeRowScanning',alternativeRowScanning)
  if (alternativeRowScanning /= 0)then
    call l4f_log(L4F_ERROR, "grib_api alternativeRowScanning not supported: " &
     //TRIM(to_char(alternativeRowScanning)))
    call raise_error()
    RETURN
  end if

else if( EditionNumber /= 1) then

  call l4f_log(L4F_ERROR, &
   "grib_api GribEditionNumber not supported: "//t2c(EditionNumber))
  call raise_error()
  RETURN

end if

call grib_get(gaid,'iScansNegatively',iScansNegatively)
call grib_get(gaid,'jScansPositively',jScansPositively)
call grib_get(gaid,'jPointsAreConsecutive',jPointsAreConsecutive)

! queste sono gia` fatte in export_gridinfo, si potrebbero evitare?!
#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, 'grib_api, Ni,Nj:'//t2c(SIZE(field,1))//','//t2c(SIZE(field,2)))
#endif
call grib_set(gaid,'Ni',SIZE(field,1))
call grib_set(gaid,'Nj',SIZE(field,2))

! Transfer data field changing scanning mode from 64
IF (iScansNegatively  == 0) THEN
  x1 = 1
  x2 = SIZE(field,1)
  xs = 1
ELSE
  x1 = SIZE(field,1)
  x2 = 1
  xs = -1
ENDIF
IF (jScansPositively == 0) THEN
  y1 = SIZE(field,2)
  y2 = 1
  ys = -1
ELSE
  y1 = 1
  y2 = SIZE(field,2)
  ys = 1
ENDIF


if (any(field == rmiss)) then

  call grib_set(gaid,'missingValue',rmiss)
  if (editionNumber == 1) then
! enable bitmap in grib1
! grib_api 1.9.9 goes into an infinite loop with second order packing here
    CALL grib_set(gaid,'packingType','grid_simple')
    call grib_set(gaid,"bitmapPresent",1)
  else
! enable bitmap in grib2
    call grib_set(gaid,"bitMapIndicator",0)
  endif

else

  if (editionNumber == 1) then
! disable bitmap in grib1
    call grib_set(gaid,"bitmapPresent",0)
  else
! disable bitmap in grib2
    call grib_set(gaid,"bitMapIndicator",255)
  endif

end if

!TODO: gestire il caso TUTTI dati mancanti

#ifdef DEBUG
CALL l4f_log(L4F_DEBUG, 'grib_api, coding field in interval: '// &
 t2c(MINVAL(field,mask=c_e(field)))//' '//t2c(MAXVAL(field,mask=c_e(field))))
CALL l4f_log(L4F_DEBUG, 'grib_api, coding field with number of missing: '// &
 t2c(COUNT(.NOT.c_e(field))))
CALL l4f_log(L4F_DEBUG, 'grib_api, sizex:'//t2c(x1)//','//t2c(x2)//','//t2c(xs))
CALL l4f_log(L4F_DEBUG, 'grib_api, sizey:'//t2c(y1)//','//t2c(y2)//','//t2c(ys))
#endif
IF ( jPointsAreConsecutive == 0) THEN
  CALL grib_set(gaid,'values', RESHAPE(field(x1:x2:xs,y1:y2:ys), &
   (/SIZE(field)/)))
ELSE
  CALL grib_set(gaid,'values', RESHAPE(TRANSPOSE(field(x1:x2:xs,y1:y2:ys)), &
   (/SIZE(field)/)))
ENDIF

END SUBROUTINE grid_id_encode_data_gribapi
#endif


#ifdef HAVE_LIBGDAL
SUBROUTINE grid_id_decode_data_gdal(gdalid, field, gdal_options)
TYPE(gdalrasterbandh),INTENT(in) :: gdalid ! gdal id
REAL,INTENT(out) :: field(:,:) ! array of decoded values
TYPE(gdal_file_id_options),INTENT(in) :: gdal_options

TYPE(gdaldataseth) :: hds
REAL(kind=c_double) :: geotrans(6), dummy1, dummy2, dummy3, dummy4
REAL :: gdalmiss
REAL,ALLOCATABLE :: buffer(:,:)
INTEGER :: ix1, iy1, ix2, iy2, ixs, iys, ord(2), ier
INTEGER(kind=c_int) :: nrx, nry
LOGICAL :: must_trans


hds = gdalgetbanddataset(gdalid) ! go back to dataset
ier = gdalgetgeotransform(hds, geotrans)

IF (geotrans(3) == 0.0_c_double .AND. geotrans(5) == 0.0_c_double) THEN
! transformation is diagonal, no transposing
  IF (geotrans(2) > 0.0_c_double) THEN
    ix1 = 1 
    ix2 = SIZE(field,1)
    ixs = 1
  ELSE
    ix1 = SIZE(field,1)
    ix2 = 1 
    ixs = -1
  ENDIF
  IF (geotrans(6) > 0.0_c_double) THEN
    iy1 = 1 
    iy2 = SIZE(field,2)
    iys = 1
  ELSE
    iy1 = SIZE(field,2)
    iy2 = 1 
    iys = -1
  ENDIF
  nrx = SIZE(field,1)
  nry = SIZE(field,2)
  ord = (/1,2/)
  must_trans = .FALSE.
!  ALLOCATE(buffer(nrx,nry))

ELSE IF (geotrans(2) == 0.0_c_double .AND. geotrans(6) == 0.0_c_double) THEN
! transformation is anti-diagonal, transposing required this should not happen
  IF (geotrans(3) > 0.0_c_double) THEN
    ix1 = 1 
    ix2 = SIZE(field,1)
    ixs = 1
  ELSE
    ix1 = SIZE(field,1)
    ix2 = 1 
    ixs = -1
  ENDIF
  IF (geotrans(5) > 0.0_c_double) THEN
    iy1 = 1 
    iy2 = SIZE(field,2)
    iys = 1
  ELSE
    iy1 = SIZE(field,2)
    iy2 = 1 
    iys = -1
  ENDIF
  nrx = SIZE(field,2)
  nry = SIZE(field,1)
  ord = (/2,1/)
  must_trans = .TRUE.
!  ALLOCATE(buffer(nry,nrx))

ELSE ! transformation is a rotation, not supported
  CALL l4f_log(L4F_ERROR, 'gdal geotransform is a generic rotation, not supported')
  CALL raise_error()
  field(:,:) = rmiss
  RETURN
ENDIF

! read data from file
CALL gdalrastersimpleread_f(gdalid, gdal_options%xmin, gdal_options%ymin, &
 gdal_options%xmax, gdal_options%ymax, buffer, dummy1, dummy2, dummy3, dummy4)

IF (.NOT.ALLOCATED(buffer)) THEN ! error in read
  CALL l4f_log(L4F_ERROR, 'gdal error in reading with gdal driver')
  CALL raise_error()
  field(:,:) = rmiss
  RETURN
ENDIF

IF (SIZE(buffer) /= (SIZE(field)))THEN
  CALL l4f_log(L4F_ERROR, 'gdal raster band and gridinfo size different')
  CALL l4f_log(L4F_ERROR, 'gdal rasterband: ' &
   //t2c(SIZE(buffer,1))//'X'//t2c(SIZE(buffer,2))//', nx,ny:' &
   //t2c(SIZE(field,ord(1)))//'X'//t2c(SIZE(field,ord(2))))
  CALL raise_error()
  field(:,:) = rmiss
  RETURN
ENDIF

! set missing value if necessary
gdalmiss = REAL(gdalgetrasternodatavalue(gdalid, ier))
IF (ier /= 0) THEN ! success -> there are missing values
#ifdef DEBUG
  CALL l4f_log(L4F_INFO, 'gdal missing data value: '//TRIM(to_char(gdalmiss)))
#endif
  WHERE(buffer(:,:) == gdalmiss)
    buffer(:,:) = rmiss
  END WHERE
ELSE
#ifdef DEBUG
  CALL l4f_log(L4F_INFO, 'gdal no missing data found in band')
#endif
ENDIF

! reshape the field
IF (must_trans) THEN
  field(ix1:ix2:ixs,iy1:iy2:iys) = TRANSPOSE(buffer)
ELSE
  field(ix1:ix2:ixs,iy1:iy2:iys) = buffer(:,:)
ENDIF


END SUBROUTINE grid_id_decode_data_gdal
#endif

END MODULE grid_id_class
