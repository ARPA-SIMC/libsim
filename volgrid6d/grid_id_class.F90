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
USE log4fortran
USE err_handling
IMPLICIT NONE

!> Derived type associated to a file-like object containing many
!! blocks/messages/records/bands of gridded data.
TYPE grid_file_id
PRIVATE
#ifdef HAVE_LIBGRIBAPI
  INTEGER :: gaid
#endif
#ifdef HAVE_LIBGDAL
  TYPE(gdaldataseth) :: gdalid
  INTEGER :: nlastband
#endif
  INTEGER :: driver
END TYPE grid_file_id


!> Derived type associated to a block/message/record/band of gridded
!! data coming from a file-like object.
TYPE grid_id
PRIVATE
#ifdef HAVE_LIBGRIBAPI
  INTEGER :: gaid
#endif
#ifdef HAVE_LIBGDAL
  TYPE(gdalrasterbandh) :: gdalid
#endif
  INTEGER :: driver
END TYPE grid_id

INTEGER,PARAMETER :: grid_id_notype = imiss !< constants to be used for associating an object to a driver: no type specified
INTEGER,PARAMETER :: grid_id_grib_api = 1 !< type grib_api specified
INTEGER,PARAMETER :: grid_id_gdal = 2 !< type gdal specified

CHARACTER(len=12),PARAMETER :: driverlist(0:2) = &
 (/'none        ','grib_api    ','gdal        '/)

#if defined HAVE_LIBGRIBAPI
INTEGER,PARAMETER :: grid_id_default = grid_id_grib_api !< default driver if none specified in constructor
#elif defined HAVE_LIBGDAL
INTEGER,PARAMETER :: grid_id_default = grid_id_gdal !< default driver if none specified in constructor
#else
INTEGER,PARAMETER :: grid_id_default = grid_id_notype !< default driver if none specified in constructor
#endif

!> Constructor for the corresponding classes in SUBROUTINE form.
!! It is alternative to the *_new function constructors.
INTERFACE init
  MODULE PROCEDURE grid_file_id_init, grid_id_init
END INTERFACE

!> Destructor for the corresponding classes.
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
INTERFACE c_e
  MODULE PROCEDURE grid_id_c_e, grid_id_c_e_v, grid_file_id_c_e, grid_file_id_c_e_v
END INTERFACE

!> Display on standard output a description of the \a grid_id object
!! provided.
INTERFACE display
  MODULE PROCEDURE grid_id_display
END INTERFACE

PRIVATE grid_file_id_delete, grid_id_delete, grid_id_copy, &
 grid_id_c_e, grid_file_id_c_e, grid_id_display

CONTAINS


!> Constructor for the \a grid_file_id class. It opens the associated
!! file(s); the driver to be used for file access is selected
!! according to the \a filename argument, to the optional argument
!! \a driver, or to the optional argument \a from_grid_id, with increasing
!! priority. If \a driver and \a from_grid_id are not provided and
!! \a filename does not contain driver information, a default is
!! chosen. If filename is an empty string or missing value, the object
!! will be empty, the same will happen in case the file cannot be
!! successfully opened. This condition can be tested with the function
!! c_e() .
SUBROUTINE grid_file_id_init(this, filename, mode, driver, from_grid_id)
TYPE(grid_file_id),INTENT(out) :: this !< object to initilised
CHARACTER(len=*),INTENT(in) :: filename !< name of file containing gridded data, in the format [driver:]pathname
CHARACTER(len=*),INTENT(in) :: mode !< access mode for file, 'r' or 'w'
INTEGER,INTENT(in),OPTIONAL :: driver !< select the driver that will be associated to the grid_file_id created, use the constants \a grid_id_notype, \a grid_id_grib_api, \a grid_id_gdal
TYPE(grid_id),INTENT(in),OPTIONAL :: from_grid_id !< select the driver as the one associated to the provided grid_id object

this = grid_file_id_new(filename, mode, driver, from_grid_id)

END SUBROUTINE grid_file_id_init


!> Constructor for the \a grid_file_id class. It opens the associated
!! file(s); the driver to be used for file access is selected
!! according to the \a filename argument, to the optional argument
!! \a driver, or to the optional argument \a from_grid_id, with increasing
!! priority. If \a driver and \a from_grid_id are not provided and
!! \a filename does not contain driver information, a default is
!! chosen. If filename is an empty string or missing value, the object
!! will be empty, the same will happen in case the file cannot be
!! successfully opened. This condition can be tested with the function
!! c_e() .
FUNCTION grid_file_id_new(filename, mode, driver, from_grid_id) RESULT(this)
CHARACTER(len=*),INTENT(in) :: filename !< name of file containing gridded data, in the format [driver:]pathname
CHARACTER(len=*),INTENT(in) :: mode !< access mode for file, 'r' or 'w'
INTEGER,INTENT(in),OPTIONAL :: driver !< select the driver that will be associated to the grid_file_id created, use the constants \a grid_id_notype, \a grid_id_grib_api, \a grid_id_gdal
TYPE(grid_id),INTENT(in),OPTIONAL :: from_grid_id !< select the driver as the one associated to the provided grid_id object
TYPE(grid_file_id) :: this

INTEGER :: n, imode, ier

#ifdef HAVE_LIBGRIBAPI
this%gaid = imiss
#endif
#ifdef HAVE_LIBGDAL
CALL gdalnullify(this%gdalid)
this%nlastband = 0
#endif
this%driver = grid_id_default ! start with default
IF (filename == '' .OR. .NOT.c_e(filename)) RETURN

n = INDEX(filename,':')
IF (n > 1) THEN ! override with driver from filename
#ifdef HAVE_LIBGRIBAPI
  IF (filename(1:n-1) == 'grib_api') THEN
    this%driver = grid_id_grib_api
  ENDIF
#endif
#ifdef HAVE_LIBGDAL
  IF (filename(1:n-1) == 'gdal') THEN
    this%driver = grid_id_gdal
  ENDIF
#endif
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
  this%gdalid = gdalopen(TRIM(filename(n+1:))//C_NULL_CHAR, imode)
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
  IF (gdalassociated(this%gdalid)) CALL gdalclose(this%gdalid)
ENDIF
CALL gdalnullify(this%gdalid)
#endif

this%driver = imiss

END SUBROUTINE grid_file_id_delete


!> Function to check whether a \a grid_file_id object has been correctly associated
!! to the requested file. It returns \a .FALSE. if the file has not
!! been opened correctly or if the object has been initialized as empty.
FUNCTION grid_file_id_c_e(this)
TYPE(grid_file_id),INTENT(in) :: this !< object to be checked
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


!> Function to check whether a \a grid_file_id object has been correctly associated
!! to the requested file. It returns \a .FALSE. if the file has not
!! been opened correctly or if the object has been initialized as empty.
FUNCTION grid_file_id_c_e_v(this)
TYPE(grid_file_id),INTENT(in) :: this(:) !< object to be checked
LOGICAL :: grid_file_id_c_e_v(SIZE(this))

INTEGER :: i

DO i = 1, SIZE(this)
  grid_file_id_c_e_v(i) = c_e(this(i))
ENDDO

END FUNCTION grid_file_id_c_e_v


!> Constructor for the \a grid_id class. It gets the next grid (grib
!! message or raster band) from the file_id provided. If the file
!! associated to the file_id provided contains no more grids, or if
!! the argument \a file_id is not provided, an empty object is
!! created; this condition can be tested with the function c_e().
!! Alternative ways to define the object (to be used in rare cases)
!! are through a grib_api template file name (\a grib_api_template
!! argument) or through a grib_api integer id obtained directly from
!! grib_api calls (\a grib_api_id argument).
SUBROUTINE grid_id_init(this, from_grid_file_id, grib_api_template, grib_api_id)
TYPE(grid_id),INTENT(out) :: this !< object to be initialized
TYPE(grid_file_id),INTENT(inout),OPTIONAL :: from_grid_file_id !< file object from which grid object has to be created
CHARACTER(len=*),INTENT(in),OPTIONAL :: grib_api_template !< grib_api template file from which grid_object has to be created
INTEGER,INTENT(in),OPTIONAL :: grib_api_id !< grib_api id obtained directly from a \a grib_get subroutine call

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
FUNCTION grid_id_new(from_grid_file_id, grib_api_template, grib_api_id) RESULT(this)
TYPE(grid_file_id),INTENT(inout),OPTIONAL :: from_grid_file_id !< file object from which grid object has to be created
CHARACTER(len=*),INTENT(in),OPTIONAL :: grib_api_template !< grib_api template file from which grid_object has to be created
INTEGER,INTENT(in),OPTIONAL :: grib_api_id !< grib_api id obtained directly from a \a grib_get subroutine call
TYPE(grid_id) :: this

INTEGER :: ier

#ifdef HAVE_LIBGRIBAPI
this%gaid = imiss
#endif
#ifdef HAVE_LIBGDAL
CALL gdalnullify(this%gdalid)
#endif

IF (PRESENT(from_grid_file_id)) THEN
this%driver = from_grid_file_id%driver
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
    IF (gdalassociated(from_grid_file_id%gdalid)) THEN
      IF (from_grid_file_id%nlastband < gdalgetrastercount(from_grid_file_id%gdalid)) THEN
        from_grid_file_id%nlastband = from_grid_file_id%nlastband + 1
        this%gdalid = &
         gdalgetrasterband(from_grid_file_id%gdalid, from_grid_file_id%nlastband)
      ENDIF
    ENDIF
  ENDIF
#endif

#ifdef HAVE_LIBGRIBAPI
ELSE IF (PRESENT(grib_api_template)) THEN
  this%driver = grid_id_grib_api
  CALL grib_new_from_template(this%gaid, grib_api_template, ier)
  IF (ier /= GRIB_SUCCESS) this%gaid = imiss
ELSE IF (PRESENT(grib_api_id)) THEN
  this%driver = grid_id_grib_api
  this%gaid = grib_api_id
#endif
ENDIF

END FUNCTION grid_id_new


!> Destructor for the \a grid_id class. It releases the memory associated with
!! the grid descriptor identifier. In grib_api this is necessary and
!! can be made also after closing the corresponding \a grid_file_id
!! object; while for gdal this is a no-operation.
SUBROUTINE grid_id_delete(this)
TYPE(grid_id),INTENT(inout) :: this !< object to be deleted

#ifdef HAVE_LIBGRIBAPI
IF (this%driver == grid_id_grib_api) THEN
  IF (c_e(this%gaid)) CALL grib_release(this%gaid)
ENDIF
this%gaid = imiss
#endif
#ifdef HAVE_LIBGDAL
CALL gdalnullify(this%gdalid)
#endif

this%driver = imiss

END SUBROUTINE grid_id_delete


!> Performs a "deep" copy of the \a grid_id object when possible.
!! For grib_api this clones the grid_id generating a new independent
!! object in memory, which can be manipulated without affecting the
!! original one. The \a grid_id object \a that does not need to be
!! initialized before the call.
SUBROUTINE grid_id_copy(this, that)
TYPE(grid_id),INTENT(in) :: this !< source object
TYPE(grid_id),INTENT(inout) :: that !< destination object, it must not be initialized

INTEGER :: gaid

that = grid_id_new()
that%driver = this%driver

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
  that%gdalid = this%gdalid ! better idea?
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
ENDIF
#endif
#ifdef HAVE_LIBGDAL
IF (this%driver == grid_id_gdal .AND. file_id%driver == grid_id_gdal) THEN
  ! not implemented, log?
ENDIF
#endif

END SUBROUTINE grid_id_export


!> Function to check whether a \a _file_id object has been correctly associated
!! to a grid. It returns \a .FALSE. if the grid has not been correctly
!! obtained from the file or if the object has been initialized as
!! empty.
FUNCTION grid_id_c_e(this)
TYPE(grid_id),INTENT(in) :: this !< object to be checked
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

END FUNCTION grid_id_c_e


!> Function to check whether a \a _file_id object has been correctly associated
!! to a grid. It returns \a .FALSE. if the grid has not been correctly
!! obtained from the file or if the object has been initialized as
!! empty.
FUNCTION grid_id_c_e_v(this)
TYPE(grid_id),INTENT(in) :: this(:) !< object to be checked
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
#endif


!> Decode and return the data array from a grid_id object.
!! The output array \a field must have a size matching the size of the
!! encoded data.
SUBROUTINE grid_id_decode_data(this, field)
TYPE(grid_id),INTENT(in) :: this
REAL,INTENT(out) :: field(:,:)

#ifdef HAVE_LIBGRIBAPI
IF (c_e(this%gaid)) CALL grid_id_decode_data_gribapi(this%gaid, field)
#endif
#ifdef HAVE_LIBGDAL
! subarea?
IF (gdalassociated(this%gdalid)) field(:,:) = gridinfo_decode_data_gdal(this, gdalid)
#endif

END SUBROUTINE grid_id_decode_data


!> Encode a data array into a grid_id object.
!! The input array \a field must have a size matching the size of the
!! dataset.
SUBROUTINE grid_id_encode_data(this, field)
TYPE(grid_id),INTENT(inout) :: this !< gridinfo object
REAL,intent(in) :: field(:,:) !< data array to be encoded

#ifdef HAVE_LIBGRIBAPI
IF (c_e(this%gaid)) CALL grid_id_encode_data_gribapi(this%gaid, field)
#endif
#ifdef HAVE_LIBGDAL
!gdalid = grid_id_get_gdalid(this%gaid)
call l4f_log(L4F_WARN,"export to gdal not implemented" )
! subarea?
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
     //TRIM(to_char(alternativeRowScanning)))
    call raise_error()
    field(:,:) = rmiss
    RETURN
  end if

else if (EditionNumber /= 1) then

  CALL l4f_log(L4F_ERROR, &
   "grib_api GribEditionNumber not supported: "//TRIM(to_char(EditionNumber)))
  call raise_error()
  field(:,:) = rmiss
  RETURN

end if

call grib_get(gaid,'iScansNegatively',iScansNegatively)
call grib_get(gaid,'jScansPositively',jScansPositively)
call grib_get(gaid,'jPointsAreConsecutive',jPointsAreConsecutive)

call grib_set(gaid,'missingValue',rmiss)
call grib_get(gaid,'numberOfPoints',numberOfPoints)
call grib_get(gaid,'numberOfValues',numberOfValues)

if (numberOfPoints /= SIZE(field))then
!if (numberOfValues /= SIZE(field))then

  CALL l4f_log(L4F_ERROR, 'grib_api numberOfPoints and grid size different')
  CALL l4f_log(L4F_ERROR, 'grib_api numberOfPoints: ' &
   //TRIM(to_char(numberOfPoints))//', nx,ny:'&
   //TRIM(to_char(SIZE(field,1)))//'X'//TRIM(to_char(SIZE(field,2))))
  call raise_error()
  field(:,:) = rmiss
  RETURN

end if

                            !     get data values
#ifdef DEBUG
call l4f_log(L4F_INFO,'grib_api number of values: '//to_char(numberOfValues))
call l4f_log(L4F_INFO,'grib_api number of points: '//to_char(numberOfPoints))
#endif

CALL grib_get(gaid,'values',vector)

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
INTEGER :: nx, ny
INTEGER :: x1, x2, xs, y1, y2, ys, ord(2)


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
   "grib_api GribEditionNumber not supported: "//TRIM(to_char(EditionNumber)))
  call raise_error()
  RETURN

end if

call grib_get(gaid,'iScansNegatively',iScansNegatively)
call grib_get(gaid,'jScansPositively',jScansPositively)
call grib_get(gaid,'jPointsAreConsecutive',jPointsAreConsecutive)

! queste sono gia` fatte in export_gridinfo, si potrebbero evitare?!
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


if (any(field== rmiss)) then

  call grib_set(gaid,'missingValue',rmiss)
  
  call grib_get(gaid,'editionNumber',editionNumber);
  if (editionNumber == 1) then
                                ! enable bitmap in a grib1
    call grib_set(gaid,"bitmapPresent",1)
  else
                                ! enable bitmap in a grib2
    call grib_set(gaid,"bitMapIndicator",0)
  endif

else

  call grib_get(gaid,'editionNumber',editionNumber);
  if (editionNumber == 1) then
                                ! enable bitmap in a grib1
    call grib_set(gaid,"bitmapPresent",0)
  else
                                ! enable bitmap in a grib2
    call grib_set(gaid,"bitMapIndicator",1)
  endif

end if


!TODO: gestire il caso TUTTI dati mancanti

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
SUBROUTINE grid_id_decode_data_gdal(gdalid, field)
INTEGER,INTENT(in) :: gdalid ! gdal id
REAL,INTENT(out) :: field(:,:) ! array of decoded values

TYPE(gdaldataseth) :: hds
REAL(kind=c_double) :: geotrans(6), invgeotrans(6)
REAL :: vector(SIZE(field)), gdalmiss
INTEGER :: ix1, iy1, ix2, iy2, ixs, iys, ord(2), ier
INTEGER(kind=c_int) :: nrx, nry


nrx =  gdalgetrasterbandxsize(gdalid)
nry =  gdalgetrasterbandysize(gdalid)

if (nrx*nry /= (SIZE(field)))then

  CALL l4f_log(L4F_ERROR, 'gdal raster band and gridinfo size different')
  CALL l4f_log(L4F_ERROR, 'gdal rasterband: ' &
   //TRIM(to_char(nrx))//'X'//TRIM(to_char(nry))//', nx,ny:' &
   //TRIM(to_char(SIZE(field,1)))//'X'//TRIM(to_char(SIZE(field,2))))
  CALL raise_error()
  field(:,:) = rmiss
  RETURN

end if

hds = gdalgetbanddataset(gdalid) ! go back to dataset
ier = gdalgetgeotransform(hds, geotrans)
! get grid corners
!CALL gdalapplygeotransform(geotrans, 0.5_c_double, 0.5_c_double, x1, y1)
!CALL gdalapplygeotransform(geotrans, &
! SIZE(field,1)-0.5_c_double, SIZE(field,2)-0.5_c_double, x2, y2)

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

ELSE IF (geotrans(2) == 0.0_c_double .AND. geotrans(6) == 0.0_c_double) THEN
! transformation is anti-diagonal, transposing required
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
ELSE ! transformation is a rotation, not supported
  CALL l4f_log(L4F_ERROR, 'gdal geotransform is a generic rotation, not supported')
  CALL raise_error()
  field(:,:) = rmiss
  RETURN
ENDIF

! read data from file
ier = gdalrasterio_float32(gdalid, GF_Read, 0_c_int, 0_c_int, nrx, nry, vector, nrx, nry)

IF (ier /= 0) THEN ! error in read
  CALL l4f_category_log(this%category,L4F_ERROR, &
   'gdal error in reading with gdal driver')
  CALL raise_error()
  vector(:) = rmiss
  RETURN
ELSE
! set missing value if necessary
  gdalmiss = gdalgetrasternodatavalue(gdalid, ier)
  IF (ier == 0) THEN ! success -> there are missing values
    WHERE(vector(:) == gdalmiss)
      vector(:) = rmiss
    END WHERE
  ENDIF
ENDIF

! reshape the field
field(ix1:ix2:ixs,iy1:iy2:iys) = &
 RESHAPE(vector, (/SIZE(field,1),SIZE(field,2)/), ORDER=ord)

END SUBROUTINE grid_id_decode_data_gdal
#endif

END MODULE grid_id_class