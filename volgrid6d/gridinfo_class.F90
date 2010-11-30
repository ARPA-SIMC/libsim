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
!> Class for managing information about a single gridded georeferenced
!! field, typically imported from an on-disk dataset like a grib file
!! (grib_api driver) or a file in a gdal-supported format (gdal
!! driver). This module defines a gridinfo (\a gridinfo_def TYPE)
!! class which can contain information about a single field on a
!! rectangular georeferenced grid, including:
!!
!! - geographical area and projection associated to the gridded data
!! - time dimension descriptor
!! - timerange (forecast, analysis, statistically processed) dimension
!!   descriptor
!! - vertical level dimension descriptor
!! - physical variable dimension descriptor
!! 
!! every object contains also an identificator of the grid (\a grid_id
!! object), carrying information about the driver used or which has to
!! be used for import/export from/to file. The identificator should be
!! associated to the gridinfo object at initialization time.
!!
!! The main methods of this class allow to:
!!
!! - extract (import methods) the information of the five dimension
!!   descriptors from the associated grid_id coming from a file
!!   (grib_api or gdal drivers)
!! - insert (export method) the information of the five dimension
!!   descriptors in the associated grid_id object, for successive
!!   write to a file (grib_api driver only)
!! - retrieve the two-dimensional field encoded into the grid_id
!!   associated to the gridinfo object
!! - encode a desired two-dimensional field into the grid_id
!!   associated to the gridinfo object
!!
!! Simple example of use: \include example_vg6d_2.f90
!! More complex example: \include example_vg6d_4.f90
!! \ingroup volgrid6d
module gridinfo_class

USE grid_class
USE datetime_class
USE vol7d_timerange_class
USE vol7d_level_class
USE volgrid6d_var_class
#ifdef HAVE_LIBGRIBAPI
USE grib_api
#endif
#ifdef HAVE_LIBGDAL
USE gdal
#endif
USE grid_id_class
use log4fortran
use optional_values


IMPLICIT NONE

character (len=255),parameter:: subcategory="gridinfo_class"


!> Object describing a single gridded message/band
TYPE gridinfo_def
  TYPE(griddim_def) :: griddim !< grid descriptor
  TYPE(datetime) :: time !< time dimension descriptor
  TYPE(vol7d_timerange) :: timerange !< timerange (forecast, analysis, statistically processed) dimension descriptor
  TYPE(vol7d_level) :: level !< vertical level dimension descriptor
  TYPE(volgrid6d_var) :: var !< physical variable dimension descriptor
  TYPE(grid_id) :: gaid !< grid identificator, carrying information about the driver for importation/exportation from/to file
  INTEGER :: category !< log4fortran category
END TYPE gridinfo_def

INTEGER, PARAMETER :: cosmo_centre(3) = (/78,80,200/) ! emission centers using COSMO

!> Constructor, it creates a new instance of the object.
INTERFACE init
  MODULE PROCEDURE gridinfo_init
END INTERFACE

!> Destructor, it releases every information associated with the object.
INTERFACE delete
  MODULE PROCEDURE gridinfo_delete
END INTERFACE

!> Clone the object, creating a new independent instance of the object
!! exactly equal to the starting one.
INTERFACE clone
  MODULE PROCEDURE gridinfo_clone
END INTERFACE

!> Import information from a file or grid_id object into the gridinfo
!! descriptors.
INTERFACE import
  MODULE PROCEDURE gridinfo_import, gridinfo_import_from_file
!  MODULE PROCEDURE import_time,import_timerange,import_level,import_gridinfo, &
!   import_volgrid6d_var,gridinfo_import_from_file
END INTERFACE

!> Export gridinfo descriptors information into a grid_id object.
INTERFACE export
  MODULE PROCEDURE gridinfo_export, gridinfo_export_to_file
!  MODULE PROCEDURE export_time,export_timerange,export_level,export_gridinfo, &
!   export_volgrid6d_var
END INTERFACE

!> Display on standard output a description of the \a gridinfo object
!! provided.
INTERFACE display
  MODULE PROCEDURE gridinfo_display, gridinfov_display
END INTERFACE

!> Decode and return the data array from a grid_id object associated
!! to a gridinfo object.
INTERFACE decode_gridinfo
  MODULE PROCEDURE gridinfo_decode_data
END INTERFACE

!> Encode a data array into a grid_id object associated to a gridinfo object.
INTERFACE encode_gridinfo
  MODULE PROCEDURE gridinfo_encode_data
END INTERFACE

PRIVATE
PUBLIC gridinfo_def,init,delete,import,export,clone
PUBLIC display,decode_gridinfo,encode_gridinfo

CONTAINS

!> Constructor, it creates a new instance of the object.
!! All the additional parameters are optional and they will be
!! initialised to the corresponding missing value if not provided.
SUBROUTINE gridinfo_init(this, gaid, griddim, time, timerange, level, var, &
 clone, categoryappend)
TYPE(gridinfo_def),intent(out) :: this !< object to be initialized
TYPE(grid_id),intent(in),optional :: gaid !< identificator of the grid to be described 
type(griddim_def),intent(in),optional :: griddim !< grid descriptor
TYPE(datetime),intent(in),optional :: time !< time dimension descriptor
TYPE(vol7d_timerange),intent(in),optional :: timerange !< timerange (forecast, analysis, statistically processed) dimension descriptor
TYPE(vol7d_level),intent(in),optional :: level !< vertical level dimension descriptor
TYPE(volgrid6d_var),intent(in),optional :: var !< physical variable dimension descriptor
logical , intent(in),optional :: clone !< if provided and \c .TRUE., the \a gaid will be cloned and not simply copied into the gridinfo object
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

character(len=512) :: a_name

if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
end if
this%category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start init gridinfo")
#endif

if (present(gaid))then
  if (optio_log(clone))then
    CALL copy(gaid,this%gaid)
  else
    this%gaid = gaid
  endif
else
  this%gaid = grid_id_new()
end if

!#ifdef DEBUG
!call l4f_category_log(this%category,L4F_DEBUG,"gaid present: "&
! //to_char(c_e(this%gaid))//" value: "//to_char(this%gaid))
!#endif

if (present(griddim))then
  call copy(griddim,this%griddim)
else
  call init(this%griddim,categoryappend=categoryappend)
end if

if (present(time))then
  this%time=time
else
  call init(this%time)
end if

if (present(timerange))then
  this%timerange=timerange
else
  call init(this%timerange)
end if

if (present(level))then
  this%level=level
else
  call init(this%level)
end if

if (present(var))then
  this%var=var
else
  call init(this%var)
end if

END SUBROUTINE gridinfo_init


!> Destructor, it releases every information associated with the object.
!! It releases memory and deletes the category for logging.
SUBROUTINE gridinfo_delete(this)
TYPE(gridinfo_def),intent(out) :: this !< object to be deleted

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start delete_gridinfo" )
#endif

call delete(this%griddim)
call delete(this%time)
call delete(this%timerange)
call delete(this%level)
call delete(this%var)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"delete gaid" )
#endif
CALL delete(this%gaid) 

!chiudo il logger
call l4f_category_delete(this%category)

END SUBROUTINE gridinfo_delete


!> Display on standard output a description of the \a gridinfo object
!! provided.  For objects imported from grib_api also the grib key
!! names and values are printed; the set of keys returned can be
!! controlled with the input variable namespace. Available namespaces
!! are "ls", to get the same default keys as the grib_ls command, and
!! "mars" to get the keys used by mars, default "ls".
SUBROUTINE gridinfo_display(this, namespace)
TYPE(gridinfo_def),intent(in) :: this !< object to display
CHARACTER (len=*),OPTIONAL :: namespace !< grib_api namespace of the keys to search for, all the keys if empty, default "ls"

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"displaying gridinfo " )
#endif

print*,"----------------------- gridinfo display ---------------------"
call display(this%griddim)
call display(this%time)
call display(this%timerange)
call display(this%level)
call display(this%var)
call display(this%gaid, namespace=namespace)
print*,"--------------------------------------------------------------"

END SUBROUTINE gridinfo_display

!> The same as gridinfo_display(), but it receives an array of
!! gridinfo objects.
SUBROUTINE gridinfov_display(this, namespace)
TYPE(gridinfo_def),INTENT(in) :: this(:) !< object to display
CHARACTER (len=*),OPTIONAL :: namespace !< grib_api namespace of the keys to search for, all the keys if empty, default "ls"

INTEGER :: i

PRINT*,"----------------------- gridinfo array -----------------------"

do i=1, size(this)

#ifdef DEBUG
  CALL l4f_category_log(this(i)%category,L4F_DEBUG, &
   "displaying gridinfo array, element "//TRIM(to_char(i)))
#endif

  call display(this(i), namespace)

end do
print*,"--------------------------------------------------------------"

END SUBROUTINE gridinfov_display


!> Clone the object, creating a new independent instance of the object
!! exactly equal to the starting one.
SUBROUTINE gridinfo_clone(this, that, categoryappend)
TYPE(gridinfo_def),INTENT(in) :: this !< source object
TYPE(gridinfo_def),INTENT(out) :: that !< cloned object, it does not need to be initalized
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category for the cloned object

CALL init(that, gaid=this%gaid, griddim=this%griddim, time=this%time, &
 timerange=this%timerange, level=this%level, var=this%var, clone=.TRUE., &
 categoryappend=categoryappend)

END SUBROUTINE gridinfo_clone


!> Import grid_id information into a gridinfo object.
!! This method imports into the descriptors of the gridinfo object \a
!! this the information carried on by the grid_id object \a this%gaid,
!! previously set, typically by reading from a file with a supported
!! driver (e.g. grib_api or gdal). An amount of information is deduced
!! from this%gaid and stored in the descriptors of gridinfo object \a
!! this.
SUBROUTINE gridinfo_import(this)
TYPE(gridinfo_def),INTENT(inout) :: this !< gridinfo object

#ifdef HAVE_LIBGRIBAPI
INTEGER :: gaid
#endif
#ifdef HAVE_LIBGDAL
TYPE(gdalrasterbandh) :: gdalid
#endif

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"import from gaid")
#endif

! griddim is imported separately in grid_class
CALL import(this%griddim, this%gaid)

#ifdef HAVE_LIBGRIBAPI
gaid = grid_id_get_gaid(this%gaid)
IF (c_e(gaid)) CALL gridinfo_import_gribapi(this, gaid)
#endif
#ifdef HAVE_LIBGDAL
gdalid = grid_id_get_gdalid(this%gaid)
IF (gdalassociated(gdalid)) CALL gridinfo_import_gdal(this, gdalid)
#endif

END SUBROUTINE gridinfo_import


!> Import a gridinfo array from a file. It receives a pointer to an array of
!! gridinfo objects which will be allocated to a size equal to the
!! number of gridded messages/bands found in the file provided and it
!! will be filled with all the data found. In case of error, the
!! gridinfo object will not be allocated, so the success can be tested
!! with \a ASSOCIATED(this).
SUBROUTINE gridinfo_import_from_file(this, filename, categoryappend)
TYPE(gridinfo_def),POINTER :: this(:) !< gridinfo array object which will be allocated and into which data will be imported
CHARACTER(len=*),INTENT(in) :: filename !< name of file to open and import, in the form [driver:]pathname
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: lunit, ngrid, stallo, category, gaid, iret
CHARACTER(len=512) :: a_name
TYPE(grid_file_id) :: input_file
TYPE(grid_id) :: input_grid

IF (PRESENT(categoryappend)) THEN
  CALL l4f_launcher(a_name,a_name_append= &
   TRIM(subcategory)//"."//TRIM(categoryappend))
ELSE
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory))
ENDIF
category=l4f_category_get(a_name)

#ifdef DEBUG
CALL l4f_category_log(category,L4F_DEBUG,"import from file")
#endif

NULLIFY(this)

input_file = grid_file_id_new(filename, 'r')

if(c_e(input_file)) then
  ngrid = grid_file_id_count(input_file)
else
  CALL l4f_category_log(category,L4F_ERROR, &
   "error opening file: "//TRIM(filename))  
  CALL raise_error()
  RETURN
end if

CALL l4f_category_log(category,L4F_INFO, &
 "found "//TRIM(to_char(ngrid))//" messages/bands in file "//TRIM(filename))

IF (ngrid > 0) THEN

  ALLOCATE (this(ngrid),stat=stallo)
  IF (stallo == 0) THEN

    ngrid = 0
    DO WHILE(.TRUE.)
      input_grid = grid_id_new(input_file)
      IF (.NOT. c_e(input_grid)) EXIT
      IF (ngrid == SIZE(this)) EXIT

      CALL l4f_category_log(category,L4F_INFO,"import gridinfo")
      ngrid = ngrid + 1
      CALL init(this(ngrid), gaid=input_grid, &
       categoryappend=TRIM(categoryappend)//TRIM(to_char(ngrid)))
      CALL import(this(ngrid))
! input_grid is intentionally not destroyed, since now it lives into this
    ENDDO

  ELSE

    NULLIFY(this)

    CALL l4f_category_log(category,L4F_ERROR,"allocating memory")
    CALL raise_error()

  ENDIF

ENDIF

! attenzione, modificare:!!!!
CALL delete(input_file)
!chiudo il logger
CALL l4f_category_delete(category)

END SUBROUTINE gridinfo_import_from_file


!> Export gridinfo descriptors information into a message/band on file.
!! This method exports the contents of the descriptors of the gridinfo
!! object \a this in the grid_id object \a this%gaid, previously set,
!! for the successive write to a file. The information stored in the
!! descriptors of gridinfo object \a this is inserted, when possible,
!! in the grid_id object.
SUBROUTINE gridinfo_export(this)
TYPE(gridinfo_def),INTENT(inout) :: this !< gridinfo object

#ifdef HAVE_LIBGRIBAPI
INTEGER :: gaid
#endif
#ifdef HAVE_LIBGDAL
TYPE(gdalrasterbandh) :: gdalid
#endif

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"export to gaid" )
#endif

! attenzione: exportando da volgrid griddim è già esportato
! griddim is exported separately in grid_class
CALL export(this%griddim, this%gaid)

#ifdef HAVE_LIBGRIBAPI
IF (grid_id_get_driver(this%gaid) == 'grib_api') THEN
  gaid = grid_id_get_gaid(this%gaid)
  IF (c_e(gaid)) CALL gridinfo_export_gribapi(this, gaid)
ENDIF
#endif
#ifdef HAVE_LIBGDAL
IF (grid_id_get_driver(this%gaid) == 'gdal') THEN
!gdalid = grid_id_get_gdalid(this%gaid)
  CALL l4f_category_log(this%category,L4F_WARN,"export to gdal not implemented" )
ENDIF
#endif

END SUBROUTINE gridinfo_export


!> Export a gridinfo array to a file. It receives an array of
!! gridinfo objects which will be exported to the given file. The
!! driver for writing to file is chosen according to the gaid
!! associated to the first gridinfo elements, and it must be the same
!! for all the elements.
SUBROUTINE gridinfo_export_to_file(this, filename, categoryappend)
TYPE(gridinfo_def) :: this(:) !< gridinfo array object which will be written to file
CHARACTER(len=*),INTENT(in) :: filename !< name of file to open and import, in the form [driver:]pathname
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: i, category
CHARACTER(len=512) :: a_name
TYPE(grid_file_id) :: output_file

IF (PRESENT(categoryappend)) THEN
  CALL l4f_launcher(a_name,a_name_append= &
   TRIM(subcategory)//"."//TRIM(categoryappend))
ELSE
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory))
ENDIF
category=l4f_category_get(a_name)

#ifdef DEBUG
CALL l4f_category_log(category,L4F_DEBUG, &
 "exporting to file "//TRIM(filename)//" "//TRIM(to_char(SIZE(this)))//" fields")
#endif

IF (SIZE(this) > 0) THEN

  ! open file
  output_file = grid_file_id_new(filename, 'w', from_grid_id=this(1)%gaid)
  IF (c_e(output_file)) THEN
    DO i = 1, SIZE(this)
      CALL export(this(i)) ! export information to gaid
      CALL export(this(i)%gaid, output_file) ! export gaid to file
    ENDDO
    ! close file
    CALL delete(output_file)

  ELSE
    CALL l4f_category_log(category,L4F_ERROR,"opening file "//TRIM(filename))
    CALL raise_error()
  ENDIF ! filename opened
ELSE
  CALL l4f_category_log(category,L4F_WARN,"empty gridinfo object, file not created")
ENDIF ! SIZE(this)

!chiudo il logger
CALL l4f_category_delete(category)

END SUBROUTINE gridinfo_export_to_file


!> Decode and return the data array from a grid_id object associated
!! to a gridinfo object. This method returns a 2-d array of proper
!! size extracted from the grid_id object associated to a gridinfo
!! object.  This can work if the gridinfo object has been correctly
!! initialised and associated to a grid from an on-disk dataset
!! (e.g. grib_api or gdal file). The result is an array of size \a
!! this%griddim%dim%nx X \a this%griddim%dim%ny so it must have been
!! properly allocated by the caller.
FUNCTION gridinfo_decode_data(this) RESULT(field)
TYPE(gridinfo_def),INTENT(in) :: this !< gridinfo object
REAL :: field(this%griddim%dim%nx, this%griddim%dim%ny) ! array of decoded values

CALL grid_id_decode_data(this%gaid, field)

END FUNCTION gridinfo_decode_data


!> Encode a data array into a grid_id object associated to a gridinfo object.
!! This method encodes a 2-d array of proper size into the grid_id
!! object associated to a gridinfo object.  This can work if the
!! gridinfo object has been correctly initialised and associated to a
!! grid_id from an on-disk (template) dataset (grib_api or gdal
!! file). The shape of the array must be conformal to the size of the
!! grid previously set in the gridinfo object descriptors.
SUBROUTINE gridinfo_encode_data(this, field)
TYPE(gridinfo_def),INTENT(inout) :: this !< gridinfo object
REAL,intent(in) :: field(:,:) !< data array to be encoded

IF (SIZE(field,1) /= this%griddim%dim%nx &
 .OR. SIZE(field,2) /= this%griddim%dim%ny) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   'gridinfo_encode: field and gridinfo object non conformal, field: ' &
   //TRIM(to_char(SIZE(field,1)))//'X'//TRIM(to_char(SIZE(field,1)))//', nx,ny:' &
   //TRIM(to_char(this%griddim%dim%nx))//'X'//TRIM(to_char(this%griddim%dim%ny)))
  CALL raise_error()
  RETURN
ENDIF

CALL grid_id_encode_data(this%gaid, field)

END SUBROUTINE gridinfo_encode_data


! =========================================
! grib_api driver specific code
! could this be moved to a separate module?
! =========================================
#ifdef HAVE_LIBGRIBAPI
SUBROUTINE gridinfo_import_gribapi(this, gaid)
TYPE(gridinfo_def),INTENT(inout) :: this ! gridinfo object
INTEGER, INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to import

call time_import_gribapi(this%time, gaid)
call timerange_import_gribapi(this%timerange,gaid)
call level_import_gribapi(this%level, gaid)
call var_import_gribapi(this%var, gaid)

call normalize_gridinfo(this)

END SUBROUTINE gridinfo_import_gribapi


! grib_api driver
SUBROUTINE gridinfo_export_gribapi(this, gaid)
TYPE(gridinfo_def),INTENT(inout) :: this ! gridinfo object
INTEGER, INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to import

CALL unnormalize_gridinfo(this)

CALL time_export_gribapi(this%time, gaid, this%timerange)
CALL timerange_export_gribapi(this%timerange, gaid, this%time)
CALL level_export_gribapi(this%level, gaid)
CALL var_export_gribapi(this%var, gaid)

END SUBROUTINE gridinfo_export_gribapi


SUBROUTINE time_import_gribapi(this,gaid)
TYPE(datetime),INTENT(out) :: this ! datetime object
INTEGER,INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to import 

INTEGER :: EditionNumber, ttimeincr, p2, unit, status
CHARACTER(len=9) :: date
CHARACTER(len=10) :: time

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

IF (EditionNumber == 1 .OR. EditionNumber == 2) THEN

  call grib_get(gaid,'dataDate',date )
  call grib_get(gaid,'dataTime',time(:5) )

  call init(this,simpledate=date(:8)//time(:4))

  IF (EditionNumber == 2) THEN

    CALL grib_get(gaid,'typeOfTimeIncrement',ttimeincr,status)
! if analysis-like statistically processed data is encountered, the
! reference time must be shifted to the end of the processing period
    IF (status == GRIB_SUCCESS .AND. ttimeincr == 1) THEN
      CALL grib_get(gaid,'lengthOfTimeRange',p2) 
      CALL grib_get(gaid,'indicatorOfUnitForTimeRange',unit)
      CALL gribtr_to_second(unit,p2,p2)
      this = this + timedelta_new(msec=p2*1000)
    ENDIF
  ENDIF

else
  call l4f_log(L4F_ERROR,'GribEditionNumber not supported')
  CALL raise_error()

end if

END SUBROUTINE time_import_gribapi


SUBROUTINE time_export_gribapi(this, gaid, timerange)
TYPE(datetime),INTENT(in) :: this ! datetime object
INTEGER,INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to export
TYPE(vol7d_timerange) :: timerange ! timerange, used for grib2 coding of statistically processed analysed data

INTEGER :: EditionNumber

CALL grib_get(gaid,'GRIBEditionNumber',EditionNumber)

IF (EditionNumber == 1) THEN

  CALL code_referencetime(this)

ELSE IF (EditionNumber == 2 )THEN

  IF (timerange%p1 >= timerange%p2) THEN ! forecast-like
    CALL code_referencetime(this)
  ELSE IF (timerange%p1 == 0) THEN ! analysis-like
    CALL code_referencetime(this-timedelta_new(msec=timerange%p2*1000))
  ELSE ! bad timerange
    CALL l4f_log( L4F_ERROR, 'Timerange with 0>p1>p2 cannot be exported in grib2')
    CALL raise_error()
  ENDIF

ELSE

  CALL l4f_log( L4F_ERROR, &
   'GribEditionNumber not supported: '//TRIM(to_char(EditionNumber)))
  CALL raise_error()

ENDIF

CONTAINS

SUBROUTINE code_referencetime(reftime)
TYPE(datetime),INTENT(in) :: reftime

INTEGER :: date,time
CHARACTER(len=17) :: date_time

! datetime is AAAAMMGGhhmmssmsc
CALL getval(reftime, simpledate=date_time)
READ(date_time(:8),'(I8)')date
READ(date_time(9:12),'(I4)')time
CALL grib_set(gaid,'dataDate',date)
CALL grib_set(gaid,'dataTime',time)

END SUBROUTINE code_referencetime

END SUBROUTINE time_export_gribapi


SUBROUTINE level_import_gribapi(this, gaid)
TYPE(vol7d_level),INTENT(out) :: this ! vol7d_level object
INTEGER,INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to import

INTEGER :: EditionNumber,level1,l1,level2,l2
INTEGER :: ltype,ltype1,scalef1,scalev1,ltype2,scalef2,scalev2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then
  
  call grib_get(gaid,'indicatorOfTypeOfLevel',ltype)
  call grib_get(gaid,'topLevel',l1)
  call grib_get(gaid,'bottomLevel',l2)

  call level_g1_to_g2(ltype,l1,l2,ltype1,scalef1,scalev1,ltype2,scalef2,scalev2)

else if (EditionNumber == 2)then

  call grib_get(gaid,'typeOfFirstFixedSurface',ltype1)
  call grib_get(gaid,'scaleFactorOfFirstFixedSurface',scalef1)
  call grib_get(gaid,'scaledValueOfFirstFixedSurface',scalev1)

  call grib_get(gaid,'typeOfSecondFixedSurface',ltype2)
  call grib_get(gaid,'scaleFactorOfSecondFixedSurface',scalef2)
  call grib_get(gaid,'scaledValueOfSecondFixedSurface',scalev2)

else

  call l4f_log(L4F_ERROR,'GribEditionNumber not supported')
  call raise_error()

end if

! Convert missing levels and units m -> mm
call level_g2_to_dballe(ltype1,scalef1,scalev1,ltype2,scalef2,scalev2, &
 level1,l1,level2,l2)

call init (this,level1,l1,level2,l2)

END SUBROUTINE level_import_gribapi


SUBROUTINE level_export_gribapi(this, gaid)
TYPE(vol7d_level),INTENT(in) :: this ! vol7d_level object
INTEGER,INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to export

INTEGER :: EditionNumber, ltype1, scalef1, scalev1, ltype2, scalef2, scalev2, &
 ltype, l1, l2

CALL level_dballe_to_g2(this%level1, this%l1, this%level2, this%l2, &
 ltype1, scalef1, scalev1, ltype2, scalef2, scalev2)

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  CALL level_g2_to_g1(ltype1,scalef1,scalev1,ltype2,scalef2,scalev2,ltype,l1,l2)
  call grib_set(gaid,'indicatorOfTypeOfLevel',ltype)
! it is important to set topLevel after, otherwise, in case of single levels
! bottomLevel=0 overwrites topLevel (aliases in grib_api)
  call grib_set(gaid,'bottomLevel',l2)
  call grib_set(gaid,'topLevel',l1)

else if (EditionNumber == 2)then

  call grib_set(gaid,'typeOfFirstFixedSurface',ltype1)
  call grib_set(gaid,'scaleFactorOfFirstFixedSurface',scalef1)
  call grib_set(gaid,'scaledValueOfFirstFixedSurface',scalev1)

  call grib_set(gaid,'typeOfSecondFixedSurface',ltype2)
  call grib_set(gaid,'scaleFactorOfSecondFixedSurface',scalef2)
  call grib_set(gaid,'scaledValueOfSecondFixedSurface',scalev2)

else

  call l4f_log(L4F_ERROR,'GribEditionNumber not supported')
  call raise_error()

end if

END SUBROUTINE level_export_gribapi


SUBROUTINE timerange_import_gribapi(this, gaid)
TYPE(vol7d_timerange),INTENT(out) :: this ! vol7d_timerange object
INTEGER,INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to import

INTEGER :: EditionNumber, tri, unit, p1_g1, p2_g1, statproc, p1, p2, status

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1) then

  CALL grib_get(gaid,'timeRangeIndicator',tri)
  CALL grib_get(gaid,'P1',p1_g1)
  CALL grib_get(gaid,'P2',p2_g1)
  CALL grib_get(gaid,'indicatorOfUnitOfTimeRange',unit)
!  CALL grib_get(gaid,'startStepInHours',p1_g1)
!  CALL grib_get(gaid,'endStepInHours',p2_g1)
  CALL timerange_g1_to_g2_second(tri, p1_g1, p2_g1, unit, statproc, p1, p2)

ELSE IF (EditionNumber == 2) THEN
  
  CALL grib_get(gaid,'forecastTime',p1)
  CALL grib_get(gaid,'indicatorOfUnitOfTimeRange',unit)
  CALL gribtr_to_second(unit,p1,p1)
  call grib_get(gaid,'typeOfStatisticalProcessing',statproc,status)

  IF (status == GRIB_SUCCESS .AND. statproc >= 0 .AND. statproc <= 9) THEN ! statistically processed
    CALL grib_get(gaid,'lengthOfTimeRange',p2) 
    CALL grib_get(gaid,'indicatorOfUnitForTimeRange',unit)
    CALL gribtr_to_second(unit,p2,p2)

  ELSE ! point in time
    statproc = 254
    p2 = 0
  
  ENDIF

  p1 = p1 + p2 ! from start to end of time interval

else

  call l4f_log(L4F_ERROR,'GribEditionNumber not supported')
  call raise_error()

end if

call init(this, statproc, p1, p2)

END SUBROUTINE timerange_import_gribapi


SUBROUTINE timerange_export_gribapi(this, gaid, reftime)
TYPE(vol7d_timerange),INTENT(in) :: this ! vol7d_timerange object
INTEGER,INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to export
TYPE(datetime) :: reftime ! reference time of data, used for coding correct end of statistical processing period in grib2

INTEGER :: EditionNumber, tri, unit, p1_g1, p2_g1, p1, p2

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

IF (EditionNumber == 1 ) THEN
! Convert vol7d_timerange members to grib1 with reasonable time unit
  CALL timerange_g2_to_g1_unit(this%timerange, this%p1, this%p2, &
   tri, p1_g1, p2_g1, unit)
! Set the native keys
  CALL grib_set(gaid,'timeRangeIndicator',tri)
  CALL grib_set(gaid,'P1',p1_g1)
  CALL grib_set(gaid,'P2',p2_g1)
  CALL grib_set(gaid,'indicatorOfUnitOfTimeRange',unit)

else if (EditionNumber == 2) then

  IF (this%timerange == 254) THEN ! point in time -> template 4.0
    CALL grib_set(gaid,'productDefinitionTemplateNumber', 0)
! Set reasonable time unit
    CALL second_to_gribtr(this%p1,p1,unit)
! Set the native keys
    CALL grib_set(gaid,'indicatorOfUnitOfTimeRange',unit)
    CALL grib_set(gaid,'forecastTime',p1)

  ELSE IF (this%timerange >= 0 .AND. this%timerange <= 9) THEN
! statistically processed -> template 4.8
    CALL grib_set(gaid,'productDefinitionTemplateNumber', 8)

    IF (this%p1 >= this%p2) THEN ! forecast-like
! Set reasonable time unit
      CALL second_to_gribtr(this%p1-this%p2,p1,unit)
      CALL grib_set(gaid,'indicatorOfUnitOfTimeRange',unit)
      CALL grib_set(gaid,'forecastTime',p1)
      CALL code_endoftimeinterval(reftime+timedelta_new(msec=this%p2*1000))
! Successive times processed have same start time of forecast,
! forecast time is incremented
      CALL grib_set(gaid,'typeOfStatisticalProcessing',this%timerange)
      CALL grib_set(gaid,'typeOfTimeIncrement',2)
      CALL second_to_gribtr(this%p2,p2,unit)
      CALL grib_set(gaid,'indicatorOfUnitForTimeRange',unit)
      CALL grib_set(gaid,'lengthOfTimeRange',p2)

    ELSE IF (this%p1 == 0) THEN ! analysis-like
! Set reasonable time unit
      CALL second_to_gribtr(this%p2,p2,unit) ! use p2 (change initial time)
      CALL grib_set(gaid,'indicatorOfUnitOfTimeRange',unit)
      CALL grib_set(gaid,'forecastTime',0)
      CALL code_endoftimeinterval(reftime)
! Successive times processed have same forecast time, start time of
! forecast is incremented
      CALL grib_set(gaid,'typeOfStatisticalProcessing',this%timerange)
      CALL grib_set(gaid,'typeOfTimeIncrement',1)
      CALL grib_set(gaid,'indicatorOfUnitForTimeRange',unit)
      CALL grib_set(gaid,'lengthOfTimeRange',p2)

    ELSE ! bad timerange
      CALL l4f_log(L4F_ERROR, &
       'Timerange with 0>p1>p2 cannot be exported in grib2')
      CALL raise_error()
    ENDIF
  ELSE
    CALL l4f_log(L4F_ERROR, &
     'typeOfStatisticalProcessing not supported: '//TRIM(to_char(this%timerange)))
    CALL raise_error()
  ENDIF

ELSE
  CALL l4f_log(L4F_ERROR, &
   'GribEditionNumber not supported: '//TRIM(to_char(EditionNumber)))
  CALL raise_error()
ENDIF

CONTAINS

! Explicitely compute and code in grib2 template 4.8 the end of
! overalltimeinterval which is not done automatically by grib_api
SUBROUTINE code_endoftimeinterval(endtime)
TYPE(datetime),INTENT(in) :: endtime

INTEGER :: year, month, day, hour, minute, msec

CALL getval(endtime, year=year, month=month, day=day, &
 hour=hour, minute=minute, msec=msec)
  CALL grib_set(gaid,'yearOfEndOfOverallTimeInterval',year)
  CALL grib_set(gaid,'monthOfEndOfOverallTimeInterval',month)
  CALL grib_set(gaid,'dayOfEndOfOverallTimeInterval',day)
  CALL grib_set(gaid,'hourOfEndOfOverallTimeInterval',hour)
  CALL grib_set(gaid,'minuteOfEndOfOverallTimeInterval',minute)
  CALL grib_set(gaid,'secondOfEndOfOverallTimeInterval',msec/1000)

END SUBROUTINE code_endoftimeinterval

END SUBROUTINE timerange_export_gribapi


SUBROUTINE var_import_gribapi(this, gaid)
TYPE(volgrid6d_var),INTENT(out) :: this ! volgrid6d_var object
INTEGER,INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to import

INTEGER :: EditionNumber, centre, discipline, category, number

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  call grib_get(gaid,'identificationOfOriginatingGeneratingCentre',centre)
  call grib_get(gaid,'gribTablesVersionNo',category)
  call grib_get(gaid,'indicatorOfParameter',number)

  call init (this, centre, category, number)

else if (EditionNumber == 2)then

  call grib_get(gaid,'identificationOfOriginatingGeneratingCentre',centre)
  call grib_get(gaid,'discipline',discipline)
  call grib_get(gaid,'parameterCategory',category)
  call grib_get(gaid,'parameterNumber',number)

  call init (this, centre, category, number, discipline)
  
else

  call l4f_log(L4F_ERROR,'GribEditionNumber not supported')
  CALL raise_error()

end if
                                ! da capire come ottenere 
!this%description
!this%unit

END SUBROUTINE var_import_gribapi


SUBROUTINE var_export_gribapi(this, gaid)
TYPE(volgrid6d_var),INTENT(in) :: this ! volgrid6d_var object
INTEGER,INTENT(in) :: gaid ! grib_api id of the grib loaded in memory to export

INTEGER ::EditionNumber

call grib_get(gaid,'GRIBEditionNumber',EditionNumber)

if (EditionNumber == 1)then

  IF (this%centre /= 255) & ! if centre missing (coming from bufr), keep template
   CALL grib_set(gaid,'identificationOfOriginatingGeneratingCentre',this%centre)
  call grib_set(gaid,'gribTablesVersionNo',this%category)
  call grib_set(gaid,'indicatorOfParameter',this%number)

else if (EditionNumber == 2)then

  call grib_set(gaid,'identificationOfOriginatingGeneratingCentre',this%centre)
  call grib_set(gaid,'discipline',this%discipline)
  call grib_set(gaid,'parameterCategory',this%category)
  call grib_set(gaid,'parameterNumber',this%number)

else

  call l4f_log(L4F_ERROR,'GribEditionNumber not supported')
  CALL raise_error()

end if

END SUBROUTINE var_export_gribapi


SUBROUTINE level_g2_to_dballe(ltype1,scalef1,scalev1,ltype2,scalef2,scalev2, lt1,l1,lt2,l2)
integer,intent(in) :: ltype1,scalef1,scalev1,ltype2,scalef2,scalev2
integer,intent(out) ::lt1,l1,lt2,l2


CALL g2_to_dballe(ltype1, scalef1, scalev1, lt1, l1)
CALL g2_to_dballe(ltype2, scalef2, scalev2, lt2, l2)

CONTAINS

SUBROUTINE g2_to_dballe(ltype, scalef, scalev, lt, l)
integer,intent(in) :: ltype,scalef,scalev
integer,intent(out) ::lt,l

integer,parameter :: height(5)=(/102,103,106,117,160/)
doubleprecision:: sl

if (ltype == 255) then
  lt = imiss
  l = imiss
else
  lt = ltype
  sl = scalev*(10.D0**(-scalef))

  if (any(ltype == height)) then
    l = NINT(sl*1000.D0)
  else
    l = NINT(sl)
  end if
end if

END SUBROUTINE g2_to_dballe

END SUBROUTINE level_g2_to_dballe


SUBROUTINE level_dballe_to_g2(lt1,l1,lt2,l2, ltype1,scalef1,scalev1,ltype2,scalef2,scalev2)
integer,intent(in) ::lt1,l1,lt2,l2
integer,intent(out) :: ltype1,scalef1,scalev1,ltype2,scalef2,scalev2


CALL dballe_to_g2(lt1, l1, ltype1, scalef1, scalev1)
CALL dballe_to_g2(lt2, l2, ltype2, scalef2, scalev2)

CONTAINS

SUBROUTINE dballe_to_g2(lt, l, ltype, scalef, scalev)
integer,intent(in) ::lt,l
integer,intent(out) :: ltype,scalef,scalev

integer,parameter :: height(5)=(/102,103,106,117,160/)

if (lt == imiss) then
  ltype = 255
  scalev = 0
  scalef = 0
else
  ltype = lt
  scalev = l
  if (any(ltype == height)) then
    scalef = 3
  else
    scalef = 0
  end if
endif

!Caso generale reale
!IF (ANY(ltype == height)) THEN
!  sl=l/1000.D0
!ELSE
!  sl=l
!END IF
!IF (ABS(sl) < PRECISION) THEN
!  scalef = 0
!  scalev = 0
!ELSE
!  magn = LOG10(ABS(sl))
!  DO scalef = magn, MAX(magn, 20)
!    IF (ABS((sl*10.D0**(scalef) - ANINT(sl*10.D0**(scalef))) / &
!     sl*10.D0**(scalef)) < PRECISION) EXIT
!  ENDDO
!  sl = scalev*(10.D0**(-scalef))
!ENDIF

END SUBROUTINE dballe_to_g2

END SUBROUTINE level_dballe_to_g2


SUBROUTINE level_g1_to_g2(ltype,l1,l2,ltype1,scalef1,scalev1,ltype2,scalef2,scalev2)
integer,intent(in) :: ltype,l1,l2
integer,intent(out) :: ltype1,scalef1,scalev1,ltype2,scalef2,scalev2

ltype1=255
scalef1=0
scalev1=0
ltype2=255
scalef2=0
scalev2=0

if (ltype > 0 .and. ltype <= 9)then 
   ltype1=ltype
else if (ltype == 20) then
  ltype1=20
  scalev1=l1
  scalef1=2
else if (ltype == 100) then
  ltype1=100
  scalev1=l1*100
else if (ltype == 101) then
  ltype1=100
  scalev1=l1*1000
  ltype2=100
  scalev2=l2*1000
else if (ltype == 102) then
  ltype1=101
else if (ltype == 103) then
  ltype1=102
  scalev1=l1
else if (ltype == 104) then
  ltype1=102
  scalev1=l1*100
  ltype2=102
  scalev2=l2*100
else if (ltype == 105) then
  ltype1=103
  scalev1=l1
else if (ltype == 106) then
  ltype1=103
  scalev1=l1*100
  ltype2=103
  scalev2=l2*100
else if (ltype == 107) then
  ltype1=104
  scalef1=4
  scalev1=l1
else if (ltype == 108) then
  ltype1=104
  scalef1=2
  scalev1=l1
  ltype2=104
  scalef2=2
  scalev2=l2
else if (ltype == 109) then
  ltype1=105
  scalev1=l1
else if (ltype == 110) then
  ltype1=105
  scalev1=l1
  ltype2=105
  scalev2=l2
else if (ltype == 111) then
  ltype1=106
  scalef1=2
  scalev1=l1
else if (ltype == 112) then
  ltype1=106
  scalef1=2
  scalev1=l1
  ltype2=106
  scalef2=2
  scalev2=l2
else if (ltype == 113) then
  ltype1=107
  scalev1=l1
else if (ltype == 114) then
  ltype1=107
  scalev1=475+l1
  ltype2=107
  scalev2=475+l2
else if (ltype == 115) then
  ltype1=108
  scalev1=l1*100
else if (ltype == 116) then
  ltype1=108
  scalev1=l1*100
  ltype2=108
  scalev2=l2*100
else if (ltype == 117) then
  ltype1=109
  scalef1=9
  scalev1=l1
  if ( btest(l1,15) ) then
    scalev1=-1*mod(l1,32768)
  endif
else if (ltype == 119) then
  ltype1=111
  scalef1=4
  scalev1=l1
else if (ltype == 120) then
  ltype1=111
  scalef1=2
  scalev1=l1
  ltype2=111
  scalef2=2
  scalev2=l2
else if (ltype == 121) then
  ltype1=100
  scalev1=(1100+l1)*100
  ltype2=100
  scalev2=(1100+l2)*100
else if (ltype == 125) then
  ltype1=103
  scalef1=2
  scalev1=l1
else if (ltype == 128) then
  ltype1=104
  scalef1=3
  scalev1=1100+l1
  ltype2=104
  scalef2=3
  scalev2=1100+l2
else if (ltype == 141) then
  ltype1=100
  scalev1=l1*100
  ltype2=100
  scalev2=(1100+l2)*100
else if (ltype == 160) then
  ltype1=160
  scalev1=l1
else

  call l4f_log(L4F_ERROR,'level_g1_to_g2: GRIB1 level '//TRIM(to_char(ltype)) &
   //' cannot be converted to GRIB2.')
  call raise_error()
endif

END SUBROUTINE level_g1_to_g2


SUBROUTINE level_g2_to_g1(ltype1,scalef1,scalev1,ltype2,scalef2,scalev2,ltype,l1,l2)
integer,intent(in) :: ltype1,scalef1,scalev1,ltype2,scalef2,scalev2
integer,intent(out) :: ltype,l1,l2

if (ltype1 > 0 .and. ltype1 <= 9 .and. ltype2 == 255) then ! simple
  ltype = ltype1
  l1 = 0
  l2 = 0
else if (ltype1 == 20 .and. ltype2 == 255) then ! isothermal
  ltype = 2
  l1 = rescale2(scalef1-2,scalev1)!*100
  l2 = 0
else if (ltype1 == 100 .and. ltype2 == 255) then ! isobaric
  ltype = 100
  l1 = rescale2(scalef1+2,scalev1)!/100
  l2 = 0
else if (ltype1 == 100 .and. ltype2 == 100) then
  ltype = 101
  l1 = rescale1(scalef1+3,scalev1)!/1000
  l2 = rescale1(scalef2+3,scalev2)!/1000
else if (ltype1 == 101 .and. ltype2 == 255) then
  ltype = 102
  l1 = 0
  l2 = 0
else if (ltype1 == 102 .and. ltype2 == 255) then ! altitude over sea level
  ltype = 103
  l1 = rescale2(scalef1,scalev1)
  l2 = 0
else if (ltype1 == 102 .and. ltype2 == 102) then
  ltype = 104
  l1 = rescale1(scalef1+2,scalev1)!/100
  l2 = rescale1(scalef2+2,scalev2)!/100
else if (ltype1 == 103 .and. ltype2 == 255) then ! height over ground
  ltype = 105
  l1 = rescale2(scalef1,scalev1)
  l2 = 0
else if (ltype1 == 103 .and. ltype2 == 103) then
  ltype = 106
  l1 = rescale1(scalef1+2,scalev1)!/100
  l2 = rescale1(scalef2+2,scalev2)!/100
else if (ltype1 == 104 .and. ltype2 == 255) then ! sigma
  ltype = 107
  l1 = rescale2(scalef1,scalev1-4)!*10000
  l2 = 0
else if (ltype1 == 104 .and. ltype2 == 104) then
  ltype = 108
  l1 = rescale1(scalef1-2,scalev1)!*100
  l2 = rescale1(scalef2-2,scalev2)!*100
else if (ltype1 == 105 .and. ltype2 == 255) then ! hybrid
  ltype = 109
  l1 = rescale2(scalef1,scalev1)
  l2 = 0
else if (ltype1 == 105 .and. ltype2 == 105) then
  ltype = 110
  l1 = rescale1(scalef1,scalev1)
  l2 = rescale1(scalef2,scalev2)
else if (ltype1 == 106 .and. ltype2 == 255) then ! depth
  ltype = 111
  l1 = rescale2(scalef1-2,scalev1)!*100
  l2 = 0
else if (ltype1 == 106 .and. ltype2 == 106) then
  ltype = 112
  l1 = rescale1(scalef1-2,scalev1)!*100
  l2 = rescale1(scalef2-2,scalev2)!*100
else ! mi sono rotto per ora

  ltype = 255
  l1 = 0
  l2 = 0
  call l4f_log(L4F_ERROR,'level_g2_to_g1: GRIB2 levels '//TRIM(to_char(ltype1))//' ' &
   //TRIM(to_char(ltype2))//' cannot be converted to GRIB1.')
  call raise_error()
endif

CONTAINS

FUNCTION rescale1(scalef, scalev) RESULT(rescale)
INTEGER,INTENT(in) :: scalef, scalev
INTEGER :: rescale

rescale = MIN(255, INT(scalev*10.0D0**(-scalef)))

END FUNCTION rescale1

FUNCTION rescale2(scalef, scalev) RESULT(rescale)
INTEGER,INTENT(in) :: scalef, scalev
INTEGER :: rescale

rescale = MIN(65535, INT(scalev*10.0D0**(-scalef)))

END FUNCTION rescale2

END SUBROUTINE level_g2_to_g1

! Convert timerange from grib1 to grib2-like way:
!
! Tri 2 (point in time) gives (hopefully temporarily) statproc 205.
!
! Tri 13 (COSMO-nudging) gives p1 (forecast time) 0 and a temporary
! 206 statproc.
!
! Further processing and correction of the values returned is
! performed in normalize_gridinfo.
SUBROUTINE timerange_g1_to_g2_second(tri, p1_g1, p2_g1, unit, statproc, p1, p2)
INTEGER,INTENT(in) :: tri, p1_g1, p2_g1, unit
INTEGER,INTENT(out) :: statproc, p1, p2

IF (tri == 0 .OR. tri == 1 .OR. tri == 10) THEN ! point in time
  statproc = 254
  CALL gribtr_to_second(unit, p1_g1, p1)
  p2 = 0
ELSE IF (tri == 2) THEN ! somewhere between p1 and p2, not good for grib2 standard
  statproc = 205
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE IF (tri == 3) THEN ! average
  statproc = 0
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE IF (tri == 4) THEN ! accumulation
  statproc = 1
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE IF (tri == 5) THEN ! difference
  statproc = 4
  CALL gribtr_to_second(unit, p2_g1, p1)
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE IF (tri == 13) THEN ! COSMO-nudging, use a temporary value then normalize
  statproc = 206 ! check if 206 is legal!
  p1 = 0 ! analysis regardless of p2_g1
  CALL gribtr_to_second(unit, p2_g1-p1_g1, p2)
ELSE
  call l4f_log(L4F_ERROR,'timerange_g1_to_g2: GRIB1 timerange '//TRIM(to_char(tri)) &
   //' cannot be converted to GRIB2.')
  CALL raise_error()
ENDIF

if (statproc == 254 .and. p2 /= 0 ) then
  call l4f_log(L4F_WARN,"inconsistence in timerange:254,"//trim(to_char(p1))//","//trim(to_char(p2)))
end if

END SUBROUTINE timerange_g1_to_g2_second


! Convert timerange from grib2-like to grib1 way:
!
! Statproc 205 (point in time, non standard, not good in grib2) is
! correctly converted to tri 2.
!
! Statproc 206 (COSMO nudging-like, non standard, not good in grib2)
! should not appear here, but is anyway converted to tri 13 (real
! COSMO-nudging).
!
! In case p1_g1<0 (i.e. statistically processed analisys data, with
! p1=0 and p2>0), COSMO-nudging tri 13 is (mis-)used.
SUBROUTINE timerange_g2_to_g1_unit(statproc, p1, p2, tri, p1_g1, p2_g1, unit)
INTEGER,INTENT(in) :: statproc, p1, p2
INTEGER,INTENT(out) :: tri, p1_g1, p2_g1, unit

INTEGER :: ptmp

IF (statproc == 0) THEN ! average
  tri = 3
  CALL second_to_gribtr(p1, p2_g1, unit)    ! here and after make sure that
  CALL second_to_gribtr(p1-p2, p1_g1, unit) ! unit is the same between calls
ELSE IF (statproc == 1) THEN ! accumulation
  tri = 4
  CALL second_to_gribtr(p1, p2_g1, unit)
  CALL second_to_gribtr(p1-p2, p1_g1, unit)
ELSE IF (statproc == 4) THEN ! difference
  tri = 5
  CALL second_to_gribtr(p1, p2_g1, unit)
  CALL second_to_gribtr(p1-p2, p1_g1, unit)
ELSE IF (statproc == 205) THEN ! point in time interval, not good for grib2 standard
  tri = 2
  CALL second_to_gribtr(p1, p2_g1, unit)
  CALL second_to_gribtr(p1-p2, p1_g1, unit)
ELSE IF (statproc == 206) THEN ! COSMO-nudging (statistical processing in the past)
! this should never happen (at least from COSMO grib1), since 206 is
! converted to something else in normalize_gridinfo; now a negative
! p1_g1 is set, it will be corrected in the next section
  tri = 13
  CALL second_to_gribtr(p1, p2_g1, unit)
  CALL second_to_gribtr(p1-p2, p1_g1, unit)
ELSE IF (statproc == 254) THEN ! point in time
  tri = 0
  CALL second_to_gribtr(p1, p1_g1, unit)
  p2_g1 = 0
ELSE
  call l4f_log(L4F_ERROR,'timerange_g2_to_g1: GRIB2 statisticalprocessing ' &
   //TRIM(to_char(statproc))//' cannot be converted to GRIB1.')
  CALL raise_error()
ENDIF

! p1 < 0 is not allowed, use COSMO trick
IF (p1_g1 < 0) THEN
  ptmp = p1_g1
  p1_g1 = 0
  p2_g1 = p2_g1 - ptmp
  tri = 13
ENDIF

END SUBROUTINE timerange_g2_to_g1_unit


!0       Minute
!1       Hour
!2       Day
!3       Month
!4       Year
!5       Decade (10 years)
!6       Normal (30 years)
!7       Century(100 years)
!8-9     Reserved
!10      3 hours
!11      6 hours
!12      12 hours
!13      Second
! attenzione, in COSMO:
!13 = 15 minuti
!14 = 30 minuti
SUBROUTINE gribtr_to_second(unit, valuein, valueout)
INTEGER,INTENT(in) :: unit, valuein
INTEGER,INTENT(out) :: valueout

INTEGER,PARAMETER :: unitlist(0:13)=(/60,3600,86400,2592000,31536000, &
 315360000,946080000,imiss,imiss,imiss,10800,21600,43200,1/)

IF (unit >= 0 .AND. unit <= 13) THEN
  IF (c_e(unitlist(unit))) THEN
    valueout = valuein*unitlist(unit)
  ELSE
    valueout = imiss
  ENDIF
ELSE
  valueout = imiss
ENDIF

END SUBROUTINE gribtr_to_second


! improve to limit the values to 255 if possible
SUBROUTINE second_to_gribtr(valuein, valueout, unit)
INTEGER,INTENT(in) :: valuein
INTEGER,INTENT(out) :: valueout, unit

IF (valuein == imiss) THEN
  valueout = imiss
  unit = 1
ELSE IF (MOD(valuein,3600) == 0) THEN ! prefer hours
  valueout = valuein/3600
  unit = 1
ELSE IF (MOD(valuein,60) == 0) THEN ! then minutes
  valueout = valuein/60
  unit = 0
ELSE ! seconds (not supported in grib1!)
  valueout = valuein
  unit = 13
ENDIF

END SUBROUTINE second_to_gribtr


! Standardize variables and timerange in DB-all.e thinking:
!
! Timerange 205 (point in time interval) is converted to maximum or
! minimum if parameter is recognized as such and parameter is
! corrected as well, otherwise 205 is kept (with possible error
! conditions later).
!
! Timerange 206 (COSMO nudging) is converted to point in time if
! interval length is 0, or to a proper timerange if parameter is
! recognized as a COSMO statistically processed parameter (and in case
! of maximum or minimum the parameter is corrected as well); if
! parameter is not recognized, it is converted to instantaneous with a
! warning.
SUBROUTINE normalize_gridinfo(this)
TYPE(gridinfo_def),intent(inout) :: this

IF (this%timerange%timerange == 205) THEN ! point in time interval

!tmin
  if (this%var == volgrid6d_var_new(255,2,16,255)) then
    this%var%number=11
    this%timerange%timerange=3
    return
  end if

!tmax
  if (this%var == volgrid6d_var_new(255,2,15,255)) then
    this%var%number=11
    this%timerange%timerange=2
    return
  end if

  IF (this%var%discipline == 255 .AND. &
   ANY(this%var%centre == cosmo_centre)) THEN ! grib1 & COSMO

    IF (this%var%category == 201) THEN ! table 201

      IF (this%var%number == 187) THEN ! wind max
        this%var%category=2
        this%var%number=32
        this%timerange%timerange=2
      ENDIF
    ENDIF
  ENDIF

ELSE IF (this%timerange%timerange == 206) THEN ! COSMO-nudging

  IF (this%timerange%p2 == 0) THEN ! point in time

    this%timerange%timerange=254

  ELSE ! guess timerange according to parameter

    IF (this%var%discipline == 255 .AND. &
     ANY(this%var%centre == cosmo_centre)) THEN ! grib1 & COSMO

      IF (this%var%category == 2) THEN ! table 2

        if (this%var%number == 11) then ! T
          this%timerange%timerange=0 ! average

        else if (this%var%number == 15) then ! T
          this%timerange%timerange=2 ! maximum
          this%var%number=11 ! reset also parameter

        else if (this%var%number == 16) then ! T
          this%timerange%timerange=3 ! minimum
          this%var%number=11 ! reset also parameter

        else if (this%var%number == 17) then ! TD
          this%timerange%timerange=0 ! average

        else if (this%var%number == 33) then ! U
          this%timerange%timerange=0 ! average

        else if (this%var%number == 34) then ! V
          this%timerange%timerange=0 ! average

        else if (this%var%number == 57) then ! evaporation
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 61) then ! TOT_PREC
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 78) then ! SNOW_CON
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 79) then ! SNOW_GSP
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 90) then ! RUNOFF
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 111) then ! fluxes
          this%timerange%timerange=0 ! average
        else if (this%var%number == 112) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 113) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 114) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 121) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 122) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 124) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 125) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 126) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 127) then
          this%timerange%timerange=0 ! average

        endif

      ELSE IF (this%var%category == 201) THEN ! table 201

        if (this%var%number == 5) then ! photosynthetic flux
          this%timerange%timerange=0 ! average

        else if (this%var%number == 20) then ! SUN_DUR
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 22) then ! radiation fluxes
          this%timerange%timerange=0 ! average
        else if (this%var%number == 23) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 24) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 25) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 26) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 27) then
          this%timerange%timerange=0 ! average

        else if (this%var%number == 42) then ! water divergence
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 102) then ! RAIN_GSP
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 113) then ! RAIN_CON
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 132) then ! GRAU_GSP
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 135) then ! HAIL_GSP
          this%timerange%timerange=1 ! accumulated

        else if (this%var%number == 187) then ! UVMAX
          this%var%category=2 ! reset also parameter
          this%var%number=32
          this%timerange%timerange=2 ! maximum

        else if (this%var%number == 218) then ! maximum 10m dynamical gust
          this%timerange%timerange=2 ! maximum

        else if (this%var%number == 219) then ! maximum 10m convective gust
          this%timerange%timerange=2 ! maximum

        endif

      ELSE IF (this%var%category == 202) THEN ! table 202

        if (this%var%number == 231) then ! sso parameters
          this%timerange%timerange=0 ! average
        else if (this%var%number == 232) then
          this%timerange%timerange=0 ! average
        else if (this%var%number == 233) then
          this%timerange%timerange=0 ! average
        endif

      ELSE ! parameter not recognized, set instantaneous?

        call l4f_category_log(this%category,L4F_WARN, &
         'normalize_gridinfo: found COSMO non instantaneous analysis 13,0,'//&
         TRIM(to_char(this%timerange%p2)))
        call l4f_category_log(this%category,L4F_WARN, &
         'associated to an apparently instantaneous parameter '//&
         TRIM(to_char(this%var%centre))//','//TRIM(to_char(this%var%category))//','//&
         TRIM(to_char(this%var%number))//','//TRIM(to_char(this%var%discipline)))
        call l4f_category_log(this%category,L4F_WARN, 'forcing to instantaneous')

        this%timerange%p2 = 0
        this%timerange%timerange = 254

      ENDIF ! category
    ENDIF ! grib1 & COSMO
  ENDIF ! p2
ENDIF ! timerange 

END SUBROUTINE normalize_gridinfo


! Destandardize variables and timerange from DB-all.e thinking:
!
! Parameters having maximum or minimum statistical processing and
! having a corresponding extreme parameter in grib table, are
! temporarily converted to timerange 205 and to the corresponding
! extreme parameter; if parameter is not recognized, the max or min
! statistical processing is kept (with possible error conditions
! later).
subroutine unnormalize_gridinfo(this)
TYPE(gridinfo_def),intent(inout) :: this

if (this%timerange%timerange == 3 )then

!tmin
  if (this%var == volgrid6d_var_new(255,2,11,255)) then
    this%var%number=16
    this%timerange%timerange=205
    return
  end if

else if (this%timerange%timerange == 2 )then

!tmax
  if (this%var == volgrid6d_var_new(255,2,11,255)) then
    this%var%number=15
    this%timerange%timerange=205
    return
  end if

  IF (ANY(this%var%centre == cosmo_centre)) THEN ! grib1 & COSMO

! wind max
    IF (this%var == volgrid6d_var_new(255,2,32,255)) THEN
      this%var%category=201
      this%var%number=187
      this%timerange%timerange=205
    ENDIF

  ENDIF

end if

end subroutine unnormalize_gridinfo
#endif


! =========================================
! gdal driver specific code
! could this be moved to a separate module?
! =========================================
#ifdef HAVE_LIBGDAL
SUBROUTINE gridinfo_import_gdal(this, gdalid)
TYPE(gridinfo_def),INTENT(inout) :: this ! gridinfo object
TYPE(gdalrasterbandh),INTENT(in) :: gdalid ! gdal rasterband pointer

TYPE(gdaldataseth) :: hds


!call time_import_gdal(this%time, gaid)
this%time = datetime_new(year=2010, month=1, day=1) ! conventional year

!call timerange_import_gdal(this%timerange,gaid)
this%timerange = vol7d_timerange_new(254, 0, 0) ! instantaneous data

!call level_import_gdal(this%level, gaid)
hds = gdalgetbanddataset(gdalid) ! go back to dataset
IF (gdalgetrastercount(hds) == 1) THEN ! single level dataset
  this%level = vol7d_level_new(1, 0) ! surface
ELSE
  this%level = vol7d_level_new(105, gdalgetbandnumber(gdalid)) ! hybrid level
ENDIF

!call var_import_gdal(this%var, gaid)
this%var = volgrid6d_var_new(centre=255, category=2, number=8) ! topography height

END SUBROUTINE gridinfo_import_gdal
#endif


end module gridinfo_class


!>\example example_vg6d_2.f90
!!\brief Programma esempio semplice per la lettura di file grib.
!!
!! Programma che legge i grib contenuti in un file e li organizza in un vettore di oggetti gridinfo


!>\example example_vg6d_4.f90
!!\brief Programma esempio semplice per la elaborazione di file grib.
!!
!! Programma che legge uno ad uno i grid contenuti in un file e li
!! elabora producendo un file di output contenente ancora grib
