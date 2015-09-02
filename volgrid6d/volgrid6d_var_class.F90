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

!> Class for managing physical variables in a grib 1/2 fashion.
!! This module defines a class which can represent Earth-science
!! related physical variables, following the classification scheme
!! adopted by WMO for grib1 and grib2 parameter definition. It also
!! defines some methods for mapping \a volgrid6d_var variables and
!! converting the corresponding fields to a matching \a vol7d_var
!! object defined in \a vol7d_var_class module, which, unlike the
!! variables defined here, defines univocally a physical quantity.
!!
!! \ingroup volgrid6d
MODULE volgrid6d_var_class
USE kinds
USE missing_values
USE err_handling
USE vol7d_var_class
USE file_utilities
USE grid_id_class

IMPLICIT NONE

!> Definition of a physical variable in grib coding style.
!! \a volgrid6d_var members are public, thus they can be freely
!! altered, but it is advisable to set them through the
!! volgrid6d_var_class::init constructor.
TYPE volgrid6d_var
  integer :: centre !< centre
  integer :: category !< grib2: category / grib1: grib table version number
  integer :: number !< parameter number
  integer :: discipline !< grib2: discipline / grib1: 255
  CHARACTER(len=65) :: description !< optional textual description of the variable
  CHARACTER(len=24) :: unit !< optional textual description of the variable's unit
END TYPE  volgrid6d_var

TYPE(volgrid6d_var),PARAMETER :: volgrid6d_var_miss= &
 volgrid6d_var(imiss,imiss,imiss,imiss,cmiss,cmiss) !< missing value volgrid6d_var.

TYPE(vol7d_var),PARAMETER :: vol7d_var_horstag(2) = (/ &
 vol7d_var('B11003', '', '', 0, 0, 0, 0, 0, 0), &
 vol7d_var('B11004', '', '', 0, 0, 0, 0, 0, 0) &
 /)

TYPE(vol7d_var),PARAMETER :: vol7d_var_horcomp(4) = (/ &! RESHAPE( (/ &
 vol7d_var('B11003', '', '', 0, 0, 0, 0, 0, 0), &
 vol7d_var('B11004', '', '', 0, 0, 0, 0, 0, 0), &
 vol7d_var('B11200', '', '', 0, 0, 0, 0, 0, 0), &
 vol7d_var('B11201', '', '', 0, 0, 0, 0, 0, 0) &
/)
!/), (/2,2/)) ! bug in gfortran

!> Class defining a real conversion function between units. It is
!! used to numerically convert a value expressed as a \a volgrid6d_var
!! variable in a value expressed as a \a vol7d_var variable and
!! vice-versa. At the moment only a linear conversion is
!! supported. Objects of this class are returned only by the \a
!! vargrib2varbufr \a varbufr2vargrib, and \a convert methods and are
!! used in the \a convert and \a compute methods defined in this
!! MODULE.
TYPE conv_func
  PRIVATE
  REAL :: a, b
END TYPE conv_func

TYPE(conv_func), PARAMETER :: conv_func_miss=conv_func(rmiss,rmiss)
TYPE(conv_func), PARAMETER :: conv_func_identity=conv_func(1.0,0.0)

TYPE vg6d_v7d_var_conv
  TYPE(volgrid6d_var) :: vg6d_var
  TYPE(vol7d_var) :: v7d_var
  TYPE(conv_func) :: c_func
! aggiungere informazioni ad es. su rotazione del vento
END TYPE vg6d_v7d_var_conv

TYPE(vg6d_v7d_var_conv), PARAMETER :: vg6d_v7d_var_conv_miss= &
 vg6d_v7d_var_conv(volgrid6d_var_miss, vol7d_var_miss, conv_func_miss)

TYPE(vg6d_v7d_var_conv), ALLOCATABLE :: conv_fwd(:), conv_bwd(:)

!> Initialize a \a volgrid6d_var object with the optional arguments provided.
!! If an argument is not provided, the corresponding object member and
!! those depending on it will be set to missing. For grib1-style
!! variables, the \a discipline argument must be omitted, it will be
!! set to 255 (grib missing value).
!!
!! \param this TYPE(volgrid6d_var),INTENT(INOUT) object to be initialized
!! \param centre INTEGER,INTENT(in),OPTIONAL centre
!! \param category INTEGER,INTENT(in),OPTIONAL grib2: category / grib1: grib table version number
!! \param number INTEGER,INTENT(in),OPTIONAL parameter number
!! \param discipline INTEGER,INTENT(in),OPTIONAL grib2: discipline / grib1: 255
!! \param description CHARACTER(len=*),INTENT(in),OPTIONAL optional textual description of the variable
!! \param unit CHARACTER(len=*),INTENT(in),OPTIONAL optional textual description of the variable's unit
INTERFACE init
  MODULE PROCEDURE volgrid6d_var_init
END INTERFACE

!> Destructor for the corresponding object, it assigns it to a missing value.
!! \param this TYPE(volgrid6d_var) object to be destroyed
INTERFACE delete
  MODULE PROCEDURE volgrid6d_var_delete
END INTERFACE


!> Logical equality operators for objects of the classes \a
!! volgrid6d_var and \a conv_func.
!! They are all defined as \c ELEMENTAL thus work also on arrays of
!! any shape.
INTERFACE OPERATOR (==)
  MODULE PROCEDURE volgrid6d_var_eq, conv_func_eq
END INTERFACE

!> Logical inequality operators for objects of the classes \a
!! volgrid6d_var and \a conv_func.
!! They are all defined as \c ELEMENTAL thus work also on arrays of
!! any shape.
INTERFACE OPERATOR (/=)
  MODULE PROCEDURE volgrid6d_var_ne, conv_func_ne
END INTERFACE

#define VOL7D_POLY_TYPE TYPE(volgrid6d_var)
#define VOL7D_POLY_TYPES _var6d
#include "array_utilities_pre.F90"

!> Display on the screen a brief content of object
INTERFACE display
  MODULE PROCEDURE display_volgrid6d_var
END INTERFACE

!> Compose two conversions into a single one.
!! Unlike scalar multiplication (and like matrix multiplication) here
!! a*b /= b*a. By convention, the second factor is applied first in
!! the result.
INTERFACE OPERATOR (*)
  MODULE PROCEDURE conv_func_mult
END INTERFACE OPERATOR (*)

!> Apply the conversion function \a this to \a values.
!! function version
INTERFACE compute
  MODULE PROCEDURE conv_func_compute
END INTERFACE

!> Apply the conversion function \a this to \a values.
!! subroutine version
INTERFACE convert
  MODULE PROCEDURE varbufr2vargrib_convert, vargrib2varbufr_convert, &
   conv_func_convert
END INTERFACE

PRIVATE
PUBLIC volgrid6d_var, volgrid6d_var_miss, volgrid6d_var_new, init, delete, &
 volgrid6d_var_normalize, &
 OPERATOR(==), OPERATOR(/=), OPERATOR(*), &
 count_distinct, pack_distinct, count_and_pack_distinct, &
 map_distinct, map_inv_distinct, &
 index, display, &
 vargrib2varbufr, varbufr2vargrib, &
 conv_func, conv_func_miss, compute, convert, &
 volgrid6d_var_hor_comp_index


CONTAINS


ELEMENTAL FUNCTION volgrid6d_var_new(centre, category, number, &
 discipline, description, unit) RESULT(this)
integer,INTENT(in),OPTIONAL :: centre !< centre 
integer,INTENT(in),OPTIONAL :: category !< grib2: category / grib1: grib table version number
integer,INTENT(in),OPTIONAL :: number !< parameter number
integer,INTENT(in),OPTIONAL :: discipline !< grib2: discipline
CHARACTER(len=*),INTENT(in),OPTIONAL :: description !< textual description of the variable
CHARACTER(len=*),INTENT(in),OPTIONAL :: unit !< textual description of the variable's unit

TYPE(volgrid6d_var) :: this !< object to be initialised

CALL init(this, centre, category, number, discipline, description, unit)

END FUNCTION volgrid6d_var_new


! documented in the interface
ELEMENTAL SUBROUTINE volgrid6d_var_init(this, centre, category, number, discipline,description,unit)
TYPE(volgrid6d_var),INTENT(INOUT) :: this ! object to be initialized
INTEGER,INTENT(in),OPTIONAL :: centre ! centre
INTEGER,INTENT(in),OPTIONAL :: category ! grib2: category / grib1: grib table version number
INTEGER,INTENT(in),OPTIONAL :: number ! parameter number
INTEGER,INTENT(in),OPTIONAL :: discipline ! grib2: discipline / grib1: 255
CHARACTER(len=*),INTENT(in),OPTIONAL :: description ! optional textual description of the variable
CHARACTER(len=*),INTENT(in),OPTIONAL :: unit ! optional textual description of the variable's unit

IF (PRESENT(centre)) THEN
  this%centre = centre
ELSE
  this%centre = imiss
  this%category = imiss
  this%number = imiss
  this%discipline = imiss
  RETURN
ENDIF

IF (PRESENT(category)) THEN
  this%category = category
ELSE
  this%category = imiss
  this%number = imiss
  this%discipline = imiss
  RETURN
ENDIF


IF (PRESENT(number)) THEN
  this%number = number
ELSE
  this%number = imiss
  this%discipline = imiss
  RETURN
ENDIF

! se sono arrivato fino a qui ho impostato centre, category e number
!per il grib 1 manca discipline e imposto 255 (missing del grib2) 

IF (PRESENT(discipline)) THEN
  this%discipline = discipline
ELSE
  this%discipline = 255
ENDIF

IF (PRESENT(description)) THEN
  this%description = description
ELSE
  this%description = cmiss
ENDIF

IF (PRESENT(unit)) THEN
  this%unit = unit
ELSE
  this%unit = cmiss
ENDIF



END SUBROUTINE volgrid6d_var_init


! documented in the interface
SUBROUTINE volgrid6d_var_delete(this)
TYPE(volgrid6d_var),INTENT(INOUT) :: this

this%centre = imiss
this%category = imiss
this%number = imiss
this%discipline = imiss
this%description = cmiss
this%unit = cmiss

END SUBROUTINE volgrid6d_var_delete


ELEMENTAL FUNCTION volgrid6d_var_eq(this, that) RESULT(res)
TYPE(volgrid6d_var),INTENT(IN) :: this, that
LOGICAL :: res

IF (this%discipline == that%discipline) THEN

  IF (this%discipline == 255) THEN ! grib1
    res = this%category == that%category .AND. &
     this%number == that%number

    IF ((this%category >= 128 .AND. this%category <= 254) .OR. &
     (this%number >= 128 .AND. this%number <= 254)) THEN
      res = res .AND. this%centre == that%centre ! local definition, centre matters
    ENDIF

  ELSE ! grib2
    res = this%category == that%category .AND. &
     this%number == that%number

    IF ((this%discipline >= 192 .AND. this%discipline <= 254) .OR. &
     (this%category >= 192 .AND. this%category <= 254) .OR. &
     (this%number >= 192 .AND. this%number <= 254)) THEN
      res = res .AND. this%centre == that%centre ! local definition, centre matters
    ENDIF
  ENDIF

ELSE ! different edition or different discipline
  res = .FALSE.
ENDIF

END FUNCTION volgrid6d_var_eq


ELEMENTAL FUNCTION volgrid6d_var_ne(this, that) RESULT(res)
TYPE(volgrid6d_var),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION volgrid6d_var_ne


#include "array_utilities_inc.F90"


!> Display on the screen a brief content of \a volgrid6d_var object.
SUBROUTINE display_volgrid6d_var(this)
TYPE(volgrid6d_var),INTENT(in) :: this !< volgrid6d_var object to display

print*,"GRIDVAR: ",this%centre,this%discipline,this%category,this%number

END SUBROUTINE display_volgrid6d_var


!> Convert a \a volgrid6d_var array object into a physically equivalent
!! \a vol7d_var array object. This method converts a grib-like array
!! of physical variables \a vargrib, to an array of unique, physically
!! based, bufr-like variables \a varbufr. The output array must have
!! enough room for the converted variables. The method additionally
!! allocates a \a conv_func array object of the same size, which can
!! successively be used to convert the numerical values of the fields
!! associated to \a vargrib to the corresponding fields in the \a
!! bufr-like representation. \a c_func will have to be deallocated by
!! the calling procedure. If a conversion is not successful, the
!! corresponding output variable is set to \a vol7d_var_miss and the
!! conversion function to \a conv_func_miss.
SUBROUTINE vargrib2varbufr(vargrib, varbufr, c_func)
TYPE(volgrid6d_var),INTENT(in) :: vargrib(:) !< array of input grib-like variables
TYPE(vol7d_var),INTENT(out) :: varbufr(:) !< array of output bufr-like variables
TYPE(conv_func),POINTER :: c_func(:) !< pointer to an array of the corresponding \a conv_func objects, allocated in the method

INTEGER :: i, n, stallo

n = MIN(SIZE(varbufr), SIZE(vargrib))
ALLOCATE(c_func(n),stat=stallo)
IF (stallo /= 0) THEN
  call l4f_log(L4F_FATAL,"allocating memory")
  call raise_fatal_error()
ENDIF

DO i = 1, n
  varbufr(i) = convert(vargrib(i), c_func(i))
ENDDO

END SUBROUTINE vargrib2varbufr


!> Convert a \a volgrid6d_var object into a physically equivalent
!! \a vol7d_var object. This method returns a physically based,
!! bufr-like representation of type \a vol7d_var of the grib-like
!! input physical variable \a vargrib. The method optionally returns
!! a \a conv_func object which can successively be used to convert the
!! numerical values of the field associated to \a vargrib to the
!! corresponding fields in the bufr-like representation. If the
!! conversion is not successful, the output variable is
!! set to \a vol7d_var_miss and the conversion function to \a
!! conv_func_miss.
FUNCTION vargrib2varbufr_convert(vargrib, c_func) RESULT(convert)
TYPE(volgrid6d_var),INTENT(in) :: vargrib !< input grib-like variable
TYPE(conv_func),INTENT(out),OPTIONAL :: c_func !< corresponding \a conv_func object
TYPE(vol7d_var) :: convert

INTEGER :: i

IF (.NOT. ALLOCATED(conv_fwd)) CALL vg6d_v7d_var_conv_setup()

DO i = 1, SIZE(conv_fwd)
  IF (vargrib == conv_fwd(i)%vg6d_var) THEN
    convert = conv_fwd(i)%v7d_var
    IF (PRESENT(c_func)) c_func = conv_fwd(i)%c_func
    RETURN
  ENDIF
ENDDO
! not found
convert = vol7d_var_miss
IF (PRESENT(c_func)) c_func = conv_func_miss

CALL l4f_log(L4F_WARN, 'vargrib2varbufr: variable '// &
 TRIM(to_char(vargrib%centre))//':'//TRIM(to_char(vargrib%category))//':'// &
 TRIM(to_char(vargrib%number))//':'//TRIM(to_char(vargrib%discipline))// &
 ' not found in table')

END FUNCTION vargrib2varbufr_convert


!> Convert a \a vol7d_var array object into a physically equivalent
!! \a volgrid6d_var array object. This method converts a bufr-like
!! array of physical variables \a vargrib, to an array of grib-like
!! variables \a varbufr. Unlike the opposite method \a
!! vargrib2varbufr, in this case the conversion is not uniqe and at
!! the moment the first matching grib-like variable is chosen, without
!! any control over the choice process. The output array must have
!! enough room for the converted variables. The method additionally
!! allocates a \a conv_func array object of the same size, which can
!! successively be used to convert the numerical values of the fields
!! associated to \a varbufr to the corresponding fields in the \a
!! grib-like representation. \a c_func will have to be deallocated by
!! the calling procedure. If a conversion is not successful, the
!! corresponding output variable is set to \a volgrid6d_var_miss and
!! the conversion function to \a conv_func_miss.
SUBROUTINE varbufr2vargrib(varbufr, vargrib, c_func, grid_id_template)
TYPE(vol7d_var),INTENT(in) :: varbufr(:) !< array of input bufr-like variables
TYPE(volgrid6d_var),INTENT(out) :: vargrib(:) !< array of output grib-like variables
TYPE(conv_func),POINTER :: c_func(:) !< pointer to an array of the corresponding \a conv_func objects, allocated in the method
TYPE(grid_id),INTENT(in),OPTIONAL :: grid_id_template !< a template (typically grib_api) to which data will be finally exported, it helps in improving variable conversion

INTEGER :: i, n, stallo

n = MIN(SIZE(varbufr), SIZE(vargrib))
ALLOCATE(c_func(n),stat=stallo)
IF (stallo /= 0) THEN
  CALL l4f_log(L4F_FATAL,"allocating memory")
  CALL raise_fatal_error()
ENDIF

DO i = 1, n
  vargrib(i) = convert(varbufr(i), c_func(i), grid_id_template)
ENDDO

END SUBROUTINE varbufr2vargrib


!> Convert a \a vol7d_var object into a physically equivalent
!! \a volgrid6d_var object. This method returns a grib-like
!! representation of type \a volgrid6d_var of the bufr-like input
!! physical variable \a varbufr. Unlike the opposite \a convert
!! method, in this case the conversion is not uniqe and at the moment
!! the first matching grib-like variable is chosen, without any
!! control over the choice process.  The method optionally returns a
!! \a conv_func object which can successively be used to convert the
!! numerical values of the field associated to \a varbufr to the
!! corresponding fields in the grib-like representation. If the
!! conversion is not successful, the output variable is set to \a
!! volgrid6d_var_miss and the conversion function to \a
!! conv_func_miss.
FUNCTION varbufr2vargrib_convert(varbufr, c_func, grid_id_template) RESULT(convert)
TYPE(vol7d_var),INTENT(in) :: varbufr !< input bufr-like variable
TYPE(conv_func),INTENT(out),OPTIONAL :: c_func !< corresponding \a conv_func object
TYPE(grid_id),INTENT(in),OPTIONAL :: grid_id_template !< a template (typically grib_api) to which data will be finally exported, it helps in improving variable conversion
TYPE(volgrid6d_var) :: convert

INTEGER :: i
#ifdef HAVE_LIBGRIBAPI
INTEGER :: gaid, editionnumber, category, centre
#endif

IF (.NOT. ALLOCATED(conv_bwd)) CALL vg6d_v7d_var_conv_setup()

#ifdef HAVE_LIBGRIBAPI
editionnumber=255; category=255; centre=255
#endif
IF (PRESENT(grid_id_template)) THEN
#ifdef HAVE_LIBGRIBAPI
  gaid = grid_id_get_gaid(grid_id_template)
  IF (c_e(gaid)) THEN
    CALL grib_get(gaid, 'GRIBEditionNumber', editionnumber)
    IF (editionnumber == 1) THEN
      CALL grib_get(gaid,'gribTablesVersionNo',category)
    ENDIF
    CALL grib_get(gaid,'centre',centre)
  ENDIF
#endif
ENDIF

DO i = 1, SIZE(conv_bwd)
  IF (varbufr == conv_bwd(i)%v7d_var) THEN
#ifdef HAVE_LIBGRIBAPI
    IF (editionnumber /= 255) THEN ! further check required (gaid present)
      IF (editionnumber == 1) THEN
        IF (conv_bwd(i)%vg6d_var%discipline /= 255) CYCLE ! wrong edition
      ELSE IF (editionnumber == 2) THEN
        IF (conv_bwd(i)%vg6d_var%discipline == 255) CYCLE ! wrong edition
      ENDIF
      IF (conv_bwd(i)%vg6d_var%centre /= 255 .AND. &
       conv_bwd(i)%vg6d_var%centre /= centre) CYCLE ! wrong centre
    ENDIF
#endif
    convert = conv_bwd(i)%vg6d_var
    IF (PRESENT(c_func)) c_func = conv_bwd(i)%c_func
    RETURN
  ENDIF
ENDDO
! not found
convert = volgrid6d_var_miss
IF (PRESENT(c_func)) c_func = conv_func_miss

CALL l4f_log(L4F_WARN, 'varbufr2vargrib: variable '// &
 trim(varbufr%btable)//" : "//trim(varbufr%description)//" : "//trim(varbufr%unit)// &
 ' not found in table')

END FUNCTION varbufr2vargrib_convert


!> Normalize a variable definition converting it to the
!! format (grib edition) specified in the (grib) template provided.
!! This allows a basic grib1 <-> grib2 conversion provided that
!! entries for both grib editions of the related variable are present
!! in the static file \a vargrib2ufr.csv. If the \a c_func variable
!! returned is not missing (i.e. /= conv_func_miss) the field value
!! should be converted as well using the conv_func::compute method .
SUBROUTINE volgrid6d_var_normalize(this, c_func, grid_id_template)
TYPE(volgrid6d_var),INTENT(inout) :: this !< variable to normalize
TYPE(conv_func),INTENT(out) :: c_func !< \a conv_func object to convert data
TYPE(grid_id),INTENT(in) :: grid_id_template !< a template (typically grib_api) to which data will be finally exported, it helps in improving variable conversion

LOGICAL :: compat
INTEGER :: gaid, editionnumber
TYPE(volgrid6d_var) :: convert2
TYPE(vol7d_var) :: tmpbufr
TYPE(conv_func) tmpc_func

compat = .TRUE.
c_func = conv_func_miss

#ifdef HAVE_LIBGRIBAPI
gaid = grid_id_get_gaid(grid_id_template)
IF (c_e(gaid)) THEN
  CALL grib_get(gaid, 'GRIBEditionNumber', editionnumber)
  compat = editionnumber == 1 .EQV. this%discipline == 255
ENDIF
#endif
IF (compat) RETURN ! nothing to do

tmpbufr = convert(this, tmpc_func)
! manage missing for speed?
this = convert(tmpbufr, c_func, grid_id_template)
c_func = c_func * tmpc_func
IF (c_func == conv_func_identity) c_func = conv_func_miss

END SUBROUTINE volgrid6d_var_normalize


! Private subroutine for reading forward and backward conversion tables
! todo: better error handling
SUBROUTINE vg6d_v7d_var_conv_setup()
INTEGER :: un, i, n, stallo

! forward, grib to bufr
un = open_package_file('vargrib2bufr.csv', filetype_data)
n=0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  n = n + 1
ENDDO

100 CONTINUE

REWIND(un)
ALLOCATE(conv_fwd(n),stat=stallo)
IF (stallo /= 0) THEN
  CALL l4f_log(L4F_FATAL,"allocating memory")
  CALL raise_fatal_error()
ENDIF

conv_fwd(:) = vg6d_v7d_var_conv_miss
CALL import_var_conv(un, conv_fwd)
CLOSE(un)

! backward, bufr to grib
un = open_package_file('vargrib2bufr.csv', filetype_data)
! use the same file for now
!un = open_package_file('varbufr2grib.csv', filetype_data)
n=0
DO WHILE(.TRUE.)
  READ(un,*,END=300)
  n = n + 1
ENDDO

300 CONTINUE

REWIND(un)
ALLOCATE(conv_bwd(n),stat=stallo)
IF (stallo /= 0) THEN
  CALL l4f_log(L4F_FATAL,"allocating memory")
  CALL raise_fatal_error()
end if

conv_bwd(:) = vg6d_v7d_var_conv_miss
CALL import_var_conv(un, conv_bwd)
DO i = 1, n
  conv_bwd(i)%c_func%a = 1./conv_bwd(i)%c_func%a
  conv_bwd(i)%c_func%b = - conv_bwd(i)%c_func%b
ENDDO
CLOSE(un)

CONTAINS

SUBROUTINE import_var_conv(un, conv_type)
INTEGER, INTENT(in) :: un
TYPE(vg6d_v7d_var_conv), INTENT(out) :: conv_type(:)

INTEGER :: i
TYPE(csv_record) :: csv
CHARACTER(len=1024) :: line
CHARACTER(len=10) :: btable
INTEGER :: centre, category, number, discipline

DO i = 1, SIZE(conv_type)
  READ(un,'(A)',END=200)line
  CALL init(csv, line)
  CALL csv_record_getfield(csv, btable)
  CALL csv_record_getfield(csv) ! skip fields for description and unit,
  CALL csv_record_getfield(csv) ! they correspond to grib information, not bufr Btable
  CALL init(conv_type(i)%v7d_var, btable=btable)

  CALL csv_record_getfield(csv, centre)
  CALL csv_record_getfield(csv, category)
  CALL csv_record_getfield(csv, number)
  CALL csv_record_getfield(csv, discipline)
  CALL init(conv_type(i)%vg6d_var, centre=centre, category=category, &
   number=number, discipline=discipline) ! controllare l'ordine

  CALL csv_record_getfield(csv, conv_type(i)%c_func%a)
  CALL csv_record_getfield(csv, conv_type(i)%c_func%b)
  CALL delete(csv)
ENDDO

200 CONTINUE

END SUBROUTINE import_var_conv

END SUBROUTINE vg6d_v7d_var_conv_setup


ELEMENTAL FUNCTION conv_func_eq(this, that) RESULT(res)
TYPE(conv_func),INTENT(IN) :: this, that
LOGICAL :: res

res = this%a == that%a .AND. this%b == that%b

END FUNCTION conv_func_eq


ELEMENTAL FUNCTION conv_func_ne(this, that) RESULT(res)
TYPE(conv_func),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION conv_func_ne


FUNCTION conv_func_mult(this, that) RESULT(mult)
TYPE(conv_func),INTENT(in) :: this
TYPE(conv_func),INTENT(in) :: that

TYPE(conv_func) :: mult

IF (this == conv_func_miss .OR. that == conv_func_miss) THEN
  mult = conv_func_miss
ELSE
  mult%a = this%a*that%a
  mult%b = this%a*that%b+this%b
ENDIF

END FUNCTION conv_func_mult

!> Apply the conversion function \a this to \a values.
!! The numerical conversion (only linear at the moment) defined by the
!! \a conv_func object \a this is applied to the \a values argument;
!! the converted result is stored in place; missing values remain
!! missing; if the conversion function is undefined (\a
!! conv_func_miss) the values are unchanged. The method is \c
!! ELEMENTAL, thus \a values can be also an array of any shape.
ELEMENTAL SUBROUTINE conv_func_compute(this, values)
TYPE(conv_func),INTENT(in) :: this !< object defining the conversion function
REAL,INTENT(inout) :: values !< value to be converted in place

IF (this /= conv_func_miss) THEN
  IF (c_e(values)) values = values*this%a + this%b
ELSE
  values=rmiss
ENDIF

END SUBROUTINE conv_func_compute


!> Return a copy of \a values converted by applying the conversion
!! function \a this.  The numerical conversion (only linear at the
!! moment) defined by the \a conv_func object \a this is applied to
!! the \a values argument and the converted result is returned;
!! missing values remain missing; if the conversion function is
!! undefined (\a conv_func_miss) the values are unchanged. The method
!! is \c ELEMENTAL, thus \a values can be also an array of any shape.
ELEMENTAL FUNCTION conv_func_convert(this, values) RESULT(convert)
TYPE(conv_func),intent(in) :: this !< object defining the conversion function
REAL,INTENT(in) :: values !< input value to be converted
REAL :: convert

convert = values
CALL compute(this, convert)

END FUNCTION conv_func_convert


!> Locate variables which are horizontal components of a vector field.
!! This method scans the \a volgrid6d_var array provided and locates
!! pairs of variables which are x and y component of the same vector
!! field.  On exit, the arrays \x xind(:) and \a yind(:) are allocated
!! to a size equal to the number of vector fields detected and their
!! corresponding elements will point to x and y components of the same
!! vector field. If inconsistencies are found, e.g. only one component
!! is detected for a field, or more than one input variable define
!! the same component, then \a xind and \a xind are nullified, thus an
!! error condition can be tested as \c .NOT.ASSOCIATED(xind). If no
!! vector fields are found then \a xind and \a xind are allocated to
!! zero size. If \a xind and \a yind are \c ASSOCIATED() after return,
!! they should be \c DEALLOCATEd by the calling procedure.
SUBROUTINE volgrid6d_var_hor_comp_index(this, xind, yind)
TYPE(volgrid6d_var),INTENT(in) :: this(:) !< array of volgrid6d_var objects (grib variable) to test
INTEGER,POINTER :: xind(:), yind(:) !< output arrays of indices pointing to matching horizontal components, allocated by this method

TYPE(vol7d_var) :: varbufr(SIZE(this))
TYPE(conv_func),POINTER :: c_func(:)
INTEGER :: i, nv, counts(SIZE(vol7d_var_horcomp))

NULLIFY(xind, yind)
counts(:) = 0

CALL vargrib2varbufr(this, varbufr, c_func)

DO i = 1, SIZE(vol7d_var_horcomp)
  counts(i) = COUNT(varbufr(:) == vol7d_var_horcomp(i))
ENDDO

IF (ANY(counts(1::2) > 1)) THEN
  CALL l4f_log(L4F_WARN, '> 1 variable refer to x component of the same field, (un)rotation impossible')
  DEALLOCATE(c_func)
  RETURN
ENDIF
IF (ANY(counts(2::2) > 1)) THEN
  CALL l4f_log(L4F_WARN, '> 1 variable refer to y component of the same field, (un)rotation impossible')
  DEALLOCATE(c_func)
  RETURN
ENDIF

! check that variables are paired and count pairs
nv = 0
DO i = 1, SIZE(vol7d_var_horcomp), 2
  IF (counts(i) == 0 .AND. counts(i+1) > 0) THEN
    CALL l4f_log(L4F_WARN, 'variable '//TRIM(vol7d_var_horcomp(i+1)%btable)// &
     ' present but the corresponding x-component '// &
     TRIM(vol7d_var_horcomp(i)%btable)//' is missing, (un)rotation impossible')
    RETURN
  ELSE  IF (counts(i+1) == 0 .AND. counts(i) > 0) THEN
    CALL l4f_log(L4F_WARN, 'variable '//TRIM(vol7d_var_horcomp(i)%btable)// &
     ' present but the corresponding y-component '// &
     TRIM(vol7d_var_horcomp(i+1)%btable)//' is missing, (un)rotation impossible')
    RETURN
  ENDIF
  IF (counts(i) == 1 .AND. counts(i+1) == 1) nv = nv + 1
ENDDO

! repeat the loop storing indices
ALLOCATE(xind(nv), yind(nv))
nv = 0
DO i = 1, SIZE(vol7d_var_horcomp), 2
  IF (counts(i) == 1 .AND. counts(i+1) == 1) THEN
    nv = nv + 1
    xind(nv) = INDEX(varbufr(:), vol7d_var_horcomp(i))
    yind(nv) = INDEX(varbufr(:), vol7d_var_horcomp(i+1))
  ENDIF
ENDDO
DEALLOCATE(c_func)

END SUBROUTINE volgrid6d_var_hor_comp_index


!> Tests whether a variable is the horizontal component of a vector field.
!! Returns \a .TRUE. if the corresponding variable is recognized as an
!! horizontal component of a vector field; if it is the case the
!! variable may need rotation in case of coordinate change.
FUNCTION volgrid6d_var_is_hor_comp(this) RESULT(is_hor_comp)
TYPE(volgrid6d_var),INTENT(in) :: this !< volgrid6d_var object (grib variable) to test
LOGICAL :: is_hor_comp

TYPE(vol7d_var) :: varbufr

varbufr = convert(this)
is_hor_comp = ANY(varbufr == vol7d_var_horcomp(:))

END FUNCTION volgrid6d_var_is_hor_comp

! before unstaggering??

!IF (.NOT. ALLOCATED(conv_fwd)) CALL vg6d_v7d_var_conv_setup()
!
!call init(varu,btable="B11003")
!call init(varv,btable="B11004")
!
! test about presence of u and v in standard table
!if ( index(conv_fwd(:)%v7d_var,varu) == 0  .or. index(conv_fwd(:)%v7d_var,varv) == 0 )then
!  call l4f_category_log(this%category,L4F_FATAL, &
!   "variables B11003 and/or B11004 (wind components) not defined by vg6d_v7d_var_conv_setup")
!  CALL raise_error()
!  RETURN
!end if
!
!if (associated(this%var))then
!  nvar=size(this%var)
!  allocate(varbufr(nvar),stat=stallo)
!  if (stallo /=0)then
!    call l4f_log(L4F_FATAL,"allocating memory")
!    call raise_fatal_error("allocating memory")
!  end if
!
!  CALL vargrib2varbufr(this%var, varbufr)
!ELSE
!  CALL l4f_category_log(this%category, L4F_ERROR, &
!   "trying to destagger an incomplete volgrid6d object")
!  CALL raise_error()
!  RETURN
!end if
!
!nvaru=COUNT(varbufr==varu)
!nvarv=COUNT(varbufr==varv)
!
!if (nvaru > 1 )then
!  call l4f_category_log(this%category,L4F_WARN, &
!   ">1 variables refer to u wind component, destaggering will not be done ")
!  DEALLOCATE(varbufr)
!  RETURN
!endif
!
!if (nvarv > 1 )then
!  call l4f_category_log(this%category,L4F_WARN, &
!   ">1 variables refer to v wind component, destaggering will not be done ")
!  DEALLOCATE(varbufr)
!  RETURN
!endif
!
!if (nvaru == 0 .and. nvarv == 0) then
!  call l4f_category_log(this%category,L4F_WARN, &
!   "no u or v wind component found in volume, nothing to do")
!  DEALLOCATE(varbufr)
!  RETURN
!endif
!
!if (COUNT(varbufr/=varu .and. varbufr/=varv) > 0) then
!  call l4f_category_log(this%category,L4F_WARN, &
!   "there are variables different from u and v wind component in C grid")
!endif


END MODULE volgrid6d_var_class
