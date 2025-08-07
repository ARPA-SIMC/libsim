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
MODULE vol7d_serialize_geojson_class
USE,INTRINSIC :: iso_c_binding
USE log4fortran
USE vol7d_class
USE vol7d_serialize_class
USE optionparser_class
USE char_utilities
IMPLICIT NONE

TYPE,EXTENDS(vol7d_serialize) :: vol7d_serialize_geojson
  PRIVATE
  CHARACTER(len=8),PUBLIC :: variant='simple'
  CONTAINS
  PROCEDURE :: vol7d_serialize_optionparser
  PROCEDURE :: vol7d_serialize_parse
  PROCEDURE :: vol7d_serialize_export
END TYPE vol7d_serialize_geojson

PRIVATE
PUBLIC vol7d_serialize_geojson, vol7d_serialize_geojson_new

CONTAINS

FUNCTION vol7d_serialize_geojson_new() RESULT(this)
TYPE(vol7d_serialize_geojson) :: this

this%vol7d_serialize = vol7d_serialize_new()

END FUNCTION vol7d_serialize_geojson_new


SUBROUTINE vol7d_serialize_optionparser(this, opt, ext)
CLASS(vol7d_serialize_geojson),INTENT(inout) :: this
TYPE(optionparser),INTENT(inout),OPTIONAL :: opt
CHARACTER(len=*),INTENT(in),OPTIONAL :: ext

IF (PRESENT(ext)) THEN
  this%ext = ext
ELSE
  this%ext = 'geojson'
ENDIF

! reset unconditionally some parameters
this%column='ana,time,timerange,level,network,var,value'
this%loop='time,timerange,level,var,ana,network'

! the generic method is not called since everything has a predefined
! configuration
! add a specific parameter
IF (present(opt)) &
 CALL optionparser_add(opt, ' ', TRIM(this%ext)//'-variant', this%variant, &
 this%variant, &
 help='variant of geojson output, accepted values are ''simple'' and ''rich''')

END SUBROUTINE vol7d_serialize_optionparser


SUBROUTINE vol7d_serialize_parse(this, category)
CLASS(vol7d_serialize_geojson),INTENT(inout) :: this !< object having undorgone the vol7d_serialize_optionparser method
TYPE(l4f_handle),INTENT(in),OPTIONAL :: category !< log4fortran category for logging error messages

! check own parameters
IF (this%variant /= 'simple' .AND. this%variant /= 'rich') THEN
  IF (PRESENT(category)) THEN
    CALL l4f_category_log(category, L4F_ERROR, 'error in command-line parameters')
    CALL l4f_category_log(category, L4F_ERROR, 'value '//TRIM(this%variant)// &
     ' not valid for --'//TRIM(this%ext)//'-variant parameter.')
  ENDIF
  CALL raise_error()
ENDIF
! call the original method
CALL this%vol7d_serialize%vol7d_serialize_parse(category)

END SUBROUTINE vol7d_serialize_parse


SUBROUTINE vol7d_serialize_export(this, iun)
CLASS(vol7d_serialize_geojson),INTENT(inout) :: this
INTEGER,INTENT(in),TARGET :: iun

INTEGER :: i, l
TYPE(vol7d_serialize_iterline) :: linei
TYPE(vol7d_serialize_itercol) :: coli

WRITE(iun,'(A)')'{"type":"FeatureCollection", "features":['

l = 0
linei = this%vol7d_serialize_iterline_new()

CALL linei%vol7d_serialize_iterline_set_callback(vol7d_ana_callback_gj, &
 vol7d_time_callback_gj, vol7d_level_callback_gj, &
 vol7d_timerange_callback_gj, vol7d_network_callback_gj, &
 vol7d_var_callback_gj, vol7d_attr_callback_gj, &
 vol7d_valuer_callback_gj, vol7d_valued_callback_gj, &
 vol7d_valuei_callback_gj, vol7d_valueb_callback_gj, vol7d_valuec_callback_gj, &
 vol7d_valuer_attr_callback_gj, vol7d_valued_attr_callback_gj, &
 vol7d_valuei_attr_callback_gj, vol7d_valueb_attr_callback_gj, vol7d_valuec_attr_callback_gj)

DO WHILE(linei%next())
! open feature
  IF (l == 0) THEN
    WRITE(iun,'(A)')'{'
  ELSE
    WRITE(iun,'(A)')',{'
  ENDIF
  l = l + 1

  i = 0
  coli = linei%vol7d_serialize_itercol_new()
  DO WHILE(coli%next())
    i = i + 1
    CALL coli%export(C_LOC(iun))
  END DO
! close property and feature
  WRITE(iun,'(A)')'}}'
END DO
! close feature list and featurecollection
WRITE(iun,'(A)')']}'

END SUBROUTINE vol7d_serialize_export


SUBROUTINE vol7d_ana_callback_gj(ana, genericptr)
TYPE(vol7d_ana), INTENT(in) :: ana
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun
REAL(kind=fp_geo) :: l1, l2

CALL C_F_POINTER(genericptr, iun)

CALL getval(ana%coord, lon=l1, lat=l2)
WRITE(iun,'(A)')'"type":"Feature", "geometry":{"type":"Point", "coordinates":['//t2c(l1, 'null')//', '//t2c(l2, 'null')//']},'
WRITE(iun,'(A)')'"properties":{'

END SUBROUTINE vol7d_ana_callback_gj


SUBROUTINE vol7d_time_callback_gj(time, genericptr)
TYPE(datetime), INTENT(in) :: time
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun
CHARACTER(len=19) :: isodate

CALL C_F_POINTER(genericptr, iun)

IF (time /= datetime_miss) THEN
  CALL getval(time, isodate=isodate)
  WRITE(iun,'(A)')'"datetime":"'//TRIM(isodate)//'",'
ELSE
  WRITE(iun,'(A)')'"datetime":null,'
ENDIF

END SUBROUTINE vol7d_time_callback_gj


SUBROUTINE vol7d_level_callback_gj(level, genericptr)
TYPE(vol7d_level), INTENT(in) :: level
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

WRITE(iun,'(A,/,A,/,A,/,A)') &
 '"level_t1":'//t2c(level%level1,'null')//',', &
 '"level_v1":'//t2c(level%l1,'null')//',', &
 '"level_t2":'//t2c(level%level2,'null')//',', &
 '"level_v2":'//t2c(level%l2,'null')//','

END SUBROUTINE vol7d_level_callback_gj


SUBROUTINE vol7d_timerange_callback_gj(timerange, genericptr)
TYPE(vol7d_timerange), INTENT(in) :: timerange
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

WRITE(iun,'(A,/,A,/,A)') &
 '"trange_pind":'//t2c(timerange%timerange,'null')//',', &
 '"trange_p1":'//t2c(timerange%p1,'null')//',', &
 '"trange_p2":'//t2c(timerange%p2,'null')//','

END SUBROUTINE vol7d_timerange_callback_gj


SUBROUTINE vol7d_network_callback_gj(network, genericptr)
TYPE(vol7d_network), INTENT(in) :: network
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

IF (c_e(network)) THEN
  WRITE(iun,'(A)')'"network":"'//TRIM(network%name)//'",'
ELSE
  WRITE(iun,'(A)')'"network":null,'
ENDIF

END SUBROUTINE vol7d_network_callback_gj


SUBROUTINE vol7d_var_callback_gj(var, genericptr)
TYPE(vol7d_var), INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

IF (c_e(var)) THEN
  WRITE(iun,'(A)')'"bcode":"'//TRIM(var%btable)//'",'
ELSE
  WRITE(iun,'(A)')'"bcode":null,'
ENDIF

END SUBROUTINE vol7d_var_callback_gj


SUBROUTINE vol7d_attr_callback_gj(var, attr, genericptr)
TYPE(vol7d_var), INTENT(in) :: var
TYPE(vol7d_var), INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

IF (c_e(var) .AND. c_e(attr)) THEN
  WRITE(iun,'(A)')'"bcode":"'//TRIM(var%btable)//'.'//TRIM(attr%btable)//'",'
ELSE
  WRITE(iun,'(A)')'"bcode":null,'
ENDIF

END SUBROUTINE vol7d_attr_callback_gj


SUBROUTINE vol7d_valuer_callback_gj(valu, var, genericptr)
REAL,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

WRITE(iun,'(A)')'"value":'//t2c(valu, 'null') !//','

END SUBROUTINE vol7d_valuer_callback_gj


SUBROUTINE vol7d_valued_callback_gj(valu, var, genericptr)
DOUBLE PRECISION,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

WRITE(iun,'(A)')'"value":'//t2c(valu, 'null') !//','

END SUBROUTINE vol7d_valued_callback_gj


SUBROUTINE vol7d_valuei_callback_gj(valu, var, genericptr)
INTEGER,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

IF (c_e(valu)) THEN
  IF (c_e(var%scalefactor) .AND. &
   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
    WRITE(iun,'(A)')'"value":'//t2c(realdat(valu, var)) !//','
  ELSE
    WRITE(iun,'(A)')'"value":'//t2c(valu) !//','
  ENDIF
ELSE
  WRITE(iun,'(A)')'"value":null'
ENDIF

END SUBROUTINE vol7d_valuei_callback_gj


SUBROUTINE vol7d_valueb_callback_gj(valu, var, genericptr)
INTEGER(kind=int_b),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

CALL vol7d_valuei_callback_gj(INT(valu), var, genericptr)

END SUBROUTINE vol7d_valueb_callback_gj


SUBROUTINE vol7d_valuec_callback_gj(valu, var, genericptr)
CHARACTER(len=*),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

IF (c_e(valu)) THEN
  IF (c_e(var%scalefactor) .AND. var%unit /= 'CCITTIA5' .AND. &
   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
    WRITE(iun,'(A)')'"value":'//t2c(realdat(valu, var)) !//','
  ELSE
    WRITE(iun,'(A)')'"value":"'//TRIM(valu)//'"'
  ENDIF
ELSE
  WRITE(iun,'(A)')'"value":null'
ENDIF

END SUBROUTINE vol7d_valuec_callback_gj


SUBROUTINE vol7d_valuer_attr_callback_gj(valu, var, attr, genericptr)
REAL,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CALL vol7d_valuer_callback_gj(valu, attr, genericptr)

END SUBROUTINE vol7d_valuer_attr_callback_gj


SUBROUTINE vol7d_valued_attr_callback_gj(valu, var, attr, genericptr)
DOUBLE PRECISION,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CALL vol7d_valued_callback_gj(valu, attr, genericptr)

END SUBROUTINE vol7d_valued_attr_callback_gj


SUBROUTINE vol7d_valuei_attr_callback_gj(valu, var, attr, genericptr)
INTEGER,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CALL vol7d_valuei_callback_gj(valu, attr, genericptr)

END SUBROUTINE vol7d_valuei_attr_callback_gj


SUBROUTINE vol7d_valueb_attr_callback_gj(valu, var, attr, genericptr)
INTEGER(kind=int_b),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CALL vol7d_valuei_callback_gj(INT(valu), attr, genericptr)

END SUBROUTINE vol7d_valueb_attr_callback_gj


SUBROUTINE vol7d_valuec_attr_callback_gj(valu, var, attr, genericptr)
CHARACTER(len=*),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL vol7d_valuec_callback_gj(valu, attr, genericptr)

END SUBROUTINE vol7d_valuec_attr_callback_gj

END MODULE vol7d_serialize_geojson_class
