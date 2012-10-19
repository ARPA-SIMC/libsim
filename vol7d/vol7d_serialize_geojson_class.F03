MODULE vol7d_serialize_geojson_class
USE,INTRINSIC :: iso_c_binding
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
  PROCEDURE :: vol7d_serialize_setup
  PROCEDURE :: vol7d_serialize_export
END TYPE vol7d_serialize_geojson

PRIVATE
PUBLIC vol7d_serialize_geojson

CONTAINS

SUBROUTINE vol7d_serialize_optionparser(this, opt, ext)
CLASS(vol7d_serialize_geojson),INTENT(inout) :: this
TYPE(optionparser),INTENT(inout),OPTIONAL :: opt
CHARACTER(len=*),INTENT(in),OPTIONAL :: ext

! reset unconditionally some parameters
this%column='ana,time,timerange,level,network,var,value'
this%loop='time,timerange,level,var,ana,network'
IF (PRESENT(ext)) THEN
  this%ext='geojson'
ELSE
  this%ext='geojson'
ENDIF

! the only configuration parameter:
IF (present(opt)) &
 CALL optionparser_add(opt, ' ', TRIM(this%ext)//'-variant', this%variant, &
 this%variant, &
 help='variant of geojson output, accepted values are ''simple'' and ''rich''')
! the original method is not called since the everything has a fixed
! configuration

END SUBROUTINE vol7d_serialize_optionparser


SUBROUTINE vol7d_serialize_parse(this, category)
CLASS(vol7d_serialize_geojson),INTENT(inout) :: this !< object having undorgone the vol7d_serialize_optionparser method
INTEGER,INTENT(in),OPTIONAL :: category !< log4fortran category for logging error messages

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


SUBROUTINE vol7d_serialize_setup(this, v7d)
CLASS(vol7d_serialize_geojson),INTENT(inout) :: this
TYPE(vol7d),INTENT(inout),TARGET :: v7d

! force configuration
!this%column='ana,time,timerange,level,network,var,value'
!this%loop='time,timerange,level,var,ana,network'
! call original method
CALL this%vol7d_serialize%vol7d_serialize_setup(v7d)

END SUBROUTINE vol7d_serialize_setup


SUBROUTINE vol7d_serialize_export(this, iun)
CLASS(vol7d_serialize_geojson),INTENT(inout) :: this
INTEGER,INTENT(in),TARGET :: iun

!CHARACTER(len=64),TARGET,ALLOCATABLE :: col(:)
CHARACTER(len=512),TARGET :: line
INTEGER :: i, l
TYPE(vol7d_serialize_iterline) :: linei
TYPE(vol7d_serialize_itercol) :: coli

!i = 0
!line = ''
!coli = this%vol7d_serialize_itercol_new()
!DO WHILE(coli%next())
!  i = i + 1
!  IF (i /= 1) THEN
!    line(LEN_TRIM(line)+1:) = ','
!  ENDIF
!  CALL coli%export(C_LOC(line(LEN_TRIM(line)+1:LEN_TRIM(line)+1)))
!END DO

! open featurecollection and feature list
WRITE(iun,'(A)')'{ "type": "FeatureCollection", "features": ['

!ALLOCATE(col(i))
l = 0
linei = this%vol7d_serialize_iterline_new()

CALL linei%vol7d_serialize_iterline_set_callback(vol7d_ana_callback_gj, &
 vol7d_time_callback_gj, vol7d_level_callback_gj, &
 vol7d_timerange_callback_gj, vol7d_network_callback_gj, &
 vol7d_var_callback_gj, vol7d_attr_callback_gj, vol7d_valuer_callback_gj, &
 vol7d_valued_callback_gj, vol7d_valuei_callback_gj, vol7d_valueb_callback_gj, &
 vol7d_valuec_callback_gj)

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
  WRITE(iun,'(A)')'} }'
END DO
! close feature list and featurecollection
WRITE(iun,'(A)')'] }'

END SUBROUTINE vol7d_serialize_export


SUBROUTINE vol7d_ana_callback_gj(ana, genericptr)
TYPE(vol7d_ana), INTENT(in) :: ana
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun
REAL(kind=fp_geo) :: l1, l2

CALL C_F_POINTER(genericptr, iun)

CALL getval(ana%coord, lon=l1, lat=l2)
WRITE(iun,'(A)')'"geometry": {"type": "Point",	"coordinates": [ '
IF (c_e(l1).AND.c_e(l2)) THEN
  WRITE(iun,'(A)') t2c(l1)//', '//t2c(l2)
ELSE
  WRITE(iun,'(A)')'NULL, NULL'
ENDIF
WRITE(iun,'(A)')' ] }, "properties": {'

END SUBROUTINE vol7d_ana_callback_gj


SUBROUTINE vol7d_time_callback_gj(time, genericptr)
TYPE(datetime), INTENT(in) :: time
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun
CHARACTER(len=19) :: isodate

CALL C_F_POINTER(genericptr, iun)

IF (time /= datetime_miss) THEN
  CALL getval(time, isodate=isodate)
  WRITE(iun,'(A)')'"reftime": "'//TRIM(isodate)//'",'
ELSE
  WRITE(iun,'(A)')'"reftime": NULL,'
ENDIF

END SUBROUTINE vol7d_time_callback_gj


SUBROUTINE vol7d_level_callback_gj(level, genericptr)
TYPE(vol7d_level), INTENT(in) :: level
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

WRITE(iun,'(A,/,A,/,A,/,A)') &
 '"level.t1": '//t2c_miss(level%level1,'NULL')//',', &
 '"level.l1": '//t2c_miss(level%l1,'NULL')//',', &
 '"level.t2": '//t2c_miss(level%level2,'NULL')//',', &
 '"level.l2": '//t2c_miss(level%l2,'NULL')//','

END SUBROUTINE vol7d_level_callback_gj


SUBROUTINE vol7d_timerange_callback_gj(timerange, genericptr)
TYPE(vol7d_timerange), INTENT(in) :: timerange
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

WRITE(iun,'(A,/,A,/,A)') &
 '"trange.type": '//t2c_miss(timerange%timerange,'NULL')//',', &
 '"trange.p1": '//t2c_miss(timerange%p1,'NULL')//',', &
 '"trange.p2": '//t2c_miss(timerange%p2,'NULL')//','

END SUBROUTINE vol7d_timerange_callback_gj


SUBROUTINE vol7d_network_callback_gj(network, genericptr)
TYPE(vol7d_network), INTENT(in) :: network
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

IF (c_e(network)) THEN
  WRITE(iun,'(A)')'"dataset": "'//TRIM(network%name)//'",'
ELSE
  WRITE(iun,'(A)')'"dataset": NULL,'
ENDIF

END SUBROUTINE vol7d_network_callback_gj


SUBROUTINE vol7d_var_callback_gj(var, genericptr)
TYPE(vol7d_var), INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

IF (c_e(var)) THEN
  WRITE(iun,'(A)')'"bcode": "'//TRIM(var%btable)//'",'
ELSE
  WRITE(iun,'(A)')'"bcode": NULL,'
ENDIF

END SUBROUTINE vol7d_var_callback_gj


SUBROUTINE vol7d_attr_callback_gj(var, attr, genericptr)
TYPE(vol7d_var), INTENT(in) :: var
TYPE(vol7d_var), INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

IF (c_e(var) .AND. c_e(attr)) THEN
  WRITE(iun,'(A)')'"bcode": "'//TRIM(var%btable)//'.'//TRIM(attr%btable)//'",'
ELSE
  WRITE(iun,'(A)')'"bcode": NULL,'
ENDIF

END SUBROUTINE vol7d_attr_callback_gj


SUBROUTINE vol7d_valuer_callback_gj(valu, genericptr)
REAL,INTENT(in) :: valu
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

!IF (c_e(valu)) THEN
  WRITE(iun,'(A)')'"value": '//t2c_miss(valu, 'NULL')//','
!ELSE
!  WRITE(iun,'(A)')'"value": NULL,'
!ENDIF

END SUBROUTINE vol7d_valuer_callback_gj


SUBROUTINE vol7d_valued_callback_gj(valu, genericptr)
DOUBLE PRECISION,INTENT(in) :: valu
TYPE(c_ptr),VALUE :: genericptr

INTEGER,POINTER :: iun

CALL C_F_POINTER(genericptr, iun)

!IF (c_e(valu)) THEN
  WRITE(iun,'(A)')'"value": '//t2c_miss(valu, 'NULL')//','
!ELSE
!  WRITE(iun,'(A)')'"value": NULL,'
!ENDIF

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
    WRITE(iun,'(A)')'"value": '//t2c(realdat(valu, var))//','
  ELSE
    WRITE(iun,'(A)')'"value": '//t2c(valu)//','
  ENDIF
ELSE
  WRITE(iun,'(A)')'"value": NULL,'
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
    WRITE(iun,'(A)')'"value": '//t2c(realdat(valu, var))//','
  ELSE
    WRITE(iun,'(A)')'"value": "'//TRIM(valu)//'",'
  ENDIF
ELSE
  WRITE(iun,'(A)')'"value": NULL,'
ENDIF

END SUBROUTINE vol7d_valuec_callback_gj

END MODULE vol7d_serialize_geojson_class
