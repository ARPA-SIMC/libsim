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

!> Extension of vol7d_class for serializing the contents of a volume.
!! This module defines a class which allows to iterate over all the
!! elements of a vol7d object and call specific callbacks for every
!! element encountered. The default callbacks provided generate a csv
!! representation of the volume, but che class can be extended in
!! F2003 sense and the callbacks can be redefined in order to obtain a
!! fully configurable serialized output from a vol7d object.
!!
!! \ingroup vol7d
MODULE vol7d_serialize_class
USE,INTRINSIC :: iso_c_binding
USE vol7d_class
USE array_utilities
USE file_utilities
USE optionparser_class
USE log4fortran
IMPLICIT NONE

TYPE vol7d_var_mapper
  INTEGER :: cat
  INTEGER :: typ
  INTEGER :: i5, i7
END TYPE vol7d_var_mapper


!> Class for serializing a vol7d object.
!! Configurable components are PUBLIC, while the others are PRIVATE and undocumented.
TYPE vol7d_serialize
  PRIVATE
  CHARACTER(len=512),PUBLIC :: column='time,timerange,ana,level,network' !< columns that appear in output
  CHARACTER(len=512),PUBLIC :: loop='time,timerange,ana,level,network' !< order of looping on vol7d descriptors, var is optional, all the others are compulsory
  CHARACTER(len=512),PUBLIC :: variable='all' !< list of variables to output, or 'all'
  CHARACTER(len=8),PUBLIC :: ext = 'ser' !< extension to use for command-line arguments
  LOGICAL,PUBLIC :: keep_miss=.FALSE. !< keep in output the elements with all missing data
  LOGICAL,PUBLIC :: no_rescale=.FALSE. !< do not rescale integer values in output
  LOGICAL,PUBLIC :: cachedesc=.FALSE. !< cache descriptors that do not change from one line to the next
  LOGICAL :: anaonly=.FALSE.
  INTEGER :: ndvar=5
  INTEGER :: icolumn(7), looporder(6), loopinvorder(6), &
   loopstart(6), loopend(6)
  TYPE(vol7d_var_mapper),ALLOCATABLE :: mapper(:)
  TYPE(vol7d),POINTER,public :: v7d=>NULL()
  PROCEDURE(default_vol7d_ana_header_callback),NOPASS,POINTER :: vol7d_ana_callback
  PROCEDURE(default_vol7d_time_header_callback),NOPASS,POINTER :: vol7d_time_callback
  PROCEDURE(default_vol7d_level_header_callback),NOPASS,POINTER :: vol7d_level_callback
  PROCEDURE(default_vol7d_timerange_header_callback),NOPASS,POINTER :: vol7d_timerange_callback
  PROCEDURE(default_vol7d_network_header_callback),NOPASS,POINTER :: vol7d_network_callback
  PROCEDURE(default_vol7d_var_header_callback),NOPASS,POINTER :: vol7d_var_callback
  PROCEDURE(default_vol7d_val_header_callback),NOPASS,POINTER :: vol7d_val_callback
  PROCEDURE(default_vol7d_value_var_header_callback),NOPASS,POINTER :: vol7d_value_var_callback
  PROCEDURE(default_vol7d_value_attr_header_callback),NOPASS,POINTER :: vol7d_value_attr_callback
  CONTAINS
  PROCEDURE :: vol7d_serialize_optionparser
  PROCEDURE :: vol7d_serialize_parse
  PROCEDURE :: vol7d_serialize_setup
  PROCEDURE :: vol7d_serialize_set_callback
  PROCEDURE :: vol7d_serialize_iterline_new
  PROCEDURE,PRIVATE :: vol7d_serialize_itercol_new_ser
  GENERIC :: vol7d_serialize_itercol_new=>vol7d_serialize_itercol_new_ser
END TYPE vol7d_serialize

!> Iterator object for iterating over "lines" in a vol7d serialization.
!! All the components are private. An object of this class is
!! constructed starting from a \a vol7d_serialize object as a result of
!! the \a vol7d_serialize_itercol_new() function method.
TYPE vol7d_serialize_iterline
  PRIVATE
  CLASS(vol7d_serialize),POINTER :: ser=>NULL()
  INTEGER :: loopind(6)
  INTEGER :: status=0
  INTEGER :: i1, i2, i3, i4, i5, i6
  INTEGER :: lastind(6)=0
  LOGICAL :: analine
  PROCEDURE(default_vol7d_ana_callback),NOPASS,POINTER :: vol7d_ana_callback
  PROCEDURE(default_vol7d_time_callback),NOPASS,POINTER :: vol7d_time_callback
  PROCEDURE(default_vol7d_level_callback),NOPASS,POINTER :: vol7d_level_callback
  PROCEDURE(default_vol7d_timerange_callback),NOPASS,POINTER :: vol7d_timerange_callback
  PROCEDURE(default_vol7d_network_callback),NOPASS,POINTER :: vol7d_network_callback
  PROCEDURE(default_vol7d_var_callback),NOPASS,POINTER :: vol7d_var_callback
  PROCEDURE(default_vol7d_attr_callback),NOPASS,POINTER :: vol7d_attr_callback
  PROCEDURE(default_vol7d_valuer_var_callback),NOPASS,POINTER :: vol7d_valuer_var_callback
  PROCEDURE(default_vol7d_valued_var_callback),NOPASS,POINTER :: vol7d_valued_var_callback
  PROCEDURE(default_vol7d_valuei_var_callback),NOPASS,POINTER :: vol7d_valuei_var_callback
  PROCEDURE(default_vol7d_valueb_var_callback),NOPASS,POINTER :: vol7d_valueb_var_callback
  PROCEDURE(default_vol7d_valuec_var_callback),NOPASS,POINTER :: vol7d_valuec_var_callback
  PROCEDURE(default_vol7d_valuer_attr_callback),NOPASS,POINTER :: vol7d_valuer_attr_callback
  PROCEDURE(default_vol7d_valued_attr_callback),NOPASS,POINTER :: vol7d_valued_attr_callback
  PROCEDURE(default_vol7d_valuei_attr_callback),NOPASS,POINTER :: vol7d_valuei_attr_callback
  PROCEDURE(default_vol7d_valueb_attr_callback),NOPASS,POINTER :: vol7d_valueb_attr_callback
  PROCEDURE(default_vol7d_valuec_attr_callback),NOPASS,POINTER :: vol7d_valuec_attr_callback
  CONTAINS
  PROCEDURE :: vol7d_serialize_iterline_set_callback
  PROCEDURE,PRIVATE :: vol7d_serialize_iterline_next
  GENERIC :: next=>vol7d_serialize_iterline_next
  PROCEDURE,PRIVATE :: vol7d_serialize_itercol_new_line
  GENERIC :: vol7d_serialize_itercol_new=>vol7d_serialize_itercol_new_line
END TYPE vol7d_serialize_iterline

!> Iterator object for iterating over "column" of a line in a vol7d serialization.
!! All the components are private. An object of this class is
!! constructed either starting from a \a vol7d_serialize
!! object (iterator over "header" columns) or from \a
!! vol7d_serialize_iterline object (iterator over "body" columns) as
!! a result of \a the vol7d_serialize_itercol_new() function method.
TYPE vol7d_serialize_itercol
  PRIVATE
  CLASS(vol7d_serialize),POINTER :: ser=>NULL()
  CLASS(vol7d_serialize_iterline),POINTER :: line=>NULL()
  INTEGER :: i=0, iend=-1
  LOGICAL :: forcemiss=.FALSE.
  CONTAINS
  PROCEDURE,PRIVATE :: vol7d_serialize_itercol_next
  GENERIC :: next=>vol7d_serialize_itercol_next
  PROCEDURE,PRIVATE :: vol7d_serialize_itercol_call
  GENERIC :: export=>vol7d_serialize_itercol_call
END TYPE vol7d_serialize_itercol

PRIVATE
PUBLIC vol7d_serialize, vol7d_serialize_new, &
 vol7d_serialize_iterline, vol7d_serialize_itercol

CONTAINS

!> Constructor for the vol7d_serialize class.
!! It has to be called when creating a new object for the sole purpose
!! of initializing the callbacks to default, which cannot be done
!! implicitly until a F2008 compiler is available.
FUNCTION vol7d_serialize_new() RESULT(this)
TYPE(vol7d_serialize) :: this

! these cannot be done in the class definition until F2008 pointer
! initialization
this%vol7d_ana_callback => default_vol7d_ana_header_callback
this%vol7d_time_callback => default_vol7d_time_header_callback
this%vol7d_level_callback => default_vol7d_level_header_callback
this%vol7d_timerange_callback => default_vol7d_timerange_header_callback
this%vol7d_network_callback => default_vol7d_network_header_callback
this%vol7d_var_callback => default_vol7d_var_header_callback
this%vol7d_val_callback => default_vol7d_val_header_callback
this%vol7d_value_var_callback => default_vol7d_value_var_header_callback
this%vol7d_value_attr_callback => default_vol7d_value_attr_header_callback

END FUNCTION vol7d_serialize_new


!> Method for adding the standard vol7d_serialize command-line options
!! to a program.
!! An initialised object of the optionparser class is used for adding
!! the relevant options, an extension is provided for formatting the
!! options and the help messages. Alternatively the configurable
!! options can be set directly since the corresponding members in the
!! \a vol7d_serialize class are PUBLIC. In both cases the \a
!! vol7d_serialize_parse method must be successively called in order
!! to process the configurable options.
SUBROUTINE vol7d_serialize_optionparser(this, opt, ext)
CLASS(vol7d_serialize),INTENT(inout) :: this
TYPE(optionparser),INTENT(inout),OPTIONAL :: opt
CHARACTER(len=*),INTENT(in),OPTIONAL :: ext

IF (PRESENT(ext)) this%ext = ext

IF (PRESENT(opt)) THEN
!CALL optionparser_add(opt, ' ', TRIM(this%ext)//'-volume', this%volume, &
! this%volume, help= &
! 'vol7d volumes to be output to csv: ''all'' for all volumes, &
! &''ana'' for station volumes only or ''data'' for data volumes only')
  CALL optionparser_add(opt, ' ', TRIM(this%ext)//'-column', this%column, &
   this%column, help= &
   'list of columns that have to appear in csv output: &
   &a comma-separated selection of ''time,timerange,level,ana,network,var,value'' &
   &in the desired order')
  CALL optionparser_add(opt, ' ', TRIM(this%ext)//'-loop', this%loop, &
   this%loop, help= &
   'order of looping on descriptors in csv output: &
   &a comma-separated selection of ''time,timerange,level,ana,network,var'' &
   &in the desired order, all the identifiers must be present, except ''var'', &
   &which, if present, enables looping on variables and attributes as well')
  CALL optionparser_add(opt, ' ', TRIM(this%ext)//'-variable', this%variable, &
   this%variable, help= &
   'list of variables that have to appear in the data columns of csv output: &
   &''all'' or a comma-separated list of B-table alphanumeric codes, e.g. &
   &''B10004,B12101'' in the desired order')
  CALL optionparser_add(opt, ' ', TRIM(this%ext)//'-keep-miss', this%keep_miss, &
   help='keep records containing only missing values in csv output, &
   &normally they are discarded')
  CALL optionparser_add(opt, ' ', TRIM(this%ext)//'-norescale', this%no_rescale, &
   help='do not rescale in output integer variables according to their &
   &scale factor')
ENDIF

END SUBROUTINE vol7d_serialize_optionparser


!> Method for parsing the command-line options provided.
!! It must be called after the call to optionparser_parse in order to
!! validate the options relevant to vol7d_serialize, in case of errors
!! the raise_error subroutine is called, with possible program abort.
!! It should be called also in case the configurable parameters are
!! set directly rather than through command-line.
SUBROUTINE vol7d_serialize_parse(this, category)
CLASS(vol7d_serialize),INTENT(inout) :: this !< object having undorgone the vol7d_serialize_optionparser method
INTEGER,INTENT(in),OPTIONAL :: category !< log4fortran category for logging error messages

CALL parse_v7d_column(this%column, this%icolumn, '--'//TRIM(this%ext)//'-column', &
 .FALSE., category)
CALL parse_v7d_column(this%loop, this%looporder, '--'//TRIM(this%ext)//'-loop', &
 .TRUE., category)

END SUBROUTINE vol7d_serialize_parse


! internal sobroutine to parse a string like
! 'time,timerange,level,ana,network,var,value' (ccol) transforming
! into an integer array of the corresponding PARAMETER values defined
! in vol7d_class (icol)
SUBROUTINE parse_v7d_column(ccol, icol, par_name, check_all, category)
CHARACTER(len=*),INTENT(in) :: ccol
INTEGER,INTENT(out) :: icol(:)
CHARACTER(len=*),INTENT(in) :: par_name
LOGICAL,INTENT(in) :: check_all
INTEGER,INTENT(in),OPTIONAL :: category

INTEGER :: i, j, nc
INTEGER,POINTER :: w_s(:), w_e(:)

nc = word_split(ccol, w_s, w_e, ',')
j = 0
icol(:) = -1
DO i = 1, MIN(nc, SIZE(icol))
  SELECT CASE(ccol(w_s(i):w_e(i)))
  CASE('time')
    j = j + 1
    icol(j) = vol7d_time_d
  CASE('timerange')
    j = j + 1
    icol(j) = vol7d_timerange_d
  CASE('level')
    j = j + 1
    icol(j) = vol7d_level_d
  CASE('ana')
    j = j + 1
    icol(j) = vol7d_ana_d
  CASE('var')
    j = j + 1
    icol(j) = vol7d_var_d
  CASE('network')
    j = j + 1
    icol(j) = vol7d_network_d
  CASE('value')
    j = j + 1
    icol(j) = 7
  CASE default
    IF (PRESENT(category)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error in command-line parameters, column '// &
       ccol(w_s(i):w_e(i))//' in '//TRIM(par_name)//' not valid.')
    ENDIF
    CALL raise_error()
  END SELECT
ENDDO
nc = j
DEALLOCATE(w_s, w_e)

IF (check_all) THEN
  IF (ALL(icol /= vol7d_time_d) .OR. ALL(icol /= vol7d_timerange_d) .OR. &
   ALL(icol /= vol7d_level_d) .OR. ALL(icol /= vol7d_ana_d) .OR. &
   ALL(icol /= vol7d_network_d)) THEN
    IF (PRESENT(category)) THEN
      CALL l4f_category_log(category, L4F_ERROR, &
       'error in command-line parameters, some columns missing in '// &
       TRIM(par_name)//' .')
    ENDIF
    CALL raise_error()
  ENDIF
  IF (ANY(icol == 7)) THEN
    IF (PRESENT(category)) THEN
      CALL l4f_category_log(category,L4F_ERROR,"column 'value' not valid in "// &
       TRIM(par_name)//' .')
    ENDIF
    CALL raise_error()
  ENDIF
ENDIF

END SUBROUTINE parse_v7d_column


SUBROUTINE vol7d_serialize_set_callback(this, vol7d_ana_callback, &
 vol7d_time_callback, vol7d_level_callback, &
 vol7d_timerange_callback, vol7d_network_callback, &
 vol7d_var_callback, vol7d_val_callback, vol7d_value_var_callback, &
 vol7d_value_attr_callback)
CLASS(vol7d_serialize),INTENT(inout) :: this
PROCEDURE(default_vol7d_ana_header_callback),OPTIONAL :: vol7d_ana_callback
PROCEDURE(default_vol7d_time_header_callback),OPTIONAL :: vol7d_time_callback
PROCEDURE(default_vol7d_level_header_callback),OPTIONAL :: vol7d_level_callback
PROCEDURE(default_vol7d_timerange_header_callback),OPTIONAL :: vol7d_timerange_callback
PROCEDURE(default_vol7d_network_header_callback),OPTIONAL :: vol7d_network_callback
PROCEDURE(default_vol7d_var_header_callback),OPTIONAL :: vol7d_var_callback
PROCEDURE(default_vol7d_val_header_callback),OPTIONAL :: vol7d_val_callback
PROCEDURE(default_vol7d_value_var_header_callback),OPTIONAL :: vol7d_value_var_callback
PROCEDURE(default_vol7d_value_attr_header_callback),OPTIONAL :: vol7d_value_attr_callback

IF (PRESENT(vol7d_ana_callback)) this%vol7d_ana_callback => vol7d_ana_callback
IF (PRESENT(vol7d_time_callback)) this%vol7d_time_callback => vol7d_time_callback
IF (PRESENT(vol7d_level_callback)) this%vol7d_level_callback => vol7d_level_callback
IF (PRESENT(vol7d_timerange_callback)) this%vol7d_timerange_callback => vol7d_timerange_callback
IF (PRESENT(vol7d_network_callback)) this%vol7d_network_callback => vol7d_network_callback
IF (PRESENT(vol7d_var_callback)) this%vol7d_var_callback => vol7d_var_callback
IF (PRESENT(vol7d_val_callback)) this%vol7d_val_callback => vol7d_val_callback
IF (PRESENT(vol7d_value_var_callback)) this%vol7d_value_var_callback => vol7d_value_var_callback
IF (PRESENT(vol7d_value_attr_callback)) this%vol7d_value_attr_callback => vol7d_value_attr_callback

END SUBROUTINE vol7d_serialize_set_callback


SUBROUTINE vol7d_serialize_iterline_set_callback(this, vol7d_ana_callback, &
 vol7d_time_callback, vol7d_level_callback, &
 vol7d_timerange_callback, vol7d_network_callback, &
 vol7d_var_callback, vol7d_attr_callback, vol7d_valuer_var_callback, &
 vol7d_valued_var_callback, vol7d_valuei_var_callback, vol7d_valueb_var_callback, &
 vol7d_valuec_var_callback, &
 vol7d_valuer_attr_callback, &
 vol7d_valued_attr_callback, vol7d_valuei_attr_callback, vol7d_valueb_attr_callback, &
 vol7d_valuec_attr_callback)
CLASS(vol7d_serialize_iterline),INTENT(inout) :: this
PROCEDURE(default_vol7d_ana_callback),OPTIONAL :: vol7d_ana_callback
PROCEDURE(default_vol7d_time_callback),OPTIONAL :: vol7d_time_callback
PROCEDURE(default_vol7d_level_callback),OPTIONAL :: vol7d_level_callback
PROCEDURE(default_vol7d_timerange_callback),OPTIONAL :: vol7d_timerange_callback
PROCEDURE(default_vol7d_network_callback),OPTIONAL :: vol7d_network_callback
PROCEDURE(default_vol7d_var_callback),OPTIONAL :: vol7d_var_callback
PROCEDURE(default_vol7d_attr_callback),OPTIONAL :: vol7d_attr_callback
PROCEDURE(default_vol7d_valuer_var_callback),OPTIONAL :: vol7d_valuer_var_callback
PROCEDURE(default_vol7d_valued_var_callback),OPTIONAL :: vol7d_valued_var_callback
PROCEDURE(default_vol7d_valuei_var_callback),OPTIONAL :: vol7d_valuei_var_callback
PROCEDURE(default_vol7d_valueb_var_callback),OPTIONAL :: vol7d_valueb_var_callback
PROCEDURE(default_vol7d_valuec_var_callback),OPTIONAL :: vol7d_valuec_var_callback
PROCEDURE(default_vol7d_valuer_attr_callback),OPTIONAL :: vol7d_valuer_attr_callback
PROCEDURE(default_vol7d_valued_attr_callback),OPTIONAL :: vol7d_valued_attr_callback
PROCEDURE(default_vol7d_valuei_attr_callback),OPTIONAL :: vol7d_valuei_attr_callback
PROCEDURE(default_vol7d_valueb_attr_callback),OPTIONAL :: vol7d_valueb_attr_callback
PROCEDURE(default_vol7d_valuec_attr_callback),OPTIONAL :: vol7d_valuec_attr_callback

IF (PRESENT(vol7d_ana_callback)) this%vol7d_ana_callback => vol7d_ana_callback
IF (PRESENT(vol7d_time_callback)) this%vol7d_time_callback => vol7d_time_callback
IF (PRESENT(vol7d_level_callback)) this%vol7d_level_callback => vol7d_level_callback
IF (PRESENT(vol7d_timerange_callback)) this%vol7d_timerange_callback => vol7d_timerange_callback
IF (PRESENT(vol7d_network_callback)) this%vol7d_network_callback => vol7d_network_callback
IF (PRESENT(vol7d_var_callback)) this%vol7d_var_callback => vol7d_var_callback
IF (PRESENT(vol7d_attr_callback)) this%vol7d_attr_callback => vol7d_attr_callback
IF (PRESENT(vol7d_valuer_var_callback)) this%vol7d_valuer_var_callback => vol7d_valuer_var_callback
IF (PRESENT(vol7d_valued_var_callback)) this%vol7d_valued_var_callback => vol7d_valued_var_callback
IF (PRESENT(vol7d_valuei_var_callback)) this%vol7d_valuei_var_callback => vol7d_valuei_var_callback
IF (PRESENT(vol7d_valueb_var_callback)) this%vol7d_valueb_var_callback => vol7d_valueb_var_callback
IF (PRESENT(vol7d_valuec_var_callback)) this%vol7d_valuec_var_callback => vol7d_valuec_var_callback
IF (PRESENT(vol7d_valuer_attr_callback)) this%vol7d_valuer_attr_callback => vol7d_valuer_attr_callback
IF (PRESENT(vol7d_valued_attr_callback)) this%vol7d_valued_attr_callback => vol7d_valued_attr_callback
IF (PRESENT(vol7d_valuei_attr_callback)) this%vol7d_valuei_attr_callback => vol7d_valuei_attr_callback
IF (PRESENT(vol7d_valueb_attr_callback)) this%vol7d_valueb_attr_callback => vol7d_valueb_attr_callback
IF (PRESENT(vol7d_valuec_attr_callback)) this%vol7d_valuec_attr_callback => vol7d_valuec_attr_callback

END SUBROUTINE vol7d_serialize_iterline_set_callback


SUBROUTINE vol7d_serialize_setup(this, v7d)
CLASS(vol7d_serialize),INTENT(inout) :: this
TYPE(vol7d),INTENT(in),TARGET :: v7d

INTEGER :: nv, nav, ndv, i, j, n
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d_var_mapper),ALLOCATABLE :: mapper_tmp(:)

!!CALL vol7d_alloc_vol(v7d) ! be safe
this%v7d => v7d

! Eliminate together with checkvarvect if the next section works well!!!
! Filter requested variables
!IF (this%variable /= 'all') THEN
!  nv = word_split(this%variable, w_s, w_e, ',')
!  CALL checkvarvect(v7d%anavar)
!  CALL checkvarvect(v7d%anaattr)
!  CALL checkvarvect(v7d%anavarattr)
!  CALL checkvarvect(v7d%dativar)
!  CALL checkvarvect(v7d%datiattr)
!  CALL checkvarvect(v7d%dativarattr)
!  CALL vol7d_reform(v7d, miss=.TRUE.) ! sort?
!  DEALLOCATE(w_s, w_e)
!ENDIF

CALL var_mapper(this%mapper, v7d)

! Filter and sort requested variables
IF (this%variable /= 'all') THEN
  nv = word_split(this%variable, w_s, w_e, ',')
  ALLOCATE(mapper_tmp(nv))
  j = 0
  DO i = 1, nv
    n = var_mapper_searchvar(this%mapper, v7d, &
     vol7d_var_new(btable=this%variable(w_s(i):w_e(i))))
    IF (n > 0) THEN
      j = j + 1
      mapper_tmp(j) = this%mapper(n)
    ENDIF
  ENDDO
  DEALLOCATE(this%mapper) ! why must I do these dealloc/alloc with gfortran??
  ALLOCATE(this%mapper(j)) ! -fcheck-bounds complains otherwise!!
  this%mapper = mapper_tmp(1:j)
  DEALLOCATE(w_s, w_e)
ENDIF

! If only ana volume, skip data-only dimensions
IF (SIZE(v7d%time) == 0) THEN
  WHERE (this%icolumn(:) == vol7d_time_d)
    this%icolumn(:) = -1
  END WHERE
ENDIF
IF (SIZE(v7d%level) == 0) THEN
  WHERE (this%icolumn(:) == vol7d_level_d)
    this%icolumn(:) = -1
  END WHERE
ENDIF
IF (SIZE(v7d%timerange) == 0) THEN
  WHERE (this%icolumn(:) == vol7d_timerange_d)
    this%icolumn(:) = -1
  END WHERE
ENDIF
this%anaonly = SIZE(v7d%time) == 0 .AND. SIZE(v7d%level) == 0 .AND. &
 SIZE(v7d%timerange) == 0

nav = COUNT(this%mapper(:)%cat == 1)
ndv = COUNT(this%mapper(:)%cat == 3)

! For column reordering
this%loopstart(:) = 1
this%loopend(:) = 0
WHERE (this%looporder(:) == vol7d_ana_d)
  this%loopend(:) = SIZE(v7d%ana)
END WHERE
WHERE (this%looporder(:) == vol7d_time_d)
  this%loopend(:) = SIZE(v7d%time)
END WHERE
WHERE (this%looporder(:) == vol7d_level_d)
  this%loopend(:) = SIZE(v7d%level)
END WHERE
WHERE (this%looporder(:) == vol7d_timerange_d)
  this%loopend(:) = SIZE(v7d%timerange)
END WHERE
WHERE (this%looporder(:) == vol7d_var_d)
  this%loopend(:) = SIZE(this%mapper)
END WHERE
WHERE (this%looporder(:) == vol7d_network_d)
  this%loopend(:) = SIZE(v7d%network)
END WHERE

! invert this%looporder
this%loopinvorder(vol7d_ana_d) = firsttrue(this%looporder(:) == vol7d_ana_d)
this%loopinvorder(vol7d_time_d) = firsttrue(this%looporder(:) == vol7d_time_d)
this%loopinvorder(vol7d_level_d) = firsttrue(this%looporder(:) == vol7d_level_d)
this%loopinvorder(vol7d_timerange_d) = firsttrue(this%looporder(:) == vol7d_timerange_d)
this%loopinvorder(vol7d_var_d) = firsttrue(this%looporder(:) == vol7d_var_d)
this%loopinvorder(vol7d_network_d) = firsttrue(this%looporder(:) == vol7d_network_d)
! there should not be missing columns here except
! this%loopinvorder(vol7d_var_d) thanks to the check in
! parse_v7d_column
IF (this%loopinvorder(vol7d_var_d) <= 0) THEN
  this%ndvar = 5
ELSE
  this%ndvar = 6
ENDIF

CONTAINS

SUBROUTINE checkvarvect(varvect)
TYPE(vol7d_varvect),INTENT(inout) :: varvect

CALL checkvar(varvect%r)
CALL checkvar(varvect%d)
CALL checkvar(varvect%i)
CALL checkvar(varvect%b)
CALL checkvar(varvect%c)

END SUBROUTINE checkvarvect

SUBROUTINE checkvar(var)
TYPE(vol7d_var),POINTER :: var(:)

INTEGER :: i, j

IF (.NOT.ASSOCIATED(var)) RETURN

v7dvarloop: DO i = 1, SIZE(var)
  csvvarloop:  DO j = 1, nv
    IF (var(i)%btable == this%variable(w_s(j):w_e(j))) THEN
      CYCLE v7dvarloop
    ENDIF
  ENDDO csvvarloop
  var(i) = vol7d_var_miss ! var not found, nullify
ENDDO v7dvarloop

END SUBROUTINE checkvar

END SUBROUTINE vol7d_serialize_setup


FUNCTION vol7d_serialize_iterline_new(this) RESULT(iterator)
CLASS(vol7d_serialize),INTENT(in),TARGET :: this
TYPE(vol7d_serialize_iterline) :: iterator

iterator%ser => this
iterator%loopind(:) = this%loopstart(:)
! these cannot be done in the class definition until F2008 pointer
! initialization
iterator%vol7d_ana_callback => default_vol7d_ana_callback
iterator%vol7d_time_callback => default_vol7d_time_callback
iterator%vol7d_level_callback => default_vol7d_level_callback
iterator%vol7d_timerange_callback => default_vol7d_timerange_callback
iterator%vol7d_network_callback => default_vol7d_network_callback
iterator%vol7d_var_callback => default_vol7d_var_callback
iterator%vol7d_attr_callback => default_vol7d_attr_callback
iterator%vol7d_valuer_var_callback => default_vol7d_valuer_var_callback
iterator%vol7d_valued_var_callback => default_vol7d_valued_var_callback
iterator%vol7d_valuei_var_callback => default_vol7d_valuei_var_callback
iterator%vol7d_valueb_var_callback => default_vol7d_valueb_var_callback
iterator%vol7d_valuec_var_callback => default_vol7d_valuec_var_callback
iterator%vol7d_valuer_attr_callback => default_vol7d_valuer_attr_callback
iterator%vol7d_valued_attr_callback => default_vol7d_valued_attr_callback
iterator%vol7d_valuei_attr_callback => default_vol7d_valuei_attr_callback
iterator%vol7d_valueb_attr_callback => default_vol7d_valueb_attr_callback
iterator%vol7d_valuec_attr_callback => default_vol7d_valuec_attr_callback

END FUNCTION vol7d_serialize_iterline_new


FUNCTION vol7d_serialize_iterline_next(this) RESULT(next)
CLASS(vol7d_serialize_iterline),INTENT(inout) :: this
LOGICAL :: next

INTEGER :: i

IF (.NOT.ASSOCIATED(this%ser)) THEN ! safety check
  this%status = 3
  next = .FALSE.
  RETURN
ENDIF

loop7d: DO WHILE(.TRUE.)

IF (this%status == 0) THEN ! first iteration
  this%status = 1
! safety check for empty volumes
  IF (ANY(this%loopind(1:this%ser%ndvar) > this%ser%loopend(1:this%ser%ndvar))) THEN
    next = .FALSE.
    this%status = 2
    RETURN
  ENDIF

ELSE ! following iterations
! final part of the loop over columns
  DO i = this%ser%ndvar, 1, -1
    IF (this%loopind(i) < this%ser%loopend(i)) THEN ! increment loop index
      this%loopind(i) = this%loopind(i) + 1
      EXIT
    ELSE ! end of loop for this index, reset and increment next index
      this%loopind(i) = this%ser%loopstart(i)
    ENDIF
  ENDDO
  IF (i == 0) THEN ! all counters have reached the end
    next = .FALSE.
    this%status = 2
    RETURN
  ENDIF
ENDIF ! first iteration

! set indices, use pointers?
  this%i1 = this%loopind(this%ser%loopinvorder(vol7d_ana_d))
  this%i2 = this%loopind(this%ser%loopinvorder(vol7d_time_d))
  this%i3 = this%loopind(this%ser%loopinvorder(vol7d_level_d))
  this%i4 = this%loopind(this%ser%loopinvorder(vol7d_timerange_d))
  this%i6 = this%loopind(this%ser%loopinvorder(vol7d_network_d))

  IF (this%ser%ndvar == 5) THEN ! all variables in one line
    this%i5 = 0
    this%analine = .FALSE.
    IF (.NOT.this%ser%keep_miss) THEN ! check whether the line has valid data
      IF (var_mapper_miss(this%ser%mapper, this%ser%v7d, &
       this%i1, this%i2, this%i3, this%i4, this%i6, this%ser%anaonly)) CYCLE
    ENDIF
  ELSE ! one variable per line
    this%i5 = this%loopind(this%ser%loopinvorder(vol7d_var_d))
    this%analine = (this%ser%mapper(this%i5)%cat <= 2)
! do not repeat ana variables for every data entry
    IF (this%analine) THEN
      IF (this%i2 /= 1 .OR. this%i3 /= 1 .OR. this%i4 /= 1) CYCLE
    ENDIF
    IF (.NOT.this%ser%keep_miss) THEN ! check whether the line has valid data
      IF (var_mapper_miss(this%ser%mapper(this%i5:this%i5), this%ser%v7d, &
       this%i1, this%i2, this%i3, this%i4, this%i6, this%analine)) CYCLE
    ENDIF
  ENDIF

  next = .TRUE.
  RETURN

END DO loop7d

END FUNCTION vol7d_serialize_iterline_next


FUNCTION vol7d_serialize_itercol_new_ser(this) RESULT(iterator)
CLASS(vol7d_serialize),INTENT(in),TARGET :: this
TYPE(vol7d_serialize_itercol) :: iterator

iterator%ser => this
IF (this%ndvar == 5) THEN ! all variables in one line
  iterator%iend = SIZE(this%icolumn) + SIZE(this%mapper)
ELSE ! one variable per line
  iterator%iend = SIZE(this%icolumn)
ENDIF

END FUNCTION vol7d_serialize_itercol_new_ser


FUNCTION vol7d_serialize_itercol_new_line(this) RESULT(iterator)
CLASS(vol7d_serialize_iterline),INTENT(in),TARGET :: this
TYPE(vol7d_serialize_itercol) :: iterator

iterator%ser => this%ser
iterator%line => this
iterator%i = 0 ! 1?
IF (this%i5 == 0) THEN ! all variables in one line
  iterator%iend = SIZE(this%ser%icolumn) + SIZE(this%ser%mapper)
ELSE ! one variable per line
  iterator%iend = SIZE(this%ser%icolumn)
ENDIF


END FUNCTION vol7d_serialize_itercol_new_line


FUNCTION vol7d_serialize_itercol_next(this) RESULT(next)
CLASS(vol7d_serialize_itercol),INTENT(inout) :: this
LOGICAL :: next

INTEGER :: icol

DO WHILE(this%i < this%iend)
  this%i = this%i + 1
!  IF (this%line%i5 == 0) THEN ! all variables in one line
  IF (this%i <= SIZE(this%ser%icolumn)) THEN
    icol = this%ser%icolumn(this%i)
    IF (icol > 0) THEN
      next = .TRUE.
      RETURN
!    ELSE
! skip to the next
    ENDIF
  ELSE ! we are in the variable part of a "all variables in one line" line
    IF (ASSOCIATED(this%line)) this%line%i5 = this%line%i5 + 1
    next = .TRUE.
    RETURN ! always return unconditioned
  ENDIF
ENDDO ! end of columns

next = .FALSE.

END FUNCTION vol7d_serialize_itercol_next


SUBROUTINE vol7d_serialize_itercol_call(this, genericptr)
CLASS(vol7d_serialize_itercol),INTENT(inout) :: this
TYPE(c_ptr),VALUE :: genericptr

INTEGER :: icol, icolorder

IF (ASSOCIATED(this%line)) THEN ! body line iterator

  IF (this%i <= SIZE(this%ser%icolumn)) THEN
    icol = this%ser%icolumn(this%i)
  ELSE
    icol = 7 ! value
  ENDIF

  IF (icol < 7) THEN ! descriptor column (was this%i <= 7)
    icolorder = this%ser%loopinvorder(icol)
! check whether column is in cache
    IF (this%line%lastind(icolorder) == this%line%loopind(icolorder) &
     .AND. this%ser%cachedesc) RETURN
! check whether column is not requested because line is ana only
    this%forcemiss = this%line%analine .AND. &
     icol /= vol7d_ana_d .AND. icol /= vol7d_network_d
! call callback
    CALL call_desc_callback(this, genericptr)
    IF (this%forcemiss) THEN
! invalidate cache
      this%line%lastind(icolorder) = 0
    ELSE
! update cache
      this%line%lastind(icolorder) = this%line%loopind(icolorder)
    ENDIF

  ELSE ! it is a column with a value
    CALL call_value_callback(this, genericptr)

  ENDIF

ELSE ! header line iterator
  IF (this%i <= 7) THEN ! descriptor column
    CALL call_header_desc_callback(this, genericptr)
  ELSE ! it is a column with a value => a variable is used as a header
    CALL call_header_value_callback(this, genericptr)
  ENDIF
ENDIF

END SUBROUTINE vol7d_serialize_itercol_call


SUBROUTINE call_header_desc_callback(this, genericptr)
CLASS(vol7d_serialize_itercol),INTENT(in) :: this
TYPE(c_ptr),VALUE :: genericptr

SELECT CASE(this%ser%icolumn(this%i))

CASE(vol7d_ana_d)
  CALL this%ser%vol7d_ana_callback(genericptr)

CASE(vol7d_time_d)
  CALL this%ser%vol7d_time_callback(genericptr)

CASE(vol7d_level_d)
  CALL this%ser%vol7d_level_callback(genericptr)

CASE(vol7d_timerange_d)
  CALL this%ser%vol7d_timerange_callback(genericptr)

CASE(vol7d_network_d)
  CALL this%ser%vol7d_network_callback(genericptr)

CASE(vol7d_var_d)
  CALL this%ser%vol7d_var_callback(genericptr)

CASE(7)
  CALL this%ser%vol7d_val_callback(genericptr)

END SELECT

END SUBROUTINE call_header_desc_callback


SUBROUTINE call_header_value_callback(this, genericptr)
CLASS(vol7d_serialize_itercol),INTENT(in) :: this
TYPE(c_ptr),VALUE :: genericptr

INTEGER :: ind, varind, attrind

! here the variable index this%line%i5 is not available, I use this%i-7 as a proxy
ind = this%i - 7
varind = this%ser%mapper(ind)%i5
attrind = this%ser%mapper(ind)%i7

SELECT CASE(this%ser%mapper(ind)%cat)
CASE(1)
  SELECT CASE(this%ser%mapper(ind)%typ)
  CASE(1)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%anavar%r(varind), 'ra', genericptr)
  CASE(2)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%anavar%d(varind), 'da', genericptr)
  CASE(3)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%anavar%i(varind), 'ia', genericptr)
  CASE(4)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%anavar%b(varind), 'ba', genericptr)
  CASE(5)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%anavar%c(varind), 'ca', genericptr)
  END SELECT
CASE(2)
  SELECT CASE(this%ser%mapper(ind)%typ)
  CASE(1)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%anavarattr%r(varind), &
     this%ser%v7d%anaattr%r(attrind), 'ra', genericptr)
  CASE(2)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%anavarattr%d(varind), &
     this%ser%v7d%anaattr%d(attrind), 'da', genericptr)
  CASE(3)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%anavarattr%i(varind), &
     this%ser%v7d%anaattr%i(attrind), 'ia', genericptr)
  CASE(4)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%anavarattr%b(varind), &
     this%ser%v7d%anaattr%b(attrind), 'ba', genericptr)
  CASE(5)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%anavarattr%c(varind), &
     this%ser%v7d%anaattr%c(attrind), 'ca', genericptr)
  END SELECT
CASE(3)
  SELECT CASE(this%ser%mapper(ind)%typ)
  CASE(1)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%dativar%r(varind), 'rd', genericptr)
  CASE(2)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%dativar%d(varind), 'dd', genericptr)
  CASE(3)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%dativar%i(varind), 'id', genericptr)
  CASE(4)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%dativar%b(varind), 'bd', genericptr)
  CASE(5)
    CALL this%ser%vol7d_value_var_callback(this%ser%v7d%dativar%c(varind), 'cd', genericptr)
  END SELECT
CASE(4)
  SELECT CASE(this%ser%mapper(ind)%typ)
  CASE(1)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%dativarattr%r(varind), &
     this%ser%v7d%datiattr%r(attrind), 'rd', genericptr)
  CASE(2)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%dativarattr%d(varind), &
     this%ser%v7d%datiattr%d(attrind), 'dd', genericptr)
  CASE(3)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%dativarattr%i(varind), &
     this%ser%v7d%datiattr%i(attrind), 'id', genericptr)
  CASE(4)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%dativarattr%b(varind), &
     this%ser%v7d%datiattr%b(attrind), 'bd', genericptr)
  CASE(5)
    CALL this%ser%vol7d_value_attr_callback(this%ser%v7d%dativarattr%c(varind), &
     this%ser%v7d%datiattr%c(attrind), 'cd', genericptr)
  END SELECT
END SELECT

END SUBROUTINE call_header_value_callback


SUBROUTINE default_vol7d_ana_header_callback(genericptr)
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = 'Longitude,Latitude'

END SUBROUTINE default_vol7d_ana_header_callback

SUBROUTINE default_vol7d_time_header_callback(genericptr)
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = 'Date'

END SUBROUTINE default_vol7d_time_header_callback

SUBROUTINE default_vol7d_level_header_callback(genericptr)
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = 'Level1,L1,Level2,L2'

END SUBROUTINE default_vol7d_level_header_callback

SUBROUTINE default_vol7d_timerange_header_callback(genericptr)
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = 'Time range,P1,P2'

END SUBROUTINE default_vol7d_timerange_header_callback

SUBROUTINE default_vol7d_network_header_callback(genericptr)
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = 'Report'

END SUBROUTINE default_vol7d_network_header_callback

SUBROUTINE default_vol7d_var_header_callback(genericptr)
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = 'Variable'

END SUBROUTINE default_vol7d_var_header_callback

SUBROUTINE default_vol7d_val_header_callback(genericptr)
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = 'Value'

END SUBROUTINE default_vol7d_val_header_callback

SUBROUTINE default_vol7d_value_var_header_callback(var, typ, genericptr)
TYPE(vol7d_var),INTENT(in) :: var
CHARACTER(len=2),INTENT(in) :: typ
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = var%btable

END SUBROUTINE default_vol7d_value_var_header_callback

SUBROUTINE default_vol7d_value_attr_header_callback(var, attr, typ, genericptr)
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
CHARACTER(len=2),INTENT(in) :: typ
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = TRIM(var%btable)//'.'//attr%btable

END SUBROUTINE default_vol7d_value_attr_header_callback


SUBROUTINE call_desc_callback(this, genericptr)
CLASS(vol7d_serialize_itercol),INTENT(in) :: this
TYPE(c_ptr),VALUE :: genericptr

INTEGER :: icol, ind, varind, attrind

icol = this%ser%icolumn(this%i)
ind = this%line%loopind(this%ser%loopinvorder(icol))

SELECT CASE(icol)

CASE(vol7d_ana_d)
  CALL this%line%vol7d_ana_callback(this%ser%v7d%ana(ind), genericptr)

CASE(vol7d_time_d)
  IF (this%forcemiss) THEN
    CALL this%line%vol7d_time_callback(datetime_miss, genericptr)
  ELSE
    CALL this%line%vol7d_time_callback(this%ser%v7d%time(ind), genericptr)
  ENDIF

CASE(vol7d_level_d)
  IF (this%forcemiss) THEN
    CALL this%line%vol7d_level_callback(vol7d_level_miss, genericptr)
  ELSE
    CALL this%line%vol7d_level_callback(this%ser%v7d%level(ind), genericptr)
  ENDIF

CASE(vol7d_timerange_d)
  IF (this%forcemiss) THEN
    CALL this%line%vol7d_timerange_callback(vol7d_timerange_miss, genericptr)
  ELSE
    CALL this%line%vol7d_timerange_callback(this%ser%v7d%timerange(ind), genericptr)
  ENDIF

CASE(vol7d_network_d)
  CALL this%line%vol7d_network_callback(this%ser%v7d%network(ind), genericptr)

CASE(vol7d_var_d)
  varind = this%ser%mapper(ind)%i5
  attrind = this%ser%mapper(ind)%i7
  SELECT CASE(this%ser%mapper(ind)%cat)
  CASE(1)
    SELECT CASE(this%ser%mapper(ind)%typ)
    CASE(1)
      CALL this%line%vol7d_var_callback(this%ser%v7d%anavar%r(varind), genericptr)
    CASE(2)
      CALL this%line%vol7d_var_callback(this%ser%v7d%anavar%d(varind), genericptr)
    CASE(3)
      CALL this%line%vol7d_var_callback(this%ser%v7d%anavar%i(varind), genericptr)
    CASE(4)
      CALL this%line%vol7d_var_callback(this%ser%v7d%anavar%b(varind), genericptr)
    CASE(5)
      CALL this%line%vol7d_var_callback(this%ser%v7d%anavar%c(varind), genericptr)
  END SELECT
  CASE(2)
    SELECT CASE(this%ser%mapper(ind)%typ)
    CASE(1)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%anavarattr%r(varind), &
       this%ser%v7d%anaattr%r(attrind), genericptr)
    CASE(2)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%anavarattr%d(varind), &
       this%ser%v7d%anaattr%d(attrind), genericptr)
    CASE(3)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%anavarattr%i(varind), &
       this%ser%v7d%anaattr%i(attrind), genericptr)
    CASE(4)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%anavarattr%b(varind), &
       this%ser%v7d%anaattr%b(attrind), genericptr)
    CASE(5)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%anavarattr%c(varind), &
       this%ser%v7d%anaattr%c(attrind), genericptr)
  END SELECT
  CASE(3)
    SELECT CASE(this%ser%mapper(ind)%typ)
    CASE(1)
      CALL this%line%vol7d_var_callback(this%ser%v7d%dativar%r(varind), genericptr)
    CASE(2)
      CALL this%line%vol7d_var_callback(this%ser%v7d%dativar%d(varind), genericptr)
    CASE(3)
      CALL this%line%vol7d_var_callback(this%ser%v7d%dativar%i(varind), genericptr)
    CASE(4)
      CALL this%line%vol7d_var_callback(this%ser%v7d%dativar%b(varind), genericptr)
    CASE(5)
      CALL this%line%vol7d_var_callback(this%ser%v7d%dativar%c(varind), genericptr)
  END SELECT
  CASE(4)
    SELECT CASE(this%ser%mapper(ind)%typ)
    CASE(1)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%dativarattr%r(varind), &
       this%ser%v7d%datiattr%r(attrind), genericptr)
    CASE(2)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%dativarattr%d(varind), &
       this%ser%v7d%datiattr%d(attrind), genericptr)
    CASE(3)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%dativarattr%i(varind), &
       this%ser%v7d%datiattr%i(attrind), genericptr)
    CASE(4)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%dativarattr%b(varind), &
       this%ser%v7d%datiattr%b(attrind), genericptr)
    CASE(5)
      CALL this%line%vol7d_attr_callback(this%ser%v7d%dativarattr%c(varind), &
       this%ser%v7d%datiattr%c(attrind), genericptr)
    END SELECT
  END SELECT

END SELECT

END SUBROUTINE call_desc_callback


SUBROUTINE default_vol7d_ana_callback(ana, genericptr)
TYPE(vol7d_ana), INTENT(in) :: ana
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)

col = trim(ADJUSTL(to_char(getlon(ana%coord),miss="",form="(f10.5)")))//&
 ','//trim(ADJUSTL(to_char(getlat(ana%coord),miss="",form="(f10.5)")))

END SUBROUTINE default_vol7d_ana_callback


SUBROUTINE default_vol7d_time_callback(time, genericptr)
TYPE(datetime), INTENT(in) :: time
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = ''
IF (time /= datetime_miss) THEN
  CALL getval(time, isodate=col(1:19))
ENDIF

END SUBROUTINE default_vol7d_time_callback


SUBROUTINE default_vol7d_level_callback(level, genericptr)
TYPE(vol7d_level), INTENT(in) :: level
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = t2c(level%level1, '')//','// &
 t2c(level%l1, '')//','// &
 t2c(level%level2, '')//','// &
 t2c(level%l2, '')

END SUBROUTINE default_vol7d_level_callback


SUBROUTINE default_vol7d_timerange_callback(timerange, genericptr)
TYPE(vol7d_timerange), INTENT(in) :: timerange
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
col = t2c(timerange%timerange, '')//','// &
 t2c(timerange%p1, '')//','//t2c(timerange%p2, '')

END SUBROUTINE default_vol7d_timerange_callback


SUBROUTINE default_vol7d_network_callback(network, genericptr)
TYPE(vol7d_network), INTENT(in) :: network
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(network)) THEN
  col = network%name
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_network_callback


SUBROUTINE default_vol7d_var_callback(var, genericptr)
TYPE(vol7d_var), INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(var)) THEN
  col = var%btable
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_var_callback


SUBROUTINE default_vol7d_attr_callback(var, attr, genericptr)
TYPE(vol7d_var), INTENT(in) :: var
TYPE(vol7d_var), INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(var) .AND. c_e(attr)) THEN
  col = TRIM(var%btable)//'.'//attr%btable
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_attr_callback


! create a var_mapper object from the v7d volume provided
SUBROUTINE var_mapper(mapper, v7d)
TYPE(vol7d_var_mapper),ALLOCATABLE :: mapper(:)
TYPE(vol7d),INTENT(in) :: v7d

INTEGER :: n

n = 0

IF (ASSOCIATED(v7d%anavar%r)) n = n + SIZE(v7d%anavar%r)
IF (ASSOCIATED(v7d%anavar%d)) n = n + SIZE(v7d%anavar%d)
IF (ASSOCIATED(v7d%anavar%i)) n = n + SIZE(v7d%anavar%i)
IF (ASSOCIATED(v7d%anavar%b)) n = n + SIZE(v7d%anavar%b)
IF (ASSOCIATED(v7d%anavar%c)) n = n + SIZE(v7d%anavar%c)

IF (ASSOCIATED(v7d%anaattr%r) .AND. ASSOCIATED(v7d%anavarattr%r)) n = n + &
 SIZE(v7d%anaattr%r) * SIZE(v7d%anavarattr%r)
IF (ASSOCIATED(v7d%anaattr%d) .AND. ASSOCIATED(v7d%anavarattr%d)) n = n + &
 SIZE(v7d%anaattr%d) * SIZE(v7d%anavarattr%d)
IF (ASSOCIATED(v7d%anaattr%i) .AND. ASSOCIATED(v7d%anavarattr%i)) n = n + &
 SIZE(v7d%anaattr%i) * SIZE(v7d%anavarattr%i)
IF (ASSOCIATED(v7d%anaattr%b) .AND. ASSOCIATED(v7d%anavarattr%b)) n = n + &
 SIZE(v7d%anaattr%b) * SIZE(v7d%anavarattr%b)
IF (ASSOCIATED(v7d%anaattr%c) .AND. ASSOCIATED(v7d%anavarattr%c)) n = n + &
 SIZE(v7d%anaattr%c) * SIZE(v7d%anavarattr%c)

IF (ASSOCIATED(v7d%dativar%r)) n = n + SIZE(v7d%dativar%r)
IF (ASSOCIATED(v7d%dativar%d)) n = n + SIZE(v7d%dativar%d)
IF (ASSOCIATED(v7d%dativar%i)) n = n + SIZE(v7d%dativar%i)
IF (ASSOCIATED(v7d%dativar%b)) n = n + SIZE(v7d%dativar%b)
IF (ASSOCIATED(v7d%dativar%c)) n = n + SIZE(v7d%dativar%c)

IF (ASSOCIATED(v7d%datiattr%r) .AND. ASSOCIATED(v7d%dativarattr%r)) n = n + &
 SIZE(v7d%datiattr%r) * SIZE(v7d%dativarattr%r)
IF (ASSOCIATED(v7d%datiattr%d) .AND. ASSOCIATED(v7d%dativarattr%d)) n = n + &
 SIZE(v7d%datiattr%d) * SIZE(v7d%dativarattr%d)
IF (ASSOCIATED(v7d%datiattr%i) .AND. ASSOCIATED(v7d%dativarattr%i)) n = n + &
 SIZE(v7d%datiattr%i) * SIZE(v7d%dativarattr%i)
IF (ASSOCIATED(v7d%datiattr%b) .AND. ASSOCIATED(v7d%dativarattr%b)) n = n + &
 SIZE(v7d%datiattr%b) * SIZE(v7d%dativarattr%b)
IF (ASSOCIATED(v7d%datiattr%c) .AND. ASSOCIATED(v7d%dativarattr%c)) n = n + &
 SIZE(v7d%datiattr%c) * SIZE(v7d%dativarattr%c)

ALLOCATE(mapper(n))

n = 0

IF (ASSOCIATED(v7d%anavar%r)) THEN
  CALL set_mapper(1, 1, 1, SIZE(v7d%anavar%r))
ENDIF
IF (ASSOCIATED(v7d%anavar%d)) THEN
  CALL set_mapper(1, 2, 1, SIZE(v7d%anavar%d))
ENDIF
IF (ASSOCIATED(v7d%anavar%i)) THEN
  CALL set_mapper(1, 3, 1, SIZE(v7d%anavar%i))
ENDIF
IF (ASSOCIATED(v7d%anavar%b)) THEN
  CALL set_mapper(1, 4, 1, SIZE(v7d%anavar%b))
ENDIF
IF (ASSOCIATED(v7d%anavar%c)) THEN
  CALL set_mapper(1, 5, 1, SIZE(v7d%anavar%c))
ENDIF

IF (ASSOCIATED(v7d%anaattr%r) .AND. ASSOCIATED(v7d%anavarattr%r)) THEN
  CALL set_mapper(2, 1, SIZE(v7d%anaattr%r), SIZE(v7d%anavarattr%r))
ENDIF
IF (ASSOCIATED(v7d%anaattr%d) .AND. ASSOCIATED(v7d%anavarattr%d)) THEN
  CALL set_mapper(2, 2, SIZE(v7d%anaattr%d), SIZE(v7d%anavarattr%d))
ENDIF
IF (ASSOCIATED(v7d%anaattr%i) .AND. ASSOCIATED(v7d%anavarattr%i)) THEN
  CALL set_mapper(2, 3, SIZE(v7d%anaattr%i), SIZE(v7d%anavarattr%i))
ENDIF
IF (ASSOCIATED(v7d%anaattr%b) .AND. ASSOCIATED(v7d%anavarattr%b)) THEN
  CALL set_mapper(2, 4, SIZE(v7d%anaattr%b), SIZE(v7d%anavarattr%b))
ENDIF
IF (ASSOCIATED(v7d%anaattr%c) .AND. ASSOCIATED(v7d%anavarattr%c)) THEN
  CALL set_mapper(2, 5, SIZE(v7d%anaattr%c), SIZE(v7d%anavarattr%c))
ENDIF

IF (ASSOCIATED(v7d%dativar%r)) THEN
  CALL set_mapper(3, 1, 1, SIZE(v7d%dativar%r))
ENDIF
IF (ASSOCIATED(v7d%dativar%d)) THEN
  CALL set_mapper(3, 2, 1, SIZE(v7d%dativar%d))
ENDIF
IF (ASSOCIATED(v7d%dativar%i)) THEN
  CALL set_mapper(3, 3, 1, SIZE(v7d%dativar%i))
ENDIF
IF (ASSOCIATED(v7d%dativar%b)) THEN
  CALL set_mapper(3, 4, 1, SIZE(v7d%dativar%b))
ENDIF
IF (ASSOCIATED(v7d%dativar%c)) THEN
  CALL set_mapper(3, 5, 1, SIZE(v7d%dativar%c))
ENDIF

IF (ASSOCIATED(v7d%datiattr%r) .AND. ASSOCIATED(v7d%dativarattr%r)) THEN
  CALL set_mapper(4, 1, SIZE(v7d%datiattr%r), SIZE(v7d%dativarattr%r))
ENDIF
IF (ASSOCIATED(v7d%datiattr%d) .AND. ASSOCIATED(v7d%dativarattr%d)) THEN
  CALL set_mapper(4, 2, SIZE(v7d%datiattr%d), SIZE(v7d%dativarattr%d))
ENDIF
IF (ASSOCIATED(v7d%datiattr%i) .AND. ASSOCIATED(v7d%dativarattr%i)) THEN
  CALL set_mapper(4, 3, SIZE(v7d%datiattr%i), SIZE(v7d%dativarattr%i))
ENDIF
IF (ASSOCIATED(v7d%datiattr%b) .AND. ASSOCIATED(v7d%dativarattr%b)) THEN
  CALL set_mapper(4, 4, SIZE(v7d%datiattr%b), SIZE(v7d%dativarattr%b))
ENDIF
IF (ASSOCIATED(v7d%datiattr%c) .AND. ASSOCIATED(v7d%dativarattr%c)) THEN
  CALL set_mapper(4, 5, SIZE(v7d%datiattr%c), SIZE(v7d%dativarattr%c))
ENDIF

CONTAINS

SUBROUTINE set_mapper(cat, typ, s1, s2)
INTEGER,INTENT(in) :: cat
INTEGER,INTENT(in) :: typ
INTEGER,INTENT(in) :: s1, s2

INTEGER :: i, j, n1

n1 = n + s1*s2
mapper(n+1:n1)%cat = cat
mapper(n+1:n1)%typ = typ
mapper(n+1:n1)%i5 = (/((i,i=1,s2),j=1,s1)/)
mapper(n+1:n1)%i7 = (/((j,i=1,s2),j=1,s1)/)
n = n1

END SUBROUTINE set_mapper

END SUBROUTINE var_mapper


! determine whether the volume mapped by mapper has missing values for
! every variable at the indicated position
FUNCTION var_mapper_miss(mapper, v7d, i1, i2, i3, i4, i6, analine) RESULT(miss)
TYPE(vol7d_var_mapper),INTENT(in) :: mapper(:)
TYPE(vol7d),INTENT(in) :: v7d
INTEGER,INTENT(in) :: i1, i2, i3, i4, i6
LOGICAL,INTENT(in) :: analine
LOGICAL :: miss

INTEGER :: ind, varind, attrind

miss = .TRUE.
DO ind = 1, SIZE(mapper)
  varind = mapper(ind)%i5
  attrind = mapper(ind)%i7

  SELECT CASE(mapper(ind)%cat)
  CASE(1)
    IF (analine) THEN
      SELECT CASE(mapper(ind)%typ)
      CASE(1)
        miss = miss .AND. .NOT.c_e(v7d%volanar(i1, varind, i6))
      CASE(2)
        miss = miss .AND. .NOT.c_e(v7d%volanad(i1, varind, i6))
      CASE(3)
        miss = miss .AND. .NOT.c_e(v7d%volanai(i1, varind, i6))
      CASE(4)
        miss = miss .AND. .NOT.c_e(v7d%volanab(i1, varind, i6))
      CASE(5)
        miss = miss .AND. .NOT.c_e(v7d%volanac(i1, varind, i6))
      END SELECT
    ENDIF
  CASE(2)
    IF (analine) THEN
      SELECT CASE(mapper(ind)%typ)
      CASE(1)
        miss = miss .AND. .NOT.c_e(v7d%volanaattrr(i1, varind, i6, attrind))
      CASE(2)
        miss = miss .AND. .NOT.c_e(v7d%volanaattrd(i1, varind, i6, attrind))
      CASE(3)
        miss = miss .AND. .NOT.c_e(v7d%volanaattri(i1, varind, i6, attrind))
      CASE(4)
        miss = miss .AND. .NOT.c_e(v7d%volanaattrb(i1, varind, i6, attrind))
      CASE(5)
        miss = miss .AND. .NOT.c_e(v7d%volanaattrc(i1, varind, i6, attrind))
      END SELECT
    ENDIF
  CASE(3)
    SELECT CASE(mapper(ind)%typ)
    CASE(1)
      miss = miss .AND. .NOT.c_e(v7d%voldatir(i1, i2, i3, i4, varind, i6))
    CASE(2)
      miss = miss .AND. .NOT.c_e(v7d%voldatid(i1, i2, i3, i4, varind, i6))
    CASE(3)
      miss = miss .AND. .NOT.c_e(v7d%voldatii(i1, i2, i3, i4, varind, i6))
    CASE(4)
      miss = miss .AND. .NOT.c_e(v7d%voldatib(i1, i2, i3, i4, varind, i6))
    CASE(5)
      miss = miss .AND. .NOT.c_e(v7d%voldatic(i1, i2, i3, i4, varind, i6))
    END SELECT
  CASE(4)
    SELECT CASE(mapper(ind)%typ)
    CASE(1)
      miss = miss .AND. .NOT.c_e(v7d%voldatiattrr(i1, i2, i3, i4, varind, i6, attrind))
    CASE(2)
      miss = miss .AND. .NOT.c_e(v7d%voldatiattrd(i1, i2, i3, i4, varind, i6, attrind))
    CASE(3)
      miss = miss .AND. .NOT.c_e(v7d%voldatiattri(i1, i2, i3, i4, varind, i6, attrind))
    CASE(4)
      miss = miss .AND. .NOT.c_e(v7d%voldatiattrb(i1, i2, i3, i4, varind, i6, attrind))
    CASE(5)
      miss = miss .AND. .NOT.c_e(v7d%voldatiattrc(i1, i2, i3, i4, varind, i6, attrind))
    END SELECT
  END SELECT
  IF (.NOT.miss) RETURN ! shortcut
ENDDO

END FUNCTION var_mapper_miss


! search for a variable in the mapper object and return the
! corresponding index, or 0 if not found
FUNCTION var_mapper_searchvar(mapper, v7d, var) RESULT(ind)
TYPE(vol7d_var_mapper),INTENT(in) :: mapper(:)
TYPE(vol7d),INTENT(in) :: v7d
TYPE(vol7d_var),INTENT(in) :: var

INTEGER :: ind
INTEGER :: varind

DO ind = 1, SIZE(mapper)
  varind = mapper(ind)%i5
!  attrind = mapper(ind)%i7

  SELECT CASE(mapper(ind)%cat)
  CASE(1)
    SELECT CASE(mapper(ind)%typ)
    CASE(1)
      IF (v7d%anavar%r(varind) == var) RETURN
    CASE(2)
      IF (v7d%anavar%d(varind) == var) RETURN
    CASE(3)
      IF (v7d%anavar%i(varind) == var) RETURN
    CASE(4)
      IF (v7d%anavar%b(varind) == var) RETURN
    CASE(5)
      IF (v7d%anavar%c(varind) == var) RETURN
    END SELECT
  CASE(3)
    SELECT CASE(mapper(ind)%typ)
    CASE(1)
      IF (v7d%dativar%r(varind) == var) RETURN
    CASE(2)
      IF (v7d%dativar%d(varind) == var) RETURN
    CASE(3)
      IF (v7d%dativar%i(varind) == var) RETURN
    CASE(4)
      IF (v7d%dativar%b(varind) == var) RETURN
    CASE(5)
      IF (v7d%dativar%c(varind) == var) RETURN
    END SELECT
  END SELECT
END DO

ind = 0 ! not found

END FUNCTION var_mapper_searchvar


SUBROUTINE call_value_callback(this, genericptr)
CLASS(vol7d_serialize_itercol),INTENT(inout) :: this
TYPE(c_ptr),VALUE :: genericptr

INTEGER :: ind, varind, attrind

ind = this%line%i5
varind = this%ser%mapper(ind)%i5
attrind = this%ser%mapper(ind)%i7

SELECT CASE(this%ser%mapper(ind)%cat)
CASE(1)
  SELECT CASE(this%ser%mapper(ind)%typ)
  CASE(1)
    CALL this%line%vol7d_valuer_var_callback(this%ser%v7d%volanar( &
     this%line%i1, varind, this%line%i6), &
     this%ser%v7d%anavar%r(varind), genericptr)
  CASE(2)
    CALL this%line%vol7d_valued_var_callback(this%ser%v7d%volanad( &
     this%line%i1, varind, this%line%i6), &
     this%ser%v7d%anavar%d(varind), genericptr)
  CASE(3)
    CALL this%line%vol7d_valuei_var_callback(this%ser%v7d%volanai( &
     this%line%i1, varind, this%line%i6), &
     this%ser%v7d%anavar%i(varind), genericptr)
  CASE(4)
    CALL this%line%vol7d_valueb_var_callback(this%ser%v7d%volanab( &
     this%line%i1, varind, this%line%i6), &
     this%ser%v7d%anavar%b(varind), genericptr)
  CASE(5)
    CALL this%line%vol7d_valuec_var_callback(this%ser%v7d%volanac( &
     this%line%i1, varind, this%line%i6), &
     this%ser%v7d%anavar%c(varind), genericptr)
  END SELECT
CASE(2)
  SELECT CASE(this%ser%mapper(ind)%typ)
  CASE(1)
    CALL this%line%vol7d_valuer_attr_callback(this%ser%v7d%volanaattrr( &
     this%line%i1, varind, this%line%i6, attrind), &
     this%ser%v7d%anavarattr%r(varind), this%ser%v7d%anaattr%r(attrind), genericptr)
  CASE(2)
    CALL this%line%vol7d_valued_attr_callback(this%ser%v7d%volanaattrd( &
     this%line%i1, varind, this%line%i6, attrind), &
     this%ser%v7d%anavarattr%d(varind), this%ser%v7d%anaattr%d(attrind), genericptr)
  CASE(3)
    CALL this%line%vol7d_valuei_attr_callback(this%ser%v7d%volanaattri( &
     this%line%i1, varind, this%line%i6, attrind), &
     this%ser%v7d%anavarattr%i(varind), this%ser%v7d%anaattr%i(attrind), genericptr)
  CASE(4)
    CALL this%line%vol7d_valueb_attr_callback(this%ser%v7d%volanaattrb( &
     this%line%i1, varind, this%line%i6, attrind), &
     this%ser%v7d%anavarattr%b(varind), this%ser%v7d%anaattr%b(attrind), genericptr)
  CASE(5)
    CALL this%line%vol7d_valuec_attr_callback(this%ser%v7d%volanaattrc( &
     this%line%i1, varind, this%line%i6, attrind), &
     this%ser%v7d%anavarattr%c(varind), this%ser%v7d%anaattr%c(attrind), genericptr)
  END SELECT
CASE(3)
  SELECT CASE(this%ser%mapper(ind)%typ)
  CASE(1)
    CALL this%line%vol7d_valuer_var_callback(this%ser%v7d%voldatir( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6), &
     this%ser%v7d%dativar%r(varind), genericptr)
  CASE(2)
    CALL this%line%vol7d_valued_var_callback(this%ser%v7d%voldatid( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6), &
     this%ser%v7d%dativar%d(varind), genericptr)
  CASE(3)
    CALL this%line%vol7d_valuei_var_callback(this%ser%v7d%voldatii( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6), &
     this%ser%v7d%dativar%i(varind), genericptr)
  CASE(4)
    CALL this%line%vol7d_valueb_var_callback(this%ser%v7d%voldatib( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6), &
     this%ser%v7d%dativar%b(varind), genericptr)
  CASE(5)
    CALL this%line%vol7d_valuec_var_callback(this%ser%v7d%voldatic( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6), &
     this%ser%v7d%dativar%c(varind), genericptr)
  END SELECT
CASE(4)
  SELECT CASE(this%ser%mapper(ind)%typ)
  CASE(1)
    CALL this%line%vol7d_valuer_attr_callback(this%ser%v7d%voldatiattrr( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6, attrind), &
     this%ser%v7d%dativarattr%r(varind), this%ser%v7d%datiattr%r(attrind), genericptr)
  CASE(2)
    CALL this%line%vol7d_valued_attr_callback(this%ser%v7d%voldatiattrd( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6, attrind), &
     this%ser%v7d%dativarattr%d(varind), this%ser%v7d%datiattr%d(attrind), genericptr)
  CASE(3)
    CALL this%line%vol7d_valuei_attr_callback(this%ser%v7d%voldatiattri( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6, attrind), &
     this%ser%v7d%dativarattr%i(varind), this%ser%v7d%datiattr%i(attrind), genericptr)
  CASE(4)
    CALL this%line%vol7d_valueb_attr_callback(this%ser%v7d%voldatiattrb( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6, attrind), &
     this%ser%v7d%dativarattr%b(varind), this%ser%v7d%datiattr%b(attrind), genericptr)
  CASE(5)
    CALL this%line%vol7d_valuec_attr_callback(this%ser%v7d%voldatiattrc( &
     this%line%i1, this%line%i2, this%line%i3, this%line%i4, varind, this%line%i6, attrind), &
     this%ser%v7d%dativarattr%c(varind), this%ser%v7d%datiattr%c(attrind), genericptr)
  END SELECT
END SELECT

END SUBROUTINE call_value_callback


SUBROUTINE default_vol7d_valuer_var_callback(valu, var, genericptr)
REAL,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(valu)) THEN
  col = t2c(valu)
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_valuer_var_callback


SUBROUTINE default_vol7d_valued_var_callback(valu, var, genericptr)
DOUBLE PRECISION,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(valu)) THEN
  col = t2c(valu)
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_valued_var_callback


SUBROUTINE default_vol7d_valuei_var_callback(valu, var, genericptr)
INTEGER,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(valu)) THEN
  IF (c_e(var%scalefactor) .AND. &
   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
    col = t2c(realdat(valu, var))
  ELSE
    col = t2c(valu)
  ENDIF
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_valuei_var_callback


SUBROUTINE default_vol7d_valueb_var_callback(valu, var, genericptr)
INTEGER(kind=int_b),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

IF (c_e(valu)) THEN
  CALL default_vol7d_valuei_var_callback(INT(valu), var, genericptr)
ELSE
  CALL C_F_POINTER(genericptr, col)
  col = ''
ENDIF

END SUBROUTINE default_vol7d_valueb_var_callback


SUBROUTINE default_vol7d_valuec_var_callback(valu, var, genericptr)
CHARACTER(len=*),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(valu)) THEN
  IF (c_e(var%scalefactor) .AND. var%unit /= 'CCITTIA5' .AND. &
   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
    col = t2c(realdat(valu, var))
  ELSE
    col = TRIM(valu)
  ENDIF
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_valuec_var_callback


SUBROUTINE default_vol7d_valuer_attr_callback(valu, var, attr, genericptr)
REAL,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CALL default_vol7d_valuer_var_callback(valu, attr, genericptr)

END SUBROUTINE default_vol7d_valuer_attr_callback


SUBROUTINE default_vol7d_valued_attr_callback(valu, var, attr, genericptr)
DOUBLE PRECISION,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CALL default_vol7d_valued_var_callback(valu, attr, genericptr)

END SUBROUTINE default_vol7d_valued_attr_callback


SUBROUTINE default_vol7d_valuei_attr_callback(valu, var, attr, genericptr)
INTEGER,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(valu)) THEN
  IF (c_e(attr%scalefactor) .AND. &
   .NOT.(attr%scalefactor == 0 .AND. attr%unit == 'NUMERIC')) THEN
    col = t2c(realdat(valu, attr))
  ELSE
    col = t2c(valu)
  ENDIF
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_valuei_attr_callback


SUBROUTINE default_vol7d_valueb_attr_callback(valu, var, attr, genericptr)
INTEGER(kind=int_b),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

IF (c_e(valu)) THEN
  CALL default_vol7d_valuei_var_callback(INT(valu), attr, genericptr)
ELSE
  CALL C_F_POINTER(genericptr, col)
  col = ''
ENDIF

END SUBROUTINE default_vol7d_valueb_attr_callback


SUBROUTINE default_vol7d_valuec_attr_callback(valu, var, attr, genericptr)
CHARACTER(len=*),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

CHARACTER(len=64),POINTER :: col

CALL C_F_POINTER(genericptr, col)
IF (c_e(valu)) THEN
  IF (c_e(attr%scalefactor) .AND. attr%unit /= 'CCITTIA5' .AND. &
   .NOT.(attr%scalefactor == 0 .AND. attr%unit == 'NUMERIC')) THEN
    col = t2c(realdat(valu, attr))
  ELSE
    col = TRIM(valu)
  ENDIF
ELSE
  col = ''
ENDIF

END SUBROUTINE default_vol7d_valuec_attr_callback


END MODULE vol7d_serialize_class
