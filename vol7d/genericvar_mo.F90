MODULE volvar_mo
USE kinds
USE missing_values
USE file_utilities
IMPLICIT NONE
PRIVATE

TYPE volvar_t
  PRIVATE
  INTEGER(kind=int_b),PUBLIC :: var(6)=(/-1_int_b,-1_int_b,-1_int_b,-1_int_b,-1_int_b,-1_int_b/)
  LOGICAL,PUBLIC :: attr=.FALSE.
  CHARACTER(len=10),PUBLIC :: btable=cmiss
  CHARACTER(len=65),PUBLIC :: description=cmiss !< descrizione testuale della variabile (opzionale)
  CHARACTER(len=24),PUBLIC :: unit=cmiss !< descrizione testuale dell'unità di misura (opzionale)
  INTEGER,PUBLIC :: scalefactor=imiss !< numero di decimali nella rappresentazione intera o character (opzionale)

  INTEGER,PUBLIC :: r=imiss !< indice della variabile nel volume degli attributi reali
  INTEGER,PUBLIC :: d=imiss !< indice della variabile nel volume degli attributi double precision
  INTEGER,PUBLIC :: i=imiss !< indice della variabile nel volume degli attributi integer
  INTEGER,PUBLIC :: b=imiss !< indice della variabile nel volume degli attributi byte
  INTEGER,PUBLIC :: c=imiss !< indice della variabile nel volume degli attributi character
  INTEGER,PUBLIC :: feat_ind=imiss !< cached index to corresponding entry in feature table
  CONTAINS
!  PROCEDURE,NOPASS,PRIVATE :: volvar_new_b
!  PROCEDURE,NOPASS,PRIVATE :: volvar_new_g
!  GENERIC,PUBLIC :: volvar_new => volvar_new_b, volvar_new_g !, volvar_new_native
  PROCEDURE,PRIVATE :: volvar_eq
  PROCEDURE,PRIVATE :: volvar_ne
  PROCEDURE,PRIVATE :: volvar_c_e
  GENERIC,PUBLIC :: operator(==) => volvar_eq
  GENERIC,PUBLIC :: operator(/=) => volvar_ne
  GENERIC,PUBLIC :: c_e => volvar_c_e
!  PROCEDURE,PUBLIC :: volvar_vartype
END TYPE volvar_t

TYPE(volvar_t),PARAMETER :: volgrid6d_var_miss=volvar_t() !(-1, -1, -1)
TYPE(volvar_t),PARAMETER :: vol7d_var_miss=volvar_t() !new(cmiss)

INTERFACE volvar_new
  MODULE PROCEDURE volvar_new_b, volvar_new_g
END INTERFACE volvar_new

TYPE volvar_features_t
  TYPE(volvar_t) :: var !< the variable (only btable is relevant)
  REAL :: posdef !< if not missing, minimum physically reasonable value for the variable
  INTEGER :: vartype !< type of variable, one of the var_* constants
END TYPE volvar_features_t

TYPE(volvar_features_t),ALLOCATABLE :: var_features(:)

! constants for vartype
INTEGER,PARAMETER :: var_ord=0 !< unclassified variable (vol7d_vartype function)
INTEGER,PARAMETER :: var_dir360=1 !< direction in degrees (vol7d_vartype function)
INTEGER,PARAMETER :: var_press=2 !< pressure in Pa (vol7d_vartype function)
INTEGER,PARAMETER :: var_ucomp=3 !< u component of a vector field (vol7d_vartype function)
INTEGER,PARAMETER :: var_vcomp=4 !< v component of a vector field (vol7d_vartype function)
INTEGER,PARAMETER :: var_wcomp=5 !< w component of a vector field (vol7d_vartype function)

!> Class defining a real conversion function between units. It is
!! used to numerically convert a value expressed as a \a volgrid6d_var
!! variable in a value expressed as a \a vol7d_var variable and
!! vice-versa. At the moment only a linear conversion is
!! supported. Objects of this class are returned only by the \a
!! vargrib2varbufr \a varbufr2vargrib, and \a convert methods and are
!! used in the \a convert and \a compute methods defined in this
!! MODULE.
TYPE conv_func_t
  PRIVATE
  REAL :: a, b
  CONTAINS
  PROCEDURE,PRIVATE :: conv_func_eq
  PROCEDURE,PRIVATE :: conv_func_ne
  PROCEDURE,PRIVATE :: conv_func_mult
  PROCEDURE,PRIVATE :: conv_func_compute
  GENERIC,PUBLIC :: operator(==) => conv_func_eq
  GENERIC,PUBLIC :: operator(/=) => conv_func_ne
  GENERIC,PUBLIC :: operator(*) => conv_func_mult
  GENERIC,PUBLIC :: compute => conv_func_compute
END TYPE conv_func_t

TYPE(conv_func_t), PARAMETER :: conv_func_miss=conv_func_t(rmiss,rmiss)
TYPE(conv_func_t), PARAMETER :: conv_func_identity=conv_func_t(1.0,0.0)

TYPE volvar_conv_t
  TYPE(volvar_t) :: vg6d_var
  TYPE(volvar_t) :: v7d_var
  TYPE(conv_func_t) :: c_func
! aggiungere informazioni ad es. su rotazione del vento
END TYPE volvar_conv_t

TYPE(volvar_conv_t), PARAMETER :: volvar_conv_miss= &
 volvar_conv_t(volgrid6d_var_miss, vol7d_var_miss, conv_func_miss)

TYPE(volvar_conv_t), ALLOCATABLE :: conv_fwd(:), conv_bwd(:)

PUBLIC :: volvar_t, volvar_new, volvar_conv_t, conv_func_t

CONTAINS

FUNCTION volvar_eq(this, that) RESULT(eq)
CLASS(volvar_t),INTENT(in) :: this
CLASS(volvar_t),INTENT(in) :: that
LOGICAL :: eq

IF (c_e(this%btable) .AND. c_e(that%btable)) THEN
  eq = this%btable == that%btable
ELSE
  eq = ALL(this%var == that%var) .AND. (this%attr .EQV. that%attr)
ENDIF

END FUNCTION volvar_eq

FUNCTION volvar_ne(this, that) RESULT(ne)
CLASS(volvar_t),INTENT(in) :: this
CLASS(volvar_t),INTENT(in) :: that
LOGICAL :: ne

ne = .NOT.volvar_eq(this, that)

END FUNCTION volvar_ne

FUNCTION volvar_c_e(this) RESULT(c_e_v)
CLASS(volvar_t),INTENT(in) :: this
LOGICAL :: c_e_v

c_e_v = ANY(this%var /= -1_int_b) .OR. c_e(this%btable)

END FUNCTION volvar_c_e

FUNCTION volvar_new_b(bcode) RESULT(volvar)
CHARACTER(len=10),INTENT(in) :: bcode
TYPE(volvar_t) :: volvar

INTEGER :: cat, num

volvar%btable = bcode

IF (bcode(1:1) == 'B') THEN
  READ(bcode,'(1X,I2,I3)', ERR=10) cat, num
  volvar%var = (/addsign(cat), addsign(num), -1_int_b, -1_int_b, -1_int_b, -1_int_b/)
  RETURN
ENDIF

IF (bcode(1:2) == '*B') THEN
  READ(bcode,'(2X,I2,I3)', ERR=10) cat, num
  volvar%var = (/addsign(cat), addsign(num), -1_int_b, -1_int_b, -1_int_b, -1_int_b/)
  volvar%attr = .TRUE.
  RETURN
ENDIF

10 CONTINUE

END FUNCTION volvar_new_b


FUNCTION volvar_new_g(ctr, cat, num, dis) RESULT(volvar)
INTEGER,INTENT(in) :: ctr
INTEGER,INTENT(in) :: cat
INTEGER,INTENT(in) :: num
INTEGER,INTENT(in),OPTIONAL :: dis
TYPE(volvar_t) :: volvar


IF (PRESENT(dis)) THEN ! grib2
  volvar%var = (/-1_int_b, -1_int_b, addsign(ctr), addsign(cat), addsign(num), addsign(dis)/)
  RETURN
ELSE ! grib1
  volvar%var = (/-1_int_b, -1_int_b, addsign(ctr), addsign(cat), addsign(num), -1_int_b/)
  RETURN
ENDIF

END FUNCTION volvar_new_g


FUNCTION addsign(uns)
INTEGER,INTENT(in) :: uns
INTEGER(kind=int_b) :: addsign

IF (uns >= 0 .AND. uns < 128) THEN
  addsign = INT(uns, int_b)
ELSE IF (uns >= 128 .AND. uns < 256) THEN
  addsign = INT(uns - 256, int_b)
ELSE
  addsign = -1_int_b
ENDIF

END FUNCTION addsign


SUBROUTINE volvar_features_set(this)
TYPE(volvar_t),INTENT(inout) :: this
INTEGER :: i

CALL volvar_features_init()

IF (ALLOCATED(var_features)) THEN
  DO i = 1, SIZE(var_features)
    IF (this == var_features(i)%var) THEN
      this%feat_ind = i
      EXIT
    ENDIF
  ENDDO
ENDIF

END SUBROUTINE volvar_features_set


!> Initialise the global table of variable features.
!! This subroutine reads the table of variable features from an
!! external file and stores it in a global array. It has to be called
!! once at the beginning of the program. At the moment it gives access
!! to the information about type of variable and positive
!! definitness. The table is based on the unique bufr-like variable
!! table. The table is contained in the csv file `vargrib.csv`.
!! It is not harmful to call this subroutine multiple times.
SUBROUTINE volvar_features_init()
INTEGER :: un, i, n
TYPE(csv_record) :: csv
CHARACTER(len=1024) :: line

IF (ALLOCATED(var_features)) RETURN

un = open_package_file('varbufr.csv', filetype_data)
n=0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  n = n + 1
ENDDO

100 CONTINUE

REWIND(un)
ALLOCATE(var_features(n))

DO i = 1, n
  READ(un,'(A)',END=200)line
  CALL init(csv, line)
  CALL csv_record_getfield(csv, var_features(i)%var%btable)
  CALL csv_record_getfield(csv)
  CALL csv_record_getfield(csv)
  CALL csv_record_getfield(csv, var_features(i)%posdef)
  CALL csv_record_getfield(csv, var_features(i)%vartype)
  CALL delete(csv)
ENDDO

200 CONTINUE
CLOSE(un)

END SUBROUTINE volvar_features_init


!> Deallocate the global table of variable features.
!! This subroutine deallocates the table of variable features
!! allocated in the `volvar_features_init` subroutine.
SUBROUTINE volvar_features_delete()
IF (ALLOCATED(var_features)) DEALLOCATE(var_features)
END SUBROUTINE volvar_features_delete


!> Return the physical type of the variable.
!! Returns a rough classification of the variable depending on the
!! physical parameter it represents. The result is one of the
!! constants vartype_* defined in the module. To be extended.
!! In order for this to work, the subroutine \a
!! volvar_features_set has to be preliminary called.
ELEMENTAL FUNCTION volvar_features_vartype(this) RESULT(vartype)
TYPE(volvar_t),INTENT(in) :: this !< volvar object to be tested
INTEGER :: vartype

IF (c_e(this%feat_ind)) THEN
  vartype = var_features(this%feat_ind)%vartype
ELSE
  vartype = imiss
ENDIF

END FUNCTION volvar_features_vartype


!> Apply a positive definite flag to a variable.
!! This subroutine resets the value of a variable depending on its
!! positive definite flag defined in the associated \a c_func object.
!! The \a c_func object can be obtained for example by the \a convert
!! (interfaced to vargrib2varbufr_convert) function. The value is
!! reset to the maximum between the value itself and and 0 (or the
!! value set in \a c_func%posdef. These values are set from the
!! varbufr.csv file.
!! In order for this to work, the subroutine \a
!! volvar_features_set has to be preliminary called.
ELEMENTAL SUBROUTINE volvar_features_posdef_apply(this, val)
TYPE(volvar_t),INTENT(in) :: this !< volvar object to be reset
REAL,INTENT(inout) :: val !< value to be reset, it is reset in place

IF (c_e(this%feat_ind)) THEN
  IF (c_e(var_features(this%feat_ind)%posdef)) THEN
    IF (c_e(val)) THEN
      val = MAX(var_features(this%feat_ind)%posdef, val)
    ENDIF
  ENDIF
ENDIF

END SUBROUTINE volvar_features_posdef_apply


! Private subroutine for reading forward and backward conversion tables
! todo: better error handling
SUBROUTINE volvar_conv_setup()
INTEGER :: un, i, n

! forward, grib to bufr
un = open_package_file('vargrib2bufr.csv', filetype_data)
n=0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  n = n + 1
ENDDO

100 CONTINUE

REWIND(un)
ALLOCATE(conv_fwd(n))

conv_fwd(:) = volvar_conv_miss
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
ALLOCATE(conv_bwd(n))

conv_bwd(:) = volvar_conv_miss
CALL import_var_conv(un, conv_bwd)
DO i = 1, n
  conv_bwd(i)%c_func%a = 1./conv_bwd(i)%c_func%a
  conv_bwd(i)%c_func%b = - conv_bwd(i)%c_func%b
ENDDO
CLOSE(un)

CONTAINS

SUBROUTINE import_var_conv(un, conv_type)
INTEGER, INTENT(in) :: un
TYPE(volvar_conv_t), INTENT(out) :: conv_type(:)

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
  conv_type(i)%v7d_var = volvar_new(btable)

  CALL csv_record_getfield(csv, centre)
  CALL csv_record_getfield(csv, category)
  CALL csv_record_getfield(csv, number)
  CALL csv_record_getfield(csv, discipline)
  conv_type(i)%vg6d_var = volvar_new(centre, category, number, discipline)

  CALL csv_record_getfield(csv, conv_type(i)%c_func%a)
  CALL csv_record_getfield(csv, conv_type(i)%c_func%b)
  CALL delete(csv)
ENDDO

200 CONTINUE

END SUBROUTINE import_var_conv

END SUBROUTINE volvar_conv_setup


ELEMENTAL FUNCTION conv_func_eq(this, that) RESULT(res)
CLASS(conv_func_t),INTENT(IN) :: this, that
LOGICAL :: res

res = this%a == that%a .AND. this%b == that%b

END FUNCTION conv_func_eq


ELEMENTAL FUNCTION conv_func_ne(this, that) RESULT(res)
CLASS(conv_func_t),INTENT(IN) :: this, that
LOGICAL :: res

res = .NOT.(this == that)

END FUNCTION conv_func_ne


FUNCTION conv_func_mult(this, that) RESULT(mult)
CLASS(conv_func_t),INTENT(in) :: this
CLASS(conv_func_t),INTENT(in) :: that

TYPE(conv_func_t) :: mult

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
CLASS(conv_func_t),INTENT(in) :: this !< object defining the conversion function
REAL,INTENT(inout) :: values !< value to be converted in place

IF (this /= conv_func_miss) THEN
  IF (c_e(values)) values = values*this%a + this%b
ELSE
  values=rmiss
ENDIF

END SUBROUTINE conv_func_compute


END MODULE volvar_mo
