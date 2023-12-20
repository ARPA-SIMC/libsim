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
!> Extension of vol7d_class with methods for performing simple
!! statistical operations on entire volumes of data. This module
!! includes methods for performing operations such as (re)averaging
!! and (re)cumulation in time of entire vol7d_class::vol7d objects
!! containing real or double precision data.
!!
!! \ingroup vol7d
MODULE vol7d_class_compute
USE datetime_class
USE vol7d_class
USE stat_proc_engine
USE simple_stat
IMPLICIT NONE

CONTAINS


!> General-purpose method for computing a statistical processing on
!! data in a vol7d object already processed with the same statistical
!! processing, on a different time interval specified by \a step and
!! \a start.  This method tries to apply all the suitable specialized
!! statistical processing methods according to the input and output
!! statistical processing requested.  The argument \a stat_proc_input
!! determines which data will be processed, while the \a stat_proc
!! argument determines the type of statistical process to be applied
!! and which will be owned by output data.
!!
!! The possible combinations are:
!!
!!  - \a stat_proc_input = 254
!!    - \a stat_proc = 0 average instantaneous observations
!!    - \a stat_proc = 2 compute maximum of instantaneous observations
!!    - \a stat_proc = 3 compute minimum of instantaneous observations
!!    - \a stat_proc = 4 compute difference of instantaneous observations
!!    - \a stat_proc = 6 compute standard deviation of instantaneous
!!      observations
!!    - \a stat_proc = 201 compute the prevailing direction (mode) on
!!      specified sectors, suitable only for variables representing an
!!      angle in degrees, e.g. wind direction
!!
!!  processing is computed on longer intervals by aggregation, see the
!!  description of vol7d_compute_stat_proc_agg()
!!
!!  - \a stat_proc_input = *
!!    - \a stat_proc = 254 consider statistically processed values as
!!      instantaneous without any extra processing
!!
!!  see the description of vol7d_decompute_stat_proc()
!!
!!  - \a stat_proc_input = 0, 1, 2, 3, 4, 200
!!    - \a stat_proc = \a stat_proc_input recompute input data on
!!      different intervals
!!
!!    the same statistical processing is applied to obtain data
!!    processed on a different interval, either longer, by
!!    aggregation, or shorter, by differences, see the description of
!!    vol7d_recompute_stat_proc_agg() and
!!    vol7d_recompute_stat_proc_diff() respectively; it is also
!!    possible to provide \a stat_proc_input \a /= \a stat_proc, but
!!    it has to be used with care.
!!
!!  - \a stat_proc_input = 0
!!    - \a stat_proc = 1
!!
!!    a time-averaged rate or flux is transformed into a
!!    time-integrated value (sometimes called accumulated) on the same
!!    interval by multiplying the values by the length of the time
!!    interval in seconds, keeping constant all the rest, including
!!    the variable; the unit of the variable implicitly changes
!!    accordingly, this is supported officially in grib2 standard, in
!!    the other cases it is a forcing of the standards.
!!
!!  - \a stat_proc_input = 1
!!    - \a stat_proc = 0
!!
!!    a time-integrated value (sometimes called accumulated) is
!!    transformed into a time-averaged rate or flux on the same
!!    interval by dividing the values by the length of the time
!!    interval in seconds, see also the previous description of the
!!    opposite computation.
!!
!! If a particular statistical processing cannot be performed on the
!! input data, the program continues with a warning and, if requested,
!! the input data is passed over to the volume specified by the \a
!! other argument, in order to allow continuation of processing.  All
!! the other parameters are passed over to the specifical statistical
!! processing methods and are documented there.
SUBROUTINE vol7d_compute_stat_proc(this, that, stat_proc_input, stat_proc, &
 step, start, full_steps, frac_valid, max_step, weighted, other)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc_input !< type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be processed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), data in output volume \a that will have a timerange of this type
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if \a .TRUE. apply processing only on intervals starting at a forecast time or a reference time modulo \a step
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
TYPE(timedelta),INTENT(in),OPTIONAL :: max_step !< maximum allowed distance in time between two contiguougs valid data within an interval, for the interval to be eligible for statistical processing
LOGICAL,INTENT(in),OPTIONAL :: weighted !< if provided and \c .TRUE., the statistical process is computed, if possible, by weighting every value with a weight proportional to its validity interval
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the accumulation computation

TYPE(vol7d) :: that1, that2, other1
INTEGER :: steps

IF (stat_proc_input == 254) THEN
  CALL l4f_log(L4F_INFO, 'computing statistical processing by aggregation '//&
   TRIM(to_char(stat_proc_input))//':'//TRIM(to_char(stat_proc)))

  CALL vol7d_compute_stat_proc_agg(this, that, stat_proc, &
   step, start, full_steps, max_step, weighted, other)

ELSE IF (stat_proc == 254) THEN
  CALL l4f_log(L4F_INFO, &
   'computing instantaneous data from statistically processed '//&
   TRIM(to_char(stat_proc_input))//':'//TRIM(to_char(stat_proc)))

! compute length of cumulation step in seconds
  CALL getval(step, asec=steps)

  IF (ANY(this%timerange(:)%p2 == steps)) THEN ! data is ready
    CALL vol7d_decompute_stat_proc(this, that, step, other, stat_proc_input)
  ELSE
    IF (ANY(this%timerange(:)%p2 == steps/2)) THEN ! need to average
! average twice on step interval, with a shift of step/2, check full_steps
      CALL vol7d_recompute_stat_proc_agg(this, that1, stat_proc_input, &
     step, full_steps=.FALSE., frac_valid=1.0)
      CALL vol7d_recompute_stat_proc_agg(this, that2, stat_proc_input, &
     step, start=that1%time(1)+step/2, full_steps=.FALSE., frac_valid=1.0)
! merge the result
      CALL vol7d_append(that1, that2, sort=.TRUE., lanasimple=.TRUE.)
! and process it
      CALL vol7d_decompute_stat_proc(that1, that, step, other, stat_proc_input)
      CALL delete(that1)
      CALL delete(that2)
    ELSE
! do nothing
    ENDIF
  ENDIF

ELSE IF (stat_proc_input == stat_proc .OR. &
 (stat_proc == 0 .OR. stat_proc == 2 .OR. stat_proc == 3)) THEN
! avg, min and max can be computed from any input, with care
  CALL l4f_log(L4F_INFO, &
   'recomputing statistically processed data by aggregation and difference '//&
   TRIM(to_char(stat_proc_input))//':'//TRIM(to_char(stat_proc)))

  IF (PRESENT(other)) THEN
    CALL vol7d_recompute_stat_proc_agg(this, that1, stat_proc, &
     step, start, full_steps, frac_valid, &
     other=other, stat_proc_input=stat_proc_input)
    CALL vol7d_recompute_stat_proc_diff(this, that2, stat_proc, &
     step, full_steps, start, other=other1)
    CALL vol7d_merge(other, other1, sort=.TRUE.)
  ELSE
    CALL vol7d_recompute_stat_proc_agg(this, that1, stat_proc, &
     step, start, full_steps, frac_valid, stat_proc_input=stat_proc_input)
    CALL vol7d_recompute_stat_proc_diff(this, that2, stat_proc, step, full_steps, &
     start)
  ENDIF

  CALL vol7d_merge(that1, that2, sort=.TRUE.)
  CALL delete(that2)
  that = that1
ELSE ! IF (stat_proc_input /= stat_proc) THEN
  IF ((stat_proc_input == 0 .AND. stat_proc == 1) .OR. &
   (stat_proc_input == 1 .AND. stat_proc == 0)) THEN
    CALL l4f_log(L4F_INFO, &
     'computing statistically processed data by integration/differentiation '// &
     t2c(stat_proc_input)//':'//t2c(stat_proc))
    CALL vol7d_compute_stat_proc_metamorph(this, that, stat_proc_input, &
     stat_proc)
  ELSE
    CALL l4f_log(L4F_ERROR, &
   'statistical processing '//t2c(stat_proc_input)//':'//t2c(stat_proc)// &
   ' not implemented or does not make sense')
    CALL raise_error()
  ENDIF
ENDIF

END SUBROUTINE vol7d_compute_stat_proc


!> Specialized method for statistically processing a set of data
!! already processed with the same statistical processing, on a
!! different time interval.  This method performs statistical
!! processing by aggregation of shorter intervals.  Only floating
!! point single or double precision data  are processed.
!!
!! The output \a that vol7d object contains elements from the original volume
!! \a this satisfying the conditions
!!  - real single or double precision variables
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type \a stat_proc (or \a stat_proc_input if provided)
!!  - any p1 (analysis/observation or forecast)
!!  - p2 > 0 (processing interval non null, non instantaneous data)
!!    and equal to a multiplier of \a step
!!
!! Output data will have timerange of type \a stat_proc and p2 = \a
!! step.  The supported statistical processing methods (parameter \a
!! stat_proc) are:
!!
!!  - 0 average
!!  - 1 accumulation
!!  - 2 maximum
!!  - 3 minimum
!!  - 4 difference
!!  - 6 standard deviation
!!  - 200 vectorial mean
!!
!! The start of processing period can be computed automatically from
!! the input intervals as the first possible interval modulo \a step,
!! or, for a better control, it can be specified explicitly by the
!! optional argument \a start.  Notice that \a start indicates the
!! beginning of the processing interval, so in the final volume, the
!! first datum may have time equal to \a start \a + \a step, e.g. in
!! the case when time is the verification time, which is typical for
!! observed datasets.
!!
!! The purpose of the optional argument \a stat_proc_input is to allow
!! processing with a certain statistical processing operator a dataset
!! already processed with a different operator, by specifying the
!! latter as stat_proc_input; this is useful, for example, if one
!! wants to compute the monthly average of daily maximum temperatures;
!! however this has to be used with care since the resulting data
!! volume will not carry all the information about the processing
!! which has been done, in the previous case, for example, the
!! temperatures will simply look like monthly average temperatures.
SUBROUTINE vol7d_recompute_stat_proc_agg(this, that, stat_proc, &
 step, start, full_steps, frac_valid, other, stat_proc_input)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if \a .TRUE. and \a start is not provided, apply processing only on intervals starting at a forecast time or a reference time modulo \a step
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the statistical processing
INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< to be used with care, type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be recomputed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument

INTEGER :: tri
INTEGER :: i, j, n, n1, ndtr, i1, i3, i5, i6
INTEGER :: linshape(1)
REAL :: lfrac_valid, frac_c, frac_m
LOGICAL,ALLOCATABLE :: ttr_mask(:,:)
TYPE(arrayof_ttr_mapper),POINTER :: map_ttr(:,:)
INTEGER,POINTER :: dtratio(:)


IF (PRESENT(stat_proc_input)) THEN
  tri = stat_proc_input
ELSE
  tri = stat_proc
ENDIF
IF (PRESENT(frac_valid)) THEN
  lfrac_valid = frac_valid
ELSE
  lfrac_valid = 1.0
ENDIF

! be safe
CALL vol7d_alloc_vol(this)
! initial check

! cleanup the original volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.) ! time-ordered volume needed
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)

CALL init(that, time_definition=this%time_definition)
CALL vol7d_alloc(that, nana=SIZE(this%ana), nlevel=SIZE(this%level), &
 nnetwork=SIZE(this%network))
IF (ASSOCIATED(this%dativar%r)) THEN
  CALL vol7d_alloc(that, ndativarr=SIZE(this%dativar%r))
  that%dativar%r = this%dativar%r
ENDIF
IF (ASSOCIATED(this%dativar%d)) THEN
  CALL vol7d_alloc(that, ndativard=SIZE(this%dativar%d))
  that%dativar%d = this%dativar%d
ENDIF
that%ana = this%ana
that%level = this%level
that%network = this%network

! compute the output time and timerange and all the required mappings
CALL recompute_stat_proc_agg_common(this%time, this%timerange, stat_proc, tri, &
 step, this%time_definition, that%time, that%timerange, map_ttr, dtratio, &
 start, full_steps)
CALL vol7d_alloc_vol(that)

ALLOCATE(ttr_mask(SIZE(this%time), SIZE(this%timerange)))
linshape = (/SIZE(ttr_mask)/)
! finally perform computations
IF (ASSOCIATED(this%voldatir)) THEN
  DO j = 1, SIZE(that%timerange)
    DO i = 1, SIZE(that%time)

      DO i1 = 1, SIZE(this%ana)
        DO i3 = 1, SIZE(this%level)
          DO i6 = 1, SIZE(this%network)
            DO i5 = 1, SIZE(this%dativar%r)

              frac_m = 0.
              DO n1 = SIZE(dtratio), 1, -1 ! precedence to longer periods
                IF (dtratio(n1) <= 0) CYCLE ! safety check
                ttr_mask = .FALSE.
                DO n = 1, map_ttr(i,j)%arraysize
                  IF (map_ttr(i,j)%array(n)%extra_info == dtratio(n1)) THEN
                    IF (c_e(this%voldatir(i1,map_ttr(i,j)%array(n)%it,i3, &
                     map_ttr(i,j)%array(n)%itr,i5,i6))) THEN
                      ttr_mask(map_ttr(i,j)%array(n)%it, &
                       map_ttr(i,j)%array(n)%itr) = .TRUE.
                    ENDIF
                  ENDIF
                ENDDO

                ndtr = COUNT(ttr_mask)
                frac_c = REAL(ndtr)/REAL(dtratio(n1))

                IF (ndtr > 0 .AND. frac_c >= MAX(lfrac_valid, frac_m)) THEN
                  frac_m = frac_c
                  SELECT CASE(stat_proc)
                  CASE (0, 200) ! average, vectorial mean
                    that%voldatir(i1,i,i3,j,i5,i6) = &
                     SUM(this%voldatir(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)/ndtr
                  CASE (1, 4) ! accumulation, difference
                    that%voldatir(i1,i,i3,j,i5,i6) = &
                     SUM(this%voldatir(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (2) ! maximum
                    that%voldatir(i1,i,i3,j,i5,i6) = &
                     MAXVAL(this%voldatir(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (3) ! minimum
                    that%voldatir(i1,i,i3,j,i5,i6) = &
                     MINVAL(this%voldatir(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (6) ! stddev
                    that%voldatir(i1,i,i3,j,i5,i6) = &
                     stat_stddev( &
                     RESHAPE(this%voldatir(i1,:,i3,:,i5,i6), shape=linshape), &
                     mask=RESHAPE(ttr_mask, shape=linshape))
                  END SELECT
                ENDIF

              ENDDO ! dtratio
            ENDDO ! var
          ENDDO ! network
        ENDDO ! level
      ENDDO ! ana
      CALL delete(map_ttr(i,j))
    ENDDO ! otime
  ENDDO ! otimerange
ENDIF

IF (ASSOCIATED(this%voldatid)) THEN
  DO j = 1, SIZE(that%timerange)
    DO i = 1, SIZE(that%time)

      DO i1 = 1, SIZE(this%ana)
        DO i3 = 1, SIZE(this%level)
          DO i6 = 1, SIZE(this%network)
            DO i5 = 1, SIZE(this%dativar%d)

              frac_m = 0.
              DO n1 = SIZE(dtratio), 1, -1 ! precedence to longer periods
                IF (dtratio(n1) <= 0) CYCLE ! safety check
                ttr_mask = .FALSE.
                DO n = 1, map_ttr(i,j)%arraysize
                  IF (map_ttr(i,j)%array(n)%extra_info == dtratio(n1)) THEN
                    IF (c_e(this%voldatid(i1,map_ttr(i,j)%array(n)%it,i3, &
                     map_ttr(i,j)%array(n)%itr,i5,i6))) THEN
                      ttr_mask(map_ttr(i,j)%array(n)%it, &
                       map_ttr(i,j)%array(n)%itr) = .TRUE.
                    ENDIF
                  ENDIF
                ENDDO

                ndtr = COUNT(ttr_mask)
                frac_c = REAL(ndtr)/REAL(dtratio(n1))

                IF (ndtr > 0 .AND. frac_c >= MAX(lfrac_valid, frac_m)) THEN
                  frac_m = frac_c
                  SELECT CASE(stat_proc)
                  CASE (0) ! average
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     SUM(this%voldatid(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)/ndtr
                  CASE (1, 4) ! accumulation, difference
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     SUM(this%voldatid(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (2) ! maximum
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     MAXVAL(this%voldatid(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (3) ! minimum
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     MINVAL(this%voldatid(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (6) ! stddev
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     stat_stddev( &
                     RESHAPE(this%voldatid(i1,:,i3,:,i5,i6), shape=linshape), &
                     mask=RESHAPE(ttr_mask, shape=linshape))
                  END SELECT
                ENDIF

              ENDDO ! dtratio
            ENDDO ! var
          ENDDO ! network
        ENDDO ! level
      ENDDO ! ana
      CALL delete(map_ttr(i,j))
    ENDDO ! otime
  ENDDO ! otimerange
ENDIF

DEALLOCATE(ttr_mask)

CALL makeother()

CONTAINS

SUBROUTINE makeother()
IF (PRESENT(other)) THEN ! create volume with the remaining data for further processing
  CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(this%timerange(:)%timerange /= tri .OR. this%timerange(:)%p2 == imiss &
   .OR. this%timerange(:)%p2 == 0)) ! or MOD(steps, this%timerange(:)%p2) == 0
ENDIF
END SUBROUTINE makeother

END SUBROUTINE vol7d_recompute_stat_proc_agg


!> Method for statistically processing a set of instantaneous data.
!! This method performs statistical processing by aggregation of
!! instantaneous data.  Only floating point single or double precision
!! data are processed.
!!
!! The output \a that vol7d object contains elements from the original volume
!! \a this satisfying the conditions
!!  - real single or double precision variables
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type 254 (instantaeous)
!!  - any p1 (analysis/observation or forecast)
!!  - p2 = 0 (processing interval null, instantaneous data)
!!
!! Output data will have timerange of type \a stat_proc, and p2 = \a
!! step.  The supported statistical processing methods (parameter \a
!! stat_proc) are:
!!
!!  - 0 average
!!  - 2 maximum
!!  - 3 minimum
!!  - 4 difference
!!  - 6 standard deviation
!!  - 201 mode (only for wind direction sectors)
!!
!! In the case of average, it is possible to weigh the data
!! proportionally to the length of the time interval for which every
!! single value is valid, i.e. halfway between the time level of the
!! value itself and the time of its nearest valid neighbours (argument
!! \a weighted). A maximum distance in time for input valid data can
!! be assigned with the optional argument \a max_step, in order to
!! filter datasets with too long "holes".
SUBROUTINE vol7d_compute_stat_proc_agg(this, that, stat_proc, &
 step, start, full_steps, max_step, weighted, other)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be computed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the computed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be computed (from grib2 table)
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if \a .TRUE. and \a start is not provided, apply processing only on intervals starting at a forecast time or a reference time modulo \a step
TYPE(timedelta),INTENT(in),OPTIONAL :: max_step !< maximum allowed distance in time between two contiguougs valid data within an interval, for the interval to be eligible for statistical processing
LOGICAL,INTENT(in),OPTIONAL :: weighted !< if provided and \c .TRUE., the statistical process is computed, if possible, by weighting every value with a weight proportional to its validity interval
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the accumulation computation
!INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< to be used with care, type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be recomputed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument

TYPE(vol7d) :: v7dtmp
INTEGER :: tri
INTEGER :: i, j, n, ninp, ndtr, i1, i3, i5, i6, vartype, maxsize
TYPE(timedelta) :: lmax_step, act_max_step
TYPE(datetime) :: pstart, pend, reftime
TYPE(arrayof_ttr_mapper),POINTER :: map_ttr(:,:)
REAL,ALLOCATABLE :: tmpvolr(:)
DOUBLE PRECISION,ALLOCATABLE :: tmpvold(:), weights(:)
LOGICAL,ALLOCATABLE :: lin_mask(:)
LOGICAL :: lweighted
CHARACTER(len=8) :: env_var

IF (PRESENT(max_step)) THEN
  lmax_step = max_step
ELSE
  lmax_step = timedelta_max
ENDIF
lweighted = optio_log(weighted)
tri = 254
! enable bad behavior for climat database
env_var = ''
CALL getenv('LIBSIM_CLIMAT_BEHAVIOR', env_var)
lweighted = lweighted .AND. LEN_TRIM(env_var) == 0
! only average can be weighted
lweighted = lweighted .AND. stat_proc == 0

! be safe
CALL vol7d_alloc_vol(this)
! initial check

! cleanup the original volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.) ! time-ordered volume needed
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)
! copy everything except time and timerange
CALL vol7d_copy(this, v7dtmp, ltime=(/.FALSE./), ltimerange=(/.FALSE./))

! create new volume
CALL init(that, time_definition=this%time_definition)
! compute the output time and timerange and all the required mappings
CALL recompute_stat_proc_agg_common(this%time, this%timerange, stat_proc, tri, &
 step, this%time_definition, that%time, that%timerange, map_ttr, start=start, &
 full_steps=full_steps)
! merge with information from original volume
CALL vol7d_merge(that, v7dtmp)

maxsize = MAXVAL(map_ttr(:,:)%arraysize)
ALLOCATE(tmpvolr(maxsize), tmpvold(maxsize), lin_mask(maxsize), weights(maxsize))
do_otimerange: DO j = 1, SIZE(that%timerange)
  do_otime: DO i = 1, SIZE(that%time)
    ninp = map_ttr(i,j)%arraysize
    IF (ninp <= 0) CYCLE do_otime
! required for some computations
    CALL time_timerange_get_period(that%time(i), that%timerange(j), &
     that%time_definition, pstart, pend, reftime)

    IF (ASSOCIATED(this%voldatir)) THEN
      DO i1 = 1, SIZE(this%ana)
        DO i3 = 1, SIZE(this%level)
          DO i6 = 1, SIZE(this%network)
            DO i5 = 1, SIZE(this%dativar%r)
! stat_proc difference treated separately here
              IF (stat_proc == 4) THEN
                IF (ninp >= 2) THEN
                  IF (map_ttr(i,j)%array(1)%extra_info == 1 .AND. &
                   map_ttr(i,j)%array(ninp)%extra_info == 2) THEN
                    IF (c_e(this%voldatir(i1,map_ttr(i,j)%array(1)%it,i3, &
                     map_ttr(i,j)%array(1)%itr,i5,i6)) .AND. &
                     c_e(this%voldatir(i1,map_ttr(i,j)%array(ninp)%it,i3, &
                     map_ttr(i,j)%array(ninp)%itr,i5,i6))) THEN
                      that%voldatir(i1,i,i3,j,i5,i6) = &
                       this%voldatir(i1,map_ttr(i,j)%array(ninp)%it,i3, &
                       map_ttr(i,j)%array(ninp)%itr,i5,i6) - &
                       this%voldatir(i1,map_ttr(i,j)%array(1)%it,i3, &
                       map_ttr(i,j)%array(1)%itr,i5,i6)
                    ENDIF
                  ENDIF
                ENDIF
                CYCLE
              ENDIF
! other stat_proc
              vartype = vol7d_vartype(this%dativar%r(i5))
              lin_mask = .FALSE.
              ndtr = 0
              DO n = 1, ninp
                IF (c_e(this%voldatir(i1,map_ttr(i,j)%array(n)%it,i3, &
                 map_ttr(i,j)%array(n)%itr,i5,i6))) THEN
                  ndtr = ndtr + 1
                  tmpvolr(ndtr) = this%voldatir(i1,map_ttr(i,j)%array(n)%it,i3, &
                   map_ttr(i,j)%array(n)%itr,i5,i6)
                  lin_mask(n) = .TRUE.
                ENDIF
              ENDDO
              IF (ndtr == 0) CYCLE
              IF (lweighted) THEN
                CALL compute_stat_proc_agg_sw(map_ttr(i,j)%array(1:ninp)%time, &
                 pstart, pend, lin_mask(1:ninp), act_max_step, weights)
              ELSE
                CALL compute_stat_proc_agg_sw(map_ttr(i,j)%array(1:ninp)%time, &
                 pstart, pend, lin_mask(1:ninp), act_max_step)
              ENDIF
              IF (act_max_step > lmax_step) CYCLE

              SELECT CASE(stat_proc)
              CASE (0) ! average
                IF (lweighted) THEN
                  that%voldatir(i1,i,i3,j,i5,i6) = &
                   SUM(REAL(weights(1:ndtr))*tmpvolr(1:ndtr))
                ELSE
                  that%voldatir(i1,i,i3,j,i5,i6) = &
                   SUM(tmpvolr(1:ndtr))/ndtr
                ENDIF
              CASE (2) ! maximum
                that%voldatir(i1,i,i3,j,i5,i6) = &
                 MAXVAL(tmpvolr(1:ndtr))
              CASE (3) ! minimum
                that%voldatir(i1,i,i3,j,i5,i6) = &
                 MINVAL(tmpvolr(1:ndtr))
              CASE (6) ! stddev
                that%voldatir(i1,i,i3,j,i5,i6) = &
                 stat_stddev(tmpvolr(1:ndtr))
              CASE (201) ! mode
! mode only for angles at the moment, with predefined histogram
                IF (vartype == var_dir360) THEN
! remove undefined wind direction (=0), improve check?
! and reduce to interval [22.5,382.5[
                  WHERE (tmpvolr(1:ndtr) == 0.0)
                    tmpvolr(1:ndtr) = rmiss
                  ELSE WHERE (tmpvolr(1:ndtr) < 22.5 .AND. tmpvolr(1:ndtr) > 0.0)
                    tmpvolr(1:ndtr) = tmpvolr(1:ndtr) + 360.
                  END WHERE
                  that%voldatir(i1,i,i3,j,i5,i6) = &
                   stat_mode_histogram(tmpvolr(1:ndtr), &
                   8, 22.5, 382.5)
                ENDIF
              END SELECT
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDIF

    IF (ASSOCIATED(this%voldatid)) THEN
      DO i1 = 1, SIZE(this%ana)
        DO i3 = 1, SIZE(this%level)
          DO i6 = 1, SIZE(this%network)
            DO i5 = 1, SIZE(this%dativar%d)
! stat_proc difference treated separately here
              IF (stat_proc == 4) THEN
                IF (n >= 2) THEN
                  IF (map_ttr(i,j)%array(1)%extra_info == 1 .AND. &
                   map_ttr(i,j)%array(ninp)%extra_info == 2) THEN
                    IF (c_e(this%voldatid(i1,map_ttr(i,j)%array(1)%it,i3, &
                     map_ttr(i,j)%array(1)%itr,i5,i6)) .AND. &
                     c_e(this%voldatid(i1,map_ttr(i,j)%array(ninp)%it,i3, &
                     map_ttr(i,j)%array(ninp)%itr,i5,i6))) THEN
                      that%voldatid(i1,i,i3,j,i5,i6) = &
                       this%voldatid(i1,map_ttr(i,j)%array(ninp)%it,i3, &
                       map_ttr(i,j)%array(ninp)%itr,i5,i6) - &
                       this%voldatid(i1,map_ttr(i,j)%array(1)%it,i3, &
                       map_ttr(i,j)%array(1)%itr,i5,i6)
                    ENDIF
                  ENDIF
                ENDIF
                CYCLE
              ENDIF
! other stat_proc
              vartype = vol7d_vartype(this%dativar%d(i5))
              lin_mask = .FALSE.
              ndtr = 0
              DO n = 1, ninp
                IF (c_e(this%voldatid(i1,map_ttr(i,j)%array(n)%it,i3, &
                 map_ttr(i,j)%array(n)%itr,i5,i6))) THEN
                  ndtr = ndtr + 1
                  tmpvold(ndtr) = this%voldatid(i1,map_ttr(i,j)%array(n)%it,i3, &
                   map_ttr(i,j)%array(n)%itr,i5,i6)
                  lin_mask(n) = .TRUE.
                ENDIF
              ENDDO
              IF (ndtr == 0) CYCLE
              IF (lweighted) THEN
                CALL compute_stat_proc_agg_sw(map_ttr(i,j)%array(1:ninp)%time, &
                 pstart, pend, lin_mask(1:ninp), act_max_step, weights)
              ELSE
                CALL compute_stat_proc_agg_sw(map_ttr(i,j)%array(1:ninp)%time, &
                 pstart, pend, lin_mask(1:ninp), act_max_step)
              ENDIF
              IF (act_max_step > lmax_step) CYCLE

              SELECT CASE(stat_proc)
              CASE (0) ! average
                IF (lweighted) THEN
                  that%voldatid(i1,i,i3,j,i5,i6) = &
                   SUM(REAL(weights(1:ndtr))*tmpvold(1:ndtr))
                ELSE
                  that%voldatid(i1,i,i3,j,i5,i6) = &
                   SUM(tmpvold(1:ndtr))/ndtr
                ENDIF
              CASE (2) ! maximum
                that%voldatid(i1,i,i3,j,i5,i6) = &
                 MAXVAL(tmpvold(1:ndtr))
              CASE (3) ! minimum
                that%voldatid(i1,i,i3,j,i5,i6) = &
                 MINVAL(tmpvold(1:ndtr))
              CASE (6) ! stddev
                that%voldatid(i1,i,i3,j,i5,i6) = &
                 stat_stddev(tmpvold(1:ndtr))
              CASE (201) ! mode
! mode only for angles at the moment, with predefined histogram
                IF (vartype == var_dir360) THEN
! remove undefined wind direction (=0), improve check?
! and reduce to interval [22.5,382.5[
                  WHERE (tmpvold(1:ndtr) == 0.0D0)
                    tmpvold(1:ndtr) = dmiss
                  ELSE WHERE (tmpvold(1:ndtr) < 22.5D0 .AND. tmpvold(1:ndtr) > 0.0D0)
                    tmpvold(1:ndtr) = tmpvold(1:ndtr) + 360.0D0
                  END WHERE
                  that%voldatid(i1,i,i3,j,i5,i6) = &
                   stat_mode_histogram(tmpvold(1:ndtr), &
                   8, 22.5D0, 382.5D0)
                ENDIF
              END SELECT
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDIF

    CALL delete(map_ttr(i,j))
  ENDDO do_otime
ENDDO do_otimerange

DEALLOCATE(map_ttr)
DEALLOCATE(tmpvolr, tmpvold, lin_mask, weights)

IF (PRESENT(other)) THEN ! create volume with the remaining data for further processing
  CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(this%timerange(:)%timerange /= tri))
ENDIF

END SUBROUTINE vol7d_compute_stat_proc_agg


!> Method to transform the timerange of a set of data from
!! statistically processed to instantaneous. The data does not change,
!! only time and timerange descriptors change.
!!
!! The output \a that vol7d object contains elements from the original volume
!! \a this satisfying the conditions
!!  - real single or double precision variables
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type 0 (average) or \a stat_proc_input if provided
!!  - p1 = 0 (end of period == reference time, analysis/observation)
!!  - p2 == \a step
!!
!! Output data will have timerange 254, p1 = 0 and p2 = 0; the time
!! dimension is shifted by half \a step so that it coincides with the
!! mid point of the input statistical processing interval.
SUBROUTINE vol7d_decompute_stat_proc(this, that, step, other, stat_proc_input)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the statistical processing
INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< type of statistical processing of data that has to be processed (from grib2 table), if not provided, averaged data (statistical processing = 0) is processed

INTEGER :: i, tri, steps


IF (PRESENT(stat_proc_input)) THEN
  tri = stat_proc_input
ELSE
  tri = 0
ENDIF
! be safe
CALL vol7d_alloc_vol(this)

! compute length of cumulation step in seconds
CALL getval(step, asec=steps)

! filter requested data
CALL vol7d_copy(this, that, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
 ltimerange=(this%timerange(:)%timerange == tri .AND. &
 this%timerange(:)%p1 == 0 .AND. this%timerange(:)%p2 == steps))

! convert timerange to instantaneous and go back half step in time
that%timerange(:)%timerange = 254
that%timerange(:)%p2 = 0
DO i = 1, SIZE(that%time(:))
  that%time(i) = that%time(i) - step/2
ENDDO

IF (PRESENT(other)) THEN ! create volume with the remaining data for further processing
  CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(this%timerange(:)%timerange /= tri .OR. &
   this%timerange(:)%p1 /= 0 .OR. this%timerange(:)%p2 /= steps))
ENDIF

END SUBROUTINE vol7d_decompute_stat_proc


!> Specialized method for statistically processing a set of data
!! already processed with the same statistical processing, on a
!! different time interval.  This method performs statistical
!! processing by difference of different intervals.  Only floating
!! point single or double precision data with analysis/observation
!! or forecast timerange are processed.
!!
!! The output \a that vol7d object contains elements from the original volume
!! \a this satisfying the conditions
!!  - real single or double precision variables
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type \a stat_proc
!!  - any p1 (analysis/observation or forecast)
!!  - p2 &gt; 0 (processing interval non null, non instantaneous data)
!!    and equal to a multiplier of \a step if \a full_steps is \c .TRUE.
!!
!! Output data will have timerange of type \a stat_proc and p2 = \a
!! step.  The supported statistical processing methods (parameter \a
!! stat_proc) are:
!!
!!  - 0 average
!!  - 1 cumulation
!!  - 4 difference
!!
!! Input volume may have any value of \a this%time_definition, and
!! that value will be conserved in the output volume.
SUBROUTINE vol7d_recompute_stat_proc_diff(this, that, stat_proc, step, full_steps, start, other)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if provided and \a .TRUE., process only data having processing interval (p2) equal to a multiplier of \a step
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
TYPE(vol7d),INTENT(out),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the statistical processing

INTEGER :: i1, i3, i5, i6, i, j, k, l, nitr, steps
INTEGER,ALLOCATABLE :: map_tr(:,:,:,:,:), f(:), keep_tr(:,:,:)
LOGICAL,ALLOCATABLE :: mask_timerange(:)
LOGICAL,ALLOCATABLE :: mask_time(:)
TYPE(vol7d) :: v7dtmp


! be safe
CALL vol7d_alloc_vol(this)
! initialise the template of the output volume
CALL init(that, time_definition=this%time_definition)

! compute length of cumulation step in seconds
CALL getval(step, asec=steps)

! compute the statistical processing relations, output time and
! timerange are defined here
CALL recompute_stat_proc_diff_common(this%time, this%timerange, stat_proc, step, &
 that%time, that%timerange, map_tr, f, keep_tr, &
 this%time_definition, full_steps, start)
nitr = SIZE(f)

! complete the definition of the empty output template
CALL vol7d_alloc(that, nana=0, nlevel=0, nnetwork=0)
CALL vol7d_alloc_vol(that)
! shouldn't we exit here with an empty volume if stat_proc/=0,1 ?
ALLOCATE(mask_time(SIZE(this%time)), mask_timerange(SIZE(this%timerange)))
DO l = 1, SIZE(this%time)
  mask_time(l) = ANY(this%time(l) == that%time(:))
ENDDO
DO l = 1, SIZE(this%timerange)
  mask_timerange(l) = ANY(this%timerange(l) == that%timerange(:))
ENDDO
! create template for the output volume, keep all ana, level, network
! and variables; copy only the timeranges already satisfying the
! requested step, if any and only the times already existing in the
! output
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
 ltimerange=mask_timerange(:), ltime=mask_time(:))
! merge output created so far with template
CALL vol7d_merge(that, v7dtmp, lanasimple=.TRUE., llevelsimple=.TRUE.)

! compute statistical processing
IF (ASSOCIATED(this%voldatir)) THEN
  DO l = 1, SIZE(this%time)
    DO k = 1, nitr
      DO j = 1, SIZE(this%time)
        DO i = 1, nitr
          IF (c_e(map_tr(i,j,k,l,1))) THEN
            DO i6 = 1, SIZE(this%network)
              DO i5 = 1, SIZE(this%dativar%r)
                DO i3 = 1, SIZE(this%level)
                  DO i1 = 1, SIZE(this%ana)
                    IF (c_e(this%voldatir(i1,l,i3,f(k),i5,i6)) .AND. &
                     c_e(this%voldatir(i1,j,i3,f(i),i5,i6))) THEN

                      IF (stat_proc == 0) THEN ! average
                        that%voldatir( &
                         i1,map_tr(i,j,k,l,1),i3,map_tr(i,j,k,l,2),i5,i6) = &
                         (this%voldatir(i1,l,i3,f(k),i5,i6)*this%timerange(f(k))%p2 - &
                         this%voldatir(i1,j,i3,f(i),i5,i6)*this%timerange(f(i))%p2)/ &
                         steps ! optimize avoiding conversions
                      ELSE IF (stat_proc == 1 .OR. stat_proc == 4) THEN ! acc, diff
                        that%voldatir( &
                         i1,map_tr(i,j,k,l,1),i3,map_tr(i,j,k,l,2),i5,i6) = &
                         this%voldatir(i1,l,i3,f(k),i5,i6) - &
                         this%voldatir(i1,j,i3,f(i),i5,i6)
                      ENDIF

                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

IF (ASSOCIATED(this%voldatid)) THEN
  DO l = 1, SIZE(this%time)
    DO k = 1, nitr
      DO j = 1, SIZE(this%time)
        DO i = 1, nitr
          IF (c_e(map_tr(i,j,k,l,1))) THEN
            DO i6 = 1, SIZE(this%network)
              DO i5 = 1, SIZE(this%dativar%d)
                DO i3 = 1, SIZE(this%level)
                  DO i1 = 1, SIZE(this%ana)
                    IF (c_e(this%voldatid(i1,l,i3,f(k),i5,i6)) .AND. &
                     c_e(this%voldatid(i1,j,i3,f(i),i5,i6))) THEN
!                      IF (.NOT.c_e(that%voldatid( &
!                       i1,map_tr(i,j,k,l,1),i3,map_tr(i,j,k,l,2),i5,i6))) THEN

                      IF (stat_proc == 0) THEN ! average
                        that%voldatid( &
                         i1,map_tr(i,j,k,l,1),i3,map_tr(i,j,k,l,2),i5,i6) = &
                         (this%voldatid(i1,l,i3,f(k),i5,i6)*this%timerange(f(k))%p2 - &
                         this%voldatid(i1,j,i3,f(i),i5,i6)*this%timerange(f(i))%p2)/ &
                         steps ! optimize avoiding conversions
                      ELSE IF (stat_proc == 1 .OR. stat_proc == 4) THEN ! acc, diff
                        that%voldatid( &
                         i1,map_tr(i,j,k,l,1),i3,map_tr(i,j,k,l,2),i5,i6) = &
                         this%voldatid(i1,l,i3,f(k),i5,i6) - &
                         this%voldatid(i1,j,i3,f(i),i5,i6)
                      ENDIF

!                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

! this should be avoided by sorting descriptors upstream
! descriptors now are sorted upstream with a dirty and expensive trick
! but the order may be scrambled in the call to vol7d_merge above
CALL vol7d_smart_sort(that, lsort_time=.TRUE., lsort_timerange=.TRUE.)

CALL makeother(.TRUE.)

CONTAINS

SUBROUTINE makeother(filter)
LOGICAL,INTENT(in) :: filter
IF (PRESENT(other)) THEN
  IF (filter) THEN ! create volume with the remaining data for further processing
    CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
     ltimerange=(this%timerange(:)%timerange /= stat_proc))
  ELSE
    CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE.)
  ENDIF
ENDIF
END SUBROUTINE makeother

END SUBROUTINE vol7d_recompute_stat_proc_diff


!> Specialized method for statistically processing a set of data
!! by integration/differentiation.
!! This method performs statistical processing by integrating
!! (accumulating) in time values representing time-average rates or
!! fluxes, (\a stat_proc_input=0 \a stat_proc=1) or by transforming a
!! time-integrated (accumulated) value in a time-average rate or flux
!! (\a stat_proc_input=1 \a stat_proc=0). Analysis/observation or
!! forecast timeranges are processed. The only operation performed is
!! respectively multiplying or dividing the values by the length of
!! the time interval in seconds.
!!
!! The output \a that vol7d object contains elements from the original
!! volume \a this satisfying the conditions
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type \a stat_proc_input (0 or 1)
!!  - any p1 (analysis/observation or forecast)
!!  - p2 &gt; 0 (processing interval non null, non instantaneous data)
!!
!! Output data will have timerange of type \a stat_proc (1 or 0) and
!! p1 and p2 equal to the corresponding input values.  The supported
!! statistical processing methods (parameter \a stat_proc) are:
!!
!!  - 0 average
!!  - 1 cumulation
!!
!! Input volume may have any value of \a this%time_definition, and
!! that value will be conserved in the output volume.
SUBROUTINE vol7d_compute_stat_proc_metamorph(this, that, stat_proc_input, stat_proc)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc_input !< type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be processed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), data in output volume \a that will have a timerange of this type

INTEGER :: j
LOGICAL,ALLOCATABLE :: tr_mask(:)
REAL,ALLOCATABLE :: int_ratio(:)
DOUBLE PRECISION,ALLOCATABLE :: int_ratiod(:)

IF (.NOT.((stat_proc_input == 0 .AND. stat_proc == 1) .OR. &
 (stat_proc_input == 1 .AND. stat_proc == 0))) THEN

  CALL l4f_log(L4F_WARN, &
   'compute_stat_proc_metamorph, can only be applied to average->accumulated timerange and viceversa')
! return an empty volume, without signaling error
  CALL init(that)
  CALL vol7d_alloc_vol(that)
  RETURN
ENDIF

! be safe
CALL vol7d_alloc_vol(this)

! useful timeranges
tr_mask = this%timerange(:)%timerange == stat_proc_input .AND. this%timerange(:)%p2 /= imiss &
 .AND. this%timerange(:)%p2 /= 0

! initial check (necessary?)
IF (COUNT(tr_mask) == 0) THEN
  CALL l4f_log(L4F_WARN, &
   'vol7d_compute, no timeranges suitable for statistical processing by metamorphosis')
  CALL init(that)
!  CALL makeother()
  RETURN
ENDIF

! copy required data and reset timerange
CALL vol7d_copy(this, that, ltimerange=tr_mask)
that%timerange(:)%timerange = stat_proc
! why next automatic f2003 allocation does not always work?
ALLOCATE(int_ratio(SIZE(that%timerange)), int_ratiod(SIZE(that%timerange)))

IF (stat_proc == 0) THEN ! average -> integral
  int_ratio = 1./REAL(that%timerange(:)%p2)
  int_ratiod = 1./DBLE(that%timerange(:)%p2)
ELSE ! cumulation
  int_ratio = REAL(that%timerange(:)%p2)
  int_ratiod = DBLE(that%timerange(:)%p2)
ENDIF

IF (ASSOCIATED(that%voldatir)) THEN
  DO j = 1, SIZE(that%timerange)
    WHERE(c_e(that%voldatir(:,:,:,j,:,:)))
      that%voldatir(:,:,:,j,:,:) = that%voldatir(:,:,:,j,:,:)*int_ratio(j)
    ELSEWHERE
      that%voldatir(:,:,:,j,:,:) = rmiss
    END WHERE
  ENDDO
ENDIF

IF (ASSOCIATED(that%voldatid)) THEN
  DO j = 1, SIZE(that%timerange)
    WHERE(c_e(that%voldatid(:,:,:,j,:,:)))
      that%voldatid(:,:,:,j,:,:) = that%voldatid(:,:,:,j,:,:)*int_ratiod(j)
    ELSEWHERE
      that%voldatid(:,:,:,j,:,:) = rmiss
    END WHERE
  ENDDO
ENDIF


END SUBROUTINE vol7d_compute_stat_proc_metamorph


SUBROUTINE vol7d_recompute_stat_proc_agg_multiv(this, that, &
 step, start, frac_valid, multiv_proc)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
!INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
!TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the statistical processing
!INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< to be used with care, type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be recomputed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument
INTEGER,INTENT(in) :: multiv_proc !< index of multivariate specific operation

INTEGER :: tri
INTEGER :: i, j, n, n1, ndtr, i1, i3, i5, i6
INTEGER :: linshape(1)
REAL :: lfrac_valid, frac_c, frac_m
LOGICAL,ALLOCATABLE :: ttr_mask(:,:)
TYPE(arrayof_ttr_mapper),POINTER :: map_ttr(:,:)
INTEGER,POINTER :: dtratio(:)
INTEGER :: stat_proc_input, stat_proc

SELECT CASE(multiv_proc)
CASE (1) ! direction of maximum
  stat_proc_input = 205
  stat_proc=205
END SELECT

tri = stat_proc_input
IF (PRESENT(frac_valid)) THEN
  lfrac_valid = frac_valid
ELSE
  lfrac_valid = 1.0
ENDIF

! be safe
CALL vol7d_alloc_vol(this)
! initial check

! cleanup the original volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.) ! time-ordered volume needed
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)

CALL init(that, time_definition=this%time_definition)
CALL vol7d_alloc(that, nana=SIZE(this%ana), nlevel=SIZE(this%level), &
 nnetwork=SIZE(this%network))
IF (ASSOCIATED(this%dativar%r)) THEN
  CALL vol7d_alloc(that, ndativarr=SIZE(this%dativar%r))
  that%dativar%r = this%dativar%r
ENDIF
IF (ASSOCIATED(this%dativar%d)) THEN
  CALL vol7d_alloc(that, ndativard=SIZE(this%dativar%d))
  that%dativar%d = this%dativar%d
ENDIF
that%ana = this%ana
that%level = this%level
that%network = this%network

! compute the output time and timerange and all the required mappings
CALL recompute_stat_proc_agg_common(this%time, this%timerange, stat_proc, tri, &
 step, this%time_definition, that%time, that%timerange, map_ttr, &
 dtratio=dtratio, start=start)
CALL vol7d_alloc_vol(that)

ALLOCATE(ttr_mask(SIZE(this%time), SIZE(this%timerange)))
linshape = (/SIZE(ttr_mask)/)
! finally perform computations
IF (ASSOCIATED(this%voldatir)) THEN
  DO j = 1, SIZE(that%timerange)
    DO i = 1, SIZE(that%time)

      DO i1 = 1, SIZE(this%ana)
        DO i3 = 1, SIZE(this%level)
          DO i6 = 1, SIZE(this%network)
            DO i5 = 1, SIZE(this%dativar%r)

              frac_m = 0.
              DO n1 = SIZE(dtratio), 1, -1 ! precedence to longer periods
                IF (dtratio(n1) <= 0) CYCLE ! safety check
                ttr_mask = .FALSE.
                DO n = 1, map_ttr(i,j)%arraysize
                  IF (map_ttr(i,j)%array(n)%extra_info == dtratio(n1)) THEN
                    IF (c_e(this%voldatir(i1,map_ttr(i,j)%array(n)%it,i3, &
                     map_ttr(i,j)%array(n)%itr,i5,i6))) THEN
                      ttr_mask(map_ttr(i,j)%array(n)%it, &
                       map_ttr(i,j)%array(n)%itr) = .TRUE.
                    ENDIF
                  ENDIF
                ENDDO

                ndtr = COUNT(ttr_mask)
                frac_c = REAL(ndtr)/REAL(dtratio(n1))

                IF (ndtr > 0 .AND. frac_c >= MAX(lfrac_valid, frac_m)) THEN
                  frac_m = frac_c
                  SELECT CASE(multiv_proc)
                  CASE (1) ! average, vectorial mean
                    that%voldatir(i1,i,i3,j,i5,i6) = &
                     SUM(this%voldatir(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)/ndtr
                  END SELECT
                ENDIF

              ENDDO ! dtratio
            ENDDO ! var
          ENDDO ! network
        ENDDO ! level
      ENDDO ! ana
      CALL delete(map_ttr(i,j))
    ENDDO ! otime
  ENDDO ! otimerange
ENDIF

IF (ASSOCIATED(this%voldatid)) THEN
  DO j = 1, SIZE(that%timerange)
    DO i = 1, SIZE(that%time)

      DO i1 = 1, SIZE(this%ana)
        DO i3 = 1, SIZE(this%level)
          DO i6 = 1, SIZE(this%network)
            DO i5 = 1, SIZE(this%dativar%d)

              frac_m = 0.
              DO n1 = SIZE(dtratio), 1, -1 ! precedence to longer periods
                IF (dtratio(n1) <= 0) CYCLE ! safety check
                ttr_mask = .FALSE.
                DO n = 1, map_ttr(i,j)%arraysize
                  IF (map_ttr(i,j)%array(n)%extra_info == dtratio(n1)) THEN
                    IF (c_e(this%voldatid(i1,map_ttr(i,j)%array(n)%it,i3, &
                     map_ttr(i,j)%array(n)%itr,i5,i6))) THEN
                      ttr_mask(map_ttr(i,j)%array(n)%it, &
                       map_ttr(i,j)%array(n)%itr) = .TRUE.
                    ENDIF
                  ENDIF
                ENDDO

                ndtr = COUNT(ttr_mask)
                frac_c = REAL(ndtr)/REAL(dtratio(n1))

                IF (ndtr > 0 .AND. frac_c >= MAX(lfrac_valid, frac_m)) THEN
                  frac_m = frac_c
                  SELECT CASE(stat_proc)
                  CASE (0) ! average
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     SUM(this%voldatid(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)/ndtr
                  CASE (1, 4) ! accumulation, difference
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     SUM(this%voldatid(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (2) ! maximum
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     MAXVAL(this%voldatid(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (3) ! minimum
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     MINVAL(this%voldatid(i1,:,i3,:,i5,i6), &
                     mask=ttr_mask)
                  CASE (6) ! stddev
                    that%voldatid(i1,i,i3,j,i5,i6) = &
                     stat_stddev( &
                     RESHAPE(this%voldatid(i1,:,i3,:,i5,i6), shape=linshape), &
                     mask=RESHAPE(ttr_mask, shape=linshape))
                  END SELECT
                ENDIF

              ENDDO ! dtratio
            ENDDO ! var
          ENDDO ! network
        ENDDO ! level
      ENDDO ! ana
      CALL delete(map_ttr(i,j))
    ENDDO ! otime
  ENDDO ! otimerange
ENDIF

DEALLOCATE(ttr_mask)

END SUBROUTINE vol7d_recompute_stat_proc_agg_multiv

!> Riempimento dei buchi temporali in un volume.
!! Questo metodo crea, a partire da un volume originale, un nuovo
!! volume dati in cui la dimensione tempo contiene tutti gli istanti
!! tra \a start e \a stopp (o tra il primo e l'ultimo livello
!! temporale) ad intervalli \a step. Gli eventuali livelli mancanti
!! vengono aggiunti riempiendo le corrispondenti posizioni dei volumi
!! dati con valori mancanti. I livelli temporali che non sono ad
!! intervalli \a step interi a partire dall'inizio, oppure quelli che
!! giacciono fuori dall'intervallo \a start:stop non vengono toccati e
!! quindi rimangono immutati nel volume finale (si veda anche la
!! descrizione di ::vol7d_filter_time). Il volume originale non
!! viene modificato e quindi dovrà essere distrutto da parte del
!! programma chiamante se il suo contenuto non è più
!! richiesto. Attenzione, se necessario la dimensione tempo (vettore
!! \a this%time del volume \a this ) viene riordinata, come effetto
!! collaterale della chiamata.
SUBROUTINE vol7d_fill_time(this, that, step, start, stopp, cyclicdt)
TYPE(vol7d),INTENT(inout) :: this
TYPE(vol7d),INTENT(inout) :: that
TYPE(timedelta),INTENT(in) :: step
TYPE(datetime),INTENT(in),OPTIONAL :: start
TYPE(datetime),INTENT(in),OPTIONAL :: stopp
TYPE(cyclicdatetime),INTENT(in),OPTIONAL :: cyclicdt !< cyclic date and time

TYPE(cyclicdatetime) :: lcyclicdt
TYPE(datetime) :: counter, lstart, lstop
INTEGER :: i, naddtime

CALL safe_start_stop(this, lstart, lstop, start, stopp)
IF (.NOT. c_e(lstart) .OR. .NOT. c_e(lstop) .OR. .NOT. c_e(step)) RETURN

lcyclicdt=cyclicdatetime_miss
if (present(cyclicdt)) then
  if(c_e(cyclicdt)) lcyclicdt=cyclicdt
end if

CALL l4f_log(L4F_INFO, 'vol7d_fill_time: time interval '//TRIM(to_char(lstart))// &
 ' '//TRIM(to_char(lstop)))

! Count the number of time levels required for completing the series
! valid also in the case (SIZE(this%time) == 0)
naddtime = 0
counter = lstart
i = 1
naddcount: DO WHILE(counter <= lstop)
  DO WHILE(i <= SIZE(this%time)) ! this%time(i) chases counter
    IF (counter < this%time(i)) THEN ! this%time(i) overtook counter
      i = MAX(i-1,1) ! go back if possible
      EXIT
    ELSE IF (counter == this%time(i) .OR. .NOT. counter == lcyclicdt) THEN ! found matching time
      counter = counter + step
      CYCLE naddcount
    ENDIF
    i = i + 1
  ENDDO
  naddtime = naddtime + 1
  counter = counter + step
ENDDO naddcount

! old universal algorithm, not optimized, check that the new one is equivalent
!naddtime = 0
!counter = lstart
!DO WHILE(counter <= lstop)
!  IF (.NOT.ANY(counter == this%time(:))) THEN
!    naddtime = naddtime + 1
!  ENDIF
!  counter = counter + step
!ENDDO

IF (naddtime > 0) THEN

  CALL init(that)
  CALL vol7d_alloc(that, ntime=naddtime)
  CALL vol7d_alloc_vol(that)

  ! Repeat the count loop setting the time levels to be added
  naddtime = 0
  counter = lstart
  i = 1
  naddadd: DO WHILE(counter <= lstop)
    DO WHILE(i <= SIZE(this%time)) ! this%time(i) chases counter
      IF (counter < this%time(i)) THEN ! this%time(i) overtook counter
        i = MAX(i-1,1) ! go back if possible
        EXIT
      ELSE IF (counter == this%time(i) .OR. .NOT. counter == lcyclicdt) THEN ! found matching time
        counter = counter + step
        CYCLE naddadd
      ENDIF
      i = i + 1
    ENDDO
    naddtime = naddtime + 1
    that%time(naddtime) = counter ! only difference
    counter = counter + step
  ENDDO naddadd

  CALL vol7d_append(that, this, sort=.TRUE.)

ELSE
!! ? why sort all dimension ?
!!  CALL vol7d_copy(this, that, lsort_time=.TRUE.)
  CALL vol7d_copy(this, that, sort=.TRUE.)
ENDIF


END SUBROUTINE vol7d_fill_time


!> Filter time dimension inside a volume.
!! Questo metodo crea, a partire da un volume originale, un nuovo
!! volume dati in cui la dimensione tempo contiene solo gli
!! istanti tra \a start e \a stopp (o tra il primo e l'ultimo livello
!! temporale) ad intervalli \a step; se specificato cyclicdt solo i 
!! corrispondenti istanti di tempo vengono ulteriormente selezionati.
!! Il volume originale non viene
!! modificato e quindi dovrà essere distrutto da parte del programma
!! chiamante se il suo contenuto non è più richiesto. Attenzione, se
!! necessario, la dimensione tempo (vettore \a this%time del volume \a
!! this ) viene riordinata, come effetto collaterale della chiamata.
SUBROUTINE vol7d_filter_time(this, that, step, start, stopp, cyclicdt)
TYPE(vol7d),INTENT(inout) :: this
TYPE(vol7d),INTENT(inout) :: that
TYPE(timedelta),INTENT(in),optional :: step !< missing value admitted
TYPE(datetime),INTENT(in),OPTIONAL :: start
TYPE(datetime),INTENT(in),OPTIONAL :: stopp
TYPE(cyclicdatetime),INTENT(in),OPTIONAL :: cyclicdt !< cyclic date and time

TYPE(datetime) :: lstart, lstop
LOGICAL, ALLOCATABLE :: time_mask(:)

CALL safe_start_stop(this, lstart, lstop, start, stopp)
IF (.NOT. c_e(lstart) .OR. .NOT. c_e(lstop)) RETURN

CALL l4f_log(L4F_INFO, 'vol7d_filter_time: time interval '//TRIM(to_char(lstart))// &
 ' '//TRIM(to_char(lstop)))

ALLOCATE(time_mask(SIZE(this%time)))

time_mask = this%time >= lstart .AND. this%time <= lstop

IF (PRESENT(cyclicdt)) THEN
  IF (c_e(cyclicdt)) THEN
    time_mask = time_mask .AND. this%time == cyclicdt
  ENDIF
ENDIF

IF (PRESENT(step)) THEN
  IF (c_e(step)) THEN
    time_mask = time_mask .AND. MOD(this%time - lstart, step) == timedelta_0 
  ENDIF
ENDIF

CALL vol7d_copy(this,that, ltime=time_mask)

DEALLOCATE(time_mask)

END SUBROUTINE vol7d_filter_time


!> Fill data volume
!! Nearest data in time is set in the time coordinate.
!! Take in account istantaneous values only.
SUBROUTINE vol7d_fill_data(this, step, start, stopp, tolerance)
TYPE(vol7d),INTENT(inout) :: this !< data volume to elaborate
TYPE(timedelta),INTENT(in) :: step !< interval in time where to fill data
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start time where to fill
TYPE(datetime),INTENT(in),OPTIONAL :: stopp !< stop time where to fill
TYPE(timedelta),INTENT(in),optional :: tolerance !< tolerance in time to find data to fill (excluding extreme) (default to step)

TYPE(datetime) :: lstart, lstop
integer :: indana , indtime ,indlevel ,indtimerange ,inddativarr, indnetwork, iindtime
type(timedelta) :: deltato,deltat, ltolerance

CALL safe_start_stop(this, lstart, lstop, start, stopp)
IF (.NOT. c_e(lstart) .OR. .NOT. c_e(lstop)) RETURN

CALL l4f_log(L4F_INFO, 'vol7d_fill_data: time interval '//TRIM(to_char(lstart))// &
 ' '//TRIM(to_char(lstop)))


ltolerance=step/2

if (present(tolerance))then
  if (c_e(tolerance)) ltolerance=tolerance
end if


do indtime=1,size(this%time)
  
  IF (this%time(indtime) < lstart .OR. this%time(indtime) > lstop .OR. &
   MOD(this%time(indtime) - lstart, step) /= timedelta_0) cycle
  do indtimerange=1,size(this%timerange)
    if (this%timerange(indtimerange)%timerange /= 254) cycle
    do indnetwork=1,size(this%network)
      do inddativarr=1,size(this%dativar%r)
        do indlevel=1,size(this%level)
          do indana=1,size(this%ana)
            
                                !find the nearest data in time if data is missing
            if (.not. c_e(this%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)))then
              deltato=timedelta_miss

              !do iindtime=max(indtime-20,1),min(indtime+20,size(this%time)) !check on a chunk: 20 should be enought
              
              do iindtime=indtime+1,size(this%time) !check forward

                if (c_e(this%voldatir  (indana, iindtime, indlevel, indtimerange, inddativarr, indnetwork )))then
                    deltat=this%time(iindtime)-this%time(indtime)

                  if  (deltat >= ltolerance) exit
                  
                  if (deltat < deltato) then
                    this%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork) = &
                     this%voldatir(indana, iindtime, indlevel, indtimerange, inddativarr, indnetwork)
                    deltato=deltat
                  end if
                end if
              end do

              do iindtime=indtime-1,1,-1 !check backward

                if (c_e(this%voldatir  (indana, iindtime, indlevel, indtimerange, inddativarr, indnetwork )))then
                  if (iindtime < indtime) then
                    deltat=this%time(indtime)-this%time(iindtime)
                  else if (iindtime > indtime) then
                    deltat=this%time(iindtime)-this%time(indtime)
                  else
                    cycle
                  end if

                  if  (deltat >= ltolerance) exit
                  
                  if (deltat < deltato) then
                    this%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork) = &
                     this%voldatir(indana, iindtime, indlevel, indtimerange, inddativarr, indnetwork)
                    deltato=deltat
                  end if
                end if
              end do

            end if
          end do
        end do
      end do
    end do
  end do
end do

END SUBROUTINE vol7d_fill_data
            

! private utility routine for checking interval and start-stop times
! in input missing start-stop values are treated as not present
! in output missing start-stop values mean "do nothing"
SUBROUTINE safe_start_stop(this, lstart, lstop, start, stopp)
TYPE(vol7d),INTENT(inout) :: this
TYPE(datetime),INTENT(out) :: lstart
TYPE(datetime),INTENT(out) :: lstop
TYPE(datetime),INTENT(in),OPTIONAL :: start
TYPE(datetime),INTENT(in),OPTIONAL :: stopp

lstart = datetime_miss
lstop = datetime_miss
! initial safety operation
CALL vol7d_alloc_vol(this)
IF (SIZE(this%time) == 0) RETURN ! avoid segmentation fault in case of empty volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.)

IF (PRESENT(start)) THEN
  IF (c_e(start)) THEN
    lstart = start
  ELSE
    lstart = this%time(1)
  ENDIF
ELSE
  lstart = this%time(1)
ENDIF
IF (PRESENT(stopp)) THEN
  IF (c_e(stopp)) THEN
    lstop = stopp
  ELSE
    lstop = this%time(SIZE(this%time))
  ENDIF
ELSE
  lstop = this%time(SIZE(this%time))
ENDIF

END SUBROUTINE safe_start_stop


!> Metodo per normalizzare la coordinata verticale.
!! Per ora la normalizzazione effettuata riporta i valori di pressione
!! nella sezione dati alla coordinata verticale sostituendo quella eventualmente presente.
!! Classicamente serve per i dati con coordinata verticale model layer (105)
!! Essendo che la pressione varia nello spazio orizzontale e nel tempo
!! questo metodo restituisce un solo profilo verticale.
SUBROUTINE vol7d_normalize_vcoord(this,that,ana,time,timerange,network)
TYPE(vol7d),INTENT(INOUT)  :: this !< oggetto da normalizzare
TYPE(vol7d),INTENT(OUT) :: that !< oggetto normalizzato
integer,intent(in)   :: time,ana,timerange,network !< indici dell'elemento da estrarre

character(len=1) :: type
integer :: ind
TYPE(vol7d_var) ::  var
LOGICAL,allocatable :: ltime(:),ltimerange(:),lana(:),lnetwork(:)
logical,allocatable :: maschera(:)


allocate(ltime(size(this%time)))
allocate(ltimerange(size(this%timerange)))
allocate(lana(size(this%ana)))
allocate(lnetwork(size(this%network)))

ltime=.false.
ltimerange=.false.
lana=.false.
lnetwork=.false.

ltime(time)=.true.
ltimerange(timerange)=.true.
lana(ana)=.true.
lnetwork(network)=.true.

call vol7d_copy(this, that,unique=.true.,&
 ltime=ltime,ltimerange=ltimerange,lana=lana,lnetwork=lnetwork )

call init(var, btable="B10004")    ! Pressure
type=cmiss
!type="i"
ind = index(that%dativar, var, type=type)

allocate(maschera(size(that%level)))

maschera = (&
 (that%level%level1 == 105.and.that%level%level2 == 105) .or. &
 (that%level%level1 == 103 .and. that%level%level2 == imiss ) .or. &
 (that%level%level1 == 102 .and. that%level%level2 == imiss )) &
 .and. c_e(that%voldatic(1,1,:,1,ind,1))


select case (type)

case("d")
  
  where (maschera)
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatid(1,1,:,1,ind,1),that%dativar%d(ind)))
    that%level%l1 = int(that%voldatid(1,1,:,1,ind,1))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("r")

  where (maschera)
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatir(1,1,:,1,ind,1),that%dativar%r(ind)))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("i")
    
  where (maschera)
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatii(1,1,:,1,ind,1),that%dativar%i(ind)))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("b")

  where (maschera)
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatib(1,1,:,1,ind,1),that%dativar%b(ind)))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("c")

  where (maschera)
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatic(1,1,:,1,ind,1),that%dativar%c(ind)))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where
    
end select

deallocate(ltime)
deallocate(ltimerange)
deallocate(lana)
deallocate(lnetwork)

END SUBROUTINE vol7d_normalize_vcoord


!!$!> Metodo per calcolare variabili derivate.
!!$!! TO DO !!
!!$SUBROUTINE vol7d_compute_var(this,that,var)
!!$TYPE(vol7d),INTENT(INOUT)  :: this !< oggetto da normalizzare
!!$TYPE(vol7d),INTENT(OUT) :: that !< oggetto normalizzato
!!$
!!$character(len=1) :: type
!!$TYPE(vol7d_var),intent(in) ::  var


!!$call init(var, btable="B10004")    ! Pressure
!!$type=cmiss
!!$call vol7d_varvect_index(that%dativar,var , type=type,index_v=ind)
!!$
!!$select case (type)
!!$
!!$case("d")
!!$  
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatid(1,1,:,1,ind,1),that%dativar%d(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$
!!$case("r")
!!$
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatir(1,1,:,1,ind,1),that%dativar%r(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$
!!$case("i")
!!$    
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatii(1,1,:,1,ind,1),that%dativar%i(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$
!!$case("b")
!!$
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatib(1,1,:,1,ind,1),that%dativar%b(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$
!!$case("c")
!!$
!!$  where (that%level%level1 == 105.and.that%level%level2 == 105)
!!$    that%level%level1 = 100
!!$    that%level%l1 = realdat(that%voldatic(1,1,:,1,ind,1),that%dativar%c(ind))
!!$    that%level%level2 = imiss
!!$    that%level%l2 = imiss
!!$  end where
!!$    
!!$end select

!!$
!!$END SUBROUTINE vol7d_compute_var
!!$



END MODULE vol7d_class_compute
