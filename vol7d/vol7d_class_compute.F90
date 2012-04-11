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
!!  processing is computed on longer intervals by aggregation, see the
!!  description of vol7d_compute_stat_proc_agg()
!!
!!  - \a stat_proc_input = *
!!    - \a stat_proc = 254 consider statistically processed values as
!!      instantaneous without any extra processing
!!  see the description of vol7d_decompute_stat_proc()
!!
!!  - \a stat_proc_input = 0, 1, 2, 3
!!    - \a stat_proc = \a stat_proc_input recompute input data on
!!      different intervals
!!    the same statistical processing is applied to obtain data
!!    processed on a different interval, either longer, by
!!    aggregation, or shorter, by differences, see the description of
!!    vol7d_recompute_stat_proc_agg() and
!!    vol7d_recompute_stat_proc_diff() respectively; it is also
!!    possible to provide \a stat_proc_input \a /= \a stat_proc, but
!!    it has to be used with care.
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
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if \a .TRUE. cumulate only on intervals starting at a forecast time modulo \a step, default is to cumulate on all possible combinations of intervals
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
TYPE(timedelta),INTENT(in),OPTIONAL :: max_step ! maximum allowed distance in time between two single valid data within a dataset, for the dataset to be eligible for statistical processing
LOGICAL,INTENT(in),OPTIONAL :: weighted !< if provided and \c .TRUE., the statistical process is computed, if possible, by weighting every value with a weight proportional to its validity interval
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the accumulation computation

TYPE(vol7d) :: that1, that2, other1
INTEGER :: steps

IF (stat_proc_input == 254) THEN
  CALL l4f_log(L4F_INFO, 'computing statistical processing by aggregation '//&
   TRIM(to_char(stat_proc_input))//':'//TRIM(to_char(stat_proc)))

  CALL vol7d_compute_stat_proc_agg(this, that, stat_proc, &
   step, start, max_step, weighted, other)

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
! average twice on step interval, with a shift of step/2
      CALL vol7d_recompute_stat_proc_agg(this, that1, stat_proc_input, &
     step, frac_valid=1.0)
      CALL vol7d_recompute_stat_proc_agg(this, that2, stat_proc_input, &
     step, start=that1%time(1)+step/2, frac_valid=1.0)
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

ELSE
  CALL l4f_log(L4F_INFO, &
   'recomputing statistically processed data by aggregation and difference '//&
   TRIM(to_char(stat_proc_input))//':'//TRIM(to_char(stat_proc)))

  IF (PRESENT(other)) THEN
    CALL vol7d_recompute_stat_proc_agg(this, that1, stat_proc, &
     step, start, frac_valid, other=other, stat_proc_input=stat_proc_input)
    CALL vol7d_recompute_stat_proc_diff(this, that2, stat_proc, &
     step, full_steps, other=other1)
    CALL vol7d_merge(other, other1, sort=.TRUE.)
  ELSE
    CALL vol7d_recompute_stat_proc_agg(this, that1, stat_proc, &
     step, start, frac_valid, stat_proc_input=stat_proc_input)
    CALL vol7d_recompute_stat_proc_diff(this, that2, stat_proc, step, full_steps)
  ENDIF

  CALL vol7d_merge(that1, that2, sort=.TRUE.)
  CALL delete(that2)
  that = that1
ENDIF

END SUBROUTINE vol7d_compute_stat_proc


!> Specialized method for statistically processing a set of data
!! already processed with the same statistical processing, on a
!! different time interval.  This method performs statistical
!! processing by aggregation of shorter intervals.  Only floating
!! point single or double precision data with analysis/observation
!! timerange are processed.
!!
!! The output \a that vol7d object contains elements from the original volume
!! \a this satisfying the conditions
!!  - real single or double precision variables
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type \a stat_proc (or \a stat_proc_input if provided)
!!  - p1 = 0 (end of period == reference time, analysis/observation)
!!  - p2 > 0 (processing interval non null, non instantaneous data)
!!    and equal to a multiplier of \a step
!!
!! Output data will have timerange of type \a stat_proc, p1 = 0 and p2
!! = \a step.  The supported statistical processing methods (parameter
!! \a stat_proc) are:
!!
!!  - 0 average
!!  - 1 cumulation
!!  - 2 maximum
!!  - 3 minimum
!!
!! The start of processing period can be computed automatically from
!! the input intervals as the first possible interval modulo \a step,
!! or, for a better control, it can be specified explicitely by the
!! optional argument \a start. Be warned that, in the final volume,
!! the first reference time will actually be \a start \a + \a step,
!! since \a start indicates the beginning of first processing
!! interval, while reference time (for analysis/oservation) is the end
!! of the interval.
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
 step, start, frac_valid, other, stat_proc_input)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the statistical processing
INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< to be used with care, type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be recomputed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument

INTEGER :: tri
INTEGER i, j, n, i1, i3, i5, i6
INTEGER,POINTER :: map_tr(:), map_trc(:,:), count_trc(:,:)
REAL :: lfrac_valid, frac_c, frac_m
LOGICAL,ALLOCATABLE :: mask_time(:)
TYPE(vol7d_timerange) :: otimerange
TYPE(vol7d) :: v7dtmp


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

CALL init(that, time_definition=this%time_definition)
! be safe
CALL vol7d_alloc_vol(this)
! initial check
IF (COUNT(this%timerange(:)%timerange == tri .AND. this%timerange(:)%p2 /= imiss &
 .AND. this%timerange(:)%p2 /= 0 .AND. this%timerange(:)%p1 == 0) == 0) THEN
  CALL l4f_log(L4F_WARN, &
   'vol7d_compute, no timeranges suitable for statistical processing by aggregation')
  CALL makeother()
  RETURN
ENDIF

! cleanup the original volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.) ! time-ordered volume needed
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)

! compute the output time and timerange and all the required mappings
CALL recompute_stat_proc_agg_common(this%time, this%timerange, stat_proc, tri, &
 step, that%time, otimerange, map_tr, map_trc, count_trc, start)

! create a template for the new volume with the elements that may not
! be present in the original one, that%time is already allocated
CALL vol7d_alloc(that, nana=0, nlevel=0, ntimerange=1, nnetwork=0)
that%timerange(1) = otimerange
CALL vol7d_alloc_vol(that)

! copy the elements of the original volume that may be useful for the
! new volume into a temporary object, this is usually useless
ALLOCATE(mask_time(SIZE(this%time)))
DO i = 1, SIZE(this%time)
  mask_time(i) = ANY(this%time(i) == that%time)
ENDDO
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
 ltimerange=(this%timerange(:) == that%timerange(1)), ltime=mask_time)
! merge with template
CALL vol7d_merge(that, v7dtmp)

! finally perform computations
! warning: mask_time is reused for a different purpose
IF (ASSOCIATED(this%voldatir)) THEN
  DO i = 1, SIZE(that%time)
    DO i1 = 1, SIZE(this%ana)
      DO i3 = 1, SIZE(this%level)
        DO i6 = 1, SIZE(this%network)
          DO i5 = 1, SIZE(this%dativar%r)
            frac_m = 0.
            DO j = 1, SIZE(map_tr)
! count the number of valid data that contribute to the current interval
              mask_time = this%voldatir(i1,:,i3,map_tr(j),i5,i6) /= rmiss .AND. &
               map_trc(:,j) == i
              n = COUNT(mask_time)
! compute the ratio between n. of valid data and n. of required data (fraction)
              frac_c = REAL(n)/count_trc(i,j)
! keep the timerange giving the maximum fraction
              IF (n > 0 .AND. frac_c >= MAX(lfrac_valid, frac_m)) THEN
                frac_m = frac_c
                IF (stat_proc == 0) THEN ! average
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)/n
                ELSE IF (stat_proc == 1) THEN ! cumulation
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (stat_proc == 2) THEN ! maximum
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   MAXVAL(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (stat_proc == 3) THEN ! minimum
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   MINVAL(this%voldatir(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

IF (ASSOCIATED(this%voldatid)) THEN
  DO i = 1, SIZE(that%time)
    DO i1 = 1, SIZE(this%ana)
      DO i3 = 1, SIZE(this%level)
        DO i6 = 1, SIZE(this%network)
          DO i5 = 1, SIZE(this%dativar%d)
            frac_m = 0.
            DO j = 1, SIZE(map_tr)
! count the number of valid data that contribute to the current interval
              mask_time = this%voldatid(i1,:,i3,map_tr(j),i5,i6) /= rdmiss .AND. &
               map_trc(:,j) == i
              n = COUNT(mask_time)
! compute the ratio between n. of valid data and n. of required data (fraction)
              frac_c = REAL(n)/count_trc(i,j)
! keep the timerange giving the maximum fraction
              IF (n > 0 .AND. frac_c >= MAX(lfrac_valid, frac_m)) THEN
                frac_m = frac_c
                IF (stat_proc == 0) THEN ! average
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)/n
                ELSE IF (stat_proc == 1) THEN ! cumulation
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (stat_proc == 2) THEN ! maximum
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   MAXVAL(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ELSE IF (stat_proc == 3) THEN ! minimum
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   MINVAL(this%voldatid(i1,:,i3,map_tr(j),i5,i6), &
                   mask=mask_time)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF

DEALLOCATE(map_tr, map_trc, count_trc, mask_time)

CALL makeother()

CONTAINS

SUBROUTINE makeother()
IF (PRESENT(other)) THEN ! create volume with the remaining data for further processing
  CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(this%timerange(:)%timerange /= tri .OR. this%timerange(:)%p2 == imiss &
   .OR. this%timerange(:)%p2 == 0 .OR. this%timerange(:)%p1 /= 0))
ENDIF
END SUBROUTINE makeother

END SUBROUTINE vol7d_recompute_stat_proc_agg


!> Method for statistically processing a set of instantaneous data.
!! This method performs statistical processing by aggregation of
!! instantaneous data.  Only floating point single or double precision
!! data with analysis/observation timerange are processed.
!!
!! The output \a that vol7d object contains elements from the original volume
!! \a this satisfying the conditions
!!  - real single or double precision variables
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type 254 (instantaeous)
!!  - p1 = 0 (end of period == reference time, analysis/observation)
!!  - p2 = 0 (processing interval null, instantaneous data)
!!
!! Output data will have timerange of type \a stat_proc, p1 = 0 and p2
!! = \a step.  The supported statistical processing methods (parameter
!! \a stat_proc) are:
!!
!!  - 0 average
!!  - 2 maximum
!!  - 3 minimum
!!
!! In the case of average, it is possible to weigh the data
!! proportionally to the length of the time interval for which every
!! single value is valid, i.e. halfway between the time level of the
!! value itself and the time of its nearest valid neighbours (argument
!! \a weighted). A maximum distance in time for input valid data can
!! be assigned with the optional argument \a max_step, in order to
!! filter datasets with too long "holes".
SUBROUTINE vol7d_compute_stat_proc_agg(this, that, stat_proc, &
 step, start, max_step, weighted, other)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be computed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the computed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be computed (from grib2 table)
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
TYPE(timedelta),INTENT(in),OPTIONAL :: max_step ! maximum allowed distance in time between two single valid data within a dataset, for the dataset to be eligible for statistical processing
LOGICAL,INTENT(in),OPTIONAL :: weighted !< if provided and \c .TRUE., the statistical process is computed, if possible, by weighting every value with a weight proportional to its validity interval
TYPE(vol7d),INTENT(inout),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the accumulation computation
!INTEGER,INTENT(in),OPTIONAL :: stat_proc_input !< to be used with care, type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be recomputed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument

INTEGER :: tri, itr, iw, iwn, i, i1, i3, i5, i6
INTEGER(kind=int_ll),ALLOCATABLE :: stepmsec(:)
INTEGER(kind=int_ll) :: dmsec
TYPE(datetime) :: w_start, w_end
TYPE(timedelta) :: sub_step, lmax_step
TYPE(vol7d_timerange) :: otimerange
TYPE(vol7d) :: v7dtmp
INTEGER,POINTER :: itime_start(:), itime_end(:)
REAL,ALLOCATABLE :: weightr(:)
DOUBLE PRECISION,ALLOCATABLE :: weightd(:)
LOGICAL,ALLOCATABLE :: mask_time(:)
LOGICAL :: lweighted

IF (PRESENT(max_step)) THEN
  lmax_step = max_step
ELSE
  lmax_step = timedelta_max
ENDIF
lweighted = optio_log(weighted)
tri = 254

CALL init(that, time_definition=this%time_definition)
! be safe
CALL vol7d_alloc_vol(this)
! initial check
itr = index(this%timerange(:), vol7d_timerange_new(tri, 0, 0))
IF (itr <= 0) THEN
  CALL l4f_log(L4F_WARN, &
   'vol7d_compute, no timeranges suitable for statistical processing by aggregation')
  CALL makeother()
  RETURN
ENDIF

! cleanup the original volume
CALL vol7d_smart_sort(this, lsort_time=.TRUE.) ! time-ordered volume needed
CALL vol7d_reform(this, miss=.FALSE., sort=.FALSE., unique=.TRUE.)
! recompute itr, it may have changed
itr = index(this%timerange(:), vol7d_timerange_new(tri, 0, 0))

! compute the output time and timerange and all the required mappings
CALL compute_stat_proc_agg_common(this%time, stat_proc, &
 step, that%time, otimerange, itime_start, itime_end, start)

! create a template for the new volume with the elements that may not
! be present in the original one, that%time is already allocated
CALL vol7d_alloc(that, nana=0, nlevel=0, ntimerange=1, nnetwork=0)
that%timerange(1) = otimerange
CALL vol7d_alloc_vol(that)

! copy the elements of the original volume that may be useful for the
! new volume into a temporary object, this is usually useless
ALLOCATE(mask_time(SIZE(this%time)))
DO i = 1, SIZE(this%time)
  mask_time(i) = ANY(this%time(i) == that%time)
ENDDO
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
 ltimerange=(this%timerange(:) == that%timerange(1)), ltime=mask_time)
! merge with template
CALL vol7d_merge(that, v7dtmp)

! compute step length taking into account "pop" intervals
ALLOCATE(stepmsec(SIZE(that%time)))
DO i = 1, SIZE(that%time)
  CALL getval(that%time(i)-(that%time(i)-step), amsec=stepmsec(i))
ENDDO

! finally perform computations
! warning: mask_time is reused for a different purpose
IF (ASSOCIATED(this%voldatir)) THEN
  ALLOCATE(weightr(SIZE(this%time)))
  DO i6 = 1, SIZE(this%network)
    DO i5 = 1, SIZE(this%dativar%r)
      DO i3 = 1, SIZE(this%level)
        DO i1 = 1, SIZE(this%ana)
          mask_time(:) = this%voldatir(i1,:,i3,itr,i5,i6) /= rmiss
          weightr(:) = rmiss
          timeloopr: DO i = 1, SIZE(that%time)

! start of first interval = start of output interval
            w_start = that%time(i) - step
! search first valid value index (iw)
            iw = itime_start(i)
            DO WHILE(iw <= itime_end(i))
              IF (mask_time(iw)) EXIT
              iw = iw + 1
            ENDDO

! loop on input data in output interval
            DO WHILE(iw <= itime_end(i))
! search next valid value index (iwn)
              iwn = iw + 1
              DO WHILE(iwn <= itime_end(i))
                IF (mask_time(iwn)) EXIT
                iwn = iwn + 1
              ENDDO
              IF (iwn > itime_end(i)) THEN ! next is the last
! end of last interval = end of output interval
                w_end = that%time(i) ! 
              ELSE ! next is not the last
! end of interval = midpoint between this and next value
                w_end = this%time(iw) + (this%time(iwn)-this%time(iw))/2
              ENDIF
! compute length of interval associated to current value
              sub_step = w_end - w_start
              IF (sub_step > lmax_step) CYCLE timeloopr ! output interval rejected
              IF (lweighted) THEN ! we should optimize for the unweighted case 
! compute relative weight
                CALL getval(sub_step, amsec=dmsec)
                weightr(iw) = REAL(dmsec)/REAL(stepmsec(i))
              ENDIF
! next becomes current
              iw = iwn
              w_start = w_end
            ENDDO

            IF (COUNT(mask_time(itime_start(i):itime_end(i))) > 0) THEN
              IF (stat_proc == 0) THEN ! average

                IF (lweighted) THEN
                  WHERE(mask_time(itime_start(i):itime_end(i)))
                    weightr(itime_start(i):itime_end(i)) = &
                     weightr(itime_start(i):itime_end(i)) * &
                     this%voldatir(i1,itime_start(i):itime_end(i),i3,itr,i5,i6)
                  END WHERE
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(weightr(itime_start(i):itime_end(i)), &
                   mask=mask_time(itime_start(i):itime_end(i)))

                ELSE
                  that%voldatir(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatir(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                   mask=mask_time(itime_start(i):itime_end(i)))/ &
                   COUNT(mask_time(itime_start(i):itime_end(i)))
                ENDIF

              ELSE IF (stat_proc == 2) THEN ! maximum

                that%voldatir(i1,i,i3,1,i5,i6) = &
                 MAXVAL( &
                 this%voldatir(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                 mask=mask_time(itime_start(i):itime_end(i)))

              ELSE IF (stat_proc == 3) THEN ! minimum

                that%voldatir(i1,i,i3,1,i5,i6) = &
                 MINVAL( &
                 this%voldatir(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                 mask=mask_time(itime_start(i):itime_end(i)))

              ENDIF
            ENDIF
          ENDDO timeloopr
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  DEALLOCATE(weightr)
ENDIF

! finally perform computations
! warning: mask_time is reused for a different purpose
IF (ASSOCIATED(this%voldatid)) THEN
  ALLOCATE(weightd(SIZE(this%time)))
  DO i6 = 1, SIZE(this%network)
    DO i5 = 1, SIZE(this%dativar%r)
      DO i3 = 1, SIZE(this%level)
        DO i1 = 1, SIZE(this%ana)
          mask_time(:) = this%voldatid(i1,:,i3,itr,i5,i6) /= rmiss
          weightd(:) = dmiss
          timeloopd: DO i = 1, SIZE(that%time)

! start of first interval = start of output interval
            w_start = that%time(i) - step
! search first valid value index (iw)
            iw = itime_start(i)
            DO WHILE(iw <= itime_end(i))
              IF (mask_time(iw)) EXIT
              iw = iw + 1
            ENDDO

! loop on input data in output interval
            DO WHILE(iw <= itime_end(i))
! search next valid value index (iwn)
              iwn = iw + 1
              DO WHILE(iwn <= itime_end(i))
                IF (mask_time(iwn)) EXIT
                iwn = iwn + 1
              ENDDO
              IF (iwn > itime_end(i)) THEN ! next is the last
! end of last interval = end of output interval
                w_end = that%time(i) ! 
              ELSE ! next is not the last
! end of interval = midpoint between this and next value
                w_end = this%time(iw) + (this%time(iwn)-this%time(iw))/2
              ENDIF
! compute length of interval associated to current value
              sub_step = w_end - w_start
              IF (sub_step > lmax_step) CYCLE timeloopd ! output interval rejected
              IF (lweighted) THEN ! we should optimize for the unweighted case 
! compute relative weight
                CALL getval(sub_step, amsec=dmsec)
                weightd(iw) = DBLE(dmsec)/DBLE(stepmsec(i))
              ENDIF
! next becomes current
              iw = iwn
              w_start = w_end
            ENDDO

            IF (COUNT(mask_time(itime_start(i):itime_end(i))) > 0) THEN
              IF (stat_proc == 0) THEN ! average

                IF (lweighted) THEN
                  WHERE(mask_time(itime_start(i):itime_end(i)))
                    weightd(itime_start(i):itime_end(i)) = &
                     weightd(itime_start(i):itime_end(i)) * &
                     this%voldatid(i1,itime_start(i):itime_end(i),i3,itr,i5,i6)
                  END WHERE
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(weightd(itime_start(i):itime_end(i)), &
                   mask=mask_time(itime_start(i):itime_end(i)))

                ELSE
                  that%voldatid(i1,i,i3,1,i5,i6) = &
                   SUM(this%voldatid(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                   mask=mask_time(itime_start(i):itime_end(i)))/ &
                   COUNT(mask_time(itime_start(i):itime_end(i)))
                ENDIF

              ELSE IF (stat_proc == 2) THEN ! maximum

                that%voldatid(i1,i,i3,1,i5,i6) = &
                 MAXVAL( &
                 this%voldatid(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                 mask=mask_time(itime_start(i):itime_end(i)))

              ELSE IF (stat_proc == 3) THEN ! minimum

                that%voldatid(i1,i,i3,1,i5,i6) = &
                 MINVAL( &
                 this%voldatid(i1,itime_start(i):itime_end(i),i3,itr,i5,i6), &
                 mask=mask_time(itime_start(i):itime_end(i)))

              ENDIF
            ENDIF
          ENDDO timeloopd
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  DEALLOCATE(weightd)
ENDIF

DEALLOCATE(stepmsec, mask_time, itime_start, itime_end)

CALL makeother()

CONTAINS

SUBROUTINE makeother()
IF (PRESENT(other)) THEN ! create volume with the remaining data for further processing
  CALL vol7d_copy(this, other, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
   ltimerange=(this%timerange(:)%timerange /= tri))
ENDIF
END SUBROUTINE makeother

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
!!
!! Input volume may have any value of \a this%time_definition, and
!! that value will be conserved in the output volume.
SUBROUTINE vol7d_recompute_stat_proc_diff(this, that, stat_proc, step, full_steps, other)
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if provided and \a .TRUE., process only data having processing interval (p2) equal to a multiplier of \a step
TYPE(vol7d),INTENT(out),OPTIONAL :: other !< optional volume that, on exit, is going to contain the data that did not contribute to the statistical processing

INTEGER :: i1, i3, i5, i6, i, j, k, l, nitr, steps
INTEGER,POINTER :: map_tr(:,:,:,:,:), f(:)
LOGICAL,POINTER :: mask_timerange(:)
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
 nitr, that%time, that%timerange, map_tr, f, mask_timerange, &
 this%time_definition, full_steps)

! complete the definition of the empty output template
CALL vol7d_alloc(that, nana=0, nlevel=0, nnetwork=0)
CALL vol7d_alloc_vol(that)

! copy the timeranges already satisfying the requested step, if any
CALL vol7d_copy(this, v7dtmp, miss=.FALSE., sort=.FALSE., unique=.FALSE., &
 ltimerange=mask_timerange(:))
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
                      ELSE IF (stat_proc == 1) THEN ! cumulation, compute MAX(0.,)?
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
                      ELSE IF (stat_proc == 1) THEN ! cumulation, compute MAX(0.,)?
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

DEALLOCATE(map_tr, f, mask_timerange)
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
TYPE(cyclicdatetime) :: lcyclicdt
LOGICAL, ALLOCATABLE :: time_mask(:)
!TYPE(vol7d) :: v7dtmp2

CALL safe_start_stop(this, lstart, lstop, start, stopp)
IF (.NOT. c_e(lstart) .OR. .NOT. c_e(lstop)) RETURN

lcyclicdt=cyclicdatetime_miss
if (present(cyclicdt)) then
  if(c_e(cyclicdt)) lcyclicdt=cyclicdt
end if

CALL l4f_log(L4F_INFO, 'vol7d_filter_time: time interval '//TRIM(to_char(lstart))// &
 ' '//TRIM(to_char(lstop)))

ALLOCATE(time_mask(SIZE(this%time)))

!call display(lcyclicdt)
time_mask = this%time >= lstart .AND. this%time <= lstop .AND. this%time == lcyclicdt


if (present(step)) then
  if (c_e(step)) then
    time_mask=time_mask .AND. MOD(this%time - lstart, step) == timedelta_0 
  end if
end if


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

select case (type)

case("d")
  
  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatid(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatid(1,1,:,1,ind,1),that%dativar%d(ind)))
    that%level%l1 = int(that%voldatid(1,1,:,1,ind,1))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("r")

  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatir(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatir(1,1,:,1,ind,1),that%dativar%r(ind)))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("i")
    
  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatii(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatii(1,1,:,1,ind,1),that%dativar%i(ind)))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("b")

  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatib(1,1,:,1,ind,1)))
    that%level%level1 = 100
    that%level%l1 = int(realdat(that%voldatib(1,1,:,1,ind,1),that%dativar%b(ind)))
    that%level%level2 = imiss
    that%level%l2 = imiss
  end where

case("c")

  where (that%level%level1 == 105.and.that%level%level2 == 105 .and. c_e(that%voldatic(1,1,:,1,ind,1)))
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


SUBROUTINE vol7d_compute_NormalizedDensityIndex(this, that, perc_vals,cyclicdt,presentperc)
 
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be computed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the computed data
!TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
!TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
!TYPE(datetime),INTENT(in),OPTIONAL :: stopp  !< end of statistical processing interval
real,intent(in) :: perc_vals(:) !< percentile values to use in compute, between 0. and 100.
TYPE(cyclicdatetime),INTENT(in) :: cyclicdt !< cyclic date and time
real,optional :: presentperc !< percentual of data present for compute (default=0.3)

integer :: indana,indtime,indvar,indnetwork,indlevel ,indtimerange ,inddativarr, indattr
integer :: i,j,narea
TYPE(vol7d_var) ::  var
character(len=vol7d_ana_lenident) :: ident
character(len=1)            :: type
integer :: areav(size(this%ana))
logical,allocatable :: mask(:,:,:)
integer,allocatable :: area(:)
REAL, DIMENSION(:),allocatable ::  ndi,limbins
real ::  lpresentperc

lpresentperc=0.3
if (present(presentperc)) then
  if (c_e(presentperc)) then
    lpresentperc=presentperc
  end if
end if

allocate (ndi(size(perc_vals)-1),limbins(size(perc_vals)))
CALL init(that, time_definition=this%time_definition)
call init(var, btable="B01192")    ! MeteoDB station ID that here is the number of area

type=cmiss
indvar = index(this%anavar, var, type=type)
indnetwork=1

!if( ind /= 0 ) then
  select case (type)
  case("d")
    areav=integerdat(this%volanad(:,indvar,indnetwork),this%anavar%d(indvar))
  case("r")
    areav=integerdat(this%volanar(:,indvar,indnetwork),this%anavar%r(indvar))
  case("i")
    areav=integerdat(this%volanai(:,indvar,indnetwork),this%anavar%i(indvar))
  case("b")
    areav=integerdat(this%volanab(:,indvar,indnetwork),this%anavar%b(indvar))
  case("c")
    areav=integerdat(this%volanac(:,indvar,indnetwork),this%anavar%c(indvar))
  case default
    areav=imiss
  end select
!end if

narea=count_distinct(areav)
allocate(area(narea))
area=pack_distinct(areav,narea)
call vol7d_alloc(that,nana=narea*(size(perc_vals)-1))

do i=1,narea
  do j=1,size(perc_vals)-1
    write(ident,'("BOX",2i3.3)')area(i),nint(perc_vals(j))
    call init(that%ana((j-1)*narea+i),ident=ident,lat=0d0,lon=0d0)
    !area((j-1)*narea+i)=area(i)
    !percentile((j-1)*narea+i)=perc_vals(j)
  end do
end do

do i=1,size(that%ana)
  call display(that%ana(i))
end do

call vol7d_alloc(that,nlevel=size(this%level), ntimerange=size(this%timerange), &
 ndativarr=size(this%dativar%r), nnetwork=1,ntime=1,ndativarattrr=size(this%dativar%r),ndatiattrr=1)

that%level=this%level
that%timerange=this%timerange
that%dativar%r=this%dativar%r
that%dativarattr%r=that%dativar%r
call init(that%datiattr%r(1), btable="B33209")    ! NDI order number
that%time(1)=cyclicdatetime_to_conventional(cyclicdt,this%time(firsttrue(this%time == cyclicdt)))

call l4f_log(L4F_INFO,"vol7d_compute_percentile conventional datetime "//to_char(that%time(1)))
call init(that%network(1),name="qcclima-ndi")

call vol7d_alloc_vol(that,inivol=.true.)

allocate (mask(size(this%ana),size(this%time),size(this%network)))

indtime=1
indnetwork=1
indattr=1
do inddativarr=1,size(this%dativar%r)
  do indtimerange=1,size(this%timerange)
    do indlevel=1,size(this%level)            ! all stations, all times, all networks
      do i=1,narea

                                !this%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)

        !create mask only with valid time
        mask = spread(spread((this%time == cyclicdt ),1,size(this%ana)),3,size(this%network))
        !delete in mask different area
        do j=1, size(mask,1)
          if (areav(j) /= area(i)) mask(j,:,:) =.false.
        end do

        ! we want more than 30% data present

        !print*,"-------------------------------------------------------------"
        !print*,"Dati presenti:", count (mask .and. c_e(this%voldatir(:,:, indlevel, indtimerange, inddativarr,:)))
        !print*,"Dati attesi:", count (mask)

        if ((float(count (mask .and. c_e(this%voldatir(:,:, indlevel, indtimerange, inddativarr,:)))) / &
            float(count (mask))) < lpresentperc) cycle
        !print*,"compute"
        !print*,"-------------------------------------------------------------"

        call NormalizedDensityIndex (&
         pack(this%voldatir(:,:, indlevel, indtimerange, inddativarr,:), &
         mask=mask), &
         perc_vals, ndi, limbins) 

        print *,"------- ndi limbins -----------"
        call display( this%timerange(indtimerange))
        call display( this%level(indlevel))
        call display( this%dativar%r(inddativarr))
        print *, ndi
        print *, limbins

        do j=1,size(perc_vals)-1
          indana=((j-1)*narea+i)
          that%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)=&
           limbins(j)

          ! this is a special case where inddativarr = inddativarr becouse we have anly real variables and attributes
          that%voldatiattrr(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork,indattr)=&
           ndi(j)

        end do
      end do
    end do
  end do
end do

deallocate (ndi,limbins,mask,area)

end SUBROUTINE vol7d_compute_NormalizedDensityIndex

SUBROUTINE vol7d_compute_percentile(this, that, perc_vals,cyclicdt,presentperc)
 
TYPE(vol7d),INTENT(inout) :: this !< volume providing data to be computed, it is not modified by the method, apart from performing a \a vol7d_alloc_vol on it
TYPE(vol7d),INTENT(out) :: that !< output volume which will contain the computed data
!TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
!TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
!TYPE(datetime),INTENT(in),OPTIONAL :: stopp  !< end of statistical processing interval
real,intent(in) :: perc_vals(:) !< percentile values to use in compute, between 0. and 100.
TYPE(cyclicdatetime),INTENT(in) :: cyclicdt !< cyclic date and time
real,optional :: presentperc !< percentual of data present for compute (default='0.3)

integer :: indana,indtime,indvar,indnetwork,indlevel ,indtimerange ,inddativarr,i,j,narea
REAL, DIMENSION(:),allocatable ::  perc
TYPE(vol7d_var) ::  var
character(len=vol7d_ana_lenident) :: ident
character(len=1)            :: type
integer :: areav(size(this%ana))
logical,allocatable :: mask(:,:,:)
integer,allocatable :: area(:)
real :: lpresentperc

lpresentperc=0.3
if (present(presentperc)) then
  if (c_e(presentperc)) then
    lpresentperc=presentperc
  end if
end if

allocate (perc(size(perc_vals)))
CALL init(that, time_definition=this%time_definition)
call init(var, btable="B01192")    ! MeteoDB station ID that here is the number of area

type=cmiss
indvar = index(this%anavar, var, type=type)
indnetwork=1

!if( ind /= 0 ) then
  select case (type)
  case("d")
    areav=integerdat(this%volanad(:,indvar,indnetwork),this%anavar%d(indvar))
  case("r")
    areav=integerdat(this%volanar(:,indvar,indnetwork),this%anavar%r(indvar))
  case("i")
    areav=integerdat(this%volanai(:,indvar,indnetwork),this%anavar%i(indvar))
  case("b")
    areav=integerdat(this%volanab(:,indvar,indnetwork),this%anavar%b(indvar))
  case("c")
    areav=integerdat(this%volanac(:,indvar,indnetwork),this%anavar%c(indvar))
  case default
    areav=imiss
  end select
!end if

narea=count_distinct(areav)
allocate(area(narea))
area=pack_distinct(areav,narea)
call vol7d_alloc(that,nana=narea*size(perc_vals))

do i=1,narea
  do j=1,size(perc_vals)
    write(ident,'("BOX",2i3.3)')area(i),nint(perc_vals(j))
    call init(that%ana((j-1)*narea+i),ident=ident,lat=0d0,lon=0d0)
    !area((j-1)*narea+i)=area(i)
    !percentile((j-1)*narea+i)=perc_vals(j)
  end do
end do

do i=1,size(that%ana)
  call display(that%ana(i))
end do

call vol7d_alloc(that,nlevel=size(this%level), ntimerange=size(this%timerange), &
 ndativarr=size(this%dativar%r), nnetwork=1,ntime=1)

that%level=this%level
that%timerange=this%timerange
that%dativar%r=this%dativar%r
that%time(1)=cyclicdatetime_to_conventional(cyclicdt,this%time(firsttrue(this%time == cyclicdt)))
call l4f_log(L4F_INFO,"vol7d_compute_percentile conventional datetime "//to_char(that%time(1)))
call init(that%network(1),name="qcclima-perc")

call vol7d_alloc_vol(that,inivol=.true.)

allocate (mask(size(this%ana),size(this%time),size(this%network)))

indtime=1
indnetwork=1
do inddativarr=1,size(this%dativar%r)
  do indtimerange=1,size(this%timerange)
    do indlevel=1,size(this%level)            ! all stations, all times, all networks
      do i=1,narea

                                !this%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)

        !create mask only with valid time
        mask = spread(spread((this%time == cyclicdt ),1,size(this%ana)),3,size(this%network))
        !delete in mask different area
        do j=1, size(mask,1)
          if (areav(j) /= area(i)) mask(j,:,:) =.false.
        end do

        ! we want more than 30% data present
        if ((float(count & 
         (mask .and. c_e(this%voldatir(:,:, indlevel, indtimerange, inddativarr,:)))&
        ) / &
        float(count (mask))) < lpresentperc) cycle

        perc= stat_percentile (&
         pack(this%voldatir(:,:, indlevel, indtimerange, inddativarr,:), &
         mask=mask), &
         perc_vals)

        print *,"------- percentile -----------"
        call display( this%timerange(indtimerange))
        call display( this%level(indlevel))
        call display( this%dativar%r(inddativarr))
        print *, perc

        do j=1,size(perc_vals)
          indana=((j-1)*narea+i)
          that%voldatir(indana, indtime, indlevel, indtimerange, inddativarr, indnetwork)=&
           perc(j)
        end do
      end do
    end do
  end do
end do

deallocate (perc,mask,area)

end SUBROUTINE vol7d_compute_percentile

END MODULE vol7d_class_compute
