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
!> Extension of volgrid6d_class with methods for performing simple
!! statistical operations on entire volumes of data. This module
!! includes methods for performing operations such as (re)averaging
!! and (re)cumulation in time of entire volgrid6d_class::volgrid6d
!! objects.
!!
!! \ingroup volgrid6d
MODULE volgrid6d_class_compute
USE datetime_class
USE volgrid6d_class
USE grid_id_class
USE stat_proc_engine
USE simple_stat
IMPLICIT NONE

CONTAINS

!> General-purpose method for computing a statistical processing on
!! data in a volgrid6d object already processed with the same statistical
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
!!    volgrid6d_recompute_stat_proc_agg() and
!!    volgrid6d_recompute_stat_proc_diff() respectively; it is also
!!    possible to provide \a stat_proc_input \a /= \a stat_proc, but
!!    it has to be used with care.
!!
!!  - \a stat_proc_input = 0
!!    - \a stat_proc = 1
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
SUBROUTINE volgrid6d_compute_stat_proc(this, that, stat_proc_input, stat_proc, &
 step, start, full_steps, frac_valid, max_step, weighted, clone)
TYPE(volgrid6d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a volgrid6d_alloc_vol on it
TYPE(volgrid6d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc_input !< type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be processed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), data in output volume \a that will have a timerange of this type
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if \a .TRUE. cumulate only on intervals starting at a forecast time modulo \a step, default is to cumulate on all possible combinations of intervals
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
TYPE(timedelta),INTENT(in),OPTIONAL :: max_step ! maximum allowed distance in time between two single valid data within a dataset, for the dataset to be eligible for statistical processing
LOGICAL,INTENT(in),OPTIONAL :: weighted !< if provided and \c .TRUE., the statistical process is computed, if possible, by weighting every value with a weight proportional to its validity interval
LOGICAL , INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE. , clone the gaid's from \a this to \a that

INTEGER :: dtmax, dtstep


IF (stat_proc_input == 254) THEN
  CALL l4f_category_log(this%category, L4F_ERROR, &
   'statistical processing of instantaneous data not implemented for gridded fields')
  CALL raise_error()

ELSE IF (stat_proc == 254) THEN
  CALL l4f_category_log(this%category, L4F_ERROR, &
   'statistical processing to instantaneous data not implemented for gridded fields')
  CALL raise_error()

ELSE IF (stat_proc_input /= stat_proc) THEN
  IF ((stat_proc_input == 0 .AND. stat_proc == 1) .OR. &
   (stat_proc_input == 1 .AND. stat_proc == 0)) THEN
    CALL l4f_category_log(this%category, L4F_INFO, &
     'computing statistically processed data by integration/differentiation '// &
     t2c(stat_proc_input)//':'//t2c(stat_proc))
    CALL volgrid6d_compute_stat_proc_metamorph(this, that, stat_proc_input, &
     stat_proc, clone)
  ELSE
    CALL l4f_category_log(this%category, L4F_ERROR, &
   'statistical processing '//t2c(stat_proc_input)//':'//t2c(stat_proc)// &
   ' not implemented or does not make sense')
    CALL raise_error()
  ENDIF

ELSE IF (COUNT(this%timerange(:)%timerange == stat_proc) == 0) THEN
  CALL l4f_category_log(this%category, L4F_WARN, &
   'no timeranges of the desired statistical processing type '//t2c(stat_proc)//' available')
! return an empty volume, without signaling error
  CALL init(that)
  CALL volgrid6d_alloc_vol(that)

ELSE
! euristically determine whether aggregation or difference is more suitable
  dtmax = MAXVAL(this%timerange(:)%p2, &
   mask=(this%timerange(:)%timerange == stat_proc))
  CALL getval(step, asec=dtstep)

#ifdef DEBUG
  CALL l4f_category_log(this%category, L4F_DEBUG, &
   'stat_proc='//t2c(stat_proc)//' dtmax='//t2c(dtmax)//' dtstep='//t2c(dtstep))
#endif

  IF (dtstep < dtmax) THEN
    CALL l4f_category_log(this%category, L4F_INFO, &
     'recomputing statistically processed data by difference '// &
     t2c(stat_proc_input)//':'//t2c(stat_proc))
    CALL volgrid6d_recompute_stat_proc_diff(this, that, stat_proc, step, &
     full_steps, start, clone)
  ELSE
    CALL l4f_category_log(this%category, L4F_INFO, &
     'recomputing statistically processed data by aggregation '// &
     t2c(stat_proc_input)//':'//t2c(stat_proc))
    CALL volgrid6d_recompute_stat_proc_agg(this, that, stat_proc, step, start, &
     frac_valid, clone)
  ENDIF

ENDIF

END SUBROUTINE volgrid6d_compute_stat_proc


!> Specialized method for statistically processing a set of data
!! already processed with the same statistical processing, on a
!! different time interval.  This method performs statistical
!! processing by aggregation of shorter intervals.  Only data with
!! analysis/observation timerange are processed.
!!
!! The output \a that volgrid6d object contains elements from the original volume
!! \a this satisfying the conditions
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
SUBROUTINE volgrid6d_recompute_stat_proc_agg(this, that, stat_proc, step, start, frac_valid, clone)
TYPE(volgrid6d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a volgrid6d_alloc_vol on it
TYPE(volgrid6d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
TYPE(datetime),INTENT(in),OPTIONAL :: start !< start of statistical processing interval
REAL,INTENT(in),OPTIONAL :: frac_valid !< minimum fraction of valid data required for considering acceptable a recomputed value, default=1.
LOGICAL , INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE. , clone the gaid's from \a this to \a that

INTEGER :: tri
INTEGER i, j, k, l, n, n1, i3, i6
INTEGER,POINTER :: map_ttr(:,:,:)
INTEGER,POINTER :: dtratio(:)
REAL :: lfrac_valid
LOGICAL :: lclone
REAL,POINTER :: voldatiin(:,:), voldatiout(:,:)


NULLIFY(voldatiin, voldatiout)
tri = stat_proc
IF (PRESENT(frac_valid)) THEN
  lfrac_valid = frac_valid
ELSE
  lfrac_valid = 1.0
ENDIF

CALL init(that)
! be safe
CALL volgrid6d_alloc_vol(this)

! when volume is not decoded it is better to clone anyway to avoid
! overwriting fields
lclone = optio_log(clone) .OR. .NOT.ASSOCIATED(this%voldati)
! initialise the output volume
CALL init(that, griddim=this%griddim, time_definition=this%time_definition)
CALL volgrid6d_alloc(that, dim=this%griddim%dim, ntimerange=1, &
 nlevel=SIZE(this%level), nvar=SIZE(this%var), ini=.FALSE.)
that%level = this%level
that%var = this%var

CALL recompute_stat_proc_agg_common_exp(this%time, this%timerange, stat_proc, tri, &
 step, this%time_definition, that%time, that%timerange, map_ttr, dtratio, start)

CALL volgrid6d_alloc_vol(that, decode=ASSOCIATED(this%voldati))

do_otimerange: DO j = 1, SIZE(that%timerange)
  do_otime: DO i = 1, SIZE(that%time)

    DO n = 1, SIZE(dtratio)
      IF (dtratio(n) <= 0) CYCLE ! safety check

      DO i6 = 1, SIZE(this%var)
        DO i3 = 1, SIZE(this%level)
          CALL volgrid_get_vol_2d(that, i3, i, j, i6, voldatiout)

          n1 = 0
          DO l = 1, SIZE(this%timerange)
            DO k = 1, SIZE(this%time)
              IF (map_ttr(k,l,1) == i .AND. map_ttr(k,l,2) == j .AND. &
               map_ttr(k,l,3) == dtratio(n)) THEN ! useful combination
                CALL volgrid_get_vol_2d(this, i3, k, l, i6, voldatiin)

                IF (n1 == 0) THEN ! first time
                  voldatiout = voldatiin
                  IF (lclone) THEN
                    CALL copy(this%gaid(i3,k,l,i6), that%gaid(i3,i,j,i6))
                  ELSE
                    that%gaid(i3,i,j,i6) = this%gaid(i3,k,l,i6)
                  ENDIF

                ELSE ! second or more time
                  SELECT CASE(stat_proc)
                  CASE (0, 1) ! average, cumulation
                    WHERE(c_e(voldatiin(:,:)) .AND. c_e(voldatiout(:,:)))
                      voldatiout(:,:) = voldatiout(:,:) + voldatiin(:,:)
                    ELSEWHERE
                      voldatiout(:,:) = rmiss
                    END WHERE
                  CASE(2) ! maximum
                    WHERE(c_e(voldatiin(:,:)) .AND. c_e(voldatiout(:,:)))
                      voldatiout(:,:) = MAX(voldatiout(:,:), voldatiin(:,:))
                    ELSEWHERE
                      voldatiout(:,:) = rmiss
                    END WHERE
                  CASE(3) ! minimum
                    WHERE(c_e(voldatiin(:,:)) .AND. c_e(voldatiout(:,:)))
                      voldatiout(:,:) = MIN(voldatiout(:,:), voldatiin(:,:))
                    ELSEWHERE
                      voldatiout(:,:) = rmiss
                    END WHERE
                  END SELECT

                ENDIF ! first time
                n1 = n1 + 1
              ENDIF ! useful combination
            ENDDO
          ENDDO
          IF (REAL(n1)/REAL(dtratio(n)) >= lfrac_valid) THEN ! success
!          IF (n1 == dtratio(n)) THEN ! success
            IF (stat_proc == 0) THEN ! average
              WHERE(c_e(voldatiout(:,:)))
                voldatiout(:,:) = voldatiout(:,:)/n1
              END WHERE
            ENDIF
            CALL volgrid_set_vol_2d(that, i3, i, j, i6, voldatiout)
          ENDIF

        ENDDO ! level
      ENDDO ! var
    ENDDO ! dtratio
  ENDDO do_otime
ENDDO do_otimerange

DEALLOCATE(dtratio, map_ttr)

END SUBROUTINE volgrid6d_recompute_stat_proc_agg


!> Specialized method for statistically processing a set of data
!! already processed with the same statistical processing, on a
!! different time interval.  This method performs statistical
!! processing by difference of different intervals.  Data with both
!! analysis/observation or forecast timerange are processed.
!!
!! The output \a that volgrid6d object contains elements from the original volume
!! \a this satisfying the conditions
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
SUBROUTINE volgrid6d_recompute_stat_proc_diff(this, that, stat_proc, step, full_steps, start, clone)
TYPE(volgrid6d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a volgrid6d_alloc_vol on it
TYPE(volgrid6d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if provided and \a .TRUE., process only data having processing interval (p2) equal to a multiple of \a step
TYPE(datetime),INTENT(in),OPTIONAL :: start !< if provided, together with \a full_steps, processes data on intervals starting at \a start +- an integer amount of \a step intervals
LOGICAL,INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE. , clone the gaid's from \a this to \a that
INTEGER :: i3, i4, i6, i, j, k, l, nitr, steps
INTEGER,POINTER :: map_tr(:,:,:,:,:), f(:)
REAL,POINTER :: voldatiin1(:,:), voldatiin2(:,:), voldatiout(:,:)
LOGICAL,POINTER :: mask_timerange(:)
LOGICAL :: lclone


! be safe
CALL volgrid6d_alloc_vol(this)
! when volume is not decoded it is better to clone anyway to avoid
! overwriting fields
lclone = optio_log(clone) .OR. .NOT.ASSOCIATED(this%voldati)
! initialise the output volume
CALL init(that, griddim=this%griddim, time_definition=this%time_definition)
CALL volgrid6d_alloc(that, dim=this%griddim%dim, &
 nlevel=SIZE(this%level), nvar=SIZE(this%var), ini=.FALSE.)
that%level = this%level
that%var = this%var

! compute length of cumulation step in seconds
CALL getval(step, asec=steps)

! compute the statistical processing relations, output time and
! timerange are defined here
CALL recompute_stat_proc_diff_common(this%time, this%timerange, stat_proc, step, &
 nitr, that%time, that%timerange, map_tr, f, mask_timerange, &
 this%time_definition, full_steps, start)

! complete the definition of the output volume
CALL volgrid6d_alloc_vol(that, decode=ASSOCIATED(this%voldati))
! allocate workspace once
IF (.NOT.ASSOCIATED(that%voldati)) THEN
  ALLOCATE(voldatiin1(this%griddim%dim%nx, this%griddim%dim%ny), &
   voldatiin2(this%griddim%dim%nx, this%griddim%dim%ny), &
   voldatiout(this%griddim%dim%nx, this%griddim%dim%ny))
ENDIF

! copy the timeranges already satisfying the requested step, if any
DO i = 1, SIZE(mask_timerange)
  IF (mask_timerange(i)) THEN
    k = firsttrue(that%timerange(:) == this%timerange(i))
#ifdef DEBUG
    CALL l4f_category_log(this%category, L4F_INFO, &
     'volgrid6d_recompute_stat_proc_diff, good timerange: '//t2c(i)// &
     '->'//t2c(k))
#endif
    IF (k > 0) THEN

      DO i6 = 1, SIZE(this%var)
        DO i4 = 1, SIZE(this%time)
          l = firsttrue(that%time(:) == this%time(i4))
          IF (l > 0) THEN
            DO i3 = 1, SIZE(this%level)
              IF (c_e(this%gaid(i3,i4,i,i6))) THEN
                IF (lclone) THEN
                  CALL copy(this%gaid(i3,i4,i,i6), that%gaid(i3,l,k,i6))
                ELSE
                  that%gaid(i3,l,k,i6) = this%gaid(i3,i4,i,i6)
                ENDIF
                IF (ASSOCIATED(that%voldati)) THEN
                  that%voldati(:,:,i3,l,k,i6) = this%voldati(:,:,i3,i4,i,i6)
                ELSE
                  CALL volgrid_get_vol_2d(this, i3, i4, i, i6, voldatiout)
                  CALL volgrid_set_vol_2d(that, i3, l, k, i6, voldatiout)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO

    ENDIF
  ENDIF
ENDDO

! compute statistical processing
DO l = 1, SIZE(this%time)
  DO k = 1, nitr
    DO j = 1, SIZE(this%time)
      DO i = 1, nitr
        IF (c_e(map_tr(i,j,k,l,1))) THEN
          DO i6 = 1, SIZE(this%var)
            DO i3 = 1, SIZE(this%level)

              IF (c_e(this%gaid(i3,j,f(i),i6)) .AND. &
               c_e(this%gaid(i3,l,f(k),i6))) THEN
! take the gaid from the second time/timerange contributing to the
! result (l,f(k))
                IF (lclone) THEN
                  CALL copy(this%gaid(i3,l,f(k),i6), &
                   that%gaid(i3,map_tr(i,j,k,l,1),map_tr(i,j,k,l,2),i6))
                ELSE
                  that%gaid(i3,map_tr(i,j,k,l,1),map_tr(i,j,k,l,2),i6) = &
                   this%gaid(i3,l,f(k),i6)
                ENDIF

! get/set 2d sections API is used
                CALL volgrid_get_vol_2d(this, i3, l, f(k), i6, voldatiin1)
                CALL volgrid_get_vol_2d(this, i3, j, f(i), i6, voldatiin2)
                IF (ASSOCIATED(that%voldati)) &
                 CALL volgrid_get_vol_2d(that, i3, &
                 map_tr(i,j,k,l,1), map_tr(i,j,k,l,2), i6, voldatiout)

                IF (stat_proc == 0) THEN ! average
                  WHERE(c_e(voldatiin1(:,:)) .AND. c_e(voldatiin2(:,:)))
                    voldatiout(:,:) = &
                     (voldatiin1(:,:)*this%timerange(f(k))%p2 - &
                     voldatiin2(:,:)*this%timerange(f(i))%p2)/ &
                     steps
                  ELSEWHERE
                    voldatiout(:,:) = rmiss
                  END WHERE
                ELSE IF (stat_proc == 1) THEN ! cumulation, compute MAX(0.,)?
                  WHERE(c_e(voldatiin1(:,:)) .AND. c_e(voldatiin2(:,:)))
                    voldatiout(:,:) = voldatiin1(:,:) - voldatiin2(:,:)
                  ELSEWHERE
                    voldatiout(:,:) = rmiss
                  END WHERE
                ENDIF

                CALL volgrid_set_vol_2d(that, i3, &
                 map_tr(i,j,k,l,1), map_tr(i,j,k,l,2), i6, voldatiout)

              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

IF (.NOT.ASSOCIATED(that%voldati)) THEN
  DEALLOCATE(voldatiin1, voldatiin2, voldatiout)
ENDIF

END SUBROUTINE volgrid6d_recompute_stat_proc_diff


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
!! The output \a that volgrid6d object contains elements from the
!! original volume \a this satisfying the conditions
!!  - timerange (vol7d_timerange_class::vol7d_timerange::timerange)
!!    of type \a stat_proc_input (0 or 1)
!!  - any p1 (analysis/observation or forecast)
!!  - p2 &gt; 0 (processing interval non null, non instantaneous data)
!!    and equal to a multiplier of \a step if \a full_steps is \c .TRUE.
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
SUBROUTINE volgrid6d_compute_stat_proc_metamorph(this, that, stat_proc_input, stat_proc, clone)
TYPE(volgrid6d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a volgrid6d_alloc_vol on it
TYPE(volgrid6d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc_input !< type of statistical processing of data that has to be processed (from grib2 table), only data having timerange of this type will be processed, the actual statistical processing performed and which will appear in the output volume, is however determined by \a stat_proc argument
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), data in output volume \a that will have a timerange of this type
LOGICAL , INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE. , clone the gaid's from \a this to \a that

INTEGER i, j, n, i3, i4, i6
INTEGER,POINTER :: map_tr(:), map_trc(:,:), count_trc(:,:)
REAL,POINTER :: voldatiin(:,:), voldatiout(:,:)
REAL,ALLOCATABLE :: int_ratio(:)
LOGICAL :: lclone

NULLIFY(voldatiin, voldatiout)

! be safe
CALL volgrid6d_alloc_vol(this)
! when volume is not decoded it is better to clone anyway to avoid
! overwriting fields
lclone = optio_log(clone) .OR. .NOT.ASSOCIATED(this%voldati)

IF (.NOT.((stat_proc_input == 0 .AND. stat_proc == 1) .OR. &
 (stat_proc_input == 1 .AND. stat_proc == 0))) THEN

  CALL l4f_category_log(this%category, L4F_WARN, &
   'compute_stat_proc_metamorph, can only be applied to average->accumulated timerange and viceversa')
! return an empty volume, without signaling error
  CALL init(that)
  CALL volgrid6d_alloc_vol(that)
  RETURN
ENDIF

! initialise the output volume
CALL init(that, griddim=this%griddim, time_definition=this%time_definition)
CALL volgrid6d_alloc(that, dim=this%griddim%dim, ntime=SIZE(this%time), &
 nlevel=SIZE(this%level), nvar=SIZE(this%var), ini=.FALSE.)
that%time = this%time
that%level = this%level
that%var = this%var

CALL compute_stat_proc_metamorph_common(stat_proc_input, this%timerange, stat_proc, &
 that%timerange, map_tr)

! complete the definition of the output volume
CALL volgrid6d_alloc_vol(that, decode=ASSOCIATED(this%voldati))

IF (stat_proc == 0) THEN ! average -> integral
  int_ratio = 1./REAL(that%timerange(:)%p2)
ELSE ! cumulation
  int_ratio = REAL(that%timerange(:)%p2)
ENDIF

DO i6 = 1, SIZE(this%var)
  DO j = 1, SIZE(map_tr)
    DO i4 = 1, SIZE(that%time)
      DO i3 = 1, SIZE(this%level)

        IF (lclone) THEN
          CALL copy(this%gaid(i3,i4,map_tr(j),i6), that%gaid(i3,i4,j,i6))
        ELSE
          that%gaid(i3,i4,map_tr(j),i6) = this%gaid(i3,i4,j,i6)
        ENDIF
        CALL volgrid_get_vol_2d(this, i3, i4, map_tr(j), i6, voldatiin)
        CALL volgrid_get_vol_2d(that, i3, i4, j, i6, voldatiout)
        WHERE (c_e(voldatiin))
          voldatiout = voldatiin*int_ratio(j)
        ELSEWHERE
          voldatiout = rmiss
        END WHERE
        CALL volgrid_set_vol_2d(that, i3, i4, j, i6, voldatiout)
      ENDDO
    ENDDO
  ENDDO
ENDDO


END SUBROUTINE volgrid6d_compute_stat_proc_metamorph

END MODULE volgrid6d_class_compute
