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
SUBROUTINE volgrid6d_recompute_stat_proc_diff(this, that, stat_proc, step, full_steps, clone)
TYPE(volgrid6d),INTENT(inout) :: this !< volume providing data to be recomputed, it is not modified by the method, apart from performing a \a volgrid6d_alloc_vol on it
TYPE(volgrid6d),INTENT(out) :: that !< output volume which will contain the recomputed data
INTEGER,INTENT(in) :: stat_proc !< type of statistical processing to be recomputed (from grib2 table), only data having timerange of this type will be recomputed and will appear in the output volume
TYPE(timedelta),INTENT(in) :: step !< length of the step over which the statistical processing is performed
LOGICAL,INTENT(in),OPTIONAL :: full_steps !< if provided and \a .TRUE., process only data having processing interval (p2) equal to a multiplier of \a step
LOGICAL , INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE. , clone the gaid's from \a this to \a that
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
 this%time_definition, full_steps)

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
    CALL l4f_log(L4F_INFO, &
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
                  that%gaid(i3,i4,k,i6) = this%gaid(i3,i4,i,i6)
                ENDIF
                IF (ASSOCIATED(that%voldati)) THEN
                  that%voldati(:,:,i3,i4,k,i6) = this%voldati(:,:,i3,l,i,i6)
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

END MODULE volgrid6d_class_compute
