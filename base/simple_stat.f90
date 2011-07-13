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
!> Module for basic statistical computations taking into account missing data.
!! The functions provided are average, variance, linear correlation
!! coefficient and percentile for a population. Missing data are
!! excluded from computations and from weighing. Both \a REAL and \a
!! DOUBLE \a PRECISION functions are available.
!!
!! \ingroup base
MODULE simple_stat
USE missing_values
IMPLICIT NONE

!> Compute the average of the random variable provided,
!! taking into account missing data. The result is of type \a REAL or
!! \a DOUBLE \a PRECISION according to the type of \a sample.
!!
!! REAL or DOUBLE PRECISION FUNCTION stat_average()
!! \param sample(:) REAL,INTENT(in) or DOUBLE PRECISION,INTENT(in) the variable to be averaged
!! \param mask(:) LOGICAL,OPTIONAL,INTENT(in) additional mask to be and'ed with missing values
INTERFACE stat_average
  MODULE PROCEDURE stat_averager, stat_averaged
END INTERFACE

!> Compute the variance of the random variable provided, taking into
!! account missing data.  The average can be returned as an optional
!! parameter since it is computed anyway. The result is of type \a REAL or
!! \a DOUBLE \a PRECISION according to the type of \a sample.
!!
!! REAL or DOUBLE PRECISION FUNCTION stat_variance()
!! \param sample(:) REAL,INTENT(in) or DOUBLE PRECISION,INTENT(in) the variable for which variance has to be computed
!! \param average REAL,OPTIONAL,INTENT(out) the average of the variable can optionally be returned
!! \param mask(:) LOGICAL,OPTIONAL,INTENT(in) additional mask to be and'ed with missing values
INTERFACE stat_variance
  MODULE PROCEDURE stat_variancer, stat_varianced
END INTERFACE

!> Compute the standard deviation of the random variable provided, taking into
!! account missing data.  The average can be returned as an optional
!! parameter since it is computed anyway. The result is of type \a REAL or
!! \a DOUBLE \a PRECISION according to the type of \a sample.
!!
!! REAL or DOUBLE PRECISION FUNCTION stat_stddev()
!! \param sample(:) REAL,INTENT(in) or DOUBLE PRECISION,INTENT(in) the variable for which standard deviation has to be computed
!! \param average REAL,OPTIONAL,INTENT(out) the average of the variable can optionally be returned
!! \param mask(:) LOGICAL,OPTIONAL,INTENT(in) additional mask to be and'ed with missing values
INTERFACE stat_stddev
  MODULE PROCEDURE stat_stddevr, stat_stddevd
END INTERFACE

!> Compute the linear correlation coefficient between the two random
!! variables provided, taking into account missing data. Data are
!! considered missing when at least one variable has a missing value.
!! The average and the variance of each variable can be returned as
!! optional parameters since they are computed anyway. The result is
!! of type \a REAL or \a DOUBLE \a PRECISION according to the type of
!! \a sample1 and \a sample2.
!!
!! REAL or DOUBLE PRECISION FUNCTION stat_linear_corr()
!! \param sample1(:) REAL,INTENT(in) or DOUBLE PRECISION,INTENT(in) the first variable
!! \param sample2(:) REAL,INTENT(in) or DOUBLE PRECISION,INTENT(in) the second variable
!! \param average1 REAL,OPTIONAL,INTENT(out) the average of the first variable can optionally be returned
!! \param average2 REAL,OPTIONAL,INTENT(out) the average of the second variable can optionally be returned
!! \param variance1 REAL,OPTIONAL,INTENT(out) the variance of the first variable can optionally be returned
!! \param variance2 REAL,OPTIONAL,INTENT(out) the variance of the second variable can optionally be returned
!! \param mask(:) LOGICAL,OPTIONAL,INTENT(in) additional mask to be and'ed with missing values
INTERFACE stat_linear_corr
  MODULE PROCEDURE stat_linear_corrr, stat_linear_corrd
END INTERFACE

!> Compute a set of percentiles for a random variable.
!! The result is a 1-d array of the same size as the array of
!! percentile values provided. Percentiles are computed by linear
!! interpolation. The result is of type \a REAL or \a DOUBLE \a
!! PRECISION according to the type of \a sample.
!!
!! REAL or DOUBLE PRECISION FUNCTION stat_percentile()
!! \param sample(:) REAL,INTENT(in) or DOUBLE PRECISION,INTENT(in) the variable for which percentiles have to be computed
!! \param perc_vals(:) REAL,INTENT(in) or DOUBLE PRECISION,INTENT(in) the percentiles values to be computed, between 0. and 100.
!! \param mask(:) LOGICAL,OPTIONAL,INTENT(in) additional mask to be and'ed with missing values
INTERFACE stat_percentile
  MODULE PROCEDURE stat_percentiler, stat_percentiled
END INTERFACE

PRIVATE
PUBLIC stat_average, stat_variance, stat_stddev, stat_linear_corr, stat_percentile

CONTAINS


FUNCTION stat_averager(sample, mask, nomiss) RESULT(average)
REAL,INTENT(in) :: sample(:)
LOGICAL,OPTIONAL,INTENT(in) :: mask(:)
LOGICAL,OPTIONAL,INTENT(in) :: nomiss ! informs that the sample does not contain missing data (for speedup, unused)

REAL :: average

INTEGER :: sample_count
LOGICAL :: sample_mask(SIZE(sample))

sample_mask = (sample /= rmiss)
IF (PRESENT(mask)) sample_mask = sample_mask .AND. mask
sample_count = COUNT(sample_mask)

IF (sample_count > 0) THEN
! compute average
  average = SUM(sample, mask=sample_mask)/sample_count
ELSE
  average = rmiss
ENDIF

END FUNCTION stat_averager


FUNCTION stat_averaged(sample, mask, nomiss) RESULT(average)
DOUBLE PRECISION,INTENT(in) :: sample(:)
LOGICAL,OPTIONAL,INTENT(in) :: mask(:)
LOGICAL,OPTIONAL,INTENT(in) :: nomiss ! informs that the sample does not contain missing data (for speedup, unused)

DOUBLE PRECISION :: average

INTEGER :: sample_count
LOGICAL :: sample_mask(SIZE(sample))

sample_mask = (sample /= dmiss)
IF (PRESENT(mask)) sample_mask = sample_mask .AND. mask
sample_count = COUNT(sample_mask)

IF (sample_count > 0) THEN
! compute average
  average = SUM(sample, mask=sample_mask)/sample_count
ELSE
  average = dmiss
ENDIF

END FUNCTION stat_averaged


FUNCTION stat_variancer(sample, average, mask, nomiss) RESULT(variance)
REAL,INTENT(in) :: sample(:)
REAL,OPTIONAL,INTENT(out) :: average
LOGICAL,OPTIONAL,INTENT(in) :: mask(:)
LOGICAL,OPTIONAL,INTENT(in) :: nomiss

REAL :: variance

REAL :: laverage
INTEGER :: sample_count, i
LOGICAL :: sample_mask(SIZE(sample))

sample_mask = (sample /= rmiss)
IF (PRESENT(mask)) sample_mask = sample_mask .AND. mask
sample_count = COUNT(sample_mask)

IF (sample_count > 0) THEN
! compute average
  laverage = SUM(sample, mask=sample_mask)/sample_count
  IF (PRESENT(average)) average = laverage
! compute sum of squares and variance
  variance = 0.
  DO i = 1, SIZE(sample)
    IF (sample_mask(i)) variance = variance + sample(i)**2
  ENDDO
!  variance = SUM(sample**2, mask=sample_mask)/sample_count - laverage
  variance = variance/sample_count - laverage**2
!  IF (sample_count > 1) THEN ! do we need this correction?
!    variance = variance*REAL(sample_count,kind=KIND(variance))/ &
!     REAL(sample_count-1,kind=KIND(variance))
!  ENDIF
ELSE
  IF (PRESENT(average)) average = rmiss
  variance = rmiss
ENDIF

END FUNCTION stat_variancer


FUNCTION stat_varianced(sample, average, mask, nomiss) RESULT(variance)
DOUBLE PRECISION,INTENT(in) :: sample(:)
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: average
LOGICAL,OPTIONAL,INTENT(in) :: mask(:)
LOGICAL,OPTIONAL,INTENT(in) :: nomiss

DOUBLE PRECISION :: variance

DOUBLE PRECISION :: laverage
INTEGER :: sample_count, i
LOGICAL :: sample_mask(SIZE(sample))

sample_mask = (sample /= dmiss)
IF (PRESENT(mask)) sample_mask = sample_mask .AND. mask
sample_count = COUNT(sample_mask)

IF (sample_count > 0) THEN
! compute average
  laverage = SUM(sample, mask=sample_mask)/sample_count
  IF (PRESENT(average)) average = laverage
! compute sum of squares and variance
  variance = 0.
  DO i = 1, SIZE(sample)
    IF (sample_mask(i)) variance = variance + sample(i)**2
  ENDDO
!  variance = SUM(sample**2, mask=sample_mask)/sample_count - laverage
  variance = variance/sample_count - laverage**2
!  IF (sample_count > 1) THEN ! do we need this correction?
!    variance = variance*REAL(sample_count,kind=KIND(variance))/ &
!     REAL(sample_count-1,kind=KIND(variance))
!  ENDIF
ELSE
  IF (PRESENT(average)) average = dmiss
  variance = dmiss
ENDIF

END FUNCTION stat_varianced


FUNCTION stat_stddevr(sample, average, mask, nomiss) RESULT(stddev)
REAL,INTENT(in) :: sample(:)
REAL,OPTIONAL,INTENT(out) :: average
LOGICAL,OPTIONAL,INTENT(in) :: mask(:)
LOGICAL,OPTIONAL,INTENT(in) :: nomiss

REAL :: stddev

stddev = stat_variance(sample, average, mask, nomiss)
IF (c_e(stddev) .AND. stddev > 0.0) stddev = SQRT(stddev)

END FUNCTION stat_stddevr


FUNCTION stat_stddevd(sample, average, mask, nomiss) RESULT(stddev)
DOUBLE PRECISION,INTENT(in) :: sample(:)
DOUBLE PRECISION,OPTIONAL,INTENT(out) :: average
LOGICAL,OPTIONAL,INTENT(in) :: mask(:)
LOGICAL,OPTIONAL,INTENT(in) :: nomiss

DOUBLE PRECISION :: stddev

stddev = stat_variance(sample, average, mask, nomiss)
IF (c_e(stddev) .AND. stddev > 0.0) stddev = SQRT(stddev)

END FUNCTION stat_stddevd


FUNCTION stat_linear_corrr(sample1, sample2, average1, average2, &
 variance1, variance2, mask, nomiss) RESULT(linear_corr)
REAL,INTENT(in) :: sample1(:)
REAL,INTENT(in) :: sample2(:)
REAL,OPTIONAL,INTENT(out) :: average1
REAL,OPTIONAL,INTENT(out) :: average2
REAL,OPTIONAL,INTENT(out) :: variance1
REAL,OPTIONAL,INTENT(out) :: variance2
LOGICAL,OPTIONAL,INTENT(in) :: mask(:)
LOGICAL,OPTIONAL,INTENT(in) :: nomiss

REAL :: linear_corr

REAL :: laverage1, laverage2, lvariance1, lvariance2
INTEGER :: sample_count, i
LOGICAL :: sample_mask(SIZE(sample1))

IF (SIZE(sample1) /= SIZE(sample2)) THEN
  IF (PRESENT(average1)) average1 = rmiss
  IF (PRESENT(average2)) average2 = rmiss
  IF (PRESENT(variance1)) variance1 = rmiss
  IF (PRESENT(variance2)) variance2 = rmiss
  linear_corr = rmiss
  RETURN
ENDIF

sample_mask = (sample1 /= rmiss .AND. sample2 /= rmiss)
IF (PRESENT(mask)) sample_mask = sample_mask .AND. mask
sample_count = COUNT(sample_mask)
IF (sample_count > 0) THEN
! compute averages
  laverage1 = SUM(sample1, mask=sample_mask)/sample_count
  laverage2 = SUM(sample2, mask=sample_mask)/sample_count
  IF (PRESENT(average1)) average1 = laverage1
  IF (PRESENT(average2)) average2 = laverage2
! compute sum of squares and variances
  lvariance1 = 0.
  lvariance2 = 0.
  DO i = 1, SIZE(sample1)
    IF (sample_mask(i))THEN
      lvariance1 = lvariance1 + sample1(i)**2
      lvariance2 = lvariance2 + sample2(i)**2
    ENDIF
  ENDDO
  lvariance1 =  lvariance1/sample_count - laverage1**2
  lvariance2 =  lvariance2/sample_count - laverage2**2
  IF (PRESENT(variance1)) variance1 = lvariance1
  IF (PRESENT(variance2)) variance2 = lvariance2
! compute correlation
  linear_corr = 0.
  DO i = 1, SIZE(sample1)
    IF (sample_mask(i)) linear_corr = linear_corr + sample1(i)*sample2(i)
  ENDDO
  linear_corr = (linear_corr/sample_count - laverage1*laverage2) / SQRT(lvariance1*lvariance2)
!  linear_corr = (SUM(sample1*sample2, mask=sample_mask)/sample_count - laverage1*laverage2) / &
!   SQRT(lvariance1*lvariance2)
ELSE
  IF (PRESENT(average1)) average1 = rmiss
  IF (PRESENT(average2)) average2 = rmiss
  IF (PRESENT(variance1)) variance1 = rmiss
  IF (PRESENT(variance2)) variance2 = rmiss
  linear_corr = rmiss
ENDIF

END FUNCTION stat_linear_corrr


FUNCTION stat_linear_corrd(sample1, sample2, average1, average2, &
 variance1, variance2, mask, nomiss) RESULT(linear_corr)
DOUBLE PRECISION, INTENT(in) :: sample1(:)
DOUBLE PRECISION, INTENT(in) :: sample2(:)
DOUBLE PRECISION, OPTIONAL, INTENT(out) :: average1
DOUBLE PRECISION, OPTIONAL, INTENT(out) :: average2
DOUBLE PRECISION, OPTIONAL, INTENT(out) :: variance1
DOUBLE PRECISION, OPTIONAL, INTENT(out) :: variance2
LOGICAL, OPTIONAL, INTENT(in) :: mask(:)
LOGICAL, OPTIONAL, INTENT(in) :: nomiss

DOUBLE PRECISION :: linear_corr

DOUBLE PRECISION :: laverage1, laverage2, lvariance1, lvariance2
INTEGER :: sample_count, i
LOGICAL :: sample_mask(SIZE(sample1))

IF (SIZE(sample1) /= SIZE(sample2)) THEN
  IF (PRESENT(average1)) average1 = dmiss
  IF (PRESENT(average2)) average2 = dmiss
  IF (PRESENT(variance1)) variance1 = dmiss
  IF (PRESENT(variance2)) variance2 = dmiss
  linear_corr = dmiss
  RETURN
ENDIF

sample_mask = (sample1 /= dmiss .AND. sample2 /= dmiss)
IF (PRESENT(mask)) sample_mask = sample_mask .AND. mask
sample_count = COUNT(sample_mask)
IF (sample_count > 0) THEN
! compute averages
  laverage1 = SUM(sample1, mask=sample_mask)/sample_count
  laverage2 = SUM(sample2, mask=sample_mask)/sample_count
  IF (PRESENT(average1)) average1 = laverage1
  IF (PRESENT(average2)) average2 = laverage2
! compute sum of squares and variances
  lvariance1 = 0.
  lvariance2 = 0.
  DO i = 1, SIZE(sample1)
    IF (sample_mask(i))THEN
      lvariance1 = lvariance1 + sample1(i)**2
      lvariance2 = lvariance2 + sample2(i)**2
    ENDIF
  ENDDO
  lvariance1 =  lvariance1/sample_count - laverage1**2
  lvariance2 =  lvariance2/sample_count - laverage2**2
  IF (PRESENT(variance1)) variance1 = lvariance1
  IF (PRESENT(variance2)) variance2 = lvariance2
! compute correlation
  linear_corr = 0.
  DO i = 1, SIZE(sample1)
    IF (sample_mask(i)) linear_corr = linear_corr + sample1(i)*sample2(i)
  ENDDO
  linear_corr = (linear_corr/sample_count - laverage1*laverage2) / SQRT(lvariance1*lvariance2)
!  linear_corr = (SUM(sample1*sample2, mask=sample_mask)/sample_count - laverage1*laverage2) / &
!   SQRT(lvariance1*lvariance2)
ELSE
  IF (PRESENT(average1)) average1 = dmiss
  IF (PRESENT(average2)) average2 = dmiss
  IF (PRESENT(variance1)) variance1 = dmiss
  IF (PRESENT(variance2)) variance2 = dmiss
  linear_corr = dmiss
ENDIF

END FUNCTION stat_linear_corrd


FUNCTION stat_percentiler(sample, perc_vals, mask, nomiss) RESULT(percentile)
REAL,INTENT(in) :: sample(:)
REAL,INTENT(in) :: perc_vals(:)
LOGICAL,OPTIONAL,INTENT(in) :: mask(:)
LOGICAL,OPTIONAL,INTENT(in) :: nomiss

REAL :: percentile(SIZE(perc_vals))

REAL :: lsample(SIZE(sample)), v, rindex
INTEGER :: sample_count, i, j, iindex
LOGICAL :: sample_mask(SIZE(sample))

percentile(:) = rmiss
sample_mask = (sample /= rmiss)
IF (PRESENT(mask)) sample_mask = sample_mask .AND. mask
sample_count = COUNT(sample_mask)
IF (sample_count == 0) RETURN ! particular case
lsample(1:sample_count) = PACK(sample, mask=sample_mask)
IF (sample_count == 1) THEN ! other particular case
  percentile(:) = lsample(1)
  RETURN
ENDIF

! sort
DO j = 2, sample_count
  v = lsample(j)
  DO i = j-1, 1, -1
    IF (v >= lsample(i)) EXIT
    lsample(i+1) = lsample(i)
  ENDDO
  lsample(i+1) = v
ENDDO

DO j = 1, SIZE(perc_vals)
  IF (perc_vals(j) >= 0. .AND. perc_vals(j) <= 100.) THEN
! compute real index of requested percentile in sample
    rindex = REAL(sample_count-1, kind=KIND(rindex))*perc_vals(j)/100.+1.
! compute integer index of previous element in sample, beware of corner cases
    iindex = MIN(MAX(INT(rindex), 1), sample_count-1)
! compute linearly interpolated percentile
    percentile(j) = lsample(iindex)*(REAL(iindex+1, kind=KIND(rindex))-rindex) &
     + lsample(iindex+1)*(rindex-REAL(iindex, kind=KIND(rindex)))
  ENDIF
ENDDO

END FUNCTION stat_percentiler


FUNCTION stat_percentiled(sample, perc_vals, mask, nomiss) RESULT(percentile) !, bin
DOUBLE PRECISION, INTENT(in) :: sample(:)
DOUBLE PRECISION, INTENT(in) :: perc_vals(:)
LOGICAL, OPTIONAL, INTENT(in) :: mask(:)
LOGICAL, OPTIONAL, INTENT(in) :: nomiss

DOUBLE PRECISION :: percentile(SIZE(perc_vals))

DOUBLE PRECISION :: lsample(SIZE(sample)), v, rindex
INTEGER :: sample_count, i, j, iindex, oindex
LOGICAL :: sample_mask(SIZE(sample))

percentile(:) = dmiss
sample_mask = (sample /= dmiss)
IF (PRESENT(mask)) sample_mask = sample_mask .AND. mask
sample_count = COUNT(sample_mask)
IF (sample_count == 0) RETURN ! particular case
lsample(1:sample_count) = PACK(sample, mask=sample_mask)
IF (sample_count == 1) THEN ! other particular case
  percentile(:) = lsample(1)
  RETURN
ENDIF

! sort
DO j = 2, sample_count
  v = lsample(j)
  DO i = j-1, 1, -1
    IF (v >= lsample(i)) EXIT
    lsample(i+1) = lsample(i)
  ENDDO
  lsample(i+1) = v
ENDDO

DO j = 1, SIZE(perc_vals)
  IF (perc_vals(j) >= 0.D0 .AND. perc_vals(j) <= 100.D0) THEN
! compute real index of requested percentile in sample
    rindex = REAL(sample_count-1, kind=KIND(rindex))*perc_vals(j)/100.+1.
! compute integer index of previous element in sample, beware of corner cases
    iindex = MIN(MAX(INT(rindex), 1), sample_count-1)
! compute linearly interpolated percentile
    percentile(j) = lsample(iindex)*(REAL(iindex+1, kind=KIND(rindex))-rindex) &
     + lsample(iindex+1)*(rindex-REAL(iindex, kind=KIND(rindex)))
    oindex = iindex
  ELSE
    percentile(j) = dmiss
  ENDIF
ENDDO

END FUNCTION stat_percentiled


END MODULE simple_stat
