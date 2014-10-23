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
! Programma di test per il module simple_stat
! migliorare a piacimento
PROGRAM simple_stat_test
USE missing_values
USE simple_stat
IMPLICIT NONE

REAL :: s1(6)=(/4.,6.,7.,8.,9.,11./), s2(6)=(/11.,9.,8.,7.,6.,4./)
REAL :: s3(6)=(/rmiss,6.,7.,8.,9.,rmiss/), s4(6)=(/rmiss,9.,rmiss,rmiss,6.,rmiss/)
REAL :: val1, val2, valv(5)
REAL :: opt1, opt2, opt3, opt4
INTEGER,ALLOCATABLE :: bin(:)
REAL, PARAMETER :: epsy=1.0E-20

PRINT*,'=== Testing simple_stat module ==='

PRINT*,'Checking average'
val1 = stat_average(s1)
val2 = stat_average(s2)
PRINT*,'averages: ',val1,val2
IF (ABS(val1-7.5) > epsy) CALL EXIT(1)
IF (ABS(val2-7.5) > epsy) CALL EXIT(1)

PRINT*,'Checking average with missing data'
val1 = stat_average(s3)
val2 = stat_average(s4)
PRINT*,'averages: ',val1,val2
IF (ABS(val1-7.5) > epsy) CALL EXIT(1)
IF (ABS(val2-7.5) > epsy) CALL EXIT(1)

PRINT*,'Checking variances'
val1 = stat_variance(s1, opt1)
val2 = stat_variance(s1, opt2)
PRINT*,'variances: ',val1,val2
PRINT*,'averages: ',opt1,opt2

PRINT*,'Checking variances with missing data'
val1 = stat_variance(s3, opt1)
val2 = stat_variance(s4, opt2)
PRINT*,'variances: ',val1,val2
PRINT*,'averages: ',opt1,opt2

PRINT*,'Checking linear correlation'
val1 = stat_linear_corr(s1, s2, opt1, opt2, opt3, opt4)
PRINT*,'correlation: ',val1
PRINT*,'averages and variances: ', opt1, opt2, opt3, opt4
IF (ABS(val1+1.) > epsy) CALL EXIT(1)

PRINT*,'Checking linear correlation with missing data'
val1 = stat_linear_corr(s3, s4, opt1, opt2, opt3, opt4)
PRINT*,'correlation: ',val1
PRINT*,'averages and variances: ', opt1, opt2, opt3, opt4
IF (ABS(val1+1.) > epsy) CALL EXIT(1)

PRINT*,'Checking linear regression'
CALL stat_linear_regression(s1, s2, val1, val2)
PRINT*,'regression coefficients: ',val1,val2
IF (ABS(val1-15.) > epsy .OR. ABS(val2+1.) > epsy) CALL EXIT(1)

PRINT*,'Checking linear regression with missing data'
CALL stat_linear_regression(s3, s4, val1, val2)
PRINT*,'regression coefficients: ',val1,val2
IF (ABS(val1-15.) > epsy .OR. ABS(val2+1.) > epsy) CALL EXIT(1)

PRINT*,'Checking percentiles'
valv = stat_percentile(s2, (/0.,25.,50.,75.,100./))
PRINT*,'percentiles: ',valv
IF (ABS(valv(1)-s2(6)) > epsy) CALL EXIT(1)
IF (ABS(valv(5)-s2(1)) > epsy) CALL EXIT(1)

PRINT*,'Checking percentiles with missing data'
valv = stat_percentile(s3, (/0.,25.,50.,75.,100./))
PRINT*,'percentiles: ',valv
IF (ABS(valv(1)-s3(2)) > epsy) CALL EXIT(1)
IF (ABS(valv(5)-s3(5)) > epsy) CALL EXIT(1)

PRINT*,'Checking binning'
CALL stat_bin(s1, bin, 4, 3.5, 11.5)
PRINT*,'bin population: ',bin
IF (ANY(bin /= (/1,2,2,1/))) CALL EXIT(1)
CALL stat_bin(s2, bin, 4, 3.5, 11.5)
PRINT*,'bin population: ',bin
IF (ANY(bin /= (/1,2,2,1/))) CALL EXIT(1)

PRINT*,'Checking binning with missing data'
CALL stat_bin(s3, bin, 4, 3.5, 11.5)
PRINT*,'bin population: ',bin
IF (ANY(bin /= (/0,2,2,0/))) CALL EXIT(1)
CALL stat_bin(s4, bin, 4, 3.5, 11.5)
PRINT*,'bin population: ',bin
IF (ANY(bin /= (/0,1,1,0/))) CALL EXIT(1)

PRINT*,'Checking mode'
val1 = stat_mode_histogram(s1, 4, 3.5, 11.5)
PRINT*,'mode: ',val1
IF (ABS(val1-6.5) > epsy) CALL EXIT(1)
val1 = stat_mode_histogram(s2, 4, 3.5, 11.5)
PRINT*,'mode: ',val1
IF (ABS(val1-6.5) > epsy) CALL EXIT(1)

PRINT*,'Checking mode with missing data'
val1 = stat_mode_histogram(s3, 4, 3.5, 11.5)
PRINT*,'mode: ',val1
IF (ABS(val1-6.5) > epsy) CALL EXIT(1)
val1 = stat_mode_histogram(s4, 4, 3.5, 11.5)
PRINT*,'mode: ',val1
IF (ABS(val1-6.5) > epsy) CALL EXIT(1)

END PROGRAM simple_stat_test
