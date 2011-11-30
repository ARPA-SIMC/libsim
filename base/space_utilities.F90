! Copyright (C) 2011  ARPA-SIM <urpsim@smr.arpa.emr.it>
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

! This module contains contng derived from NCL - UNIX Version 6.0.0
!	$Id: contng.f,v 1.4 2008-07-27 00:16:57 haley Exp $
!                                                                      
!                Copyright (C)  2000
!        University Corporation for Atmospheric Research
!                All Rights Reserved
!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are
!met:
!
!Neither the names of NCAR's Computational and Information Systems
!Laboratory, the University Corporation for Atmospheric Research, nor
!the names of its contributors may be used to endorse or promote
!products derived from this Software without specific prior written
!permission.
!
!Redistributions of source code must retain the above copyright
!notice, this list of conditions, and the disclaimer below.
!
!Redistributions in binary form must reproduce the above copyright
!notice, this list of conditions, and the disclaimer below in the
!documentation and/or other materials provided with the distribution.
!
!THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!EXPRESS OR IMPLIED, INCLUDING, BUT NOT LIMITED TO THE WARRANTIES OF
!MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!NONINFRINGEMENT. IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT
!HOLDERS BE LIABLE FOR ANY CLAIM, INDIRECT, INCIDENTAL, SPECIAL,
!EXEMPLARY, OR CONSEQUENTIAL DAMAGES OR OTHER LIABILITY, WHETHER IN AN
!ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
!SOFTWARE.

#include "config.h"

!> This module defines objects and methods for managing data volumes
!! on rectangular georeferenced grids.  The data are accomodated in a
!! multi-dimensional array with 6 predefined dimensions. Different
!! geographic coordinates and projections are supported, mainly
!! inspired by grib coding standards. The \a volgrid6d object contains
!! information and data on an homogeneous grid definition, while
!! different grids are managed as arrays of \a volgrid6d objects.
!! Every object contains also an identificator of the grid (\a grid_id
!! object), carrying information about the driver used or which has to
!! be used for import/export from/to file.  With the help of \a
!! gridinfo_def class, data can be imported and exported to the
!! supported formats, mainly grib1 and grib2 through grib_api and many
!! GIS-style formats through gdal.
!!
!! Simple example program \include example_vg6d_3.f90
!!
!!\ingroup base

module space_utilities

use log4fortran
use char_utilities

implicit none

type :: triangles
  integer,pointer ::  ipt(:) => null(), ipl(:) => null()
  integer :: nt=imiss,nl=imiss
end type triangles

type :: xy
  double precision :: x,y
end type xy

!> Distructor for triangles.
!! delete triangles
INTERFACE delete
  MODULE PROCEDURE triangles_delete
END INTERFACE

INTERFACE triangles_compute
  MODULE PROCEDURE triangles_compute_r, triangles_compute_d, triangles_compute_c
END INTERFACE

private
public triangles, triangles_new, delete, triangles_compute, xy


contains

!> initialize triangles
function triangles_new(ndp) result(this)
type(triangles) :: this !< triangles to initialize
integer,intent(in) :: ndp !< number of station to triangulate

! those are done by type definition
!this%nt=imiss
!this%nl=imiss
!nullify(this%ipt,this%ipl)

if (c_e(ndp) .and. ndp >= 3)then
  allocate(this%ipt(6*ndp-15), this%ipl(6*ndp))
else
  this%nt=0
  this%nl=0
end if
return
end function triangles_new


!> delete triangles
subroutine triangles_delete(this)
type(triangles) :: this !< triangles to delete

if (associated(this%ipt)) deallocate(this%ipt)
if (associated(this%ipl)) deallocate(this%ipl)

this%nt=imiss
this%nl=imiss

end subroutine triangles_delete


integer function triangles_compute_r (XD,YD,tri)
real,intent(in)  ::  XD(:) !< ARRAY OF DIMENSION NDP CONTAINING THE X COORDINATES OF THE DATA POINTS
real,intent(in)  ::  YD(:) !< ARRAY OF DIMENSION NDP CONTAINING THE Y COORDINATES OF THE DATA POINTS.
type (triangles),intent(inout) :: tri !< computed triangles
type (xy) :: co(size(xd))

if (tri%nt /= 0) then
  co%x=dble(XD)
  co%y=dble(YD)
  triangles_compute_r = CONTNG_simc (co,tri%NT,tri%IPT,tri%NL,tri%IPL)
end if
end function triangles_compute_r

integer function triangles_compute_d (XD,YD,tri)
double precision,intent(in)  ::  XD(:) !< ARRAY OF DIMENSION NDP CONTAINING THE X COORDINATES OF THE DATA POINTS
double precision,intent(in)  ::  YD(:) !< ARRAY OF DIMENSION NDP CONTAINING THE Y COORDINATES OF THE DATA POINTS.
type (triangles),intent(inout) :: tri !< computed triangles
type (xy) :: co(size(xd))

if (tri%nt /= 0) then
  co%x=XD
  co%y=YD
  triangles_compute_d = CONTNG_simc (co,tri%NT,tri%IPT,tri%NL,tri%IPL)
end if
end function triangles_compute_d

integer function triangles_compute_c (co,tri)
type (xy),intent(in) :: co(:)
type (triangles),intent(inout) :: tri !< computed triangles

if (tri%nt /= 0) then
  triangles_compute_c = CONTNG_simc (co,tri%NT,tri%IPT,tri%NL,tri%IPL)
end if
end function triangles_compute_c


!> THIS SUBROUTINE PERFORMS TRIANGULATION.
!! IT DIVIDES THE X-Y PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA
!! POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE
!! BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS
!! CORRESPONDING TO THE BORDER LINE SEGMENTS.
!! AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE
!! ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS
!! OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE,
!! LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE.
!!
!! Return 0 if all right
!! return 1 if IDENTICAL INPUT DATA POINTS FOUND
!! return 2 if ALL DATA ARE COLLINEAR DATA POINTS
integer function CONTNG_simc (co,NT,IPT,NL,IPL)

type (xy), intent(in)  ::  co(:) !< ARRAY OF DIMENSION NDP CONTAINING THE COORDINATES OF THE DATA POINTS
integer, intent(out) :: NT !< NUMBER OF TRIANGLES
integer, intent(out) :: NL !< NUMBER OF BORDER LINE SEGMENTS
!> ARRAY OF DIMENSION 6*NDP-15, WHERE THE POINT
!!           NUMBERS OF THE VERTEXES OF THE (IT)TH TRIANGLE
!!           ARE TO BE STORED AS THE (3*IT-2)ND, (3*IT-1)ST,
!!           AND (3*IT)TH ELEMENTS, IT=1,2,...,NT
integer,intent(out) :: IPT(:)
!> ARRAY OF DIMENSION 6*NDP, WHERE THE POINT
!!           NUMBERS OF THE END POINTS OF THE (IL)TH BORDER
!!           LINE SEGMENT AND ITS RESPECTIVE TRIANGLE NUMBER
!!           ARE TO BE STORED AS THE (3*IL-2)ND, (3*IL-1)ST,
!!           AND (3*IL)TH ELEMENTS, IL=1,2,..., NL.
integer,intent(out) :: IPL(:)

!!$C THE internal PARAMETERS ARE
!!$C     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED
!!$C           INTERNALLY AS A WORK AREA,
!!$C     IWP = INTEGER ARRAY OF DIMENSION NDP USED
!!$C           INTERNALLY AS A WORK AREA,
!!$C     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A
!!$C           WORK AREA.
integer :: IWL(18*size(co)), IWP(size(co))

double precision  :: WK(size(co))
integer    ::   ITF(2), NREP=100
real       :: RATIO=1.0E-6
double precision :: AR,ARMN,ARMX,DSQ12,DSQI,DSQMN,DSQMX,DX,DX21,DXMN,DXMX,DY,DY21,DYMN,DYMX
double precision :: X1,Y1
integer :: ilf,IP,IP1,IP1P1,IP2,IP3,IPL1,IPL2,IPLJ1,IPLJ2,IPMN1,IPMN2,IPT1,IPT2,IPT3
INTEGER :: IPTI,IPTI1,IPTI2,IREP,IT,IT1T3,IT2T3,ITS,ITT3
INTEGER :: ILFT2,ITT3R,JLT3,JP,JP1,JP2,JP2T3,JP3T3,JPC,JPMN,JPMX,JWL,JWL1
INTEGER :: JWL1MN,NDP,NDPM1,NL0,NLF,NLFC,NLFT2,NLN,NLNT3,NLT3,NSH,NSHT3,NT0,NTF
INTEGER :: NTT3,NTT3P3
logical :: err

integer ::i,mloc(size(co)-1),mlocall(1),mlocv(1)
type(xy) :: dmp

 !
 ! PRELIMINARY PROCESSING
 !

NT = imiss
NL = imiss

CONTNG_simc=0
ndp=size(co)
NDPM1 = NDP-1
 !
 ! DETERMINES THE CLOSEST PAIR OF DATA POINTS AND THEIR MIDPOINT.
 !

call l4f_log(L4F_DEBUG,"start triangulation")

do i=1,size(co)-1
  mlocv=minloc(vdsqf(co(i),co(i+1:)))+i
  mloc(i)=mlocv(1)
end do

mlocall=minloc((/(vdsqf(co(i),co(mloc(i))),i=1,size(mloc))/))

DSQMN = vdsqf(co(mlocall(1)),co(mloc(mlocall(1))))
IPMN1 = mlocall(1)
IPMN2 = mloc(mlocall(1))

call l4f_log(L4F_DEBUG,"end triangulation closest pair")
!!$print *, DSQMN, IPMN1, IPMN2

IF (DSQMN == 0.) then
   !
   !  ERROR, IDENTICAL INPUT DATA POINTS
   !
  call l4f_log(L4F_ERROR,"CONTNG-IDENTICAL INPUT DATA POINTS FOUND")
  CONTNG_simc=1
  RETURN
end IF

!!$call l4f_log(L4F_DEBUG,"start your")
!!$
!!$DSQMN = DSQF(XD(1),YD(1),XD(2),YD(2))
!!$IPMN1 = 1
!!$IPMN2 = 2
!!$DO  IP1=1,NDPM1
!!$  X1 = XD(IP1)
!!$  Y1 = YD(IP1)
!!$  IP1P1 = IP1+1
!!$  DO  IP2=IP1P1,NDP
!!$    DSQI = DSQF(X1,Y1,XD(IP2),YD(IP2))
!!$
!!$    IF (DSQI == 0.) then
!!$ !
!!$ !  ERROR, IDENTICAL INPUT DATA POINTS
!!$ !
!!$      call l4f_log(L4F_ERROR,"CONTNG-IDENTICAL INPUT DATA POINTS FOUND AT "//t2c(ip1)//" AND"//t2c(ip2))
!!$      CONTNG_simc=1
!!$      RETURN
!!$    end IF
!!$
!!$    IF (DSQI .GE. DSQMN) CYCLE
!!$    DSQMN = DSQI
!!$    IPMN1 = IP1
!!$    IPMN2 = IP2
!!$  end DO
!!$end DO
!!$
!!$call l4f_log(L4F_DEBUG,"end your")
!!$print *, DSQMN, IPMN1, IPMN2

DSQ12 = DSQMN
DMP%x = (co(IPMN1)%x+co(IPMN2)%x)/2.0
DMP%y = (co(IPMN1)%y+co(IPMN2)%y)/2.0
 !
 ! SORTS THE OTHER (NDP-2) DATA POINTS IN ASCENDING ORDER OF
 ! DISTANCE FROM THE MIDPOINT AND STORES THE SORTED DATA POINT
 ! NUMBERS IN THE IWP ARRAY.
 !
JP1 = 2
DO  IP1=1,NDP
  IF (IP1.EQ.IPMN1 .OR. IP1.EQ.IPMN2) cycle
  JP1 = JP1+1
  IWP(JP1) = IP1
  WK(JP1) = vDSQF(DMP,co(IP1))
end DO
DO  JP1=3,NDPM1
  DSQMN = WK(JP1)
  JPMN = JP1
  DO  JP2=JP1,NDP
    IF (WK(JP2) .GE. DSQMN)  cycle
    DSQMN = WK(JP2)
    JPMN = JP2
  end DO
  ITS = IWP(JP1)
  IWP(JP1) = IWP(JPMN)
  IWP(JPMN) = ITS
  WK(JPMN) = WK(JP1)
end DO

call l4f_log(L4F_DEBUG,"end triangulation sort")

 !
 ! IF NECESSARY, MODIFIES THE ORDERING IN SUCH A WAY THAT THE
 ! FIRST THREE DATA POINTS ARE NOT COLLINEAR.
 !
AR = DSQ12*RATIO
X1 = co(IPMN1)%x
Y1 = co(IPMN1)%y
DX21 = co(IPMN2)%x-X1
DY21 = co(IPMN2)%y-Y1

err=.true.
DO  JP=3,NDP
  IP = IWP(JP)
  IF (ABS((co(IP)%y-Y1)*DX21-(co(IP)%x-X1)*DY21) .GT. AR) then
    err=.false.
    exit
  end IF
end DO
if (err) then
  call l4f_log(L4F_DEBUG,"CONTNG - ALL COLLINEAR DATA POINTS")
  CONTNG_simc=2
  return
end if
IF (JP /= 3) then
  JPMX = JP
  JP = JPMX+1
  DO  JPC=4,JPMX
    JP = JP-1
    IWP(JP) = IWP(JP-1)
  end DO
  IWP(3) = IP
end IF
call l4f_log(L4F_DEBUG,"end triangulation collinear")

 !
 ! FORMS THE FIRST TRIANGLE.  STORES POINT NUMBERS OF THE VER-
 ! TEXES OF THE TRIANGLE IN THE IPT ARRAY, AND STORES POINT NUM-
 ! BERS OF THE BORDER LINE SEGMENTS AND THE TRIANGLE NUMBER IN
 ! THE IPL ARRAY.
 !
IP1 = IPMN1
IP2 = IPMN2
IP3 = IWP(3)
IF (SIDE(co(IP1),co(IP2),co(IP3)) < 10.0) then
  IP1 = IPMN2
  IP2 = IPMN1
end IF

NT0 = 1
NTT3 = 3
IPT(1) = IP1
IPT(2) = IP2
IPT(3) = IP3
NL0 = 3
NLT3 = 9
IPL(1) = IP1
IPL(2) = IP2
IPL(3) = 1
IPL(4) = IP2
IPL(5) = IP3
IPL(6) = 1
IPL(7) = IP3
IPL(8) = IP1
IPL(9) = 1

call l4f_log(L4F_DEBUG,"end triangulation first triangle")

 !
 ! ADDS THE REMAINING (NDP-3) DATA POINTS, ONE BY ONE.
 !
L400 : DO  JP1=4,NDP
  IP1 = IWP(JP1)
  X1 = co(IP1)%x
  Y1 = co(IP1)%y
 !
 ! - DETERMINES THE VISIBLE BORDER LINE SEGMENTS.
 !
  IP2 = IPL(1)
  JPMN = 1
  DXMN = co(IP2)%x-X1
  DYMN = co(IP2)%y-Y1
  DSQMN = DXMN**2+DYMN**2
  ARMN = DSQMN*RATIO
  JPMX = 1
  DXMX = DXMN
  DYMX = DYMN
  DSQMX = DSQMN
  ARMX = ARMN
  DO  JP2=2,NL0
    IP2 = IPL(3*JP2-2)
    DX = co(IP2)%x-X1
    DY = co(IP2)%y-Y1
    AR = DY*DXMN-DX*DYMN
    IF (AR <= ARMN) then
      DSQI = DX**2+DY**2
      IF (AR.GE.(-ARMN) .AND. DSQI.GE.DSQMN) GO TO  230
      JPMN = JP2
      DXMN = DX
      DYMN = DY
      DSQMN = DSQI
      ARMN = DSQMN*RATIO
    end IF
230 AR = DY*DXMX-DX*DYMX
    IF (AR .LT. (-ARMX)) cycle
    DSQI = DX**2+DY**2
    IF (AR.LE.ARMX .AND. DSQI.GE.DSQMX) cycle
    JPMX = JP2
    DXMX = DX
    DYMX = DY
    DSQMX = DSQI
    ARMX = DSQMX*RATIO
  end DO
  IF (JPMX .LT. JPMN) JPMX = JPMX+NL0
  NSH = JPMN-1
  IF (NSH > 0) then
 !
 ! - SHIFTS (ROTATES) THE IPL ARRAY TO HAVE THE INVISIBLE BORDER
 ! - LINE SEGMENTS CONTAINED IN THE FIRST PART OF THE IPL ARRAY.
 !
    NSHT3 = NSH*3
    DO  JP2T3=3,NSHT3,3
      JP3T3 = JP2T3+NLT3
      IPL(JP3T3-2) = IPL(JP2T3-2)
      IPL(JP3T3-1) = IPL(JP2T3-1)
      IPL(JP3T3) = IPL(JP2T3)
    end DO
    DO  JP2T3=3,NLT3,3
      JP3T3 = JP2T3+NSHT3
      IPL(JP2T3-2) = IPL(JP3T3-2)
      IPL(JP2T3-1) = IPL(JP3T3-1)
      IPL(JP2T3) = IPL(JP3T3)
    end DO
    JPMX = JPMX-NSH
 !
 ! - ADDS TRIANGLES TO THE IPT ARRAY, UPDATES BORDER LINE
 ! - SEGMENTS IN THE IPL ARRAY, AND SETS FLAGS FOR THE BORDER
 ! - LINE SEGMENTS TO BE REEXAMINED IN THE IWL ARRAY.
 !
  end IF

  JWL = 0
  L310 : DO JP2=JPMX,NL0
    JP2T3 = JP2*3
    IPL1 = IPL(JP2T3-2)
    IPL2 = IPL(JP2T3-1)
    IT = IPL(JP2T3)
    !
    ! - - ADDS A TRIANGLE TO THE IPT ARRAY.
    !
    NT0 = NT0+1
    NTT3 = NTT3+3
    IPT(NTT3-2) = IPL2
    IPT(NTT3-1) = IPL1
    IPT(NTT3) = IP1
    !
    ! - - UPDATES BORDER LINE SEGMENTS IN THE IPL ARRAY.
    !
    IF (JP2 == JPMX) then
      IPL(JP2T3-1) = IP1
      IPL(JP2T3) = NT0
    end IF
    IF (JP2 == NL0) then
      NLN = JPMX+1
      NLNT3 = NLN*3
      IPL(NLNT3-2) = IP1
      IPL(NLNT3-1) = IPL(1)
      IPL(NLNT3) = NT0
    end IF
 !
 ! - - DETERMINES THE VERTEX THAT DOES NOT LIE ON THE BORDER
 ! - - LINE SEGMENTS.
 !
    ITT3 = IT*3
    IPTI = IPT(ITT3-2)
    IF (IPTI.NE.IPL1 .AND. IPTI.NE.IPL2) GO TO  300
    IPTI = IPT(ITT3-1)
    IF (IPTI.NE.IPL1 .AND. IPTI.NE.IPL2) GO TO  300
    IPTI = IPT(ITT3)
 !
 ! - - CHECKS IF THE EXCHANGE IS NECESSARY.
 !
300 IF (CONXCH_simc(co%X,co%Y,IP1,IPTI,IPL1,IPL2) .EQ. 0) cycle L310
 !
 ! - - MODIFIES THE IPT ARRAY WHEN NECESSARY.
 !
    IPT(ITT3-2) = IPTI
    IPT(ITT3-1) = IPL1
    IPT(ITT3) = IP1
    IPT(NTT3-1) = IPTI
    IF (JP2 .EQ. JPMX) IPL(JP2T3) = IT
    IF (JP2.EQ.NL0 .AND. IPL(3).EQ.IT) IPL(3) = NT0
 !
 ! - - SETS FLAGS IN THE IWL ARRAY.
 !
    JWL = JWL+4
    IWL(JWL-3) = IPL1
    IWL(JWL-2) = IPTI
    IWL(JWL-1) = IPTI
    IWL(JWL) = IPL2
  end DO L310
  NL0 = NLN
  NLT3 = NLNT3
  NLF = JWL/2
  IF (NLF .EQ. 0) cycle L400
 !
 ! - IMPROVES TRIANGULATION.
 !
  NTT3P3 = NTT3+3
  DO IREP=1,NREP
    L370 : DO ILF=1,NLF
      ILFT2 = ILF*2
      IPL1 = IWL(ILFT2-1)
      IPL2 = IWL(ILFT2)
 !
 ! - - LOCATES IN THE IPT ARRAY TWO TRIANGLES ON BOTH SIDES OF
 ! - - THE FLAGGED LINE SEGMENT.
 !
      NTF = 0
      DO  ITT3R=3,NTT3,3
        ITT3 = NTT3P3-ITT3R
        IPT1 = IPT(ITT3-2)
        IPT2 = IPT(ITT3-1)
        IPT3 = IPT(ITT3)
        IF (IPL1.NE.IPT1 .AND. IPL1.NE.IPT2 .AND. IPL1.NE.IPT3) cycle
        IF (IPL2.NE.IPT1 .AND. IPL2.NE.IPT2 .AND. IPL2.NE.IPT3) cycle
        NTF = NTF+1
        ITF(NTF) = ITT3/3
        IF (NTF .EQ. 2) GO TO  330
      end DO
      IF (NTF .LT. 2) cycle  L370
 !
 ! - - DETERMINES THE VERTEXES OF THE TRIANGLES THAT DO NOT LIE
 ! - - ON THE LINE SEGMENT.
 !
330   IT1T3 = ITF(1)*3
      IPTI1 = IPT(IT1T3-2)
      IF (IPTI1.NE.IPL1 .AND. IPTI1.NE.IPL2) GO TO  340
      IPTI1 = IPT(IT1T3-1)
      IF (IPTI1.NE.IPL1 .AND. IPTI1.NE.IPL2) GO TO  340
      IPTI1 = IPT(IT1T3)
340   IT2T3 = ITF(2)*3
      IPTI2 = IPT(IT2T3-2)
      IF (IPTI2.NE.IPL1 .AND. IPTI2.NE.IPL2) GO TO  350
      IPTI2 = IPT(IT2T3-1)
      IF (IPTI2.NE.IPL1 .AND. IPTI2.NE.IPL2) GO TO  350
      IPTI2 = IPT(IT2T3)
 !
 ! - - CHECKS IF THE EXCHANGE IS NECESSARY.
 !
350   IF (CONXCH_simc(co%X,co%Y,IPTI1,IPTI2,IPL1,IPL2) .EQ. 0) cycle L370
 !
 ! - - MODIFIES THE IPT ARRAY WHEN NECESSARY.
 !
      IPT(IT1T3-2) = IPTI1
      IPT(IT1T3-1) = IPTI2
      IPT(IT1T3) = IPL1
      IPT(IT2T3-2) = IPTI2
      IPT(IT2T3-1) = IPTI1
      IPT(IT2T3) = IPL2
 !
 ! - - SETS NEW FLAGS.
 !
      JWL = JWL+8
      IWL(JWL-7) = IPL1
      IWL(JWL-6) = IPTI1
      IWL(JWL-5) = IPTI1
      IWL(JWL-4) = IPL2
      IWL(JWL-3) = IPL2
      IWL(JWL-2) = IPTI2
      IWL(JWL-1) = IPTI2
      IWL(JWL) = IPL1
      DO  JLT3=3,NLT3,3
        IPLJ1 = IPL(JLT3-2)
        IPLJ2 = IPL(JLT3-1)
        IF ((IPLJ1.EQ.IPL1 .AND. IPLJ2.EQ.IPTI2) .OR. &
         (IPLJ2.EQ.IPL1 .AND. IPLJ1.EQ.IPTI2))  IPL(JLT3) = ITF(1)
        IF ((IPLJ1.EQ.IPL2 .AND. IPLJ2.EQ.IPTI1) .OR. &
         (IPLJ2.EQ.IPL2 .AND. IPLJ1.EQ.IPTI1))  IPL(JLT3) = ITF(2)
      end DO
    end DO L370
    NLFC = NLF
    NLF = JWL/2
    IF (NLF .EQ. NLFC) cycle L400
 !
 ! - - RESETS THE IWL ARRAY FOR THE NEXT ROUND.
 !
    JWL = 0
    JWL1MN = (NLFC+1)*2
    NLFT2 = NLF*2
    DO   JWL1=JWL1MN,NLFT2,2
      JWL = JWL+2
      IWL(JWL-1) = IWL(JWL1-1)
      IWL(JWL) = IWL(JWL1)
    end DO
    NLF = JWL/2
  end DO
end DO L400

call l4f_log(L4F_DEBUG,"end triangulation appending")

 !
 ! REARRANGE THE IPT ARRAY SO THAT THE VERTEXES OF EACH TRIANGLE
 ! ARE LISTED COUNTER-CLOCKWISE.
 !
DO  ITT3=3,NTT3,3
  IP1 = IPT(ITT3-2)
  IP2 = IPT(ITT3-1)
  IP3 = IPT(ITT3)
  IF (SIDE(co(IP1),co(IP2),co(IP3)) .GE. 10.0) cycle
  IPT(ITT3-2) = IP2
  IPT(ITT3-1) = IP1
end DO
call l4f_log(L4F_DEBUG,"end triangulation rearranging")

NT = NT0
NL = NL0

call l4f_log(L4F_DEBUG,"end triangulation")

RETURN

contains

!!$double precision function     DSQF(U1,V1,U2,V2)
!!$double precision,intent(in) :: U1,V1,U2,V2
!!$
!!$DSQF = (U2-U1)**2+(V2-V1)**2
!!$end function DSQF


elemental double precision function vDSQF(co1,co2)
type(xy),intent(in) :: co1,co2

vDSQF = (co2%x-co1%x)**2+(co2%y-co1%y)**2
if (vdsqf == 0.d0) vdsqf = huge(vdsqf)
end function VDSQF


!!$double precision function  SIDE(U1,V1,U2,V2,U3,V3)
!!$double precision,intent(in):: U1,V1,U2,V2,U3,V3
!!$
!!$SIDE = (V3-V1)*(U2-U1)-(U3-U1)*(V2-V1)
!!$end function SIDE

double precision function  SIDE(co1,co2,co3)
type(xy),intent(in):: co1,co2,co3

SIDE = (co3%y-co1%y)*(co2%x-co1%x)-(co3%x-co1%x)*(co2%y-co1%y)
end function SIDE

end function CONTNG_simc


!> THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO
!! TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION
!! BY C. L. LAWSON.
!! THIS FUNCTION RETURNS A VALUE 1 (ONE) WHEN AN EXCHANGE IS
!! NEEDED, AND 0 (ZERO) OTHERWISE.
INTEGER FUNCTION CONXCH_simc (X,Y,I1,I2,I3,I4)

double precision,intent(in) ::  X(:), Y(:) !< ARRAYS CONTAINING THE COORDINATES OF THE DATA POINTS
!> POINT NUMBERS OF FOUR POINTS P1,P2,P3, AND P4 THAT FORM A QUADRILATERAL
!!                   WITH P3 AND P4 CONNECTED DIADONALLY.
integer,intent(in) :: I1,I2,I3,I4 

double precision ::  X0(4), Y0(4)
double precision :: C2SQ,C1SQ,A3SQ,B2SQ,B3SQ,A1SQ,A4SQ,B1SQ,B4SQ,A2SQ,C4SQ,C3SQ
integer :: IDX
double precision :: S1SQ,S2SQ,S3SQ,S4SQ,U1,U2,U3,U4

EQUIVALENCE     (C2SQ,C1SQ),(A3SQ,B2SQ),(B3SQ,A1SQ),(A4SQ,B1SQ), &
 (B4SQ,A2SQ),(C4SQ,C3SQ)

X0(1) = X(I1)
Y0(1) = Y(I1)
X0(2) = X(I2)
Y0(2) = Y(I2)
X0(3) = X(I3)
Y0(3) = Y(I3)
X0(4) = X(I4)
Y0(4) = Y(I4)
IDX = 0
U3 = (Y0(2)-Y0(3))*(X0(1)-X0(3))-(X0(2)-X0(3))*(Y0(1)-Y0(3))
U4 = (Y0(1)-Y0(4))*(X0(2)-X0(4))-(X0(1)-X0(4))*(Y0(2)-Y0(4))
IF (U3*U4 > 0.0) then
  U1 = (Y0(3)-Y0(1))*(X0(4)-X0(1))-(X0(3)-X0(1))*(Y0(4)-Y0(1))
  U2 = (Y0(4)-Y0(2))*(X0(3)-X0(2))-(X0(4)-X0(2))*(Y0(3)-Y0(2))
  A1SQ = (X0(1)-X0(3))**2+(Y0(1)-Y0(3))**2
  B1SQ = (X0(4)-X0(1))**2+(Y0(4)-Y0(1))**2
  C1SQ = (X0(3)-X0(4))**2+(Y0(3)-Y0(4))**2
  A2SQ = (X0(2)-X0(4))**2+(Y0(2)-Y0(4))**2
  B2SQ = (X0(3)-X0(2))**2+(Y0(3)-Y0(2))**2
  C3SQ = (X0(2)-X0(1))**2+(Y0(2)-Y0(1))**2
  S1SQ = U1*U1/(C1SQ*MAX(A1SQ,B1SQ))
  S2SQ = U2*U2/(C2SQ*MAX(A2SQ,B2SQ))
  S3SQ = U3*U3/(C3SQ*MAX(A3SQ,B3SQ))
  S4SQ = U4*U4/(C4SQ*MAX(A4SQ,B4SQ))
  IF (MIN(S1SQ,S2SQ) < MIN(S3SQ,S4SQ)) IDX = 1
end IF
CONXCH_simc = IDX
RETURN

END FUNCTION CONXCH_SIMC

end module space_utilities
