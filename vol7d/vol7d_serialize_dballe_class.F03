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
MODULE vol7d_serialize_dballe_class
USE,INTRINSIC :: iso_c_binding
USE vol7d_serialize_class
USE optionparser_class
USE vol7d_class
USE dballe_class
IMPLICIT NONE

TYPE,EXTENDS(vol7d_serialize) :: vol7d_serialize_dballe
  !!TYPE(vol7d_ana) :: ana  !< identificativo della stazione da exportare
  CONTAINS
!  PROCEDURE :: vol7d_serialize_optionparser
  PROCEDURE :: vol7d_serialize_export
END TYPE vol7d_serialize_dballe

type counter
  integer :: nvar,nattr
end type counter

PRIVATE
PUBLIC vol7d_serialize_dballe, vol7d_serialize_dballe_new

CONTAINS

FUNCTION vol7d_serialize_dballe_new() RESULT(this)
TYPE(vol7d_serialize_dballe) :: this
!!$TYPE(vol7d_ana),INTENT(in),optional :: ana  !< identificativo della stazione da exportare
!!$
!!$if (present(ana))then
!!$  this%ana=ana
!!$else
!!$  call init(this%ana)
!!$end if

this%vol7d_serialize = vol7d_serialize_new()

! reset unconditionally some parameters
this%column = 'ana,time,timerange,level,network'
this%loop = 'ana,time,timerange,level,network'
this%keep_miss = .FALSE.
this%cachedesc = .TRUE.
! call original parse method to fix tuning parameters without forcing
! to call option methods explicitely
CALL this%vol7d_serialize_parse()

END FUNCTION vol7d_serialize_dballe_new


!SUBROUTINE vol7d_serialize_parse(this, category)
!CLASS(vol7d_serialize_geojson),INTENT(inout) :: this !< object having undorgone the vol7d_serialize_optionparser method
!INTEGER,INTENT(in),OPTIONAL :: category !< log4fortran category for logging error messages
!
!!! do nothing
!
!END SUBROUTINE vol7d_serialize_parse



SUBROUTINE vol7d_ana_callback_dba(ana, genericptr)
TYPE(vol7d_ana), INTENT(in) :: ana
TYPE(c_ptr),VALUE :: genericptr

type(dbametaanddata),POINTER :: metaanddata

CALL C_F_POINTER(genericptr, metaanddata)

metaanddata%metadata%ana%vol7d_ana=ana

END SUBROUTINE vol7d_ana_callback_dba


SUBROUTINE vol7d_time_callback_dba(time, genericptr)
TYPE(datetime), INTENT(in) :: time
TYPE(c_ptr),VALUE :: genericptr

type(dbametaanddata),POINTER :: metaanddata

CALL C_F_POINTER(genericptr, metaanddata)

metaanddata%metadata%datetime%datetime=time

END SUBROUTINE vol7d_time_callback_dba

SUBROUTINE vol7d_timerange_callback_dba(timerange, genericptr)
TYPE(vol7d_timerange), INTENT(in) :: timerange
TYPE(c_ptr),VALUE :: genericptr

type(dbametaanddata),POINTER :: metaanddata

CALL C_F_POINTER(genericptr, metaanddata)

metaanddata%metadata%timerange%vol7d_timerange=timerange

END SUBROUTINE vol7d_timerange_callback_dba


SUBROUTINE vol7d_level_callback_dba(level, genericptr)
TYPE(vol7d_level), INTENT(in) :: level
TYPE(c_ptr),VALUE :: genericptr

type(dbametaanddata),POINTER :: metaanddata

CALL C_F_POINTER(genericptr, metaanddata)

metaanddata%metadata%level%vol7d_level=level

END SUBROUTINE vol7d_level_callback_dba


SUBROUTINE vol7d_network_callback_dba(network, genericptr)
TYPE(vol7d_network), INTENT(in) :: network
TYPE(c_ptr),VALUE :: genericptr

type(dbametaanddata),POINTER :: metaanddata

CALL C_F_POINTER(genericptr, metaanddata)

metaanddata%metadata%network%vol7d_network=network

END SUBROUTINE vol7d_network_callback_dba


SUBROUTINE vol7d_valuer_callback_dba(valu, var, genericptr)
REAL,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatar(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valuer_callback_dba


SUBROUTINE vol7d_valued_callback_dba(valu, var, genericptr)
double precision,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatad(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valued_callback_dba


SUBROUTINE vol7d_valuei_callback_dba(valu, var, genericptr)
integer,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatai(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valuei_callback_dba


SUBROUTINE vol7d_valueb_callback_dba(valu, var, genericptr)
INTEGER(kind=int_b),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatab(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valueb_callback_dba


SUBROUTINE vol7d_valuec_callback_dba(valu, var, genericptr)
character(len=*),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatac(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valuec_callback_dba


SUBROUTINE vol7d_valuer_attr_callback_dba(valu, var, attr, genericptr)
REAL,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatar(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valuer_attr_callback_dba


SUBROUTINE vol7d_valued_attr_callback_dba(valu, var, attr, genericptr)
double precision,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatad(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valued_attr_callback_dba


SUBROUTINE vol7d_valuei_attr_callback_dba(valu, var, attr, genericptr)
integer,INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatai(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valuei_attr_callback_dba


SUBROUTINE vol7d_valueb_attr_callback_dba(valu, var, attr, genericptr)
INTEGER(kind=int_b),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatab(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valueb_attr_callback_dba


SUBROUTINE vol7d_valuec_attr_callback_dba(valu, var, attr, genericptr)
character(len=*),INTENT(in) :: valu
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr
type(dbametaanddata),POINTER :: metaanddata
integer :: i

CALL C_F_POINTER(genericptr, metaanddata)

do i =1, size(metaanddata%dataattrv%dataattr)
  if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) then
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatac(var%btable,valu))
    exit
  end if
end do

END SUBROUTINE vol7d_valuec_attr_callback_dba


SUBROUTINE vol7d_void_callback_dba(genericptr)
TYPE(c_ptr),VALUE :: genericptr
end SUBROUTINE vol7d_void_callback_dba


SUBROUTINE vol7d_value_var_header_callback_dba(var, typ, genericptr)
TYPE(vol7d_var),INTENT(in) :: var
CHARACTER(len=1),INTENT(in) :: typ
TYPE(c_ptr),VALUE :: genericptr

type (counter),POINTER :: conta

CALL C_F_POINTER(genericptr, conta)
conta%nvar=conta%nvar+1

END SUBROUTINE vol7d_value_var_header_callback_dba


SUBROUTINE vol7d_value_attr_header_callback_dba(var, attr, typ, genericptr)
TYPE(vol7d_var),INTENT(in) :: var
TYPE(vol7d_var),INTENT(in) :: attr
CHARACTER(len=1),INTENT(in) :: typ
TYPE(c_ptr),VALUE :: genericptr

type (counter),POINTER :: conta

CALL C_F_POINTER(genericptr, conta)
conta%nattr=conta%nattr+1

END SUBROUTINE vol7d_value_attr_header_callback_dba

SUBROUTINE vol7d_var_callback_dba(var, genericptr)
TYPE(vol7d_var), INTENT(in) :: var
TYPE(c_ptr),VALUE :: genericptr

END SUBROUTINE vol7d_var_callback_dba


SUBROUTINE vol7d_attr_callback_dba(var, attr, genericptr)
TYPE(vol7d_var), INTENT(in) :: var
TYPE(vol7d_var), INTENT(in) :: attr
TYPE(c_ptr),VALUE :: genericptr

END SUBROUTINE vol7d_attr_callback_dba



SUBROUTINE vol7d_serialize_export(this, metaanddatal)
CLASS(vol7d_serialize_dballe),INTENT(inout) :: this
type(dbametaanddatalist),INTENT(inout) :: metaanddatal
TYPE(vol7d_serialize_iterline) :: linei
TYPE(vol7d_serialize_itercol) :: coli

type (counter),target :: conta
type(dbametaanddata),target :: metaanddata
integer :: i,j

conta=counter(0,0)

! set "header" callbacks
CALL this%vol7d_serialize_set_callback(&
 vol7d_void_callback_dba,&
 vol7d_void_callback_dba,&
 vol7d_void_callback_dba,&
 vol7d_void_callback_dba,&
 vol7d_void_callback_dba,&
 vol7d_void_callback_dba,&
 vol7d_void_callback_dba,&
 vol7d_value_var_callback=vol7d_value_var_header_callback_dba, &
 vol7d_value_attr_callback=vol7d_value_attr_header_callback_dba)

! loop over headers and count columns
coli = this%vol7d_serialize_itercol_new() ! column iterator from this (header line)
DO WHILE(coli%next())
! call callbacks
  CALL coli%export(C_LOC(conta))
END DO

conta%nattr=conta%nattr/conta%nvar

allocate(metaanddata%dataattrv%dataattr(conta%nvar))
do i =1,conta%nvar
  allocate(metaanddata%dataattrv%dataattr(i)%attrv%dcv(conta%nattr))
enddo

! loop over lines
linei = this%vol7d_serialize_iterline_new()

! set "data lines" callbacks
CALL linei%vol7d_serialize_iterline_set_callback(vol7d_ana_callback_dba &
 ,vol7d_time_callback_dba, vol7d_level_callback_dba &
 ,vol7d_timerange_callback_dba, vol7d_network_callback_dba &
 ,vol7d_var_callback_dba, vol7d_attr_callback_dba&
 ,vol7d_valuer_callback_dba &
 ,vol7d_valued_callback_dba &
 ,vol7d_valuei_callback_dba &
 ,vol7d_valueb_callback_dba &
 ,vol7d_valuec_callback_dba &
 ,vol7d_valuer_attr_callback_dba &
 ,vol7d_valued_attr_callback_dba &
 ,vol7d_valuei_attr_callback_dba &
 ,vol7d_valueb_attr_callback_dba &
 ,vol7d_valuec_attr_callback_dba &
 )

DO WHILE(linei%next())
  coli = linei%vol7d_serialize_itercol_new() ! column iterator from linei (data line)
  DO WHILE(coli%next())
! call callbacks
    CALL coli%export(C_LOC(metaanddata))
  END DO

! manage time definition
  if (this%v7d%time_definition == 0) then
    metaanddata%metadata%datetime%datetime = &
     metaanddata%metadata%datetime%datetime + &
     timedelta_new(msec=metaanddata%metadata%timerange%vol7d_timerange%p1*1000)
  end if

!!  set to missing
  do i =1,conta%nvar
    if (.not. allocated(metaanddata%dataattrv%dataattr(i)%dat)) &
    allocate(metaanddata%dataattrv%dataattr(i)%dat,source=dbadatai())
    do j=1,conta%nattr
      if (.not. allocated(metaanddata%dataattrv%dataattr(i)%attrv%dcv(j)%dat)) &
      allocate(metaanddata%dataattrv%dataattr(i)%attrv%dcv(j)%dat,source=dbadatac())
    end do
  enddo

  call metaanddatal%append(metaanddata)

!!  deallocate
  do i =1,conta%nvar
    deallocate(metaanddata%dataattrv%dataattr(i)%dat)
    do j=1,conta%nattr
      deallocate(metaanddata%dataattrv%dataattr(i)%attrv%dcv(j)%dat)
    end do
  enddo

END DO

END SUBROUTINE vol7d_serialize_export

!!$SUBROUTINE default_vol7d_valuec_callback(valu, var, genericptr)
!!$CHARACTER(len=*),INTENT(in) :: valu
!!$TYPE(vol7d_var),INTENT(in) :: var
!!$TYPE(c_ptr),VALUE :: genericptr
!!$
!!$CHARACTER(len=64),POINTER :: col
!!$
!!$CALL C_F_POINTER(genericptr, col)
!!$IF (c_e(valu)) THEN
!!$  IF (c_e(var%scalefactor) .AND. var%unit /= 'CCITTIA5' .AND. &
!!$   .NOT.(var%scalefactor == 0 .AND. var%unit == 'NUMERIC')) THEN
!!$    col = t2c(realdat(valu, var))
!!$  ELSE
!!$    col = TRIM(valu)
!!$  ENDIF
!!$ELSE
!!$  col = ''
!!$ENDIF
!!$
!!$END SUBROUTINE default_vol7d_valuec_callback


END MODULE vol7d_serialize_dballe_class
