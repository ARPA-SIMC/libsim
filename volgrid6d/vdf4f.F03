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

#include "config.h"

module vdf4f


USE iso_c_binding, ONLY: C_INT, C_FLOAT, C_DOUBLE, C_PTR, C_SIZE_T,C_CHAR

implicit none
private

Interface


  Integer(C_INT) Function create_metadata_c(xyzdim,vdctype) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN) :: xyzdim(3)
  Integer(C_INT),    Intent(IN), VALUE :: vdctype
  End Function create_metadata_c

  Integer(C_INT) Function create_writer_c(filename) BIND(C)
  import
  Character(C_CHAR), Intent(IN) :: filename
  End Function create_writer_c

  Integer(C_INT) Function set_num_timesteps_c(ntime ) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN), VALUE :: ntime
  end Function set_num_timesteps_c


  Integer(C_INT) Function set_variables_names_c(nvar, varnames, len ) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN), VALUE :: nvar
  Character(C_CHAR), Intent(IN) :: varnames(nvar)
  Integer(C_SIZE_T), Intent(IN), VALUE :: len
  end Function set_variables_names_c

  Integer(C_INT) function vdf4f_set_comment_c(comment) BIND(C)
  import
  Character(C_CHAR), Intent(IN) :: comment
  end function vdf4f_set_comment_c

  Integer(C_INT) function vdf4f_set_ts_comment_c(ts,comment) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN), VALUE :: ts
  Character(C_CHAR), Intent(IN) :: comment
  end function vdf4f_set_ts_comment_c

  Integer(C_INT) function vdf4f_set_v_comment_c(ts,var,comment) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN), VALUE :: ts
  Character(C_CHAR), Intent(IN) :: var
  Character(C_CHAR), Intent(IN) :: comment
  end function vdf4f_set_v_comment_c

  Integer(C_INT) function vdf4f_set_grid_extents_c(extents) BIND(C)
  import
  Real(C_DOUBLE), Intent(IN) :: extents(6)
  end function vdf4f_set_grid_extents_c

  Integer(C_INT) function vdf4f_set_coord_system_type_c(coordsystemtype) BIND(C)
  import
  Character(C_CHAR), Intent(IN) :: coordsystemtype
  end function vdf4f_set_coord_system_type_c

  Integer(C_INT) function vdf4f_set_grid_type_c(gridtype) BIND(C)
  import
  Character(C_CHAR), Intent(IN) :: gridtype
  end function vdf4f_set_grid_type_c

  Integer(C_INT) function vdf4f_set_map_projection_c(mapprojection) BIND(C)
  import
  Character(C_CHAR), Intent(IN) :: mapprojection
  end function vdf4f_set_map_projection_c

!  int set_grid_permutation(long permutation[3], char **errmsg);

  Integer(C_INT) function write_metadata_c(filename) BIND(C)
  import
  Character(C_CHAR), Intent(IN) :: filename
  end function write_metadata_c

  Integer(C_INT) Function vdf4f_write_c(volume, xyzdim, ntime,nvar,varnames,len, rzscan) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN) :: xyzdim(3)
  Integer(C_SIZE_T), Intent(IN), VALUE :: ntime
  Integer(C_SIZE_T), Intent(IN), VALUE :: nvar
  Real(C_FLOAT),  Intent(IN) :: volume(xyzdim(1),xyzdim(2),xyzdim(3),ntime,nvar)
  Character(C_CHAR), Intent(IN) :: varnames(nvar)
  Integer(C_SIZE_T), Intent(IN), VALUE :: len
  Integer(C_INT),     Intent(IN), VALUE :: rzscan
  End Function vdf4f_write_c

  Integer(C_INT) function destroy_metadata_c() BIND(C)
  import
  end function destroy_metadata_c

  Integer(C_INT) function destroy_writer_c() BIND(C)
  import
  end function destroy_writer_c

  Integer(C_INT) function get_err_msg_c(errmsg,len) BIND(C)
  import
  Character(C_CHAR), Intent(OUT) :: errmsg
  Integer(C_SIZE_T), Intent(OUT) :: len
  end function get_err_msg_c

  Integer(C_INT) function set_missing_value_c(missingv ) BIND(C)
  import
  Real(C_DOUBLE), VALUE :: missingv 
  end function set_missing_value_c

  Integer(C_INT) function get_missing_value_c(missingv ) BIND(C)
  import
  Real(C_DOUBLE),  Intent(OUT) :: missingv 
  end function get_missing_value_c

  Integer(C_INT) Function set_variables_2d_xy_c(nvar, varnames, len ) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN), VALUE :: nvar
  Character(C_CHAR), Intent(IN) :: varnames(nvar)
  Integer(C_SIZE_T), Intent(IN), VALUE :: len
  end Function set_variables_2d_xy_c


  Integer(C_INT) Function vdf4f_write_2d_xy_c(volume, xydim, ntime,nvar,varnames,len) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN) :: xydim(2)
  Integer(C_SIZE_T), Intent(IN), VALUE :: ntime
  Integer(C_SIZE_T), Intent(IN), VALUE :: nvar
  Real(C_FLOAT),  Intent(IN) :: volume(xydim(1),xydim(2),ntime,nvar)
  Character(C_CHAR), Intent(IN) :: varnames(nvar)
  Integer(C_SIZE_T), Intent(IN), VALUE :: len
  End Function vdf4f_write_2d_xy_c

End Interface

public vdf4f_write, vdf4f_create_metadata, vdf4f_create_writer, vdf4f_set_num_timesteps, vdf4f_set_variables_names, &
 vdf4f_set_comment, vdf4f_set_ts_comment, vdf4f_set_v_comment, vdf4f_set_grid_extents, &
 vdf4f_set_coord_system_type, vdf4f_set_grid_type, &
 vdf4f_set_map_projection, vdf4f_write_metadata, destroy_metadata_c, destroy_writer_c, vdf4f_get_err_msg, &
 vdf4f_set_missing_value,  vdf4f_get_missing_value, vdf4f_set_variables_2d_xy, vdf4f_write_2d_xy

!> interface to different architectures (cast some type)

interface vdf4f_write
#if SIZEOF_SIZE_T == 8
  module procedure vdf4f_write_amd64
#endif
  module procedure vdf4f_write_i386
end interface


interface vdf4f_create_metadata
#if SIZEOF_SIZE_T == 8
  module procedure create_metadata_amd64
#endif
  module procedure create_metadata_i386
end interface


interface vdf4f_set_num_timesteps
#if SIZEOF_SIZE_T == 8
  module procedure set_num_timesteps_amd64
#endif
  module procedure set_num_timesteps_i386
end interface

interface vdf4f_set_variables_names
#if SIZEOF_SIZE_T == 8
  module procedure set_variables_names_amd64
#endif
  module procedure set_variables_names_i386
end interface

interface vdf4f_set_ts_comment
#if SIZEOF_SIZE_T == 8
  module procedure vdf4f_set_ts_comment_amd64
#endif
  module procedure vdf4f_set_ts_comment_i386
end interface


interface vdf4f_set_v_comment
#if SIZEOF_SIZE_T == 8
  module procedure vdf4f_set_v_comment_amd64
#endif
  module procedure vdf4f_set_v_comment_i386
end interface

interface vdf4f_set_variables_2d_xy
#if SIZEOF_SIZE_T == 8
  module procedure set_variables_2d_xy_amd64
#endif
  module procedure set_variables_2d_xy_i386
end interface


interface vdf4f_write_2d_xy
#if SIZEOF_SIZE_T == 8
  module procedure vdf4f_write_2d_xy_amd64
#endif
  module procedure vdf4f_write_2d_xy_i386
end interface vdf4f_write_2d_xy


contains

integer function vdf4f_write_i386(volume, xyzdim, ntime,nvar,varnames, rzscan)

Real,intent(in)     :: volume(:,:,:,:,:)
Integer(C_SIZE_T),intent(in)  :: xyzdim(3)
integer(C_SIZE_T),intent(in) ::  nvar, ntime
Character(len=*),intent(in)    :: varnames(nvar)
Integer, intent(in) ::  rzscan


!integer(C_SIZE_T)  :: nvar_c
!integer(C_SIZE_T)  :: xyzdim_c(3)
!integer(C_SIZE_T)  :: ntime_c
integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer(C_SIZE_T) :: i

!nvar_c=nvar
!ntime_c=ntime
!xyzdim_c=xyzdim

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)    ! future : C_NULL_CHAR
end do

vdf4f_write_i386 = vdf4f_write_c(volume, xyzdim, ntime, nvar, varnames_c, len_c, rzscan)

return 

end function vdf4f_write_i386


integer function vdf4f_write_amd64(volume, xyzdim, ntime,nvar,varnames,rzscan)


Real,intent(in)     :: volume(:,:,:,:,:)
Integer,intent(in)  :: xyzdim(3)
Integer,intent(in)  :: nvar,ntime
Character(len=*),intent(in)    :: varnames(nvar)
integer, intent(in) ::  rzscan


Integer(C_SIZE_T)  :: nvar_c
Integer(C_SIZE_T)  :: xyzdim_c(3)
Integer(C_SIZE_T)  :: ntime_c
Integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer :: i


nvar_c=nvar
ntime_c=ntime
xyzdim_c=xyzdim

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)     ! future : C_NULL_CHAR
end do

vdf4f_write_amd64 = vdf4f_write_c(volume, xyzdim_c, ntime_c, nvar_c, varnames_c, len_c, rzscan)

return 

end function vdf4f_write_amd64



Integer Function create_metadata_i386(xyzdim, vdctype)
Integer(C_SIZE_T),intent(in)  :: xyzdim(3)
Integer(C_INT), Intent(IN), VALUE :: vdctype

create_metadata_i386 = create_metadata_c(xyzdim, vdctype)
End Function create_metadata_i386

Integer Function create_metadata_amd64(xyzdim, vdctype)
Integer,intent(in)  :: xyzdim(3)
Integer(C_INT), Intent(IN), VALUE :: vdctype
Integer(C_SIZE_T)  :: xyzdim_c(3)
xyzdim_c=xyzdim
create_metadata_amd64 = create_metadata_c(xyzdim_c,vdctype)
End Function create_metadata_amd64


Integer  function vdf4f_create_writer(filename)
Character(len=*), Intent(IN) :: filename
vdf4f_create_writer = create_writer_c(trim(filename)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_create_writer

Integer Function set_num_timesteps_i386(ntime )
Integer(C_SIZE_T), Intent(IN), VALUE :: ntime
set_num_timesteps_i386 = set_num_timesteps_c(ntime)
end Function set_num_timesteps_i386

Integer Function set_num_timesteps_amd64(ntime )
Integer, Intent(IN) :: ntime
Integer(C_SIZE_T) :: ntime_c
ntime_c=ntime
set_num_timesteps_amd64 = set_num_timesteps_c(ntime_c)
end Function set_num_timesteps_amd64


Integer Function set_variables_names_i386(nvar, varnames)
Integer(C_SIZE_T), Intent(IN), VALUE :: nvar
Character(len=*), Intent(IN) :: varnames(nvar)

integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer(C_SIZE_T) :: i

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)     ! future : C_NULL_CHAR
end do

set_variables_names_i386 = set_variables_names_c(nvar, varnames_c, len_c)

end Function set_variables_names_i386

Integer Function set_variables_names_amd64(nvar, varnames)
Integer :: nvar
Integer(C_SIZE_T) :: nvar_c
Character(len=*), Intent(IN) :: varnames(nvar)

integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer :: i

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)    ! future : C_NULL_CHAR
end do

nvar_c=nvar

set_variables_names_amd64 = set_variables_names_c(nvar_c, varnames_c, len_c)

end Function set_variables_names_amd64


Integer function vdf4f_set_comment(comment)
Character(len=*),intent(in)    :: comment
vdf4f_set_comment = vdf4f_set_comment_c(trim(comment)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_set_comment

Integer function vdf4f_set_ts_comment_i386(ts,comment)
Integer(C_SIZE_T), Intent(IN), VALUE :: ts
Character(len=*),intent(in)    :: comment
vdf4f_set_ts_comment_i386 = vdf4f_set_ts_comment_c(ts,trim(comment)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_set_ts_comment_i386


Integer function vdf4f_set_ts_comment_amd64(ts,comment)
Integer :: ts
Integer(C_SIZE_T) :: ts_c
Character(len=*),intent(in)    :: comment
ts_c=ts
vdf4f_set_ts_comment_amd64 = vdf4f_set_ts_comment_c(ts_c,trim(comment)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_set_ts_comment_amd64


Integer function vdf4f_set_v_comment_i386(ts,var,comment)
Integer(C_SIZE_T), Intent(IN), VALUE :: ts
Character(len=*),intent(in)    :: var
Character(len=*),intent(in)    :: comment
vdf4f_set_v_comment_i386 = vdf4f_set_v_comment_c(ts, trim(var)//char(0), trim(comment)//char(0))     ! future : C_NULL_CHAR
end function vdf4f_set_v_comment_i386

Integer function vdf4f_set_v_comment_amd64(ts,var,comment)
Integer :: ts
Integer(C_SIZE_T) :: ts_c
Character(len=*),intent(in)    :: var
Character(len=*),intent(in)    :: comment

ts_c=ts

vdf4f_set_v_comment_amd64 = vdf4f_set_v_comment_c(ts_c, trim(var)//char(0), trim(comment)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_set_v_comment_amd64

Integer function vdf4f_set_grid_extents(extents)
Doubleprecision, Intent(IN) :: extents(6)
vdf4f_set_grid_extents = vdf4f_set_grid_extents_c(extents)
end function vdf4f_set_grid_extents

Integer function vdf4f_set_coord_system_type(coordsystemtype)
Character(len=*), Intent(IN) :: coordsystemtype
vdf4f_set_coord_system_type = vdf4f_set_coord_system_type_c(trim(coordsystemtype)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_set_coord_system_type

Integer function vdf4f_set_grid_type(gridtype)
Character(len=*), Intent(IN) :: gridtype
vdf4f_set_grid_type = vdf4f_set_grid_type_c(trim(gridtype)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_set_grid_type

Integer function vdf4f_set_map_projection(mapprojection)
Character(len=*), Intent(IN) :: mapprojection
vdf4f_set_map_projection = vdf4f_set_map_projection_c(trim(mapprojection)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_set_map_projection


function vdf4f_get_err_msg()

integer, parameter :: lenfun=255
character(len=lenfun) :: vdf4f_get_err_msg
Integer(C_SIZE_T) :: len
Character(len=lenfun) :: errmsg

integer :: ier

len = lenfun
ier = get_err_msg_c(errmsg,len)
vdf4f_get_err_msg=errmsg(:min(len,lenfun))

return 
end function vdf4f_get_err_msg


Integer  function vdf4f_write_metadata(filename)
Character(len=*), Intent(IN) :: filename
vdf4f_write_metadata = write_metadata_c(trim(filename)//char(0))    ! future : C_NULL_CHAR
end function vdf4f_write_metadata


Integer function vdf4f_set_missing_value(missingv)
doubleprecision,  Intent(IN) :: missingv 
vdf4f_set_missing_value = set_missing_value_c(missingv)
end function vdf4f_set_missing_value

Integer function vdf4f_get_missing_value(missingv)
doubleprecision,  Intent(OUT) :: missingv 
vdf4f_get_missing_value = get_missing_value_c(missingv)
end function vdf4f_get_missing_value


Integer Function set_variables_2d_xy_i386(nvar, varnames)
Integer(C_SIZE_T), Intent(IN), VALUE :: nvar
Character(len=*), Intent(IN) :: varnames(nvar)

integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer(C_SIZE_T) :: i

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)     ! future : C_NULL_CHAR
end do

set_variables_2d_xy_i386 = set_variables_2d_xy_c(nvar, varnames_c, len_c)

end Function set_variables_2d_xy_i386

Integer Function set_variables_2d_xy_amd64(nvar, varnames)
Integer :: nvar
Integer(C_SIZE_T) :: nvar_c
Character(len=*), Intent(IN) :: varnames(nvar)

integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer :: i

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)    ! future : C_NULL_CHAR
end do

nvar_c=nvar

set_variables_2d_xy_amd64 = set_variables_2d_xy_c(nvar_c, varnames_c, len_c)

end Function set_variables_2d_xy_amd64



integer function vdf4f_write_2d_xy_i386(volume, xydim, ntime,nvar,varnames)

Real,intent(in)     :: volume(:,:,:,:)
Integer(C_SIZE_T),intent(in)  :: xydim(2)
integer(C_SIZE_T),intent(in) ::  nvar, ntime
Character(len=*),intent(in)    :: varnames(nvar)

!integer(C_SIZE_T)  :: nvar_c
!integer(C_SIZE_T)  :: xydim_c(2)
!integer(C_SIZE_T)  :: ntime_c
integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer(C_SIZE_T) :: i

!nvar_c=nvar
!ntime_c=ntime
!xydim_c=xydim

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)    ! future : C_NULL_CHAR
end do

vdf4f_write_2d_xy_i386 = vdf4f_write_2d_xy_c(volume, xydim, ntime, nvar, varnames_c, len_c)

return 

end function vdf4f_write_2d_xy_i386


integer function vdf4f_write_2d_xy_amd64(volume, xydim, ntime,nvar,varnames)

Real,intent(in)     :: volume(:,:,:,:)
Integer,intent(in)  :: xydim(2)
Integer,intent(in)  :: nvar,ntime
Character(len=*),intent(in)    :: varnames(nvar)

Integer(C_SIZE_T)  :: nvar_c
Integer(C_SIZE_T)  :: xydim_c(2)
Integer(C_SIZE_T)  :: ntime_c
Integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1) :: varnames_c(nvar)
integer :: i

nvar_c=nvar
ntime_c=ntime
xydim_c=xydim

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)     ! future : C_NULL_CHAR
end do

vdf4f_write_2d_xy_amd64 = vdf4f_write_2d_xy_c(volume, xydim_c, ntime_c, nvar_c, varnames_c, len_c)

return 

end function vdf4f_write_2d_xy_amd64


end module vdf4f
