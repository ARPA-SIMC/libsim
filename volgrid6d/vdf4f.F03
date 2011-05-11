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


module vdf4f


USE iso_c_binding, ONLY: C_INT, C_FLOAT, C_DOUBLE, C_PTR, C_SIZE_T,C_CHAR

implicit none
private

Interface


  Integer(C_INT) Function create_metadata_c(xyzdim) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN) :: xyzdim(3)
  End Function create_metadata_c

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

  Function vdf4f_write_c(volume, xyzdim, ntime,nvar,varnames,len, filename) BIND(C)
  import
  Integer(C_SIZE_T), Intent(IN) :: xyzdim(3)
  Integer(C_SIZE_T), Intent(IN), VALUE :: ntime
  Integer(C_SIZE_T), Intent(IN), VALUE :: nvar
  Real(C_FLOAT),  Intent(IN) :: volume(xyzdim(1),xyzdim(2),xyzdim(3),ntime,nvar)
  Character(C_CHAR), Intent(IN) :: varnames(nvar)
  Integer(C_SIZE_T), Intent(IN), VALUE :: len
  Character(C_CHAR), Intent(IN) :: filename
  Integer(C_INT) :: vdf4f_write_c
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

End Interface

public vdf4f_write, create_metadata, set_num_timesteps, set_variables_names, &
 vdf4f_set_comment, vdf4f_set_ts_comment, vdf4f_set_v_comment, vdf4f_set_grid_extents, &
 vdf4f_set_coord_system_type, vdf4f_set_grid_type, &
 vdf4f_set_map_projection, write_metadata, destroy_metadata_c, destroy_writer_c, get_err_msg

!> interface to different architectures (cast some type)
interface vdf4f_write

#ifdef AMD64
  module procedure vdf4f_write_amd64
#endif

  module procedure vdf4f_write_i386

end interface

contains

integer function vdf4f_write_i386(volume, xyzdim, ntime,nvar,varnames, filename)

Real,intent(in)     :: volume(:,:,:,:,:)
Integer(C_SIZE_T),intent(in)  :: xyzdim(3)
integer(C_SIZE_T),intent(in) ::  nvar, ntime
Character(len=*),intent(in)    :: varnames(nvar)
Character(len=*),intent(in)    :: filename


!nteger(C_SIZE_T)  :: nvar_c
!nteger(C_SIZE_T)  :: xyzdim_c(3)
!nteger(C_SIZE_T)  :: ntime_c
integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer :: i

!var_c=nvar
!time_c=ntime
!yzdim_c=xyzdim

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)
end do

vdf4f_write_i386 = vdf4f_write_c(volume, xyzdim, ntime, nvar, varnames_c, len_c, trim(filename)//char(0))

return 

end function vdf4f_write_i386


integer function vdf4f_write_amd64(volume, xyzdim, ntime,nvar,varnames, filename)


Real,intent(in)     :: volume(:,:,:,:,:)
Integer,intent(in)  :: xyzdim(3)
Integer,intent(in)  :: nvar,ntime
Character(len=*),intent(in)    :: varnames(nvar)
Character(len=*),intent(in)    :: filename


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
  varnames_c(i)=trim(varnames(i))//char(0)
end do

vdf4f_write_amd64 = vdf4f_write_c(volume, xyzdim_c, ntime_c, nvar_c, varnames_c, len_c, trim(filename)//char(0))

return 

end function vdf4f_write_amd64



Integer Function create_metadata(xyzdim)
Integer(C_SIZE_T),intent(in)  :: xyzdim(3)
create_metadata = create_metadata_c(xyzdim)
End Function create_metadata

Integer Function set_num_timesteps(ntime )
Integer(C_SIZE_T), Intent(IN), VALUE :: ntime
set_num_timesteps = set_num_timesteps_c(ntime)
end Function set_num_timesteps


Integer Function set_variables_names(nvar, varnames)
Integer(C_SIZE_T), Intent(IN), VALUE :: nvar
Character(len=*), Intent(IN) :: varnames(nvar)

integer(C_SIZE_T)  :: len_c
Character(len=LEN(varnames)+1)    :: varnames_c(nvar)
integer :: i

len_c=LEN(varnames)+1
do i=1, nvar
  varnames_c(i)=trim(varnames(i))//char(0)
end do

set_variables_names = set_variables_names_c(nvar, varnames_c, len_c)

end Function set_variables_names


Integer function vdf4f_set_comment(comment)
Character(len=*),intent(in)    :: comment
vdf4f_set_comment = vdf4f_set_comment_c(trim(comment)//char(0))
end function vdf4f_set_comment

Integer function vdf4f_set_ts_comment(ts,comment)
Integer(C_SIZE_T), Intent(IN), VALUE :: ts
Character(len=*),intent(in)    :: comment
vdf4f_set_ts_comment = vdf4f_set_ts_comment_c(ts,trim(comment)//char(0))
end function vdf4f_set_ts_comment

Integer function vdf4f_set_v_comment(ts,var,comment)
Integer(C_SIZE_T), Intent(IN), VALUE :: ts
Character(len=*),intent(in)    :: var
Character(len=*),intent(in)    :: comment
vdf4f_set_v_comment = vdf4f_set_v_comment_c(ts, trim(var)//char(0), trim(comment)//char(0))
end function vdf4f_set_v_comment

Integer function vdf4f_set_grid_extents(extents)
Doubleprecision, Intent(IN) :: extents(6)
vdf4f_set_grid_extents = vdf4f_set_grid_extents_c(extents)
end function vdf4f_set_grid_extents

Integer function vdf4f_set_coord_system_type(coordsystemtype)
Character(len=*), Intent(IN) :: coordsystemtype
vdf4f_set_coord_system_type = vdf4f_set_coord_system_type_c(trim(coordsystemtype)//char(0))
end function vdf4f_set_coord_system_type

Integer function vdf4f_set_grid_type(gridtype)
Character(len=*), Intent(IN) :: gridtype
vdf4f_set_grid_type = vdf4f_set_grid_type_c(trim(gridtype)//char(0))
end function vdf4f_set_grid_type

Integer function vdf4f_set_map_projection(mapprojection)
Character(len=*), Intent(IN) :: mapprojection
vdf4f_set_map_projection = vdf4f_set_map_projection_c(trim(mapprojection)//char(0))
end function vdf4f_set_map_projection


function get_err_msg()

integer, parameter :: lenfun=255
character(len=lenfun) :: get_err_msg
Integer(C_SIZE_T) :: len
Character(len=lenfun) :: errmsg

integer :: ier

len = lenfun
ier = get_err_msg_c(errmsg,len)
get_err_msg=errmsg(:min(len,lenfun))

return 
end function get_err_msg


Integer  function write_metadata(filename)
Character(len=*), Intent(IN) :: filename
write_metadata = write_metadata_c(trim(filename)//char(0))
end function write_metadata

end module vdf4f
