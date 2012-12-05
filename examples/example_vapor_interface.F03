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

program example_vapor_interface

use vdf4f
use missing_values

implicit none

character(len=255) :: lfilename="vg6d.vdf", mapprojection
integer,parameter :: ntime=2, nlevel=50, nvar=2, nx=50, ny=50
logical :: exist
integer :: ier, xyzdim(3)
character(len=255) :: varnames(nvar),vardescriptions(nvar),tsdescriptions(ntime)
doubleprecision :: extents(6),vdfmiss
integer ::  irzscan=0 !<  if true reverse Z (level) order in vdf export
real,allocatable :: voldati(:,:,:,:,:)
integer :: ix,iy,ilevel,itime,ivar,i,j

allocate (voldati(nx,ny,nlevel,ntime,nvar))

inquire(file=lfilename,EXIST=exist)
if (exist) then
  print *,"file exist; cannot open new file: "//trim(lfilename)
  CALL exit(1)
end if


do ix=1,nx
  do iy=1,ny
    do ilevel=1,nlevel
      do itime=1,ntime
        do ivar=1,nvar
          if (ix > nx/2 .and. iy > ny/2 .and. ilevel > nlevel/2 ) then 
            voldati(ix,iy,ilevel,itime,ivar)=ix+iy+ilevel+itime
          else
            voldati(ix,iy,ilevel,itime,ivar)=rmiss
          end if
        end do
      end do
    end do
  end do
end do


print *,"writing on file: "//trim(lfilename)

xyzdim(1)=nx 
xyzdim(2)=ny
xyzdim(3)=nlevel

do i=1, nvar          
  write (varnames(i),'(a3,i2.2)') "Var",i
  write (vardescriptions(i),*) "Description"
end do

do i=1,ntime
  write (tsdescriptions(i),*) "2012-10-23 12:00"
end do
extents=(/-0.5d0, -14.d0 ,   0.d0, 2.563d0, -11.25d0,  1.d0/)
mapprojection = "+proj=latlon +ellps=sphere"

if(ier==0) print*,"VDF: call vdf4f_create_metadata"
ier = vdf4f_create_metadata(xyzdim)
!if(ier==0) ier = vdf4f_set_missing_value(dble(rmiss))
if(ier==0) ier = vdf4f_set_missing_value(1000.d0)
ier=0

if(ier==0) print*,"VDF: call vdf4f_set_num_timesteps"
if(ier==0) ier = vdf4f_set_num_timesteps(ntime)
if(ier==0) print*,"VDF: call vdf4f_set_variables_names"
if(ier==0) ier = vdf4f_set_variables_names(nvar, varnames)

do i=1,ntime
  if(ier==0) print*,"VDF: call vdf4f_set_ts_comment"
  if(ier==0) ier = vdf4f_set_ts_comment(i-1,tsdescriptions(i))
  do j=1,nvar
    print*,"VDF: call vdf4f_set_v_comment"
    if(ier==0) ier = vdf4f_set_v_comment(i-1,varnames(j),vardescriptions(j))
  end do
end do

if(ier==0) print *,"VDF: call vdf4f_set_comment"
if(ier==0) ier = vdf4f_set_comment("vogrid6d exported")
if(ier==0) print *,"VDF: call vdf4f_set_grid_extent"
if(ier==0) ier = vdf4f_set_grid_extents(extents=extents)
                                ! if(ier==0) ier = vdf4f_set_coord_system_type(coordsystemtype="spherical")
if(ier==0) ier = vdf4f_set_coord_system_type(coordsystemtype="cartesian")
if(ier==0) ier = vdf4f_set_grid_type(gridtype="regular")
if(ier==0) ier = vdf4f_set_map_projection(mapprojection=mapprojection)
                                ! if(ier==0) int vdf4f_set_grid_permutation(long permutation[3]);
if(ier==0) print*,"VDF: call vdf4f_write_metadata"
if(ier==0) ier = vdf4f_write_metadata(lfilename)

if(ier==0) print*,"VDF: call vdf4f_write"
if(ier==0) print*,"scan VDF (vapor file) for times"
if(ier==0) ier = vdf4f_write(voldati, xyzdim, ntime, nvar, varnames, lfilename, irzscan)  
if (ier /= 0) then
  print*,"ERROR exporting to vdf: "//vdf4f_get_err_msg()
  CALL exit(1)
end if

if(ier==0) ier = vdf4f_get_missing_value(vdfmiss)
!if(ier==0) print *,"vdfmiss:",vdfmiss,dble(rmiss),rmiss
if(ier==0) print *,"vdfmiss:",vdfmiss,dble(1000.),1000.
ier=0

ier = destroy_metadata_c()
ier = destroy_writer_c()

end program example_vapor_interface
