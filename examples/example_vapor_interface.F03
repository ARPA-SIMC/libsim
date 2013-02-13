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
integer,parameter :: ntime=3, nlevel=40, nvar=3, nvar2d=3, nx=50, ny=55
logical :: exist
integer :: ier, xyzdim(3)
character(len=255) :: varnames(nvar+nvar2d),varnames2d(nvar2d),vardescriptions(nvar+nvar2d),tsdescriptions(ntime)
doubleprecision :: extents(6),vdfmiss=rmiss,extelemin,extelemax
integer ::  irzscan=0 !<  if true reverse Z (level) order in vdf export
real,allocatable :: voldati(:,:,:,:,:)
real,allocatable :: voldati2d(:,:,:,:)
integer :: ix,iy,ilevel,itime,ivar,i,j

allocate (voldati(nx,ny,nlevel,ntime,nvar))
allocate (voldati2d(nx,ny,ntime,nvar2d))

inquire(file=lfilename,EXIST=exist)
if (exist) then
  print *,"file exist; cannot open new file: "//trim(lfilename)
  CALL exit(1)
end if


do ix=1,nx
  do iy=1,ny
    do itime=1,ntime
      do ivar=1,nvar2d

        if ( ivar == 1 ) then
          voldati2d(ix,iy,itime,ivar)=float(ix+iy)/float(nx+ny)
!!$          if (ix > nx/2 .and. iy > ny/2 ) then 
!!$            voldati2d(ix,iy,itime,ivar)=.1
!!$          else
!!$            voldati2d(ix,iy,itime,ivar)=1.
!!$          end if
        else
          if (ix > nx/2 .and. iy > ny/2 .and. ivar == nvar2d) then 
            voldati2d(ix,iy,itime,ivar)=real(vdfmiss)
          else
            voldati2d(ix,iy,itime,ivar)=ix+iy+itime
          end if
        end if

      end do
    end do
  end do
end do

do ix=1,nx
  do iy=1,ny
    do itime=1,ntime
      do ivar=1,nvar
        do ilevel=1,nlevel

          if ( ivar == 1 ) then
            !voldati(ix,iy,ilevel,itime,ivar)=float(ix+iy+(ilevel)*10)/float(nx+ny+nlevel*10) +&
            voldati  (ix,iy,ilevel,itime,ivar)=float(ilevel)/float(nlevel) +&
            voldati2d(ix,iy,itime,1)
          else
            if (ix > nx/2 .and. iy > ny/2 .and. ilevel > nlevel/2 .and. ivar == nvar) then 
              voldati(ix,iy,ilevel,itime,ivar)=real(vdfmiss)
            else
              voldati(ix,iy,ilevel,itime,ivar)=ix+iy+ilevel+itime
            end if
          end if

        end do
      end do
    end do
  end do
end do

!voldati(:,:,1,:,1)=voldati2d(:,:,:,1)

!print *, voldati(1,1,1,1,1),voldati2d(1,1,1,1)
!print *, voldati(nx,ny,1,1,1),voldati2d(nx,ny,1,1)
print *, maxval(voldati2d(:,:,1,1))

!extelemin = maxval (voldati(:,:,1,:,1))
extelemin = 0.d0
!extelemax = minval (voldati(:,:,nlevel,:,1))
extelemax = maxval (voldati(:,:,nlevel,:,1))

!extelemin = 0
!extelemax = 1
 
print *, "extent", extelemin,extelemax

print *,"writing on file: "//trim(lfilename)

xyzdim(1)=nx 
xyzdim(2)=ny
xyzdim(3)=nlevel

do i=1, nvar          
  write (varnames(i),'(a3,i2.2)') "Var",i
  write (vardescriptions(i),*) "Description"
end do

do i=1, nvar2d
  write (varnames(nvar+i),'(a5,i2.2)') "Var2d",i
  write (vardescriptions(nvar+i),*) "Description"
  write (varnames2d(i),'(a5,i2.2)') "Var2d",i
end do

varnames(1)="ELEVATION"
varnames(nvar+1)="HGT"
varnames2d(1)= "HGT"

do i=1,ntime
  write (tsdescriptions(i),*) "2012-10-23 12:00"
end do
extents=(/-0.5d0, -14.d0 ,   extelemin, 2.563d0, -11.25d0,  extelemax/)
mapprojection = "+proj=latlon +ellps=sphere"

print*,"VDF: call vdf4f_create_metadata"
ier = vdf4f_create_metadata(xyzdim,vdctype=2)
if(ier==0) print*,"VDF: call set_missing_value"
if(ier==0) ier = vdf4f_set_missing_value(vdfmiss)

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


if(ier==0) print*,"VDF: call vdf4f_set_num_timesteps"
if(ier==0) ier = vdf4f_set_num_timesteps(ntime)


if(ier==0) print *,"VDF: call vdf4f_set_comment"
if(ier==0) ier = vdf4f_set_comment("vogrid6d exported")
if(ier==0) print *,"VDF: call vdf4f_set_grid_extent"
if(ier==0) ier = vdf4f_set_grid_extents(extents=extents)
                                ! if(ier==0) ier = vdf4f_set_coord_system_type(coordsystemtype="spherical")
if(ier==0) ier = vdf4f_set_coord_system_type(coordsystemtype="cartesian")
if(ier==0) ier = vdf4f_set_grid_type(gridtype="regular")
!if(ier==0) ier = vdf4f_set_grid_type(gridtype="layered")
if(ier==0) ier = vdf4f_set_map_projection(mapprojection=mapprojection)
                                ! if(ier==0) int vdf4f_set_grid_permutation(long permutation[3]);
if(ier==0) print*,"VDF: call vdf4f_write_metadata"
if(ier==0) ier = vdf4f_write_metadata(lfilename)

if(ier==0) ier = destroy_metadata_c()

if(ier==0) print*,"VDF: call vdf4f_create_writer"
if(ier==0) ier = vdf4f_create_writer(lfilename)
if(ier==0) print*,"VDF: call vdf4f_write"
if(ier==0) ier = vdf4f_write(voldati, xyzdim, ntime, nvar, varnames, irzscan)  

if(ier==0) ier = destroy_writer_c()

if(ier==0) print*,"VDF: call vdf4f_create_metadata_from_file"
if(ier==0) ier = vdf4f_create_metadata_from_file(lfilename)

if(ier==0) print*,"VDF: call vdf4f_set_variables_2d_xy"
if(ier==0) ier = vdf4f_set_variables_2d_xy(nvar2d, varnames2d)

if(ier==0) print*,"VDF: call vdf4f_write_metadata"
if(ier==0) ier = vdf4f_write_metadata(lfilename)
if(ier==0) ier = destroy_metadata_c()


if(ier==0) print*,"VDF: call vdf4f_create_writer"
if(ier==0) ier = vdf4f_create_writer(lfilename)

if(ier==0) print*,"VDF: call vdf4f_write_2d_xy"
if(ier==0) ier = vdf4f_write_2d_xy(voldati2d, xyzdim(:2), ntime, nvar2d, varnames2d)

if(ier==0) ier = vdf4f_get_missing_value(vdfmiss)
if(ier==0) print *,"vdfmiss:",vdfmiss

if(ier==0) ier = destroy_writer_c()

if (ier /= 0) then
  print*, "errocode:",ier
  print*, "ERROR exporting to vdf: "//vdf4f_get_err_msg()
  CALL exit(1)
end if

end program example_vapor_interface
