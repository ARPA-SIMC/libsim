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

module volgrid6d_vapor_class

use log4fortran
USE volgrid6d_class
USE volgrid6d_var_class
use vol7d_class
use vol7d_dballe_class
use missing_values
use char_utilities
use vdf4f
use err_handling
use optional_values
use grid_class

implicit none


!> Export an object
INTERFACE export
  MODULE PROCEDURE volgrid6d_export_to_vapor
END INTERFACE

private

public export

contains

!>\brief Write on file Volgrid6d volume in vdf format for vapor.
!! Write bg6d volume in wavelet vapor file.
subroutine volgrid6d_export_to_vapor (this,normalize,rzscan,filename,filename_auto)

TYPE(volgrid6d),INTENT(IN) :: this !< volume volgrid6d to write
logical,intent(in) :: normalize !<  if true normalize variables to v7d (dballe) standard
logical,intent(in),optional :: rzscan      !<  if true reverse Z (level) order in vdf export
character(len=*),intent(in),optional :: filename !< file name to write
character(len=*),intent(out),optional :: filename_auto !< generated  file name if "filename" is missing

character(len=254) :: lfilename
integer :: ntime, ntimerange, ntimera, nlevel, nvar
logical :: exist
integer :: ier, xyzdim(3),ivar,i,j,retstat
character(len=255),allocatable :: varnames(:),vardescriptions(:),tsdescriptions(:)
doubleprecision :: extents(6)

TYPE(conv_func), pointer :: c_func(:)
TYPE(vol7d_var),allocatable :: varbufr(:)
type(vol7d_var),pointer :: dballevar(:)
CHARACTER(len=255) :: proj_type,mapprojection

integer :: zone, irzscan
DOUBLE PRECISION :: xoff, yoff, ellips_smaj_axis, ellips_flatt

call l4f_category_log(this%category,L4F_DEBUG,"export to vapor")

call vol7d_dballe_import_dballevar(dballevar)

if (optio_log(rzscan)) then
  irzscan=1
else
  irzscan=0
end if

ntime=imiss
ntimerange=imiss
nlevel=imiss
nvar=imiss
ntimera=imiss

lfilename="vg6d.vdf"
if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if

if (present(filename_auto))filename_auto=lfilename


inquire(file=lfilename,EXIST=exist)
if (exist) then
  call l4f_category_log(this%category,L4F_ERROR,"file exist; cannot open new file: "//trim(lfilename))
  CALL raise_error()
end if

call l4f_category_log(this%category,L4F_INFO,"writing on file: "//trim(lfilename))

if (associated(this%time)) ntime=size(this%time)
if (associated(this%timerange)) ntimerange=size(this%timerange)
if (associated(this%level)) nlevel=size(this%level)
if (associated(this%var)) nvar=size(this%var)

if (c_e(ntime) .and. c_e(ntimerange) .and. c_e(nlevel) .and. c_e(nvar)) then

  allocate(varnames(nvar),vardescriptions(nvar),varbufr(nvar))

  call get_val (this%griddim, nx=xyzdim(1) , ny=xyzdim(2) )

  !xyzdim(1)=this%griddim%dim%nx
  !xyzdim(2)=this%griddim%dim%ny
  xyzdim(3)=nlevel

  if (associated(this%voldati)) then 

    if (optio_log(normalize)) then
      CALL vargrib2varbufr(this%var, varbufr, c_func)

                                ! Rescale valid data according to variable conversion table
      IF (ASSOCIATED(c_func)) THEN

        DO ivar = 1, nvar
          this%voldati(:,:,:,:,:,ivar) = convert(c_func(ivar),this%voldati(:,:,:,:,:,ivar))
        ENDDO
        DEALLOCATE(c_func)

      ENDIF

      do ivar=1,nvar
        
        j=firsttrue(varbufr(ivar)%btable == dballevar(:)%btable)
        
        if ( j > 0 )then
          varbufr(ivar)%description =  dballevar(j)%description
          varbufr(ivar)%unit =  dballevar(j)%unit
          varbufr(ivar)%scalefactor =  dballevar(j)%scalefactor
          
          varnames(ivar) = varbufr(ivar)%btable
          vardescriptions(ivar) = trim(varbufr(ivar)%description)//"_"//trim(varbufr(ivar)%unit)
          !vardescriptions(ivar) = "V"//wash_char(trim(varbufr(ivar)%description)//"_"//trim(varbufr(ivar)%unit))

        else

          varnames(ivar) = "Vnotnormalized_"//t2c(ivar)
          vardescriptions(ivar) = "None"
          
        end if
      
      end do
    else

      do ivar=1, nvar
        varnames(ivar) = "V"//trim(to_char(this%var(ivar)%number))
        !varnames(ivar) = wash_char("VAR_"//trim(to_char(this%var(ivar)%number)))
      end do
    end if

    if (this%time_definition == 1) then
      ntimera=ntime
      allocate(tsdescriptions(ntimera))

      do i=1,ntimera
        tsdescriptions(i)=to_char(this%time(i))
      end do
    else
      ntimera=ntimerange
      allocate(tsdescriptions(ntimera))

      do i=1,ntimera
        tsdescriptions(i)=to_char(this%timerange(i))
      end do
    end if

    !extents=(/-0.5d0, -14.d0 ,   0.d0, 2.563d0, -11.25d0,  1.d0/)
    call get_val (this%griddim, xmin=extents(1),ymin=extents(2), xmax=extents(4) , ymax=extents(5))
    extents(3)=0.d0
    extents(6)=1.d0
    call get_val (this%griddim, proj_type=proj_type)

    select case (proj_type)
    case ("regular_ll")

      call l4f_category_log(this%category,L4F_INFO,"VDF: proj support this projection: "//trim(proj_type))
      mapprojection = "+proj=latlon"

    case ("rotated_ll")

      call l4f_category_log(this%category,L4F_WARN,"VDF: proj do not support this projection: "//trim(proj_type))
      call l4f_category_log(this%category,L4F_WARN,"VDF: you have to considerate it as rotated coordinates")

      !call get_val (this%griddim, proj_type, longitude_south_pole, latitude_south_pole, angle_rotation)
      !write (mapprojection,*)"-m 57.295779506 +proj=ob_tran +o_proj=latlon +o_lat_p=",32.5"," +o_lon_p=",0"," +lon_0=",10
      mapprojection = "+proj=latlon"


    case ("UTM")
      
      call l4f_category_log(this%category,L4F_INFO,"VDF: proj support this projection: "//trim(proj_type))
      call get_val (this%griddim, xmin=extents(1),ymin=extents(2), xmax=extents(4) , ymax=extents(5),&
       zone=zone, xoff=xoff, yoff=yoff,  ellips_smaj_axis=ellips_smaj_axis, ellips_flatt=ellips_flatt)

      extents(3)=0.d0
      extents(6)=300000.d0

      mapprojection ="+proj=utm  +zone="//t2c(zone)

      !mapprojection ="+proj=utm  +zone="//t2c(zone)//" +a="//t2c(ellips_smaj_axis)//&
      ! " +f="//t2c(ellips_flatt)//" +x_0="//t2c(xoff)//" +y_0="//t2c(yoff)

    case default

      call l4f_category_log(this%category,L4F_WARN,"VDF: proj or vdf export do not support this projection: "//trim(proj_type))
      mapprojection = cmiss

    end select

    call l4f_category_log(this%category,L4F_DEBUG,"VDF: call create_metadata")
    ier = create_metadata(xyzdim)
    call l4f_category_log(this%category,L4F_DEBUG,"VDF: call set_num_timesteps")
    if(ier==0) ier = set_num_timesteps(ntimera)
    call l4f_category_log(this%category,L4F_DEBUG,"VDF: call set_variables_names")
    if(ier==0) ier = set_variables_names(nvar, varnames)

    call l4f_category_log(this%category,L4F_DEBUG,"VDF: call set_v_comment")
    call l4f_category_log(this%category,L4F_DEBUG,"VDF: call set_ts_comment")
    
    do i=1,ntimera
      if(ier==0) ier = vdf4f_set_ts_comment(i-1,tsdescriptions(i))
      do j=1,nvar
        if(ier==0) ier = vdf4f_set_v_comment(i-1,varnames(j),vardescriptions(j))
      end do
    end do


    call l4f_category_log(this%category,L4F_DEBUG,"VDF: call vdf4f_set_comment")
    if(ier==0) ier = vdf4f_set_comment("vogrid6d exported")
    if(ier==0) ier = vdf4f_set_grid_extents(extents=extents)
    !if(ier==0) ier = vdf4f_set_coord_system_type(coordsystemtype="spherical")
    if(ier==0) ier = vdf4f_set_coord_system_type(coordsystemtype="cartesian")
    if(ier==0) ier = vdf4f_set_grid_type(gridtype="regular")
    if(ier==0) ier = vdf4f_set_map_projection(mapprojection=mapprojection)
     
    !!!!  if(ier==0) int vdf4f_set_grid_permutation(long permutation[3]);
    call l4f_category_log(this%category,L4F_DEBUG,"VDF: call write_metadata")
    if(ier==0) ier = write_metadata(lfilename)
    call l4f_category_log(this%category,L4F_DEBUG,"VDF: call vdf4f_write")

    if (this%time_definition == 1) then

      if (ntimerange /= 1) then
        call l4f_category_log(this%category,L4F_WARN,"VDF: writing only fisth timerange, there are:"//t2c(ntimerange))
      end if

      call l4f_category_log(this%category,L4F_INFO,"scan VDF (vapor file) for times")
      if(ier==0) ier = vdf4f_write(this%voldati(:,:,:,:,1,:), xyzdim, ntime, nvar, varnames, lfilename, &
       irzscan)  

    else

      if (ntime /= 1) then
        call l4f_category_log(this%category,L4F_WARN,"VDF: writing only fisth time, there are:"//t2c(ntime))
      end if

      call l4f_category_log(this%category,L4F_INFO,"scan VDF (vapor file) for timeranges")
      if(ier==0) ier = vdf4f_write(this%voldati(:,:,:,1,:,:), xyzdim, ntimerange, nvar, varnames, lfilename, &
       irzscan)  

    end if

    if (ier /= 0) then
      call l4f_category_log(this%category,L4F_ERROR,"export to vdf: "//get_err_msg())
    end if

    !todo: check if all are allocated
    deallocate(varnames,vardescriptions,tsdescriptions,varbufr)

    retstat = destroy_metadata_c()
    retstat = destroy_writer_c()

    if (ier /= 0) then
      call l4f_category_log(this%category,L4F_ERROR,"exporting to vdf")
      CALL raise_fatal_error("exporting to vdf")
    end if

  else

    call l4f_category_log(this%category,L4F_WARN,"volume with voldati not associated: not exported to vdf")

  end if

else

  call l4f_category_log(this%category,L4F_WARN,"volume with some dimensions to 0: not exported to vdf")

end if

end subroutine volgrid6d_export_to_vapor

end module volgrid6d_vapor_class