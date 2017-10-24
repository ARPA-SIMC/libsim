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
!> \defgroup volgrid6d Libsim package, volgrid6d library.
!! The libsim volgrid6d library contains classes for managing data on
!! regular rectangular grids, tipically the output of a numerical
!! weather prediction model, and for their import from a WMO GRIB file
!! of from other geophysical data file formats. In order to compile
!! and link programs using this library, you have to insert the
!! required \c USE statements in the program units involved, specify
!! the location of module files when compiling (tipically \c
!! -I/usr/lib/gfortran/modules or \c -I/usr/lib64/gfortran/modules or
!! \c -I/usr/include) and indicate the library name \c -lsim_volgrid6d
!! when linking, assuming that the library has been installed in a
!! default location.

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
!! Example of transformation from volgrid6d to volgrid6d \include example_vg6d_5.f90
!! Example of transformation from volgrid6d to vol7d \include example_vg6d_6.f90
!! Example of transformation from vol7d to volgrid6d \include example_vg6d_7.f90
!!
!!\ingroup volgrid6d
MODULE volgrid6d_class
USE vol7d_level_class
USE vol7d_timerange_class
USE vol7d_class
USE grid_transform_class
USE geo_proj_class
USE grid_class
USE grid_dim_class
USE datetime_class
USE volgrid6d_var_class
USE log4fortran
USE array_utilities
USE grid_id_class
USE gridinfo_class
USE optional_values
!USE file_utilities
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif
IMPLICIT NONE

character (len=255),parameter:: subcategory="volgrid6d_class"

!> Object describing a rectangular, homogeneous gridded dataset
type volgrid6d
  type(griddim_def) :: griddim !< grid descriptor
  TYPE(datetime),pointer :: time(:) !< time dimension descriptor
  TYPE(vol7d_timerange),pointer :: timerange(:) !< timerange (forecast, analysis, statistically processed) dimension descriptor
  TYPE(vol7d_level),pointer :: level(:) !< vertical level dimension descriptor
  TYPE(volgrid6d_var),pointer :: var(:) !< physical variable dimension descriptor
  TYPE(grid_id),POINTER :: gaid(:,:,:,:) !< array of grid identifiers, carrying information about the driver for import/export from/to file, indices are: (level,time,timerange,var)
  REAL,POINTER :: voldati(:,:,:,:,:,:) !< array of data, indices are: (x,y,level,time,timerange,var)
  integer :: time_definition !< time definition; 0=time is reference time ; 1=time is validity time
  integer :: category !< log4fortran category
end type volgrid6d

!> Constructor, it creates a new instance of the object.
INTERFACE init
  MODULE PROCEDURE volgrid6d_init
END INTERFACE

!> Destructor, it releases every information and memory buffer
!! associated with the object.
INTERFACE delete
  MODULE PROCEDURE volgrid6d_delete, volgrid6dv_delete
END INTERFACE

!> Import an object dirctly from a native file, from a \a gridinfo object
!! or from a supported file format through a \a gridinfo object.
INTERFACE import
  MODULE PROCEDURE volgrid6d_read_from_file
  MODULE PROCEDURE import_from_gridinfo, import_from_gridinfovv, &
   volgrid6d_import_from_file
END INTERFACE

!> Export an object dirctly to a native file, to a \a gridinfo object
!! or to a supported file format through a \a gridinfo object.
INTERFACE export
  MODULE PROCEDURE volgrid6d_write_on_file
  MODULE PROCEDURE export_to_gridinfo, export_to_gridinfov, export_to_gridinfovv,&
   volgrid6d_export_to_file
END INTERFACE

! methods for computing transformations through an initialised
! grid_transform object, probably too low level to be interfaced
INTERFACE compute
  MODULE PROCEDURE volgrid6d_transform_compute, volgrid6d_v7d_transform_compute,&
   v7d_volgrid6d_transform_compute, v7d_v7d_transform_compute
END INTERFACE

!> Transform between any combination of \a volgrid6d and \a vol7d objects
!! by means of a \a transform_def object describing the transformation.
INTERFACE transform
  MODULE PROCEDURE volgrid6d_transform, volgrid6dv_transform,&
   volgrid6d_v7d_transform, volgrid6dv_v7d_transform, v7d_volgrid6d_transform, &
   v7d_v7d_transform
END INTERFACE

INTERFACE wind_rot
  MODULE PROCEDURE vg6d_wind_rot
END INTERFACE

INTERFACE wind_unrot
  MODULE PROCEDURE vg6d_wind_unrot
END INTERFACE

!> Display on standard output a description of the \a volgrid6d object
!! provided.
INTERFACE display
  MODULE PROCEDURE display_volgrid6d,display_volgrid6dv
END INTERFACE

!> Reduce some dimensions (level and timerage) for semplification (rounding).
!! You can use this for simplify and use variables in computation like alchimia
!! where fields have to be on the same coordinate
!! examples:
!! means in time for short periods and istantaneous values
!! 2 meter and surface levels
!! If there are data on more then one almost equal levels or timeranges, the first var present (at least one point)
!! will be taken (order is by icreasing var index).
!! You can use predefined values for classic semplification
!! almost_equal_levels and almost_equal_timeranges
!! The level or timerange in output will be defined by the first element of level and timerange list
INTERFACE rounding
  MODULE PROCEDURE vg6d_rounding, vg6dv_rounding
END INTERFACE

private

PUBLIC volgrid6d,init,delete,export,import,compute,transform, &
 wind_rot,wind_unrot,vg6d_c2a,display,volgrid6d_alloc,volgrid6d_alloc_vol, &
 volgrid_get_vol_2d, volgrid_set_vol_2d, volgrid_get_vol_3d, volgrid_set_vol_3d
PUBLIC rounding, vg6d_reduce

CONTAINS


!> Constructor, it creates a new instance of the object.
!! The constructor should be explicitly used only in rare cases,
!! \a volgrid6d objects are usually created through the \a import
!! interface.
SUBROUTINE volgrid6d_init(this, griddim, time_definition, categoryappend)
TYPE(volgrid6d) :: this !< object to be initialized
TYPE(griddim_def),OPTIONAL :: griddim !< grid descriptor
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< 0=time is reference time; 1=time is validity time
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

character(len=512) :: a_name

if (present(categoryappend))then
   call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
   call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
this%category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"init")
#endif

call init(this%griddim)

if (present(griddim))then
  call copy (griddim,this%griddim)
end if

 ! call init(this%time)         
 ! call init(this%timerange)    
 ! call init(this%level)        
 ! call init(this%var)          

if(present(time_definition)) then
  this%time_definition = time_definition
else
  this%time_definition = 0 !default to reference time
end if

nullify (this%time,this%timerange,this%level,this%var)
nullify (this%gaid,this%voldati)          

END SUBROUTINE volgrid6d_init


!> Allocate the dimension descriptors of the \a volgrid6d object.
!! This method allocates the horizontal grid descriptor and the one
!! dimensional arrays of the dimensions
!! - time
!! - vertical level
!! - timerange
!! - physical variable
!!
!! This method should be explicitly used only in rare cases, it is
!! usually called implicitly through the \a import interface.
SUBROUTINE volgrid6d_alloc(this, dim, ntime, nlevel, ntimerange, nvar, ini)
TYPE(volgrid6d),INTENT(inout) :: this !< object whose decriptors should be allocated
TYPE(grid_dim),INTENT(in),OPTIONAL :: dim !< horizontal grid size X, Y
INTEGER,INTENT(in),OPTIONAL :: ntime !< number of time levels
INTEGER,INTENT(in),OPTIONAL :: nlevel !< number of vertical levels
INTEGER,INTENT(in),OPTIONAL :: ntimerange !< number of different timeranges
INTEGER,INTENT(in),OPTIONAL :: nvar !< number of physical variables
LOGICAL,INTENT(in),OPTIONAL :: ini !< if provided and \c .TRUE., for each allocated dimension descriptor the constructor is called without extra parameters, thus initializing everything as missing value

INTEGER :: i, stallo
LOGICAL :: linit

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"alloc")
#endif

IF (PRESENT(ini)) THEN
  linit = ini
ELSE
  linit = .FALSE.
ENDIF


if (present(dim)) call copy (dim,this%griddim%dim)


IF (PRESENT(ntime)) THEN
  IF (ntime >= 0) THEN
    IF (ASSOCIATED(this%time)) DEALLOCATE(this%time)
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"alloc ntime "//to_char(ntime))
#endif
    ALLOCATE(this%time(ntime),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(this%category,L4F_FATAL,"allocating memory")
      CALL raise_fatal_error()
    end if
    IF (linit) THEN
      DO i = 1, ntime
        this%time(i) = datetime_miss
!        CALL init(this%time(i)) ! senza argomento inizializza a zero non missing
                                 ! baco di datetime?
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nlevel)) THEN
  IF (nlevel >= 0) THEN
    IF (ASSOCIATED(this%level)) DEALLOCATE(this%level)
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"alloc nlevel "//to_char(nlevel))
#endif
    ALLOCATE(this%level(nlevel),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(this%category,L4F_FATAL,"allocating memory")
      CALL raise_fatal_error()
    end if
    IF (linit) THEN
      DO i = 1, nlevel
        CALL init(this%level(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(ntimerange)) THEN
  IF (ntimerange >= 0) THEN
    IF (ASSOCIATED(this%timerange)) DEALLOCATE(this%timerange)
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"alloc ntimerange "//to_char(ntimerange))
#endif
    ALLOCATE(this%timerange(ntimerange),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(this%category,L4F_FATAL,"allocating memory")
      CALL raise_fatal_error()
    end if
    IF (linit) THEN
      DO i = 1, ntimerange
        CALL init(this%timerange(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvar)) THEN
  IF (nvar >= 0) THEN
    IF (ASSOCIATED(this%var)) DEALLOCATE(this%var)
#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"alloc nvar "//to_char(nvar))
#endif
    ALLOCATE(this%var(nvar),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(this%category,L4F_FATAL,"allocating memory")
      CALL raise_fatal_error()
    end if
    IF (linit) THEN
      DO i = 1, nvar
        CALL init(this%var(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF

end SUBROUTINE volgrid6d_alloc


!> Allocate the data array of the \a volgrid6d object.
!! This method allocates the main 6-dimensional data array
!! \a this%voldati and the 4-dimensional \a grid_id array \a this%gaid
!! with a shape dictated by the previous call(s) to \a vol7d_alloc().
!! if any descriptor (except horizontal grid) has not been allocated
!! yet, it is allocated here with a size of 1.  This method should be
!! explicitly used only in rare cases, it is usually called implicitly
!! through the \a import interface.
SUBROUTINE volgrid6d_alloc_vol(this, ini, inivol, decode)
TYPE(volgrid6d),INTENT(inout) :: this !< object whose decriptors should be allocated
LOGICAL,INTENT(in),OPTIONAL :: ini !< if provided and \c .TRUE., for each dimension descriptor not yet allocated and allocated here the constructor is called without extra parameters, thus initializing the element as missing value
LOGICAL,INTENT(in),OPTIONAL :: inivol !< if provided and \c .FALSE., the allocated volumes will not be initialized to missing values
LOGICAL,INTENT(in),OPTIONAL :: decode !< if provided and \c .TRUE., the \a this%voldati volume is allocated, otherwise only \a this%gaid will be allocated

INTEGER :: stallo
LOGICAL :: linivol

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"start alloc_vol")
#endif

IF (PRESENT(inivol)) THEN ! opposite condition, cannot use optio_log
  linivol = inivol
ELSE
  linivol = .TRUE.
ENDIF

IF (this%griddim%dim%nx > 0 .AND. this%griddim%dim%ny > 0) THEN
! allocate dimension descriptors to a minimal size for having a
! non-null volume
  IF (.NOT. ASSOCIATED(this%var)) CALL volgrid6d_alloc(this, nvar=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%time)) CALL volgrid6d_alloc(this, ntime=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%level)) CALL volgrid6d_alloc(this, nlevel=1, ini=ini)
  IF (.NOT. ASSOCIATED(this%timerange)) CALL volgrid6d_alloc(this, ntimerange=1, ini=ini)
  
  IF (optio_log(decode)) THEN
    IF (.NOT.ASSOCIATED(this%voldati)) THEN
#ifdef DEBUG
      CALL l4f_category_log(this%category,L4F_DEBUG,"alloc voldati volume")
#endif

      ALLOCATE(this%voldati(this%griddim%dim%nx,this%griddim%dim%ny,&
       SIZE(this%level), SIZE(this%time), &
       SIZE(this%timerange), SIZE(this%var)),stat=stallo)
      IF (stallo /= 0)THEN
        CALL l4f_category_log(this%category,L4F_FATAL,"allocating memory")
        CALL raise_fatal_error()
      ENDIF
  
! this is not really needed if we can check other flags for a full
! field missing values
      IF (linivol) this%voldati = rmiss
      this%voldati = rmiss
    ENDIF
  ENDIF

  IF (.NOT.ASSOCIATED(this%gaid)) THEN
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,"alloc gaid volume")
#endif
    ALLOCATE(this%gaid(SIZE(this%level), SIZE(this%time), &
     SIZE(this%timerange), SIZE(this%var)),stat=stallo)
    IF (stallo /= 0)THEN
      CALL l4f_category_log(this%category,L4F_FATAL,"allocating memory")
      CALL raise_fatal_error()
    ENDIF

    IF (linivol) THEN
!!$    DO i=1 ,SIZE(this%gaid,1)
!!$      DO ii=1 ,SIZE(this%gaid,2)
!!$        DO iii=1 ,SIZE(this%gaid,3)
!!$          DO iiii=1 ,SIZE(this%gaid,4)
!!$            this%gaid(i,ii,iii,iiii) = grid_id_new() ! optimize?
!!$          ENDDO
!!$        ENDDO
!!$      ENDDO
!!$    ENDDO

      this%gaid = grid_id_new()
    ENDIF
  ENDIF
  
ELSE
  CALL l4f_category_log(this%category,L4F_FATAL,'volgrid6d_alloc_vol: &
   &trying to allocate a volume with an invalid or unspecified horizontal grid')
  CALL raise_fatal_error()
ENDIF

END SUBROUTINE volgrid6d_alloc_vol


!> Return a 2-d pointer to a x-y slice of a volume. This method works
!! both with volumes having allocated and non-allocated this%voldati
!! array, and it returns a pointer to a 2-d slice either from the
!! allocated this%voldati array or from the grid_id object on file or
!! in memory. In the second case the pointer should be either
!! ALLOCATE'd to the expected size or NULLIFY'ed, and if NULLIFY'ed,
!! it is allocated within the method, thus it will have to be
!! deallocated by the caller when not in use anymore. Since this
!! method may be called many times by a program, it is optimized for
!! speed and it does not make any check about the matching size of the
!! pointer and the array or about the allocation status of \a this, so
!! it should be called only when everything has been checked to be in
!! good shape.
SUBROUTINE volgrid_get_vol_2d(this, ilevel, itime, itimerange, ivar, voldati)
TYPE(volgrid6d),INTENT(in) :: this !< object from which the slice has to be retrieved
INTEGER,INTENT(in) :: ilevel !< index of vertical level of the slice
INTEGER,INTENT(in) :: itime !< index of time level of the slice
INTEGER,INTENT(in) :: itimerange !< index of timerange of the slice
INTEGER,INTENT(in) :: ivar !< index of physical variable of the slice
REAL,POINTER :: voldati(:,:) !< pointer to the data, if \a this%voldati is already allocated, it will just point to the requested slice, otherwise it will be allocated if and only if it is nullified on entry

IF (ASSOCIATED(this%voldati)) THEN
  voldati => this%voldati(:,:,ilevel,itime,itimerange,ivar)
  RETURN
ELSE
  IF (.NOT.ASSOCIATED(voldati)) THEN
    ALLOCATE(voldati(this%griddim%dim%nx,this%griddim%dim%ny))
  ENDIF
  CALL grid_id_decode_data(this%gaid(ilevel,itime,itimerange,ivar), voldati)
ENDIF

END SUBROUTINE volgrid_get_vol_2d


!> Return a 3-d pointer to a x-y-z slice of a volume. This method works
!! both with volumes having allocated and non-allocated this%voldati
!! array, and it returns a pointer to a 3-d slice either from the
!! allocated this%voldati array or from the grid_id object on file or
!! in memory. In the second case the pointer should be either
!! ALLOCATE'd to the expected size or NULLIFY'ed, and if NULLIFY'ed,
!! it is allocated within the method, thus it will have to be
!! deallocated by the caller when not in use anymore. Since this
!! method may be called many times by a program, it is optimized for
!! speed and it does not make any check about the matching size of the
!! pointer and the array or about the allocation status of \a this, so
!! it should be called only when everything has been checked to be in
!! good shape.
SUBROUTINE volgrid_get_vol_3d(this, itime, itimerange, ivar, voldati)
TYPE(volgrid6d),INTENT(in) :: this !< object from which the slice has to be retrieved
INTEGER,INTENT(in) :: itime !< index of time level of the slice
INTEGER,INTENT(in) :: itimerange !< index of timerange of the slice
INTEGER,INTENT(in) :: ivar !< index of physical variable of the slice
REAL,POINTER :: voldati(:,:,:) !< pointer to the data, if \a this%voldati is already allocated, it will just point to the requested slice, otherwise it will be allocated if and only if it is nullified on entry

INTEGER :: ilevel

IF (ASSOCIATED(this%voldati)) THEN
  voldati => this%voldati(:,:,:,itime,itimerange,ivar)
  RETURN
ELSE
  IF (.NOT.ASSOCIATED(voldati)) THEN
    ALLOCATE(voldati(this%griddim%dim%nx,this%griddim%dim%ny,SIZE(this%level)))
  ENDIF
  DO ilevel = 1, SIZE(this%level)
    CALL grid_id_decode_data(this%gaid(ilevel,itime,itimerange,ivar), &
     voldati(:,:,ilevel))
  ENDDO
ENDIF

END SUBROUTINE volgrid_get_vol_3d


!> Reset a 2-d x-y slice of a volume after the data have been modified.
!! This method works both with volumes having allocated and
!! non-allocated this%voldati array, and it updates the requested
!! slice.  In case \a this%voldati is already allocated, this is a
!! no-operation while in the other case this method encodes the filed
!! provided into the grid_id object on file or in memory. Since this
!! method may be called many times by a program, it is optimized for
!! speed and it does not make any check about the matching size of the
!! field and the array or about the allocation status of \a this, so
!! it should be called only when everything has been checked to be in
!! good shape.
SUBROUTINE volgrid_set_vol_2d(this, ilevel, itime, itimerange, ivar, voldati)
TYPE(volgrid6d),INTENT(inout) :: this !< object in which slice has to be updated
INTEGER,INTENT(in) :: ilevel !< index of vertical level of the slice
INTEGER,INTENT(in) :: itime !< index of time level of the slice
INTEGER,INTENT(in) :: itimerange !< index of timerange of the slice
INTEGER,INTENT(in) :: ivar !< index of physical variable of the slice
REAL,INTENT(in) :: voldati(:,:) !< updated values of the slice

IF (ASSOCIATED(this%voldati)) THEN
  RETURN
ELSE
  CALL grid_id_encode_data(this%gaid(ilevel,itime,itimerange,ivar), voldati)
ENDIF

END SUBROUTINE volgrid_set_vol_2d


!> Reset a 3-d x-y-z slice of a volume after the data have been modified.
!! This method works both with volumes having allocated and
!! non-allocated this%voldati array, and it updates the requested
!! slice.  In case \a this%voldati is already allocated, this is a
!! no-operation while in the other case this method encodes the filed
!! provided into the grid_id object on file or in memory. Since this
!! method may be called many times by a program, it is optimized for
!! speed and it does not make any check about the matching size of the
!! field and the array or about the allocation status of \a this, so
!! it should be called only when everything has been checked to be in
!! good shape.
SUBROUTINE volgrid_set_vol_3d(this, itime, itimerange, ivar, voldati)
TYPE(volgrid6d),INTENT(inout) :: this !< object in which slice has to be updated
INTEGER,INTENT(in) :: itime !< index of time level of the slice
INTEGER,INTENT(in) :: itimerange !< index of timerange of the slice
INTEGER,INTENT(in) :: ivar !< index of physical variable of the slice
REAL,INTENT(in) :: voldati(:,:,:) !< updated values of the slice

INTEGER :: ilevel

IF (ASSOCIATED(this%voldati)) THEN
  RETURN
ELSE
  DO ilevel = 1, SIZE(this%level)
    CALL grid_id_encode_data(this%gaid(ilevel,itime,itimerange,ivar), &
     voldati(:,:,ilevel))
  ENDDO
ENDIF

END SUBROUTINE volgrid_set_vol_3d


!> Destructor, it releases every information and memory buffer
!! associated with the object. It should be called also for objects
!! crated through the \a import interface.
SUBROUTINE volgrid6d_delete(this)
TYPE(volgrid6d),INTENT(inout) :: this

INTEGER :: i, ii, iii, iiii

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"delete")
#endif

if (associated(this%gaid))then

  DO i=1 ,SIZE(this%gaid,1)
    DO ii=1 ,SIZE(this%gaid,2)
      DO iii=1 ,SIZE(this%gaid,3)
        DO iiii=1 ,SIZE(this%gaid,4)
          CALL delete(this%gaid(i,ii,iii,iiii))
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  DEALLOCATE(this%gaid)

end if

call delete(this%griddim)

!  call delete(this%time)
!  call delete(this%timerange)
!  call delete(this%level)
!  call delete(this%var)

if (associated( this%time )) deallocate(this%time)
if (associated( this%timerange )) deallocate(this%timerange)
if (associated( this%level )) deallocate(this%level)
if (associated( this%var )) deallocate(this%var)

if (associated(this%voldati))deallocate(this%voldati)


                                !chiudo il logger
call l4f_category_delete(this%category)
  
END SUBROUTINE volgrid6d_delete


!>\brief Scrittura su file di un volume Volgrid6d.
!! Scrittura su file unformatted di un intero volume Volgrid6d.
!! Il volume viene serializzato e scritto su file.
!! Il file puo' essere aperto opzionalmente dall'utente. Si possono passare
!! opzionalmente unità e nome del file altrimenti assegnati internamente a dei default; se assegnati internamente 
!! tali parametri saranno in output.
!! Se non viene fornito il nome file viene utilizzato un file di default con nome pari al nome del programma in 
!! esecuzione con postfisso ".vg6d".
!! Come parametro opzionale c'è la description che insieme alla data corrente viene inserita nell'header del file.
subroutine volgrid6d_write_on_file (this,unit,description,filename,filename_auto)

TYPE(volgrid6d),INTENT(IN) :: this !< volume volgrid6d da scrivere 
integer,optional,intent(inout) :: unit !< unità su cui scrivere; se passata =0 ritorna il valore rielaborato (default =rielaborato internamente con getlun )
character(len=*),intent(in),optional :: filename !< nome del file su cui scrivere
character(len=*),intent(out),optional :: filename_auto !< nome del file generato se "filename" è omesso
character(len=*),INTENT(IN),optional :: description !< descrizione del volume

integer :: lunit
character(len=254) :: ldescription,arg,lfilename
integer :: ntime, ntimerange, nlevel, nvar
integer :: tarray(8)
logical :: opened,exist

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"write on file")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0

!call idate(im,id,iy)
call date_and_time(values=tarray)
call getarg(0,arg)

if (present(description))then
  ldescription=description
else
  ldescription="Volgrid6d generated by: "//trim(arg)
end if

if (.not. present(unit))then
  lunit=getunit()
else
  if (unit==0)then
    lunit=getunit()
    unit=lunit
  else
    lunit=unit
  end if
end if

lfilename=trim(arg)//".vg6d"
if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if

if (present(filename_auto))filename_auto=lfilename


inquire(unit=lunit,opened=opened)
if (.not. opened) then 
  inquire(file=lfilename,EXIST=exist)
  if (exist) CALL raise_error('file exist; cannot open new file')
  if (.not.exist) open (unit=lunit,file=lfilename,form="UNFORMATTED")
  !print *, "opened: ",lfilename
end if

if (associated(this%time)) ntime=size(this%time)
if (associated(this%timerange)) ntimerange=size(this%timerange)
if (associated(this%level)) nlevel=size(this%level)
if (associated(this%var)) nvar=size(this%var)


write(unit=lunit)ldescription
write(unit=lunit)tarray

call write_unit( this%griddim,lunit)
write(unit=lunit) ntime, ntimerange, nlevel, nvar

!! prime 4 dimensioni
if (associated(this%time))      call write_unit(this%time, lunit)
if (associated(this%level))     write(unit=lunit)this%level
if (associated(this%timerange)) write(unit=lunit)this%timerange
if (associated(this%var))       write(unit=lunit)this%var    


!! Volumi di valori dati

if (associated(this%voldati))     write(unit=lunit)this%voldati

if (.not. present(unit)) close(unit=lunit)

end subroutine volgrid6d_write_on_file


!>\brief Lettura da file di un volume Volgrid6d.
!! Lettura da file unformatted di un intero volume Volgrid6d.
!! Questa subroutine comprende volgrid6d_alloc e volgrid6d_alloc_vol.
!! Il file puo' essere aperto opzionalmente dall'utente. Si possono passare
!! opzionalmente unità e nome del file altrimenti assegnati internamente a dei default; se assegnati internamente 
!! tali parametri saranno in output.
subroutine volgrid6d_read_from_file (this,unit,filename,description,tarray,filename_auto)

TYPE(volgrid6d),INTENT(OUT) :: this !< Volume volgrid6d da leggere
integer,intent(inout),optional :: unit !< unità su cui è stato aperto un file; se =0 rielaborato internamente (default = elaborato internamente con getunit)
character(len=*),INTENT(in),optional :: filename !< nome del file eventualmente da aprire (default = nome dell'eseguibile.v7d)
character(len=*),intent(out),optional :: filename_auto !< nome del file generato se "filename" è omesso
character(len=*),INTENT(out),optional :: description !< descrizione del volume letto
integer,intent(out),optional :: tarray(8) !< vettore come definito da "date_and_time" della data di scrittura del volume

integer :: ntime, ntimerange, nlevel, nvar

character(len=254) :: ldescription,lfilename,arg
integer :: ltarray(8),lunit
logical :: opened,exist

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"read from file")
#endif

call getarg(0,arg)

if (.not. present(unit))then
  lunit=getunit()
else
  if (unit==0)then
    lunit=getunit()
    unit=lunit
  else
    lunit=unit
  end if
end if

lfilename=trim(arg)//".vg6d"
if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

if (present(filename))then
  if (filename /= "")then
    lfilename=filename
  end if
end if

if (present(filename_auto))filename_auto=lfilename


inquire(unit=lunit,opened=opened)
if (.not. opened) then 
  inquire(file=lfilename,EXIST=exist)
  IF (.NOT. exist) CALL raise_fatal_error('file '//TRIM(lfilename)//' does not exist, cannot open')
  open (unit=lunit,file=lfilename,form="UNFORMATTED")
end if

read(unit=lunit)ldescription
read(unit=lunit)ltarray

call l4f_log(L4F_INFO,"Info: reading volgrid6d from file: "//trim(lfilename))
call l4f_log(L4F_INFO,"Info: description: "//trim(ldescription))
!call l4f_log("Info: written on ",ltarray)

if (present(description))description=ldescription
if (present(tarray))tarray=ltarray


call read_unit( this%griddim,lunit)
read(unit=lunit) ntime, ntimerange, nlevel, nvar


call volgrid6d_alloc (this, &
 ntime=ntime, ntimerange=ntimerange, nlevel=nlevel, nvar=nvar)

call volgrid6d_alloc_vol (this)

if (associated(this%time))      call read_unit(this%time, lunit)
if (associated(this%level))     read(unit=lunit)this%level
if (associated(this%timerange)) read(unit=lunit)this%timerange
if (associated(this%var))       read(unit=lunit)this%var    


!! Volumi di valori 

if (associated(this%voldati))     read(unit=lunit)this%voldati

if (.not. present(unit)) close(unit=lunit)

end subroutine volgrid6d_read_from_file


!> Import a single \a gridinfo object into a \a volgrid6d object.
!! This methods imports a single gridded field from a \a gridinfo
!! object into a \a volgrid6d object, inserting it into the
!! multidimensional structure of volgrid6d. The volgrid6d object must
!! have been already initialized and the dimensions specified with
!! volgrid6d_alloc(). If the \a force argument is missing or \a
!! .FALSE. , the volgrid6d object dimension descriptors (time,
!! timerange, vertical level, physical variable) must already have
!! space for the corresponding values coming from gridinfo, otherwise
!! the object will be rejected; this means that all the volgrid6d
!! dimension descriptors should be correctly assigned. If \a force is
!! \a .TRUE. , the gridinfo dimension descriptors that do not fit into
!! available descriptors in the \a volgrid6d structure, will be
!! accomodated in a empty (i.e. equal to missing value) descriptor, if
!! available, otherwise the gridinfo will be rejected.  The
!! descriptor of the grid in the \a volgrid object is assigned to the
!! descriptor contained in \a gridinfo if it is missing in \a volgrid,
!! otherwise it is checked and the object is rejected if grids do not
!! match.
SUBROUTINE import_from_gridinfo(this, gridinfo, force, dup_mode, clone, &
 isanavar)
TYPE(volgrid6d),INTENT(inout) :: this !< object in which to import
TYPE(gridinfo_def),INTENT(in) :: gridinfo !< gridinfo object to be imported
LOGICAL,INTENT(in),OPTIONAL :: force !< if provided and \c .TRUE., the gridinfo is forced into an empty element of \a this, if required and possible
INTEGER,INTENT(in),OPTIONAL :: dup_mode !< determines the behavior in case of duplicate metadata: if \a dup_mode is not provided or 0, a duplicate field overwrites, if \a dup_mode is 1, duplicate fields are merged with priority to the last
LOGICAL , INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE. , clone the gaid's from \a gridinfo to \a this
LOGICAL,INTENT(IN),OPTIONAL :: isanavar !< if provides and \a .TRUE., the gridinfo object is treated as time-independent and replicated for every time and timerange

CHARACTER(len=255) :: type
INTEGER :: itime0, itimerange0, itime1, itimerange1, itime, itimerange, &
 ilevel, ivar, ldup_mode
LOGICAL :: dup
TYPE(datetime) :: correctedtime
REAL,ALLOCATABLE :: tmpgrid(:,:)

IF (PRESENT(dup_mode)) THEN
  ldup_mode = dup_mode
ELSE
  ldup_mode = 0
ENDIF

call get_val(this%griddim,proj_type=type)

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"import_from_gridinfo: "//trim(type))
#endif

if (.not. c_e(type))then
  call copy(gridinfo%griddim, this%griddim)
! ho gia` fatto init, altrimenti non potrei fare la get_val(this%griddim)
! per cui meglio non ripetere
!   call init(this,gridinfo%griddim,categoryappend)
  CALL volgrid6d_alloc_vol(this, ini=.TRUE.) ! decode?

else if (.not. (this%griddim == gridinfo%griddim ))then

  CALL l4f_category_log(this%category,L4F_ERROR, &
   "volgrid and gridinfo grid type or size are different, gridinfo rejected")
  CALL raise_error()
  RETURN

end if

! Cerco gli indici del campo da inserire, se non trovo metto nel primo missing
ilevel = index(this%level, gridinfo%level)
IF (ilevel == 0 .AND. optio_log(force)) THEN
  ilevel = index(this%level, vol7d_level_miss)
  IF (ilevel /= 0) this%level(ilevel) = gridinfo%level
ENDIF

IF (ilevel == 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "volgrid6d: level not valid for volume, gridinfo rejected")
  CALL raise_error()
  RETURN
ENDIF

IF (optio_log(isanavar)) THEN ! assign to all times and timeranges
  itime0 = 1
  itime1 = SIZE(this%time)
  itimerange0 = 1
  itimerange1 = SIZE(this%timerange)
ELSE ! usual case
  correctedtime = gridinfo%time
  IF (this%time_definition == 1) correctedtime = correctedtime + &
   timedelta_new(sec=gridinfo%timerange%p1)
  itime0 = index(this%time, correctedtime)
  IF (itime0 == 0 .AND. optio_log(force)) THEN
    itime0 = index(this%time, datetime_miss)
    IF (itime0 /= 0) this%time(itime0) = correctedtime
  ENDIF
  IF (itime0 == 0) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     "volgrid6d: time not valid for volume, gridinfo rejected")
    CALL raise_error()
    RETURN
  ENDIF
  itime1 = itime0

  itimerange0 = index(this%timerange,gridinfo%timerange)
  IF (itimerange0 == 0 .AND. optio_log(force)) THEN
    itimerange0 = index(this%timerange, vol7d_timerange_miss)
    IF (itimerange0 /= 0) this%timerange(itimerange0) = gridinfo%timerange
  ENDIF
  IF (itimerange0 == 0) THEN
    CALL l4f_category_log(this%category,L4F_ERROR, &
     "volgrid6d: timerange not valid for volume, gridinfo rejected")
    CALL raise_error()
    RETURN
  ENDIF
  itimerange1 = itimerange0
ENDIF

ivar = index(this%var, gridinfo%var)
IF (ivar == 0 .AND. optio_log(force)) THEN
  ivar = index(this%var, volgrid6d_var_miss)
  IF (ivar /= 0) this%var(ivar) = gridinfo%var
ENDIF
IF (ivar == 0) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "volgrid6d: var not valid for volume, gridinfo rejected")
  CALL raise_error()
  RETURN
ENDIF

DO itimerange = itimerange0, itimerange1
  DO itime = itime0, itime1
    IF (ASSOCIATED(this%gaid)) THEN
      dup = .FALSE.
      IF (c_e(this%gaid(ilevel,itime,itimerange,ivar))) THEN
        dup = .TRUE.
        CALL l4f_category_log(this%category,L4F_WARN,"gaid exist: grib duplicated")
! avoid memory leaks
        IF (optio_log(clone)) CALL delete(this%gaid(ilevel,itime,itimerange,ivar))
      ENDIF

      IF (optio_log(clone)) THEN
        CALL copy(gridinfo%gaid, this%gaid(ilevel,itime,itimerange,ivar))
#ifdef DEBUG
        CALL l4f_category_log(this%category,L4F_DEBUG,"cloning to a new gaid")
#endif
      ELSE
        this%gaid(ilevel,itime,itimerange,ivar) = gridinfo%gaid
      ENDIF

      IF (ASSOCIATED(this%voldati))THEN
        IF (.NOT.dup .OR. ldup_mode == 0) THEN
          this%voldati(:,:,ilevel,itime,itimerange,ivar) = decode_gridinfo(gridinfo)
        ELSE IF (ldup_mode == 1) THEN
          tmpgrid = decode_gridinfo(gridinfo) ! f2003 automatic allocation
          WHERE(c_e(tmpgrid))
            this%voldati(:,:,ilevel,itime,itimerange,ivar) = tmpgrid(:,:)
          END WHERE
        ELSE IF (ldup_mode == 2) THEN
          WHERE(.NOT.c_e(this%voldati(:,:,ilevel,itime,itimerange,ivar)))
            this%voldati(:,:,ilevel,itime,itimerange,ivar) = decode_gridinfo(gridinfo)
          END WHERE
        ENDIF
      ENDIF

    ELSE
      CALL l4f_category_log(this%category,L4F_ERROR, &
       "gaid not allocated, you probably need to call volgrid6d_alloc_vol first")
      CALL raise_error()
      RETURN
    ENDIF
  ENDDO
ENDDO


END SUBROUTINE import_from_gridinfo


!> Export a single grid of a \a volgrid6d object to a \a gridinfo_def object.
!! A single 2d slice of a \a volgrid6d at a specified location is
!! written into a \a gridinfo_def object, including the \a grid_id
!! which can be used for the successive export to file.
SUBROUTINE export_to_gridinfo(this, gridinfo, itime, itimerange, ilevel, ivar, &
 gaid_template, clone)
TYPE(volgrid6d),INTENT(in) :: this !< volume to be exported
TYPE(gridinfo_def),INTENT(inout) :: gridinfo !< output gridinfo_def object
INTEGER :: itime !< index within \a this of the element to be exported for the time dimension
INTEGER :: itimerange !< index within \a this of the element to be exported for the timerange dimension
INTEGER :: ilevel !< index within \a this of the element to be exported for the vertical level dimension
INTEGER :: ivar !< index within \a this of the element to be exported for the variable dimension
TYPE(grid_id),INTENT(in),OPTIONAL :: gaid_template !< \a grid_id template to be used for output data replacing the one contained in \a this
LOGICAL,INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE., clone the grid_id included in \a this rather than making a shallow copy

TYPE(grid_id) :: gaid
LOGICAL :: usetemplate
REAL,POINTER :: voldati(:,:)
TYPE(datetime) :: correctedtime

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,"export_to_gridinfo")
#endif

IF (.NOT.c_e(this%gaid(ilevel,itime,itimerange,ivar))) THEN
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,"empty gaid found, skipping export")
#endif
  RETURN
ENDIF

usetemplate = .FALSE.
IF (PRESENT(gaid_template)) THEN
  CALL copy(gaid_template, gaid)
#ifdef DEBUG
  CALL l4f_category_log(this%category,L4F_DEBUG,"template cloned to a new gaid")
#endif
  usetemplate = c_e(gaid)
ENDIF

IF (.NOT.usetemplate) THEN
  IF (optio_log(clone)) THEN
    CALL copy(this%gaid(ilevel,itime,itimerange,ivar), gaid)
#ifdef DEBUG
    CALL l4f_category_log(this%category,L4F_DEBUG,"original gaid cloned to a new one")
#endif
  ELSE
    gaid = this%gaid(ilevel,itime,itimerange,ivar)
  ENDIF
ENDIF

IF (this%time_definition == 1) THEN
  correctedtime = this%time(itime) - &
   timedelta_new(sec=this%timerange(itimerange)%p1)
ELSE
  correctedtime = this%time(itime)
ENDIF

CALL init(gridinfo,gaid, this%griddim, correctedtime, this%timerange(itimerange), &
 this%level(ilevel), this%var(ivar))

! reset the gridinfo, bad but necessary at this point for encoding the field
CALL export(gridinfo%griddim, gridinfo%gaid)
! encode the field
IF (ASSOCIATED(this%voldati)) THEN
  CALL encode_gridinfo(gridinfo, this%voldati(:,:,ilevel,itime,itimerange,ivar))
ELSE IF (usetemplate) THEN ! field must be forced into template in this case
  NULLIFY(voldati)
  CALL volgrid_get_vol_2d(this, ilevel, itime, itimerange, ivar, voldati)
  CALL encode_gridinfo(gridinfo, voldati)
  DEALLOCATE(voldati)
ENDIF

END SUBROUTINE export_to_gridinfo


!> Import an array of \a gridinfo objects into an array of \a
!! volgrid6d objects.
!! This method imports an array of gridded fields from an \a
!! arrayof_gridinfo object into a suitable number of \a volgrid6d
!! objects. The number of \a volgrid6d allocated is determined by the
!! number of different grids encountered in \a arrayof_gridinfo.
!! Unlike the import for a single \a gridinfo, here the \a volgrid6d
!! object is a non-associated pointer to a 1-d array of uninitialized
!! objects, and all the dimension descriptors in each of the objects
!! are allocated and assigned within the method according to the data
!! contained in \a gridinfov.
!! If the \a anavar array argument is provided, all the input messages
!! whose variable maps to one of the B-table variables contained in \a
!! anavar are treated as time-independent (AKA anagraphic data,
!! station data, etc.), thus their time and timerange are ignored and
!! they are replicated for every time and timerange present in the
!! corresponding data volume.
SUBROUTINE import_from_gridinfovv(this, gridinfov, dup_mode, clone, decode, &
 time_definition, anavar, categoryappend)
TYPE(volgrid6d),POINTER :: this(:) !< object in which to import
TYPE(arrayof_gridinfo),INTENT(in) :: gridinfov !< array of gridinfo objects to be imported
INTEGER,INTENT(in),OPTIONAL :: dup_mode !< determines the behavior in case of duplicate metadata: if \a dup_mode is not provided or 0, a duplicate field overwrites, if \a dup_mode is 1, duplicate fields are merged with priority to the last
LOGICAL , INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE. , clone the gaid's from \a gridinfo to \a this
LOGICAL,INTENT(in),OPTIONAL :: decode !< if provided and \a .FALSE. the data volume in the elements of \a this is not allocated and successive work will be performed on grid_id's
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< 0=time is reference time; 1=time is validity time
CHARACTER(len=*),INTENT(IN),OPTIONAL :: anavar(:) !< list of variables (B-table code equivalent) to be treated as time-independent data
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: i, j, stallo
INTEGER :: ngrid, ntime, ntimerange, nlevel, nvar
INTEGER :: category
CHARACTER(len=512) :: a_name
TYPE(datetime),ALLOCATABLE :: correctedtime(:)
LOGICAL,ALLOCATABLE :: isanavar(:)
TYPE(vol7d_var) :: lvar

! category temporanea (altrimenti non possiamo loggare)
if (present(categoryappend))then
  call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
  call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
category=l4f_category_get(a_name)

#ifdef DEBUG
call l4f_category_log(category,L4F_DEBUG,"start import_from_gridinfovv")
#endif

ngrid=count_distinct(gridinfov%array(1:gridinfov%arraysize)%griddim,back=.true.)
CALL l4f_category_log(category,L4F_INFO, t2c(ngrid)// &
 ' different grid definition(s) found in input data')

ALLOCATE(this(ngrid),stat=stallo)
IF (stallo /= 0)THEN
  CALL l4f_category_log(category,L4F_FATAL,"allocating memory")
  CALL raise_fatal_error()
ENDIF
DO i = 1, ngrid
  IF (PRESENT(categoryappend))THEN
    CALL init(this(i), time_definition=time_definition, categoryappend=TRIM(categoryappend)//"-vol"//t2c(i))
  ELSE
    CALL init(this(i), time_definition=time_definition, categoryappend="vol"//t2c(i))
  ENDIF
ENDDO

this(:)%griddim=pack_distinct(gridinfov%array(1:gridinfov%arraysize)%griddim, &
 ngrid, back=.TRUE.)

! mark elements as ana variables (time-independent)
ALLOCATE(isanavar(gridinfov%arraysize))
isanavar(:) = .FALSE.
IF (PRESENT(anavar)) THEN
  DO i = 1, gridinfov%arraysize
    DO j = 1, SIZE(anavar)
      lvar = convert(gridinfov%array(i)%var)
      IF (lvar%btable == anavar(j)) THEN
        isanavar(i) = .TRUE.
        EXIT
      ENDIF
    ENDDO
  ENDDO
  CALL l4f_category_log(category,L4F_INFO,t2c(COUNT(isanavar))//'/'// &
   t2c(gridinfov%arraysize)//' constant-data messages found in input data')
ENDIF

! create time corrected for time_definition
ALLOCATE(correctedtime(gridinfov%arraysize))
correctedtime(:) = gridinfov%array(1:gridinfov%arraysize)%time
IF (PRESENT(time_definition)) THEN
  IF (time_definition == 1) THEN
    DO i = 1, gridinfov%arraysize
      correctedtime(i) = correctedtime(i) + &
       timedelta_new(sec=gridinfov%array(i)%timerange%p1)
    ENDDO
  ENDIF
ENDIF

DO i = 1, ngrid
  IF (PRESENT(anavar)) THEN
    j = COUNT((this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim) &
     .AND. .NOT.isanavar(:))
    IF (j <= 0) THEN
      CALL l4f_category_log(category, L4F_FATAL, 'grid n.'//t2c(i)// &
       ' has only constant data, this is not allowed')
      CALL l4f_category_log(category, L4F_FATAL, 'please check anavar argument')
      CALL raise_fatal_error()
    ENDIF
  ENDIF
  ntime = count_distinct(correctedtime, &
   mask=(this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim) &
   .AND. .NOT.isanavar(:), back=.TRUE.)
  ntimerange = count_distinct(gridinfov%array(1:gridinfov%arraysize)%timerange, &
   mask=(this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim) &
   .AND. .NOT.isanavar(:), back=.TRUE.)
  nlevel = count_distinct(gridinfov%array(1:gridinfov%arraysize)%level, &
   mask=(this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim), &
   back=.TRUE.)
  nvar = count_distinct(gridinfov%array(1:gridinfov%arraysize)%var, &
   mask=(this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim), &
   back=.TRUE.)

#ifdef DEBUG
  CALL l4f_category_log(this(i)%category,L4F_DEBUG,"alloc volgrid6d index: "//t2c(i))
#endif

  CALL volgrid6d_alloc(this(i),this(i)%griddim%dim,ntime=ntime, &
   ntimerange=ntimerange,nlevel=nlevel,nvar=nvar)

  this(i)%time = pack_distinct(correctedtime, ntime, &
   mask=(this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim) &
   .AND. .NOT.isanavar(:), back=.TRUE.)
  CALL sort(this(i)%time)

  this(i)%timerange = pack_distinct(gridinfov%array( &
   1:gridinfov%arraysize)%timerange, ntimerange, &
   mask=(this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim) &
   .AND. .NOT.isanavar(:), back=.TRUE.)
  CALL sort(this(i)%timerange)

  this(i)%level=pack_distinct(gridinfov%array(1:gridinfov%arraysize)%level, &
   nlevel,mask=(this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim), &
   back=.TRUE.)
  CALL sort(this(i)%level)

  this(i)%var=pack_distinct(gridinfov%array(1:gridinfov%arraysize)%var, nvar, &
   mask=(this(i)%griddim == gridinfov%array(1:gridinfov%arraysize)%griddim), &
   back=.TRUE.)

#ifdef DEBUG
  CALL l4f_category_log(this(i)%category,L4F_DEBUG,"alloc_vol volgrid6d index: "//t2c(i))
#endif
  CALL volgrid6d_alloc_vol(this(i), decode=decode)

ENDDO

DEALLOCATE(correctedtime)

DO i = 1, gridinfov%arraysize

#ifdef DEBUG
  CALL l4f_category_log(category,L4F_DEBUG,"import from gridinfov index: "//t2c(i))
  CALL l4f_category_log(category,L4F_INFO, &
   "to volgrid6d index: "//t2c(index(this%griddim, gridinfov%array(i)%griddim)))
#endif

  CALL import(this(index(this%griddim, gridinfov%array(i)%griddim)), &
   gridinfov%array(i), dup_mode=dup_mode, clone=clone, isanavar=isanavar(i))

ENDDO

!chiudo il logger temporaneo
CALL l4f_category_delete(category)

END SUBROUTINE import_from_gridinfovv


!> Export a \a volgrid6d object to an \a arrayof_gridinfo object.
!! The multidimensional \a volgrid6d structure is serialized into a
!! one-dimensional array of gridinfo_def objects, which is allocated
!! to the proper size if not already allocated, or it is extended
!! keeping the old data if any.
SUBROUTINE export_to_gridinfov(this, gridinfov, gaid_template, clone)
TYPE(volgrid6d),INTENT(inout) :: this !< volume to be exported
TYPE(arrayof_gridinfo),INTENT(inout) :: gridinfov !< output array of gridinfo_def objects
TYPE(grid_id),INTENT(in),OPTIONAL :: gaid_template !< \a grid_id template to be used for output data replacing the one contained in \a this
LOGICAL,INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE., clone the grid_id included in \a this rather than making a shallow copy

INTEGER :: i ,itime, itimerange, ilevel, ivar
INTEGER :: ntime, ntimerange, nlevel, nvar
TYPE(gridinfo_def) :: gridinfol

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,"start export_to_gridinfov")
#endif

! this is necessary in order not to repeatedly and uselessly copy the
! same array of coordinates for each 2d grid during export, the
! side-effect is that the computed projection in this is lost
CALL dealloc(this%griddim%dim)

i=0
ntime=size(this%time)
ntimerange=size(this%timerange)
nlevel=size(this%level)
nvar=size(this%var)

DO itime=1,ntime
  DO itimerange=1,ntimerange
    DO ilevel=1,nlevel
      DO ivar=1,nvar

        CALL init(gridinfol)
        CALL export(this, gridinfol, itime, itimerange, ilevel, ivar, &
         gaid_template=gaid_template, clone=clone)
        IF (c_e(gridinfol%gaid)) THEN
          CALL insert(gridinfov, gridinfol)
        ELSE
          CALL delete(gridinfol)
        ENDIF
        
      ENDDO
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE export_to_gridinfov


!> Export an array of \a volgrid6d objects to an \a arrayof_gridinfo object.
!! The multidimensional \a volgrid6d structures are serialized into a
!! one-dimensional array of gridinfo_def objects, which is allocated
!! to the proper size if not already allocated, or it is extended
!! keeping the old data if any.
SUBROUTINE export_to_gridinfovv(this, gridinfov, gaid_template, clone)
!, &
! categoryappend)
TYPE(volgrid6d),INTENT(inout) :: this(:) !< volume array to be exported
TYPE(arrayof_gridinfo),INTENT(inout) :: gridinfov !< output array of gridinfo_def objects
TYPE(grid_id),INTENT(in),OPTIONAL :: gaid_template !< \a grid_id template to be used for output data replacing the one contained in \a this
LOGICAL,INTENT(in),OPTIONAL :: clone !< if provided and \c .TRUE., clone the grid_id included in \a this rather than making a shallow copy

INTEGER :: i

DO i = 1, SIZE(this)
#ifdef DEBUG
  CALL l4f_category_log(this(i)%category,L4F_DEBUG, &
   "export_to_gridinfovv grid index: "//t2c(i))
#endif
  CALL export(this(i), gridinfov, gaid_template=gaid_template, clone=clone)
ENDDO

END SUBROUTINE export_to_gridinfovv


!> Import the content of a supported file (like grib or gdal-supported
!! format) into an array of \a volgrid6d objects.  This method imports
!! a set of gridded fields from a file object into a suitable number
!! of \a volgrid6d objects. The data are imported by creating a
!! temporary \a gridinfo object, importing it into the \a volgrid6d
!! object cloning the gaid's and then destroying the gridinfo, so it
!! works similarly to volgrid6d_class::import_from_gridinfovv() method.
!! For a detailed explanation of the \a anavar argument, see the
!! documentation of volgrid6d_class::import_from_gridinfovv() method.
SUBROUTINE volgrid6d_import_from_file(this, filename, dup_mode, decode, &
 time_definition, anavar, categoryappend)
TYPE(volgrid6d),POINTER :: this(:) !< object in which to import
CHARACTER(len=*),INTENT(in) :: filename !< name of file from which to import
INTEGER,INTENT(in),OPTIONAL :: dup_mode !< determines the behavior in case of duplicate metadata: if \a dup_mode is not provided or 0, a duplicate field overwrites, if \a dup_mode is 1, duplicate fields are merged with priority to the last
LOGICAL,INTENT(in),OPTIONAL :: decode !< if provided and \a .FALSE. the data volume in the elements of \a this is not allocated and successive work will be performed on grid_id's
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< 0=time is reference time; 1=time is validity time
CHARACTER(len=*),INTENT(IN),OPTIONAL :: anavar(:) !< list of variables (B-table code equivalent) to be treated as time-independent data
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

TYPE(arrayof_gridinfo) :: gridinfo
INTEGER :: category
CHARACTER(len=512) :: a_name

NULLIFY(this)

IF (PRESENT(categoryappend))THEN
  CALL l4f_launcher(a_name,a_name_append= &
   TRIM(subcategory)//"."//TRIM(categoryappend))
ELSE
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory))
ENDIF
category=l4f_category_get(a_name)

CALL import(gridinfo, filename=filename, categoryappend=categoryappend)
  
IF (gridinfo%arraysize > 0) THEN

  CALL import(this, gridinfo, dup_mode=dup_mode, clone=.TRUE., decode=decode, &
   time_definition=time_definition, anavar=anavar, &
   categoryappend=categoryappend)

  CALL l4f_category_log(category,L4F_INFO,"deleting gridinfo")
  CALL delete(gridinfo)

ELSE
  CALL l4f_category_log(category,L4F_INFO,"file does not contain gridded data")
ENDIF

! close logger
CALL l4f_category_delete(category)

END SUBROUTINE volgrid6d_import_from_file


!> High level method for exporting a volume array to file.
!! All the information contained into an array of \a volgrid6d
!! objects, i.e. dimension descriptors and data, is exported to a file
!! using the proper output driver (typically grib_api for grib
!! format). If a template is provided, it will determine the
!! characteristic of the output file, otherwise the \a grid_id
!! descriptors contained in the volgrid6d object will be used
SUBROUTINE volgrid6d_export_to_file(this, filename, gaid_template, categoryappend)
TYPE(volgrid6d) :: this(:) !< volume(s) to be exported
CHARACTER(len=*),INTENT(in) :: filename !< output file name
TYPE(grid_id),INTENT(in),OPTIONAL :: gaid_template !< template for the output file, if provided the grid_id information stored in the volgrid6d objects will be ignored
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

TYPE(arrayof_gridinfo) :: gridinfo
INTEGER :: category
CHARACTER(len=512) :: a_name

IF (PRESENT(categoryappend)) THEN
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory)//"."//TRIM(categoryappend))
ELSE
  CALL l4f_launcher(a_name,a_name_append=TRIM(subcategory))
ENDIF
category=l4f_category_get(a_name)

#ifdef DEBUG
CALL l4f_category_log(category,L4F_DEBUG,"start export to file")
#endif

CALL l4f_category_log(category,L4F_INFO,"writing volgrid6d to grib file: "//TRIM(filename))

!IF (ASSOCIATED(this)) THEN
  CALL export(this, gridinfo, gaid_template=gaid_template, clone=.TRUE.)
  IF (gridinfo%arraysize > 0) THEN
    CALL export(gridinfo, filename)
    CALL delete(gridinfo)
  ENDIF
!ELSE
!  CALL l4f_category_log(category,L4F_INFO,"volume volgrid6d is not associated")
!ENDIF

! close logger
CALL l4f_category_delete(category)

END SUBROUTINE volgrid6d_export_to_file


!> Array destructor for \a volgrid6d class.
!! Delete an array of \a volgrid6d objects and deallocate the array
!! itself.
SUBROUTINE volgrid6dv_delete(this)
TYPE(volgrid6d),POINTER :: this(:) !< vector of volgrid6d object

INTEGER :: i

IF (ASSOCIATED(this)) THEN
  DO i = 1, SIZE(this)
#ifdef DEBUG
    CALL l4f_category_log(this(i)%category,L4F_DEBUG, &
     "delete volgrid6d vector index: "//TRIM(to_char(i)))
#endif
    CALL delete(this(i))
  ENDDO
  DEALLOCATE(this)
ENDIF

END SUBROUTINE volgrid6dv_delete


! Internal method for performing grid to grid computations
SUBROUTINE volgrid6d_transform_compute(this, volgrid6d_in, volgrid6d_out, &
 lev_out, var_coord_vol, clone)
TYPE(grid_transform),INTENT(in) :: this ! oggetto di trasformazione per il grigliato
type(volgrid6d), INTENT(in) :: volgrid6d_in ! oggetto da trasformare
type(volgrid6d), INTENT(inout) :: volgrid6d_out ! oggetto trasformato; deve essere completo (init, alloc, alloc_vol)
TYPE(vol7d_level),INTENT(in),OPTIONAL :: lev_out(:) ! vol7d_level object defining target vertical grid, for vertical interpolations
INTEGER,INTENT(in),OPTIONAL :: var_coord_vol ! index of variable defining vertical coordinate values in input volume
LOGICAL,INTENT(in),OPTIONAL :: clone ! se fornito e \c .TRUE., clona i gaid da volgrid6d_in a volgrid6d_out

INTEGER :: ntime, ntimerange, inlevel, onlevel, nvar, &
 itime, itimerange, ilevel, ivar, levshift, levused, lvar_coord_vol, spos
REAL,POINTER :: voldatiin(:,:,:), voldatiout(:,:,:), coord_3d_in(:,:,:)
TYPE(vol7d_level) :: output_levtype


#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_transform_compute")
#endif

ntime=0
ntimerange=0
inlevel=0
onlevel=0
nvar=0
lvar_coord_vol = optio_i(var_coord_vol)

if (associated(volgrid6d_in%time))then
  ntime=size(volgrid6d_in%time)
  volgrid6d_out%time=volgrid6d_in%time
end if

if (associated(volgrid6d_in%timerange))then
  ntimerange=size(volgrid6d_in%timerange)
  volgrid6d_out%timerange=volgrid6d_in%timerange
end if

IF (ASSOCIATED(volgrid6d_in%level))THEN
  inlevel=SIZE(volgrid6d_in%level)
ENDIF
IF (PRESENT(lev_out)) THEN
  onlevel=SIZE(lev_out)
  volgrid6d_out%level=lev_out
ELSE IF (ASSOCIATED(volgrid6d_in%level))THEN
  onlevel=SIZE(volgrid6d_in%level)
  volgrid6d_out%level=volgrid6d_in%level
ENDIF

if (associated(volgrid6d_in%var))then
  nvar=size(volgrid6d_in%var)
  volgrid6d_out%var=volgrid6d_in%var
end if
! allocate once for speed
IF (.NOT.ASSOCIATED(volgrid6d_in%voldati)) THEN
  ALLOCATE(voldatiin(volgrid6d_in%griddim%dim%nx, volgrid6d_in%griddim%dim%ny, &
   inlevel))
ENDIF
IF (.NOT.ASSOCIATED(volgrid6d_out%voldati)) THEN
  ALLOCATE(voldatiout(volgrid6d_out%griddim%dim%nx, volgrid6d_out%griddim%dim%ny, &
   onlevel))
ENDIF

CALL get_val(this, levshift=levshift, levused=levused)
spos = imiss
IF (c_e(lvar_coord_vol)) THEN
  CALL get_val(this%trans, output_levtype=output_levtype)
  IF (output_levtype%level1 == 103 .OR. output_levtype%level1 == 108) THEN
    spos = firsttrue(volgrid6d_in%level(:) == vol7d_level_new(1))
    IF (spos == 0) THEN
      CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
       'output level '//t2c(output_levtype%level1)// &
       ' requested, but height/press of surface not provided in volume')
    ENDIF
    IF (.NOT.c_e(levshift) .AND. .NOT.c_e(levused)) THEN
      CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
       'internal inconsistence, levshift and levused undefined when they should be')
    ENDIF
  ENDIF
ENDIF

DO ivar=1,nvar
!  IF (c_e(var_coord_vol)) THEN
!    IF (ivar == var_coord_vol) CYCLE ! skip coordinate variable in output
!  ENDIF
  DO itimerange=1,ntimerange
    DO itime=1,ntime
! skip empty columns where possible, improve
      IF (c_e(levshift) .AND. c_e(levused)) THEN
        IF (.NOT.ANY(c_e( &
         volgrid6d_in%gaid(levshift+1:levshift+levused,itime,itimerange,ivar) &
         ))) CYCLE
      ENDIF
      DO ilevel = 1, MIN(inlevel,onlevel)
! if present gaid copy it
        IF (c_e(volgrid6d_in%gaid(ilevel,itime,itimerange,ivar)) .AND. .NOT. &
         c_e(volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))) THEN

          IF (optio_log(clone)) THEN
            CALL copy(volgrid6d_in%gaid(ilevel,itime,itimerange,ivar),&
             volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))
#ifdef DEBUG
            CALL l4f_category_log(volgrid6d_in%category,L4F_DEBUG, &
             "cloning gaid, level "//t2c(ilevel))
#endif
          ELSE
            volgrid6d_out%gaid(ilevel,itime,itimerange,ivar) = &
             volgrid6d_in%gaid(ilevel,itime,itimerange,ivar)
          ENDIF
        ENDIF
      ENDDO
! if out level are more, we have to clone anyway
      DO ilevel = MIN(inlevel,onlevel) + 1, onlevel
        IF (c_e(volgrid6d_in%gaid(inlevel,itime,itimerange,ivar)) .AND. .NOT. &
         c_e(volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))) then

          CALL copy(volgrid6d_in%gaid(inlevel,itime,itimerange,ivar),&
           volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))
#ifdef DEBUG
          CALL l4f_category_log(volgrid6d_in%category,L4F_DEBUG, &
           "forced cloning gaid, level "//t2c(inlevel)//"->"//t2c(ilevel))
#endif
        ENDIF
      ENDDO

      IF (c_e(lvar_coord_vol)) THEN
        NULLIFY(coord_3d_in)
        CALL volgrid_get_vol_3d(volgrid6d_in, itime, itimerange, lvar_coord_vol, &
         coord_3d_in)
        IF (c_e(spos)) THEN ! compute difference wrt surface coordinate
          IF (spos == 0) THEN ! error condition, set all to missing and goodnight
            coord_3d_in(:,:,levshift+1:levshift+levused) = rmiss
          ELSE
            DO ilevel = levshift+1, levshift+levused
              WHERE(c_e(coord_3d_in(:,:,ilevel)) .AND. c_e(coord_3d_in(:,:,spos)))
                coord_3d_in(:,:,ilevel) = coord_3d_in(:,:,ilevel) - &
                 coord_3d_in(:,:,spos)
              ELSEWHERE
                coord_3d_in(:,:,ilevel) = rmiss
              END WHERE
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      CALL volgrid_get_vol_3d(volgrid6d_in, itime, itimerange, ivar, &
       voldatiin)
      IF (ASSOCIATED(volgrid6d_out%voldati)) & ! improve!!!!
       CALL volgrid_get_vol_3d(volgrid6d_out, itime, itimerange, ivar, &
       voldatiout)
      IF (c_e(lvar_coord_vol)) THEN
        CALL compute(this, voldatiin, voldatiout, convert(volgrid6d_in%var(ivar)), &
         coord_3d_in(:,:,levshift+1:levshift+levused)) ! subset coord_3d_in
      ELSE
        CALL compute(this, voldatiin, voldatiout, convert(volgrid6d_in%var(ivar)))
      ENDIF
      CALL volgrid_set_vol_3d(volgrid6d_out, itime, itimerange, ivar, &
       voldatiout)
    ENDDO
  ENDDO
ENDDO

IF (c_e(lvar_coord_vol)) THEN
  DEALLOCATE(coord_3d_in)
ENDIF
IF (.NOT.ASSOCIATED(volgrid6d_in%voldati)) THEN
  DEALLOCATE(voldatiin)
ENDIF
IF (.NOT.ASSOCIATED(volgrid6d_out%voldati)) THEN
  DEALLOCATE(voldatiout)
ENDIF


END SUBROUTINE volgrid6d_transform_compute


!> Performs the specified abstract transformation on the data provided.
!! The abstract transformation is specified by \a this parameter; the
!! corresponding specifical transformation (\a grid_transform object)
!! is created and destroyed internally. The output transformed object
!! is created internally and it does not require preliminary
!! initialisation.
SUBROUTINE volgrid6d_transform(this, griddim, volgrid6d_in, volgrid6d_out, &
 lev_out, volgrid6d_coord_in, maskgrid, maskbounds, clone, decode, categoryappend)
TYPE(transform_def),INTENT(in) :: this !< object specifying the abstract transformation
TYPE(griddim_def),INTENT(in),OPTIONAL :: griddim !< griddim specifying the output grid (required by most transformation types)
! TODO ripristinare intent(in) dopo le opportune modifiche in grid_class.F90
TYPE(volgrid6d),INTENT(inout) :: volgrid6d_in !< object to be transformed, it is not modified, despite the INTENT(inout)
TYPE(volgrid6d),INTENT(out) :: volgrid6d_out !< transformed object, it does not require initialisation
TYPE(vol7d_level),INTENT(in),OPTIONAL,TARGET :: lev_out(:) !< vol7d_level object defining target vertical grid, for vertical interpolations
TYPE(volgrid6d),INTENT(in),OPTIONAL :: volgrid6d_coord_in !< object providing time constant input vertical coordinate for some kind of vertical interpolations
REAL,INTENT(in),OPTIONAL :: maskgrid(:,:) !< 2D field to be used for defining subareas according to its values, it must have the same shape as the field to be interpolated (for transformation subtype 'maskfill')
REAL,INTENT(in),OPTIONAL :: maskbounds(:) !< array of boundary values for defining a subset of valid points where the values of \a maskgrid are within the first and last value of \a maskbounds (for transformation type 'metamorphosis:maskfill')
LOGICAL,INTENT(in),OPTIONAL :: clone !< if provided and \a .TRUE. , clone the \a gaid's from \a volgrid6d_in to \a volgrid6d_out
LOGICAL,INTENT(in),OPTIONAL :: decode !< determine whether the data in \a volgrid6d_out should be decoded or remain coded in gaid, if not provided, the decode status is taken from \a volgrid6d_in
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

TYPE(grid_transform) :: grid_trans
TYPE(vol7d_level),POINTER :: llev_out(:)
TYPE(vol7d_level) :: input_levtype, output_levtype
TYPE(vol7d_var) :: vcoord_var
INTEGER :: i, k, ntime, ntimerange, nlevel, nvar, var_coord_in, var_coord_vol, &
 cf_out, nxc, nyc, nxi, nyi, i3, i4, i5, i6, &
 ulstart, ulend, spos
REAL,ALLOCATABLE :: coord_3d_in(:,:,:)
TYPE(geo_proj) :: proj_in, proj_out
CHARACTER(len=80) :: trans_type
LOGICAL :: ldecode
LOGICAL,ALLOCATABLE :: mask_in(:)

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category, L4F_DEBUG, "start volgrid6d_transform")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (associated(volgrid6d_in%time)) ntime=size(volgrid6d_in%time)
if (associated(volgrid6d_in%timerange)) ntimerange=size(volgrid6d_in%timerange)
if (associated(volgrid6d_in%level)) nlevel=size(volgrid6d_in%level)
if (associated(volgrid6d_in%var)) nvar=size(volgrid6d_in%var)

IF (ntime == 0 .OR. ntimerange == 0 .OR. nlevel == 0 .OR. nvar == 0) THEN
  CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
   "trying to transform an incomplete volgrid6d object, ntime="//t2c(ntime)// &
   ' ntimerange='//t2c(ntimerange)//' nlevel='//t2c(nlevel)//' nvar='//t2c(nvar))
  CALL init(volgrid6d_out) ! initialize to empty
  CALL raise_error()
  RETURN
ENDIF

CALL get_val(this, trans_type=trans_type)

! store desired output component flag and unrotate if necessary
cf_out = imiss
IF (PRESENT(griddim) .AND. (trans_type == 'inter' .OR. trans_type == 'boxinter' &
 .OR. trans_type == 'stencilinter')) THEN ! improve condition!!
  CALL get_val(volgrid6d_in%griddim, proj=proj_in)
  CALL get_val(griddim, component_flag=cf_out, proj=proj_out)
! if different projections wind components must be referred to geographical system
  IF (proj_in /= proj_out) CALL vg6d_wind_unrot(volgrid6d_in)
ELSE IF (PRESENT(griddim)) THEN ! just get component_flag, the rest is rubbish
  CALL get_val(griddim, component_flag=cf_out)
ENDIF


var_coord_in = imiss
var_coord_vol = imiss
IF (trans_type == 'vertint') THEN
  IF (PRESENT(lev_out)) THEN

! if volgrid6d_coord_in provided and allocated, check that it fits
    IF (PRESENT(volgrid6d_coord_in)) THEN
      IF (ASSOCIATED(volgrid6d_coord_in%voldati)) THEN

! strictly 1 time and 1 timerange
        IF (SIZE(volgrid6d_coord_in%voldati,4) /= 1 .OR. &
         SIZE(volgrid6d_coord_in%voldati,5) /= 1) THEN
          CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
           'volume providing constant input vertical coordinate must have &
           &only 1 time and 1 timerange')
          CALL init(volgrid6d_out) ! initialize to empty
          CALL raise_error()
          RETURN
        ENDIF

! search for variable providing vertical coordinate
        CALL get_val(this, output_levtype=output_levtype)
        vcoord_var = vol7d_var_new(vol7d_level_to_var(output_levtype))
        IF (.NOT.c_e(vcoord_var)) THEN
          CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
           'requested output level type '//t2c(output_levtype%level1)// &
           ' does not correspond to any known physical variable for &
           &providing vertical coordinate')
          CALL init(volgrid6d_out) ! initialize to empty
          CALL raise_error()
          RETURN
        ENDIF

        DO i = 1, SIZE(volgrid6d_coord_in%var)
          IF (convert(volgrid6d_coord_in%var(i)) == vcoord_var) THEN
            var_coord_in = i
            EXIT
          ENDIF
        ENDDO

        IF (.NOT.c_e(var_coord_in)) THEN
          CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
           'volume providing constant input vertical coordinate contains no &
           &variables matching output level type '//t2c(output_levtype%level1))
          CALL init(volgrid6d_out) ! initialize to empty
          CALL raise_error()
          RETURN
        ENDIF
        CALL l4f_category_log(volgrid6d_in%category, L4F_INFO, &
         'Coordinate for vertint found in coord volume at position '// &
         t2c(var_coord_in))

! check horizontal grid
! this is too strict (component flag and so on)
!        IF (volgrid6d_coord_in%griddim /= volgrid6d_in%griddim) THEN
        CALL get_val(volgrid6d_coord_in%griddim, nx=nxc, ny=nyc)
        CALL get_val(volgrid6d_in%griddim, nx=nxi, ny=nyi)
        IF (nxc /= nxi .OR. nyc /= nyi) THEN
          CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
           'volume providing constant input vertical coordinate must have &
           &the same grid as the input')
          CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
           'vertical coordinate: '//t2c(nxc)//'x'//t2c(nyc)// &
           ', input volume: '//t2c(nxi)//'x'//t2c(nyi))
          CALL init(volgrid6d_out) ! initialize to empty
          CALL raise_error()
          RETURN
        ENDIF

! check vertical coordinate system
        CALL get_val(this, input_levtype=input_levtype)
        mask_in = & ! implicit allocation
         (volgrid6d_coord_in%level(:)%level1 == input_levtype%level1) .AND. &
         (volgrid6d_coord_in%level(:)%level2 == input_levtype%level2)
        ulstart = firsttrue(mask_in)
        ulend = lasttrue(mask_in)
        IF (ulstart == 0 .OR. ulend == 0) THEN
          CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
           'coordinate file does not contain levels of type '// &
           t2c(input_levtype%level1)//'/'//t2c(input_levtype%level2)// &
           ' specified for input data')
          CALL init(volgrid6d_out) ! initialize to empty
          CALL raise_error()
          RETURN
        ENDIF

        coord_3d_in = volgrid6d_coord_in%voldati(:,:,ulstart:ulend,1,1,var_coord_in) ! implicit allocation
! special case
        IF (output_levtype%level1 == 103 .OR. &
         output_levtype%level1 == 108) THEN ! surface coordinate needed
          spos = firsttrue(volgrid6d_coord_in%level(:) == vol7d_level_new(1))
          IF (spos == 0) THEN
            CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
             'output level '//t2c(output_levtype%level1)// &
             ' requested, but height/press of surface not provided in coordinate file')
            CALL init(volgrid6d_out) ! initialize to empty
            CALL raise_error()
            RETURN
          ENDIF
          DO k = 1, SIZE(coord_3d_in,3)
            WHERE(c_e(coord_3d_in(:,:,k)) .AND. &
             c_e(volgrid6d_coord_in%voldati(:,:,spos,1,1,var_coord_in)))
              coord_3d_in(:,:,k) = coord_3d_in(:,:,k) - &
               volgrid6d_coord_in%voldati(:,:,spos,1,1,var_coord_in)
            ELSEWHERE
              coord_3d_in(:,:,k) = rmiss
            END WHERE
          ENDDO
        ENDIF

      ENDIF
    ENDIF

    IF (.NOT.c_e(var_coord_in)) THEN ! search for coordinate within volume
! search for variable providing vertical coordinate
      CALL get_val(this, output_levtype=output_levtype)
      vcoord_var = vol7d_var_new(vol7d_level_to_var(output_levtype))
      IF (c_e(vcoord_var)) THEN
        DO i = 1, SIZE(volgrid6d_in%var)
          IF (convert(volgrid6d_in%var(i)) == vcoord_var) THEN
            var_coord_vol = i
            EXIT
          ENDIF
        ENDDO

        IF (c_e(var_coord_vol)) THEN
          CALL l4f_category_log(volgrid6d_in%category, L4F_INFO, &
           'Coordinate for vertint found in input volume at position '// &
           t2c(var_coord_vol))
        ENDIF

      ENDIF
    ENDIF

    CALL init(volgrid6d_out, griddim=volgrid6d_in%griddim, &
     time_definition=volgrid6d_in%time_definition, categoryappend=categoryappend)
    IF (c_e(var_coord_in)) THEN
      CALL init(grid_trans, this, lev_in=volgrid6d_in%level, lev_out=lev_out, &
       coord_3d_in=coord_3d_in, categoryappend=categoryappend)
    ELSE
      CALL init(grid_trans, this, lev_in=volgrid6d_in%level, lev_out=lev_out, &
       categoryappend=categoryappend)
    ENDIF

    CALL get_val(grid_trans, output_level_auto=llev_out) ! get levels if auto-generated
    IF (.NOT.ASSOCIATED(llev_out)) llev_out => lev_out
    nlevel = SIZE(llev_out)
  ELSE
    CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
     'volgrid6d_transform: vertint requested but lev_out not provided')
    CALL init(volgrid6d_out) ! initialize to empty
    CALL raise_error()
    RETURN
  ENDIF

ELSE
  CALL init(volgrid6d_out, griddim=griddim, &
   time_definition=volgrid6d_in%time_definition, categoryappend=categoryappend)
  CALL init(grid_trans, this, in=volgrid6d_in%griddim, out=volgrid6d_out%griddim, &
   maskgrid=maskgrid, maskbounds=maskbounds, categoryappend=categoryappend)
ENDIF


IF (c_e(grid_trans)) THEN ! transformation is valid

  CALL volgrid6d_alloc(volgrid6d_out, ntime=ntime, nlevel=nlevel, &
   ntimerange=ntimerange, nvar=nvar)

  IF (PRESENT(decode)) THEN ! explicitly set decode status
    ldecode = decode
  ELSE ! take it from input
    ldecode = ASSOCIATED(volgrid6d_in%voldati)
  ENDIF
! force decode if gaid is readonly
  decode_loop: DO i6 = 1,nvar
    DO i5 = 1, ntimerange
      DO i4 = 1, ntime
        DO i3 = 1, nlevel
          IF (c_e(volgrid6d_in%gaid(i3,i4,i5,i6))) THEN
            ldecode = ldecode .OR. grid_id_readonly(volgrid6d_in%gaid(i3,i4,i5,i6))
            EXIT decode_loop
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO decode_loop

  IF (PRESENT(decode)) THEN
    IF (ldecode.NEQV.decode) THEN
      CALL l4f_category_log(volgrid6d_in%category, L4F_WARN, &
     'volgrid6d_transform: decode status forced to .TRUE. because driver does not allow copy')
    ENDIF
  ENDIF

  CALL volgrid6d_alloc_vol(volgrid6d_out, decode=ldecode)

!ensure unproj was called
!call griddim_unproj(volgrid6d_out%griddim)

  IF (trans_type == 'vertint') THEN
#ifdef DEBUG
    CALL l4f_category_log(volgrid6d_in%category, L4F_DEBUG, &
     "volgrid6d_transform: vertint to "//t2c(nlevel)//" levels")
#endif
    CALL compute(grid_trans, volgrid6d_in, volgrid6d_out, lev_out=llev_out, &
     var_coord_vol=var_coord_vol, clone=clone)
  ELSE
    CALL compute(grid_trans, volgrid6d_in, volgrid6d_out, clone=clone)
  ENDIF

  IF (cf_out == 0) THEN ! unrotated components are desired
    CALL wind_unrot(volgrid6d_out) ! unrotate if necessary
  ELSE IF (cf_out == 1) THEN ! rotated components are desired
    CALL wind_rot(volgrid6d_out) ! rotate if necessary
  ENDIF

ELSE
! should log with grid_trans%category, but it is private
  CALL l4f_category_log(volgrid6d_in%category, L4F_ERROR, &
   'volgrid6d_transform: transformation not valid')
  CALL raise_error()
ENDIF

CALL delete (grid_trans)

END SUBROUTINE volgrid6d_transform


!> Performs the specified abstract transformation on the arrays of
!! data provided.  The abstract transformation is specified by \a this
!! parameter; the corresponding specifical transformation (\a
!! grid_transform object) is created and destroyed internally. The
!! output transformed object is created internally and it does not
!! require preliminary initialisation. According to the input data and
!! to the transformation type, the output array may have of one or
!! more \a volgrid6d elements on different grids.
SUBROUTINE volgrid6dv_transform(this, griddim, volgrid6d_in, volgrid6d_out, &
 lev_out, volgrid6d_coord_in, maskgrid, maskbounds, clone, decode, categoryappend)
TYPE(transform_def),INTENT(in) :: this !< object specifying the abstract transformation
TYPE(griddim_def),INTENT(in),OPTIONAL :: griddim !< griddim specifying the output grid (required by most transformation types)
! TODO ripristinare intent(in) dopo le opportune modifiche in grid_class.F90
TYPE(volgrid6d),INTENT(inout) :: volgrid6d_in(:) !< object to be transformed, it is an array of volgrid6d objects, each of which will be transformed, it is not modified, despite the INTENT(inout)
TYPE(volgrid6d),POINTER :: volgrid6d_out(:) !< transformed object, it is a non associated pointer to an array of volgrid6d objects which will be allocated by the method
TYPE(vol7d_level),INTENT(in),OPTIONAL :: lev_out(:) !< vol7d_level object defining target vertical grid
TYPE(volgrid6d),INTENT(in),OPTIONAL :: volgrid6d_coord_in !< object providing time constant input vertical coordinate for some kind of vertical interpolations
REAL,INTENT(in),OPTIONAL :: maskgrid(:,:) !< 2D field to be used for defining subareas according to its values, it must have the same shape as the field to be interpolated (for transformation subtype 'maskfill')
REAL,INTENT(in),OPTIONAL :: maskbounds(:) !< array of boundary values for defining a subset of valid points where the values of \a maskgrid are within the first and last value of \a maskbounds (for transformation type 'metamorphosis:maskfill')
LOGICAL,INTENT(in),OPTIONAL :: clone !< if provided and \a .TRUE. , clone the \a gaid's from \a volgrid6d_in to \a volgrid6d_out 
LOGICAL,INTENT(in),OPTIONAL :: decode !< if provided and \a .FALSE. the data volume is not allocated, but work is performed on grid_id's
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: i, stallo


allocate(volgrid6d_out(size(volgrid6d_in)),stat=stallo)
if (stallo /= 0)then
  call l4f_log(L4F_FATAL,"allocating memory")
  call raise_fatal_error()
end if

do i=1,size(volgrid6d_in)
  call transform(this, griddim, volgrid6d_in(i), volgrid6d_out(i), &
   lev_out=lev_out, volgrid6d_coord_in=volgrid6d_coord_in, &
   maskgrid=maskgrid, maskbounds=maskbounds, &
   clone=clone, decode=decode, categoryappend=categoryappend)
end do

END SUBROUTINE volgrid6dv_transform


! Internal method for performing grid to sparse point computations
SUBROUTINE volgrid6d_v7d_transform_compute(this, volgrid6d_in, vol7d_out, &
 networkname, noconvert)
TYPE(grid_transform),INTENT(in) :: this ! oggetto di trasformazione per grigliato
type(volgrid6d), INTENT(in) :: volgrid6d_in ! oggetto da trasformare
type(vol7d), INTENT(inout) :: vol7d_out ! oggetto trasformato
CHARACTER(len=*),OPTIONAL,INTENT(in) :: networkname ! imposta il network in vol7d_out (default='generic')
LOGICAL,OPTIONAL,INTENT(in) :: noconvert !< do not try to match variable and convert values during transform

INTEGER :: nntime, nana, ntime, ntimerange, nlevel, nvar, stallo
INTEGER :: itime, itimerange, ivar, inetwork
REAL,ALLOCATABLE :: voldatir_out(:,:,:)
TYPE(conv_func),POINTER :: c_func(:)
TYPE(datetime),ALLOCATABLE :: validitytime(:,:)
REAL,POINTER :: voldatiin(:,:,:)

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_v7d_transform_compute")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0
NULLIFY(c_func)

if (present(networkname))then
  call init(vol7d_out%network(1),name=networkname)
else
  call init(vol7d_out%network(1),name='generic')
end if

if (associated(volgrid6d_in%timerange))then
  ntimerange=size(volgrid6d_in%timerange)
  vol7d_out%timerange=volgrid6d_in%timerange
end if

if (associated(volgrid6d_in%time))then
  ntime=size(volgrid6d_in%time)

  if (vol7d_out%time_definition == volgrid6d_in%time_definition) then

                                ! i time sono definiti uguali: assegno
    vol7d_out%time=volgrid6d_in%time

  else
                                ! converto reference in validity
    allocate (validitytime(ntime,ntimerange),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(volgrid6d_in%category,L4F_FATAL,"allocating memory")
      call raise_fatal_error()
    end if

    do itime=1,ntime
      do itimerange=1,ntimerange
        if (vol7d_out%time_definition > volgrid6d_in%time_definition) then
          validitytime(itime,itimerange) = &
           volgrid6d_in%time(itime) + timedelta_new(sec=volgrid6d_in%timerange(itimerange)%p1)
        else
          validitytime(itime,itimerange) = &
           volgrid6d_in%time(itime) - timedelta_new(sec=volgrid6d_in%timerange(itimerange)%p1)
        end if
      end do
    end do

    nntime = count_distinct(reshape(validitytime,(/ntime*ntimerange/)), back=.TRUE.)
    vol7d_out%time=pack_distinct(reshape(validitytime,(/ntime*ntimerange/)), nntime,back=.TRUE.)

  end if
end if

IF (ASSOCIATED(volgrid6d_in%level)) THEN
  nlevel = SIZE(volgrid6d_in%level)
  vol7d_out%level=volgrid6d_in%level
ENDIF

IF (ASSOCIATED(volgrid6d_in%var)) THEN
  nvar = SIZE(volgrid6d_in%var)
  IF (.NOT. optio_log(noconvert)) THEN
    CALL vargrib2varbufr(volgrid6d_in%var, vol7d_out%dativar%r, c_func)
  ENDIF
ENDIF

nana = SIZE(vol7d_out%ana)

! allocate once for speed
IF (.NOT.ASSOCIATED(volgrid6d_in%voldati)) THEN
  ALLOCATE(voldatiin(volgrid6d_in%griddim%dim%nx, volgrid6d_in%griddim%dim%ny, &
   nlevel))
ENDIF

ALLOCATE(voldatir_out(nana,1,nlevel),stat=stallo)
IF (stallo /= 0) THEN
  CALL l4f_category_log(volgrid6d_in%category,L4F_FATAL,"allocating memory")
  CALL raise_fatal_error()
ENDIF

inetwork=1
do itime=1,ntime
  do itimerange=1,ntimerange
!    do ilevel=1,nlevel
      do ivar=1,nvar
        
                                !non è chiaro se questa sezione è utile o no
                                !ossia il compute sotto sembra prevedere voldatir_out solo in out
!!$        if (vol7d_out%time_definition == volgrid6d_in%time_definition) then
!!$          voldatir_out=reshape(vol7d_out%voldatir(:,itime,ilevel,itimerange,ivar,inetwork),(/nana,1/))
!!$        else
!!$          voldatir_out=reshape(vol7d_out%voldatir(:,index(vol7d_out%time,validitytime(itime,ilevel)),ilevel,itimerange,ivar,inetwork),(/nana,1/))
!!$        end if

        CALL volgrid_get_vol_3d(volgrid6d_in, itime, itimerange, ivar, &
         voldatiin)

        CALL compute(this, voldatiin, voldatir_out, vol7d_out%dativar%r(ivar))

        if (vol7d_out%time_definition == volgrid6d_in%time_definition) then
          vol7d_out%voldatir(:,itime,:,itimerange,ivar,inetwork) = &
           voldatir_out(:,1,:)
        else
          vol7d_out%voldatir(:,index(vol7d_out%time,validitytime(itime,itimerange)),:,itimerange,ivar,inetwork)=&
           RESHAPE(voldatir_out,(/nana,nlevel/))
        end if

! 1 indice della dimensione "anagrafica"
! 2 indice della dimensione "tempo"
! 3 indice della dimensione "livello verticale"
! 4 indice della dimensione "intervallo temporale"
! 5 indice della dimensione "variabile"
! 6 indice della dimensione "rete"

      end do
!    end do
  end do
end do

deallocate(voldatir_out)
IF (.NOT.ASSOCIATED(volgrid6d_in%voldati)) THEN
  DEALLOCATE(voldatiin)
ENDIF
if (allocated(validitytime)) deallocate(validitytime)

! Rescale valid data according to variable conversion table
IF (ASSOCIATED(c_func)) THEN
  DO ivar = 1, nvar
    CALL compute(c_func(ivar), vol7d_out%voldatir(:,:,:,:,ivar,:))
  ENDDO
  DEALLOCATE(c_func)
ENDIF

end SUBROUTINE volgrid6d_v7d_transform_compute


!> Performs the specified abstract transformation on the data provided.
!! The abstract transformation is specified by \a this parameter; the
!! corresponding specifical transformation (\a grid_transform object)
!! is created and destroyed internally. The output transformed object
!! is created internally and it does not require preliminary
!! initialisation.
SUBROUTINE volgrid6d_v7d_transform(this, volgrid6d_in, vol7d_out, v7d, &
 maskgrid, maskbounds, networkname, noconvert, categoryappend)
TYPE(transform_def),INTENT(in) :: this !< object specifying the abstract transformation
TYPE(volgrid6d),INTENT(inout) :: volgrid6d_in !< object to be transformed, it is not modified, despite the INTENT(inout)
TYPE(vol7d),INTENT(out) :: vol7d_out !< transformed object, it does not requires initialisation
TYPE(vol7d),INTENT(in),OPTIONAL :: v7d !< object containing a list of points over which transformation has to be done (required by some transformation types)
REAL,INTENT(in),OPTIONAL :: maskgrid(:,:) !< 2D field to be used for defining subareas according to its values, it must have the same shape as the field to be interpolated (for transformation type 'maskinter')
REAL,INTENT(in),OPTIONAL :: maskbounds(:) !< array of boundary values for defining subareas from the values of \a maskgrid, the number of subareas is SIZE(maskbounds) - 1, if not provided a default based on extreme values of \a makgrid is used
CHARACTER(len=*),OPTIONAL,INTENT(in) :: networkname !< set the output network name in vol7d_out (default='generic')
LOGICAL,OPTIONAL,INTENT(in) :: noconvert !< do not try to match variable and convert values during transform
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

type(grid_transform) :: grid_trans
INTEGER :: ntime, ntimerange, nlevel, nvar, nana, time_definition, nnetwork, stallo
INTEGER :: itime, itimerange, inetwork
TYPE(datetime),ALLOCATABLE :: validitytime(:,:)
INTEGER,ALLOCATABLE :: point_index(:)
TYPE(vol7d) :: v7d_locana

#ifdef DEBUG
call l4f_category_log(volgrid6d_in%category,L4F_DEBUG,"start volgrid6d_v7d_transform")
#endif

call vg6d_wind_unrot(volgrid6d_in)

ntime=0
ntimerange=0
nlevel=0
nvar=0
nnetwork=1

call get_val(this,time_definition=time_definition)
if (.not. c_e(time_definition)) then
  time_definition=1  ! default to validity time
endif

if (present(v7d)) then
  v7d_locana = v7d
else
  call init(v7d_locana,time_definition=time_definition)
endif

if (associated(volgrid6d_in%timerange)) ntimerange=size(volgrid6d_in%timerange)

if (associated(volgrid6d_in%time)) then

  ntime=size(volgrid6d_in%time)
  
  if (time_definition /= volgrid6d_in%time_definition) then
    
                                ! converto reference in validity
    allocate (validitytime(ntime,ntimerange),stat=stallo)
    if (stallo /=0)then
      call l4f_category_log(volgrid6d_in%category,L4F_FATAL,"allocating memory")
      call raise_fatal_error()
    end if

    do itime=1,ntime
      do itimerange=1,ntimerange
        if (time_definition > volgrid6d_in%time_definition) then
          validitytime(itime,itimerange) = &
           volgrid6d_in%time(itime) + timedelta_new(sec=volgrid6d_in%timerange(itimerange)%p1)
        else
          validitytime(itime,itimerange) = &
           volgrid6d_in%time(itime) - timedelta_new(sec=volgrid6d_in%timerange(itimerange)%p1)
        end if
      end do
    end do

    ntime = count_distinct(reshape(validitytime,(/ntime*ntimerange/)), back=.TRUE.)
    deallocate (validitytime)
  
  end if
end if


if (associated(volgrid6d_in%level)) nlevel=size(volgrid6d_in%level)
if (associated(volgrid6d_in%var)) nvar=size(volgrid6d_in%var)

CALL init(grid_trans, this, volgrid6d_in%griddim, v7d_locana, &
 maskgrid=maskgrid, maskbounds=maskbounds, categoryappend=categoryappend)
CALL init (vol7d_out,time_definition=time_definition)

IF (c_e(grid_trans)) THEN

  nana=SIZE(v7d_locana%ana)
  CALL vol7d_alloc(vol7d_out, nana=nana, ntime=ntime, nlevel=nlevel, &
   ntimerange=ntimerange, ndativarr=nvar, nnetwork=nnetwork)
  vol7d_out%ana = v7d_locana%ana

  CALL get_val(grid_trans, output_point_index=point_index)
  IF (ALLOCATED(point_index)) THEN
! check that size(point_index) == nana?
    CALL vol7d_alloc(vol7d_out, nanavari=1)
    CALL init(vol7d_out%anavar%i(1), 'B01192')
  ENDIF

  CALL vol7d_alloc_vol(vol7d_out)

  IF (ALLOCATED(point_index)) THEN
    DO inetwork = 1, nnetwork
      vol7d_out%volanai(:,1,inetwork) = point_index(:)
    ENDDO
  ENDIF
  CALL compute(grid_trans, volgrid6d_in, vol7d_out, networkname, noconvert)
ELSE
  CALL l4f_log(L4F_ERROR, 'vg6d_v7d_transform: transformation not valid')
  CALL raise_error()
ENDIF

CALL delete(grid_trans)

#ifdef HAVE_DBALLE
! set variables to a conformal state
CALL vol7d_dballe_set_var_du(vol7d_out)
#endif

IF (.NOT. PRESENT(v7d)) THEN
  CALL delete(v7d_locana)
ENDIF

END SUBROUTINE volgrid6d_v7d_transform


!> Performs the specified abstract transformation on the arrays of
!! data provided.  The abstract transformation is specified by \a this
!! parameter; the corresponding specifical transformation (\a
!! grid_transform object) is created and destroyed internally. The
!! output transformed object is created internally and it does not
!! require preliminary initialisation. The transformation performed on
!! each element of the input \a volgrid6d array object is merged into
!! a single \a vol7d output object.
SUBROUTINE volgrid6dv_v7d_transform(this, volgrid6d_in, vol7d_out, v7d, &
 maskgrid, maskbounds, networkname, noconvert, categoryappend)
TYPE(transform_def),INTENT(in) :: this !< object specifying the abstract transformation
TYPE(volgrid6d),INTENT(inout) :: volgrid6d_in(:) !< object to be transformed, it is an array of volgrid6d objects, each of which will be transformed, it is not modified, despite the INTENT(inout)
TYPE(vol7d),INTENT(out) :: vol7d_out !< transformed object, it does not require initialisation
TYPE(vol7d),INTENT(in),OPTIONAL :: v7d !< object containing a list of points over which transformation has to be done (required by some transformation types)
REAL,INTENT(in),OPTIONAL :: maskgrid(:,:) !< 2D field to be used for defining subareas according to its values, it must have the same shape as the field to be interpolated (for transformation type 'maskinter')
REAL,INTENT(in),OPTIONAL :: maskbounds(:) !< array of boundary values for defining subareas from the values of \a maskgrid, the number of subareas is SIZE(maskbounds) - 1, if not provided a default based on extreme values of \a makgrid is used
CHARACTER(len=*),OPTIONAL,INTENT(in) :: networkname !< set the output network name in vol7d_out (default='generic')
LOGICAL,OPTIONAL,INTENT(in) :: noconvert !< do not try to match variable and convert values during transform
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

integer :: i
TYPE(vol7d) :: v7dtmp


CALL init(v7dtmp)
CALL init(vol7d_out)

DO i=1,SIZE(volgrid6d_in)
  CALL transform(this, volgrid6d_in(i), v7dtmp, v7d=v7d, &
   maskgrid=maskgrid, maskbounds=maskbounds, &
   networkname=networkname, noconvert=noconvert, categoryappend=categoryappend)
  CALL vol7d_append(vol7d_out, v7dtmp)
ENDDO

END SUBROUTINE volgrid6dv_v7d_transform


! Internal method for performing sparse point to grid computations
SUBROUTINE v7d_volgrid6d_transform_compute(this, vol7d_in, volgrid6d_out, networkname, gaid_template)
TYPE(grid_transform),INTENT(in) :: this ! object specifying the specific transformation
type(vol7d), INTENT(in) :: vol7d_in ! object to be transformed
type(volgrid6d), INTENT(inout) :: volgrid6d_out ! transformed object
CHARACTER(len=*),OPTIONAL,INTENT(in) :: networkname ! select the network to be processed from the \a vol7d input object, default the first network
TYPE(grid_id),OPTIONAL,INTENT(in) :: gaid_template ! the template (typically grib_api) to be associated with output data, it also helps in improving variable conversion

integer :: nana, ntime, ntimerange, nlevel, nvar
INTEGER :: ilevel, itime, itimerange, ivar, inetwork

REAL,POINTER :: voldatiout(:,:,:)
type(vol7d_network) :: network
TYPE(conv_func), pointer :: c_func(:)
!TODO category sarebbe da prendere da vol7d
#ifdef DEBUG
call l4f_category_log(volgrid6d_out%category,L4F_DEBUG,"start v7d_volgrid6d_transform_compute")
#endif

ntime=0
ntimerange=0
nlevel=0
nvar=0

if (present(networkname))then
  call init(network,name=networkname)
  inetwork= index(vol7d_in%network,network)
else
  inetwork=1
end if

! no time_definition conversion implemented, output will be the same as input
if (associated(vol7d_in%time))then
  ntime=size(vol7d_in%time)
  volgrid6d_out%time=vol7d_in%time
end if

if (associated(vol7d_in%timerange))then
  ntimerange=size(vol7d_in%timerange)
  volgrid6d_out%timerange=vol7d_in%timerange
end if

if (associated(vol7d_in%level))then
  nlevel=size(vol7d_in%level)
  volgrid6d_out%level=vol7d_in%level
end if

if (associated(vol7d_in%dativar%r))then
  nvar=size(vol7d_in%dativar%r)
  CALL varbufr2vargrib(vol7d_in%dativar%r, volgrid6d_out%var, c_func, gaid_template)
end if

nana=SIZE(vol7d_in%voldatir, 1)
! allocate once for speed
IF (.NOT.ASSOCIATED(volgrid6d_out%voldati)) THEN
  ALLOCATE(voldatiout(volgrid6d_out%griddim%dim%nx, volgrid6d_out%griddim%dim%ny, &
   nlevel))
ENDIF

DO ivar=1,nvar
  DO itimerange=1,ntimerange
    DO itime=1,ntime

! clone the gaid template where I have data
      IF (PRESENT(gaid_template)) THEN
        DO ilevel = 1, nlevel
          IF (ANY(c_e(vol7d_in%voldatir(:,itime,ilevel,itimerange,ivar,inetwork)))) THEN
            CALL copy(gaid_template, volgrid6d_out%gaid(ilevel,itime,itimerange,ivar))
          ELSE
            volgrid6d_out%gaid(ilevel,itime,itimerange,ivar) = grid_id_new()
          ENDIF
        ENDDO
      ENDIF

! get data
      IF (ASSOCIATED(volgrid6d_out%voldati)) & ! improve!!!!
       CALL volgrid_get_vol_3d(volgrid6d_out, itime, itimerange, ivar, &
       voldatiout)
! do the interpolation
      CALL compute(this, &
       vol7d_in%voldatir(:,itime,:,itimerange,ivar,inetwork), voldatiout, &
       vol7d_in%dativar%r(ivar))
! rescale valid data according to variable conversion table
      IF (ASSOCIATED(c_func)) THEN
        CALL compute(c_func(ivar), voldatiout(:,:,:))
      ENDIF
! put data
      CALL volgrid_set_vol_3d(volgrid6d_out, itime, itimerange, ivar, &
       voldatiout)

    ENDDO
  ENDDO
ENDDO

IF (.NOT.ASSOCIATED(volgrid6d_out%voldati)) THEN
  DEALLOCATE(voldatiout)
ENDIF
IF (ASSOCIATED(c_func)) THEN
  DEALLOCATE(c_func)
ENDIF

END SUBROUTINE v7d_volgrid6d_transform_compute


!> Performs the specified abstract transformation on the data provided.
!! The abstract transformation is specified by \a this parameter; the
!! corresponding specifical transformation (\a grid_transform object)
!! is created and destroyed internally. The output transformed object
!! is created internally and it does not require preliminary
!! initialisation.
SUBROUTINE v7d_volgrid6d_transform(this, griddim, vol7d_in, volgrid6d_out, &
 networkname, gaid_template, categoryappend)
TYPE(transform_def),INTENT(in) :: this !< object specifying the abstract transformation
TYPE(griddim_def),INTENT(in),OPTIONAL :: griddim !< griddim specifying the output grid (required by most transformation types)
! TODO ripristinare intent(in) dopo le opportune modifiche in grid_class.F90
TYPE(vol7d),INTENT(inout) :: vol7d_in !< object to be transformed, it is not modified, despite the INTENT(inout)
TYPE(volgrid6d),INTENT(out) :: volgrid6d_out !< transformed object, it does not require initialisation
CHARACTER(len=*),OPTIONAL,INTENT(in) :: networkname  !< select the network to be processed from the \a vol7d input object, default the first network
TYPE(grid_id),OPTIONAL,INTENT(in) :: gaid_template !< the template (typically grib_api) to be associated with output data, it also helps in improving variable conversion
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

type(grid_transform) :: grid_trans
integer :: ntime, ntimerange, nlevel, nvar


!TODO la category sarebbe da prendere da vol7d
!call l4f_category_log(vol7d_out%category,L4F_DEBUG,"start volgrid6d_transform")

CALL vol7d_alloc_vol(vol7d_in) ! be safe
ntime=SIZE(vol7d_in%time)
ntimerange=SIZE(vol7d_in%timerange)
nlevel=SIZE(vol7d_in%level)
nvar=0
if (associated(vol7d_in%dativar%r)) nvar=size(vol7d_in%dativar%r)

IF (nvar <= 0) THEN ! use vol7d category once it will be implemented
  CALL l4f_log(L4F_ERROR, &
   "trying to transform a vol7d object incomplete or without real variables")
  CALL init(volgrid6d_out) ! initialize to empty
  CALL raise_error()
  RETURN
ENDIF

CALL init(grid_trans, this, vol7d_in, griddim, categoryappend=categoryappend)
CALL init(volgrid6d_out, griddim, time_definition=vol7d_in%time_definition, & 
 categoryappend=categoryappend)

IF (c_e(grid_trans)) THEN

  CALL volgrid6d_alloc(volgrid6d_out, griddim%dim, ntime=ntime, nlevel=nlevel, &
   ntimerange=ntimerange, nvar=nvar)
! can I avoid decode=.TRUE. here, using gaid_template?
  CALL volgrid6d_alloc_vol(volgrid6d_out, decode=.TRUE.)

  CALL compute(grid_trans, vol7d_in, volgrid6d_out, networkname, gaid_template)

  CALL vg6d_wind_rot(volgrid6d_out)
ELSE
! should log with grid_trans%category, but it is private
  CALL l4f_log(L4F_ERROR, 'v7d_vg6d_transform: transformation not valid')
  CALL raise_error()
ENDIF

CALL delete(grid_trans)

END SUBROUTINE v7d_volgrid6d_transform


! Internal method for performing sparse point to sparse point computations
SUBROUTINE v7d_v7d_transform_compute(this, vol7d_in, vol7d_out, lev_out, &
 var_coord_vol)
TYPE(grid_transform),INTENT(in) :: this ! oggetto di trasformazione per grigliato
type(vol7d), INTENT(in) :: vol7d_in ! oggetto da trasformare
type(vol7d), INTENT(inout) :: vol7d_out ! oggetto trasformato
TYPE(vol7d_level),INTENT(in),OPTIONAL :: lev_out(:) ! vol7d_level object defining target vertical grid, for vertical interpolations
INTEGER,INTENT(in),OPTIONAL :: var_coord_vol ! index of variable defining vertical coordinate values in input volume

INTEGER :: itime, itimerange, ilevel, ivar, inetwork, &
 levshift, levused, lvar_coord_vol, spos
REAL,ALLOCATABLE :: coord_3d_in(:,:,:)
TYPE(vol7d_level) :: output_levtype

lvar_coord_vol = optio_i(var_coord_vol)
vol7d_out%time(:) = vol7d_in%time(:)
vol7d_out%timerange(:) = vol7d_in%timerange(:)
IF (PRESENT(lev_out)) THEN
  vol7d_out%level(:) = lev_out(:)
ELSE
  vol7d_out%level(:) = vol7d_in%level(:)
ENDIF
vol7d_out%network(:) = vol7d_in%network(:)
IF (ASSOCIATED(vol7d_in%dativar%r)) THEN ! work only when real vars are available
  vol7d_out%dativar%r(:) = vol7d_in%dativar%r(:)

  CALL get_val(this, levshift=levshift, levused=levused)
  spos = imiss
  IF (c_e(lvar_coord_vol)) THEN
    CALL get_val(this%trans, output_levtype=output_levtype)
    IF (output_levtype%level1 == 103 .OR. output_levtype%level1 == 108) THEN
      spos = firsttrue(vol7d_in%level(:) == vol7d_level_new(1))
      IF (spos == 0) THEN
        CALL l4f_log(L4F_ERROR, &
         'output level '//t2c(output_levtype%level1)// &
         ' requested, but height/press of surface not provided in volume')
      ENDIF
      IF (.NOT.c_e(levshift) .AND. .NOT.c_e(levused)) THEN
        CALL l4f_log(L4F_ERROR, &
         'internal inconsistence, levshift and levused undefined when they should be')
      ENDIF
      ALLOCATE(coord_3d_in(SIZE(vol7d_in%ana),1,SIZE(vol7d_in%level)))
    ENDIF

  ENDIF

  DO inetwork = 1, SIZE(vol7d_in%network)
    DO ivar = 1, SIZE(vol7d_in%dativar%r)
      DO itimerange = 1, SIZE(vol7d_in%timerange)
        DO itime = 1, SIZE(vol7d_in%time)

! dirty trick to make voldatir look like a 2d-array of shape (nana,1)
          IF (c_e(lvar_coord_vol)) THEN
            IF (c_e(spos)) THEN ! compute difference wrt surface coordinate
              IF (spos == 0) THEN ! error condition, set all to missing and goodnight
                coord_3d_in(:,:,levshift+1:levshift+levused) = rmiss
              ELSE
                DO ilevel = levshift+1, levshift+levused
                  WHERE(c_e(vol7d_in%voldatir(:,itime:itime,ilevel,itimerange,lvar_coord_vol,inetwork)) .AND. &
                  c_e(vol7d_in%voldatir(:,itime:itime,spos,itimerange,lvar_coord_vol,inetwork))) 
                    coord_3d_in(:,:,ilevel) = vol7d_in%voldatir(:,itime:itime,ilevel,itimerange,lvar_coord_vol,inetwork) - &
                     vol7d_in%voldatir(:,itime:itime,spos,itimerange,lvar_coord_vol,inetwork)
                  ELSEWHERE
                    coord_3d_in(:,:,ilevel) = rmiss
                  END WHERE
                ENDDO
              ENDIF
              CALL compute(this, &
               vol7d_in%voldatir(:,itime,:,itimerange,ivar,inetwork), &
               vol7d_out%voldatir(:,itime:itime,:,itimerange,ivar,inetwork), &
               var=vol7d_in%dativar%r(ivar), &
               coord_3d_in=coord_3d_in)
            ELSE
              CALL compute(this, &
               vol7d_in%voldatir(:,itime,:,itimerange,ivar,inetwork), &
               vol7d_out%voldatir(:,itime:itime,:,itimerange,ivar,inetwork), &
               var=vol7d_in%dativar%r(ivar), &
               coord_3d_in=vol7d_in%voldatir(:,itime:itime,:,itimerange, &
               lvar_coord_vol,inetwork))
            ENDIF
          ELSE
            CALL compute(this, &
             vol7d_in%voldatir(:,itime,:,itimerange,ivar,inetwork), &
             vol7d_out%voldatir(:,itime:itime,:,itimerange,ivar,inetwork), &
             var=vol7d_in%dativar%r(ivar))
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO

ENDIF

END SUBROUTINE v7d_v7d_transform_compute


!> Performs the specified abstract transformation on the data provided.
!! The abstract transformation is specified by \a this parameter; the
!! corresponding specifical transformation (\a grid_transform object)
!! is created and destroyed internally. The output transformed object
!! is created internally and it does not require preliminary
!! initialisation. The success of the transformation can be checked
!! with the \a c_e method: c_e(vol7d_out).
SUBROUTINE v7d_v7d_transform(this, vol7d_in, vol7d_out, v7d, lev_out, vol7d_coord_in, &
 categoryappend)
TYPE(transform_def),INTENT(in) :: this !< object specifying the abstract transformation
TYPE(vol7d),INTENT(inout) :: vol7d_in !< object to be transformed, it is not modified, despite the INTENT(inout)
TYPE(vol7d),INTENT(out) :: vol7d_out !< transformed object, it does not require initialisation
TYPE(vol7d),INTENT(in),OPTIONAL :: v7d !< object containing a list of points over which transformation has to be done (required by some transformation types)
TYPE(vol7d_level),INTENT(in),OPTIONAL,TARGET :: lev_out(:) !< vol7d_level object defining target vertical grid, for vertical interpolations
TYPE(vol7d),INTENT(in),OPTIONAL :: vol7d_coord_in !< object providing time constant input vertical coordinate for some kind of vertical interpolations
CHARACTER(len=*),INTENT(in),OPTIONAL :: categoryappend !< append this suffix to log4fortran namespace category

INTEGER :: nvar, inetwork
TYPE(grid_transform) :: grid_trans
TYPE(vol7d_level),POINTER :: llev_out(:)
TYPE(vol7d_level) :: input_levtype, output_levtype
TYPE(vol7d_var) :: vcoord_var
REAL,ALLOCATABLE :: coord_3d_in(:,:,:)
INTEGER :: var_coord_in, var_coord_vol, i, k, ulstart, ulend, spos
INTEGER,ALLOCATABLE :: point_index(:)
TYPE(vol7d) :: v7d_locana, vol7d_tmpana
CHARACTER(len=80) :: trans_type
LOGICAL,ALLOCATABLE :: mask_in(:), point_mask(:)

CALL vol7d_alloc_vol(vol7d_in) ! be safe
nvar=0
IF (ASSOCIATED(vol7d_in%dativar%r)) nvar=SIZE(vol7d_in%dativar%r)

CALL init(v7d_locana)
IF (PRESENT(v7d)) v7d_locana = v7d
CALL init(vol7d_out, time_definition=vol7d_in%time_definition)

CALL get_val(this, trans_type=trans_type)

var_coord_vol = imiss
IF (trans_type == 'vertint') THEN

  IF (PRESENT(lev_out)) THEN

! if vol7d_coord_in provided and allocated, check that it fits
    var_coord_in = -1
    IF (PRESENT(vol7d_coord_in)) THEN
      IF (ASSOCIATED(vol7d_coord_in%voldatir) .AND. &
       ASSOCIATED(vol7d_coord_in%dativar%r)) THEN

! strictly 1 time, 1 timerange and 1 network
        IF (SIZE(vol7d_coord_in%voldatir,2) /= 1 .OR. &
         SIZE(vol7d_coord_in%voldatir,4) /= 1 .OR. &
         SIZE(vol7d_coord_in%voldatir,6) /= 1) THEN
          CALL l4f_log(L4F_ERROR, &
           'volume providing constant input vertical coordinate must have &
           &only 1 time, 1 timerange and 1 network')
          CALL raise_error()
          RETURN
        ENDIF

! search for variable providing vertical coordinate
        CALL get_val(this, output_levtype=output_levtype)
        vcoord_var = vol7d_var_new(vol7d_level_to_var(output_levtype))
        IF (.NOT.c_e(vcoord_var)) THEN
          CALL l4f_log(L4F_ERROR, &
           'requested output level type '//t2c(output_levtype%level1)// &
           ' does not correspond to any known physical variable for &
           &providing vertical coordinate')
          CALL raise_error()
          RETURN
        ENDIF

        var_coord_in = INDEX(vol7d_coord_in%dativar%r, vcoord_var)

        IF (var_coord_in <= 0) THEN
          CALL l4f_log(L4F_ERROR, &
           'volume providing constant input vertical coordinate contains no &
           &real variables matching output level type '//t2c(output_levtype%level1))
          CALL raise_error()
          RETURN
        ENDIF
        CALL l4f_log(L4F_INFO, &
         'Coordinate for vertint found in coord volume at position '// &
         t2c(var_coord_in))

! check vertical coordinate system
        CALL get_val(this, input_levtype=input_levtype)
        mask_in = & ! implicit allocation
         (vol7d_coord_in%level(:)%level1 == input_levtype%level1) .AND. &
         (vol7d_coord_in%level(:)%level2 == input_levtype%level2)
        ulstart = firsttrue(mask_in)
        ulend = lasttrue(mask_in)
        IF (ulstart == 0 .OR. ulend == 0) THEN
          CALL l4f_log(L4F_ERROR, &
           'coordinate file does not contain levels of type '// &
           t2c(input_levtype%level1)//'/'//t2c(input_levtype%level2)// &
           ' specified for input data')
          CALL raise_error()
          RETURN
        ENDIF

        coord_3d_in = vol7d_coord_in%voldatir(:,1:1,ulstart:ulend,1,var_coord_in,1) ! dirty 1:1, implicit allocation
! special case
        IF (output_levtype%level1 == 103 &
         .OR. output_levtype%level1 == 108) THEN ! surface coordinate needed
          spos = firsttrue(vol7d_coord_in%level(:) == vol7d_level_new(1))
          IF (spos == 0) THEN
            CALL l4f_log(L4F_ERROR, &
             'output level '//t2c(output_levtype%level1)// &
             ' requested, but height/press of surface not provided in coordinate file')
            CALL raise_error()
            RETURN
          ENDIF
          DO k = 1, SIZE(coord_3d_in,3)
            WHERE(c_e(coord_3d_in(:,:,k)) .AND. &
             c_e(vol7d_coord_in%voldatir(:,1:1,spos,1,var_coord_in,1)))
              coord_3d_in(:,:,k) = coord_3d_in(:,:,k) - &
               vol7d_coord_in%voldatir(:,1:1,spos,1,var_coord_in,1)
            ELSEWHERE
              coord_3d_in(:,:,k) = rmiss
            END WHERE
          ENDDO
        ENDIF

      ENDIF
    ENDIF

    IF (var_coord_in <= 0) THEN ! search for coordinate within volume
! search for variable providing vertical coordinate
      CALL get_val(this, output_levtype=output_levtype)
      vcoord_var = vol7d_var_new(vol7d_level_to_var(output_levtype))
      IF (c_e(vcoord_var)) THEN
        DO i = 1, SIZE(vol7d_in%dativar%r)
          IF (vol7d_in%dativar%r(i) == vcoord_var) THEN
            var_coord_vol = i
            EXIT
          ENDIF
        ENDDO

        IF (c_e(var_coord_vol)) THEN
          CALL l4f_log(L4F_INFO, &
           'Coordinate for vertint found in input volume at position '// &
           t2c(var_coord_vol))
        ENDIF

      ENDIF
    ENDIF

    IF (var_coord_in > 0) THEN
      CALL init(grid_trans, this, lev_in=vol7d_in%level, lev_out=lev_out, &
       coord_3d_in=coord_3d_in, categoryappend=categoryappend)
    ELSE
      CALL init(grid_trans, this, lev_in=vol7d_in%level, lev_out=lev_out, &
       categoryappend=categoryappend)
    ENDIF

    CALL get_val(grid_trans, output_level_auto=llev_out) ! get levels if auto-generated
    IF (.NOT.associated(llev_out)) llev_out => lev_out

    IF (c_e(grid_trans)) THEN! .AND. nvar > 0) THEN

      CALL vol7d_alloc(vol7d_out, nana=SIZE(vol7d_in%ana), &
       ntime=SIZE(vol7d_in%time), ntimerange=SIZE(vol7d_in%timerange), &
       nlevel=SIZE(llev_out), nnetwork=SIZE(vol7d_in%network), ndativarr=nvar)
      vol7d_out%ana(:) = vol7d_in%ana(:)

      CALL vol7d_alloc_vol(vol7d_out)

! no need to check c_e(var_coord_vol) here since the presence of
! this%coord_3d_in (external) has precedence over coord_3d_in internal
! in grid_transform_compute
      CALL compute(grid_trans, vol7d_in, vol7d_out, llev_out, &
       var_coord_vol=var_coord_vol)
    ELSE
      CALL l4f_log(L4F_ERROR, 'v7d_v7d_transform: transformation not valid')
      CALL raise_error()
    ENDIF
  ELSE
    CALL l4f_log(L4F_ERROR, &
     'v7d_v7d_transform: vertint requested but lev_out not provided')
    CALL raise_error()
  ENDIF

ELSE

  CALL init(grid_trans, this, vol7d_in, v7d_locana, &
   categoryappend=categoryappend)
! if this init is successful, I am sure that v7d_locana%ana is associated

  IF (c_e(grid_trans)) THEN! .AND. nvar > 0) THEN

    CALL vol7d_alloc(vol7d_out, nana=SIZE(v7d_locana%ana), &
     ntime=SIZE(vol7d_in%time), ntimerange=SIZE(vol7d_in%timerange), &
     nlevel=SIZE(vol7d_in%level), nnetwork=SIZE(vol7d_in%network), ndativarr=nvar)
    vol7d_out%ana = v7d_locana%ana

    CALL get_val(grid_trans, point_mask=point_mask, output_point_index=point_index)

    IF (ALLOCATED(point_index)) THEN
      CALL vol7d_alloc(vol7d_out, nanavari=1)
      CALL init(vol7d_out%anavar%i(1), 'B01192')
    ENDIF

    CALL vol7d_alloc_vol(vol7d_out)

    IF (ALLOCATED(point_index)) THEN
      DO inetwork = 1, SIZE(vol7d_in%network)
        vol7d_out%volanai(:,1,inetwork) = point_index(:)
      ENDDO
    ENDIF
    CALL compute(grid_trans, vol7d_in, vol7d_out)

    IF (ALLOCATED(point_mask)) THEN ! keep full ana
      IF (SIZE(point_mask) /= SIZE(vol7d_in%ana)) THEN
        CALL l4f_log(L4F_WARN, &
         'v7d_v7d_transform: inconsistency in point size: '//t2c(SIZE(point_mask)) &
         //':'//t2c(SIZE(vol7d_in%ana)))
      ELSE
#ifdef DEBUG
        CALL l4f_log(L4F_DEBUG, 'v7d_v7d_transform: merging ana from in to out')
#endif
        CALL vol7d_copy(vol7d_in, vol7d_tmpana, &
         lana=point_mask, lnetwork=(/.TRUE./), &
         ltime=(/.FALSE./), ltimerange=(/.FALSE./), llevel=(/.FALSE./))
        CALL vol7d_append(vol7d_out, vol7d_tmpana)
      ENDIF
    ENDIF

  ELSE
    CALL l4f_log(L4F_ERROR, 'v7d_v7d_transform: transformation not valid')
    CALL raise_error()
  ENDIF

ENDIF

CALL delete (grid_trans)
IF (.NOT. PRESENT(v7d)) CALL delete(v7d_locana)

END SUBROUTINE v7d_v7d_transform


!> Unrotate the wind components.
!! It converts u and v components of vector quantities relative to the
!! defined grid in the direction of increasing x and y coordinates to
!! u and v components relative to easterly and notherly direction. The
!! original fields are overwritten.
!! \todo Check and correct wind component flag (to be moved in
!! griddim_def?)
subroutine vg6d_wind_unrot(this)
type(volgrid6d) :: this !< object containing wind to be unrotated

integer :: component_flag

call get_val(this%griddim,component_flag=component_flag)

if (component_flag == 1) then
  call l4f_category_log(this%category,L4F_INFO, &
   "unrotating vector components")
  call vg6d_wind__un_rot(this,.false.)
  call set_val(this%griddim,component_flag=0)
else
  call l4f_category_log(this%category,L4F_INFO, &
   "no need to unrotate vector components")
end if

end subroutine vg6d_wind_unrot


!> Rotate the wind components.
!! It converts u and v components of vector quantities 
!! relative to easterly and notherly direction to
!! defined grid in the direction of increasing x and y coordinates.
!! The original fields are overwritten.
subroutine vg6d_wind_rot(this)
type(volgrid6d) :: this !< object containing wind to be rotated

integer :: component_flag

call get_val(this%griddim,component_flag=component_flag)

if (component_flag == 0) then
  call l4f_category_log(this%category,L4F_INFO, &
   "rotating vector components")
  call vg6d_wind__un_rot(this,.true.)
  call set_val(this%griddim,component_flag=1)
else
  call l4f_category_log(this%category,L4F_INFO, &
   "no need to rotate vector components")
end if

end subroutine vg6d_wind_rot


! Generic UnRotate the wind components.
SUBROUTINE vg6d_wind__un_rot(this,rot)
TYPE(volgrid6d) :: this ! object containing wind to be (un)rotated
LOGICAL :: rot ! if .true. rotate else unrotate

INTEGER :: i, j, k, l, a11, a12, a21, a22, stallo
double precision,pointer :: rot_mat(:,:,:)
real,allocatable :: tmp_arr(:,:)
REAL,POINTER :: voldatiu(:,:), voldativ(:,:)
INTEGER,POINTER :: iu(:), iv(:)

IF (.NOT.ASSOCIATED(this%var)) THEN
  CALL l4f_category_log(this%category, L4F_ERROR, &
   "trying to unrotate an incomplete volgrid6d object")
  CALL raise_fatal_error()
!  RETURN
ENDIF

CALL volgrid6d_var_hor_comp_index(this%var, iu, iv)
IF (.NOT.ASSOCIATED(iu)) THEN
  CALL l4f_category_log(this%category,L4F_ERROR, &
   "unrotation impossible")
  CALL raise_fatal_error()
!  RETURN
ENDIF

! Temporary workspace
ALLOCATE(tmp_arr(this%griddim%dim%nx, this%griddim%dim%ny),stat=stallo)
IF (stallo /= 0) THEN
  CALL l4f_category_log(this%category, L4F_FATAL, "allocating memory")
  CALL raise_fatal_error()
ENDIF
! allocate once for speed
IF (.NOT.ASSOCIATED(this%voldati)) THEN
  ALLOCATE(voldatiu(this%griddim%dim%nx, this%griddim%dim%ny), &
   voldativ(this%griddim%dim%nx, this%griddim%dim%ny))
ENDIF

CALL griddim_unproj(this%griddim)
CALL wind_unrot(this%griddim, rot_mat)

a11=1
if (rot)then
  a12=2
  a21=3
else
  a12=3
  a21=2
end if
a22=4

DO l = 1, SIZE(iu)
  DO k = 1, SIZE(this%timerange)
    DO j = 1, SIZE(this%time)
      DO i = 1, SIZE(this%level)
! get data
        CALL volgrid_get_vol_2d(this, i, j, k, iu(l), voldatiu)
        CALL volgrid_get_vol_2d(this, i, j, k, iv(l), voldativ)
! convert units forward
!        CALL compute(conv_fwd(iu(l)), voldatiu)
!        CALL compute(conv_fwd(iv(l)), voldativ)

! multiply wind components by rotation matrix
        WHERE(voldatiu /= rmiss .AND. voldativ /= rmiss)
          tmp_arr(:,:) = real(voldatiu(:,:)*rot_mat(:,:,a11) + &
           voldativ(:,:)*rot_mat(:,:,a12))
          voldativ(:,:) = real(voldatiu(:,:)*rot_mat(:,:,a21) + &
           voldativ(:,:)*rot_mat(:,:,a22))
          voldatiu(:,:) = tmp_arr(:,:)
        END WHERE
! convert units backward
!        CALL uncompute(conv_fwd(iu(l)), voldatiu)
!        CALL uncompute(conv_fwd(iv(l)), voldativ)
! put data
        CALL volgrid_set_vol_2d(this, i, j, k, iu(l), voldatiu)
        CALL volgrid_set_vol_2d(this, i, j, k, iv(l), voldativ)
      ENDDO
    ENDDO
  ENDDO
ENDDO

IF (.NOT.ASSOCIATED(this%voldati)) THEN
  DEALLOCATE(voldatiu, voldativ)
ENDIF
DEALLOCATE(rot_mat, tmp_arr, iu, iv)

END SUBROUTINE vg6d_wind__un_rot


!!$ try to understand the problem:
!!$
!!$ case:
!!$
!!$ 1) we have only one volume: we have to provide the direction of shift
!!$                           compute H and traslate on it
!!$ 2) we have two volumes:
!!$      1) volume U and volume V: compute H and traslate on it
!!$      2) volume U/V and volume H : translate U/V on H
!!$ 3) we have tree volumes: translate U and V on H
!!$
!!$ strange cases:
!!$ 1) do not have U in volume U
!!$ 2) do not have V in volume V
!!$ 3) we have others variables more than U and V in volumes U e V
!!$
!!$
!!$ so the steps are:
!!$ 1) find the volumes 
!!$ 2) define or compute H grid
!!$ 3) trasform the volumes in H 

!!$ N.B.
!!$ case 1) for only one vg6d (U or V) is not managed, but
!!$ the not pubblic subroutines will work but you have to know what you want to do


!> Convert grids type C to type A.
!! Use this to interpolate data from grid type C to grid type A
!! Grids type are defined by Arakawa.
!!
!! We need to find these types of area in a vg6d array
!! T   area of points with temterature etc.
!! U   area of points with u components of winds
!! V   area of points with v components of winds
!!
!! this method works if it finds
!! two volumes:
!!      1) volume U and volume V: compute H and traslate on it
!!      2) volume U/V and volume H : translate U/V on H
!! three volumes: translate U and V on H
!!
!! try to work well on more datasets at once
subroutine vg6d_c2a (this)

TYPE(volgrid6d),INTENT(inout)  :: this(:)      !< vettor of volumes volgrid6d to elaborate

integer :: ngrid,igrid,jgrid,ugrid,vgrid,tgrid
doubleprecision :: xmin, xmax, ymin, ymax
doubleprecision :: xmin_t, xmax_t, ymin_t, ymax_t
doubleprecision :: step_lon_t,step_lat_t
character(len=80) :: type_t,type
TYPE(griddim_def):: griddim_t

ngrid=size(this)

do igrid=1,ngrid

  call init(griddim_t)

  call get_val(this(igrid)%griddim,xmin=xmin_t, xmax=xmax_t, ymin=ymin_t, ymax=ymax_t,proj_type=type_t)
  step_lon_t=(xmax_t-xmin_t)/dble(this(igrid)%griddim%dim%nx-1)
  step_lat_t=(ymax_t-ymin_t)/dble(this(igrid)%griddim%dim%ny-1)

  do jgrid=1,ngrid

    ugrid=imiss
    vgrid=imiss
    tgrid=imiss

#ifdef DEBUG
    call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: search U/V/T points:"//to_char(igrid)//to_char(jgrid))
    call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: test delta: "//to_char(step_lon_t)//to_char(step_lat_t))
#endif
    
    if (this(igrid)%griddim == this(jgrid)%griddim ) cycle

    if (this(igrid)%griddim%dim%nx == this(jgrid)%griddim%dim%nx .and. &
     this(igrid)%griddim%dim%ny == this(jgrid)%griddim%dim%ny ) then

      call get_val(this(jgrid)%griddim,xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,proj_type=type)
      
      if (type_t /= type )cycle

#ifdef DEBUG
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: test U "//&
       to_char(xmin)//to_char(xmax)//to_char(ymin)//to_char(ymax))

      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"diff coordinate lon"//&
       to_char(abs(xmin - (xmin_t+step_lon_t/2.d0)))//&
       to_char(abs(xmax - (xmax_t+step_lon_t/2.d0))))
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"diff coordinate lat"//&
       to_char(abs(ymin - (ymin_t+step_lat_t/2.d0)))//&
       to_char(abs(ymax - (ymax_t+step_lat_t/2.d0))))
#endif

      if ( abs(xmin - (xmin_t+step_lon_t/2.d0)) < 1.d-3 .and. abs(xmax - (xmax_t+step_lon_t/2.d0)) < 1.d-3 ) then
        if ( abs(ymin - ymin_t) < 1.d-3 .and. abs(ymax - ymax_t) < 1.d-3 ) then

#ifdef DEBUG
          call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: found U")
#endif
          ugrid=jgrid
          tgrid=igrid

        end if
      end if

#ifdef DEBUG
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: test V "//&
       to_char(xmin)//to_char(xmax)//to_char(ymin)//to_char(ymax))
#endif

      if ( abs(ymin - (ymin_t+step_lat_t/2.d0)) < 1.d-3 .and. abs(ymax - (ymax_t+step_lat_t/2.d0)) < 1.d-3 ) then
        if ( abs(xmin - xmin_t) < 1.d-3 .and. abs(xmax - xmax_t) < 1.d-3  ) then
          
#ifdef DEBUG
          call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: found V")
#endif
          vgrid=jgrid
          tgrid=igrid

        end if
      end if


      ! test if we have U and V

#ifdef DEBUG
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: test U and V"//&
       to_char(xmin_t)//to_char(xmax_t)//to_char(ymin_t)//to_char(ymax_t)//&
       to_char(xmin)//to_char(xmax)//to_char(ymin)//to_char(ymax))

      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"UV diff coordinate lon"//&
       to_char(abs(xmin_t - xmin)-step_lon_t/2.d0)//&
       to_char(abs(xmax_t - xmax)-step_lon_t/2.d0))
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"UV diff coordinate lat"//&
       to_char(abs(ymin_t - ymin) -step_lat_t/2.d0)//&
       to_char(abs(ymax_t - ymax)-step_lat_t/2.d0))
#endif
      
      if ( abs(ymin - (ymin_t+step_lat_t/2.d0)) < 2.d-3 .and. abs(ymax - (ymax_t+step_lat_t/2.d0)) < 2.d-3 ) then
        if ( abs(xmin_t - (xmin+step_lon_t/2.d0)) < 2.d-3 .and. abs(xmax_t - (xmax+step_lon_t/2.d0)) < 2.d-3 ) then
          
#ifdef DEBUG
          call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid: found U and V case up and right")
#endif

          vgrid=jgrid
          ugrid=igrid

          call init(griddim_t,xmin=xmin, xmax=xmax, ymin=ymin_t, ymax=ymax_t)

        end if
      end if
    end if

                                ! abbiamo almeno due volumi: riportiamo U e/o V su T
    if (c_e(ugrid)) then
#ifdef DEBUG
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid U points: interpolate U on T "//to_char(tgrid)//to_char(ugrid))
#endif
      if (c_e(tgrid))then
        call vg6d_c2a_grid(this(ugrid),this(tgrid)%griddim,cgrid=1)
      else
        call vg6d_c2a_grid(this(ugrid),griddim_t,cgrid=1)
      end if
      call vg6d_c2a_mat(this(ugrid),cgrid=1)
    end if
    
    if (c_e(vgrid)) then
#ifdef DEBUG
      call l4f_category_log(this(igrid)%category,L4F_DEBUG,"C grid V points: interpolate V on T "//to_char(tgrid)//to_char(vgrid))
#endif
      if (c_e(tgrid))then
        call vg6d_c2a_grid(this(vgrid),this(tgrid)%griddim,cgrid=2)
      else
        call vg6d_c2a_grid(this(vgrid),griddim_t,cgrid=2)
      end if
      call vg6d_c2a_mat(this(vgrid),cgrid=2)
    end if

  end do

  call delete(griddim_t)

end do
  

end subroutine vg6d_c2a


! Convert C grid to A grid
subroutine vg6d_c2a_grid(this,griddim_t,cgrid)

type(volgrid6d),intent(inout) :: this ! object containing fields to be translated (U or V points)
type(griddim_def),intent(in),optional :: griddim_t ! object containing grid of T points
integer,intent(in) :: cgrid ! in C grid (Arakawa) we have 0=T,1=U,2=V points

doubleprecision :: xmin, xmax, ymin, ymax
doubleprecision :: step_lon,step_lat


if (present(griddim_t)) then

 call get_val(griddim_t,xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
 call set_val(this%griddim,xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
! improve grid definition precision
 CALL griddim_setsteps(this%griddim)

else

  select case (cgrid)

  case(0)

#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"C grid: T points, nothing to do")
#endif
    return

  case (1)

#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"C grid: U points, we need interpolation")
#endif

    call get_val(this%griddim, xmin=xmin, xmax=xmax)
    step_lon=(xmax-xmin)/dble(this%griddim%dim%nx-1)
    xmin=xmin-step_lon/2.d0
    xmax=xmax-step_lon/2.d0
    call set_val(this%griddim, xmin=xmin, xmax=xmax)
! improve grid definition precision
    CALL griddim_setsteps(this%griddim)
    
  case (2)

#ifdef DEBUG
    call l4f_category_log(this%category,L4F_DEBUG,"C grid: V points, we need interpolation")
#endif

    call get_val(this%griddim, ymin=ymin, ymax=ymax)
    step_lat=(ymax-ymin)/dble(this%griddim%dim%ny-1)
    ymin=ymin-step_lat/2.d0
    ymax=ymax-step_lat/2.d0
    call set_val(this%griddim, ymin=ymin, ymax=ymax)
! improve grid definition precision
    CALL griddim_setsteps(this%griddim)
    
  case default

    call l4f_category_log(this%category,L4F_FATAL,"C grid type not known")
    call raise_fatal_error ()

  end select

end if


call griddim_unproj(this%griddim)


end subroutine vg6d_c2a_grid

! Convert C grid to A grid
subroutine vg6d_c2a_mat(this,cgrid)

type(volgrid6d),intent(inout) :: this ! object containing fields to be translated
integer,intent(in) :: cgrid ! in C grid (Arakawa) we have 0=T,1=U,2=V points

INTEGER :: i, j, k, iv, stallo
REAL,ALLOCATABLE :: tmp_arr(:,:)
REAL,POINTER :: voldatiuv(:,:)


IF (cgrid == 0) RETURN ! nothing to do
IF (cgrid /= 1 .AND. cgrid /= 2) THEN ! wrong cgrid
  CALL l4f_category_log(this%category,L4F_FATAL,"C grid type "// &
   TRIM(to_char(cgrid))//" not known")
  CALL raise_error()
  RETURN
ENDIF

! Temporary workspace
ALLOCATE(tmp_arr(this%griddim%dim%nx, this%griddim%dim%ny),stat=stallo)
if (stallo /=0)then
  call l4f_log(L4F_FATAL,"allocating memory")
  call raise_fatal_error()
end if

! allocate once for speed
IF (.NOT.ASSOCIATED(this%voldati)) THEN
  ALLOCATE(voldatiuv(this%griddim%dim%nx, this%griddim%dim%ny), stat=stallo)
  IF (stallo /= 0) THEN
    CALL l4f_log(L4F_FATAL,"allocating memory")
    CALL raise_fatal_error()
  ENDIF
ENDIF

IF (cgrid == 1) THEN ! U points to H points
  DO iv = 1, SIZE(this%var)
    DO k = 1, SIZE(this%timerange)
      DO j = 1, SIZE(this%time)
        DO i = 1, SIZE(this%level)
          tmp_arr(:,:) = rmiss
          CALL volgrid_get_vol_2d(this, i, j, k, iv, voldatiuv)

! West boundary extrapolation
          WHERE(voldatiuv(1,:) /= rmiss .AND. voldatiuv(2,:) /= rmiss)
            tmp_arr(1,:) = voldatiuv(1,:) - (voldatiuv(2,:) - voldatiuv(1,:)) / 2.
          END WHERE

! Rest of the field
          WHERE(voldatiuv(1:this%griddim%dim%nx-1,:) /= rmiss .AND. &
           voldatiuv(2:this%griddim%dim%nx,:) /= rmiss)
            tmp_arr(2:this%griddim%dim%nx,:) = &
             (voldatiuv(1:this%griddim%dim%nx-1,:) + &
             voldatiuv(2:this%griddim%dim%nx,:)) / 2.
          END WHERE

          voldatiuv(:,:) = tmp_arr
          CALL volgrid_set_vol_2d(this, i, j, k, iv, voldatiuv)
        ENDDO
      ENDDO
    ENDDO
  ENDDO

ELSE IF (cgrid == 2) THEN ! V points to H points
  DO iv = 1, SIZE(this%var)
    DO k = 1, SIZE(this%timerange)
      DO j = 1, SIZE(this%time)
        DO i = 1, SIZE(this%level)
          tmp_arr(:,:) = rmiss
          CALL volgrid_get_vol_2d(this, i, j, k, iv, voldatiuv)

! South boundary extrapolation
          WHERE(voldatiuv(:,1) /= rmiss .AND. voldatiuv(:,2) /= rmiss)
            tmp_arr(:,1) = voldatiuv(:,1) - (voldatiuv(:,2) - voldatiuv(:,1)) / 2.
          END WHERE

! Rest of the field
          WHERE(voldatiuv(:,1:this%griddim%dim%ny-1) /= rmiss .AND. &
           voldatiuv(:,2:this%griddim%dim%ny) /= rmiss)
            tmp_arr(:,2:this%griddim%dim%ny) = &
             (voldatiuv(:,1:this%griddim%dim%ny-1) + &
             voldatiuv(:,2:this%griddim%dim%ny)) / 2.
          END WHERE

          voldatiuv(:,:) = tmp_arr
          CALL volgrid_set_vol_2d(this, i, j, k, iv, voldatiuv)
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDIF ! tertium non datur

IF (.NOT.ASSOCIATED(this%voldati)) THEN
  DEALLOCATE(voldatiuv)
ENDIF
DEALLOCATE (tmp_arr)

end subroutine vg6d_c2a_mat


!> \brief Display object on screen
!!
!! Show brief content on screen.
subroutine display_volgrid6d (this)

TYPE(volgrid6d),intent(in) :: this !< object to display
integer :: i

#ifdef DEBUG
call l4f_category_log(this%category,L4F_DEBUG,"ora mostro gridinfo " )
#endif

print*,"----------------------- volgrid6d display ---------------------"
call display(this%griddim)

IF (ASSOCIATED(this%time))then
  print*,"---- time vector ----"
  print*,"elements=",size(this%time)
  do i=1, size(this%time)
    call display(this%time(i))
  end do
end IF

IF (ASSOCIATED(this%timerange))then
  print*,"---- timerange vector ----"
  print*,"elements=",size(this%timerange)
  do i=1, size(this%timerange)
    call display(this%timerange(i))
  end do
end IF

IF (ASSOCIATED(this%level))then
  print*,"---- level vector ----"
  print*,"elements=",size(this%level)
  do i=1, size(this%level)
    call display(this%level(i))
  end do
end IF

IF (ASSOCIATED(this%var))then
  print*,"---- var vector ----"
  print*,"elements=",size(this%var)
  do i=1, size(this%var)
    call display(this%var(i))
  end do
end IF

IF (ASSOCIATED(this%gaid))then
  print*,"---- gaid vector (present mask only) ----"
  print*,"elements=",shape(this%gaid)
  PRINT*,c_e(RESHAPE(this%gaid,(/SIZE(this%gaid)/)))
end IF

print*,"--------------------------------------------------------------"


end subroutine display_volgrid6d


!> \brief Display vector of object on screen
!!
!! Show brief content on screen.
subroutine display_volgrid6dv (this)

TYPE(volgrid6d),intent(in) :: this(:) !< vector of object to display
integer :: i

print*,"----------------------- volgrid6d  vector ---------------------"

print*,"elements=",size(this)

do i=1, size(this)

#ifdef DEBUG
  call l4f_category_log(this(i)%category,L4F_DEBUG,"ora mostro il vettore volgrid6d" )
#endif

  call display(this(i))

end do
print*,"--------------------------------------------------------------"

end subroutine display_volgrid6dv


!> Reduce some dimensions (level and timerage) for semplification (rounding).
!! Vector version of vg6dv_rounding
subroutine vg6dv_rounding(vg6din,vg6dout,level,timerange,nostatproc,merge)
type(volgrid6d),intent(in) :: vg6din(:)  !< input volume
type(volgrid6d),intent(out),pointer :: vg6dout(:) !> output volume
type(vol7d_level),intent(in),optional :: level(:) !< almost equal level list
type(vol7d_timerange),intent(in),optional :: timerange(:) !< almost equal timerange list
logical,intent(in),optional :: merge !< if there are data on more then one almost equal levels or timeranges will be merged POINT BY POINT with priority for the fird data found ordered by icreasing var index
logical,intent(in),optional :: nostatproc !< do not take in account statistical processing code in timerange and P2

integer :: i

allocate(vg6dout(size(vg6din)))

do i = 1, size(vg6din)
  call vg6d_rounding(vg6din(i),vg6dout(i),level,timerange,nostatproc,merge)
end do

end subroutine vg6dv_rounding

!> Reduce some dimensions (level and timerage) for semplification (rounding).
!! You can use this for simplify and use variables in computation like alchimia
!! where fields have to be on the same coordinate
!! examples:
!! means in time for short periods and istantaneous values
!! 2 meter and surface levels
!! If there are data on more then one almost equal levels or timeranges, the first var present (at least one point)
!! will be taken (order is by icreasing var index).
!! You can use predefined values for classic semplification
!! almost_equal_levels and almost_equal_timeranges
!! The level or timerange in output will be defined by the first element of level and timerange list
subroutine vg6d_rounding(vg6din,vg6dout,level,timerange,nostatproc,merge)
type(volgrid6d),intent(in) :: vg6din  !< input volume
type(volgrid6d),intent(out) :: vg6dout !> output volume
type(vol7d_level),intent(in),optional :: level(:) !< almost equal level list
type(vol7d_timerange),intent(in),optional :: timerange(:) !< almost equal timerange list
logical,intent(in),optional :: merge !< if there are data on more then one almost equal levels or timeranges
!! will be merged POINT BY POINT with priority for the fird data found ordered by icreasing var index
logical,intent(in),optional :: nostatproc !< do not take in account statistical processing code in timerange and P2

integer :: ilevel,itimerange
type(vol7d_level) :: roundlevel(size(vg6din%level)) 
type(vol7d_timerange) :: roundtimerange(size(vg6din%timerange))

roundlevel=vg6din%level

if (present(level))then
  do ilevel = 1, size(vg6din%level)
    if ((any(vg6din%level(ilevel) .almosteq. level))) then
      roundlevel(ilevel)=level(1)
    end if
  end do
end if

roundtimerange=vg6din%timerange

if (present(timerange))then
  do itimerange = 1, size(vg6din%timerange)
    if ((any(vg6din%timerange(itimerange) .almosteq. timerange))) then
      roundtimerange(itimerange)=timerange(1)
    end if
  end do
end if

!set istantaneous values everywere
!preserve p1 for forecast time
if (optio_log(nostatproc)) then
  roundtimerange(:)%timerange=254
  roundtimerange(:)%p2=imiss
end if


call vg6d_reduce(vg6din,vg6dout,roundlevel,roundtimerange,merge)

end subroutine vg6d_rounding

!> Reduce some dimensions (level and timerage).
!! You can pass a volume and specify duplicated levels and timeranges in roundlevel and roundtimerange;
!! you get unique levels and timeranges in output.
!! If there are data on equal levels or timeranges, the first var present (at least one point)
!! will be taken (order is by icreasing var index).
!! you can specify merge and if there are data on equal levels or timeranges will be merged POINT BY POINT 
!! with priority for the first data found ordered by icreasing var index (require to decode all the data)
!! Data are decoded only if needed so the output should be with or without voldata allocated
subroutine vg6d_reduce(vg6din,vg6dout,roundlevel,roundtimerange,merge)
type(volgrid6d),intent(in) :: vg6din  !< input volume
type(volgrid6d),intent(out) :: vg6dout   !< output volume
type(vol7d_level),intent(in) :: roundlevel(:) !< new level list to use for rounding
type(vol7d_timerange),intent(in) :: roundtimerange(:) !< new timerange list to use for rounding
logical,intent(in),optional :: merge !< if there are data on equal levels or timeranges will be merged POINT BY POINT with priority for the first data found ordered by icreasing var index (require to decode all the data)

integer :: nlevel,ntime,ntimerange,nvar,ilevel,itimerange,ivar,indl,indt,itime,nx,ny
real,allocatable :: vol2d(:,:)

nx=vg6din%griddim%dim%nx
ny=vg6din%griddim%dim%ny
nlevel=count_distinct(roundlevel,back=.true.)
ntime=size(vg6din%time)
ntimerange=count_distinct(roundtimerange,back=.true.)
nvar=size(vg6din%var)

call init(vg6dout, vg6din%griddim, vg6din%time_definition, categoryappend="generated by vg6d_reduce")
call volgrid6d_alloc(vg6dout, vg6din%griddim%dim, ntime, nlevel, ntimerange, nvar)

if ( ASSOCIATED(vg6din%voldati) .or. optio_log(merge)) then          
  call volgrid6d_alloc_vol(vg6dout,inivol=.true.,decode=.true.)
  allocate(vol2d(nx,ny))
else
  call volgrid6d_alloc_vol(vg6dout,inivol=.true.,decode=.false.)
end if

vg6dout%time=vg6din%time
vg6dout%var=vg6din%var
vg6dout%timerange=pack_distinct(roundtimerange,ntimerange,back=.true.)
vg6dout%level=pack_distinct(roundlevel,nlevel,back=.true.)
! sort modified dimensions
CALL sort(vg6dout%timerange)
CALL sort(vg6dout%level)

do ilevel=1,size(vg6din%level)
  indl=index(vg6dout%level,roundlevel(ilevel))
  do itimerange=1,size(vg6din%timerange)
    indt=index(vg6dout%timerange,roundtimerange(itimerange))
    do ivar=1, nvar
      do itime=1,ntime

        if ( ASSOCIATED(vg6din%voldati)) then
          vol2d=vg6din%voldati(:,:,ilevel,itime,itimerange,ivar)
        end if

        if (optio_log(merge)) then          

          if ( .not. ASSOCIATED(vg6din%voldati)) then
            CALL grid_id_decode_data(vg6din%gaid(ilevel,itime,itimerange,ivar), vol2d)
          end if

          !! merge present data point by point
          where (.not. c_e(vg6dout%voldati(:,:,indl,itime,indt,ivar))) 
          
            vg6dout%voldati(:,:,indl,itime,indt,ivar)=vol2d
          
          end where
        else if ( ASSOCIATED(vg6din%voldati)) then
          if (.not. any(c_e(vg6dout%voldati(:,:,indl,itime,indt,ivar))))then
            vg6dout%voldati(:,:,indl,itime,indt,ivar)=vol2d
          end if
        end if
      
        if (c_e(vg6din%gaid(ilevel,itime,itimerange,ivar)).and. .not. c_e(vg6dout%gaid(indl,itime,indt,ivar)))then
          call copy (vg6din%gaid(ilevel,itime,itimerange,ivar), vg6dout%gaid(indl,itime,indt,ivar))
        end if
      end do
    end do
  end do
end do

if ( ASSOCIATED(vg6din%voldati) .or. optio_log(merge)) then          
  deallocate(vol2d)
end if

end subroutine vg6d_reduce


end module volgrid6d_class



!>\example example_vg6d_3.f90
!!\brief Programma esempio semplice per gridinfo e volgrid6d.
!!
!! Programma che importa da file un vettore di gridinfo poi lo importa in volgrid6d. Da volgrid6d viene di nuovo creato un vettore di gridinfo per poi exportare su file.

!>\example example_vg6d_5.f90
!!\brief  Programma trasformazione da volgrid6d a volgrid6d
!!
!! Legge grib da un file e li organizza in un vettore di strutture
!! volgrid6d mettendoli a disposizione per eventuali elaborazioni;
!! vengono poi riesportati a un file grib

!>\example  example_vg6d_8.f90
!! \brief Programma scrittura su file vettore di anagrafica

!>\example example_vg6d_6.f90
!! \brief Programma trasformazione da volgrid6d a vol7d

!>\example example_vg6d_7.f90
!! \brief Programma trasformazione da vol7d a volgrid6d

!>\example example_vg6d_9.f90
!! \brief  Example to create a grib editionNumber = 2 file from data generated in memory using a grib_api template
