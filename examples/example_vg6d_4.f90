program demo4

use gridinfo_class
use grid_class
use log4fortran
use grib_api
use volgrid6d_class
use char_utilities

implicit none

integer :: category,ier
character(len=512):: a_name,infile='../data/in.grb',outfile='out.grb'
type (gridinfo_def) :: gridinfo
character:: ch
integer                            ::  ifile,ofile,gaid
integer                            ::  iret

integer :: ix,iy,fx,fy,iox,ioy,fox,foy,inx,iny,fnx,fny,newx,newy
doubleprecision ::  ilon=0.,ilat=30.,flon=30.,flat=60.
real, allocatable :: field(:,:),fieldz(:,:)
type(griddim_def) :: griddim_out
type(transform_def) :: trans
type(grid_transform) :: grid_trans

integer :: nx=30,ny=30,component_flag=0
doubleprecision :: lon_min=0., lon_max=30., lat_min=30., lat_max=60.
doubleprecision :: latitude_south_pole=-32.5,longitude_south_pole=10.,angle_rotation=0.
character(len=80) :: type='regular_ll',trans_type='inter',sub_type='near'

doubleprecision ::x,y,lon,lat


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo4")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"transforming from file:"//trim(infile))
call l4f_category_log(category,L4F_INFO,"transforming to   file:"//trim(outfile))
if(trans_type == 'inter')then

  call init(griddim_out,&
   type=type,nx=nx,ny=ny, &
   lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max, component_flag=component_flag, &
   latitude_south_pole=latitude_south_pole,longitude_south_pole=longitude_south_pole,angle_rotation=angle_rotation, &
   categoryappend="regular_ll")

  call griddim_unproj(griddim_out)

  print*,'grid di interpolazione >>>>>>>>>>>>>>>>>>>>'
  call display(griddim_out)

end if

call init(trans, trans_type=trans_type,sub_type=sub_type, &
 ilon=ilon,ilat=ilat,flon=flon,flat=flat,&
 categoryappend="trasformation")

call grib_open_file(ifile, trim(infile),'r')
call grib_open_file(ofile, trim(outfile),'w')


! Loop on all the messages in a file.

!     a new grib message is loaded from file
!     gaid is the grib id to be used in subsequent calls

gaid=-1
call  grib_new_from_file(ifile,gaid, iret) 


DO WHILE (iret == GRIB_SUCCESS)

   call l4f_category_log(category,L4F_INFO,"import gridinfo")

   call init (gridinfo,gaid=gaid,categoryappend="importato")
   call import(gridinfo)

   call display(gridinfo,namespace="")

   call l4f_category_log(category,L4F_INFO,"import")

   allocate (field(gridinfo%griddim%dim%nx,gridinfo%griddim%dim%ny))

   field=decode_gridinfo(gridinfo)

   call init(grid_trans, trans, in=gridinfo%griddim,out=griddim_out,categoryappend="gridtrasformato")

   call display(griddim_out)

   allocate (fieldz(griddim_out%dim%nx,griddim_out%dim%ny))

   call compute(grid_trans, field, fieldz)

   call delete(gridinfo%griddim)
   call copy(griddim_out,gridinfo%griddim,categoryappend="clonato")

! oppure per mantenere il vecchio gridinfo
!   call clone(gridinfo , gridinfo_out)
!   call delete(gridinfo_out%griddim)
!   call copy(griddim_out,gridinfo_out%griddim)

   call encode_gridinfo(gridinfo,fieldz)
   call export (gridinfo)
   call display(gridinfo,namespace="")

   call grib_write(gridinfo%gaid,ofile)

   call delete (grid_trans)
   call delete (gridinfo)
   deallocate (field,fieldz)

   gaid=-1
   call grib_new_from_file(ifile,gaid, iret)
   
end do

call delete (trans)
call delete(griddim_out)

call grib_close_file(ifile)
call grib_close_file(ofile)

call l4f_category_log(category,L4F_INFO,"terminato")

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo4

