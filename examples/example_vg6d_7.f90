program demo7

use log4fortran
use volgrid6d_class
use grid_class
use grid_transform_class
USE vol7d_dballe_class
USE vol7d_class
USE grib_api

implicit none

integer :: category,ier,i
character(len=512):: a_name,filename="out.bufr"
type (volgrid6d),pointer  :: volgrid(:)
type(transform_def) :: trans
type(vol7d_ana) :: ana(3)
type(vol7d) :: vol7d_in
TYPE(vol7d_dballe) :: v7d_import
type(griddim_def) :: griddim_out

integer :: nx=40,ny=40,component_flag=0,gaid_template
doubleprecision :: lon_min=0., lon_max=30., lat_min=30., lat_max=60.
doubleprecision :: latitude_south_pole=-32.5,longitude_south_pole=10.,angle_rotation=0.
character(len=80) :: type='regular_ll',trans_type='inter',sub_type='linear'

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo7")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

allocate (volgrid(1))

call init(griddim_out,&
 type=type,nx=nx,ny=ny, &
 lon_min=lon_min, lon_max=lon_max, lat_min=lat_min, lat_max=lat_max, component_flag=component_flag, &
 latitude_south_pole=latitude_south_pole,longitude_south_pole=longitude_south_pole,angle_rotation=angle_rotation, &
 categoryappend=type)

call griddim_unproj(griddim_out)

print*,'grid di interpolazione >>>>>>>>>>>>>>>>>>>>'
call display(griddim_out)

call grib_new_from_template (gaid_template,"regular_ll_sfc_grib1")

!trasformation object
call init(trans, trans_type=trans_type,sub_type=sub_type, categoryappend="trasformation")

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d_import,file=.true.,write=.false.,filename=filename,&
 categoryappend="importBUFR",format="BUFR")

call import (v7d_import,var=(/"B12001"/),varkind=(/"r"/))

call display(v7d_import%vol7d)

call l4f_category_log(category,L4F_INFO,"trasformato")
call transform(trans,griddim_out, vol7d_in=v7d_import%vol7d, volgrid6d_out=volgrid(1), categoryappend="trasform->")

call l4f_category_log(category,L4F_INFO,"export to GRIB")
call export (volgrid,gaid_template=gaid_template,categoryappend="volume scritto")

if (associated(volgrid)) call delete(volgrid)

call l4f_category_log(category,L4F_INFO,"terminato")

call delete (v7d_import)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo7
