program demo5

use log4fortran
use grib_api
use volgrid6d_class
use grid_class
use char_utilities
implicit none

integer :: category,ier,gaid_template
character(len=512):: a_name
type (volgrid6d),pointer  :: volgrid(:),volgrid_out(:)
type(transform_def) :: trans

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo5")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

call import (volgrid,filename="in.grb",categoryappend="volume letto")

! qui posso fare tutti i conti possibili

call init(trans, trans_type="zoom",sub_type="coord", &
 ilon=-10.d0,ilat=40.d0,flon=10.d0,flat=50.d0,&
 categoryappend="trasformation")

!call transform(trans,griddim, volgrid, volgrid_out,categoryappend="trasforma")
call transform(trans, volgrid6d_in=volgrid, volgrid6d_out=volgrid_out,clone=.true.,categoryappend="trasforma")

call display(volgrid_out(1)%griddim)

call l4f_category_log(category,L4F_INFO,"trasformato")

if (associated(volgrid)) call delete(volgrid)

!call grib_new_from_template (gaid_template,"regular_ll_sfc_grib1")

call l4f_category_log(category,L4F_INFO,"export a un nuovo file grib")

!call export (volgrid,filename="out.grb",gaid_template=gaid_template,categoryappend="exportazione")
call export (volgrid_out,filename="out.grb",categoryappend="exportazione")

call l4f_category_log(category,L4F_INFO,"terminato")

if (associated(volgrid_out)) call delete(volgrid_out)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo5
