program demo6

use log4fortran
use volgrid6d_class
use grid_class
USE vol7d_dballe_class
USE vol7d_class

implicit none

integer :: category,ier,i,nana
character(len=512):: a_name,filename="out.bufr"
type (volgrid6d),pointer  :: volgrid(:),volgrid_out(:)
type(transform_def) :: trans
type(vol7d_ana),allocatable :: ana(:)
type(vol7d),pointer :: vol7d_out(:)
TYPE(vol7d_dballe) :: v7d_exp


!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo5")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

!!$ nana=5
!!$ allocate(ana(nana))
!!$ !target points 
!!$ call init(ana(1),lat=45.D0,lon=11.D0)
!!$ call init(ana(2),lat=45.6D0,lon=11.8D0)
!!$ call init(ana(3),lat=46.6D0,lon=12.8D0)
!!$ call init(ana(4),lat=40.6D0,lon=11.8D0)
!!$ call init(ana(5),lat=40.0D0,lon=10.0D0)

open (unit=1,file="ana.txt",status="old",form="unformatted")
read(1)nana
allocate(ana(nana))
call read_unit(ana,unit=1)
close(unit=1)


!trasformation object
call init(trans, trans_type="inter",sub_type="bilin", categoryappend="trasformation")

call import (volgrid,filename="in.grb",categoryappend="volume letto")

call transform(trans,ana, volgrid6d_in=volgrid, vol7d_out=vol7d_out,categoryappend="trasforma")

call l4f_category_log(category,L4F_INFO,"trasformato")

if (associated(volgrid)) call delete(volgrid)

call l4f_category_log(category,L4F_INFO,"export to BUFR")


! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_exp,file=.true.,write=.true.,wipe=.true.,filename=filename,&
categoryappend="exportBUFR",format="BUFR")

do i = 1 , size(vol7d_out)
  call display(vol7d_out(i))
  call vol7d_copy (vol7d_out(i), v7d_exp%vol7d) 
  call export (v7d_exp,template="synop")
end do

call l4f_category_log(category,L4F_INFO,"terminato")

call delete (v7d_exp)
if (associated(vol7d_out)) call delete(vol7d_out)
deallocate (vol7d_out)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo6
