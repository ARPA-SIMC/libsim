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
program demo8

use log4fortran
USE vol7d_dballe_class
USE vol7d_class

implicit none

integer :: category,ier
character(len=512):: a_name,filename="synop_t.bufr"
TYPE(vol7d_dballe) :: v7d_dba
TYPE(vol7d) :: v7d_ana

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo8")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d_dba,file=.true.,write=.false.,filename=filename,&
 categoryappend="importBUFR",format="BUFR")

call import (v7d_dba,var=(/"B12101"/),varkind=(/"r"/))

call l4f_category_log(category,L4F_INFO,"importato vol7d")

call display(v7d_dba%vol7d)

call l4f_category_log(category,L4F_INFO,"export to ana file")

call init (v7d_ana)

call vol7d_copy (v7d_dba%vol7d,v7d_ana)

call delete (v7d_dba)

!pulisco i dati che a me sono inutili
call delete(v7d_ana,dataonly=.true.)
  call vol7d_alloc (v7d_ana, &
   ntime=0, ntimerange=0, nlevel=0, &
   ndativarr=0, ndativari=0, ndativarb=0, ndativard=0, ndativarc=0,&
   ndatiattrr=0, ndatiattri=0, ndatiattrb=0, ndatiattrd=0, ndatiattrc=0,&
   ndativarattrr=0, ndativarattri=0, ndativarattrb=0, ndativarattrd=0, ndativarattrc=0)
  
call display(v7d_ana)

CALL export (v7d_ana,filename="ana.v7d",description="Solo anagrafica")

call delete (v7d_ana)

call l4f_category_log(category,L4F_INFO,"terminato")


!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program demo8
