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
PROGRAM v7ddballe_dba2v7d
! Programma di esempio di estrazione e scrittura dall'archivio DB-all.e
USE datetime_class
USE log4fortran
USE vol7d_class
USE vol7d_dballenew_class

IMPLICIT NONE

TYPE(vol7d_dballe) :: v7d_dba
integer :: category,ier
character(len=512):: a_name,filename="../data/example_temp.bufr"

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name)
!init di log4fortran
ier=l4f_init()
call l4f_category_log(category,L4F_INFO,"inizio")

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_dba,filename=filename,file=.true.,categoryappend="dballenewapi")

!CALL import(v7d_dba,var=["B12101"],attr=["*B33192"],anavar=["B12101"],anaattr=["*B33192"])
CALL import(v7d_dba)

Print *,"Fine lettura dati"

call display(v7d_dba%vol7d)

call l4f_category_log(category,L4F_INFO,"fine import")

CALL delete (v7d_dba) 

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM v7ddballe_dba2v7d
