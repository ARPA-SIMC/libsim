! Copyright (C) 2016  ARPA-SIM <urpsim@smr.arpa.emr.it>
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
PROGRAM v7ddballe_test
! Programma di esempio di estrazione e scrittura dall'archivio DB-all.e
use dballe_class
USE log4fortran
USE vol7d_class
USE vol7d_dballe_class

IMPLICIT NONE

TYPE(vol7d_dballe) :: v7d_dba
TYPE(vol7d) :: v7d

INTEGER :: ier
TYPE(l4f_handle) :: category
CHARACTER(len=512):: a_name,filename="vol7d_dballe_test.bufr",filenameout="vol7d_dballe_test_out.bufr"

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name)
!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get_handle(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")

print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
print*,"!!                           import/export from file"
print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_dba,filename=filename,file=.true.,categoryappend="v7ddballe_read")

call l4f_category_log(category,L4F_INFO,"End data init")

call l4f_category_log(category,L4F_INFO,"start import")

CALL import(v7d_dba&
 ,var=["B10004","B12101","B12103"]&
! ,var=["B10004","B12101"]&
! ,var=["B10004","B12103"]&
! ,var=["B12101","B12103"]&
                                !                   ,varkind=["r","i","c"]&
                                !                   ,attr=["*B33196","*B33192","*B33193"],attrkind=["b","c","b"]&
                                !                   ,anavar=["B12101"],anaattr=["*B33192"]&
 )

call l4f_category_log(category,L4F_INFO,"end import")

call vol7d_copy(v7d_dba%vol7d,v7d)
CALL delete (v7d_dba) 

call display(v7d)


CALL init(v7d_dba,filename=filenameout,file=.true.,write=.true.,wipe=.true.,categoryappend="v7ddballe_write")

call vol7d_copy(v7d,v7d_dba%vol7d)

call l4f_category_log(category,L4F_INFO,"start export")
CALL export(v7d_dba)
call l4f_category_log(category,L4F_INFO,"end export")

CALL delete (v7d_dba) 
CALL delete (v7d) 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM v7ddballe_test
