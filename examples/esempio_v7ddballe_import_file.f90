PROGRAM v7ddballe_import_file
! Programma di esempio di lettura da file e scrittura su file formato BUFR
!legge i dati da file formattato

USE vol7d_class
USE vol7d_netcdf_class
USE vol7d_dballe_class
use log4fortran

IMPLICIT NONE

integer :: category,ier
character(len=512):: a_name,filename

TYPE(vol7d_dballe) :: v7d_file,v7d_dba

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="demo",a_name_append="main")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"demo lettura BUFR")
call l4f_category_log(category,L4F_INFO,"inizializzazioni")

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7d_file,file=.true.,categoryappend="importBUFR")

call l4f_category_log(category,L4F_INFO,"Leggo i dati")
CALL import(v7d_file,var="B12001")
!CALL import(v7d_file)

call l4f_category_log(category,L4F_INFO,"Fine lettura")

!CALL init(v7d_dba,dsn="test",user="test",write=.true.,wipe=.true.,categoryappend="exportdba")
filename="new.bufr"
CALL init(v7d_dba,file=.true.,write=.true.,wipe=.true.&
 ,filename=filename,categoryappend="exportBUFR")

v7d_dba%vol7d=v7d_file%vol7d

call l4f_category_log(category,L4F_INFO,"inizio export")

call export(v7d_dba, template="generic")

call l4f_category_log(category,L4F_INFO,"fine export")


!CALL delete (v7d_file) 
CALL delete (v7d_dba) 

call l4f_category_log(category,L4F_INFO,"finito")


!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program v7ddballe_import_file
