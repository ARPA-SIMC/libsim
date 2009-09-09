PROGRAM v7ddballe_2bufr
! Programma di esempio di estrazione e scrittura dall'archivio DB-all.e
USE datetime_class
USE vol7d_dballe_class
USE vol7d_class
USE log4fortran

IMPLICIT NONE

character(len=19) :: database,user,password
namelist  /odbc/database,user,password

TYPE(vol7d) :: v7d
TYPE(vol7d_dballe) ::v7d_exp

integer :: category,ier
character(len=512):: a_name

!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name)

!init di log4fortran
ier=l4f_init()
call l4f_category_log(category,L4F_INFO,"inizio")


                                ! lettura della namelist utilizzata per definire il DSN
open(10,file='odbc.nml',status='old')
read(10,nml=odbc,iostat=ier)

if (ier /= 0 )then
  call l4f_category_log(category,L4F_ERROR,"Errore durante la lettura della namelist odbc")
  call exit (ier)
end if
close(10)



CALL init(v7d)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in export
CALL init(v7d_exp,dsn=database,user=user,password=password,write=.true.,wipe=.true.,repinfo="repinfoclimat.csv")

CALL import(v7d,filename="climaprec.v7d")

Print *,"Fine lettura dati"

call display(v7d)

v7d_exp%vol7d   =  v7d

Print *,"Scrivo i dati"

CALL export(v7d_exp)

call l4f_category_log(category,L4F_INFO,"fine export")

CALL delete (v7d_exp) 

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

END PROGRAM v7ddballe_2bufr
