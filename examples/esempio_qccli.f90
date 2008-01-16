                                ! Programma esempio di controllo dei dati per soglie

program esempio_qccli

use modqccli
use vol7d_dballe_class

implicit none

integer :: io,ier
character(len=19) :: database,user,password

                                !tipi derivati.
TYPE(geo_coord)    :: coordmin, coordmax 
TYPE(datetime)     :: ti, tf
type(qcclitype)    :: v7dqccli
type(vol7d_dballe) :: v7ddballe

                                ! namelist utilizzata per definire il DSN
namelist  /odbc/database,user,password

                                ! lettura della namelist utilizzata per definire il DSN
open(10,file='odbc.nml',status='old')
read(10,nml=odbc,iostat=io)

if (io /= 0 )then
  print *,"Errore durante la lettura della namelist odbc"
  call exit (1)
end if
close(10)

                                ! Definisco le date iniziale e finale
CALL init(ti, year=2007, month=9, day=15, hour=00)
CALL init(tf, year=2007, month=9, day=16, hour=00)

                                ! Definisco il box delle coordinate
CALL init(coordmin,lat=43.70_fp_geo,lon=9.16_fp_geo)
CALL init(coordmax,lat=45.2_fp_geo,lon=12.84_fp_geo)

                                ! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7ddballe,dsn=database,user=user,password=password,write=.true.,wipe=.false.)

print*,"inizio importazione dati"
CALL import(v7ddballe,var=(/"B13011"/),varkind=(/"r"/),&
 anavar=(/"B07001"/),anavarkind=(/"i"/),&
 attr=(/"*B33196","*B33192"/),attrkind=(/"b","b"/)&
 ,timei=ti,timef=tf,coordmin=coordmin,coordmax=coordmax)
print*,"finita importazione dati"

print*,"inizio qc"

                                ! chiamiamo il "costruttore" per il Q.C.
call init(v7dqccli,v7ddballe%vol7d,ier,v7ddballe%data_id )
if (ier /= 0 ) then
  print * , "errore qccliinit ier=",ier
  call exit(1)
end if

call alloc(v7dqccli,ier)
if (ier /= 0 ) then
  print * , "errore qcclialloc ier=",ier
  call exit(1)
end if

  
print*,"Controllo climatico"
call quaconcli(v7dqccli,ier)
if( ier /= 0) then
  print *, "errore qccli= ",ier
  call exit(1)
end if

print*,"inizio esportazione dati"
CALL export(v7ddballe,attr_only=.true.)
print*,"finito esportazione dati"


                                ! il "distruttore" del Q.C.
call delete(v7dqccli,ier)
call delete(v7ddballe)
stop
  
end program esempio_qccli
  
  
