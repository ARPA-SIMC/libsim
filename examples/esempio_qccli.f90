                                ! Programma esempio di controllo dei dati per soglie

program esempio_qccli

use modqccli

implicit none

character(len=19) :: database,user,password
TYPE(datetime) :: ti, tf

                                !tipi derivati.
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
CALL init(ti, year=2007, month=3, day=18, hour=12)
CALL init(tf, year=2007, month=3, day=21, hour=00)

                                ! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in import
CALL init(v7ddballe,dsn=database,user=user,password=password,write=.true.,wipe=.false.)

CALL import(v7ddballe,var=(/"B12001"/),varkind=(/"r"/),attr=(/"*B33195","*B33192"/),attrkind=(/"i","b"/))


                                ! chiamiamo il "costruttore" per il Q.C.
call init(v7dqccli,v7ddballe%v7d,ier)
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


!CALL export(v7ddballe,attr_only=.true.)
CALL export(v7ddballe)
  

                                ! il "distruttore" del Q.C.
call delete(v7dqccli,ier)
call delete(v7ddballe)
stop
  
end program esempio_qccli
  
  
