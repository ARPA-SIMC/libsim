PROGRAM v7ddballe_import_export
! Programma di esempio di estrazione e scrittura dall'archivio DB-all.e
USE datetime_class
USE vol7d_dballe_class
USE vol7d_class

IMPLICIT NONE

TYPE(vol7d_dballe) :: v7d,v7d_exp
TYPE(datetime) :: ti, tf
CHARACTER(len=12) :: c
INTEGER :: i, n
REAL, POINTER :: vol2d(:,:)

! Definisco le date iniziale e finale
CALL init(ti, year=2007, month=3, day=18, hour=12)
CALL init(tf, year=2007, month=3, day=21, hour=00)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto
CALL init(v7d)

! Importo i dati, variabile 'B13011' della btable (precipitazione),
CALL import(v7d, 'B13011', 255, ti, tf, timerange=vol7d_timerange(4,-1800,0), attr=(/"*B33192","*B33007"/))
Print *,"ho estratto i dati",shape(v7d%vol7d%voldatir)

! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto in scrittura
CALL init(v7d_exp,dsn="test1",user="test",write=.true.,wipe=.true.)

v7d_exp%vol7d=v7d%vol7d

CALL export(v7d_exp)

CALL delete (v7d) !che corrisponde anche a v7d_exp

END PROGRAM v7ddballe_import_export
