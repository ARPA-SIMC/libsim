PROGRAM v7ddballe
! Programma di esempio di estrazione dall'archivio DB-all.e
USE datetime_class
USE vol7d_dballe_class
USE vol7d_class

IMPLICIT NONE

TYPE(vol7d_dballe) :: v7d
TYPE(vol7d_network) :: dummy_network
TYPE(datetime) :: ti, tf
CHARACTER(len=12) :: c
INTEGER :: i, n
REAL, POINTER :: vol2d(:,:)

! Definisco le date iniziale e finale
CALL init(ti, year=2007, month=3, day=19, hour=12)
CALL init(tf, year=2007, month=3, day=21, hour=00)
CALL init(dummy_network, 1)
! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto
CALL init(v7d,dsn="test",user="test",password="")
! Importo i dati, variabile 'B13011' della btable (precipitazione),
! rete 18 (FIDUPO), 20 (SIMNBO), 21 (SIMNPR)
CALL import(v7d, 'B13011', 18, ti, tf, set_network=dummy_network)
PRINT*,SHAPE(v7d%vol7d%voldatir)
CALL import(v7d, 'B13011', (/20,21/), ti, tf, set_network=dummy_network)
PRINT*,SHAPE(v7d%vol7d%voldatir)
! Creo una vista su un array bidimensionale che scorre le dimensioni
! dell'anagrafica e del tempo (vol7d_ana_d, vol7d_time_d)
CALL vol7d_get_voldatir(v7d%vol7d, (/vol7d_ana_d,vol7d_time_d/), vol2dp=vol2d)
! Calcolo la media e la stampo assieme all'istante
DO i = 1, SIZE(v7d%vol7d%time)
  CALL getval(v7d%vol7d%time(i), oraclesimdate=c)
  n = COUNT (vol2d(:,i) /= rmiss)
  IF (n > 0) THEN
    PRINT*, c, ' prec. media:', SUM(vol2d(:,i), mask=(vol2d(:,i) /= rmiss))/n
  ENDIF
ENDDO


END PROGRAM v7ddballe
