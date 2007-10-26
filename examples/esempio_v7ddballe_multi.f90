PROGRAM v7ddballe
! Programma di esempio di estrazione dall'archivio DB-all.e
USE datetime_class
USE vol7d_class
USE vol7d_class_compute
USE vol7d_dballe_class

IMPLICIT NONE

TYPE(vol7d_dballe) :: v7d
TYPE(vol7d_network) :: dummy_network,network(8)
TYPE(datetime) :: ti, tf, tc
TYPE(vol7d) :: vol_cumh, vol_cumd
TYPE(timedelta) :: dt_cum
CHARACTER(len=12) :: c
INTEGER :: i,j, n
REAL, POINTER :: vol2d(:,:), vol2d_cum(:,:)
INTEGER :: un
CHARACTER(len=512) :: filesim
TYPE(geo_coordvect),POINTER :: macroa(:)
INTEGER, ALLOCATABLE :: in_macroa(:)

! Definisco le date iniziale e finale
CALL init(ti, year=2007, month=3, day=18, hour=00)
CALL init(tf, year=2007, month=3, day=21, hour=00)
CALL init(dummy_network, 255)
CALL init(network(1), 11)
CALL init(network(2), 13)
CALL init(network(3), 15)
CALL init(network(4), 18)
CALL init(network(5), 19)
CALL init(network(6), 20)
CALL init(network(7), 21)
CALL init(network(8), 22)
!CALL init(network(9), 12)
!CALL init(network(10), 17)


! Chiamo il costruttore della classe vol7d_dballe per il mio oggetto
CALL init(v7d,dsn="test",user="test",password="")
! Importo i dati, variabile 'B13011' della btable (precipitazione),
! rete 18 (FIDUPO), 20 (SIMNBO), 21 (SIMNPR)
CALL import(v7d, 'B13011', network, timei=ti, timef=tf, set_network=dummy_network,varkind=(/"r"/))

PRINT*,SHAPE(v7d%vol7d%voldatir)

call export(v7d%vol7d)

! Cumulo i dati su intervalli orari
CALL init(dt_cum, hour=1)
CALL vol7d_cumulate(v7d%vol7d, vol_cumh, dt_cum)
! Mi faccio dare una "vista" bidimensionale dei miei dati
CALL vol7d_get_voldatir(vol_cumh, (/vol7d_ana_d,vol7d_time_d/), vol2dp=vol2d_cum)
! Stampo la media su tutte le stazioni ora per ora
DO i = 1, SIZE(vol_cumh%time)
  CALL getval(vol_cumh%time(i), oraclesimdate=c)
  n = COUNT (vol2d_cum(:,i) /= rmiss)
  IF (n > 0) THEN
    PRINT'(2A,G12.5)',c,' prec. media (mm): ', &
     SUM(vol2d_cum(:,i), mask=(vol2d_cum(:,i) /= rmiss))/n
  ENDIF
ENDDO

! Cumulo i dati su intervalli giornalieri
CALL init(dt_cum, day=1)
CALL init(tc, year=2007, month=3, day=17, hour=00)
CALL vol7d_cumulate(vol_cumh, vol_cumd, dt_cum, tc)
! Mi faccio dare una "vista" bidimensionale dei miei dati
CALL vol7d_get_voldatir(vol_cumd, (/vol7d_ana_d,vol7d_time_d/), vol2dp=vol2d_cum)

! Importo un file con le macroaree Emilia Romagna
un = open_package_file('polipciv4.dat', filetype_data)
IF (un < 0) STOP 1
INQUIRE(unit=un, name=filesim)
CLOSE(un)
CALL import(macroa, shpfilesim=filesim)
ALLOCATE(in_macroa(SIZE(vol_cumd%ana)))
in_macroa = 0
! Determino l'appartenenza delle stazioni alle macroaree
DO i = 1, SIZE(vol_cumd%ana)
  DO j = 1, SIZE(macroa)
    IF (geo_coord_inside(vol_cumd%ana(i)%coord, macroa(j))) THEN
      in_macroa(i) = j
      CYCLE
    ENDIF
  ENDDO
ENDDO

! Stampo la media su tutte le stazioni di ogni macroarea giorno per giorno
DO i = 1, SIZE(vol_cumd%time)
  CALL getval(vol_cumd%time(i), oraclesimdate=c)
  DO j = 1, SIZE(macroa)
    n = COUNT(vol2d_cum(:,i) /= rmiss .AND. in_macroa(:) == j)
    IF (n > 0) THEN
      PRINT'(2A,I3,A,G12.5)',c,' macroarea: ',j,' prec media (mm): ', &
       SUM(vol2d_cum(:,i), mask=(vol2d_cum(:,i) /= rmiss &
       .AND. in_macroa(:) == j))/n
    ENDIF
  ENDDO
ENDDO

END PROGRAM v7ddballe
