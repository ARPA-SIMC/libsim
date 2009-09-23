PROGRAM esempio_vol7d
USE datetime_class ! strano ma purtroppo ci vuole !
USE vol7d_class
IMPLICIT NONE

TYPE(vol7d) :: v7d1, v7d2, v7d3, v7dc, v7dl, v7dr
TYPE(datetime) :: enddate
DOUBLE PRECISION, POINTER :: vol1dd(:)
REAL, POINTER :: vol1dr(:), vol2dr(:,:)
INTEGER, POINTER :: vol1di(:)
INTEGER :: i, n1 = 5, n2 = 2

! prepara la strada a v7d
CALL init (v7d1)
CALL init (v7d2)
CALL init (v7d3)

! alloca i descrittori del volume, chiamabile piu' volte se con parametri diversi
CALL vol7d_alloc(v7d1, ndativard=1, ndatiattrb=2, ndativarattrb=1, ntime=n1)
CALL vol7d_alloc(v7d2, ndativard=1, ndativari=1, ntime=n2)
CALL vol7d_alloc(v7d3, nanavari=1)

! alloca il volume dati v7d
CALL vol7d_alloc_vol(v7d1)
CALL vol7d_alloc_vol(v7d2)
CALL vol7d_alloc_vol(v7d3)

! ho detto che c'è una variabile dati reale (ndativarr=1) e la definisco
CALL init(v7d1%dativar%d(1), btable='B12001')
CALL init(v7d2%dativar%d(1), btable='B12001')
CALL init(v7d2%dativar%i(1), btable='B12003', scalefactor=2)
! ho detto che c'è una variabile ana intera (nanavari=1) e la definisco
CALL init(v7d3%anavar%i(1), btable='B10000')

! anagrafica
CALL init(v7d1%ana(1), lon=11.5_fp_geo, lat=44.5_fp_geo)
CALL init(v7d2%ana(1), lon=11.5_fp_geo, lat=44.5_fp_geo)
CALL init(v7d3%ana(1), lon=11.5_fp_geo, lat=44.5_fp_geo)

! livello
CALL init(v7d1%level(1), level1=105, l1=10)
CALL init(v7d2%level(1), level1=105, l1=10)

! scadenza
CALL init(v7d1%timerange(1), timerange=4, p1=-3600, p2=0)
CALL init(v7d2%timerange(1), timerange=4, p1=-3600, p2=0)

! rete
CALL init(v7d1%network(1), name='generic')
CALL init(v7d2%network(1), name='generic')
CALL init(v7d3%network(1), name='generic')

! ho detto che ci sono due attributi dati byte (ndatiattrb=2) e li definisco
CALL init(v7d1%datiattr%b(1), btable='B33007')
CALL init(v7d1%datiattr%b(2), btable='B33192')

! ho detto che ho 1 variabile che ha attributi di tipo byte: è la variabile B22070 
CALL init(v7d1%dativarattr%b(1), btable='B22070')

DO i=1,n1
  ! ci scrivo le date di riferimento
  v7d1%time(i) = datetime_new(year=2007, month=3, day=i+5)
  ! ci scrivo il dato reale
  v7d1%voldatid(1,i,1,1,1,1)=295.4+0.5*i
  ! ci scrivo gli attributi byte
  v7d1%voldatiattrb(1,i,1,1,1,1,1)=60
  v7d1%voldatiattrb(1,i,1,1,1,1,2)=40
END DO

DO i=1,n2
  ! ci scrivo le date di riferimento
  v7d2%time(i) = datetime_new(year=2007, month=3, day=(i-1)*10+1)
  ! ci scrivo il dato reale
  v7d2%voldatid(1,i,1,1,1,1)=306.4+0.50*i
  v7d2%voldatii(1,i,1,1,1,1)=30640+50*i-300
END DO

v7d3%volanai(1,1,1) = 33

call display(v7d1)
call display(v7d2)
call display(v7d3)

! creo una vista su un vettore unidimensionale
! che scorre solo la dimensione del tempo (vol7d_time_d)
CALL vol7d_get_voldatid(v7d1, (/vol7d_time_d/), vol1dd)
PRINT*,'Volume 1, dati double:'
PRINT *,vol1dd
CALL vol7d_get_voldatid(v7d2, (/vol7d_time_d/), vol1dd)
PRINT*,'Volume 2, dati double:'
PRINT *,vol1dd
CALL vol7d_get_voldatii(v7d2, (/vol7d_time_d/), vol1di)
PRINT*,'Volume 2, dati integer:'
PRINT *,vol1di

! fondo i due volumi v7d1 e v7d2 ottenendo in un solo volume v7d1
CALL vol7d_merge(v7d1, v7d2, sort=.TRUE.)
! gli aggiungo anche l'altro volume con soli dati di anagrafica
CALL vol7d_merge(v7d1, v7d3, sort=.TRUE.)

CALL vol7d_get_voldatid(v7d1, (/vol7d_time_d/), vol1dd)
PRINT*,'Volume merge, dati double:'
PRINT *,vol1dd
CALL vol7d_get_voldatii(v7d1, (/vol7d_time_d/), vol1di)
PRINT*,'Volume merge, dati integer:'
PRINT *,vol1di
PRINT*,'Volume merge, attributi:'
PRINT*,v7d1%volanai

! converto il volume in real e distruggo l'originale
CALL vol7d_convr(v7d1, v7dr)
CALL delete(v7d1)

! ricreo la vista unidimensionale
CALL vol7d_get_voldatir(v7dr, (/vol7d_time_d,vol7d_var_d/), vol2dp=vol2dr)
PRINT*,'Volume conv, dati real:'
PRINT*,vol2dr
PRINT*,'Volume conv, attributi:'
PRINT*,v7dr%volanai

! copio il volume in un altro e distruggo l'originale
CALL vol7d_copy(v7dr, v7dc)
CALL delete (v7dr)

CALL vol7d_get_voldatir(v7dc, (/vol7d_time_d,vol7d_var_d/), vol2dp=vol2dr)
PRINT*,'Volume copy, dati real:'
PRINT*,vol2dr
PRINT*,'Volume copy, attributi:'
PRINT*,v7dc%volanai

! riformo il volume togliendo il penultimo livello temporale
v7dc%time(SIZE(v7dc%time)-1) = datetime_miss
CALL vol7d_reform(v7dc, miss=.TRUE.)

CALL vol7d_get_voldatir(v7dc, (/vol7d_time_d,vol7d_var_d/), vol2dp=vol2dr)
PRINT*,'Volume reform, (tolto il penultimo livello temporale) dati:'
PRINT*,vol2dr
PRINT*,'Volume reform, (tolto il penultimo livello temporale) attributi:'
PRINT*,v7dc%volanai

! copio il volume in un altro tenendo solo le date prima di enddate
!CALL init(enddate, year=2007, month=3, day=8)
CALL vol7d_copy(v7dc, v7dl, ltime=(v7dc%time < datetime_new(year=2007, month=3, day=8)))
CALL vol7d_get_voldatir(v7dl, (/vol7d_time_d,vol7d_var_d/), vol2dp=vol2dr)
PRINT*,'Volume copy (tolte le ultime date), dati:'
PRINT*,vol2dr
PRINT*,'Volume copy (tolte le ultime date), attributi:'
PRINT*,v7dl%volanai


END PROGRAM esempio_vol7d
