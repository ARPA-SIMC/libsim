PROGRAM esempio_vol7d
USE vol7d_class

TYPE(vol7d) :: v7d1, v7d2, v7d3, v7dc
REAL, POINTER :: vol1d(:)
INTEGER :: i, n1 = 5, n2 = 2

! prepara la strada a v7d
CALL init (v7d1)
CALL init (v7d2)
CALL init (v7d3)

! alloca i descrittori del volume, chiamabile piu' volte se con parametri diversi
CALL vol7d_alloc(v7d1, ndativarr=1,ndatiattrb=2, ndativarattrb=1, ntime=n1)
CALL vol7d_alloc(v7d2, ndativarr=1, ntime=n2)
CALL vol7d_alloc(v7d3, nanavari=1)

! alloca il volume dati v7d
CALL vol7d_alloc_vol(v7d1)
CALL vol7d_alloc_vol(v7d2)
CALL vol7d_alloc_vol(v7d3)

! ho detto che c'è una variabile dati reale (ndativarr=1) e la definisco
v7d1%dativar%r(1)%btable="B22070"
v7d2%dativar%r(1)%btable="B22070"
! ho detto che c'è una variabile ana intera (nanavari=1) e la definisco
v7d3%anavar%i(1)%btable="B10000"

! anagrafica
CALL init(v7d1%ana(1), lon=11.5_fp_geo, lat=44.5_fp_geo)
CALL init(v7d2%ana(1), lon=11.5_fp_geo, lat=44.5_fp_geo)
CALL init(v7d3%ana(1), lon=11.5_fp_geo, lat=44.5_fp_geo)

! livello
CALL init(v7d1%level(1), level=105, l1=10)
CALL init(v7d2%level(1), level=105, l1=10)

! scadenza
CALL init(v7d1%timerange(1), timerange=4, p1=-3600, p2=0)
CALL init(v7d2%timerange(1), timerange=4, p1=-3600, p2=0)

! rete
CALL init(v7d1%network(1), id=10)
CALL init(v7d2%network(1), id=10)
CALL init(v7d3%network(1), id=10)

! ho detto che ci sono due attributi dati byte (ndatiattrb=2) e li definisco
v7d1%datiattr%b(1)%btable="B33007"
v7d1%datiattr%b(2)%btable="B33192"

! ho detto che ho 1 variabile che ha attributi di tipo byte: è la variabile B22070 
v7d1%dativarattr%b(1)%btable="B22070"

DO i=1,n1
  ! ci scrivo le date di riferimento
  CALL init(v7d1%time(i), year=2007, month=3, day=i+5)
  ! ci scrivo il dato reale
  v7d1%voldatir(1,i,1,1,1,1)=22.4+0.5*i
  ! ci scrivo gli attributi byte
  v7d1%voldatiattrb(1,i,1,1,1,1,1)=60
  v7d1%voldatiattrb(1,i,1,1,1,1,2)=40
END DO

DO i=1,n2
  ! ci scrivo le date di riferimento
  CALL init(v7d2%time(i), year=2007, month=3, day=(i-1)*10+1)
  ! ci scrivo il dato reale
  v7d2%voldatir(1,i,1,1,1,1)=26.4+0.5*i
END DO

v7d3%volanai(1,1,1) = 33

! creo una vista su un vettore unidimensionale
! che scorre solo la dimensione del tempo (vol7d_time_d)
CALL vol7d_get_voldatir(v7d1, (/vol7d_time_d/), vol1d)
PRINT*,'Volume 1, dati:'
PRINT *,vol1d
CALL vol7d_get_voldatir(v7d2, (/vol7d_time_d/), vol1d)
PRINT*,'Volume 2, dati:'
PRINT *,vol1d

! fondo i due volumi v7d e v7d2 ottenendo in un solo volume v7d
CALL vol7d_merge(v7d1, v7d2, sort=.TRUE.)
! gli aggiungo anche l'altro volume con soli dati di anagrafica
CALL vol7d_merge(v7d1, v7d3, sort=.TRUE.)

! ricreo la vista unidimensionale
CALL vol7d_get_voldatir(v7d1, (/vol7d_time_d/), vol1d)
PRINT*,'Volume 1+2+3, dati:'
PRINT*,vol1d
PRINT*,'Volume 1+2+3, attributi:'
PRINT*,v7d1%volanai

CALL vol7d_copy(v7d1, v7dc)
CALL delete (v7d1)

CALL vol7d_get_voldatir(v7dc, (/vol7d_time_d/), vol1d)
PRINT*,'Volume copia, dati:'
PRINT*,vol1d
PRINT*,'Volume copia, attributi:'
PRINT*,v7dc%volanai


END PROGRAM esempio_vol7d
