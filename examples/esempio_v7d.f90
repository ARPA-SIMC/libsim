PROGRAM esempio_vol7d
USE vol7d_class

TYPE(vol7d) :: v7d, v7d2
REAL, POINTER :: vol1d(:)
INTEGER :: i, n = 5, n2 = 2

! prepara la strada a v7d
CALL init (v7d)
CALL init (v7d2)

! alloca i descrittori del volume chiamabile piu' volte se con parametri diversi
CALL vol7d_alloc(v7d, ndativarr=1,ndatiattrb=2, ndativarattrb=1, ntime=n)
CALL vol7d_alloc(v7d2, ndativarr=1, ntime=n2)

! alloca il volume dati v7d
CALL vol7d_alloc_vol(v7d)
CALL vol7d_alloc_vol(v7d2)

! ho detto che c'è una variabile reale (ndativarr=1) e la definisco
v7d%dativar%r(1)%btable="B22070"
v7d2%dativar%r(1)%btable="B22070"

! anagrafica
CALL init(v7d%ana(1), lon=11.5_geoprec, lat=44.5_geoprec)
CALL init(v7d2%ana(1), lon=11.5_geoprec, lat=44.5_geoprec)

! livello
CALL init(v7d%level(1), level=105, l1=10)
CALL init(v7d2%level(1), level=105, l1=10)

! scadenza
CALL init(v7d%timerange(1), timerange=4, p1=-3600, p2=0)
CALL init(v7d2%timerange(1), timerange=4, p1=-3600, p2=0)

! rete
CALL init(v7d%network(1), id=10)
CALL init(v7d2%network(1), id=10)


! ho detto che ci sono due attributi byte (ndatiattrb=2) e li definisco
v7d%datiattr%b(1)%btable="B33007"
v7d%datiattr%b(2)%btable="B33192"

! ho detto che ho 1 variabile che ha attributi di tipo byte: è la variabile B22070 
v7d%dativarattr%b(1)%btable="B22070"

DO i=1,n
  ! ci scrivo le date di riferimento
  CALL init(v7d%time(i), year=2007, month=3, day=i+5)
  ! ci scrivo il dato reale
  v7d%voldatir(1,i,1,1,1,1)=22.4+0.5*i
  ! ci scrivo gli attributi byte
  v7d%voldatiattrb(1,i,1,1,1,1,1)=60
  v7d%voldatiattrb(1,i,1,1,1,1,2)=40
END DO

DO i=1,n2
  ! ci scrivo le date di riferimento
  CALL init(v7d2%time(i), year=2007, month=3, day=(i-1)*10+1)
  ! ci scrivo il dato reale
  v7d2%voldatir(1,i,1,1,1,1)=26.4+0.5*i
END DO

! creo una vista su un vettore unidimensionale
! che scorre solo la dimensione del tempo (vol7d_time_d)
CALL vol7d_get_voldatir(v7d, (/vol7d_time_d/), vol1d)
PRINT *,vol1d

! fondo i due volumi v7d e v7d2 ottenendo in un solo volume v7d
CALL vol7d_merge(v7d, v7d2, sort=.TRUE.)

! ricreo la vista unidimensionale
CALL vol7d_get_voldatir(v7d, (/vol7d_time_d/), vol1d)
PRINT*,vol1d

CALL delete (v7d)

END PROGRAM esempio_vol7d
