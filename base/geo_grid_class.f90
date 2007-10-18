!> \brief Classe per la gestione di campi su griglia rettangolare.
!!
!! Questo modulo definisce una classe per la
!! gestione di campi, eventualmente georeferenziati, definiti su una
!! griglia rettangolare con coordinate geografiche (anche ruotate) o UTM.
!! Gestisce 5 dimensioni (x, y, z, variabile, tempo) e fornisce sia una
!! vista completa di tutte le 5 dimensioni che una vista bidimensionale
!! (x,y) di una sola sezione orizzontale alla volta.
!! La classe ha le componenti di tipo \a PRIVATE, per
!! cui non possono essere manipolate direttamente ma solo tramite i
!! relativi metodi.
!! \ingroup base
MODULE geo_grid_class
USE kinds
USE missing_values
USE char_utilities
USE err_handling
USE geo_coord_class

IMPLICIT NONE

!> Kind reale usato per i dati dei campi grigliati.
!! Questa costante deve essere utilizzata nelle dichiarazioni degli
!! array per il passaggio dei campi della griglia, tipicamente
!! \a field2d e \a field5d, e per assegnare loro dei valori,
!! ad esempio con:
!! \code
!! REAL(kind=fp_gg), POINTER :: z(:,:)
!! z = REAL(x, kind=fp_gg)
!! z = gg_miss
!! \endcode
INTEGER, PARAMETER :: fp_gg = fp_s

!> Valore mancante per i dati dei campi grigliati
REAL(kind=fp_gg), PARAMETER :: gg_miss = rsmiss

!> Indica che la griglia ha un sistema di coordinate rettangolare generico, queste costanti vanno utilizzate nei metodi ::setval e ::getval per specificare o accertare la proiezione su cui sono definiti i dati.
INTEGER, PARAMETER :: gg_proj_gen=-1
INTEGER, PARAMETER :: gg_proj_geo=0 !< indica che la griglia ha un sistema di coordinate georeferenziato geografico
INTEGER, PARAMETER :: gg_proj_utm=1 !< indica che la griglia ha un sistema di coordinate georeferenziato UTM
INTEGER, PARAMETER :: gg_proj_georot=2 !< indica che la griglia ha un sistema di coordinate georeferenziato geografico ruotato

!> Definisce una griglia 5d eventualmente georeferenziata
TYPE geo_grid
  PRIVATE
  INTEGER :: proj, nx, ny
  INTEGER :: fuso, elliss
  REAL :: x1, x2, y1, y2, dx, dy, &
   xrot, yrot, rot
  INTEGER :: nlev, nvar, ntim, curlev, curvar, curtim
  REAL, POINTER :: vcp(:)
  REAL(kind=fp_gg),POINTER :: field2d(:,:), field5d(:,:,:,:,:)
  REAL,POINTER :: coord(:,:,:)
END TYPE geo_grid

!> Costruttore per la classe geo_grid.
INTERFACE init
  MODULE PROCEDURE gg_init
END INTERFACE

!> Distruttore per la classe geo_grid.
INTERFACE delete
  MODULE PROCEDURE gg_delete
END INTERFACE

!> Metodo di allocazione della griglia per la classe geo_grid.
INTERFACE alloc
  MODULE PROCEDURE gg_alloc
END INTERFACE

!> Metodo di deallocazione della griglia per la classe geo_grid.
INTERFACE dealloc
  MODULE PROCEDURE gg_dealloc
END INTERFACE

!> Metodo per impostare una vista 2d di un oggetto della classe geo_grid.
INTERFACE set_2d_slice
  MODULE PROCEDURE gg_set_2d_slice
END INTERFACE

!> Restituisce il valore delle componenti dell'oggetto nella forma desiderata.
INTERFACE getval
  MODULE PROCEDURE gg_getval
END INTERFACE

!> Imposta il valore delle componenti dell'oggetto desiderate.
INTERFACE setval
  MODULE PROCEDURE gg_setval
END INTERFACE

PRIVATE gg_adjust_coord, rtlld

CONTAINS

!> Inizializza un oggetto geo_grid, non riceve alcun
!! parametro tranne l'oggetto stesso.  Attenzione, è necessario chiamare
!! sempre il costruttore per evitare di avere dei puntatori in uno stato
!! indefinito.
SUBROUTINE gg_init(this)
TYPE(geo_grid) :: this

this%proj = gg_proj_gen
this%fuso = imiss
this%elliss = imiss
this%nx = 0
this%ny = 0
this%x1 = rmiss
this%x2 = rmiss
this%y1 = rmiss
this%y2 = rmiss
this%dx = 1.0
this%dy = 1.0
this%nlev = 1
this%nvar = 1
this%ntim = 1
this%curlev = 1
this%curvar = 1
this%curtim = 1
NULLIFY(this%vcp, this%field2d, this%field5d, this%coord)

END SUBROUTINE gg_init


!> Distrugge l'oggetto in maniera pulita, liberando l'eventuale spazio
!! dinamicamente allocato.
SUBROUTINE gg_delete(this)
TYPE(geo_grid),INTENT(INOUT) :: this

CALL gg_dealloc(this)
CALL gg_init(this)

END SUBROUTINE gg_delete


!> Alloca dinamicamente lo spazio per il contenuto della griglia
!! 2/5-dimensionale. Può essere chiamata solo dopo che siano già stati
!! impostati i valori di \a nx e \a ny tramite la
!! \a ::setval; se non specificate, le dimensioni aggiuntive z,
!! variabile e tempo sono poste a 1 e quindi la vista 2-d e la vista 5-d
!! coincidono.  Se la griglia è già stata allocata in precedenza e almeno
!! una delle dimensioni aggiuntive z, variabile, tempo, è stata
!! successivamente incrementata tramite la \a ::setval, la griglia
!! viene riallocata alla nuova dimensione conservando i dati precedenti
!! ove già esistevano.
RECURSIVE SUBROUTINE gg_alloc(this, ier)
TYPE (geo_grid) :: this !< oggetto geo_grid in cui allocare la griglia
INTEGER, INTENT(OUT) :: ier !< codice di errore in uscita, errore se \c /=0

INTEGER :: sh(5)
REAL(kind=fp_gg), POINTER :: tmpptr(:,:,:,:,:)

ier = 0
IF (this%nx <= 0 .OR. this%ny <= 0) THEN
  CALL raise_error('cannot allocate, nx and/or ny not set', 1, ier)
  RETURN
ENDIF
IF (ASSOCIATED(this%field5d)) THEN ! time/var/level extension
  tmpptr => this%field5d
  NULLIFY(this%field5d)
  sh = SHAPE(tmpptr)
  CALL gg_alloc(this, ier)
  IF (ier == 0) THEN
    this%field5d = gg_miss
    IF (sh(1) /= this%nx .OR. sh(2) /= this%ny .OR. sh(3) > this%nlev &
     .OR. sh(4) > this%nvar .OR. sh(5) > this%ntim) THEN
      CALL raise_warning('grid not extendible I discard the old data')
    ELSE
      this%field5d(:,:,1:sh(3),1:sh(4),1:sh(5)) = tmpptr(:,:,:,:,:)
    ENDIF
  ENDIF
  DEALLOCATE(tmpptr)
ELSE
  ALLOCATE(this%field5d(this%nx, this%ny, this%nlev, this%nvar, this%ntim), &
   STAT=ier)
  IF (ier /= 0) THEN
    CALL raise_error('cannot allocate '// &
     to_char(this%nx*this%ny*this%nlev*this%nvar*this%ntim)//' words')
    RETURN
  ENDIF
  CALL gg_adjust_coord(this)
ENDIF
CALL set_2d_slice(this) ! Assign field2d

END SUBROUTINE gg_alloc


!> Dealloca lo spazio eventualmente allocato in precedenza per la
!! griglia. A differenza del distruttore ::delete, non modifica la descrizione
!! della griglia, per cui dopo la chiamata lo spazio può essere
!! riallocato senza necessariamente ridefinire tutta la griglia.
SUBROUTINE gg_dealloc(this)
TYPE(geo_grid),INTENT(INOUT) :: this

IF (ASSOCIATED(this%field5d)) DEALLOCATE(this%field5d)
NULLIFY(this%field2d)
IF (ASSOCIATED(this%vcp)) DEALLOCATE(this%vcp)
IF (ASSOCIATED(this%coord)) DEALLOCATE(this%coord)

END SUBROUTINE gg_dealloc


!> Imposta il valore di uno o più componenti di un oggetto
!! geo_grid. Tutti i parametri tranne \a this sono opzionali.
!! I componenti di geo_grid possono essere cambiati solo nello stato
!! "deallocato", tranne \a nlev, \a nvar e \a ntim che possono
!! essere estesi nello stato "allocato", dopodiché è necessario
!! chiamare la ::alloc per riallocare il volume conservando i dati vecchi.
!! È comunque sempre possibile chiamare la
!! ::dealloc e modificare alcuni dei parametri prima di una
!! successiva allocazione.  Non è necessario impostare tutti i valori
!! \a x1, \a x2, \a y1, \a y2, \a dx, \a dy, al momento della chiamata
!! del metodo ::alloc l'eventuale valore mancante per x o y è ricalcolato.
SUBROUTINE gg_setval(this, nx, ny, nlev, nvar, ntim, proj, fuso, elliss, x1, x2, &
 y1, y2, dx, dy, xrot, yrot, rot)
TYPE (geo_grid) :: this !< oggetto di cui impostare i componenti
INTEGER, INTENT(IN), OPTIONAL :: nx !< numero di punti sull'asse x
INTEGER, INTENT(IN), OPTIONAL :: ny !< numero di punti sull'asse y
INTEGER, INTENT(IN), OPTIONAL :: nlev !< numero di punti sull'asse z
INTEGER, INTENT(IN), OPTIONAL :: nvar !< numero di variabili
INTEGER, INTENT(IN), OPTIONAL :: ntim !< numero di istanti temporali
INTEGER, INTENT(IN), OPTIONAL :: proj !< proiezione geografica, usare una delle costanti \a ::gg_proj_gen, \a ::gg_proj_geo, ecc.
INTEGER, INTENT(IN), OPTIONAL :: fuso !< fuso nel caso di sistema di coordinate UTM (<0 per l'Emisfero sud)
INTEGER, INTENT(IN), OPTIONAL :: elliss !< ellissoide (vedi geo_coord_class::nelliss e compagnia)
REAL, INTENT(IN), OPTIONAL :: x1 !< longitudine dell'estremo ovest della griglia
REAL, INTENT(IN), OPTIONAL :: x2 !< longitudine dell'estremo est della griglia
REAL, INTENT(IN), OPTIONAL :: y1 !< latitudine dell'estremo sud della griglia
REAL, INTENT(IN), OPTIONAL :: y2 !< latitudine dell'estremo sud della griglia
REAL, INTENT(IN), OPTIONAL :: dx !< passo di griglia lungo l'asse x
REAL, INTENT(IN), OPTIONAL :: dy !< passo di griglia lungo l'asse y
REAL, INTENT(IN), OPTIONAL :: xrot !< coordinata x dell'origine del sistema di coordinate ruotate (ha senso nel caso di proj=::gg_proj_georot)
REAL, INTENT(IN), OPTIONAL :: yrot !< coordinata y dell'origine del sistema di coordinate ruotate (ha senso nel caso di proj=::gg_proj_georot)
REAL, INTENT(IN), OPTIONAL :: rot !< angolo di rotazione (ha senso nel caso di proj=::gg_proj_rot)

IF (ASSOCIATED(this%field5d)) THEN ! Following members can only be extended
  IF (PRESENT(nlev)) THEN
    this%nlev = MAX(this%nlev, nlev)
  ENDIF
  IF (PRESENT(nvar)) THEN
    this%nvar = MAX(this%nvar, nvar)
  ENDIF
  IF (PRESENT(ntim)) THEN
    this%ntim = MAX(this%ntim, ntim)
  ENDIF
ELSE ! In deallocated state everything can be changed
  IF (PRESENT(nlev)) this%nlev = MAX(nlev, 1)
  IF (PRESENT(nvar)) this%nvar = MAX(nvar, 1)
  IF (PRESENT(ntim)) this%ntim = MAX(ntim, 1)
  IF (PRESENT(nx)) this%nx = MAX(nx, 0)
  IF (PRESENT(ny)) this%ny = MAX(ny, 0)
  IF (PRESENT(proj)) this%proj = proj
  IF (PRESENT(fuso)) this%fuso = fuso
  IF (PRESENT(elliss)) this%elliss = elliss
  IF (PRESENT(x1)) this%x1 = x1
  IF (PRESENT(x2)) this%x2 = x2
  IF (PRESENT(y1)) this%y1 = y1
  IF (PRESENT(y2)) this%y2 = y2
  IF (PRESENT(dx)) this%dx = dx
  IF (PRESENT(dy)) this%dy = dy
  IF (PRESENT(xrot)) this%xrot = xrot
  IF (PRESENT(yrot)) this%yrot = yrot
  IF (PRESENT(rot)) this%rot = rot
ENDIF

END SUBROUTINE gg_setval


!> Restituisce il valore di uno o più componenti di un oggetto
!! geo_grid o un puntatore alla griglia dei dati. I parametri
!! che restituiscono un puntatore possono restituire un puntatore
!! non associato se il corrispondente array non è stato ancora
!! allocato, per cui può essere opportuno inserire un controllo
!! con la funzione \c ASSOCIATED().
!! I puntatori vengono associati direttamente ai dati stessi e non
!! ad una loro copia, per cui non devono essere deallocati e ogni
!! modifica al loro contenuto influenza il contenuto dell'oggetto.
SUBROUTINE gg_getval(this, nx, ny, nlev, nvar, ntim, proj, fuso, elliss, x1, x2, &
 y1, y2, dx, dy, xrot, yrot, rot, field2d, field5d, coord)
TYPE (geo_grid) :: this !< oggetto di cui restituire il componente
INTEGER, INTENT(OUT), OPTIONAL :: nx !< numero di punti sull'asse x
INTEGER, INTENT(OUT), OPTIONAL :: ny !< numero di punti sull'asse y
INTEGER, INTENT(OUT), OPTIONAL :: nlev !< numero di punti sull'asse z
INTEGER, INTENT(OUT), OPTIONAL :: nvar !< numero di variabili
INTEGER, INTENT(OUT), OPTIONAL :: ntim !< numero di istanti temporali
INTEGER, INTENT(OUT), OPTIONAL :: proj !< proiezione geografica, restituisce il valore di una delle costanti \a ::gg_proj_gen, \a ::gg_proj_geo, ecc.
INTEGER, INTENT(OUT), OPTIONAL :: fuso !< fuso nel caso di sistema di coordinate UTM (<0 per l'Emisfero sud)
INTEGER, INTENT(OUT), OPTIONAL :: elliss !< ellissoide (vedi geo_coord_class::nelliss e compagnia)
REAL, INTENT(OUT), OPTIONAL :: x1 !< longitudine dell'estremo ovest della griglia
REAL, INTENT(OUT), OPTIONAL :: x2 !< longitudine dell'estremo est della griglia
REAL, INTENT(OUT), OPTIONAL :: y1 !< latitudine dell'estremo sud della griglia
REAL, INTENT(OUT), OPTIONAL :: y2 !< latitudine dell'estremo sud della griglia
REAL, INTENT(OUT), OPTIONAL :: dx !< passo di griglia lungo l'asse x
REAL, INTENT(OUT), OPTIONAL :: dy !< passo di griglia lungo l'asse y
REAL, INTENT(OUT), OPTIONAL :: xrot !< coordinata x dell'origine del sistema di coordinate ruotate (ha senso nel caso di proj=::gg_proj_georot)
REAL, INTENT(OUT), OPTIONAL :: yrot !< coordinata y dell'origine del sistema di coordinate ruotate (ha senso nel caso di proj=::gg_proj_georot)
REAL, INTENT(OUT), OPTIONAL :: rot !< angolo di rotazione (ha senso nel caso di proj=::gg_proj_rot)
!> restituisce un puntatore alla vista 2d dei dati impostata con ::set_2d_slice,
!! le dimensioni sono, rispettivamente, \a x e \a y
REAL(kind=fp_gg), POINTER, OPTIONAL :: field2d(:,:)
!> restituisce un puntatore al volume 5d completo dei dati, le dimensioni sono,
!! rispettivamente, \a x, \a y, \a z, variabile e tempo
REAL(kind=fp_gg), POINTER, OPTIONAL :: field5d(:,:,:,:,:)
!> restituisce un puntatore alle coordinate dei dati eventualmente calcolate
!! con il metodo ::gg_compute_coord; si tratta di un array 3d con le prime due
!! dimensioni \a x, \a y, e la terza dimensione di 2 elementi, rispettivamente
!! le componenti x e y della coordinata
REAL, POINTER, OPTIONAL :: coord(:,:,:)

IF (PRESENT(nlev)) nlev = this%nlev
IF (PRESENT(nvar)) nvar = this%nvar
IF (PRESENT(ntim)) ntim = this%ntim
IF (PRESENT(nx)) nx = this%nx
IF (PRESENT(ny)) ny = this%ny
IF (PRESENT(proj)) proj = this%proj
IF (PRESENT(fuso)) fuso = this%fuso
IF (PRESENT(elliss)) elliss = this%elliss
IF (PRESENT(x1)) x1 = this%x1
IF (PRESENT(x2)) x2 = this%x2
IF (PRESENT(y1)) y1 = this%y1
IF (PRESENT(y2)) y2 = this%y2
IF (PRESENT(dx)) dx = this%dx
IF (PRESENT(dy)) dy = this%dy
IF (PRESENT(xrot)) xrot = this%xrot
IF (PRESENT(yrot)) yrot = this%yrot
IF (PRESENT(rot)) rot = this%rot

IF (PRESENT(field2d)) field2d => this%field2d
IF (PRESENT(field5d)) field5d => this%field5d
IF (PRESENT(coord)) coord => this%coord

END SUBROUTINE gg_getval


!> Imposta una vista 2d su una sezione orizzontale della griglia di dati
!! per una variabile e ad un istante fissati. I valori passati vengono
!! troncati agli estremi validi per la griglia corrente.
SUBROUTINE gg_set_2d_slice(this, nlev, nvar, ntim)
TYPE (geo_grid) :: this !< oggetto di cui determinare la sezione
INTEGER, INTENT(IN), OPTIONAL :: nlev !< livello verticale della sezione
INTEGER, INTENT(IN), OPTIONAL :: nvar !< variabile della sezione
INTEGER, INTENT(IN), OPTIONAL :: ntim !< istante temporale della sezione

IF (PRESENT(nlev)) this%curlev = MAX(1,MIN(nlev,this%nlev))
IF (PRESENT(nvar)) this%curvar = MAX(1,MIN(nvar,this%nvar))
IF (PRESENT(ntim)) this%curtim = MAX(1,MIN(ntim,this%ntim))
IF (ASSOCIATED(this%field5d)) &
 this%field2d => this%field5d(:,:,this%curlev,this%curvar,this%curtim)

END SUBROUTINE gg_set_2d_slice


SUBROUTINE gg_adjust_coord(this)
TYPE (geo_grid) :: this

IF (this%proj == gg_proj_gen) THEN
  this%x1 = 1.0
  this%x2 = REAL(this%nx)
  this%dx = 1.0
  this%y1 = 1.0
  this%y2 = REAL(this%ny)
  this%dy = 1.0
ELSE
  IF (this%x1 == rmiss .AND. this%x2 == rmiss) THEN
    this%x1 = 1.0
    this%x2 = this%x1 + (this%nx - 1)*this%dx
  ELSE IF (this%x1 /= rmiss .AND. this%x2 /= rmiss .AND. this%nx > 1) THEN
    this%dx = (this%x2 - this%x1)/(this%nx - 1)
  ELSE IF (this%x2 == rmiss .AND. this%x1 /= rmiss) THEN
    this%x2 = this%x1 + (this%nx - 1)*this%dx
  ELSE IF (this%x1 == rmiss .AND. this%x2 /= rmiss) THEN
    this%x1 = this%x2 - (this%nx - 1)*this%dx
  ENDIF
  IF (this%y1 == rmiss .AND. this%y2 == rmiss) THEN
    this%y1 = 1.0
    this%y2 = this%y1 + (this%ny - 1)*this%dy
  ELSE IF (this%y1 /= rmiss .AND. this%y2 /= rmiss .AND. this%ny > 1) THEN
    this%dy = (this%y2 - this%y1)/(this%ny - 1)
  ELSE IF (this%y2 == rmiss .AND. this%y1 /= rmiss) THEN
    this%y2 = this%y1 + (this%ny - 1)*this%dy
  ELSE IF (this%y1 == rmiss .AND. this%y2 /= rmiss) THEN
    this%y1 = this%y2 - (this%ny - 1)*this%dy
  ENDIF
ENDIF

END SUBROUTINE gg_adjust_coord


!> Calcola le coordinate di tutti i punti di griglia secondo il sistema
!! specificato. Le coordinate possono essere ottenute tramite la ::getval
!! nel parametro coord.
SUBROUTINE gg_compute_coord(this)
TYPE (geo_grid) :: this !< oggetto di cui calcolare le coordinate

INTEGER :: i, j
REAL :: cosy, siny

IF (.NOT. ASSOCIATED(this%field5d)) CALL gg_adjust_coord(this)
IF (ASSOCIATED(this%coord)) DEALLOCATE(this%coord)
ALLOCATE(this%coord(this%nx,this%ny,2))
IF (this%proj == gg_proj_gen) THEN
  DO i = 1, this%nx
    this%coord(i,:,1) = REAL(i)
  ENDDO
  DO j = 1, this%ny
    this%coord(:,j,2) = REAL(j)
  ENDDO
ELSE
  DO i = 1, this%nx
    this%coord(i,:,1) = this%x1+(i-1)*this%dx ! make sure at this%nx = this%x2 ?
  ENDDO
  DO j = 1, this%ny
    this%coord(:,j,2) = this%y1+(j-1)*this%dy
  ENDDO
  IF (this%proj == gg_proj_georot) THEN
    cosy = COSD(this%yrot)
    siny = SIND(this%yrot)
    DO j = 1, this%ny
      DO i = 1, this%nx
        CALL rtlld(this%coord(i, j, 1), this%coord(i, j, 2), this%xrot, cosy, siny)
      ENDDO
    ENDDO
  ENDIF
ENDIF

END SUBROUTINE gg_compute_coord


SUBROUTINE rtlld(x, y, x0, cy0, sy0)
REAL, INTENT(inout) :: x, y
REAL, INTENT(in) :: x0, cy0, sy0

REAL :: sx, sy, cx, cy

sx = SIND(x)
cx = COSD(x)
sy = SIND(y)
cy = COSD(y)

y = ASIND(sy0*cy*cx+cy0*sy)
x = x0 + ASIND(sx*cy/COSD(y))

END SUBROUTINE rtlld


!> Determina se ciascuno dei punti di una griglia 2d giace dentro o fuori
!! da un insieme di poligoni sul piano \a xy.
!! L'insieme di poligoni è specificato come
!! un vettore di oggetti della classe geo_coord_class::geo_coordvect.
!! Il metodo ha due modalità di funzionamento:
!! - se è fornito \a spoly (array intero 2d), esso in uscita conterrà,
!!   per ogni punto di \a this, l'indice del poligono dentro cui si trova il
!!   punto, o 0 nel caso non si trovi dentro a nessun poligono; nel caso un
!!   punto si trovi dentro a più poligoni intersecantisi, l'indice è quello
!!   del primo poligono che soddisfa il criterio di contenimento
!! - se è fornito \a mpoly (array logico 3d), esso in uscita conterrà,
!!   per ogni punto (prime 2 dimensioni) e per ogni poligono (terza dimensione),
!!   \c .TRUE. o \c .FALSE. a seconda che il punto sia o meno contenuto
!!   nel poligono.
!!
!! La modalità di chiamata con il parametro \a mpoly
!! fornisce un'informazione più completa, ma il calcolo risulta molto più
!! lungo; nel caso si abbia a che fare con poligoni non intersecantisi,
!! come avviene nella maggior parte dei casi, la modalità con
!! \a spoly è consigliata.
!! \a spoly e \a mpoly sono puntatori che vengono associati nella ::gg_inside
!! ad array allocati dinamicamente, per cui essi dovranno essere deallocati
!! a cura del programma chiamante.
SUBROUTINE gg_inside(this, poly, spoly, mpoly)
TYPE(geo_grid) :: this !< griglia
TYPE(geo_coordvect), INTENT(IN) :: poly(:) !< vettore di poligoni
INTEGER, POINTER, OPTIONAL :: spoly(:,:) !< array con l'indice del poligono
LOGICAL, POINTER, OPTIONAL :: mpoly(:,:,:) !< array con le condizioni di contenimento per ogni poligono

INTEGER :: i, j, p
TYPE(geo_coord) :: point

IF (PRESENT(spoly)) NULLIFY(spoly)
IF (PRESENT(mpoly)) NULLIFY(mpoly)
CALL gg_compute_coord(this)
IF (.NOT. ASSOCIATED(this%coord)) RETURN
IF (PRESENT(spoly)) THEN
  ALLOCATE(spoly(this%nx, this%ny))
  spoly(:,:) = 0
  DO j = 1, this%ny
    DO i = 1, this%nx
      IF (this%proj == gg_proj_utm) THEN
        CALL init(point, utme=REAL(this%coord(i,j,1),kind=fp_utm), &
         utmn=REAL(this%coord(i,j,2),kind=fp_utm), &
         fuso=this%fuso, elliss=this%elliss)
      ELSE
        CALL init(point, lon=REAL(this%coord(i,j,1),kind=fp_geo), &
         lat=REAL(this%coord(i,j,2),kind=fp_geo))
      ENDIF
      DO p = 1, SIZE(poly)
        IF (geo_coord_inside(point, poly(p))) THEN
          spoly(i,j) = p
          CYCLE
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ELSE IF (PRESENT(mpoly)) THEN
  ALLOCATE(mpoly(this%nx, this%ny, SIZE(poly)))
  mpoly(:,:,:) = .FALSE.
  DO j = 1, this%ny
    DO i = 1, this%nx
      IF (this%proj == gg_proj_utm) THEN
        CALL init(point, utme=REAL(this%coord(i,j,1),kind=fp_utm), &
         utmn=REAL(this%coord(i,j,2),kind=fp_utm), &
         fuso=this%fuso, elliss=this%elliss)
      ELSE
        CALL init(point, lon=REAL(this%coord(i,j,1),kind=fp_geo), &
         lat=REAL(this%coord(i,j,2),kind=fp_geo))
      ENDIF
      DO p = 1, SIZE(poly)
        mpoly(i,j,p) = geo_coord_inside(point, poly(p))
      ENDDO
    ENDDO
  ENDDO
ENDIF

END SUBROUTINE gg_inside


! Transformation methods acting only on one field at a time
! and not on grid description

!> Applica un filtro a 5 punti alla vista 2d dell'oggetto \a this.
!! Influenza solo la sezione 2d selezionata, gli altri eventuali campi
!! non sono modificati, come pure la descrizione della griglia.
SUBROUTINE gg_filter5p(this, times)
TYPE(geo_grid), INTENT(INOUT) :: this !< griglia da filtrare
INTEGER, INTENT(in), OPTIONAL :: times !< numero di iterazioni del filtro, default=1

INTEGER :: i, j, im1, ip1, jm1, jp1, l, ltimes, sh(2)
REAL(kind=fp_gg), ALLOCATABLE :: tmpbuff(:,:)

IF (PRESENT(times)) THEN
  ltimes = times
ELSE
  ltimes = 1
ENDIF
IF (ASSOCIATED(this%field2d) .AND. ltimes > 0) THEN
  sh = SHAPE(this%field2d)
  ALLOCATE(tmpbuff(sh(1), sh(2)))

  DO l = 1, ltimes
    DO j = 1, sh(2)
      jm1=MAX(j-1,1)
      jp1=MIN(j+1,sh(2))
      DO i = 1, sh(1)
        im1=MAX(i-1,1)
        ip1=MIN(i+1,sh(1))
        tmpbuff(i,j) = 0.2_fp_gg*(this%field2d(i,j) + &
         this%field2d(im1,j) + &
         this%field2d(ip1,j) + &
         this%field2d(i,jm1) + &
         this%field2d(i,jp1))
      ENDDO
    ENDDO
  ENDDO
  this%field2d = tmpbuff

  DEALLOCATE(tmpbuff)
ENDIF

END SUBROUTINE gg_filter5p


!> Riscala il contenuto numerico della vista 2d dell'oggetto \a this.
!! Influenza solo la sezione 2d selezionata, gli altri eventuali campi
!! non sono modificati, come pure la descrizione della griglia.
SUBROUTINE gg_rescale(this, convm, convs)
TYPE(geo_grid), INTENT(INOUT) :: this !< griglia da riscalare
REAL(kind=fp_gg), INTENT(in) :: convm !< fattore moltiplicativo
REAL(kind=fp_gg), INTENT(in) :: convs !< addendo

IF (convm /= 1. .OR. convs /= 0.) THEN ! Convert
  WHERE(this%field2d(:,:) /= gg_miss)
    this%field2d(:,:) = convs + &
     this%field2d(:,:)*convm
  END WHERE
ENDIF

END SUBROUTINE gg_rescale


! Transformation methods acting on the whole grid
! including grid description => involve reallocating field5d

!> Ritaglia la griglia \a this del numero di punti specificati.
!! Tutta la griglia 5d viene ritagliata e riallocata, per cui
!! eventuali puntatori associati tramite la ::getval non sono più
!! validi e non vanno utilizzati prima di aver richiamato nuovamente
!! la ::getval.
SUBROUTINE gg_cut(this, i1, i2, j1, j2, ier)
TYPE(geo_grid), INTENT(INOUT) :: this !< oggetto da ritagliare
INTEGER, INTENT(IN) :: i1 !< numero di punti da ritagliare a ovest
INTEGER, INTENT(IN) :: i2 !< numero di punti da ritagliare a est
INTEGER, INTENT(IN) :: j1 !< numero di punti da ritagliare a sud
INTEGER, INTENT(IN) :: j2 !< numero di punti da ritagliare a nord
INTEGER, INTENT(OUT) :: ier !< se /=0 si è verificato un errore nei parametri di taglio

INTEGER :: sh(5)
REAL(kind=fp_gg), POINTER :: tmp5d(:,:,:,:,:)

IF (i1 < 0 .OR. i2 < 0 .OR. j1 < 0 .OR. j2 < 0) THEN
  CALL raise_Error('invalid cut parameters', 1, ier)
  RETURN
ENDIF
IF (i1+i2 > this%nx .OR. j1+j2 > this%ny) THEN
  CALL raise_error('cut bigger than actual size of area', 1, ier)
  RETURN
ENDIF

sh = SHAPE(this%field5d)
IF (sh(1) /= this%nx .OR. sh(2) /= this%ny) THEN
  CALL raise_error('internal grid error', 3, ier)
  RETURN
ENDIF
ALLOCATE(tmp5d(sh(1)-(i1+i2), sh(2)-(j1+j2), sh(3), sh(4), sh(5)))
tmp5d(:,:,:,:,:) = this%field5d(i1+1:sh(1)-i2, j1+1:sh(2)-j2, :, :, :)

DEALLOCATE(this%field5d)
this%field5d => tmp5d
! Adjust the grid description
this%nx = this%nx-(i1+i2)
this%ny = this%ny-(j1+j2)
this%x1 = this%x1+i1*this%dx
this%x2 = this%x2-i2*this%dx
this%y1 = this%y1+j1*this%dy
this%y2 = this%y2-j2*this%dy
CALL set_2d_slice(this) ! Assign field2d

END SUBROUTINE gg_cut


!> Rigriglia \a this su una nuova griglia in cui ogni punto
!! è uguale alla media di \a ngx X \a ngy punti della griglia originaria
!! (medie su box).
!! Il descrittore della griglia viene aggiornato coerentemente.
!! Tutta la griglia 5d viene ricalcolata e riallocata, per cui
!! eventuali puntatori associati tramite la ::getval non sono più
!! validi e non vanno utilizzati prima di aver richiamato nuovamente
!! la ::getval.
SUBROUTINE gg_regrid(this, ngx, ngy, ier)
TYPE(geo_grid), INTENT(INOUT) :: this !> oggetto da rigrigliare
INTEGER, INTENT(IN) :: ngx !> numero di punti su cui fare la media lungo l'asse x
INTEGER, INTENT(IN) :: ngy !> numero di punti su cui fare la media lungo l'asse y
INTEGER, INTENT(OUT) :: ier !< se /=0 si è verificato un errore nei parametri di rigrigliamento

INTEGER :: i, j, ie, je, ii, jj, navg, l3, l4, l5
INTEGER :: sh(5)
REAL(kind=fp_gg), POINTER :: tmp5d(:,:,:,:,:)

! Sanity checks
IF (ngx <= 0 .OR. ngy <= 0 .OR. ngx > this%nx .OR. ngy > this%ny) THEN
  CALL raise_Error('invalid regrid parameters', 1, ier)
  RETURN
ENDIF
IF (ngx == 1 .AND. ngy == 1) RETURN ! Nothing to do

sh = SHAPE(this%field5d)
IF (sh(1) /= this%nx .OR. sh(2) /= this%ny) THEN
  CALL raise_error('internal grid error', 3, ier)
  RETURN
ENDIF
ALLOCATE(tmp5d(sh(1)/ngx, sh(2)/ngy, sh(3), sh(4), sh(5)))

DO l5 = 1, sh(5)
  DO l4 = 1, sh(4)
    DO l3 = 1, sh(3)
      ii = 0
      jj = 0
      DO j = 1, this%ny-ngy+1, ngy
        je = j+ngy-1
        jj = jj+1
        DO i = 1, this%nx-ngx+1, ngx
          ie = i+ngx-1
          ii = ii+1
          navg = COUNT(this%field5d(i:ie,j:je,l3,l4,l5) /= gg_miss)
          IF (navg > 0) THEN
            tmp5d(ii,jj,l3,l4,l5) = SUM(this%field5d(i:ie,j:je,l3,l4,l5), &
             MASK=(this%field5d(i:ie,j:je,l3,l4,l5) /= gg_miss))/navg
          ELSE
            tmp5d(ii,jj,l3,l4,l5) = gg_miss
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO
ENDDO

DEALLOCATE(this%field5d)
this%field5d => tmp5d
! Adjust the grid description
this%x1 = this%x1+(ngx-1)*0.5*this%dx
this%y1 = this%y1+(ngy-1)*0.5*this%dy
this%nx = this%nx/ngx
this%ny = this%ny/ngy
this%dx = this%dx*ngx
this%dy = this%dy*ngy
this%x2 = this%x1+(this%nx-1)*this%dx
this%y2 = this%y1+(this%ny-1)*this%dy
CALL set_2d_slice(this) ! Assign field2d

END SUBROUTINE gg_regrid

END MODULE geo_grid_class
