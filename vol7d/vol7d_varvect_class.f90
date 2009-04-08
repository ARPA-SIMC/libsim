!> Classe per gestire un vettore di oggetti di tipo vol7d_var_class::vol7d_var.
!! Questo modulo definisce una classe per gestire un vettore di ogetti vol7d_var,
!! cioè variabili meteorologiche osservate, anche aventi tipi numerici diversi.
!! \ingroup vol7d
MODULE vol7d_varvect_class
USE kinds
USE missing_values
USE vol7d_var_class
USE optional_values
use err_handling

IMPLICIT NONE

!> Definisce un vettore di vol7d_var_class::vol7d_var per ogni tipo di dato
!! supportato.
!! Un puntatore non associato indica che non c'è nessuna variabile avente
!! dati di quel tipo.
!! I membri di \a vol7d_varvect sono pubblici e quindi liberamente
!! accessibili e scrivibili, ma è comunque consigliato allocarli tramite
!! l'apposito metodo.
TYPE vol7d_varvect
  TYPE(vol7d_var),POINTER :: r(:) !< vettore di variabili reali
  TYPE(vol7d_var),POINTER :: d(:) !< vettore di variabili a doppia precisione
  TYPE(vol7d_var),POINTER :: i(:) !< vettore di variabili intere
  TYPE(vol7d_var),POINTER :: b(:) !< vettore di variabili byte
  TYPE(vol7d_var),POINTER :: c(:) !< vettore di variabili carattere
END TYPE  vol7d_varvect


!> Costruttore per la classe vol7d_varvect.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE vol7d_varvect_init
END INTERFACE

!> Distruttore per la classe vol7d_varvect.
INTERFACE delete
  MODULE PROCEDURE vol7d_varvect_delete
END INTERFACE

INTERFACE index
  MODULE PROCEDURE vol7d_varvect_index,vol7d_varvect_indexvect
END INTERFACE

!> \brief display on the screen a brief content of object
INTERFACE display
  MODULE PROCEDURE display_varvect
END INTERFACE


CONTAINS

!> Inizializza un oggetto di tipo vol7d_varvect.
!! Non riceve alcun parametro tranne l'oggetto stesso.  Attenzione, è necessario
!! comunque chiamare sempre il costruttore per evitare di avere dei puntatori in
!! uno stato indefinito.
SUBROUTINE vol7d_varvect_init(this)
TYPE(vol7d_varvect),INTENT(INOUT) :: this !< oggetto da inizializzare

NULLIFY(this%r, this%d, this%i, this%b, this%c)

END SUBROUTINE vol7d_varvect_init


!> Distrugge l'oggetto in maniera pulita, liberando l'eventuale memoria
!! dinamicamente allocata.
elemental SUBROUTINE vol7d_varvect_delete(this)
TYPE(vol7d_varvect),INTENT(INOUT) :: this !< oggetto da distruggere

IF (ASSOCIATED(this%r)) DEALLOCATE(this%r)
IF (ASSOCIATED(this%d)) DEALLOCATE(this%d)
IF (ASSOCIATED(this%i)) DEALLOCATE(this%i)
IF (ASSOCIATED(this%b)) DEALLOCATE(this%b)
IF (ASSOCIATED(this%c)) DEALLOCATE(this%c)

END SUBROUTINE vol7d_varvect_delete


!> Metodo per allocare i vettori di variabili richiesti.
!! Se uno dei parametri \a nvar* non è presente o è <= 0 non viene
!! allocato niente per quel tipo di variabile.
!! Il metodo può essere chiamato più volte per allocare successivamente
!! diversi tipi di variabili.
SUBROUTINE vol7d_varvect_alloc(this, nvarr, nvard, nvari, nvarb, nvarc, ini)
TYPE(vol7d_varvect),INTENT(INOUT) :: this !< oggetto in cui allocare i vettori
INTEGER,INTENT(in),OPTIONAL :: nvarr !< numero di variabili con dati reali
INTEGER,INTENT(in),OPTIONAL :: nvard !< numero di variabili con dati a doppia precisione
INTEGER,INTENT(in),OPTIONAL :: nvari !< numero di variabili con dati interi
INTEGER,INTENT(in),OPTIONAL :: nvarb !< numero di variabili con dati byte
INTEGER,INTENT(in),OPTIONAL :: nvarc !< numero di variabili con dati carattere
LOGICAL,INTENT(in),OPTIONAL :: ini !< se fornito e vale \c .TRUE., viene chiamato il costruttore vol7d_var_class::init (senza parametri opzionali) per ognuna delle variabili allocate in ciascun vettore

INTEGER :: i
LOGICAL :: linit

IF (PRESENT(ini)) THEN
  linit = ini
ELSE
  linit = .FALSE.
ENDIF

IF (PRESENT(nvarr)) THEN
  IF (nvarr > 0) THEN
    IF (ASSOCIATED(this%r)) DEALLOCATE(this%r)
    ALLOCATE(this%r(nvarr))
    IF (linit) THEN
      DO i = 1, nvarr
        CALL init(this%r(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvard)) THEN
  IF (nvard > 0) THEN
    IF (ASSOCIATED(this%d)) DEALLOCATE(this%d)
    ALLOCATE(this%d(nvard))
    IF (linit) THEN
      DO i = 1, nvard
        CALL init(this%d(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvari)) THEN
  IF (nvari > 0) THEN
    IF (ASSOCIATED(this%i)) DEALLOCATE(this%i)
    ALLOCATE(this%i(nvari))
    IF (linit) THEN
      DO i = 1, nvari
        CALL init(this%i(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvarb)) THEN
  IF (nvarb > 0) THEN
    IF (ASSOCIATED(this%b)) DEALLOCATE(this%b)
    ALLOCATE(this%b(nvarb))
    IF (linit) THEN
      DO i = 1, nvarb
        CALL init(this%b(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF
IF (PRESENT(nvarc)) THEN
  IF (nvarc > 0) THEN
    IF (ASSOCIATED(this%c)) DEALLOCATE(this%c)
    ALLOCATE(this%c(nvarc))
    IF (linit) THEN
      DO i = 1, nvarc
        CALL init(this%c(i))
      ENDDO
    ENDIF
  ENDIF
ENDIF

END SUBROUTINE vol7d_varvect_alloc


!> Cerca l'indice del primo o ultimo elemento di this uguale a search
subroutine vol7d_varvect_index(this, search, mask, back,type,index_v)

TYPE(vol7d_varvect),intent(in) :: this !< object to search in
type(vol7d_var),INTENT(in) ::  search !< what to search
LOGICAL,INTENT(in),OPTIONAL :: mask(:), back
character(len=*),intent(inout),optional :: type
INTEGER :: index_v !< indice del primo o ultimo elemento di this uguale a search


index_v=0

select case (optio_c(type,1))

case ("d")
  if (associated(this%d))then
    index_v=index(this%d(:), search, mask, back) ! vettore di variabili a doppia precisione
  end if
    
case ("r")
  if (associated(this%r))then
    index_v=index(this%r(:), search, mask, back) ! vettore di variabili reali
  end if

case ("i")
  if (associated(this%i))then
    index_v=index(this%i(:), search, mask, back) ! vettore di variabili intere
  end if

case ("b")
  if (associated(this%b))then
    index_v=index(this%b(:), search, mask, back) ! vettore di variabili byte
  end if

case ("c")
  if (associated(this%c))then
    index_v=index(this%c(:), search, mask, back) ! vettore di variabili carattere
  end if

case (cmiss)

  if (associated(this%d))then
    index_v=index(this%d(:), search, mask, back) ! vettore di variabili a doppia precisione
    if (present(type)) type="d"
  end if

  if(index_v == 0)then
    if (associated(this%r))then
      index_v=index(this%r(:), search, mask, back) ! vettore di variabili reali
      if (present(type)) type="r"
    end if
  end if
  
  if(index_v == 0)then
    if (associated(this%i))then
      index_v=index(this%i(:), search, mask, back) ! vettore di variabili intere
      if (present(type)) type="i"
  end if
end if
  
  if(index_v == 0)then
    if (associated(this%b))then
      index_v=index(this%b(:), search, mask, back) ! vettore di variabili byte
      if (present(type)) type="b"
    end if
  end if
  
  if(index_v == 0)then
    if (associated(this%c))then
      index_v=index(this%c(:), search, mask, back) ! vettore di variabili carattere
      if (present(type)) type="c"
    end if
  end if

  if (index_v == 0) type=cmiss

case default

  CALL raise_error('variable type not contemplated: '//type)

end select

end subroutine vol7d_varvect_index


!> Cerca l'indice del primo o ultimo elemento di this uguale a search
subroutine vol7d_varvect_indexvect(this, search, type,index_v)

TYPE(vol7d_varvect),intent(in) :: this !< object to search in
type(vol7d_var),INTENT(in) ::  search(:) !< what to search
character(len=*),intent(inout) :: type(:) !type of vector found ("d","r","i","b","c")
INTEGER :: index_v(:) !< indice del primo o ultimo elemento di this uguale a search

integer :: i

do i =1 ,size(search)
  call vol7d_varvect_index(this, search(i),type=type(i),index_v=index_v(i))
end do

end subroutine vol7d_varvect_indexvect


!> \brief display on the screen a brief content of vol7d_var object
subroutine display_varvect(this)

TYPE(vol7d_varvect),INTENT(in) :: this !< vol7d_varvect object to display

if (associated(this%d))then
PRINT *,"-----------------  varvect  --------------------------"
  print*,"double precision elements=",size(this%d)
  call display(this%d(:)) ! vettore di variabili a doppia precisione
end if

if (associated(this%r))then
PRINT *,"-----------------  varvect  --------------------------"
  print*,"real elements=",size(this%r)
  call display(this%r(:)) ! vettore di variabili reali
end if

if (associated(this%i))then
PRINT *,"-----------------  varvect  --------------------------"
  print*,"integer elements=",size(this%i)
  call display(this%i(:)) ! vettore di variabili intere
end if

if (associated(this%b))then
PRINT *,"-----------------  varvect  --------------------------"
  print*,"byte elements=",size(this%b)
  call display(this%b(:)) ! vettore di variabili byte
end if

if (associated(this%c))then
PRINT *,"-----------------  varvect  --------------------------"
  print*,"character elements=",size(this%c)
  call display(this%c(:)) ! vettore di variabili carattere
end if


end subroutine display_varvect


END MODULE vol7d_varvect_class
