
!>\brief Controllo di qualità climatico.
!! Questo modulo permette di effettuare una valutazione della probabilità che un certo intervallo 
!! di misura ha di verificarsi. Per fare ciò si utilizzano una serie di percentili precedentemente calcolati.
!! Il clima (percentili) sono suddivisi per macroarea, altezza dal livello del mare e mese dell'anno.
!!
!! definizione delle macroaree:
!! le macroaree sono tre bsate sulle macroaree definite piu'
!! generalmente al SIM; queste prime macroaree sono definite dal file di
!! default polipciv4.dat. Attribuendo una numerazione che parte da Sud e Est e scorre prima verso Nord
!! le nuove aree vengono cosi' definite:
!!  \arg area clima  1 -> macroarea SIM  7,8
!!  \arg area clima  2 -> macroarea SIM  5,6
!!  \arg area clima  3 -> macroarea SIM  1,2,3,4 
!!
!!Le altezze invece vengono cosi' definite:
!!  classe altezza = (altezza+150)/250
!! ottenendo un indice da 1 a 10 (inserendo altezze in metri).
!! Questo indice viene utilizzato per selezionare un livello tipico utilizzato nella descrizione del clima 
!! con leveltype=103:
!! \arg livello(10) = (/50,175,375,625,875,1125,1375,1625,1875,2125/)
!!
!! Area e percentile vengono utilizzati per costruire l'ident dell'anagrafica del Vol7d del clima.
!! Il clima infatti è memorizzato su file nel formato binario di Vol7d utilizzando come anno per i 
!! dati il 1001, ora 00 e minuti 00, ora contenuto nel file climaprec.v7d.
!! ecco come viene definito l'ident del clima:
!! \arg write(ident,'("BOX-",2i2.2)')iarea,perc   ! macro-area e percentile
!! In questo modo è possibile inserire nel Vol7d del clima qualsiasi parametro per leveltype e timerange.
!! Il clima viene letto dalla init.
!! Dopo l'allocazione di memoria le successive operazioni svolte da qccli sono principalmente le seguenti:
!! \arg non trattare in alcun modo i dati invalidati (manualmente)
!! \arg selezionare i dati per cui è possibile effettuare il controllo (area, variabile,confidenza, etc)
!! \arg ai dati selezionati viene attribuita una confidenza pari al percentile richiesto se inferiore al valore fornito dal clima
!! o 100-percentile se superiore 
!!
!! Per considerare valido un dato in ingresso (da sottoporre al controllo) è utilizzato in attributo dei dati 
!! che deve contenere la flag di eventuale invalidazione (manuale); la confidenza al dato calcolata viene scritta
!! in un attributo del dato. Questi due attributi possono essere specificati nella chiamata oppure assunti come 
!! default relativamente al primo e secondo attributo ai dati del volume.
!!
!! Oltre all'attributo con la confidenza, se presente, viene scritto anche l'id relativo ai dati a cui sono
!! state attribuite le confidenze, operazione necessaria per l'ottimizzazione della riscrittura dei dati.
!!
!!\ingroup qc

!> \todo Bisognerebbe validare il volume sottoposto al controllo per vedere se ha i requisiti.
!!\todo Per ora il controllo climatico è stato sviluppato e testato per le precipitazioni: 
!!gli altri parametri devono essere implementati apportando piccole modifiche. 
!!

!#!! Programma Esempio del controllo climatico per le precipitazioni:
!#!! \include  esempio_qccli.f90


module modqccli


use modqc
use vol7d_class
use geo_coord_class
use vol7d_utilities

implicit none

public

!>\brief Oggetto principale per il controllo diqualità
type :: qcclitype

  type (vol7d),pointer :: v7d !< Volume dati da controllare
  type (vol7d) :: clima !< Clima di tutte le variabili da controllare
  integer,pointer :: data_id_in(:,:,:,:,:) !< Indici dati del DB in input
  integer,pointer :: data_id_out(:,:,:,:,:) !< Indici dati del DB in output
  integer, pointer :: in_macroa(:) !< Maacroarea di appartenenza delle stazioni
  TYPE(geo_coordvect),POINTER :: macroa(:) !< serie di coordinate che definiscono le macroaree

end type qcclitype


!>\brief Inizializzazione
interface init
  module procedure qccliinit
end interface

!>\brief  Allocazione di memoria
interface alloc
  module procedure qcclialloc
end interface

!>\brief Cancellazione
interface delete
  module procedure qcclidelete
end interface


contains

!>\brief Init del controllo di qualità climatico.
!!Effettua la lettura dei file e altre operazioni di inizializzazione.

subroutine qccliinit(qccli,v7d,ier,data_id_in,macropath,climapath)

type(qcclitype),intent(in out) :: qccli !< Oggetto per il controllo climatico
type (vol7d),intent(in),target:: v7d !< Il volume Vol7d da controllare
integer ,intent(out) :: ier !< condizione di errore ( tutto OK = O )
integer,intent(in),optional,target:: data_id_in(:,:,:,:,:) !< Indici dei dati in DB
character(len=512),intent(in),optional :: macropath !< file delle macroaree
character(len=512),intent(in),optional :: climapath !< file con il volume del clima

integer :: istat,iuni
character(len=512) :: filepath

ier=0

! riporto il volume dati nel mio oggetto
qccli%v7d => v7d

if (present(data_id_in))then
  qccli%data_id_in => data_id_in
end if

nullify ( qccli%in_macroa )

if (present(macropath))then
  filepath=macropath
else
 filepath=get_package_filepath('polipciv4.dat', filetype_data)
end if

CALL import(qccli%macroa, shpfilesim=filepath)

if (present(climapath))then
  filepath=macropath
else
 filepath=get_package_filepath('climaprec.v7d', filetype_data)
end if


call init(qccli%clima)

iuni=getunit()
open (unit=iuni,file=filepath,form="UNFORMATTED")
call read_from_file (qccli%clima,unit=iuni)
close (unit=iuni)

return
end subroutine qccliinit


!>\brief Allocazioni di memoria
subroutine qcclialloc(qccli,ier)
                                ! pseudo costruttore con distruttore automatico

type(qcclitype),intent(in out) :: qccli !< Oggetto per il controllo climatico
integer ,intent(out):: ier !< stato di errore

integer :: istat,istatt,nv
integer :: sh(5)

! se ti sei dimenticato di deallocare ci penso io
call  qcclidealloc(qccli,ier)
if ( ier /= 0 )then
  ier=2
  return
end if

istatt=0
ier=0

!!$if (associated (qccli%v7d%dativar%r )) then
!!$  nv=size(qccli%v7d%dativar%r)
!!$
!!$  allocate(qccli%valminr(nv),stat=istat)
!!$  istatt=istatt+istat
!!$  allocate(qccli%valmaxr(nv),stat=istat)
!!$  istatt=istatt+istat
!!$
!!$  if (istatt /= 0) ier=1
!!$
!!$end if

if (associated (qccli%v7d%ana )) then
  allocate (qccli%in_macroa(size(qccli%v7d%ana )),stat=istatt)
  if (istatt /= 0) ier=1
end if

if (associated(qccli%data_id_in))then
  sh=shape(qccli%data_id_in)
  allocate (qccli%data_id_out(sh(1),sh(2),sh(3),sh(4),sh(5)),stat=istatt)
  if (istatt /= 0)then
    ier=1
  else
    qccli%data_id_out=imiss
  end if
end if

return

end subroutine qcclialloc


!>\brief Deallocazione della memoria

subroutine qcclidealloc(qccli,ier)
                                ! pseudo distruttore

type(qcclitype),intent(in out) :: qccli !< Oggetto per l controllo climatico
integer,intent(out) :: ier !< Condizione di errore

ier=0

!!$if ( associated ( qccli%valminr)) then
!!$  deallocate(qccli%valminr)
!!$end if
!!$
!!$if ( associated ( qccli%valmaxr)) then
!!$  deallocate(qccli%valmaxr)
!!$end if

if (associated (qccli%in_macroa)) then
  deallocate (qccli%in_macroa)
end if

if (associated(qccli%data_id_out))then
  deallocate (qccli%data_id_out)
end if

return
end subroutine qcclidealloc


!>\brief Cancellazione


subroutine qcclidelete(qccli,ier)
                                ! decostruttore a mezzo
type(qcclitype),intent(in out) :: qccli !< Oggetto per l controllo climatico
integer ,intent(out) :: ier !< Condizione di errore

integer :: istat

ier=0

call qcclidealloc(qccli,ier)

call delete(qccli%clima)

return
end subroutine qcclidelete


!>\brief Controllo di Qualità climatico.
!!Questo è il vero e proprio controllo di qualità climatico.
!!Avendo a disposizione un volume dati climatico 
!!contenente i percentili suddivisi per area, altezza sul livello
!!del mare, per mese dell'anno viene selezionato il percentile (default=80%) e sulla base di questo 
!!vengono assegnate le opportune confidenze.


SUBROUTINE QuaConCLI (qccli,ier,tbattrin,tbattrout,&
 anamask,timemask,levelmask,timerangemask,varmask,networkmask)


type(qcclitype),intent(in out) :: qccli !< Oggetto per il controllo di qualità
integer ,intent(out) :: ier !< Condizione di errore
character (len=10) ,intent(in),optional :: tbattrin !< attributo con la confidenza in input
character (len=10) ,intent(in),optional :: tbattrout !< attributo con la confidenza in output
logical ,intent(in),optional :: anamask(:) !< Filtro sulle anagrafiche
logical ,intent(in),optional :: timemask(:) !< Filtro sul tempo
logical ,intent(in),optional :: levelmask(:) !< Filtro sui livelli
logical ,intent(in),optional :: timerangemask(:) !< filtro sui timerange
logical ,intent(in),optional :: varmask(:) !< Filtro sulle variabili
logical ,intent(in),optional :: networkmask(:) !< Filtro sui network
integer,intent(in),optional :: perc   !< Percentile soglia da usare nel controllo(default: 80)


CHARACTER(len=vol7d_ana_lenident) :: ident
REAL(kind=fp_geo) :: lat,lon
integer :: imese
                                !local
integer :: i,j,indtbattrin,indtbattrout,i1,i2,i3,i4,i5,i6
logical :: anamaskl(size(qccli%v7d%ana)), timemaskl(size(qccli%v7d%time)), levelmaskl(size(qccli%v7d%level)), &
 timerangemaskl(size(qccli%v7d%timerange)), varmaskl(size(qccli%v7d%dativar%r)), networkmaskl(size(qccli%v7d%network)) 

integer :: indana ,indanavar, indtime ,indlevel ,indtimerange ,inddativarr, indnetwork
integer :: indcana,           indctime,indclevel,indctimerange,indcdativarr,indcnetwork
real :: datoqui,climaqui
integer :: altezza,mh,iarea,lperc


TYPE(vol7d_ana)  :: ana
TYPE(datetime)   :: time
TYPE(vol7d_level):: level
type(vol7d_var)  :: anavar


!call qccli_validate (qccli)

if (present(perc))then
  lperc=perc
else
  perc=80
end if

if (present(tbattrin))then
  indtbattrin = firsttrue(qccli%v7d%dativarattr%r(:)%btable == tbattrin)
else
  indtbattrin=1
end if

if (present(tbattrout))then
  indtbattrout = firsttrue(qccli%v7d%dativarattr%r(:)%btable == tbattrout)
else
  indtbattrout=2
end if

if (indtbattrin <=0 .or. indtbattrout <= 0 ) then
  ier=5
  return
end if

if(present(anamask)) then
  anamaskl = anamask
else
  anamaskl = .true.
endif
if(present(timemask)) then
  timemaskl = timemask
else
  timemaskl = .true.
endif
if(present(levelmask)) then
  levelmaskl = levelmask
else
  levelmaskl = .true.
endif
if(present(timerangemask)) then
  timerangemaskl = timerangemask
else
  timerangemaskl = .true.
endif
if(present(varmask)) then
  varmaskl = varmask
else
  varmaskl = .true.
endif
if(present(networkmask)) then
  networkmaskl = networkmask
else
  networkmaskl = .true.
endif

qccli%v7d%voldatiattrr(:,:,:,:,:,:,indtbattrout)=bmiss


! valuto in quale macroarea sono le stazioni
qccli%in_macroa = imiss

DO i = 1, SIZE(qccli%v7d%ana)
  DO j = 1, SIZE(qccli%macroa)
    IF (inside(qccli%v7d%ana(i)%coord, qccli%macroa(j))) THEN
      qccli%in_macroa(i) = j
      EXIT
    ENDIF
  ENDDO
ENDDO


do indana=1,size(qccli%v7d%ana)

  ! rielabora le macroarea facendole Valentine thinking

  if (qccli%in_macroa(indana) >= 1 .and. qccli%in_macroa(indana) <= 4 ) iarea=3
  if (qccli%in_macroa(indana) == 5 .or.  qccli%in_macroa(indana) == 6 ) iarea=2
  if (qccli%in_macroa(indana) == 7 .or.  qccli%in_macroa(indana) == 8 ) iarea=1


  do  indnetwork=1,size(qccli%v7d%network)
    do indlevel=1,size(qccli%v7d%level)
      do indtimerange=1,size(qccli%v7d%timerange)
        do inddativarr=1,size(qccli%v7d%dativar%r)

          do indtime=1,size(qccli%v7d%time)
            if (anamaskl(indana).and.timemaskl(indtime).and.levelmaskl(indlevel).and. &
             timerangemaskl(indtimerange).and.varmaskl(inddativarr).and.networkmaskl(indnetwork).and.&
             c_e(qccli%v7d%voldatir(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork)))then
              if( invalidated(qccli%v7d%voldatiattrb&
               (indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrin))) cycle
              
              lat=0.0d0
              lon=0.0d0
              write(ident,'("BOX-",2i2.2)')iarea,perc   ! macro-area e percentile
              call init(ana,lat=lat,lon=lon,ident=ident)

              CALL getval(qccli%v7d%time(indtime), month=imese)
              call init(time, year=1001, month=imese, day=1, hour=00,minute=00)
              call init(anavar,"B07001" )
              indanavar        = firsttrue(qccli%v7d%anavar%i  == anavar)

              if (indanavar <=0 )cycle
              altezza= qccli%v7d%volanai(indana,indanavar,indnetwork)
              call macro_height(altezza,mh)
              call init(level, 103,mh,0)

              indcnetwork      = 1
              indcana          = firsttrue(qccli%clima%ana     == ana)
              indctime         = firsttrue(time                              == qccli%clima%time)
              indclevel        = firsttrue(level                             == qccli%clima%level)
              indctimerange    = firsttrue(qccli%v7d%timerange(indtimerange) == qccli%clima%timerange)
              indcdativarr     = firsttrue(qccli%v7d%dativar%r(inddativarr)  == qccli%clima%dativar%r)

              !print *,"dato  ",qccli%v7d%timerange(indtimerange) 
              !print *,"clima ",qccli%clima%timerange
              !print *,indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork
              if (indcana <= 0 .or. indctime <= 0 .or. indclevel <= 0 .or. indctimerange <= 0 .or. indcdativarr <= 0 &
               .or. indcnetwork <= 0 ) cycle

              datoqui = qccli%v7d%voldatir  (indana ,indtime ,indlevel ,indtimerange ,inddativarr, indnetwork )
              climaqui= qccli%clima%voldatir(indcana,indctime,indclevel,indctimerange,indcdativarr,indcnetwork)

              if (c_e(climaqui).and. c_e(datoqui))then
                !print *,"macroarea,iarea,mese,altezza,mh ",qccli%in_macroa(indana),iarea,imese,altezza,mh
                !print*,"dato,clima ",datoqui,climaqui
                if ( datoqui > climaqui) then
                  
                  qccli%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=100-perc

                else
                  
                  qccli%v7d%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrout)=perc
                  
                end if
 
                if ( associated ( qccli%data_id_in)) then
                  qccli%data_id_in(indana,indtime,indlevel,indtimerange,indnetwork)=&
                   qccli%data_id_out(indana,indtime,indlevel,indtimerange,indnetwork)
                end if
                
              end if
            end if
          end do
        end do
      end do
    end do
  end do
end do


ier=0
return

end subroutine QUACONCLI


subroutine macro_height(altezza,mh)

integer :: altezza,mh
integer :: livello(10) = (/50,175,375,625,875,1125,1375,1625,1875,2125/)

if (c_e(altezza))then
  mh=livello(min(max((altezza+150)/250,1),10))
else
  mh=imiss
end if

end subroutine macro_height



!!$subroutine qccli_validate(qccli)
!!$type(qcclitype),intent(in) :: qccli
!!$
!!$!todo da validare
!!$
!!$return
!!$end subroutine qccli_validate

end module modqccli


!> \example esempio_qccli.f90
!! Un programma esempio del modulo qccli

