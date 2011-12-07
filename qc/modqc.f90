! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!> \defgroup qc Pacchetto libsim, libreria qc.
!! Procedure per il controllo di qualità.
!! Al momento è implementato solo il controllo di qualità climatico.


!> Utilities and defines for quality control.
!! 
!! Concise, high-value definitions of Data Quality by expert users,
!! analysts, implementers and journalists. This is a great starting point
!! to learn about Data Quality.
!! 
!! Data Quality: The Accuracy Dimension
!! 
!! "Data quality is defined as follows: data has quality if it satisfies
!! the requirements of its intended use. It lacks quality to the extent
!! that it does not satisfy the requirement. In other words, data quality
!! depends as much on the intended use as it does on the data itself. To
!! satisfy the intended use, the data must be accurate, timely, relevant,
!! complete, understood, and trusted."  Jack E. Olson
!! 
!! No Data Left Behind: Federal Student Aid - A Case History
!! 
!! "Data quality institutionalizes a set of repeatable processes to
!! continuously monitor data and improve data accuracy, completeness,
!! timeliness and relevance."  Holly Hyland and Lisa Elliott, Federal
!! Student Aid
!! 
!! Data Quality: It's a Family Affair
!! 
!! Data Quality definition: "The state of completeness, consistency,
!! timeliness and accuracy that makes data appropriate for a specific
!! use."  Wim Helmer, Dun & Bradstreet
!! 
!! Data Quality and Quality Management - Examples of Quality Evaluation
!! Procedures and Quality Management in European National Mapping
!! Agencies
!! 
!! "Quality is defined as the totality of characteristics of a product
!! that bear on its ability to satisfy stated and implied needs (ISO
!! 8402, 1994). In the new ISO/DIS 9000:2000 standard (2000) the
!! definition of quality is: 'Ability of a set of inherent
!! characteristics of a product, system or process to fulfill
!! requirements of customers and other interested parties.' This
!! indicates that data quality and quality management are very closely
!! related. Data quality is part of the organisation's total quality
!! management." Antti Jakobsson
!! 
!! text below from Wikipedia
!! http://it.wikipedia.org/wiki/Test_di_verifica_d%27ipotesi
!! http://creativecommons.org/licenses/by-sa/3.0/deed.it
!!  L'ambito statistico
!! 
!! Nel secondo caso la situazione è modificata in quanto interviene un
!! elemento nuovo, ovvero il caso. Si supponga di avere una moneta
!! recante due facce contrassegnate con testa e croce. Volendo verificare
!! l'ipotesi di bilanciamento della moneta si eseguono 20 lanci e si
!! contano quelli che danno esito testa. La conseguenza del bilanciamento
!! consiste nell'osservare un valore di teste attorno a 10. Tuttavia
!! anche in ipotesi di bilanciamento non si può escludere di osservare 20
!! teste. D'altronde, l'ipotesi di bilanciamento è logicamente
!! compatibile con un numero di teste variante da 0 a 20. In tale
!! contesto una qualsiasi decisione in merito all'ipotesi da verificare
!! comporta un rischio di errore. Ad esempio rigettare l'ipotesi di
!! bilanciamento della moneta avendo osservato 20 teste su 20 lanci
!! comporta il rischio di prendere una decisione errata. Nel procedere
!! alla verifica dell'ipotesi di bilanciamento della moneta, si ricorre a
!! una variabile casuale X. Tale variabile casuale X è una variabile
!! aleatoria discreta con distribuzione binomiale B(20; 0,5), dove 20
!! indica il numero di lanci e 0,5 la probabilità che si verifichi
!! l'evento "testa".
!! 
!! Il risultato sperimentale si deve quindi confrontare con tale
!! distribuzione: quanto è distante tale risultato dal valore medio della
!! distribuzione B(20; 0,5)? Per rispondere alla domanda si deve
!! individuare un valore caratteristico della distribuzione B(20;
!! 0,5). Nel nostro caso tale valore caratteristico è il valore medio
!! 20/2 = 10. Per valutare la distanza tra il valore sperimentale e
!! quello atteso si valuta la probabilità di ottenere un valore
!! sperimentale lontano dal valore medio di B(20; 0,5), ossìa nel caso
!! che dal nostro esperimento risulti X=15 (15 teste dopo 20 lanci), si
!! calcola P{|X-10|>=15-10} quindi P{X<=5 oppure X>=15}=0,041.
!! 
!! Quindi, usando una moneta ben bilanciata, la probabilità di ottenere
!! un numero di teste X >= 15 (oppure X <= 5) dopo 20 lanci è pari a
!! 0,041 ossia al 4,1%. Giudicando bassa tale probabilità si rifiuterà
!! l'ipotesi di bilanciamento della moneta in esame, accettando quindi il
!! rischio del 4,1% di compiere un errore nel rifiutarla. Di solito, il
!! valore della probabilità adottato per rifiutare l'ipotesi nulla è <
!! 0,05. Tale valore è detto livello di significatività ed è definibile
!! come segue: il livello di significatività sotto l'ipotesi nulla è la
!! probabilità di cadere nella zona di rifiuto quando l'ipotesi nulla è
!! vera. Tale livello di significatività si indica convenzionalmente con
!! α. Il livello di significatività osservato α del test per il quale si
!! rifiuterebbe l'ipotesi nulla è detto valore-p (p-value). Riprendendo
!! l'esempio sopra riportato il valore-p è pari a 0,041. Adottando
!! nell'esempio α = 0,05, si rifiuterà l'ipotesi se
!! P{|X-10|>=x}<0,05. Tale condizione si raggiunge appunto se X<6 oppure
!! X>14. Tale insieme di valori si definisce convenzionalmente come
!! regione di rifiuto. Viceversa l'insieme { 6,7...14} si definisce regione
!! di accettazione. In questo modo si è costruita una regola di
!! comportamento per verificare l'ipotesi di bilanciamento della
!! moneta. Tale regola definisce il test statistico.
!! 
!! In termini tecnici l'ipotesi da verificare si chiama ipotesi nulla e
!! si indica con H0, mentre l'ipotesi alternativa con H1. Nel caso della
!! moneta, se p è la probabilità di ottenere testa in un lancio la
!! verifica di ipotesi si traduce nel seguente sistema:
!! 
!!     H_0: p = \frac{1}{2}
!!     H_1: p \ne \frac{1}{2}
!! 
!! Come già osservato, il modo di condurre un test statistico comporta un
!! rischio di errore. Nella pratica statistica si individuano due tipi di
!! errori:
!! 
!!    1. rifiutare H0 quando è vera, errore di primo tipo (α) (o errore di prima specie);
!!    2. accettare H0 quando è falsa, errore di secondo tipo (β) (o errore di seconda specie).
!! 
!! Tornando all'esempio della moneta in cui la regione di accettazione è
!! data dall'insieme di valori {6..14}, la probabilità di rifiutare H0
!! quando è vera è stato calcolato pari a 0,041.Tale probabilità
!! rappresenta il rischio di incorrere in un errore di primo tipo e si
!! indica con α. Per valutare la probabilità di un errore di secondo tipo
!! è necessario specificare un valore di p in caso di verità di H1. Si
!! supponga che p=0,80, in tal caso la distribuzione di X è una
!! B(20;0,80)
!! 
!! Con tale distribuzione di probabilità, l'errore di tipo 2 si calcola
!! sommando le probabilità relative ai valori di X della zona di
!! accettazione. Si trova quindi che la probabilità cercata è pari a
!! circa 0,20. Tale probabilità quantifica il rischio di incorrere
!! nell'errore di tipo 2. e si indica convenzionalmente con β. La
!! quantità 1-β si chiama potenza del test ed esprime quindi la capacità
!! di un test statistico riconoscere la falsità di H0 quando questa è
!! effettivamente falsa. La potenza del test trova applicazione nella
!! pratica statistica in fase di pianificazione di un esperimento.
!!
!!\ingroup qc

module modqc
use kinds
use missing_values
use optional_values
use vol7d_class

implicit none


!> Definisce il livello di attendibilit� per i dati validi
type :: qcpartype
  integer (kind=int_b):: att
end type qcpartype

!> Per dafault i dati con confidenza inferiore o uguale a 30 vengono scartati
type(qcpartype)  :: qcpar=qcpartype(30)

integer, parameter :: nqcattrvars=4
CHARACTER(len=10),parameter :: qcattrvarsbtables(nqcattrvars)=(/"*B33196","*B33192","*B33193","*B33194"/)

type :: qcattrvars
  TYPE(vol7d_var) :: vars(nqcattrvars)
  CHARACTER(len=10)   :: btables(nqcattrvars)
end type qcattrvars

!> Variables user in Quality Control
interface init
  module procedure init_qcattrvars
end interface

!> Remove data under a defined grade of confidence.
interface peeled
  module procedure peeledrb, peeleddb, peeledbb, peeledib,peeledcb &
   ,peeledri, peeleddi, peeledbi, peeledii,peeledci
end interface


!> Check data validity based on single confidence
interface vd
  module procedure vdi,vdb
end interface

!> Test di dato invalidato
interface invalidated
  module procedure invalidatedi,invalidatedb
end interface

private

public vd, init, qcattrvars_new, invalidated, peeled, vol7d_peeling
public qcattrvars, nqcattrvars, qcattrvarsbtables
public qcpar, qcpartype, qcsummaryflagi, qcsummaryflagb

contains

!> Test di validit� di dati integer
elemental logical function vdi(flag)

integer,intent(in)  :: flag !< confidenza
      
if(flag <= qcpar%att .and. c_e(flag))then
  vdi=.false.
else
  vdi=.true.
end if

return
end function vdi


!> Test di validità di dati byte
elemental logical function vdb(flag)

integer (kind=int_b),intent(in) :: flag !< confidenza
      
if(flag <= qcpar%att .and. c_e(flag))then
  vdb=.false.
else
  vdb=.true.
end if

return
end function vdb


!> Test di dato invalidato intero
elemental logical function invalidatedi(flag)

integer,intent(in)  :: flag !< attributo di invalidazione del dato
      
if(c_e(flag))then
  invalidatedi=.true.
else
  invalidatedi=.false.
end if

return
end function invalidatedi


!> Test di dato invalidato byte
elemental logical function invalidatedb(flag)

integer (kind=int_b),intent(in) :: flag !< attributo di invalidazione del dato
      
if(c_e(flag))then
  invalidatedb=.true.
else
  invalidatedb=.false.
end if

return
end function invalidatedb


!> Check data validity based on multiple confidences.
!! Compute final decision boolean flag
ELEMENTAL LOGICAL FUNCTION qcsummaryflagi(flaginv, flag1, flag2, flag3)
integer,intent(in),optional :: flaginv
integer,intent(in),optional :: flag1
integer,intent(in),optional :: flag2
integer,intent(in),optional :: flag3

qcsummaryflagi = .NOT.invalidated(optio_l(flaginv)) .AND. &
 vd(optio_l(flag1)) .AND. vd(optio_l(flag2)) .AND. vd(optio_l(flag3))

END FUNCTION qcsummaryflagi


!> Check data validity based on multiple confidences.
!! Compute final decision boolean flag
ELEMENTAL LOGICAL FUNCTION qcsummaryflagb(flaginv, flag1, flag2, flag3)
integer(kind=int_b),intent(in),optional :: flaginv
integer(kind=int_b),intent(in),optional :: flag1
integer(kind=int_b),intent(in),optional :: flag2
integer(kind=int_b),intent(in),optional :: flag3

qcsummaryflagb = .NOT.invalidated(optio_b(flaginv)) .AND. &
 vd(optio_b(flag1)) .AND. vd(optio_b(flag2)) .AND. vd(optio_b(flag3))

END FUNCTION qcsummaryflagb


! byte attributes below

elemental real function peeledrb(data,flaginv,flag1,flag2,flag3)

real, intent(in) :: data
integer(kind=int_b), intent(in),optional :: flaginv
integer(kind=int_b), intent(in),optional :: flag1
integer(kind=int_b), intent(in),optional :: flag2
integer(kind=int_b), intent(in),optional :: flag3

if (qcsummaryflagb(flaginv,flag1,flag2,flag3)) then
  peeledrb=data
else
  peeledrb=rmiss
end if

end function peeledrb

elemental real(kind=fp_d) function peeleddb(data,flaginv,flag1,flag2,flag3)

real(kind=fp_d), intent(in) :: data
integer(kind=int_b), intent(in),optional :: flaginv
integer(kind=int_b), intent(in),optional :: flag1
integer(kind=int_b), intent(in),optional :: flag2
integer(kind=int_b), intent(in),optional :: flag3

if (qcsummaryflagb(flaginv,flag1,flag2,flag3)) then
  peeleddb=data
else
  peeleddb=dmiss
end if

end function peeleddb


elemental integer function peeledib(data,flaginv,flag1,flag2,flag3)

integer, intent(in) :: data
integer(kind=int_b), intent(in),optional :: flaginv
integer(kind=int_b), intent(in),optional :: flag1
integer(kind=int_b), intent(in),optional :: flag2
integer(kind=int_b), intent(in),optional :: flag3

if (qcsummaryflagb(flaginv,flag1,flag2,flag3)) then
  peeledib=data
else
  peeledib=imiss
end if

end function peeledib


elemental integer(kind=int_b) function peeledbb(data,flaginv,flag1,flag2,flag3)

integer(kind=int_b), intent(in) :: data
integer(kind=int_b), intent(in),optional :: flaginv
integer(kind=int_b), intent(in),optional :: flag1
integer(kind=int_b), intent(in),optional :: flag2
integer(kind=int_b), intent(in),optional :: flag3

if (qcsummaryflagb(flaginv,flag1,flag2,flag3)) then
  peeledbb=data
else
  peeledbb=ibmiss
end if

end function peeledbb


elemental character(len=vol7d_cdatalen) function peeledcb(data,flaginv,flag1,flag2,flag3)

character(len=vol7d_cdatalen), intent(in) :: data
integer(kind=int_b), intent(in),optional :: flaginv
integer(kind=int_b), intent(in),optional :: flag1
integer(kind=int_b), intent(in),optional :: flag2
integer(kind=int_b), intent(in),optional :: flag3

if (qcsummaryflagb(flaginv,flag1,flag2,flag3)) then
  peeledcb=data
else
  peeledcb=cmiss
end if

end function peeledcb


! integer attributes below
! here flaginv is not optional

elemental real function peeledri(data,flaginv,flag1,flag2,flag3)

real, intent(in) :: data
integer, intent(in) :: flaginv
integer, intent(in),optional :: flag1
integer, intent(in),optional :: flag2
integer, intent(in),optional :: flag3

if (qcsummaryflagi(flaginv,flag1,flag2,flag3)) then
  peeledri=data
else
  peeledri=rmiss
end if

end function peeledri

elemental real(kind=fp_d) function peeleddi(data,flaginv,flag1,flag2,flag3)

real(kind=fp_d), intent(in) :: data
integer, intent(in) :: flaginv
integer, intent(in),optional :: flag1
integer, intent(in),optional :: flag2
integer, intent(in),optional :: flag3

if (qcsummaryflagi(flaginv,flag1,flag2,flag3)) then
  peeleddi=data
else
  peeleddi=dmiss
end if

end function peeleddi


elemental integer function peeledii(data,flaginv,flag1,flag2,flag3)

integer, intent(in) :: data
integer, intent(in) :: flaginv
integer, intent(in),optional :: flag1
integer, intent(in),optional :: flag2
integer, intent(in),optional :: flag3

if (qcsummaryflagi(flaginv,flag1,flag2,flag3)) then
  peeledii=data
else
  peeledii=imiss
end if

end function peeledii


elemental integer(kind=int_b) function peeledbi(data,flaginv,flag1,flag2,flag3)

integer(kind=int_b), intent(in) :: data
integer, intent(in) :: flaginv
integer, intent(in),optional :: flag1
integer, intent(in),optional :: flag2
integer, intent(in),optional :: flag3

if (qcsummaryflagi(flaginv,flag1,flag2,flag3)) then
  peeledbi=data
else
  peeledbi=ibmiss
end if

end function peeledbi


elemental character(len=vol7d_cdatalen) function peeledci(data,flaginv,flag1,flag2,flag3)

character(len=vol7d_cdatalen), intent(in) :: data
integer, intent(in) :: flaginv
integer, intent(in),optional :: flag1
integer, intent(in),optional :: flag2
integer, intent(in),optional :: flag3

if (qcsummaryflagi(flaginv,flag1,flag2,flag3)) then
  peeledci=data
else
  peeledci=cmiss
end if

end function peeledci



subroutine init_qcattrvars(this)

type(qcattrvars),intent(inout) :: this
integer :: i

this%btables(:) =qcattrvarsbtables
do i =1, nqcattrvars
  call init(this%vars(i),this%btables(i))
end do

end subroutine init_qcattrvars


type(qcattrvars) function  qcattrvars_new()

call init(qcattrvars_new)

end function qcattrvars_new


!> Remove data under the predefined grade of confidence.
!! If neither \a keep_attr nor \a delete_attr are passed, all the
!! attributes will be deleted after peeling; if \a keep_attr is
!! provided, only attributed listed in \a keep_attr will be kept in
!! output, (\a delete_attr will be ignored); if \a delete_attr is
!! provided, attributed listed in \a delete_attr will be deleted from
!! output.
SUBROUTINE vol7d_peeling(this, data_id, keep_attr, delete_attr, preserve, purgeana)
TYPE(vol7d),INTENT(INOUT)  :: this !< object that has to be peeled
integer,INTENT(inout),pointer,OPTIONAL :: data_id(:,:,:,:,:) !< data ID to use with dballe DB (for fast write of attributes)
CHARACTER(len=*),INTENT(in),OPTIONAL :: keep_attr(:) !< Btable of attributes that should be kept after removing data
CHARACTER(len=*),INTENT(in),OPTIONAL :: delete_attr(:) !< Btable of attributes that should be deleted after removing data
logical,intent(in),optional :: preserve !< preserve all attributes if true (alternative to keep_attr and delete_attr)
logical,intent(in),optional :: purgeana !< if true remove ana with all data missing

integer :: inddativar,inddatiattrinv,inddatiattrcli,inddatiattrtem,inddatiattrspa,inddativarattr
type(qcattrvars) :: attrvars
integer :: invi,clii,temi,spai
INTEGER(kind=int_b)  :: invb,clib,temb,spab

call init(attrvars)


if (associated(this%datiattr%b)) then
  inddatiattrinv = firsttrue(attrvars%vars(1) == this%datiattr%b) !indice attributo
  inddatiattrcli = firsttrue(attrvars%vars(2) == this%datiattr%b) !indice attributo
  inddatiattrtem = firsttrue(attrvars%vars(3) == this%datiattr%b) !indice attributo
  inddatiattrspa = firsttrue(attrvars%vars(4) == this%datiattr%b) !indice attributo

  invb=ibmiss
  clib=ibmiss
  temb=ibmiss
  spab=ibmiss

!!$  if (inddatiattrinv > 0) invb = this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattrinv)
!!$  if (inddatiattrcli > 0) clib = this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattrcli)
!!$  if (inddatiattrtem > 0) temb = this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattrtem)
!!$  if (inddatiattrspa > 0) spab = this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattrspa)


                                !byte attributes !

  if (c_e(invb) .or. c_e(clib) .or. c_e(temb) .or. c_e(spab) ) then  ! solo se c'� l'attributo

    if (associated(this%dativar%r)) then
      do inddativar=1,size(this%dativar%r)   ! per tutte le variabili reali
        inddativarattr  = this%dativar%r(inddativar)%b
        if (inddativarattr > 0) then         ! se la variabile ha quell'attributo (byte)
          this%voldatir(:,:,:,:,inddativar,:) = peeled(this%voldatir(:,:,:,:,inddativar,:), &
           invb,clib,temb,spab)
        end if
      end do
    endif

    if (associated(this%dativar%d)) then
      do inddativar=1,size(this%dativar%d)
        inddativarattr  = this%dativar%d(inddativar)%b
        if (inddativarattr > 0) then
          this%voldatid(:,:,:,:,inddativar,:) = peeled(this%voldatid(:,:,:,:,inddativar,:), &
           invb,clib,temb,spab)
        end if
      end do
    endif

    if (associated(this%dativar%i)) then
      do inddativar=1,size(this%dativar%i)
        inddativarattr  = this%dativar%i(inddativar)%b
        if (inddativarattr > 0) then
          this%voldatii(:,:,:,:,inddativar,:) = peeled(this%voldatii(:,:,:,:,inddativar,:), &
           invb,clib,temb,spab)
        end if
      end do
    endif

    if (associated(this%dativar%b)) then
      do inddativar=1,size(this%dativar%b)
        inddativarattr  = this%dativar%b(inddativar)%b
        if (inddativarattr > 0) then
          this%voldatib(:,:,:,:,inddativar,:) = peeled(this%voldatib(:,:,:,:,inddativar,:), &
           invb,clib,temb,spab)
        end if
      end do
    endif

    if (associated(this%dativar%c)) then
      do inddativar=1,size(this%dativar%c)
        inddativarattr  = this%dativar%c(inddativar)%b
        if (inddativarattr > 0) then
          this%voldatic(:,:,:,:,inddativar,:) = peeled(this%voldatic(:,:,:,:,inddativar,:), &
           invb,clib,temb,spab)
        end if
      end do
    endif

  end if
end if

if (associated(this%datiattr%i)) then

  inddatiattrinv = firsttrue(attrvars%vars(1) == this%datiattr%i) !indice attributo
  inddatiattrcli = firsttrue(attrvars%vars(2) == this%datiattr%i) !indice attributo
  inddatiattrtem = firsttrue(attrvars%vars(3) == this%datiattr%i) !indice attributo
  inddatiattrspa = firsttrue(attrvars%vars(4) == this%datiattr%i) !indice attributo

  invi=imiss
  clii=imiss
  temi=imiss
  spai=imiss

!!$  if (inddatiattrinv > 0) invi = this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattrinv)
!!$  if (inddatiattrcli > 0) clii = this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattrcli)
!!$  if (inddatiattrtem > 0) temi = this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattrtem)
!!$  if (inddatiattrspa > 0) spai = this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattrspa)



                                !integer attributes !

  if (c_e(invi) .or. c_e(clii) .or. c_e(temi) .or. c_e(spai)) then  ! solo se c'� l'attributo

    if (associated(this%dativar%r)) then
      do inddativar=1,size(this%dativar%r)   ! per tutte le variabili reali
        inddativarattr  = this%dativar%r(inddativar)%i
        if (inddativarattr > 0) then         ! se la variabile ha quell'attributo (integer)
          this%voldatir(:,:,:,:,inddativar,:) = peeled(this%voldatir(:,:,:,:,inddativar,:), &
           invi,clii,temi,spai)
        end if
      end do
    endif

    if (associated(this%dativar%d)) then
      do inddativar=1,size(this%dativar%d)
        inddativarattr  = this%dativar%d(inddativar)%i
        if (inddativarattr > 0) then
          this%voldatid(:,:,:,:,inddativar,:) = peeled(this%voldatid(:,:,:,:,inddativar,:), &
           invi,clii,temi,spai)
        end if
      end do
    endif

    if (associated(this%dativar%i)) then
      do inddativar=1,size(this%dativar%i)
        inddativarattr  = this%dativar%i(inddativar)%i
        if (inddativarattr > 0) then
          this%voldatii(:,:,:,:,inddativar,:) = peeled(this%voldatii(:,:,:,:,inddativar,:), &
           invi,clii,temi,spai)
        end if
      end do
    endif

    if (associated(this%dativar%b)) then
      do inddativar=1,size(this%dativar%b)
        inddativarattr  = this%dativar%b(inddativar)%i
        if (inddativarattr > 0) then
          this%voldatib(:,:,:,:,inddativar,:) = peeled(this%voldatib(:,:,:,:,inddativar,:), &
           invi,clii,temi,spai)
        end if
      end do
    endif

    if (associated(this%dativar%c)) then
      do inddativar=1,size(this%dativar%c)
        inddativarattr  = this%dativar%c(inddativar)%i
        if (inddativarattr > 0) then
          this%voldatic(:,:,:,:,inddativar,:) = peeled(this%voldatic(:,:,:,:,inddativar,:), &
           invi,clii,temi,spai)
        end if
      end do
    endif

  end if

end if

IF (.NOT.PRESENT(keep_attr) .AND. .NOT.PRESENT(delete_attr) .and. .not. optio_log(preserve)) THEN ! destroy all attributes
  IF (ASSOCIATED(this%voldatiattrr)) DEALLOCATE(this%voldatiattrr)
  IF (ASSOCIATED(this%voldatiattrd)) DEALLOCATE(this%voldatiattrd)
  IF (ASSOCIATED(this%voldatiattri)) DEALLOCATE(this%voldatiattri)
  IF (ASSOCIATED(this%voldatiattrb)) DEALLOCATE(this%voldatiattrb)
  IF (ASSOCIATED(this%voldatiattrc)) DEALLOCATE(this%voldatiattrc)

  CALL delete(this%datiattr)
  CALL delete(this%dativarattr)
END IF

IF (PRESENT(keep_attr)) THEN ! set to missing non requested attributes and reform

  if (optio_log(preserve)) call l4f_log(L4F_WARN,"preserve parameter ignored: keep_attr passed")
  CALL keep_var(this%datiattr%r)
  CALL keep_var(this%datiattr%d)
  CALL keep_var(this%datiattr%i)
  CALL keep_var(this%datiattr%b)
  CALL keep_var(this%datiattr%c)
  CALL qc_reform(this,data_id, miss=.TRUE., purgeana=purgeana)

ELSE IF (PRESENT(delete_attr)) THEN ! set to missing requested attributes and reform

  if (optio_log(preserve)) call l4f_log(L4F_WARN,"preserve parameter ignored: delete_attr passed")
  CALL delete_var(this%datiattr%r)
  CALL delete_var(this%datiattr%d)
  CALL delete_var(this%datiattr%i)
  CALL delete_var(this%datiattr%b)
  CALL delete_var(this%datiattr%c)
  CALL qc_reform(this,data_id, miss=.TRUE., purgeana=purgeana)

ELSE IF (PRESENT(purgeana)) THEN

  CALL qc_reform(this,data_id, purgeana=purgeana)

ENDIF


CONTAINS


!> Like vol7d_reform but manage data_id with less options
subroutine qc_reform(this,data_id,miss, purgeana)
TYPE(vol7d),INTENT(INOUT)  :: this !< object that has to be reformed
integer,INTENT(inout),pointer,OPTIONAL :: data_id(:,:,:,:,:) !< data ID to use with dballe DB (for fast write of attributes)
logical,intent(in),optional :: miss !< remove everithing related with missing position in description vector 
logical,intent(in),optional :: purgeana !< if true remove ana with all data missing

integer,pointer :: data_idtmp(:,:,:,:,:)
logical,allocatable :: llana(:)
integer,allocatable :: anaind(:)
integer :: i,j,nana

if (optio_log(purgeana)) then
  allocate(llana(size(this%ana)))
  llana =.false.
  do i =1,size(this%ana)
    if (associated(this%voldatii)) llana(i)= llana(i) .or. any(c_e(this%voldatii(i,:,:,:,:,:)))
    if (associated(this%voldatir)) llana(i)= llana(i) .or. any(c_e(this%voldatir(i,:,:,:,:,:)))
    if (associated(this%voldatid)) llana(i)= llana(i) .or. any(c_e(this%voldatid(i,:,:,:,:,:)))
    if (associated(this%voldatib)) llana(i)= llana(i) .or. any(c_e(this%voldatib(i,:,:,:,:,:)))
    if (associated(this%voldatic)) llana(i)= llana(i) .or. any(c_e(this%voldatic(i,:,:,:,:,:)))
  end do

  nana=count(llana)
  allocate(data_idtmp(nana,size(data_id,2),size(data_id,3),size(data_id,4),size(data_id,5)))

  allocate(anaind(nana))

  J=0
  do i=1,size(this%ana)
    if (llana(i)) then
      j=j+1
      anaind(j)=i
    end if
  end do

  data_idtmp=data_id(anaind,:,:,:,:)

  deallocate(data_id)
  data_id=>data_idtmp
  
  call vol7d_reform(this,miss=miss,lana=llana)

  deallocate(llana,anaind)

else

  call vol7d_reform(this,miss)

end if

end subroutine qc_reform


SUBROUTINE keep_var(var)
TYPE(vol7d_var),intent(inout),POINTER :: var(:)

INTEGER :: i

IF (ASSOCIATED(var)) THEN
  DO i = 1, SIZE(var)
    IF (ALL(var(i)%btable /= keep_attr(:))) THEN ! n.b. ALL((//)) = .TRUE.
      var(i)%btable = vol7d_var_miss%btable
    ENDIF
  ENDDO
ENDIF

END SUBROUTINE keep_var

SUBROUTINE delete_var(var)
TYPE(vol7d_var),intent(inout),POINTER :: var(:)

INTEGER :: i

IF (ASSOCIATED(var)) THEN
  DO i = 1, SIZE(var)
    IF (ANY(var(i)%btable == delete_attr(:))) THEN ! n.b. ANY((//)) = .FALSE.
      var(i) = vol7d_var_miss
    ENDIF
  ENDDO
ENDIF

END SUBROUTINE delete_var

END SUBROUTINE vol7d_peeling


end module modqc
