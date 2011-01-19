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


!> Utilità e definizioni per il controllo di qualità
!!\ingroup qc

module modqc

! Copyright (C) 2007

! Questo programma è software libero; è lecito ridistribuirlo e/o
! modificarlo secondo i termini della Licenza Pubblica Generica SMR come
! pubblicata da ARPA SMR ; o la versione 1 della licenza o (a scelta)
! una versione successiva.

! Questo programma è distribuito nella speranza che sia utile, ma SENZA
! ALCUNA GARANZIA; senza neppure la garanzia implicita di
! COMMERCIABILITÀ o di APPLICABILITÀ PER UN PARTICOLARE SCOPO. Si veda
! la Licenza Pubblica Generica SMR per avere maggiori dettagli.

! Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
! Generica SMR insieme a questo programma; in caso contrario, la si può
! ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA) Servizio
! Idro Meteorologico  (SIM), Viale Silvani 6, 40122 Bologna, Italia

use kinds
use missing_values
use optional_values
use vol7d_class

implicit none

private

public vd,init,qcattrvars_new,invalidated,peeled,vol7d_peeling
public nqcattrvars, qcattrvarsbtables

!> Definisce il livello di attendibilità per i dati validi
type :: qcpartype
  integer (kind=int_b):: att
end type qcpartype

!> Per dafault i dati con confidenza inferiore a 50 vengono scartati
type(qcpartype)  :: qcpar=qcpartype(50)

integer, parameter :: nqcattrvars=4
CHARACTER(len=10),parameter   ::qcattrvarsbtables(nqcattrvars)=(/"*B33196","*B33192","*B33193","*B33194"/)

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


!> Test di validità dei dati
interface vd
  module procedure vdi,vdb
end interface

!> Test di dato invalidato
interface invalidated
  module procedure invalidatedi,invalidatedb
end interface

contains

!> Test di validità di dati integer
elemental logical function vdi(flag)

integer,intent(in)  :: flag !< confidenza
      
if(flag < qcpar%att .and. c_e(flag))then
  vdi=.false.
else
  vdi=.true.
end if

return
end function vdi


!> Test di validità di dati byte

elemental logical function vdb(flag)

integer (kind=int_b),intent(in) :: flag !< confidenza
      
if(flag < qcpar%att .and. c_e(flag))then
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

! byte attributes below

elemental real function peeledrb(data,flaginv,flag1,flag2,flag3)

real, intent(in) :: data
integer(kind=int_b), intent(in),optional :: flaginv
integer(kind=int_b), intent(in),optional :: flag1
integer(kind=int_b), intent(in),optional :: flag2
integer(kind=int_b), intent(in),optional :: flag3

if (invalidated(optio_b(flaginv)) .and. vd(optio_b(flag1))  .and. vd(optio_b(flag2))  .and. vd(optio_b(flag3))) then 
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

if (invalidated(optio_b(flaginv)) .and. vd(optio_b(flag1))  .and. vd(optio_b(flag2))  .and. vd(optio_b(flag3))) then 
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

if (invalidated(optio_b(flaginv)) .and. vd(optio_b(flag1))  .and. vd(optio_b(flag2))  .and. vd(optio_b(flag3))) then 
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

if (invalidated(optio_b(flaginv)) .and. vd(optio_b(flag1))  .and. vd(optio_b(flag2))  .and. vd(optio_b(flag3))) then 
  peeledbb=data
else
  peeledbb=bmiss
end if

end function peeledbb


elemental character(len=vol7d_cdatalen) function peeledcb(data,flaginv,flag1,flag2,flag3)

character(len=vol7d_cdatalen), intent(in) :: data
integer(kind=int_b), intent(in),optional :: flaginv
integer(kind=int_b), intent(in),optional :: flag1
integer(kind=int_b), intent(in),optional :: flag2
integer(kind=int_b), intent(in),optional :: flag3

if (invalidated(optio_b(flaginv)) .and. vd(optio_b(flag1))  .and. vd(optio_b(flag2))  .and. vd(optio_b(flag3))) then 
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

if (invalidated(optio_l(flaginv)) .and. vd(optio_l(flag1))  .and. vd(optio_l(flag2))  .and. vd(optio_l(flag3))) then 
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

if (invalidated(optio_l(flaginv)) .and. vd(optio_l(flag1))  .and. vd(optio_l(flag2))  .and. vd(optio_l(flag3))) then 
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

if (invalidated(optio_l(flaginv)) .and. vd(optio_l(flag1))  .and. vd(optio_l(flag2))  .and. vd(optio_l(flag3))) then 
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

if (invalidated(optio_l(flaginv)) .and. vd(optio_l(flag1))  .and. vd(optio_l(flag2))  .and. vd(optio_l(flag3))) then 
  peeledbi=data
else
  peeledbi=bmiss
end if

end function peeledbi


elemental character(len=vol7d_cdatalen) function peeledci(data,flaginv,flag1,flag2,flag3)

character(len=vol7d_cdatalen), intent(in) :: data
integer, intent(in) :: flaginv
integer, intent(in),optional :: flag1
integer, intent(in),optional :: flag2
integer, intent(in),optional :: flag3

if (invalidated(optio_l(flaginv)) .and. vd(optio_l(flag1))  .and. vd(optio_l(flag2))  .and. vd(optio_l(flag3))) then 
  peeledci=data
else
  peeledci=cmiss
end if

end function peeledci



subroutine init_qcattrvars(this)

type(qcattrvars),intent(inout) :: this
integer :: i,j

this%btables(:) =qcattrvarsbtables
do i =1, nqcattrvars
  call init(this%vars(i),this%btables(i))
end do

end subroutine init_qcattrvars


type(qcattrvars) function  qcattrvars_new()

call init(qcattrvars_new)

end function qcattrvars_new


!> Remove data under a defined grade of confidence.
SUBROUTINE vol7d_peeling(this)
TYPE(vol7d),INTENT(INOUT)  :: this !< object to peeling

integer :: inddativar,inddatiattr,inddativarattr
integer :: indqcattrvars
type(qcattrvars) :: attrvars

call init(attrvars)

do indqcattrvars =1,nqcattrvars


  if (associated(this%datiattr%b)) then
    inddatiattr     = firsttrue(attrvars%vars(indqcattrvars)  == this%datiattr%b) !indice attributo

    !byte attributes !

    if (inddatiattr > 0) then  ! solo se c'è l'attributo

      do inddativar=1,size(this%dativar%r)   ! per tutte le variabili reali
        inddativarattr  = this%dativar%r(inddativar)%b
        if (inddativarattr > 0) then         ! se la variabile ha quell'attributo (byte)
          this%voldatir(:,:,:,:,inddativar,:) = peeled(this%voldatir(:,:,:,:,inddativar,:), &
           this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

      do inddativar=1,size(this%dativar%d)
        inddativarattr  = this%dativar%d(inddativar)%b
        if (inddativarattr > 0) then
          this%voldatid(:,:,:,:,inddativar,:) = peeled(this%voldatid(:,:,:,:,inddativar,:), &
           this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

      do inddativar=1,size(this%dativar%i)
        inddativarattr  = this%dativar%i(inddativar)%b
        if (inddativarattr > 0) then
          this%voldatii(:,:,:,:,inddativar,:) = peeled(this%voldatii(:,:,:,:,inddativar,:), &
           this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

      do inddativar=1,size(this%dativar%b)
        inddativarattr  = this%dativar%b(inddativar)%b
        if (inddativarattr > 0) then
          this%voldatib(:,:,:,:,inddativar,:) = peeled(this%voldatib(:,:,:,:,inddativar,:), &
           this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

      do inddativar=1,size(this%dativar%c)
        inddativarattr  = this%dativar%c(inddativar)%b
        if (inddativarattr > 0) then
          this%voldatic(:,:,:,:,inddativar,:) = peeled(this%voldatic(:,:,:,:,inddativar,:), &
           this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

    end if

    !integer attributes !

    if (inddatiattr > 0) then  ! solo se c'è l'attributo

      do inddativar=1,size(this%dativar%r)   ! per tutte le variabili reali
        inddativarattr  = this%dativar%r(inddativar)%i
        if (inddativarattr > 0) then         ! se la variabile ha quell'attributo (byte)
          this%voldatir(:,:,:,:,inddativar,:) = peeled(this%voldatir(:,:,:,:,inddativar,:), &
           this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

      do inddativar=1,size(this%dativar%d)
        inddativarattr  = this%dativar%d(inddativar)%i
        if (inddativarattr > 0) then
          this%voldatid(:,:,:,:,inddativar,:) = peeled(this%voldatid(:,:,:,:,inddativar,:), &
           this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

      do inddativar=1,size(this%dativar%i)
        inddativarattr  = this%dativar%i(inddativar)%i
        if (inddativarattr > 0) then
          this%voldatii(:,:,:,:,inddativar,:) = peeled(this%voldatii(:,:,:,:,inddativar,:), &
           this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

      do inddativar=1,size(this%dativar%b)
        inddativarattr  = this%dativar%b(inddativar)%i
        if (inddativarattr > 0) then
          this%voldatib(:,:,:,:,inddativar,:) = peeled(this%voldatib(:,:,:,:,inddativar,:), &
           this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

      do inddativar=1,size(this%dativar%c)
        inddativarattr  = this%dativar%c(inddativar)%i
        if (inddativarattr > 0) then
          this%voldatic(:,:,:,:,inddativar,:) = peeled(this%voldatic(:,:,:,:,inddativar,:), &
           this%voldatiattri(:,:,:,:,inddativarattr,:,inddatiattr))
        end if
      end do

    end if

  end if
end do

IF (ASSOCIATED(this%voldatiattrr)) DEALLOCATE(this%voldatiattrr)
IF (ASSOCIATED(this%voldatiattrd)) DEALLOCATE(this%voldatiattrd)
IF (ASSOCIATED(this%voldatiattri)) DEALLOCATE(this%voldatiattri)
IF (ASSOCIATED(this%voldatiattrb)) DEALLOCATE(this%voldatiattrb)
IF (ASSOCIATED(this%voldatiattrc)) DEALLOCATE(this%voldatiattrc)

CALL delete(this%datiattr)
CALL delete(this%dativarattr)

END SUBROUTINE vol7d_peeling


end module modqc
