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

!> Definisce il livello di attendibilità per i dati validi
type :: qcpartype
  integer (kind=int_b):: att
end type qcpartype

!> Per dafault i dati con confidenza inferiore a 50 vengono scartati
type(qcpartype)  :: qcpar=qcpartype(50)

integer, parameter :: nqcattrvars=4

type :: qcattrvars
  TYPE(vol7d_var) :: vars(nqcattrvars)
  CHARACTER(len=10)   :: btables(nqcattrvars)
end type qcattrvars

!> Varables user in Quality Control
interface init
  module procedure init_qcattrvars
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

elemental real function peeled(data,flaginv,flag1,flag2,flag3)

real, intent(in) :: data
integer(kind=int_b), intent(in),optional :: flaginv
integer(kind=int_b), intent(in),optional :: flag1
integer(kind=int_b), intent(in),optional :: flag2
integer(kind=int_b), intent(in),optional :: flag3

if (invalidated(optio_b(flaginv)) .and. vd(optio_b(flag1))  .and. vd(optio_b(flag2))  .and. vd(optio_b(flag3))) then 
  peeled=data
else
  peeled=rmiss
end if

end function peeled


subroutine init_qcattrvars(this)

type(qcattrvars),intent(inout) :: this
integer :: i,j

this%btables(:) =(/"*B33196","*B33192","*B33193","*B33194"/)
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

integer :: inddativarr,inddatiattr,inddativarattr,i
integer :: indqcattrvars
type(qcattrvars) :: attrvars

call init(attrvars)

do indqcattrvars =1,nqcattrvars
  if (associated(this%datiattr%b)) then
    inddatiattr     = firsttrue(attrvars%vars(indqcattrvars)  == this%datiattr%b)

    if (inddatiattr > 0) then
      do inddativarr=1,size(this%dativar%r)
        inddativarattr  = firsttrue(this%dativar%r(inddativarr)   == this%dativarattr%b)
        
        this%voldatir(:,:,:,:,inddativarr,:) = peeled(this%voldatir(:,:,:,:,inddativarr,:), &
         this%voldatiattrb(:,:,:,:,inddativarattr,:,inddatiattr))
      end do
    end if

  end if
end do

!!$do indana=1,size(this%ana)
!!$  do  indnetwork=1,size(this%network)
!!$    do indlevel=1,size(this%level)
!!$      do indtimerange=1,size(this%timerange)
!!$        do inddativarr=1,size(this%dativar%r)
!!$          do indtime=1,size(this%time)
!!$
!!$            this%voldatir(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork) = &
!!$             peeled(this%voldatir(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork),&
!!$             this%voldatiattrb(indana,indtime,indlevel,indtimerange,inddativarr,indnetwork,indtbattrin))
!!$              
!!$          end do
!!$        end do
!!$      end do
!!$    end do
!!$  end do
!!$end do


END SUBROUTINE vol7d_peeling


end module modqc
