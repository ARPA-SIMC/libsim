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

implicit none

public

!> Definisce il livello di attendibilità per i dati validi
type :: qcpartype
  integer (kind=int_b):: att
end type qcpartype

!> Per dafault i dati con confidenza inferiore a 50 vengono scartati
type(qcpartype)  :: qcpar=qcpartype(50)


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
logical function vdi(flag)

integer  :: flag !< confidenza
      
if(flag < qcpar%att .and. c_e(flag))then
  vdi=.false.
else
  vdi=.true.
end if

return
end function vdi


!> Test di validità di dati byte

logical function vdb(flag)

integer (kind=int_b) :: flag !< confidenza
      
if(flag < qcpar%att .and. c_e(flag))then
  vdb=.false.
else
  vdb=.true.
end if

return
end function vdb


!> Test di dato invalidato intero
logical function invalidatedi(flag)

integer  :: flag !< attributo di invalidazione del dato
      
if(c_e(flag))then
  invalidatedi=.true.
else
  invalidatedi=.false.
end if

return
end function invalidatedi


!> Test di dato invalidato byte

logical function invalidatedb(flag)

integer (kind=int_b) :: flag !< attributo di invalidazione del dato
      
if(c_e(flag))then
  invalidatedb=.true.
else
  invalidatedb=.false.
end if

return
end function invalidatedb


end module modqc
