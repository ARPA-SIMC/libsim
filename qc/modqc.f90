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


type :: qcpartype
  integer (kind=int_b):: att
end type qcpartype

type(qcpartype)  :: qcpar=qcpartype(50)


interface vd
  module procedure vdi,vdb
end interface

interface invalidated
  module procedure invalidatedi,invalidatedb
end interface

contains

logical function vdi(flag)

integer  :: flag
      
if(flag < qcpar%att .and. c_e(flag))then
  vdi=.false.
else
  vdi=.true.
end if

return
end function vdi

logical function vdb(flag)

integer (kind=int_b) :: flag
      
if(flag < qcpar%att .and. c_e(flag))then
  vdb=.false.
else
  vdb=.true.
end if

return
end function vdb



logical function invalidatedi(flag)

integer  :: flag
      
if(c_e(flag))then
  invalidatedi=.true.
else
  invalidatedi=.false.
end if

return
end function invalidatedi

logical function invalidatedb(flag)

integer (kind=int_b) :: flag
      
if(c_e(flag))then
  invalidatedb=.true.
else
  invalidatedb=.false.
end if

return
end function invalidatedb


end module modqc
