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
  !integer, parameter,private :: qcparatt=50

contains

    logical function vd(flag)

      integer (kind=int_b) :: flag
      
      if(flag < qcpar%att .and. c_e(flag))then
         vd=.false.
      else
         vd=.true.
      end if

      return
    end function vd


end module modqc
