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
!> \brief Estensione di vol7d_class per importare volumi di dati
!! dall'archivio Oracle del SIM.
!!
!! Questo modulo definisce gli oggetti e i metodi per importare
!! un volume di dati dall'archivio Oracle del SIM in un oggetto di tipo
!! vol7d_class::vol7d.
!!
!! \todo estrarre la maggior quantità di informazione possibile sulle
!! variabili direttamente da Oracle.
!!
!! \ingroup vol7d
MODULE vol7d_oraclesim_class
USE kinds
USE char_utilities
USE vol7d_class
USE file_utilities
IMPLICIT NONE

!> Definisce un'istanza di estrazione dall'archivio Oracle SIM.
!! Estende vol7d_class::vol7d aggiungendo le informazioni necessarie
!! all'estrazione. L'utente, pur potendo accedere a tutti i componenti
!! dell'oggetto, dovrà preoccuparsi del solo componente vol7d.
TYPE vol7d_oraclesim
  TYPE(vol7d) :: vol7d !< oggetto di tipo vol7d che conterrà i dati estratti
  INTEGER(kind=ptr_c) :: connid
END TYPE vol7d_oraclesim

TYPE ora_var_conv_static
  CHARACTER(len=10) :: varbt
  REAL :: afact, bfact
END TYPE ora_var_conv_static

TYPE ora_var_conv_db
  INTEGER :: varora ! inutile, eliminare a regime
  CHARACTER(len=20) :: unit
  TYPE(vol7d_level) :: level
  TYPE(vol7d_timerange) :: timerange
  CHARACTER(len=20) :: description
END TYPE ora_var_conv_db

TYPE attr_builder
  CHARACTER(len=10) :: btable ! codice B dell'attributo
  INTEGER :: vartype ! tipo, 1=intero, 2=character, standardizzare!
END TYPE attr_builder

INTEGER,EXTERNAL :: oraclesim_getnet, oraclesim_getdatahead, oraclesim_getdatavol, &
 oraclesim_getanahead, oraclesim_getanavol, &
 oraclesim_getvarhead, oraclesim_getvarvol
INTEGER(kind=ptr_c),EXTERNAL :: oraclesim_init

INTEGER,ALLOCATABLE ::stazo(:), varo(:), valid(:)
REAL,ALLOCATABLE :: valore1(:), valore2(:)
INTEGER(kind=int_b),ALLOCATABLE :: cdatao(:,:), cflag(:,:), valore3(:,:)
CHARACTER(len=12),ALLOCATABLE :: fdatao(:)
INTEGER :: nmax=0, nact=0, nvarmax=0
INTEGER,PARAMETER :: nmaxmin=100000, nmaxmax=5000000, oraclesim_netmax=50, &
 datelen=13, flaglen=10, cvallen=8
 
! tabelle nuove di conversione variabili da btable a oraclesim
TYPE(ora_var_conv_static),ALLOCATABLE :: vartable_s(:)
TYPE(ora_var_conv_db),ALLOCATABLE :: vartable_db(:)
! attributi di dati disponibili
TYPE(attr_builder) :: dataattr_builder(6) = (/ & ! types: rdibc
 attr_builder('*B33195', 3), &
 attr_builder('*B33192', 4), &
 attr_builder('*B33193', 4), &
 attr_builder('*B33194', 4), &
 attr_builder('*B33196', 4), &
 attr_builder('*B33197', 4) /)

! tabella reti e anagrafica
TYPE(vol7d) :: netana(oraclesim_netmax)
LOGICAL :: networktable(oraclesim_netmax) = .FALSE.
INTEGER, PARAMETER :: netana_nvarr=3, netana_nvari=1, netana_nvarc=2

PRIVATE
PUBLIC vol7d_oraclesim, init, delete, import!, oraclesim_netmax

!> Costruttore per la classe vol7d_oraclesim.
!! Deve essere richiamato 
!! per tutti gli oggetti di questo tipo definiti in un programma.
INTERFACE init
  MODULE PROCEDURE vol7d_oraclesim_init
END INTERFACE

!> Distruttore per la classe vol7d_oraclesim.
INTERFACE delete
  MODULE PROCEDURE vol7d_oraclesim_delete
END INTERFACE

!> Metodi di importazione dati dall'archivio Oracle.
INTERFACE import
  MODULE PROCEDURE vol7d_oraclesim_import, vol7d_oraclesim_importana
END INTERFACE

CONTAINS

!> Inizializza un oggetto di tipo vol7d_oraclesim.
!! L'inizializzazione include la connessione al db Oracle.
!! Alla prima chiamata in un programma, provvede anche ad estrarre
!! dal database le tabelle di conversione variabili.
!! Trattandosi di un'estensione di vol7d, provvede ad inizializzare
!! anche l'oggetto vol7d contenuto.
SUBROUTINE vol7d_oraclesim_init(this, time_definition, dsn, user, password, WRITE, wipe)
TYPE(vol7d_oraclesim),INTENT(out) :: this !< Oggetto da inizializzare
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< 0=time is reference time; 1=time is validity time
CHARACTER(len=*), INTENT(in),OPTIONAL :: dsn !< Nome del database, se non fornito usa il nome standard per l'archivio Oracle del SIM
CHARACTER(len=*), INTENT(in),OPTIONAL :: user !< Nome utente per il server Oracle, se non fornito usa il nome standard per l'archivio Oracle del SIM
CHARACTER(len=*), INTENT(in),OPTIONAL :: password !< Password per il server Oracle, se non fornito usa la password standard per l'archivio Oracle del SIM
LOGICAL,INTENT(in),OPTIONAL :: WRITE !< Non utilizzato, presente per compatibilità
LOGICAL,INTENT(in),OPTIONAL :: wipe !< Non utilizzato, presente per compatibilità

CHARACTER(len=32) :: ldsn, luser, lpassword

INTEGER :: err
INTEGER(kind=int_b) :: msg(256)

ldsn = 'metw'
luser = 'leggo'
lpassword = 'meteo'
IF (PRESENT(dsn)) THEN
  IF (c_e(dsn)) ldsn = dsn
ENDIF
IF (PRESENT(user)) THEN
  IF (c_e(user)) luser = user
ENDIF
IF (PRESENT(password)) THEN
  IF (c_e(password)) lpassword = password
ENDIF

this%connid = oraclesim_init(fchar_to_cstr(TRIM(luser)), &
 fchar_to_cstr(TRIM(lpassword)), fchar_to_cstr(TRIM(ldsn)), err)
IF (err /= 0) THEN
  CALL oraclesim_geterr(this%connid, msg)
  CALL oraclesim_delete(this%connid)
  CALL l4f_log(L4F_FATAL, TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF
CALL vol7d_oraclesim_alloc(nmaxmin)
IF (.NOT. ALLOCATED(vartable_db)) CALL vol7d_oraclesim_setup_conv_new(this)
nact = nact + 1 ! Tengo il conto delle istanze attive
CALL init(this%vol7d, time_definition)

END SUBROUTINE vol7d_oraclesim_init


!> Distrugge l'oggetto in maniera pulita.
!! La connessione al server Oracle viene chiusa.
!! Trattandosi di un'estensione di vol7d, provvede a distruggere
!! anche l'oggetto vol7d contenuto.
SUBROUTINE vol7d_oraclesim_delete(this)
TYPE(vol7d_oraclesim) :: this

CALL oraclesim_delete(this%connid)
CALL delete(this%vol7d)
nact = MAX(nact - 1, 0) ! Tengo il conto delle istanze attive
IF (nact == 0) THEN
  CALL vol7d_oraclesim_dealloc()
  DEALLOCATE(vartable_s)
  DEALLOCATE(vartable_db)
ENDIF

END SUBROUTINE vol7d_oraclesim_delete


!> Importa un volume vol7d dall'archivio Oracle SIM.
!! Tutti i descrittori vengono assegnati correttamente, compresa
!! l'anagrafica delle stazioni e gli attributi dei dati.  Attualmente
!! l'importazione crea un volume di dati reali
!! vol7d_class::vol7d::voldatir con le osservazioni richieste, un
!! eventuale volume di variabili di anagrafica intere, reali e/o
!! carattere vol7d_class::vol7d::volanai, vol7d_class::vol7d::volanar,
!! vol7d_class::vol7d::volanac se il parametro \a anavar viene fornito
!! e, infine, un eventuale volume di attributi dei dati, interi e/o
!! carattere vol7d_class::vol7d::voldatiattri,
!! vol7d_class::vol7d::voldatiattrc se il parametro \a attr viene
!! fornito.
!!
!! La rete è identificata dal nome e non più da un codice numerico; il
!! nome deve essere quello inserito nel db Oracle, senza distinzione
!! tra lettere maiuscole e minuscole. Per riferimento, ecco riportata
!! una tabella di conversione che potrebbe non essere completa:
!!
!! <TABLE><TR><TD>Id</TD><TD>Nome</TD><TD>Id</TD><TD>Nome</TD>
!! <TD>Id</TD><TD>Nome</TD><TD>Id</TD><TD>Nome</TD></TR>
!! <TR><TD> 1</TD><TD>SYNOP    </TD>
!! <TD> 2</TD><TD>TEMP     </TD>
!! <TD> 3</TD><TD>BOA      </TD>
!! <TD> 4</TD><TD>IDRMGI   </TD></TR>
!! <TR><TD> 5</TD><TD>CER      </TD>
!! <TD> 6</TD><TD>UMSUOL   </TD>
!! <TD> 7</TD><TD>CRO      </TD>
!! <TD> 8</TD><TD>CAELAMI  </TD></TR>
!! <TR><TD> 9</TD><TD>ETG      </TD>
!! <TD>10</TD><TD>METAR    </TD>
!! <TD>11</TD><TD>LOCALI   </TD>
!! <TD>12</TD><TD>FIDUTO   </TD></TR>
!! <TR><TD>13</TD><TD>AGRMET   </TD>
!! <TD>14</TD><TD>POLLINI  </TD>
!! <TD>15</TD><TD>URBANE   </TD>
!! <TD>16</TD><TD>NURBAN   </TD></TR>
!! <TR><TD>17</TD><TD>FIDUMA   </TD>
!! <TD>18</TD><TD>FIDUPO   </TD>
!! <TD>19</TD><TD>ICIRFE   </TD>
!! <TD>20</TD><TD>SIMNBO   </TD></TR>
!! <TR><TD>21</TD><TD>SIMNPR   </TD>
!! <TD>22</TD><TD>SPDSRA   </TD>
!! <TD>23</TD><TD>EMLOKM   </TD>
!! <TD>24</TD><TD>LILOKM   </TD></TR>
!! <TR><TD>25</TD><TD>PILOKM   </TD>
!! <TD>26</TD><TD>TRLOKM   </TD>
!! <TD>27</TD><TD>VELOKM   </TD>
!! <TD>28</TD><TD>SALOKM   </TD></TR>
!! <TR><TD>29</TD><TD>LOLOKM   </TD>
!! <TD>30</TD><TD>MALOKM   </TD>
!! <TD>31</TD><TD>FRLOKM   </TD>
!! <TD>32</TD><TD>BZLOKM   </TD></TR>
!! <TR><TD>33</TD><TD>PO267    </TD>
!! <TD>35</TD><TD>CLIMAT   </TD>
!! <TD>36</TD><TD>GIAS     </TD>
!! <TD>37</TD><TD>IDROST   </TD></TR>
!! <TR><TD>38</TD><TD>VMSTAT   </TD>
!! <TD>39</TD><TD>IDRMEC   </TD>
!! <TD>40</TD><TD>CLINUR   </TD>
!! <TD>41</TD><TD>IDRSTA   </TD></TR>
!! <TR><TD>42</TD><TD>SYREP    </TD>
!! <TD>43</TD><TD>FREATI   </TD>
!! <TD>44</TD><TD>COREMO   </TD>
!! <TD>45</TD><TD>IDRTL9   </TD></TR>
!! <TR><TD>46</TD><TD>FIDUVE   </TD>
!! <TD>47</TD><TD>FIDULI   </TD>
!! </TABLE>
!!
!! Le variabili di anagrafica attualmente disponibili sono:
!!  - 'B07030' station height (reale)
!!  - 'B07031' barometer height (reale)
!!  - 'B01192' Oracle station id (intero)
!!  - 'B01019' station name (carattere)
!!  - 'B01194' Report (network) mnemonic (carattere)
!!
!! Gli attributi di dati attualmente disponibili sono:
!!  - '*B33195' MeteoDB variable ID (intero)
!!  - '*B33192' Climatological and consistency check (byte)
!!  - '*B33193' Time consistency (byte)
!!  - '*B33194' Space consistency (byte)
!!  - '*B33196' Data has been invalidated (byte)
!!  - '*B33197' Manual replacement in substitution (byte)
!!
!! Non sono attualmente previsti attributi di anagrafica.
!!
!! Gestisce le flag di qualità SIM 'fase 0.1', cioè:
!!  - '1' dato invalidato manualmente -&gt; restituisce valore mancante
!!  - '2' dato sostituito manualmente -&gt; restituisce il dato originale
!!  - '3' dato sostituito su un intervallo diverso -&gt; restituisce valore mancante
!!
!! Nel caso non sia stato trovato nulla in archivio per i parametri
!! richiesti, il volume risultante è vuoto e quindi inutilizzabile;
!! per evitare errori fatali, controllare l'oggetto \a
!! vol7d_oraclesim_class::vol7d_oraclesim::vol7d con la funzione \a
!! c_e, se restituisce \a .FALSE. non deve essere usato.
SUBROUTINE vol7d_oraclesim_import(this, var, network, timei, timef, level, &
 timerange, anavar, attr, anaattr, set_network)
TYPE(vol7d_oraclesim),INTENT(inout) :: this !< oggetto in cui importare i dati
CHARACTER(len=*),INTENT(in) :: var(:) !< lista delle variabili da importare, codice alfanumerico della tabella B locale
TYPE(vol7d_network),INTENT(in) :: network(:) !< lista di reti da estrarre, inizializzata con il nome che ha nell'archivio Oracle SIM (irrilevante se maiuscolo o minuscolo)
TYPE(datetime),INTENT(in) :: timei !< istante iniziale delle osservazioni da estrarre (estremo incluso)
TYPE(datetime),INTENT(in) :: timef !< istante finale delle osservazioni da estrarre (estremo incluso)
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level !< estrae solo il livello verticale fornito, default=tutti
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange !< estrae solo i dati con intervallo temporale (es. istantaneo, cumulato, ecc.) analogo al timerange fornito, default=tutti
CHARACTER(len=*),INTENT(in),OPTIONAL :: anavar(:) !< lista delle variabili di anagrafica da importare, codice alfanumerico della tabella B locale, se non fornito non ne importa nessuna
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:) !< lista degli attributi delle variabili da importare, codice alfanumerico della tabella B locale, se non fornito non ne importa nessuno
CHARACTER(len=*),INTENT(in),OPTIONAL :: anaattr(:) !< lista degli attributi delle variabili di anagrafica da importare, codice alfanumerico della tabella B locale, se non fornito non ne importa nessuno
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network !< se fornito, collassa tutte le reti nell'unica rete indicata, eliminando le eventuali stazioni comuni a reti diverse

INTEGER :: i

DO i = 1, SIZE(network)
  CALL vol7d_oraclesim_importvvns(this, var, network(i), timei, timef, &
   level, timerange, anavar, attr, anaattr, set_network)
ENDDO

END SUBROUTINE vol7d_oraclesim_import


! Routine interna che fa la vera importazione, una rete alla volta,
! non documentata per non incasinare doxygen
SUBROUTINE vol7d_oraclesim_importvvns(this, var, network, timei, timef, level, &
 timerange, anavar, attr, anaattr, set_network)
TYPE(vol7d_oraclesim),INTENT(inout) :: this
CHARACTER(len=*),INTENT(in) :: var(:)
TYPE(vol7d_network),INTENT(in) :: network
TYPE(datetime),INTENT(in) :: timei
TYPE(datetime),INTENT(in) :: timef
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: anavar(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: anaattr(:)
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network

TYPE(vol7d) :: v7dtmp, v7dtmp2, v7dtmpana
TYPE(datetime) :: odatetime
INTEGER :: i, j, k, nvar, nobs, nobso, ntime, nana, nvout, nvin, nvbt, netid
CHARACTER(len=12),ALLOCATABLE :: tmtmp(:)
CHARACTER(len=12) :: datai, dataf
INTEGER,ALLOCATABLE :: anatmp(:), vartmp(:), mapdatao(:), mapstazo(:), varlist(:)
LOGICAL,ALLOCATABLE :: lana(:)
LOGICAL :: found, non_valid, lnon_valid, varbt_req(nvarmax)
INTEGER(kind=int_b) :: msg(256)
LOGICAL :: lanar(netana_nvarr), lanai(netana_nvari), lanac(netana_nvarc), &
 full_qcinfo
! per attributi
INTEGER :: attr_out_ind(SIZE(dataattr_builder)), nda_type(5)

CALL getval(timei, simpledate=datai)
CALL getval(timef, simpledate=dataf)

! chiedo ad oracle l'identificativo numerico della rete richiesta
netid = vol7d_oraclesim_get_netid(this, network)
IF (netid <= 0 .OR. netid >= oraclesim_netmax) RETURN

CALL l4f_log(L4F_INFO, 'in oraclesim_class rete: '//TRIM(network%name)// &
 ' id: '//TRIM(to_char(netid)))

! Importo l'anagrafica per la rete se necessario
CALL vol7d_oraclesim_ora_ana(this, netid)

! Conto le variabili da estrarre
varbt_req(:) = .FALSE.

DO nvin = 1, SIZE(var)
  found = .FALSE.
  DO nvbt = 1, nvarmax
    IF (vartable_s(nvbt)%varbt == var(nvin)) THEN

      IF (PRESENT(level))THEN
        IF (vartable_db(nvbt)%level /= level) CYCLE
      END IF

      IF (PRESENT(timerange))THEN
        IF (vartable_db(nvbt)%timerange /= timerange) CYCLE
      END IF

      found = .TRUE.
      varbt_req(nvbt) = .TRUE.
    ENDIF
  ENDDO
  IF (.NOT.found) CALL l4f_log(L4F_WARN, 'variabile '//TRIM(var(nvin))// &
   ' non trovata nelle tabelle, la ignoro')
ENDDO

nvar = COUNT(varbt_req)
IF (nvar == 0) THEN
  CALL l4f_log(L4F_WARN, 'nessuna delle variabili richieste '//TRIM(var(1))// &
   '... e` valida')
! messaggio da cancellare in futuro
  CALL l4f_log(L4F_WARN, 'attenzione che ora le variabili      B10061,B12001,B12003')
  CALL l4f_log(L4F_WARN, 'vanno sostituite rispettivamente con B10060,B12101,B12103')
  
  RETURN
ENDIF

ALLOCATE(varlist(nvar))
varlist = PACK((/(i,i=1,nvarmax)/), varbt_req)

CALL l4f_log(L4F_INFO, 'in oraclesim_class, nvar='//to_char(nvar))

! Controllo gli attributi richiesti
! inizializzo a 0 attributi
attr_out_ind(:) = 0
nda_type(:) = 0
! controllo cosa e` stato richiesto
IF (PRESENT(attr)) THEN
  DO j = 1, SIZE(dataattr_builder)
    DO i = 1, SIZE(attr)
      IF (attr(i) == dataattr_builder(j)%btable .OR. &
       attr(i) == dataattr_builder(j)%btable(2:) .OR. attr(i) == '*') THEN
        nda_type(dataattr_builder(j)%vartype) = &
         nda_type(dataattr_builder(j)%vartype) + 1
        attr_out_ind(j) = nda_type(dataattr_builder(j)%vartype)
        EXIT
      ENDIF
    ENDDO
  ENDDO
ENDIF
! B33192, B33196, B33197 allow full qc, update when necessary
full_qcinfo = (attr_out_ind(2) /= 0) .AND. (attr_out_ind(5) /= 0) .AND. &
 (attr_out_ind(6) /= 0)

! Comincio l'estrazione
nobs = oraclesim_getdatahead(this%connid, fchar_to_cstr(datai), &
 fchar_to_cstr(dataf), netid, varlist, SIZE(varlist))
IF (nobs < 0) THEN
  CALL oraclesim_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraclesim_getdatahead, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF
CALL l4f_log(L4F_INFO, 'in oraclesim_getdatahead, nobs='//TRIM(to_char(nobs)))

CALL vol7d_oraclesim_alloc(nobs) ! Mi assicuro di avere spazio
i = oraclesim_getdatavol(this%connid, nobs, nobso, cdatao, stazo, varo, valore1, &
 valore2, valore3, cflag, rmiss)
IF (i /= 0) THEN
  CALL oraclesim_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraclesim_getdatavol, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF

nobs = nobso ! sbagliato? ne prende comunque nobs!? forse MIN(nobs, nobso)?
DO i = 1, nobs
  fdatao(i) = cstr_to_fchar(cdatao(:,i)) ! Converto la data da char C a CHARACTER
  CALL oraclesim_decode_value(valore1(i), valore2(i), valore3(:,i), cflag(:,i))
ENDDO

non_valid = .FALSE. ! ottimizzazione per la maggior parte dei casi
nana = count_distinct(stazo(1:nobs), back=.TRUE.)
ntime = count_distinct(fdatao(1:nobs), back=.TRUE.)
nvar = count_distinct(varo(1:nobs), back=.TRUE.)
ALLOCATE(anatmp(nana), tmtmp(ntime), vartmp(nvar), mapdatao(nobs), mapstazo(nobs))
anatmp(:) = pack_distinct(stazo(1:nobs), nana, back=.TRUE.)
CALL pack_distinct_c(fdatao(1:nobs), tmtmp, back=.TRUE.)
vartmp(:) = pack_distinct(varo(1:nobs), nvar, back=.TRUE.)
CALL l4f_log(L4F_INFO, 'in oraclesim_class onvar='//TRIM(to_char(nvar)))

DO i = 1, nana
  IF (.NOT. ANY(anatmp(i) == netana(netid)%volanai(:,1,1))) THEN
    non_valid = .TRUE.
    CALL l4f_log(L4F_WARN, 'stazione oraclesim '//TRIM(to_char(anatmp(i)))// &
     ' non trovata nell''anagrafica della rete '//TRIM(network%name)// &
     ', la ignoro')
    WHERE(stazo(1:nobs) == anatmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
ENDDO

! Praticamente inutile se mi fido di oracle
DO i = 1, ntime
 odatetime = datetime_new(simpledate=tmtmp(i))
  IF (odatetime < timei .OR. odatetime > timef) THEN
    non_valid = .TRUE.
    CALL l4f_log(L4F_WARN, 'data oraclesim '//tmtmp(i)//' inattesa, la ignoro')
    WHERE(fdatao(1:nobs) == tmtmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
ENDDO

! Praticamente inutile se mi fido di oracle
DO i = 1, nvar
  lnon_valid = .FALSE.
  IF (vartmp(i) < 1 .OR. vartmp(i) > nvarmax) THEN
    lnon_valid = .TRUE.
    CALL l4f_log(L4F_WARN, 'variabile oraclesim '//TRIM(to_char(vartmp(i)))// &
     ' fuori limiti, la ignoro')
  ELSE IF (.NOT. varbt_req(vartmp(i))) THEN
    lnon_valid = .TRUE.
    CALL l4f_log(L4F_WARN, 'variabile oraclesim '//TRIM(to_char(vartmp(i)))// &
     ' non richiesta, la ignoro')
  ENDIF
  IF (lnon_valid) THEN
    non_valid = .TRUE.
    WHERE(varo(1:nobs) == vartmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
  lnon_valid = .FALSE.
ENDDO

! ricreo gli elenchi solo se ci sono dati rigettati
IF (non_valid) THEN
  DEALLOCATE(anatmp, tmtmp, vartmp)
  WHERE (stazo(1:nobs) == 0) ! mal comune, mezzo gaudio
    fdatao(1:nobs) = ''
    varo(1:nobs) = 0
  END WHERE
  nana = count_distinct(stazo(1:nobs), back=.TRUE., mask=(stazo(1:nobs) /= 0))
  ntime = count_distinct(fdatao(1:nobs), back=.TRUE., mask=(stazo(1:nobs) /= 0))
  nvar = count_distinct(varo(1:nobs), back=.TRUE., mask=(stazo(1:nobs) /= 0))
  ALLOCATE(anatmp(nana), tmtmp(ntime), vartmp(nvar))
  anatmp(:) = pack_distinct(stazo(1:nobs), nana, back=.TRUE., &
   mask=(stazo(1:nobs) /= 0))
  CALL pack_distinct_c(fdatao(1:nobs), tmtmp, back=.TRUE., &
   mask=(stazo(1:nobs) /= 0))
  vartmp(:) = pack_distinct(varo(1:nobs), nvar, back=.TRUE., &
   mask=(stazo(1:nobs) /= 0))
! creo la mappatura
  mapstazo(:) = 0
  mapdatao(:) = 0
  mapstazo(1:nobs) = map_distinct(stazo(1:nobs), back=.TRUE., &
   mask=(stazo(1:nobs) /= 0))
  mapdatao(1:nobs) = map_distinct(fdatao(1:nobs), back=.TRUE., &
   mask=(stazo(1:nobs) /= 0))
ELSE
! creo la mappatura
  mapstazo(:) = map_distinct(stazo(1:nobs), back=.TRUE.)
  mapdatao(1:nobs) = map_distinct(fdatao(1:nobs), back=.TRUE.)
ENDIF

! ciclo sulle variabili per riempire vol7d
CALL init(v7dtmp2) ! nel caso di nvar/nobs = 0
DO i = 1, nvar
  CALL init(v7dtmp)

  CALL vol7d_alloc(v7dtmp, ntime=ntime, nana=nana, &
   nlevel=1, ntimerange=1, nnetwork=1, ndativarr=1, &
   ndatiattrr=nda_type(1), ndatiattrd=nda_type(2), &
   ndatiattri=nda_type(3), ndatiattrb=nda_type(4), &
   ndatiattrc=nda_type(5), &
   ndativarattrr=MIN(nda_type(1),1), ndativarattrd=MIN(nda_type(2),1), &
   ndativarattri=MIN(nda_type(3),1), ndativarattrb=MIN(nda_type(4),1), &
   ndativarattrc=MIN(nda_type(5),1)) ! per var/attr 0=.NOT.PRESENT()

  IF (i == 1) THEN ! la prima volta inizializzo i descrittori fissi
    IF (PRESENT(set_network)) THEN
      IF (set_network /= vol7d_network_miss) THEN
        v7dtmp%network(1) = set_network ! set to dummy network
      ELSE
        v7dtmp%network(1) = network
      ENDIF
    ELSE
      v7dtmp%network(1) = network
    ENDIF
    ALLOCATE(lana(SIZE(netana(netid)%ana)))
    lana = .FALSE.
    DO j = 1, nana
      k = INDEX(netana(netid)%volanai(:,1,1), anatmp(j))
      v7dtmp%ana(j) = netana(netid)%ana(k) ! attenzione ai puntatori
      lana(k) = .TRUE.
    ENDDO
! se sono richieste delle variabili di anagrafica
! copio il sottoinsieme di anagrafica che mi interessa in tmpana
! e lo fondo col volume appena creato
    IF (PRESENT(anavar)) THEN
! queste funzionano anche se SIZE(anavar) == 0 grazie al fatto che
! ANY(var(SIZE == 0)) = .FALSE.
      DO j = 1, SIZE(netana(netid)%anavar%r)
        lanar(j) = ANY(netana(netid)%anavar%r(j)%btable == anavar)
      ENDDO
      DO j = 1, SIZE(netana(netid)%anavar%i)
        lanai(j) = ANY(netana(netid)%anavar%i(j)%btable == anavar)
      ENDDO
      DO j = 1, SIZE(netana(netid)%anavar%c)
        lanac(j) = ANY(netana(netid)%anavar%c(j)%btable == anavar)
      ENDDO
! qui copio il volume di anagrafica statico in un volume temporaneo
! per successiva rielaborazione; la rete e` sovrascritta per cui la
! rete del volume statico, inizializzata ad un valore "dummy", viene
! buttata, se in futuro non fosse piu` cosi` dovro` cambiare il
! codice in vol7d_oraclesim_ora_ana per inizializzare correttamente la
! rete
      CALL vol7d_copy(netana(netid), v7dtmpana, lana=lana, &
       lanavarr=lanar, lanavari=lanai, lanavarc=lanac)!, unique=.TRUE.)
      v7dtmpana%network(1) = v7dtmp%network(1) ! faccio coincidere la rete
! fondo v7dtmpana appena creato con v7dtmp
! qui faccio affidamenteo sul fatto che vol7d_merge conserva l'ordine
! del primo argomento (v7dtmp e v7dtmpana hanno la stessa anagrafica
! ma con un ordinamento in generale diverso)
      CALL vol7d_merge(v7dtmp, v7dtmpana)
    ENDIF
    DEALLOCATE(lana)

    DO j = 1, ntime
      v7dtmp%time(j) = datetime_new(simpledate=tmtmp(j))
    ENDDO
  ELSE ! successivamente li copio da quelli precedenti
    v7dtmp%time = v7dtmp2%time
    v7dtmp%ana = v7dtmp2%ana
    v7dtmp%network = v7dtmp2%network
  ENDIF
  CALL init(v7dtmp%dativar%r(1), vartable_s(vartmp(i))%varbt, &
   unit=vartable_db(vartmp(i))%unit)
  v7dtmp%level(1) = vartable_db(vartmp(i))%level
  v7dtmp%timerange(1) = vartable_db(vartmp(i))%timerange

! Copio la variabile per gli attributi
  IF (ASSOCIATED(v7dtmp%dativarattr%r)) &
   v7dtmp%dativarattr%r(1) = v7dtmp%dativar%r(1)
  IF (ASSOCIATED(v7dtmp%dativarattr%d)) &
   v7dtmp%dativarattr%d(1) = v7dtmp%dativar%r(1)
  IF (ASSOCIATED(v7dtmp%dativarattr%i)) &
   v7dtmp%dativarattr%i(1) = v7dtmp%dativar%r(1)
  IF (ASSOCIATED(v7dtmp%dativarattr%b)) &
   v7dtmp%dativarattr%b(1) = v7dtmp%dativar%r(1)
  IF (ASSOCIATED(v7dtmp%dativarattr%c)) &
   v7dtmp%dativarattr%c(1) = v7dtmp%dativar%r(1)

! Creo le variabili degli attributi
  DO j = 1, SIZE(dataattr_builder)
    IF (attr_out_ind(j) > 0) THEN
      IF (dataattr_builder(j)%vartype == 1) THEN
        CALL init(v7dtmp%datiattr%r(attr_out_ind(j)), &
         dataattr_builder(j)%btable, unit='NUMERIC', scalefactor=0)
      ELSE IF (dataattr_builder(j)%vartype == 2) THEN
        CALL init(v7dtmp%datiattr%d(attr_out_ind(j)), &
         dataattr_builder(j)%btable, unit='NUMERIC', scalefactor=0)
      ELSE IF (dataattr_builder(j)%vartype == 3) THEN
        CALL init(v7dtmp%datiattr%i(attr_out_ind(j)), &
         dataattr_builder(j)%btable, unit='NUMERIC', scalefactor=0)
      ELSE IF (dataattr_builder(j)%vartype == 4) THEN
        CALL init(v7dtmp%datiattr%b(attr_out_ind(j)), &
         dataattr_builder(j)%btable, unit='NUMERIC', scalefactor=0)
      ELSE IF (dataattr_builder(j)%vartype == 5) THEN
        CALL init(v7dtmp%datiattr%c(attr_out_ind(j)), &
         dataattr_builder(j)%btable, unit='CCITTIA5', scalefactor=0)
      ENDIF
    ENDIF
  ENDDO

! Alloco e riempio il volume di dati
  CALL vol7d_alloc_vol(v7dtmp, inivol=.TRUE.)
!  v7dtmp%voldatir(:,:,:,:,:,:) = rmiss ! da eliminare, grazie a inivol=.TRUE.
  DO j = 1, nobs
! Solo la variabile corrente e, implicitamente, dato non scartato
    IF (varo(j) /= vartmp(i)) CYCLE
    v7dtmp%voldatir(mapstazo(j),mapdatao(j),1,1,1,1) = &
     valore1(j)*vartable_s(vartmp(i))%afact+vartable_s(vartmp(i))%bfact
  ENDDO
! Imposto gli attributi richiesti
  IF (attr_out_ind(1) > 0) THEN ! variable id
    DO j = 1, nobs
! Solo la variabile corrente e, implicitamente, dato non scartato
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattri(mapstazo(j),mapdatao(j),1,1,1,1,attr_out_ind(1)) = varo(j)
    ENDDO
  ENDIF
  IF (attr_out_ind(2) > 0) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattrb(mapstazo(j),mapdatao(j),1,1,1,1,attr_out_ind(2)) = &
       make_qcflag_clim(cflag(:,j))
    ENDDO
  ENDIF
  IF (attr_out_ind(3) > 0) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattrb(mapstazo(j),mapdatao(j),1,1,1,1,attr_out_ind(3)) = &
       make_qcflag_time(cflag(:,j))
    ENDDO
  ENDIF
  IF (attr_out_ind(4) > 0) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattrb(mapstazo(j),mapdatao(j),1,1,1,1,attr_out_ind(4)) = &
       make_qcflag_space(cflag(:,j))
    ENDDO
  ENDIF
  IF (attr_out_ind(5) > 0) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattrb(mapstazo(j),mapdatao(j),1,1,1,1,attr_out_ind(5)) = &
       make_qcflag_inv(cflag(:,j))
    ENDDO
  ENDIF
  IF (attr_out_ind(6) > 0) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattrb(mapstazo(j),mapdatao(j),1,1,1,1,attr_out_ind(6)) = &
       make_qcflag_repl(cflag(:,j))
    ENDDO
  ENDIF

! Fondo il volume appena estratto con quello del ciclo precedente
  CALL vol7d_merge(v7dtmp2, v7dtmp, sort=.FALSE.)
ENDDO

! Fondo a sua volta tutto il volume estratto con il contenuto di this
CALL vol7d_merge(this%vol7d, v7dtmp2, sort=.FALSE.)
!CALL vol7d_reform(this%vol7d, sort=.TRUE., unique=.TRUE.)
CALL vol7d_smart_sort(this%vol7d, lsort_time=.TRUE.)
! Pulizie finali non incluse
DEALLOCATE(anatmp, tmtmp, vartmp, mapdatao, mapstazo)

END SUBROUTINE vol7d_oraclesim_importvvns


!> Importa un volume vol7d di anagrafica stazioni dall'archivio Oracle
!! SIM.  Viene assegnato il vettore di anagrafica delle reti richieste
!! e viene creato un eventuale volume di variabili di anagrafica
!! intere, reali e/o carattere vol7d_class::vol7d::volanai,
!! vol7d_class::vol7d::volanar, vol7d_class::vol7d::volanac se il
!! parametro \a anavar viene fornito. Le variabili di anagrafica
!! attualmente disponibili sono:
!!  - 'B07030' station height (reale)
!!  - 'B07031' barometer height (reale)
!!  - 'B01192' Oracle station id (intero)
!!  - 'B01019' station name (carattere)
!!  - 'B01194' Report (network) mnemonic (carattere)
!!
!! Nota: questo metodo importa l'anagrafica di tutte le stazioni delle
!! reti richieste, mentre il metodo import importa solamente
!! l'anagrafica delle stazioni per cui sono disponibili dati nel
!! periodo richiesto.
SUBROUTINE vol7d_oraclesim_importana(this, network, anavar, set_network)
TYPE(vol7d_oraclesim),INTENT(inout) :: this !< oggetto in cui importare l'anagrafica
TYPE(vol7d_network),INTENT(in) :: network(:) !< rete di cui estrarre l'anagrafica, inizializzata con il nome che ha nell'archivio SIM
CHARACTER(len=*),INTENT(in),OPTIONAL :: anavar(:) !< lista delle variabili di anagrafica da importare, codice alfanumerico della tabella B locale, se non fornito non ne importa nessuna
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network !< se fornito, collassa tutte le reti nell'unica rete indicata, eliminando le eventuali stazioni comuni a reti diverse

TYPE(vol7d) :: v7dtmpana
INTEGER :: i, j, nvout, netid
LOGICAL :: lanar(netana_nvarr), lanai(netana_nvari), lanac(netana_nvarc)


DO i = 1, SIZE (network)
! chiedo ad oracle l'identificativo numerico della rete richiesta
  netid = vol7d_oraclesim_get_netid(this, network(i))
  IF (netid <= 0 .OR. netid >= oraclesim_netmax) RETURN

! Importo l'anagrafica per la rete se necessario
  CALL vol7d_oraclesim_ora_ana(this, netid)

! se sono richieste delle variabili di anagrafica
! copio il sottoinsieme di anagrafica che mi interessa in tmpana
! e lo fondo col volume appena creato
  IF (PRESENT(anavar)) THEN
    DO j = 1, SIZE(netana(netid)%anavar%r)
      lanar(j) = ANY(netana(netid)%anavar%r(j)%btable == anavar)
    ENDDO
    DO j = 1, SIZE(netana(netid)%anavar%i)
      lanai(j) = ANY(netana(netid)%anavar%i(j)%btable == anavar)
    ENDDO
    DO j = 1, SIZE(netana(netid)%anavar%c)
      lanac(j) = ANY(netana(netid)%anavar%c(j)%btable == anavar)
    ENDDO
  ELSE
    lanar(:) = .FALSE.
    lanai(:) = .FALSE.
    lanac(:) = .FALSE.
  ENDIF
  CALL vol7d_copy(netana(netid), v7dtmpana, &
   lanavarr=lanar, lanavari=lanai, lanavarc=lanac)
! sovrascrivo la rete
  IF (PRESENT(set_network)) THEN
    IF (set_network /= vol7d_network_miss) THEN
      v7dtmpana%network(1) = set_network
    ELSE
      v7dtmpana%network(1) = network(i)
    ENDIF
  ELSE
    v7dtmpana%network(1) = network(i)
  ENDIF
! fondo v7dtmpana appena creato con v7dtmp
  CALL vol7d_merge(this%vol7d, v7dtmpana)
ENDDO

END SUBROUTINE vol7d_oraclesim_importana



!=================
! Routine private
!=================

! Alloca o rialloca i vettori di lavoro per le routine di accesso a oracle
SUBROUTINE vol7d_oraclesim_alloc(n)
INTEGER,INTENT(in) :: n

IF (nmax >= n) RETURN ! c'e' gia' posto sufficiente
IF (ALLOCATED(stazo)) DEALLOCATE(stazo, varo, valid, valore1, valore2, valore3, &
 cdatao, fdatao, cflag)
ALLOCATE(stazo(n), varo(n), valid(n), valore1(n), valore2(n), valore3(cvallen, n), &
 cdatao(datelen, n), fdatao(n), cflag(flaglen,n))

nmax = n

END SUBROUTINE vol7d_oraclesim_alloc


! Delloca i vettori di lavoro per le routine di accesso a oracle
! e le anagrafiche eventualemnte lette
SUBROUTINE vol7d_oraclesim_dealloc()

INTEGER :: i

DO i = 1, oraclesim_netmax
  IF (networktable(i)) THEN
    CALL delete(netana(i))
    networktable(i) = .FALSE.
  ENDIF
ENDDO
IF (ALLOCATED(stazo)) DEALLOCATE(stazo, varo, valid, valore1, valore2, valore3, &
 cdatao, fdatao, cflag)
nmax = 0

END SUBROUTINE vol7d_oraclesim_dealloc


! Legge la tabella di conversione per le variabili
SUBROUTINE vol7d_oraclesim_setup_conv_new(this)
TYPE(vol7d_oraclesim), INTENT(inout) :: this

INTEGER :: nv, nvo, i
INTEGER(kind=int_b) :: msg(256)
INTEGER, ALLOCATABLE :: identnr(:), lt1(:), l1(:), lt2(:), l2(:), &
 pind(:), p1(:), p2(:)

! make it repeatable: allocate and statically assign only once
IF (.NOT.ALLOCATED(vartable_s)) THEN
! file generato dalla script data/libsim_gen_varconv_ora.py -f
  INCLUDE 'vol7d_oraclesim_class_vartable.F90'
ENDIF

nvarmax = SIZE(vartable_s)
IF (ALLOCATED(vartable_db)) DEALLOCATE(vartable_db)

nv = oraclesim_getvarhead(this%connid)
IF (nv < 0) THEN ! errore oracle
  CALL oraclesim_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraclesim_getvarhead, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF

ALLOCATE(identnr(nv), lt1(nv), l1(nv), lt2(nv), l2(nv), pind(nv), p1(nv), p2(nv))
identnr(:) = 0
i = oraclesim_getvarvol(this%connid, nv, nvo, identnr, &
 lt1, l1, lt2, l2, pind, p1, p2, imiss)

IF (i /= 0) THEN
  CALL oraclesim_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraclesim_getvarvol, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF

i = MAXVAL(identnr)
IF (i > nvarmax) THEN
  CALL l4f_log(L4F_WARN, 'in oraclesim, variables missing from static table: ' &
   //TRIM(to_char(nvarmax))//'/'//TRIM(to_char(i)))
  CALL l4f_log(L4F_WARN, 'you should update the software')
ENDIF

ALLOCATE(vartable_db(nvarmax))
vartable_db(:) = ora_var_conv_db(imiss, cmiss, vol7d_level_miss, &
 vol7d_timerange_miss, cmiss)

DO i = 1, SIZE(identnr)
  IF (identnr(i) > 0 .AND. identnr(i) <= nvarmax) THEN
    vartable_db(identnr(i)) = ora_var_conv_db(identnr(i), cmiss, &
     vol7d_level_new(lt1(i),l1(i),lt2(i),l2(i)), &
     vol7d_timerange_new(pind(i),p1(i),p2(i)), cmiss)
  ENDIF
ENDDO

DEALLOCATE(identnr, lt1, l1, lt2, l2, pind, p1, p2)

CALL l4f_log(L4F_INFO, 'in oraclesim '//TRIM(to_char(nvarmax))//' variables read')

END SUBROUTINE vol7d_oraclesim_setup_conv_new


! Importa l'anagrafica per la rete specificata
SUBROUTINE vol7d_oraclesim_ora_ana(this, netid)
TYPE(vol7d_oraclesim), INTENT(inout) :: this
INTEGER,INTENT(in) :: netid

INTEGER :: i, nana, vnana
REAL(kind=fp_geo),ALLOCATABLE :: tmpll(:,:)
INTEGER(kind=int_b),ALLOCATABLE :: tmpname(:,:), tmpnet(:,:)
INTEGER(kind=int_b) :: msg(256)
LOGICAL :: ismiss
INTEGER :: q1, q2! eliminare

IF (networktable(netid)) RETURN ! gia` fatto
networktable(netid) = .TRUE.
CALL init(netana(netid))
nana = oraclesim_getanahead(this%connid, netid)
IF (nana < 0) THEN ! errore oracle
  CALL oraclesim_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraclesim_getanahead, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF

ALLOCATE(tmpll(nana,2), tmpname(vol7d_cdatalen+1,nana), &
 tmpnet(vol7d_cdatalen+1,nana))
CALL vol7d_alloc(netana(netid), nnetwork=1, nana=nana, &
 nanavarr=netana_nvarr, nanavari=netana_nvari, nanavarc=netana_nvarc)
CALL vol7d_alloc_vol(netana(netid))

CALL init(netana(netid)%anavar%r(1), btable='B07030', unit='M') ! station height
CALL init(netana(netid)%anavar%r(2), btable='B07031', unit='M') ! barometer height
! la prossima riga dovra` essere eliminata ad avvenuta transizione a dballe 5.5
CALL init(netana(netid)%anavar%r(3), btable='B07001', unit='M') ! station height
CALL init(netana(netid)%anavar%i(1), btable='B01192', unit='NUMERIC', &
 scalefactor=0) ! Oracle station id
CALL init(netana(netid)%anavar%c(1), btable='B01019', unit='CCITTIA5', &
 scalefactor=0) ! station name
CALL init(netana(netid)%anavar%c(2), btable='B01194', unit='CCITTIA5', &
 scalefactor=0) ! network name

i = oraclesim_getanavol(this%connid, nana, vnana, vol7d_cdatalen+1, &
 netana(netid)%volanai(1,1,1), tmpll(1,1), tmpll(1,2), &
 netana(netid)%volanar(1,1,1), netana(netid)%volanar(1,2,1), &
 tmpname(1,1), tmpnet(1,1), rmiss, dmiss)
IF (i /= 0) THEN
  CALL oraclesim_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraclesim_getanavol, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF
! la prossima riga dovra` essere eliminata ad avvenuta transizione a dballe 5.5
! modificare congiuntamente anche netana_nvarr=2
netana(netid)%volanar(:,3,1) = netana(netid)%volanar(:,1,1)

ismiss = .FALSE.
DO i = 1, nana
! station
  netana(netid)%volanac(i,1,1) = cstr_to_fchar(tmpname(:,i))
  IF (.NOT. c_e(tmpll(i,1)) .OR. .NOT. c_e(tmpll(i,2))) THEN
    CALL init(netana(netid)%ana(i))
    ismiss = .TRUE.
  ELSE
    CALL init(netana(netid)%ana(i), lon=tmpll(i,1), lat=tmpll(i,2))
  ENDIF
  IF (netana(netid)%volanac(i,1,1) == 'S. Cresci in Valcava') THEN
    netana(netid)%volanac(i,1,1) = cmiss
    ismiss = .TRUE.
  ENDIF
! network
  netana(netid)%volanac(i,2,1) = cstr_to_fchar(tmpnet(:,i))
  IF (netana(netid)%volanac(i,2,1) == '') netana(netid)%volanac(i,2,1) = cmiss
  CALL init(netana(netid)%network(1), name=netana(netid)%volanac(i,2,1))
ENDDO

DEALLOCATE(tmpll, tmpname, tmpnet)

IF (ismiss) THEN
! eliminare eventualmente le stazioni mancanti con una vol7d_reform
  CALL l4f_log(L4F_WARN, 'l''anagrafica della rete '//TRIM(to_char(netid))// &
   ' in Oracle')
  CALL l4f_log(L4F_WARN, 'contiene stazioni con coordinate o nomi non validi')
  CALL l4f_log(L4F_WARN, 'avverti chi di dovere!')
ENDIF

CALL vol7d_reform(netana(netid), unique=.TRUE.) ! eliminate double stations

CALL l4f_log(L4F_INFO, 'ho estratto l''anagrafica di '//TRIM(to_char(nana))// &
 ' stazioni per la rete '//TRIM(to_char(netid)))

END SUBROUTINE vol7d_oraclesim_ora_ana


! Restituisce il codice numerico associato ad una rete
FUNCTION vol7d_oraclesim_get_netid(this, network) RESULT(netid)
TYPE(vol7d_oraclesim),INTENT(inout) :: this
TYPE(vol7d_network),INTENT(in) :: network

INTEGER :: netid

INTEGER(kind=int_b) :: msg(256)

netid = oraclesim_getnet(this%connid, fchar_to_cstr(uppercase(TRIM(network%name))))
IF (netid < 0) THEN ! errore oracle
  CALL oraclesim_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraclesim_getnet, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ELSE IF (netid == 0) THEN ! no_data probabilmente la rete non esiste
  CALL l4f_log(L4F_ERROR, 'in oraclesim rete '//TRIM(network%name)//' non trovata in db')
  RETURN
ELSE IF (netid >= oraclesim_netmax) THEN ! rete valida ma non prevista dal codice
  CALL l4f_log(L4F_ERROR, 'in oraclesim rete '//TRIM(network%name)//' trovata in db ma non ancora supportata')
  RETURN
ENDIF
CALL l4f_log(L4F_INFO, 'in oraclesim_class rete: '//TRIM(network%name)// &
 ' id: '//TRIM(to_char(netid)))

END FUNCTION vol7d_oraclesim_get_netid

! Funzioni per interpretare la flag di qualita` SIM
! CdQ climatologico
FUNCTION make_qcflag_clim(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER(kind=int_b) :: flag

flag = make_qcflag(simflag(2:3))

END FUNCTION make_qcflag_clim

! CdQ temporale
FUNCTION make_qcflag_time(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER(kind=int_b) :: flag

flag = make_qcflag(simflag(4:5))

END FUNCTION make_qcflag_time

! CdQ spaziale
FUNCTION make_qcflag_space(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER(kind=int_b) :: flag

flag = make_qcflag(simflag(6:7))

END FUNCTION make_qcflag_space

! Generica funzione per interpretare la flag di qualita`, 2 cifre in
! carattere, '00' equivale a flag assente
FUNCTION make_qcflag(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(2)
INTEGER(kind=int_b) :: flag

REAL :: rflag
! [48,54] => [-2,4] no! [100.,0.]
! 48 = bene, 53 = male
! 54 = fuori dai limiti del sensore
! 00 = missing
rflag = 100. - &
 (((simflag(1)-ICHAR('0'))*10 + simflag(2)-ICHAR('0')) - 48.)*100./6.
IF (rflag < 0. .OR. rflag > 100.) THEN
  flag = ibmiss
ELSE
  flag = NINT(rflag)
ENDIF

END FUNCTION make_qcflag

! Gestione flag di qualita`, per compatibilita` con quanto fatto da
! dballe, queste funzioni restituiscono 0 se la flag richiesta e`
! presente o dato mancante se essa e` assente.
! Dato invalidato manualmente
FUNCTION make_qcflag_inv(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER(kind=int_b) :: flag

IF (simflag(1) == ICHAR('1')) THEN
  flag = 0
ELSE
  flag = ibmiss
ENDIF

END FUNCTION make_qcflag_inv

! Dato modificato manualmente
FUNCTION make_qcflag_repl(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER(kind=int_b) :: flag

IF (simflag(1) == ICHAR('2')) THEN
  flag = 1 ! 1 the value has been substituted, 0 the value is the original value
ELSE
  flag = ibmiss
ENDIF

END FUNCTION make_qcflag_repl


SUBROUTINE oraclesim_decode_value(valore1, valore2, valore3, cflag)
REAL,INTENT(inout) :: valore1
REAL,INTENT(in) :: valore2
INTEGER(kind=int_b),INTENT(in) :: valore3(cvallen), cflag(flaglen)

CHARACTER(len=cvallen) :: cval
INTEGER :: v3

IF (cflag(1) == ICHAR('0') .OR. cflag(1) == ICHAR('1')) THEN
  IF (valore3(1) /= ICHAR(' ')) THEN
    cval = cstr_to_fchar(valore3)
    READ(v3,'(I8)',end=100,err=100)cval
    IF (v3 < 0) valore1 = rmiss
  ENDIF
ELSE IF (cflag(1) == ICHAR('2')) THEN
  valore1 = valore2
ELSE
  valore1 = rmiss
ENDIF

RETURN
! error in decoding valore3
100 valore1 = rmiss
RETURN

END SUBROUTINE oraclesim_decode_value


!SUBROUTINE oraclesim_decode_value_simple(valore1, valore2, valore3, cflag)
!REAL,INTENT(inout) :: valore1
!REAL,INTENT(in) :: valore2
!INTEGER(kind=int_b),INTENT(in) :: valore3(cvallen), cflag(flaglen)
!
!! simplified algorithm
!IF (cflag(1) == ICHAR('1')) THEN
!  valore1 = rmiss
!ELSE IF (cflag(1) == ICHAR('2')) THEN
!  valore1 = valore2
!ENDIF
!
!IF (make_qcflag_clim(cflag) < 100) valore1 = rmiss
!
!END SUBROUTINE oraclesim_decode_value_simple

END MODULE vol7d_oraclesim_class

