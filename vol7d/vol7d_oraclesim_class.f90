!> \brief Estensione di vol7d_class per importare volumi di dati
!! dall'archivio Oracle del SIM.
!!
!! Questo modulo definisce gli oggetti e i metodi per importare
!! un volume di dati dall'archivio Oracle del SIM in un oggetto di tipo
!! vol7d_class::vol7d.
!!
!! Attenzione, dal 23/09/2009 circa, la rete è identificata dal nome e
!! non più da un codice numerico; il nome deve essere quello inserito
!! nel db Oracle ma, per il momento, senza distinzione tra lettere
!! maiuscole e minuscole. Per riferimento, ecco riportata una tabella
!! di conversione che potrebbe non essere completa:
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
  INTEGER :: ounit !< informazione di servizio
  INTEGER(kind=ptr_c) :: connid
END TYPE vol7d_oraclesim

TYPE ora_var_conv
  INTEGER :: varora
  CHARACTER(len=10) :: varbt
  CHARACTER(len=20) :: unit
  TYPE(vol7d_level) :: level
  TYPE(vol7d_timerange) :: timerange
  CHARACTER(len=20) :: description
  REAL :: afact, bfact
  INTEGER :: netid
END TYPE ora_var_conv

INTEGER,EXTERNAL :: oraextra_getnet, oraextra_gethead, oraextra_getdata, &
 oraextra_getanahead, oraextra_getanadata ! da sostituire con include/interface ?!
INTEGER(kind=ptr_c),EXTERNAL :: oraextra_init

INTEGER,ALLOCATABLE ::stazo(:), varo(:), valid(:)
REAL,ALLOCATABLE :: valore1(:), valore2(:)
INTEGER(kind=int_b),ALLOCATABLE :: cdatao(:,:), cflag(:,:)
!CHARACTER(len=1),ALLOCATABLE :: valore3(:)
CHARACTER(len=12),ALLOCATABLE :: fdatao(:)
INTEGER :: nmax=0, nact=0
INTEGER,PARAMETER :: nmaxmin=100000, nmaxmax=5000000, oraclesim_netmax=50, &
 datelen=13, flaglen=10
 
! tabella di conversione variabili da btable a oraclesim
TYPE(ora_var_conv),ALLOCATABLE :: vartable(:)
! tabella reti e anagrafica
TYPE(vol7d) :: netana(oraclesim_netmax)
LOGICAL :: networktable(oraclesim_netmax) = .FALSE.
INTEGER, PARAMETER :: netana_nvarr=2, netana_nvari=1, netana_nvarc=1

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
  MODULE PROCEDURE vol7d_oraclesim_import
END INTERFACE

CONTAINS

!> Inizializza un oggetto di tipo vol7doraclesim.
!! Trattandosi di un'estensione di vol7d, provvede ad inizializzare
!! anche l'oggetto vol7d contenuto.
!! Alla prima chiamata in un programma, provvede anche ad importare
!! le tabelle di conversione variabili dal file varmap.csv.
SUBROUTINE vol7d_oraclesim_init(this, time_definition, dsn, user, password, WRITE, wipe)
INTEGER,INTENT(IN),OPTIONAL :: time_definition !< 0=time is reference time; 1=time is validity time
TYPE(vol7d_oraclesim),INTENT(out) :: this !< Oggetto da inizializzare
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

this%connid = oraextra_init(fchar_to_cstr(TRIM(luser)), &
 fchar_to_cstr(TRIM(lpassword)), fchar_to_cstr(TRIM(ldsn)), err)
IF (err /= 0) THEN
  CALL oraextra_geterr(this%connid, msg)
  CALL oraextra_delete(this%connid)
  CALL l4f_log(L4F_FATAL, TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF
CALL vol7d_oraclesim_alloc(nmaxmin)
IF (.NOT. ALLOCATED(vartable)) CALL vol7d_oraclesim_setup_conv()
nact = nact + 1 ! Tengo il conto delle istanze attive
CALL init(this%vol7d, time_definition)

END SUBROUTINE vol7d_oraclesim_init


!> Distrugge l'oggetto in maniera pulita.
!! Trattandosi di un'estensione di vol7d, provvede a distruggere
!! anche l'oggetto vol7d contenuto.
SUBROUTINE vol7d_oraclesim_delete(this)
TYPE(vol7d_oraclesim) :: this

CALL oraextra_delete(this%connid)
CALL delete(this%vol7d)
nact = MAX(nact - 1, 0) ! Tengo il conto delle istanze attive
IF (nact == 0) THEN
  CALL vol7d_oraclesim_dealloc()
  DEALLOCATE(vartable)
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
!! Le variabili di anagrafica attualmente disponibili sono:
!!  - 'B07001' station height (reale)
!!  - 'B07031' barometer height (reale)
!!  - 'B01192' Oracle station id (intero)
!!  - 'B01019' station name (carattere)
!!
!! Gli attributi di dati attualmente disponibili sono:
!!  - 'B01193' Report (network) code (intero)
!!  - 'B01194' Report (network) mnemonic (carattere)
!!  - 'B33192' Climatological and consistency check (intero)
!!  - 'B33192' Time consistency (intero)
!!  - 'B33192' Space consistency (intero)
!!  - 'B33195' MeteoDB variable ID (intero)
!!  - 'B33196' Data has been invalidated (intero)
!!  - 'B33197' Manual replacement in substitution (intero)
!!
!! Non sono attualmente previsti attributi di anagrafica.
!!
!! Gestisce le flag di qualità SIM 'fase 0.1', cioè:
!!  - '1' dato invalidato manualmente -&gt; restituisce valore mancante
!!  - '2' dato sostituito manualmente -&gt; restituisce il dato sostituito
!!  - '3' dato invalidato automaticamente -&gt; restituisce valore mancante
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
TYPE(vol7d_network),INTENT(in) :: network(:) !< lista di reti da estrarre, inizializzata con il nome che ha nell'archivio SIM
TYPE(datetime),INTENT(in) :: timei !< istante iniziale delle osservazioni da estrarre (estremo incluso)
TYPE(datetime),INTENT(in) :: timef !< istante finale delle osservazioni da estrarre (estremo incluso)
TYPE(vol7d_level),INTENT(in),OPTIONAL :: level !< estrae solo il livello verticale fornito, default=tutti
TYPE(vol7d_timerange),INTENT(in),OPTIONAL :: timerange !< estrae solo i dati con intervallo temporale (es. istantaneo, cumulato, ecc.) analogo al timerange fornito, default=tutti
!> variabili da importare secondo la tabella B locale o relativi alias relative ad anagrafica
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
LOGICAL :: found, non_valid, varbt_req(SIZE(vartable))
INTEGER(kind=int_b) :: msg(256)
LOGICAL :: lanar(netana_nvarr), lanai(netana_nvari), lanac(netana_nvarc)
! per attributi
INTEGER :: ndai, ndac, attr_netid, attr_netname, attr_varid, attr_qcflag_clim, &
 attr_qcflag_time, attr_qcflag_space, attr_qcflag_inv, attr_qcflag_repl

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
  DO nvbt = 1, SIZE(vartable)
    IF (vartable(nvbt)%varbt == var(nvin) .AND. &
     vartable(nvbt)%netid == netid) THEN

      IF (PRESENT(level))THEN
        IF (vartable(nvbt)%level /= level) CYCLE
      END IF

      IF (PRESENT(timerange))THEN
        IF (vartable(nvbt)%timerange /= timerange) CYCLE
      END IF

      found = .TRUE.
      varbt_req(nvbt) = .TRUE.
    ENDIF
  ENDDO
  IF (.NOT.found) CALL l4f_log(L4F_WARN, 'variabile '//TRIM(var(nvin))// &
   ' non valida per la rete '//TRIM(network%name)//', la ignoro')
ENDDO

nvar = COUNT(varbt_req)
ALLOCATE(varlist(nvar))
varlist = PACK(vartable(:)%varora, varbt_req)
IF (nvar == 0) THEN
  CALL l4f_log(L4F_WARN, 'nessuna delle variabili '//TRIM(var(1))// &
   ' e` valida per la rete '//TRIM(network%name))
  RETURN
ENDIF
CALL l4f_log(L4F_INFO, 'in oraclesim_class, nvar='//to_char(nvar))

! Controllo gli attributi richiesti
! inizializzo a 0 attributi
attr_netid = imiss
attr_netname = imiss
attr_varid = imiss
attr_qcflag_clim = imiss
attr_qcflag_time = imiss
attr_qcflag_space = imiss
attr_qcflag_inv = imiss
attr_qcflag_repl = imiss
ndai = 0; ndac = 0
! controllo cosa e` stato richiesto
IF (PRESENT(attr)) THEN
  DO i = 1, SIZE(attr)
    IF (attr(i) == 'B01193') THEN
      ndai = ndai + 1
      attr_netid = ndai
    ELSE IF (attr(i) == 'B01194') THEN
      ndac = ndac + 1
      attr_netname = ndac
    ELSE IF (attr(i) == 'B33195') THEN
      ndai = ndai + 1
      attr_varid = ndai
    ELSE IF (attr(i) == 'B33192') THEN
      ndai = ndai + 1
      attr_qcflag_clim = ndai
    ELSE IF (attr(i) == 'B33193') THEN
      ndai = ndai + 1
      attr_qcflag_time = ndai
    ELSE IF (attr(i) == 'B33194') THEN
      ndai = ndai + 1
      attr_qcflag_space = ndai
    ELSE IF (attr(i) == 'B33196') THEN
      ndai = ndai + 1
      attr_qcflag_inv = ndai
    ELSE IF (attr(i) == 'B33197') THEN
      ndai = ndai + 1
      attr_qcflag_repl = ndai
    ELSE
      CALL l4f_log(L4F_WARN, 'attributo variabile oraclesim '//TRIM(attr(i))// &
       ' non valido, lo ignoro')
    ENDIF
  ENDDO
ENDIF

! Comincio l'estrazione
nobs = oraextra_gethead(this%connid, fchar_to_cstr(datai), &
 fchar_to_cstr(dataf), netid, varlist, SIZE(varlist))
IF (nobs < 0) THEN
  CALL oraextra_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraextra_gethead, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF
CALL l4f_log(L4F_INFO, 'in oraextra_gethead, nobs='//TRIM(to_char(nobs)))

CALL vol7d_oraclesim_alloc(nobs) ! Mi assicuro di avere spazio
i = oraextra_getdata(this%connid, nobs, nobso, cdatao, stazo, varo, valore1, &
 valore2, cflag, rmiss)
IF (i /= 0) THEN
  CALL oraextra_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraextra_getdata, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF

nobs = nobso
DO i = 1, nobs
  fdatao(i) = cstr_to_fchar(cdatao(:,i)) ! Converto la data da char C a CHARACTER
  IF (c_e(make_qcflag_inv(cflag(:,i))) .OR. c_e(make_qcflag_invaut(cflag(:,i)))) THEN
! dato invalidato manualmente o automaticamente
    valore1(i) = rmiss ! forzo dato mancante
  ELSE IF (c_e(make_qcflag_repl(cflag(:,i)))) THEN ! dato modificato manualmente
! il valore buono e` il secondo a meno che esso non sia mancante
! come nei casi indicati da vpavan@arpa.emr.it e-mail del 14/07/2008:
! ==
! variabile precipitazione o bagnatura fogliare. I dati originali
! (cumulate) saltano una mezzora, o piu` di una, ma il dato successivo al
! periodo mancante e` uguale all'ultimo dato buono. Se ne desume che non
! e` piovuto per tutto il periodo e il valore 0 viene immesso nel primo
! campo.
! ==
! in tal caso e` buono il primo
    IF (valore2(i) /= rmiss) valore1(i) = valore2(i)
  ENDIF

ENDDO
non_valid = .FALSE. ! ottimizzazione per la maggior parte dei casi
nana = count_distinct(stazo(1:nobs), back=.TRUE.)
ntime = count_distinct(fdatao(1:nobs), back=.TRUE.)
nvar = count_distinct(varo(1:nobs), back=.TRUE.)
ALLOCATE(anatmp(nana), tmtmp(ntime), vartmp(nvar))
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

! Praticamente inutile se mi fido della query
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

! Praticamente inutile se mi fido della query
DO i = 1, nvar
  IF (.NOT.ANY((vartmp(i) == vartable(:)%varora) .AND. varbt_req(:))) THEN
    non_valid = .TRUE.
    CALL l4f_log(L4F_WARN, 'variabile oraclesim '//TRIM(to_char(vartmp(i)))// &
     ' inattesa, la ignoro')
    WHERE(varo(1:nobs) == vartmp(i))
      stazo(1:nobs) = 0
    END WHERE
  ENDIF
ENDDO

! ricreo gli elenchi solo se ci sono dati rigettati
IF (non_valid) THEN
  DEALLOCATE(anatmp, tmtmp, vartmp)
  WHERE (stazo(1:nobs) == 0) ! mal comune, mezzo gaudio
    fdatao(1:nobs) = ''
    varo(1:nobs) = 0
  END WHERE
  nana = count_distinct(stazo(1:nobs), back=.TRUE., mask=(stazo(1:nobs) /= 0))
  ntime = count_distinct(fdatao(1:nobs), back=.TRUE., mask=(fdatao(1:nobs) /= ''))
  nvar = count_distinct(varo(1:nobs), back=.TRUE., mask=(varo(1:nobs) /= 0))
  ALLOCATE(anatmp(nana), tmtmp(ntime), vartmp(nvar))
  anatmp(:) = pack_distinct(stazo(1:nobs), nana, back=.TRUE., mask=(stazo(1:nobs) /= 0))
  CALL pack_distinct_c(fdatao(1:nobs), tmtmp, back=.TRUE., mask=(fdatao(1:nobs) /= ''))
  vartmp(:) = pack_distinct(varo(1:nobs), nvar, back=.TRUE., mask=(varo(1:nobs) /= 0))
ENDIF

! creo la mappatura
ALLOCATE(mapdatao(nobs), mapstazo(nobs))
DO i = 1, nana
  WHERE(stazo(1:nobs) == anatmp(i))
    mapstazo(1:nobs) = i
  END WHERE
ENDDO
DO i = 1, ntime
  WHERE(fdatao(1:nobs) == tmtmp(i))
    mapdatao(1:nobs) = i
  END WHERE
ENDDO
! ciclo sulle variabili per riempire vol7d
CALL init(v7dtmp2) ! nel caso di nvar/nobs = 0
DO i = 1, nvar
  CALL init(v7dtmp)

  CALL vol7d_alloc(v7dtmp, ntime=ntime, nana=nana, &
   nlevel=1, ntimerange=1, nnetwork=1, ndativarr=1, &
   ndatiattri=ndai, ndatiattrc=ndac, &
   ndativarattri=MIN(ndai,1), ndativarattrc=MIN(ndac,1)) ! per var/attr 0=.NOT.PRESENT()

  IF (i == 1) THEN ! la prima volta inizializzo i descrittori fissi
    IF (PRESENT(set_network)) THEN
      v7dtmp%network(1) = set_network ! dummy network
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
       lanavarr=lanar, lanavari=lanai, lanavarc=lanac)
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
  nvbt = INDEX(vartable(:)%varora, vartmp(i),  mask=varbt_req(:))
  CALL init(v7dtmp%dativar%r(1), vartable(nvbt)%varbt, unit=vartable(nvbt)%unit)
  v7dtmp%level(1) = vartable(nvbt)%level
  v7dtmp%timerange(1) = vartable(nvbt)%timerange

! Copio la variabile per gli attributi
  IF (ASSOCIATED(v7dtmp%dativarattr%i)) &
   v7dtmp%dativarattr%i(1) = v7dtmp%dativar%r(1)
  IF (ASSOCIATED(v7dtmp%dativarattr%c)) &
   v7dtmp%dativarattr%c(1) = v7dtmp%dativar%r(1)

! Creo le variabili degli attributi
  IF (c_e(attr_netid)) CALL init(v7dtmp%datiattr%i(attr_netid), 'B01193', &
   unit='NUMERIC', scalefactor=0)
  IF (c_e(attr_netname)) CALL init(v7dtmp%datiattr%c(attr_netname), 'B01194', &
   unit='CCITTIA5', scalefactor=0)
  IF (c_e(attr_varid)) CALL init(v7dtmp%datiattr%i(attr_varid), 'B33195', &
   unit='NUMERIC', scalefactor=0)
  IF (c_e(attr_qcflag_clim)) CALL init(v7dtmp%datiattr%i(attr_qcflag_clim), &
   'B33192', unit='NUMERIC', scalefactor=0)
  IF (c_e(attr_qcflag_time)) CALL init(v7dtmp%datiattr%i(attr_qcflag_time), &
   'B33193', unit='NUMERIC', scalefactor=0)
  IF (c_e(attr_qcflag_space)) CALL init(v7dtmp%datiattr%i(attr_qcflag_space), &
   'B33194', unit='NUMERIC', scalefactor=0)
  IF (c_e(attr_qcflag_inv)) CALL init(v7dtmp%datiattr%i(attr_qcflag_inv), &
   'B33196', unit='NUMERIC', scalefactor=0)
  IF (c_e(attr_qcflag_repl)) CALL init(v7dtmp%datiattr%i(attr_qcflag_repl), &
   'B33197', unit='NUMERIC', scalefactor=0)

! Alloco e riempio il volume di dati
  CALL vol7d_alloc_vol(v7dtmp, inivol=.TRUE.)
!  v7dtmp%voldatir(:,:,:,:,:,:) = rmiss ! da eliminare, grazie a inivol=.TRUE.
  DO j = 1, nobs
! Solo la variabile corrente e, implicitamente, dato non scartato
    IF (varo(j) /= vartmp(i)) CYCLE
    v7dtmp%voldatir(mapstazo(j),mapdatao(j),1,1,1,1) = &
     valore1(j)*vartable(nvbt)%afact+vartable(nvbt)%bfact
  ENDDO
! Imposto gli attributi richiesti
  IF (c_e(attr_netid)) THEN ! report code, alias network id
    v7dtmp%voldatiattri(:,:,1,1,1,1,attr_netid) = netid
  ENDIF
  IF (c_e(attr_netname)) THEN ! report mnemonic, alias network name
    v7dtmp%voldatiattrc(:,:,1,1,1,1,attr_netname) = network%name
  ENDIF
  IF (c_e(attr_varid)) THEN ! variable id
    DO j = 1, nobs
! Solo la variabile corrente e, implicitamente, dato non scartato
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattri(mapstazo(j),mapdatao(j),1,1,1,1,attr_varid) = varo(j)
    ENDDO
  ENDIF
  IF (c_e(attr_qcflag_clim)) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattri(mapstazo(j),mapdatao(j),1,1,1,1,attr_qcflag_clim) = &
       make_qcflag_clim(cflag(:,j))
    ENDDO
  ENDIF
  IF (c_e(attr_qcflag_time)) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattri(mapstazo(j),mapdatao(j),1,1,1,1,attr_qcflag_time) = &
       make_qcflag_time(cflag(:,j))
    ENDDO
  ENDIF
  IF (c_e(attr_qcflag_space)) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattri(mapstazo(j),mapdatao(j),1,1,1,1,attr_qcflag_space) = &
       make_qcflag_space(cflag(:,j))
    ENDDO
  ENDIF
  IF (c_e(attr_qcflag_inv)) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattri(mapstazo(j),mapdatao(j),1,1,1,1,attr_qcflag_inv) = &
       make_qcflag_inv(cflag(:,j))
    ENDDO
  ENDIF
  IF (c_e(attr_qcflag_repl)) THEN
    DO j = 1, nobs
      IF (varo(j) /= vartmp(i)) CYCLE
      v7dtmp%voldatiattri(mapstazo(j),mapdatao(j),1,1,1,1,attr_qcflag_repl) = &
       make_qcflag_repl(cflag(:,j))
    ENDDO
  ENDIF

! Fondo il volume appena estratto con quello del ciclo precedente
  CALL vol7d_merge(v7dtmp2, v7dtmp, sort=.FALSE.)
ENDDO

! Fondo a sua volta tutto il volume estratto con il contenuto di this
CALL vol7d_merge(this%vol7d, v7dtmp2, sort=.FALSE.)
CALL vol7d_smart_sort(this%vol7d, ltime=.TRUE.)
! Pulizie finali non incluse
DEALLOCATE(anatmp, tmtmp, vartmp, mapdatao, mapstazo)

END SUBROUTINE vol7d_oraclesim_importvvns


!> Importa un volume vol7d di anagrafica stazioni dall'archivio Oracle
!! SIM.  Viene assegnato il vettore di anagrafica delle reti richieste
!! e viene creato un eventuale volume di variabili di anagrafica
!! intere, reali e/o carattere vol7d_class::vol7d::volanai,
!! vol7d_class::vol7d::volanar, vol7d_class::vol7d::volanac se il
!! parametro \a anavar viene fornito; le variabili di anagrafica
!! attualmente disponibili sono:
!!  - 'B07001' station height (reale)
!!  - 'B07031' barometer height (reale)
!!  - 'B01192' Oracle station id (intero)
!!  - 'B01019' station name (carattere)
!!
!! Nota: questo metodo importa l'anagrafica di tutte le stazioni delle
!! reti richieste, mentre il metodo import importa solamente
!! l'anagrafica delle stazioni per cui sono disponibili dati nel
!! periodo richiesto.
!!
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
    v7dtmpana%network(1) = set_network
  ELSE
    v7dtmpana%network(1) = network(i)
  ENDIF
! fondo v7dtmpana appena creato con v7dtmp
  CALL vol7d_merge(v7dtmpana, this%vol7d)
ENDDO

END SUBROUTINE vol7d_oraclesim_importana



!=================
! Routine private
!=================

! Alloca o rialloca i vettori di lavoro per le routine di accesso a oracle
SUBROUTINE vol7d_oraclesim_alloc(n)
INTEGER,INTENT(in) :: n

IF (nmax >= n) RETURN ! c'e' gia' posto sufficiente
IF (ALLOCATED(stazo)) DEALLOCATE(stazo, varo, valid, valore1, valore2, &
 cdatao, fdatao, cflag)
ALLOCATE(stazo(n), varo(n), valid(n), valore1(n), valore2(n), &
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
IF (ALLOCATED(stazo)) DEALLOCATE(stazo, varo, valid, valore1, valore2, &
 cdatao, fdatao, cflag)
nmax = 0

END SUBROUTINE vol7d_oraclesim_dealloc


! Legge la tabella di conversione per le variabili
SUBROUTINE vol7d_oraclesim_setup_conv()
INTEGER,PARAMETER :: nf=16 ! formato file
INTEGER :: i, sep(nf), n1, n2, un, i1, i2, i3, i4
CHARACTER(len=512) :: line
CHARACTER(len=64) :: buf
TYPE(csv_record) :: csv

un = open_package_file('varmap.csv', filetype_data)
IF (un < 0) then
  CALL l4f_log(L4F_FATAL, 'in oraclesim, cannot find variable file')
  CALL raise_fatal_error()
END IF
i = 0
DO WHILE(.TRUE.)
  READ(un,'(A)',END=100)line
  i = i + 1
ENDDO
100 CONTINUE

IF (i > 0) THEN
  IF (ALLOCATED(vartable)) DEALLOCATE(vartable)
  ALLOCATE(vartable(i))
  REWIND(un)
  i = 0
  readline: DO WHILE(.TRUE.)
    READ(un,'(A)',END=120)line
    CALL init(csv, line)
    i = i + 1
    CALL csv_record_getfield(csv, vartable(i)%varora)
    CALL csv_record_getfield(csv, vartable(i)%varbt)
    CALL csv_record_getfield(csv, buf)
    vartable(i)%unit = buf ! uso buf per evitare un warning stringa troppo corta
    CALL csv_record_getfield(csv, i1)
    CALL csv_record_getfield(csv, i2)
    CALL csv_record_getfield(csv, i3)
    CALL csv_record_getfield(csv, i4)
    CALL init(vartable(i)%level, i1, i2, i3, i4)
    CALL csv_record_getfield(csv, i1)
    CALL csv_record_getfield(csv, i2)
    CALL csv_record_getfield(csv, i3)
    CALL init(vartable(i)%timerange, i1, i2, i3)
    CALL csv_record_getfield(csv)
    CALL csv_record_getfield(csv, buf)
    vartable(i)%description = buf ! uso buf per evitare un warning stringa troppo corta
    CALL csv_record_getfield(csv, vartable(i)%afact)
    CALL csv_record_getfield(csv, vartable(i)%bfact)
    CALL csv_record_getfield(csv, vartable(i)%netid)
    CALL delete(csv)
  ENDDO readline
120 CONTINUE

  CALL l4f_log(L4F_INFO, 'Ho letto '//TRIM(to_char(i))//' variabili dalla tabella')
ENDIF
CLOSE(un)

END SUBROUTINE vol7d_oraclesim_setup_conv


! Importa l'anagrafica per la rete specificata
SUBROUTINE vol7d_oraclesim_ora_ana(this, netid)
TYPE(vol7d_oraclesim), INTENT(inout) :: this
INTEGER,INTENT(in) :: netid

INTEGER :: i, nana, vnana
REAL(kind=fp_geo),ALLOCATABLE :: tmpll(:,:)
INTEGER(kind=int_b),ALLOCATABLE :: tmpname(:,:)
INTEGER(kind=int_b) :: msg(256)
LOGICAL :: ismiss
INTEGER :: q1, q2! eliminare

IF (networktable(netid)) RETURN ! gia` fatto
networktable(netid) = .TRUE.
CALL init(netana(netid))
nana = oraextra_getanahead(this%connid, netid)
IF (nana < 0) THEN ! errore oracle
  CALL oraextra_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraextra_getanahead, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF

ALLOCATE(tmpll(nana,2), tmpname(vol7d_cdatalen+1,nana))
CALL vol7d_alloc(netana(netid), nnetwork=1, nana=nana, &
 nanavarr=netana_nvarr, nanavari=netana_nvari, nanavarc=netana_nvarc)
CALL vol7d_alloc_vol(netana(netid))
! attenzione, in futuro potrebbe essere necessario inizializzare
! correttamente la rete nell'anagrafica statica
CALL init(netana(netid)%network(1), name='dummy')
CALL init(netana(netid)%anavar%r(1), btable='B07001', unit='M') ! station height
CALL init(netana(netid)%anavar%r(2), btable='B07031', unit='M') ! barometer height
CALL init(netana(netid)%anavar%i(1), btable='B01192', unit='NUMERIC', &
 scalefactor=0) ! Oracle station id
CALL init(netana(netid)%anavar%c(1), btable='B01019', unit='CCITTIA5', &
 scalefactor=0) ! station name

i = oraextra_getanadata(this%connid, nana, vnana, vol7d_cdatalen+1, &
 netana(netid)%volanai(1,1,1), tmpll(1,1), tmpll(1,2), &
 netana(netid)%volanar(1,1,1), netana(netid)%volanar(1,2,1), &
 tmpname(1,1), rmiss, dmiss)
IF (i /= 0) THEN
  CALL oraextra_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraextra_getanadata, '//TRIM(cstr_to_fchar(msg)))
  CALL raise_fatal_error()
ENDIF

ismiss = .FALSE.
DO i = 1, nana
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
ENDDO

DEALLOCATE(tmpll, tmpname)

IF (ismiss) THEN
! eliminare eventualmente le stazioni mancanti con una vol7d_reform
  CALL l4f_log(L4F_WARN, 'l''anagrafica della rete '//TRIM(to_char(netid))// &
   ' in Oracle')
  CALL l4f_log(L4F_WARN, 'contiene stazioni con coordinate o nomi non validi')
  CALL l4f_log(L4F_WARN, 'avverti chi di dovere!')
ENDIF

CALL l4f_log(L4F_INFO, 'ho estratto l''anagrafica di '//TRIM(to_char(nana))// &
 ' stazioni per la rete '//TRIM(to_char(netid)))

END SUBROUTINE vol7d_oraclesim_ora_ana


! Restituisce il codice numerico associato ad una rete
FUNCTION vol7d_oraclesim_get_netid(this, network) RESULT(netid)
TYPE(vol7d_oraclesim),INTENT(inout) :: this
TYPE(vol7d_network),INTENT(in) :: network

INTEGER :: netid

INTEGER(kind=int_b) :: msg(256)

netid = oraextra_getnet(this%connid, fchar_to_cstr(uppercase(TRIM(network%name))))
IF (netid < 0) THEN ! errore oracle
  CALL oraextra_geterr(this%connid, msg)
  CALL l4f_log(L4F_FATAL, 'in oraextra_getnet, '//TRIM(cstr_to_fchar(msg)))
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
INTEGER :: flag

flag = make_qcflag(simflag(2:3))

END FUNCTION make_qcflag_clim

! CdQ temporale
FUNCTION make_qcflag_time(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER :: flag

flag = make_qcflag(simflag(4:5))

END FUNCTION make_qcflag_time

! CdQ spaziale
FUNCTION make_qcflag_space(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER :: flag

flag = make_qcflag(simflag(6:7))

END FUNCTION make_qcflag_space

! Generica funzione per interpretare la flag di qualita`, 2 cifre in
! carattere, '00' equivale a flag assente
FUNCTION make_qcflag(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(2)
INTEGER :: flag

flag = (simflag(1)-ICHAR('0'))*10 + simflag(2)-ICHAR('0')
IF (flag <= 0 .OR. flag > 99) flag = imiss

END FUNCTION make_qcflag

! Gestione flag di qualita` fase 0.1, per compatibilita` con quanto
! fatto da dballe, queste funzioni restituiscono 0 se la flag
! richiesta e` presente o dato mancante se essa e` assente.
! Dato invalidato manualmente
FUNCTION make_qcflag_inv(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER :: flag

IF (simflag(1) == ICHAR('1')) THEN
  flag = 0
ELSE
  flag = imiss
ENDIF

END FUNCTION make_qcflag_inv

! Dato modificato manualmente
FUNCTION make_qcflag_repl(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER :: flag

IF (simflag(1) == ICHAR('2')) THEN
  flag = 0
ELSE
  flag = imiss
ENDIF

END FUNCTION make_qcflag_repl

! Dato invalidato automaticamente
FUNCTION make_qcflag_invaut(simflag) RESULT(flag)
INTEGER(kind=int_b) :: simflag(flaglen)
INTEGER :: flag

IF (simflag(1) == ICHAR('3')) THEN
  flag = 0
ELSE
  flag = imiss
ENDIF

END FUNCTION make_qcflag_invaut


END MODULE vol7d_oraclesim_class

