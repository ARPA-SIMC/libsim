#!/bin/sh

# Script per creare l'anagrafica delle reti del DB Oracle SIM
# in un formato comprensibile dalla libsim.
# Eseguire libsim_gen_ana_ora.sh -h per vedere l'help.

query_reti()
# Funzione per la creazione dell'anagrafica delle reti
# presenti nell'archivio oracle ordinate per identificativo
# e descrittore di rete.
#
# Il primo parametro e' il nome file su cui scrivere l'uscita.
#---------------------------------------------------------------------
{
    cat > $sqlfile <<EOF
set lines 40
set pages 10000
set verify off
set feed off
set heading off
set trimspool on
spool $spoolfile
col IDENTNR            format 9999
col DESCRIZIONE        format A9
SELECT NVL(G.identnr,-9999) IDENTNR,
       SUBSTR(NVL(G.descrizione,'NULL'),1,9) DESCRIZIONE,
       g.id_tabella_vm
FROM MET_GRUPPI_STAZIONI G
WHERE G.IDENTNR = &&1;
exit

EOF

    ((id = 0))
    rm -f $retifile
    while [ $id -le 72 ] ;do
	((id = id + 1))

	sqlplus -S leggo/meteo@metw @$sqlfile $id
	grep -i . $spoolfile >> $1

    done
    rm -f $sqlfile $spoolfile
}

crea_anag_reti_simana()
# Funzione per la creazione dell'anagrafica delle stazioni
# presenti nell'archivio oracle per una singola rete.
#
# Il primo parametro e' l'identificativo numerico della rete;
# il secondo parametro e' il nome file su cui scrivere l'uscita.
# da chiarire se usare pm.z_quota_slm o st.z_quota_stazione
{
    cat > $sqlfile <<EOF
set lines 2000
set pages 10000
set verify off
set feed off
set heading off
set trimspool on
spool $spoolfile
col STAZID             format 99999
col YLAT               format 999.9999999
col XLONG              format 999.9999999
col QPOZZ              format 99999
col QSTAZ              format 99999
col MACROAREA          format A3
--col NOME               format A22
SELECT NVL(st.identnr,-9999) STAZID, 
       NVL(pm.y_lat_cent,-99.9999999) YLAT, NVL(pm.x_long_cent,-99.9999999) XLONG,
       NVL(st.z_quota_pozzetto,-9999) QPOZZ, NVL(st.z_quota_stazione,-9999) QSTAZ,
       '''' || RPAD(NVL(pm.macrobacino,'0'),1) || '''' MACROAREA,
       '''' || REPLACE(REPLACE(SUBSTR(NVL(st.nome,'NULL'),1,20),'''', ''''''), '"') || '''' NOME
  FROM met_stazioni_misura st,
       met_punti_misura pm
 WHERE st.gsta_identnr  = &&1 
   AND pm.identnr(+)    = st.pmis_identnr
ORDER by upper(st.identnr);
exit

EOF


    sqlplus -S leggo/meteo@metw @$sqlfile $1 >/dev/null
    grep -i . $spoolfile > $2
    if [ -f "$2" -a ! -s "$2" ]; then
	rm -f $2
    else
	echo "Creato il file di anagrafica $2"
    fi
    rm -f $sqlfile $spoolfile
}


usage()
{
    echo "Uso: $0 [<base>_<nnet>.<ext>]"
    echo "Se non viene fornito il parametro opzionale, estrae l'anagrafica"
    echo " di tutte le reti presenti nel DB Oracle SIM e le mette"
    echo " in file nominati net_<nnet>.simana ."
    echo "Se viene fornito il parametro opzionale, estrae l'anagrafica"
    echo " della sola rete <nnet>, che quindi deve avere un valore"
    echo " numerico, e la mette nel file <base>_<nnet>.<ext> ."
}


sqlfile=sql_query_$$.sql
spoolfile=sql_spool_$$.lst
retifile=reti_id_descr.txt

if [ "$#" -gt 1 -o "$1" = "-h" ]; then
    echo "Errore: sintassi non valida"
    usage
    exit 1
fi
if [ -n "$1" ]; then
    nnet=${1##*_}
    nnet=${nnet%%.*} # come accertarsi che nnet sia numerico???
    if [ -n "$nnet" -a "$nnet" != "$1" ]; then
	crea_anag_reti_simana $nnet $1 # anagrafica per la rete $nnet
	if [ ! -s "$1" ]; then # il file e' vuoto o mancante, errore
	    echo "Non ho trovato nessuna stazione per la rete $nnet"
	    exit 1
	fi
    else
	echo "Errore: nome $1 non valido"
	usage
	exit 1
    fi
else
    query_reti $retifile # creo il file delle reti presenti in oracle 
    while read campo1 campo2 campo3; do
	crea_anag_reti_simana $campo1 net_`printf "%03d" $campo1`.simana
    done < $retifile
fi


# Per riferimento futuro, ecco la chiamata piu' generale:
# set lines 2000
# set pages 10000
# set verify off
# set feed off
# set heading on
# set trimspool on
# spool MacroAree.spool
# col STAZID             format 99999
# col NOME               format A25
# col DESCRIZIONE        format A10
# col QSTAZ              format 99999
# col QPOZZ              format 99999
# col YLAT               format 999.9999999
# col XLONG              format 999.9999999
# col COMUNE             format A25
# col PROV               format A4
# col REGIONE            format A25
# col INIVAL             format A10
# col FINVAL             format A10
# col BACINO             format A32
# col SOTTOBACINO        format A25
# col MACROAREA          format A4
# select NVL(st.identnr,-9999) STAZID, substr(NVL(st.nome,'NULL'),1,25) NOME,
#        substr(NVL(gs.descrizione,'NULL'),1,9) DESCRIZIONE,
#        NVL(st.z_quota_stazione,-9999) QSTAZ, NVL(st.z_quota_pozzetto,-9999) QPOZZ,
#        NVL(pm.y_lat_cent,-99.9999999) YLAT, NVL(pm.x_long_cent,-99.9999999) XLONG,
#        substr(NVL(gc.nome,'NULL'),1,25) COMUNE,rpad(NVL(gp.sigla,'NULL'),4) PROV,
#        substr(NVL(gr.nome,'NULL'),1,25) REGIONE,
#        TO_CHAR(NVL(st.data_inizio,TO_DATE('09-09-9999','DD-MM-YYYY')),'DD-MM-YYYY') INIVAL,
#        TO_CHAR(NVL(st.data_fine,TO_DATE('09-09-9999','DD-MM-YYYY')),'DD-MM-YYYY') FINVAL,
#        substr(NVL(bc.des,'NULL'),1,32) BACINO,substr(NVL(sbc.des,'NULL'),1,25) SOTTOBACINO,
#        rpad(NVL(pm.macrobacino,'NULL'),4) MACROAREA
#   from met_gruppi_stazioni gs, met_stazioni_misura st,
#        met_punti_misura pm,geo_comuni gc,
#        geo_province gp, geo_regioni gr,
#        met_bacini bc, met_bacini sbc
#  where gs.frequenza    < 1440
#    and pm.macrobacino     = '&&1' 
#    and st.gsta_identnr = gs.identnr
#    and pm.identnr(+)   = st.pmis_identnr
#    and bc.cod(+)       = pm.bacino
#    and sbc.cod(+)      = pm.sottobacino
#    and gc.cod_istat(+) = pm.com_identnr
#    and gp.id_pro(+)    = pm.prv_identnr
#    and gr.id_reg(+)    = pm.reg_identnr
# order by upper(st.nome) ;
# exit
