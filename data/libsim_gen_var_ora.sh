#!/bin/sh

# Script per creare la tabella delle variabili del DB Oracle SIM
# in un formato comprensibile dalla libsim.
# Eseguire libsim_gen_var_ora.sh -h per vedere l'help.

usage()
{
    echo "Uso: $0"
    echo "Crea la tabella di conversione variabili da DB Oracle SIM"
    echo "a BTABLE per le applicazioni libsim che accedono a Oracle"
    echo "a partire da un template autocontenuto nella script"
    echo "e la mette in un file nominato varmap.csv."
}

sqlfile=sql_query_$$.sql
spoolfile=sql_spool_$$.lst
varmap=varmap.csv

if [ "$#" -gt 1 -o "$1" = "-h" ]; then
    echo "Errore: sintassi non valida"
    usage
    exit 1
fi

cat > $sqlfile <<EOF
set lines 40
set pages 10000
set verify off
set feed off
set heading off
-- SELECT V.IDENTNR --, V.DESCRIZIONE
-- FROM   MET_GRUPPI_STAZIONI G,
--        MET_CONFIGURAZIONI_TIPI_REPORT C,
--        MET_VARIABILI_DEFINITE V
-- WHERE  G.IDENTNR = &&1 AND
--        C.TIR_CODICE = G.DESCRIZIONE AND
--        V.IDENTNR = C.VARD_IDENTNR
-- ORDER BY V.IDENTNR;


SELECT G.IDENTNR
--, V.DESCRIZIONE, G.DESCRIZIONE
FROM   MET_GRUPPI_STAZIONI G,
       MET_CONFIGURAZIONI_TIPI_REPORT C,
       MET_VARIABILI_DEFINITE V
WHERE  G.IDENTNR = C.GSTA_IDENTNR AND
       C.TIR_CODICE = G.DESCRIZIONE AND
       C.VARD_IDENTNR = &&1 AND
       V.IDENTNR = &&1 --C.VARD_IDENTNR
ORDER BY G.IDENTNR;
exit;
EOF

while read line; do
    var=${line%%,*}
#    reti=`sqlplus -S leggo/meteo@metw @$sqlfile $var`
    reti=`sqlplus -S leggo/meteo@metw @$sqlfile $var`
    for rete in $reti; do
	echo "$line,$rete"
    done
done >$varmap <<EOF
158,B12001,C->K,103,2000,0,0,254,0,0,Temperatura (IDRO),TEMPERATURE/DRY-BULB TEMPERATURE,1.0,273.15
159,B13011,mm->KG/M**2,1,0,0,0,1,1800,0,precipitazione in 30 minuti (IDRO),TOTAL PRECIPITATION / TOTAL WATER EQUIVALENT,1.0,0.0
160,B13011,mm->KG/M**2,1,0,0,0,1,3600,0,Precipitazione nell'ora (IDRO),TOTAL PRECIPITATION / TOTAL WATER EQUIVALENT,1.0,0.0
162,B13013,m->M,1,0,0,0,254,0,0,neve (spessore dello strato) (IDRO),TOTAL SNOW DEPTH,1.0,0.0
165,B11001,gsess->DEGREE TRUE,103,10000,0,0,254,0,0,Vento: direzione media degli ultimi 10 minuti (IDRO),WIND DIRECTION,1.0,0.0
166,B11002,M/S->M/S,103,10000,0,0,254,0,0,Vento: velocita' media degli ultimi 10 minuti (IDRO),WIND SPEED,1.0,0.0
172,B13011,mm->KG/M**2,1,0,0,0,1,900,0,precipitazione nei 15 minuti ( IDRO ),TOTAL PRECIPITATION / TOTAL WATER EQUIVALENT,1.0,0.0
220,B13011,mm/10->KG/M**2,1,0,0,0,1,10800,0,precipitazione (LOKM),TOTAL PRECIPITATION / TOTAL WATER EQUIVALENT,1.0,0.0
221,B12001,C/10->K,103,2000,0,0,254,0,0,temperatura (LOKM),TEMPERATURE/DRY-BULB TEMPERATURE,0.1,273.15
222,B11001,degree true->DEGREE TRUE,103,10000,0,0,254,0,0,vento direzione (LOKM),WIND DIRECTION,1.0,0.0
223,B11002,m/s/10->M/S,103,10000 0,0,254,0,0,vento velocita' (LOKM),WIND SPEED,0.1,0.0
139,B13003,%->%,103,2000,0,0,254,0,0,umidita' istantanea (AGRMET),RELATIVE HUMIDITY,1.0,0.0
140,B13003,%->%,103,2000,0,0,0,0,3600,umidita' media (AGRMET),RELATIVE HUMIDITY,1.0,0.0
224,B13003,%->%,103,2000,0,0,254,0,0,umidita' a 2m (LOKM),RELATIVE HUMIDITY,1.0,0.0
225,B12001,C/10->K,103,2000,0,0,3,43200,0,temperatura minima (LOKM),TEMPERATURE/DRY-BULB TEMPERATURE,0.1,273.15
226,B12001,C/10->K,103,2000,0,0,2,43200,0,temperatura massima (LOKM),TEMPERATURE/DRY-BULB TEMPERATURE,0.1,273.15
EOF
rm -f $sqlfile


# 4,B11001,gsess->DEGREE TRUE,103,10000,0,0,254,0,0,vento direzione (SYNOP),WIND DIRECTION,1.0,0.0
# 5,B11002,nodi->M/S,103,10000,0,0,254,0,0,vento velocita' (SYNOP),WIND SPEED,0.514444,0.0
# 5,B11002,nodi->M/S,105,10,0,254,0,0,vento velocita' (SYNOP),WIND SPEED
