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
159,B13011,mm->KG/M**2,1,0,0,0,1,0,1800,precipitazione in 30 minuti (IDRO),TOTAL PRECIPITATION / TOTAL WATER EQUIVALENT,1.0,0.0
160,B13011,mm->KG/M**2,1,0,0,0,1,0,3600,Precipitazione nell'ora (IDRO),TOTAL PRECIPITATION / TOTAL WATER EQUIVALENT,1.0,0.0
162,B13013,m->M,1,0,0,0,254,0,0,neve (spessore dello strato) (IDRO),TOTAL SNOW DEPTH,1.0,0.0
165,B11001,gsess->DEGREE TRUE,103,10000,0,0,254,0,0,Vento: direzione media degli ultimi 10 minuti (IDRO),WIND DIRECTION,1.0,0.0
166,B11002,M/S->M/S,103,10000,0,0,254,0,0,Vento: velocita' media degli ultimi 10 minuti (IDRO),WIND SPEED,1.0,0.0
172,B13011,mm->KG/M**2,1,0,0,0,1,0,900,precipitazione nei 15 minuti ( IDRO ),TOTAL PRECIPITATION / TOTAL WATER EQUIVALENT,1.0,0.0
220,B13011,mm/10->KG/M**2,1,0,0,0,1,0,10800,precipitazione (LOKM),TOTAL PRECIPITATION / TOTAL WATER EQUIVALENT,1.0,0.0
221,B12001,C/10->K,103,2000,0,0,254,0,0,temperatura (LOKM),TEMPERATURE/DRY-BULB TEMPERATURE,0.1,273.15
222,B11001,degree true->DEGREE TRUE,103,10000,0,0,254,0,0,vento direzione (LOKM),WIND DIRECTION,1.0,0.0
223,B11002,m/s/10->M/S,103,10000,0,0,254,0,0,vento velocita' (LOKM),WIND SPEED,0.1,0.0
139,B13003,%->%,103,2000,0,0,254,0,0,umidita' istantanea (AGRMET),RELATIVE HUMIDITY,1.0,0.0
140,B13003,%->%,103,2000,0,0,0,0,3600,umidita' media (AGRMET),RELATIVE HUMIDITY,1.0,0.0
224,B13003,%->%,103,2000,0,0,254,0,0,umidita' a 2m (LOKM),RELATIVE HUMIDITY,1.0,0.0
225,B12001,C/10->K,103,2000,0,0,3,0,43200,temperatura minima (LOKM),TEMPERATURE/DRY-BULB TEMPERATURE,0.1,273.15
226,B12001,C/10->K,103,2000,0,0,2,0,43200,temperatura massima (LOKM),TEMPERATURE/DRY-BULB TEMPERATURE,0.1,273.15
360,B15200,numero->?,103,15000,0,0,0,0,86400,Graminacee_Graminacee indistinte (POLLINI),(description not available),1.0,0.0
361,B15201,numero->?,103,15000,0,0,0,0,86400,Betulacee_Ontano nero (POLLINI),(description not available),1.0,0.0
362,B15202,numero->?,103,15000,0,0,0,0,86400,Betulacee_Betulla (POLLINI),(description not available),1.0,0.0
363,B15203,numero->?,103,15000,0,0,0,0,86400,Betulacee_Betulacee indistinte (POLLINI),(description not available),1.0,0.0
364,B15204,numero->?,103,15000,0,0,0,0,86400,Composite_Ambrosia (POLLINI),(description not available),1.0,0.0
365,B15205,numero->?,103,15000,0,0,0,0,86400,Composite_Artemisia (POLLINI),(description not available),1.0,0.0
366,B15206,numero->?,103,15000,0,0,0,0,86400,Composite_Composite indistinte (POLLINI),(description not available),1.0,0.0
367,B15207,numero->?,103,15000,0,0,0,0,86400,Corilacee_Nocciolo (POLLINI),(description not available),1.0,0.0
368,B15208,numero->?,103,15000,0,0,0,0,86400,Corilacee_Carpino bianco -Carpino nero (POLLINI),(description not available),1.0,0.0
369,B15209,numero->?,103,15000,0,0,0,0,86400,Corilacee_Corilacee indistinte (POLLINI),(description not available),1.0,0.0
370,B15210,numero->?,103,15000,0,0,0,0,86400,Fagacee_Castagno (POLLINI),(description not available),1.0,0.0
371,B15211,numero->?,103,15000,0,0,0,0,86400,Fagacee_Faggio (POLLINI),(description not available),1.0,0.0
372,B15212,numero->?,103,15000,0,0,0,0,86400,Fagacee_Quercia (POLLINI),(description not available),1.0,0.0
373,B15213,numero->?,103,15000,0,0,0,0,86400,Fagacee_Fagacee indistinte (POLLINI),(description not available),1.0,0.0
374,B15214,numero->?,103,15000,0,0,0,0,86400,Oleacee_Olivo (POLLINI),(description not available),1.0,0.0
375,B15215,numero->?,103,15000,0,0,0,0,86400,Oleacee_Frassino (POLLINI),(description not available),1.0,0.0
376,B15216,numero->?,103,15000,0,0,0,0,86400,Oleacee_Oleacee indistinte (POLLINI),(description not available),1.0,0.0
377,B15217,numero->?,103,15000,0,0,0,0,86400,Plantaginacee_Plantaginacee indistinte (POLLINI),(description not available),1.0,0.0
378,B15218,numero->?,103,15000,0,0,0,0,86400,Urticacee_Urticacee indistinte (POLLINI),(description not available),1.0,0.0
379,B15219,numero->?,103,15000,0,0,0,0,86400,Cupressacee - Taxacee indistinte_Cipresso comune (POLLINI),(description not available),1.0,0.0
380,B15220,numero->?,103,15000,0,0,0,0,86400,Cupressacee - Taxacee indistinte_Cupressacee - Taxacee indistin,(description not available),1.0,0.0
381,B15221,numero->?,103,15000,0,0,0,0,86400,Chenopodiacee - Amarantacee Indistinte_Amaranto (POLLINI),(description not available),1.0,0.0
382,B15222,numero->?,103,15000,0,0,0,0,86400,Chenopodiacee - Amarantacee Indistinte_Chenopodiacee - Amaranta,(description not available),1.0,0.0
383,B15223,numero->?,103,15000,0,0,0,0,86400,Poligonacee_Poligonacee indistinte (POLLINI),(description not available),1.0,0.0
384,B15224,numero->?,103,15000,0,0,0,0,86400,Euphorbiacee_Euforbiacee indistinte (POLLINI),(description not available),1.0,0.0
385,B15225,numero->?,103,15000,0,0,0,0,86400,Mirtacee_Mirtacee indistinte (POLLINI),(description not available),1.0,0.0
386,B15226,numero->?,103,15000,0,0,0,0,86400,Ulmacee_Bagolaro comune (POLLINI),(description not available),1.0,0.0
387,B15227,numero->?,103,15000,0,0,0,0,86400,Ulmacee_Olmo campestre (POLLINI),(description not available),1.0,0.0
388,B15228,numero->?,103,15000,0,0,0,0,86400,Ulmacee_Ulmacee indistinte (POLLINI),(description not available),1.0,0.0
389,B15229,numero->?,103,15000,0,0,0,0,86400,Platanacee_Platanacee indistinte (POLLINI),(description not available),1.0,0.0
390,B15230,numero->?,103,15000,0,0,0,0,86400,Aceraceae_Aceracee indistinte (POLLINI),(description not available),1.0,0.0
391,B15231,numero->?,103,15000,0,0,0,0,86400,Pinacee_Pinacee indistinte (POLLINI),(description not available),1.0,0.0
392,B15232,numero->?,103,15000,0,0,0,0,86400,Salicacee_Salice comune (POLLINI),(description not available),1.0,0.0
393,B15233,numero->?,103,15000,0,0,0,0,86400,Salicacee_Pioppo (POLLINI),(description not available),1.0,0.0
394,B15234,numero->?,103,15000,0,0,0,0,86400,Salicacee_Salicacee indistinte (POLLINI),(description not available),1.0,0.0
395,B15235,numero->?,103,15000,0,0,0,0,86400,Ciperacee_Ciperacee indistinte (POLLINI),(description not available),1.0,0.0
396,B15236,numero->?,103,15000,0,0,0,0,86400,Juglandacee_Juglandacee indistinte (POLLINI),(description not available),1.0,0.0
397,B15237,numero->?,103,15000,0,0,0,0,86400,Ippocastanacee_Ippocastanacee indistinte (POLLINI),(description not available),1.0,0.0
398,B15238,numero->?,103,15000,0,0,0,0,86400,Spore fungine_Alternaria (POLLINI),(description not available),1.0,0.0
399,B15239,numero->?,103,15000,0,0,0,0,86400,Spore fungine_Botrytis (POLLINI),(description not available),1.0,0.0
400,B15240,numero->?,103,15000,0,0,0,0,86400,Spore fungine_Stemphylium (POLLINI),(description not available),1.0,0.0
401,B15241,numero->?,103,15000,0,0,0,0,86400,Spore fungine_Cladosporium (POLLINI),(description not available),1.0,0.0
402,B15242,numero->?,103,15000,0,0,0,0,86400,Spore fungine_Epicoccum (POLLINI),(description not available),1.0,0.0
513,B15243,numero->?,103,15000,0,0,0,0,86400,Altri Pollini / Non Identificati_Altri pollini identificati (PO,(description not available),1.0,0.0
514,B15244,numero->?,103,15000,0,0,0,0,86400,Altri Pollini / Non Identificati_Pollini non identificati (POLL,(description not available),1.0,0.0
515,B15245,numero->?,103,15000,0,0,0,0,86400,Altre Spore / Non identificati_Altre spore fungine (POLLINI),(description not available),1.0,0.0
516,B15246,numero->?,103,15000,0,0,0,0,86400,Altre Spore / Non identificati_Spore fungine non identificate (,(description not available),1.0,0.0
538,B15247,numero->?,103,15000,0,0,0,0,86400,Graminacee_Graminacee indistinte 1 (POLLINI),(description not available),1.0,0.0
539,B15248,numero->?,103,15000,0,0,0,0,86400,Plantaginacee_Plantaginacee indistinte 1 (POLLINI),(description not available),1.0,0.0
540,B15249,numero->?,103,15000,0,0,0,0,86400,Urticacee_Urticacee indistinte 1 (POLLINI),(description not available),1.0,0.0
541,B15250,numero->?,103,15000,0,0,0,0,86400,Poligonacee_Poligonacee indistinte 1 (POLLINI),(description not available),1.0,0.0
542,B15251,numero->?,103,15000,0,0,0,0,86400,Euphorbiacee_Euforbiacee indistinte 1 (POLLINI),(description not available),1.0,0.0
543,B15252,numero->?,103,15000,0,0,0,0,86400,Mirtacee_Mirtacee indistinte 1 (POLLINI),(description not available),1.0,0.0
544,B15253,numero->?,103,15000,0,0,0,0,86400,Platanacee_Platanacee indistinte 1 (POLLINI),(description not available),1.0,0.0
545,B15254,numero->?,103,15000,0,0,0,0,86400,Aceraceae_Aceracee indistinte 1 (POLLINI),(description not available),1.0,0.0
546,B15255,numero->?,103,15000,0,0,0,0,86400,Pinacee_Pinacee indistinte 1 (POLLINI),(description not available),1.0,0.0
547,B15000,numero->?,103,15000,0,0,0,0,86400,Ciperacee_Ciperacee indistinte 1 (POLLINI),(description not available),1.0,0.0
548,B15001,numero->?,103,15000,0,0,0,0,86400,Juglandacee_Juglandacee indistinte 1 (POLLINI),(description not available),1.0,0.0
549,B15002,numero->?,103,15000,0,0,0,0,86400,Ippocastanacee_Ippocastanacee indistinte 1 (POLLINI),(description not available),1.0,0.0
EOF
rm -f $sqlfile


# 4,B11001,gsess->DEGREE TRUE,103,10000,0,0,254,0,0,vento direzione (SYNOP),WIND DIRECTION,1.0,0.0
# 5,B11002,nodi->M/S,103,10000,0,0,254,0,0,vento velocita' (SYNOP),WIND SPEED,0.514444,0.0
# 5,B11002,nodi->M/S,105,10,0,254,0,0,vento velocita' (SYNOP),WIND SPEED
