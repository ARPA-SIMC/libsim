#!/bin/bash

#
# Script unix per la produzione dell'anagrafica delle stazioni per rete
#
#



#interrogo per rete - occorre fornire il file di elenco delle reti in iput



# lo script riceve in ingresso il file
# che passa tre argomenti
# 1:  identificativo numerico di rete, utilizzato per interrogare oracle
# 2:  descrittivo mnemonico di rete, utilizzato per re-indirizzare l'output
# 3:  identificativo tabella  VM - che separa misure in tempo reale da 
#     riepilogativi vari (1 = SYNOP, SIMNPR, LOCALI - 4 = SYREP, CLIMAT, CLINUR

while read campo1 campo2 campo3 ;do

  sqlplus leggo/meteo@metw @Query_reti  $campo1
  mv prova_selvini.spool $campo2''.txt''

done



exit
