#!/bin/bash

#
# Procedura per la creazione dell'anagrafica delle reti
# presenti nell'archivio oracle ordinate per identificativo
# e descrittore di rete
#
# P.es 1  SYNOP
#
#---------------------------------------------------------------------


#interrogo l'archivio oracle

while read campo1 campo2 campo3  ;do

  name=simana_new_0$campo1.txt

  echo "ecco il nome" $name

  sqlplus leggo/meteo@metw @Query_da_idrete_a_simana  $campo1
  grep -i [.]  prova_selvini.spool >> $name

done

exit
