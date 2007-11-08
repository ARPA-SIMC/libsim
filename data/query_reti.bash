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

((id = 0))
while [ $id -le 72 ] ;do

  ((id = id + 1))

  sqlplus leggo/meteo@metw @Query_da_idrete_a_descr  $id
  grep -i $id  prova_selvini.spool >> reti_id_descr.txt

done

exit
