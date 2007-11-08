#!/bin/bash

#
# Script unix per la produzione dell'anagrafica delle stazioni per rete
#
#



while read campo1  ;do

  sqlplus leggo/meteo@metw @Query_MacroArea  $campo1
  mv MacroAree.spool $campo1''.txt''

done

exit
