#!/bin/bash

PATH=/usr/local/bin:$PATH

rm reti_id_descr.txt
rm simana*new*.txt
rm AGRMET.txt BOA.txt all_staz.TXT BZLOKM.txt CAELAMI.txt
rm CER.txt CLIMAT.txt CLINUR.txt
rm A.txt B.txt C.txt D.txt E.txt F.txt G.txt H.txt
rm CRO.txt EMLOKM.txt ETG.txt FIDUMA.txt FIDUPO.txt
rm FRLOKM.txt GIAS.txt ICIRFE.txt PO267.txt POLLINI.txt
rm SALOKM.txt SIMNBO.txt SIMNPR.txt SPDSRA.txt SYNOP.txt
rm SYREP.txt  TEMP.txt  TRLOKM.txt  UMSUOL.txt
rm URBANE.txt  VELOKM.txt  VMSTAT.txt
rm FIDUTO.txt FREATI.txt IDRMEC.txt IDRMGI.txt IDROST.txt
rm IDRSTA.txt LILOKM.txt LOCALI.txt LOLOKM.txt
rm MALOKM.txt METAR.txt NURBAN.txt PILOKM.txt 

./query_reti.bash  #creo il file identnr delle  reti presenti in oracle 
./crea_anag_reti.bash  < reti_id_descr.txt      #anagrafica per reti
# ora creo il file master totale che include tutta l'anagrafica
cat *.txt >> all_staz.TXT
./crea_anag_macro_aree.bash  < macro_descr.TXT  #anagrafica per macroaree
./crea_anag_reti_simana.bash < reti_id_descr_simana.txt

exit
