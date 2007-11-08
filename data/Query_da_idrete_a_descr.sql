set lines 40
set pages 10000
set verify off
set feed off          
set heading on
set trimspool on 

spool prova_selvini.spool
col IDENTNR            format 9999 
col DESCRIZIONE        format A9
SELECT NVL(G.identnr,-9999) IDENTNR,
       substr(NVL(G.descrizione,'NULL'),1,9)DESCRIZIONE,
       g.id_tabella_vm
FROM  MET_GRUPPI_STAZIONI G
WHERE G.IDENTNR = &&1 ;

exit;
