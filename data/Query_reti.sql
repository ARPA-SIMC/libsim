set lines 2000
set pages 10000
set verify off
set feed off
set heading on
set trimspool on
spool prova_selvini.spool
col STAZID             format 99999
col NOME               format A25
col QSTAZ              format 99999
col QPOZZ              format 99999
col YLAT               format 999.9999999
col XLONG              format 999.9999999
col COMUNE             format A25
col PROV               format A4
col REGIONE            format A25
col NAZIONE            format A25
col INIVAL             format A10
col FINVAL             format A10
col BACINO             format A32
col SOTTOBACINO        format A25
col MACROAREA          format A4
select NVL(st.identnr,-9999) STAZID, substr(NVL(st.nome,'NULL'),1,25) NOME,
       NVL(st.z_quota_stazione,-9999) QSTAZ, NVL(st.z_quota_pozzetto,-9999) QPOZZ,
       NVL(pm.y_lat_cent,-99.9999999) YLAT, NVL(pm.x_long_cent,-99.9999999) XLONG,
       substr(NVL(gc.nome,'NULL'),1,25) COMUNE,rpad(NVL(gp.sigla,'NULL'),4) PROV,
       substr(NVL(gr.nome,'NULL'),1,25) REGIONE,
       substr(NVL(gn.cod_stato,'NULL'),1,25) NAZIONE,
       TO_CHAR(NVL(st.data_inizio,TO_DATE('09-09-9999','DD-MM-YYYY')),'DD-MM-YYYY') INIVAL,
       TO_CHAR(NVL(st.data_fine,TO_DATE('09-09-9999','DD-MM-YYYY')),'DD-MM-YYYY') FINVAL,
       substr(NVL(bc.des,'NULL'),1,32) BACINO,substr(NVL(sbc.des,'NULL'),1,25) SOTTOBACINO,
       rpad(NVL(pm.macrobacino,'NULL'),4) MACROAREA
  from met_stazioni_misura st,
       met_punti_misura pm,geo_comuni gc,
       geo_province gp, geo_regioni gr,
       geo_nazioni gn,
       met_bacini bc, met_bacini sbc
 where st.gsta_identnr  = &&1 
   and pm.identnr(+)    = st.pmis_identnr
   and bc.cod(+)        = pm.bacino
   and sbc.cod(+)       = pm.sottobacino
   and gc.cod_istat(+)  = pm.com_identnr
   and gp.id_pro(+)     = pm.prv_identnr
   and gr.id_reg(+)     = pm.reg_identnr
   and gn.cod_stato(+)  = pm.naz_identnr
order by upper(st.nome) ;
exit
