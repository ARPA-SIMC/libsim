set lines 2000
set pages 10000
set verify off
set feed off
set heading on
set trimspool on
spool prova_selvini.spool
col STAZID             format 99999
col YLAT               format 999.9999999
col XLONG              format 999.9999999
col QPOZZ              format 99999
col QSTAZ              format 99999
select NVL(st.identnr,-9999) STAZID, 
       NVL(pm.y_lat_cent,-99.9999999) YLAT, NVL(pm.x_long_cent,-99.9999999) XLONG,
       NVL(st.z_quota_pozzetto,-9999) QPOZZ,  NVL(st.z_quota_stazione,-9999) QSTAZ 
  from met_stazioni_misura st,
       met_punti_misura pm
 where st.gsta_identnr  = &&1 
   and pm.identnr(+)    = st.pmis_identnr
order by upper(st.nome) ;
exit
