#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>
#include <alloca.h>
#include "config.h"

#define DATELEN 13
#define CVALLEN 8
#define FLAGLEN 10
#define TABLEN 16
#define MSGLEN 256
#define NAMELEN 21
#define BTABVARLEN 6

/* Define SQL statements to be used in program. */

static char *varquery = (char *)
  "SELECT \
v.IDENTNR, NVL(v.LTYPE1_NEW,-999), NVL(v.L1_NEW,-999), \
NVL(v.LTYPE2_NEW,-999), NVL(v.L2_NEW,-999), NVL(v.PIND_NEW,-999), \
NVL(v.P1_NEW,-999), NVL(v.P2_NEW,-999) \
FROM MET_VARIABILI_DEFINITE v \
WHERE v.BLOCAL_NEW != ' ' AND v.BLOCAL_NEW IS NOT NULL AND v.IDENTNR IS NOT NULL \
ORDER by v.IDENTNR";

/*
  "SELECT \
v.IDENTNR NVL(v.LTYPE1_NEW,-999) NVL(v.L1_NEW,-999) \
NVL(v.LTYPE2_NEW,-999) NVL(v.L2_NEW NVL(v.PIND_NEW,-999) \
NVL(v.P1_NEW,-999) NVL(v.P2_NEW,-999) \
FROM MET_VARIABILI_DEFINITE v \
WHERE v.BLOCAL_NEW BLOCAL = :var \
ORDER by upper(v.IDENTNR)";
*/

/*
SELECT v.IDENTNR IDENT, v.BLOCAL_NEW BLOCAL, v.UMIS_CODICE_PRINCIPALE CODICE,
       u.ABBREVIAZIONE, v.DESCRIZIONE
FROM   MET_VARIABILI_DEFINITE v, MET_UNITA_MISURA u
WHERE BLOCAL_NEW != ' ' AND v.UMIS_CODICE_PRINCIPALE = u.CODICE
ORDER BY IDENTNR;
*/

/* versione vecchia piu` complicata che richiedeva rete e variabile
static char *tabquery = (char *)
  "SELECT \
RTRIM(DECODE(gs.id_tabvar_mod,'Y',tv1.nome_tabella,'N',tv2.nome_tabella)) \
FROM met_gruppi_stazioni gs, met_variabili_definite va, \
     met_tabelle_vm tv1, met_tabelle_vm tv2 \
WHERE gs.identnr = :net AND va.identnr = :var AND \
      tv1.id_tabella = gs.id_tabella_vm AND tv2.id_tabella = va.id_tabella_vm";
*/

/* versione piu` semplice che non richiede la rete
static char *tabquery = (char *)
  "SELECT \
RTRIM(tv.nome_tabella) FROM met_variabili_definite va, met_tabelle_vm tv \
WHERE va.identnr = :var AND tv.id_tabella = va.id_tabella_vm";
*/

/* terza versione che richiede solo la rete, potrebbe non funzionare
   con la rete CAELAMI, ma per ora me ne frego */
static char *tabquery = (char *)
  "SELECT \
RTRIM(tv.nome_tabella) FROM met_gruppi_stazioni gs, met_tabelle_vm tv \
WHERE gs.identnr = :net AND tv.id_tabella = gs.id_tabella_vm";

static char *netquery = (char *)
  "SELECT G.IDENTNR FROM MET_GRUPPI_STAZIONI G WHERE G.DESCRIZIONE = :netdesc";

static char *anaquery = (char *)
  "SELECT NVL(st.identnr,-9999), \
NVL(pm.x_long_cent,-999.9), NVL(pm.y_lat_cent,-999.9), \
NVL(st.z_quota_stazione,-9999.9), NVL(st.z_quota_pozzetto,-9999.9), \
SUBSTR(NVL(st.nome,'S. Cresci in Valcava'),1,20), \
SUBSTR(gs.descrizione,1,20) \
FROM met_stazioni_misura st, met_punti_misura pm, met_gruppi_stazioni gs \
WHERE st.gsta_identnr = :net AND pm.identnr(+) = st.pmis_identnr AND \
gs.identnr = :net \
ORDER by upper(st.identnr)";

static char *query1 = (char *)
  "SELECT TO_CHAR(a.dset_istante_wmo_fine,'YYYYMMDDHH24MI'), \
a.dset_stam_identnr,a.vard_identnr, \
NVL(a.valore_principale,1.0E+15),NVL(a.valore_ausiliario,1.0E+15), \
NVL(valore_chiaro, ' '),LPAD(NVL(a.flag,'000000000'), 9, '0') \
FROM ";

static char *query2 = (char *)
  " a WHERE \
dset_istante_wmo_fine >= TO_DATE(:ti,'YYYYMMDDHH24MI') AND \
dset_istante_wmo_fine <= TO_DATE(:tf,'YYYYMMDDHH24MI') AND \
EXISTS(SELECT /*+use_nl*/ s.identnr \
from stazioni_misura s WHERE s.gsta_identnr = :net AND \
a.dset_stam_identnr = s.identnr) AND a.vard_identnr";

static char *query3 = (char *)
  " ORDER BY dset_istante_wmo_fine,dset_stam_identnr";

/* candidata query moderna

SELECT TO_CHAR(a.dset_istante_wmo_fine,'YYYYMMDDHH24MI'),
a.dset_stam_identnr,a.vard_identnr,
NVL(a.valore_principale,1.0E+15),NVL(a.valore_ausiliario,1.0E+15),
NVL(a.valore_chiaro, ' '),LPAD(NVL(a.flag,'000000000'), 9, '0'),
s.gsta_identnr FROM vm a, met_stazioni_misura s WHERE
a.dset_istante_wmo_fine >= sysdate -1 AND a.dset_istante_wmo_fine <=
sysdate and a.vard_identnr in (select va.identnr from
met_variabili_definite va where va.blocal_new = 'B13011' ) AND
a.dset_stam_identnr = s.identnr;

*/


/* Struttura per ricordare una connessione oracle e fare login una volta sola. */
typedef struct _OracleDbConnection 
{
  OCIEnv *envhp;
  OCIError *errhp;
  OCISvcCtx *svchp;
  OCIStmt *stmthp;
  OCIDefine *defn1p;
  OCIDefine *defn2p;
  OCIDefine *defn3p;
  OCIDefine *defn4p;
  OCIDefine *defn5p;
  OCIDefine *defn6p;
  OCIDefine *defn7p;
  OCIDefine *defn8p;
  OCIBind *bnd1p;
  OCIBind *bnd2p;
  OCIBind *bnd3p;
  OCIBind *bnd4p;
  char table[TABLEN];
  char errmsg[MSGLEN];
} OracleDbConnection;


OracleDbConnection *FC_FUNC_(oraclesim_init, ORACLESIM_INIT)
     (char *, char *, char *, int*);
void FC_FUNC_(oraclesim_delete, ORACLESIM_DELETE)
     (OracleDbConnection **);
void FC_FUNC_(oraclesim_geterr, ORACLESIM_GETERR)
     (OracleDbConnection **, char *);
int FC_FUNC_(oraclesim_getnet, ORACLESIM_GETNET)
     (OracleDbConnection **, const char *);
int FC_FUNC_(oraclesim_getdatahead, ORACLESIM_GETDATAHEAD)
     (OracleDbConnection **, char *, char *, int *, int *, int *);
int FC_FUNC_(oraclesim_getdatavol, ORACLESIM_GETDATAVOL)
     (OracleDbConnection **, int *, int *, char *, int *, int *,
      float *, float *, char *, char *, float *);
int FC_FUNC_(oraclesim_getanahead, ORACLESIM_GETANAHEAD)
     (OracleDbConnection **, int *);
int FC_FUNC_(oraclesim_getanavol, ORACLESIM_GETANAVOL)
     (OracleDbConnection **, int *, int *, int *, int *, double *, double *,
      float *, float *, char *, char *, float *, double *);
int FC_FUNC_(oraclesim_getvarhead, ORACLESIM_GETVARHEAD)
     (OracleDbConnection ** /*, int * */);
int FC_FUNC_(oraclesim_getvarvol, ORACLESIM_GETVARVOL)
     (OracleDbConnection **, int *, int *, int *,
      int *, int *, int *, int *,
      int *, int *, int *, int *);
     static int gettab(OracleDbConnection *, int *, int, int *);
static void datadefine(OracleDbConnection *,
		       char *, int, int *, int *, float *, float *, char *, char *, int, int);
static void anadefine(OracleDbConnection *,
		      int *, double *, double *, float *, float *, char *, char *, int);
static void vardefine(OracleDbConnection *,
		      int *, int *, int *, int *, int *, int *, int *, int *);
static sword checkerr(OracleDbConnection *, sword);
static int countrows(OracleDbConnection *);


OracleDbConnection *FC_FUNC_(oraclesim_init, ORACLESIM_INIT)
     (char *username, char *password, char *dbname, int *err) {
  sword status;
  OracleDbConnection *dbconnid;

  if ((dbconnid = (OracleDbConnection *) malloc(sizeof(OracleDbConnection)))
      == NULL) {
    *err = 1;
    return dbconnid;
  }

  dbconnid->envhp = NULL;
  dbconnid->errhp = NULL;
  dbconnid->svchp = NULL;
  dbconnid->stmthp = NULL;
  dbconnid->defn1p = NULL;
  dbconnid->defn2p = NULL; 
  dbconnid->defn3p = NULL;
  dbconnid->defn4p = NULL;
  dbconnid->defn5p = NULL;
  dbconnid->defn6p = NULL;
  dbconnid->defn7p = NULL;
  dbconnid->defn8p = NULL;
  dbconnid->bnd1p = NULL;
  dbconnid->bnd2p = NULL;
  dbconnid->bnd3p = NULL;
  dbconnid->bnd4p = NULL;
  dbconnid->table[0] = 0;
  dbconnid->errmsg[0] = 0;

  if ((status = OCIEnvCreate((OCIEnv **) &dbconnid->envhp, (ub4) OCI_DEFAULT,
			     (dvoid *) 0, (dvoid * (*)(dvoid *,size_t)) 0,
			     (dvoid * (*)(dvoid *, dvoid *, size_t)) 0,
			     (void (*)(dvoid *, dvoid *)) 0, (size_t) 0,
			     (dvoid **) 0)) != OCI_SUCCESS) {
    snprintf(dbconnid->errmsg, MSGLEN, 
	     "Oracle error - OCIEnvCreate failed with status = %d", status);
    *err = 2;
  }
  /* Alloco l'error handle */
  else if (checkerr(NULL, OCIHandleAlloc(dbconnid->envhp,
					 (dvoid **) &dbconnid->errhp,
					 OCI_HTYPE_ERROR, (size_t) 0, (dvoid **) 0))
	   != OCI_SUCCESS) *err = 3;
  /* Mi connetto */
  else if (checkerr(dbconnid, OCILogon(dbconnid->envhp, dbconnid->errhp,
				       &dbconnid->svchp,
				       (text *) username, strlen(username),
				       (text *) password, strlen(password),
				       (text *) dbname, strlen(dbname)))
	   != OCI_SUCCESS) *err = 4;
  /* Alloco lo statement handle */
  else if (checkerr(dbconnid, OCIHandleAlloc(dbconnid->envhp,
					    (dvoid **) &dbconnid->stmthp,
					     OCI_HTYPE_STMT, (size_t) 0,
					     (dvoid **) 0))
	   != OCI_SUCCESS) *err = 5;
  else
    *err = 0;
  return dbconnid;

}


void FC_FUNC_(oraclesim_delete, ORACLESIM_DELETE) (OracleDbConnection **fdbconnid) {
  OracleDbConnection *dbconnid = *fdbconnid;
  if (dbconnid != NULL) {
    if (dbconnid->svchp != NULL) OCILogoff(dbconnid->svchp, dbconnid->errhp);
    if (dbconnid->stmthp != NULL) OCIHandleFree(dbconnid->stmthp, OCI_HTYPE_STMT);
    if (dbconnid->errhp != NULL) OCIHandleFree(dbconnid->errhp, OCI_HTYPE_ERROR);
    if (dbconnid->envhp != NULL) OCIHandleFree(dbconnid->envhp, OCI_HTYPE_ENV);
    free(dbconnid);
  }
  *fdbconnid = 0;
}


void FC_FUNC_(oraclesim_geterr, ORACLESIM_GETERR)
     (OracleDbConnection **fdbconnid, char *errmsg) {
  if (*fdbconnid == NULL)
    strcpy(errmsg, "Oracle error, allocation problem");
  else
    strcpy(errmsg, (*fdbconnid)->errmsg);
}


int FC_FUNC_(oraclesim_getnet, ORACLESIM_GETNET)
     (OracleDbConnection **fdbconnid, const char *netdesc) {
  OracleDbConnection *dbconnid = *fdbconnid;
  sword status;
  int onetid;

  /* Preparo l'estrazione per ottenere il numero della rete dal nome */
  checkerr(dbconnid,
	   OCIStmtPrepare(dbconnid->stmthp, dbconnid->errhp,
			  (text *) netquery,
			  (ub4) strlen(netquery),
			  (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  checkerr(dbconnid,
	   OCIBindByName(dbconnid->stmthp, &dbconnid->bnd1p,
			 dbconnid->errhp, (text *) ":netdesc",
			 -1, (dvoid *) netdesc,
			 (sword) strlen(netdesc)+1, SQLT_STR, (dvoid *) NULL,
			 (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
			 OCI_DEFAULT));

  checkerr(dbconnid,
	   OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn1p,
			  dbconnid->errhp, 1,
			  (dvoid *) &onetid, sizeof(onetid), SQLT_INT,
			  (dvoid *) 0,
			  (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));


  /* Lancio l'estrazione */
  if ((status = OCIStmtExecute(dbconnid->svchp, dbconnid->stmthp, dbconnid->errhp,
			       (ub4) 0, (ub4) 0,
			       (CONST OCISnapshot *) NULL, (OCISnapshot *) NULL,
 			       OCI_STMT_SCROLLABLE_READONLY)) != OCI_SUCCESS) {
    if (status != OCI_NO_DATA) {
      checkerr(dbconnid, status);
      return -1;
    }
  }
  /* Prendo la prima riga di dati */
  if ((status = OCIStmtFetch2(dbconnid->stmthp, dbconnid->errhp, (ub4) 1,
			      (ub2) OCI_FETCH_NEXT, (sb4) 0,
			      (ub4) OCI_DEFAULT)) != OCI_SUCCESS) {
    if (status == OCI_NO_DATA) {
      return 0;
    } else {
      checkerr(dbconnid, status);
      return -1;
    }
  }
  return onetid;
}


int gettab(OracleDbConnection *dbconnid, int *var, int nvar, int *net) {
  sword status;

  checkerr(dbconnid,
	   OCIStmtPrepare(dbconnid->stmthp, dbconnid->errhp,
			  (text *) tabquery,
			  (ub4) strlen(tabquery),
			  (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  /*  checkerr(dbconnid,
	   OCIBindByName(dbconnid->stmthp, &dbconnid->bnd1p,
			 dbconnid->errhp, (text *) ":var",
			 -1, (dvoid *) var,
			 (sword) sizeof(*var), SQLT_INT, (dvoid *) NULL,
			 (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
			 OCI_DEFAULT)); */

  checkerr(dbconnid,
	   OCIBindByName(dbconnid->stmthp, &dbconnid->bnd1p,
			 dbconnid->errhp, (text *) ":net",
			 -1, (dvoid *) net,
			 (sword) sizeof(*net), SQLT_INT, (dvoid *) NULL,
			 (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
			 OCI_DEFAULT));

  checkerr(dbconnid,
	   OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn1p,
			  dbconnid->errhp, 1,
			  (dvoid *) dbconnid->table, TABLEN, SQLT_STR,
			  (dvoid *) NULL,
			  (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));


  /* Lancio l'estrazione */
  if ((status = OCIStmtExecute(dbconnid->svchp, dbconnid->stmthp, dbconnid->errhp,
			       (ub4) 0, (ub4) 0,
			       (CONST OCISnapshot *) NULL, (OCISnapshot *) NULL,
 			       OCI_STMT_SCROLLABLE_READONLY)) != OCI_SUCCESS) {
    if (status != OCI_NO_DATA) {
      checkerr(dbconnid, status);
      return -1;
    }
  }
  /* Prendo la prima riga di dati */
  if ((status = OCIStmtFetch2(dbconnid->stmthp, dbconnid->errhp, (ub4) 1,
			      (ub2) OCI_FETCH_NEXT, (sb4) 0,
			      (ub4) OCI_DEFAULT)) != OCI_SUCCESS) {
    if (status == OCI_NO_DATA) {
      return -1;
    } else {
      checkerr(dbconnid, status);
      return -1;
    }
  }
  return 0;
}


void datadefine(OracleDbConnection *dbconnid,
		char *odate, int datelen, int *ostatid, int *ovarid,
		float *ovalp, float *ovala, char *ovalc, char *oflag, int cvallen, int flaglen) {

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn1p,
 				    dbconnid->errhp, 1,
 				    (dvoid *) odate, datelen, SQLT_STR,
 				    (dvoid *) 0,
 				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn2p,
				    dbconnid->errhp, 2,
				    (dvoid *) ostatid, sizeof(*ostatid), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn3p,
				    dbconnid->errhp, 3,
				    (dvoid *) ovarid, sizeof(*ovarid), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn4p,
				    dbconnid->errhp, 4,
				    (dvoid *) ovalp, sizeof(*ovalp), SQLT_FLT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn5p,
				    dbconnid->errhp, 5,
				    (dvoid *) ovala, sizeof(*ovala), SQLT_FLT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn6p,
				    dbconnid->errhp, 6,
				    (dvoid *) ovalc, cvallen, SQLT_STR,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn6p,
				    dbconnid->errhp, 7,
				    (dvoid *) oflag, flaglen, SQLT_STR,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

}


int FC_FUNC_(oraclesim_getdatahead, ORACLESIM_GETDATAHEAD)
     (OracleDbConnection **fdbconnid, char *ti, char *tf,
      int *net, int *var, int *nvar) {
  OracleDbConnection *dbconnid = *fdbconnid;
  int i;
  char *query;

  /* valori validi =1, mancanti =-1,  inutilizzati per ora, ci pensa la query */
  /*   sb2 otab_ind, ovalp_ind, ovala_ind, oflag_ind; */
  char odate[DATELEN], ovalc[CVALLEN], oflag[FLAGLEN];
  int ostatid, ovarid;
  float ovalp, ovala;

  /* Eseguo l'estrazione per ottenere il nome della tabella,
     complicazione per colpa di CAELAMI */
  if ((i = gettab(dbconnid, var, *nvar, net)) != 0) return i;

  /* Ricostruisco la richiesta dati con il nome della tabella ottenuto */
  query = alloca(strlen(query1)+strlen(dbconnid->table)+strlen(query2)+
		 10*(*nvar+1)+strlen(query3)+1);
  strcpy(query, query1);
  strcat(query, dbconnid->table);
  strcat(query, query2);
  if (*nvar > 1) {
    strcat(query, " IN (");
    for (i=0; i < (*nvar); i++) {
      snprintf(query+strlen(query), 10, "%d", var[i]);
      if (i < (*nvar)-1) strcat(query, ",");
    }
    strcat(query, ")");
  } else {
    strcat(query, " = ");
    snprintf(query+strlen(query), 10, "%d", *var);
  }
  strcat(query, query3);

  /* Preparo l'estrazione dati */
  checkerr(dbconnid, OCIStmtPrepare(dbconnid->stmthp, dbconnid->errhp,
				    (text *) query,
				    (ub4) strlen(query),
				    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));


  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd1p,
				   dbconnid->errhp, (text *) ":ti",
				   -1, (dvoid *) ti,
				   DATELEN, SQLT_STR, (dvoid *) NULL,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd2p,
				   dbconnid->errhp, (text *) ":tf",
				   -1, (dvoid *) tf,
				   DATELEN, SQLT_STR, (dvoid *) NULL,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd3p,
				   dbconnid->errhp, (text *) ":net",
				   -1, (dvoid *) net,
				   (sword) sizeof(*net), SQLT_INT, (dvoid *) NULL,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  /* definisco l'uscita */
  datadefine(dbconnid, odate, DATELEN, &ostatid, &ovarid, &ovalp, &ovala, ovalc, oflag, CVALLEN, FLAGLEN);
  return countrows(dbconnid);
}


int FC_FUNC_(oraclesim_getdatavol, ORACLESIM_GETDATAVOL)
     (OracleDbConnection **fdbconnid, int *nr, int *vnr, 
      char *odate, int *ostatid, int *ovarid,
      float *ovalp, float *ovala, char *ovalc, char *oflag, float *rmiss) {
  OracleDbConnection *dbconnid = *fdbconnid;
  sword status;
  int i, ret;

  /* definisco l'uscita */
  datadefine(dbconnid, odate, DATELEN, ostatid, ovarid, ovalp, ovala, ovalc, oflag, CVALLEN, FLAGLEN);

  status=checkerr(dbconnid, OCIStmtFetch2(dbconnid->stmthp, dbconnid->errhp,
					  (ub4) *nr, OCI_FETCH_FIRST, (sb4) 0,
					  (ub4) OCI_DEFAULT));
  if (status == OCI_SUCCESS || status == OCI_NO_DATA) {
    if (checkerr(dbconnid, OCIAttrGet(dbconnid->stmthp, OCI_HTYPE_STMT, vnr, 0, 
				      OCI_ATTR_ROW_COUNT, dbconnid->errhp))
	== OCI_SUCCESS) {
      for (i=0; i<*vnr; i++) { /* inserisco i dati mancanti stile libsim */
 	if (ovalp[i] > 1.0E+14) ovalp[i] = *rmiss;
  	if (ovala[i] > 1.0E+14) ovala[i] = *rmiss;
      }
    } else {
      *vnr = 0;
    }
    ret = 0;
  } else {
    *vnr = 0;
    ret = 2;
  }
  return ret;
}


void anadefine(OracleDbConnection *dbconnid,
	       int *ostatid, double *olon, double *olat, float *oqs, float *oqp,
	       char *oname, char *onet, int namelen) {
  
  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn1p,
				    dbconnid->errhp, 1,
				    (dvoid *) ostatid, sizeof(*ostatid), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn2p,
				    dbconnid->errhp, 2,
				    (dvoid *) olon, sizeof(*olon), SQLT_FLT,
				    (dvoid *) NULL,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn3p,
				    dbconnid->errhp, 3,
				    (dvoid *) olat, sizeof(*olat), SQLT_FLT,
				    (dvoid *) NULL,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn4p,
				    dbconnid->errhp, 4,
				    (dvoid *) oqs, sizeof(*oqs), SQLT_FLT,
				    (dvoid *) NULL,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn5p,
				    dbconnid->errhp, 5,
				    (dvoid *) oqp, sizeof(*oqp), SQLT_FLT,
				    (dvoid *) NULL,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn6p,
				    dbconnid->errhp, 6,
				    (dvoid *) oname, namelen, SQLT_STR,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn7p,
				    dbconnid->errhp, 7,
				    (dvoid *) onet, namelen, SQLT_STR,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

}


int FC_FUNC_(oraclesim_getanahead, ORACLESIM_GETANAHEAD)
     (OracleDbConnection **fdbconnid, int *net) {
  OracleDbConnection *dbconnid = *fdbconnid;

  int ostatid;
  float oqs, oqp;
  double olon, olat;
  char oname[NAMELEN], onet[NAMELEN];
  
  /* Preparo l'estrazione anagrafica */
  checkerr(dbconnid, OCIStmtPrepare(dbconnid->stmthp, dbconnid->errhp,
				    (text *) anaquery,
				    (ub4) strlen(anaquery),
				    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));


  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd1p,
				   dbconnid->errhp, (text *) ":net",
				   -1, (dvoid *) net,
				   (sword) sizeof(*net), SQLT_INT, (dvoid *) NULL,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  /* definisco l'uscita */
  anadefine(dbconnid, &ostatid, &olon, &olat, &oqs, &oqp, oname, onet, NAMELEN);
  return countrows(dbconnid);
}


int FC_FUNC_(oraclesim_getanavol, ORACLESIM_GETANAVOL)
     (OracleDbConnection **fdbconnid, int *nr, int *vnr, int *namelen,
      int *ostatid, double *olon, double *olat, float *oqs, float *oqp,
      char *oname, char *onet, float *rmiss, double *dmiss) {
  OracleDbConnection *dbconnid = *fdbconnid;
  int i, ret;
  sword status;

  /* definisco l'uscita */
  anadefine(dbconnid, ostatid, olon, olat, oqs, oqp, oname, onet, *namelen);

  status=checkerr(dbconnid, OCIStmtFetch2(dbconnid->stmthp, dbconnid->errhp,
					  (ub4) *nr, OCI_FETCH_FIRST, (sb4) 0,
					  (ub4) OCI_DEFAULT));
  if (status == OCI_SUCCESS || status == OCI_NO_DATA) {
    if (checkerr(dbconnid, OCIAttrGet(dbconnid->stmthp, OCI_HTYPE_STMT, vnr, 0, 
				      OCI_ATTR_ROW_COUNT, dbconnid->errhp))
	== OCI_SUCCESS) {
      for (i=0; i<*vnr; i++) { /* inserisco i dati mancanti stile libsim */
 	if (olon[i] < -999.) olon[i] = *dmiss;
 	if (olat[i] < -999.) olat[i] = *dmiss;
  	if (oqs[i] < -9999.) oqs[i] = *rmiss;
  	if (oqp[i] < -9999.) oqp[i] = *rmiss;
      }
    } else {
      *vnr = 0;
    }
    ret = 0;
  } else {
    *vnr = 0;
    ret = 2;
  }
  return ret;
}


void vardefine(OracleDbConnection *dbconnid,
	       int *oidentnr, int *olt1, int *ol1,
	       int *olt2, int *ol2, int *opind, int *op1, int *op2) {

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn1p,
 				    dbconnid->errhp, 1,
 				    (dvoid *) oidentnr, sizeof(*oidentnr), SQLT_INT,
 				    (dvoid *) 0,
 				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn2p,
				    dbconnid->errhp, 2,
				    (dvoid *) olt1, sizeof(*olt1), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn3p,
				    dbconnid->errhp, 3,
				    (dvoid *) ol1, sizeof(*ol1), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn4p,
				    dbconnid->errhp, 4,
				    (dvoid *) olt2, sizeof(*olt2), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn5p,
				    dbconnid->errhp, 5,
				    (dvoid *) ol2, sizeof(*ol2), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn6p,
				    dbconnid->errhp, 6,
				    (dvoid *) opind, sizeof(*opind), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn7p,
				    dbconnid->errhp, 7,
				    (dvoid *) op1, sizeof(*op1), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn8p,
				    dbconnid->errhp, 8,
				    (dvoid *) op2, sizeof(*op2), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

}


int FC_FUNC_(oraclesim_getvarhead, ORACLESIM_GETVARHEAD)
     (OracleDbConnection **fdbconnid /*, int *var */) {
  OracleDbConnection *dbconnid = *fdbconnid;

  int oidentnr, olt1, ol1, olt2, ol2, opind, op1, op2;

  /* Preparo l'estrazione variabili */
  checkerr(dbconnid, OCIStmtPrepare(dbconnid->stmthp, dbconnid->errhp,
				    (text *) varquery,
				    (ub4) strlen(varquery),
				    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  /*
  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd1p,
				   dbconnid->errhp, (text *) ":var",
				   -1, (dvoid *) var,
				   (sword) sizeof(*var), SQLT_INT, (dvoid *) NULL,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));
  */
  
  /* definisco l'uscita */
  vardefine(dbconnid, &oidentnr, &olt1, &ol1, &olt2, &ol2, &opind, &op1, &op2);
  return countrows(dbconnid);
}


int FC_FUNC_(oraclesim_getvarvol, ORACLESIM_GETVARVOL)
     (OracleDbConnection **fdbconnid, int *nr, int *vnr, int *oidentnr,
      int *olt1, int *ol1, int *olt2, int *ol2,
      int *opind, int *op1, int *op2, int *imiss) {
  OracleDbConnection *dbconnid = *fdbconnid;
  int i, ret;
  sword status;

  /* definisco l'uscita */
  vardefine(dbconnid, oidentnr, olt1, ol1, olt2, ol2, opind, op1, op2);

  status=checkerr(dbconnid, OCIStmtFetch2(dbconnid->stmthp, dbconnid->errhp,
					  (ub4) *nr, OCI_FETCH_FIRST, (sb4) 0,
					  (ub4) OCI_DEFAULT));
  if (status == OCI_SUCCESS || status == OCI_NO_DATA) {
    if (checkerr(dbconnid, OCIAttrGet(dbconnid->stmthp, OCI_HTYPE_STMT, vnr, 0, 
				      OCI_ATTR_ROW_COUNT, dbconnid->errhp))
	== OCI_SUCCESS) {
      for (i=0; i<*vnr; i++) { /* inserisco i dati mancanti stile libsim */
 	if (olt1[i] == -999) {
	  olt1[i] = *imiss;
	  ol1[i] = *imiss;
	} else {
	  if (ol1[i] == -999) ol1[i] = *imiss;
	}
	if (olt2[i] == 0 || olt2[i] == -999) { /* in Oracle qui c'e` 0 al posto di mancante */
	  olt2[i] = *imiss;
	  ol2[i] = *imiss;
	} else {
	  if (ol2[i] == -999) ol2[i] = *imiss;
	}
	if (opind[i] == -999) opind[i] = *imiss;
	if (op1[i] == -999) op1[i] = *imiss;
	if (op2[i] == -999) op2[i] = *imiss;
      }
    } else {
      *vnr = 0;
    }
    ret = 0;
  } else {
    *vnr = 0;
    ret = 2;
  }
  return ret;
}

int countrows(OracleDbConnection *dbconnid) {
  int nr;
  sword status;
  
  /* Lancio l'estrazione */
  if ((status = OCIStmtExecute(dbconnid->svchp, dbconnid->stmthp, dbconnid->errhp,
			       (ub4) 0, (ub4) 0,
			       (CONST OCISnapshot *) NULL, (OCISnapshot *) NULL,
 			       OCI_STMT_SCROLLABLE_READONLY)) != OCI_SUCCESS) {
    if (status != OCI_NO_DATA) {
      checkerr(dbconnid, status);
      return -1;
    }
  }
  /* Vado all'ultima riga e chiedo dove sono */
  if ((status=OCIStmtFetch2(dbconnid->stmthp, dbconnid->errhp, (ub4) 1,
			    (ub2) OCI_FETCH_LAST, (sb4) 0,
			    (ub4) OCI_DEFAULT)) != OCI_SUCCESS) {
    if (status != OCI_NO_DATA) {
      checkerr(dbconnid, status);
      return -1;
    }
  }
  if (checkerr(dbconnid, OCIAttrGet(dbconnid->stmthp, OCI_HTYPE_STMT, &nr, 0, 
				    OCI_ATTR_CURRENT_POSITION, dbconnid->errhp))
      != OCI_SUCCESS) return -1;
  return nr;
}


sword checkerr(OracleDbConnection *dbconnid, sword status)
{
  text errbuf[MSGLEN];
  sb4 errcode = 0;

  switch (status)
  {
  case OCI_SUCCESS:
    break;
  case OCI_SUCCESS_WITH_INFO:
    strcpy(dbconnid->errmsg, "Oracle error, OCI_SUCCESS_WITH_INFO");
    break;
  case OCI_NEED_DATA:
    strcpy(dbconnid->errmsg, "Oracle error, OCI_NEED_DATA");
     break;
  case OCI_NO_DATA:
    strcpy(dbconnid->errmsg, "Oracle error, OCI_NODATA");
    break;
  case OCI_ERROR:
    if (dbconnid) {
      OCIErrorGet(dbconnid->errhp, (ub4) 1, (text *) NULL, &errcode,
		  errbuf, (ub4) sizeof(errbuf), OCI_HTYPE_ERROR);
      errbuf[MSGLEN-1] = 0;
      snprintf(dbconnid->errmsg, MSGLEN, "Oracle error, %s", errbuf);
    }
    break;
  case OCI_INVALID_HANDLE:
    strcpy(dbconnid->errmsg, "Oracle error, OCI_INVALID_HANDLE");
    break;
  case OCI_STILL_EXECUTING:
    strcpy(dbconnid->errmsg, "Oracle error, OCI_STILL_EXECUTE");
    break;
  case OCI_CONTINUE:
    strcpy(dbconnid->errmsg, "Oracle error, OCI_CONTINUE");
    break;
  default:
    dbconnid->errmsg[0] = 0;
    break;
  }
  return status;
}

