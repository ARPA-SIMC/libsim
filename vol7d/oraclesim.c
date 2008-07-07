#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <oci.h>
#include <alloca.h>
#include "config.h"

#define DATELEN 13
#define CVALLEN 257
#define FLAGLEN 10
#define TABLEN 16
#define MSGLEN 256

/* Define SQL statements to be used in program. */
static char *tabquery = (char *)
  "SELECT \
RTRIM(DECODE(gs.id_tabvar_mod,'Y',tv1.nome_tabella,'N',tv2.nome_tabella)) \
FROM met_gruppi_stazioni gs, met_variabili_definite va, \
     met_tabelle_vm tv1, met_tabelle_vm tv2 \
WHERE gs.identnr = :net AND va.identnr = :var AND \
      tv1.id_tabella = gs.id_tabella_vm AND tv2.id_tabella = va.id_tabella_vm";

static char *query1 = (char *)
  "SELECT TO_CHAR(a.dset_istante_wmo_fine,'YYYYMMDDHH24MI'), \
a.dset_stam_identnr,a.vard_identnr, \
a.valore_principale,a.valore_ausiliario \
FROM ";

/* ,a.valore_chiaro,a.flag \ */

static char *query2 = (char *)
  " a WHERE \
dset_istante_wmo_fine >= TO_DATE(:ti,'YYYYMMDDHH24MI') AND \
dset_istante_wmo_fine <= TO_DATE(:tf,'YYYYMMDDHH24MI') AND \
EXISTS(SELECT /*+use_nl*/ s.identnr \
from stazioni_misura s WHERE s.gsta_identnr = :net AND \
a.dset_stam_identnr = s.identnr) AND a.vard_identnr";

static char *query3 = (char *)
  " ORDER BY dset_istante_wmo_fine,dset_stam_identnr";

/* Struttura per tenere una connessione oracle e fare login una volta sola. */
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
  OCIBind *bnd1p;
  OCIBind *bnd2p;
  OCIBind *bnd3p;
  OCIBind *bnd4p;
  char table[TABLEN];
  char errmsg[MSGLEN];

} OracleDbConnection;

OracleDbConnection *FC_FUNC_(oraextra_init, ORAEXTRA_INIT)
     (char *, char *, char *, int*);
int FC_FUNC_(oraextra_gethead, ORAEXTRA_GETHEAD)
     (OracleDbConnection **, char *, char *, int *, int *, int *);
int FC_FUNC_(oraextra_getdata, ORAEXTRA_GETDATA)
     (OracleDbConnection **, int *, int *, char *, int *, int *,
      float *, float */* , char *, char * */, float *);
void FC_FUNC_(oraextra_delete, ORAEXTRA_DELETE)
     (OracleDbConnection **);
void FC_FUNC_(oraextra_geterr, ORAEXTRA_GETERR)
     (OracleDbConnection **, char *);
static sword checkerr(OracleDbConnection *, sword);


OracleDbConnection *FC_FUNC_(oraextra_init, ORAEXTRA_INIT)
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
			     (dvoid **) 0) != OCI_SUCCESS)) {
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


int FC_FUNC_(oraextra_gethead, ORAEXTRA_GETHEAD)
     (OracleDbConnection **fdbconnid, char *ti, char *tf,
      int *net, int *var, int *nvar) {
  OracleDbConnection *dbconnid = *fdbconnid;
  int i, nr;
  sword status;
  char *query;

  sb2 ti_ind, tf_ind, net_ind, var_ind, otab_ind;
  sb2 ovalp_ind, ovala_ind;
  char odate[DATELEN]/* , ovalc[CVALLEN], oflg[CVALLEN] */;
  int ostatid, ovarid;
  float ovalp, ovala;

  /* Preparo l'estrazione per ottenere il nome della tabella,
     complicazione per colpa di CAELAMI */
  checkerr(dbconnid, OCIStmtPrepare(dbconnid->stmthp, dbconnid->errhp,
				    (text *) tabquery,
				    (ub4) strlen(tabquery),
				    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));

  /* valori validi =1, mancanti =-1 */
  net_ind = 1;
  var_ind = 1;

  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd3p,
				   dbconnid->errhp, (text *) ":net",
				   -1, (dvoid *) net,
				   (sword) sizeof(*net), SQLT_INT, (dvoid *) &net_ind,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd4p,
				   dbconnid->errhp, (text *) ":var",
				   -1, (dvoid *) var,
				   (sword) sizeof(*var), SQLT_INT, (dvoid *) &var_ind,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn1p,
				    dbconnid->errhp, 1,
				    (dvoid *) &dbconnid->table, TABLEN, SQLT_STR,
				    (dvoid *) &otab_ind,
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
  if ((status=OCIStmtFetch2(dbconnid->stmthp, dbconnid->errhp, (ub4) 1,
			    (ub2) OCI_FETCH_NEXT, (sb4) 0,
			    (ub4) OCI_DEFAULT)) != OCI_SUCCESS) {
    if (status == OCI_NO_DATA) {
      return -1;
    }
    else {
      checkerr(dbconnid, status);
      return -1;
    }
  }

  
  /* Ricostruisco la query interessante con il nome della tabella ottenuto */
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

  /* Preparo l'estrazione interessante */
  checkerr(dbconnid, OCIStmtPrepare(dbconnid->stmthp, dbconnid->errhp,
				    (text *) query,
				    (ub4) strlen(query),
				    (ub4) OCI_NTV_SYNTAX, (ub4) OCI_DEFAULT));


  /* valori validi =1, mancanti =-1 */
  var_ind = 1;
  ti_ind = 1;
  tf_ind = 1;
  net_ind = 1;
  
  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd1p,
				   dbconnid->errhp, (text *) ":ti",
				   -1, (dvoid *) ti,
				   DATELEN, SQLT_STR, (dvoid *) &ti_ind,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd2p,
				   dbconnid->errhp, (text *) ":tf",
				   -1, (dvoid *) tf,
				   DATELEN, SQLT_STR, (dvoid *) &tf_ind,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  checkerr(dbconnid, OCIBindByName(dbconnid->stmthp, &dbconnid->bnd3p,
				   dbconnid->errhp, (text *) ":net",
				   -1, (dvoid *) net,
				   (sword) sizeof(*net), SQLT_INT, (dvoid *) &net_ind,
				   (ub2 *) 0, (ub2 *) 0, (ub4) 0, (ub4 *) 0,
				   OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn1p,
 				    dbconnid->errhp, 1,
 				    (dvoid *) &odate, DATELEN, SQLT_STR,
 				    (dvoid *) 0,
 				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn2p,
				    dbconnid->errhp, 2,
				    (dvoid *) &ostatid, sizeof(ostatid), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn3p,
				    dbconnid->errhp, 3,
				    (dvoid *) &ovarid, sizeof(ovarid), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn4p,
				    dbconnid->errhp, 4,
				    (dvoid *) &ovalp, sizeof(ovalp), SQLT_FLT,
				    (dvoid *) &ovalp_ind,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn5p,
				    dbconnid->errhp, 5,
				    (dvoid *) &ovala, sizeof(ovala), SQLT_FLT,
				    (dvoid *) &ovala_ind,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

/*   checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn6p, */
/* 				    dbconnid->errhp, 6, */
/* 				    (dvoid *) &dbconnid->ovalc, CVALLEN, SQLT_STR, */
/* 				    (dvoid *) &dbconnid->ovalc_ind, */
/* 				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT)); */

/*   checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn7p, */
/* 				    dbconnid->errhp, 7, */
/* 				    (dvoid *) &dbconnid->oflg, CVALLEN, SQLT_STR, */
/* 				    (dvoid *) &dbconnid->oflg_ind, */
/* 				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT)); */

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


int FC_FUNC_(oraextra_getdata, ORAEXTRA_GETDATA)
     (OracleDbConnection **fdbconnid, int *nr, int *vnr, 
      char *odate, int *ostatid, int *ovarid,
      float *ovalp, float *ovala /*, char *ovalc, char *oflg */, float *rmiss) {
  OracleDbConnection *dbconnid = *fdbconnid;
  sword status = OCI_SUCCESS;
  sb2 *ovalp_ind, *ovala_ind;
  int i, ret;

  ovalp_ind = malloc((*nr)*sizeof(*ovalp_ind));
  ovala_ind = malloc((*nr)*sizeof(*ovala_ind));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn1p,
 				    dbconnid->errhp, 1,
 				    (dvoid *) odate, DATELEN, SQLT_STR,
 				    (dvoid *) 0,
 				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn2p,
				    dbconnid->errhp, 2,
				    (dvoid *) ostatid, sizeof(ostatid[0]), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn3p,
				    dbconnid->errhp, 3,
				    (dvoid *) ovarid, sizeof(ovarid[0]), SQLT_INT,
				    (dvoid *) 0,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn4p,
				    dbconnid->errhp, 4,
				    (dvoid *) ovalp, sizeof(ovalp[0]), SQLT_FLT,
				    (dvoid *) ovalp_ind,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  checkerr(dbconnid, OCIDefineByPos(dbconnid->stmthp, &dbconnid->defn5p,
				    dbconnid->errhp, 5,
				    (dvoid *) ovala, sizeof(ovala[0]), SQLT_FLT,
				    (dvoid *) ovala_ind,
				    (ub2 *) 0, (ub2 *) 0, OCI_DEFAULT));

  status=checkerr(dbconnid, OCIStmtFetch2(dbconnid->stmthp, dbconnid->errhp,
					  (ub4) *nr, OCI_FETCH_FIRST, (sb4) 0,
					  (ub4) OCI_DEFAULT));
  if (status == OCI_SUCCESS || status == OCI_NO_DATA) {
    if (checkerr(dbconnid, OCIAttrGet(dbconnid->stmthp, OCI_HTYPE_STMT, vnr, 0, 
				      OCI_ATTR_ROW_COUNT, dbconnid->errhp))
	== OCI_SUCCESS)
      for (i=0; i<*vnr; i++) {
	if (ovalp_ind[i] == -1) ovalp[i] = *rmiss;
	if (ovala_ind[i] == -1) ovala[i] = *rmiss;
      }
    ret = 0;
  } else {
    *vnr = 0;
    ret = 2;
  }
  free(ovalp_ind);
  free(ovala_ind);
  return ret;
}


void FC_FUNC_(oraextra_delete, ORAEXTRA_DELETE) (OracleDbConnection **fdbconnid) {
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


void FC_FUNC_(oraextra_geterr, ORAEXTRA_GETERR)
     (OracleDbConnection **fdbconnid, char *errmsg) {
  if (*fdbconnid == NULL)
    strcpy(errmsg, "Oracle error, allocation problem");
  else
    strcpy(errmsg, (*fdbconnid)->errmsg);
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

