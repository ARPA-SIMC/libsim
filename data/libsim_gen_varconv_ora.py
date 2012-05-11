#!/usr/bin/python

import csv
import subprocess
import sys


def UniConvTable():
    """Restituisce un dizionario basato sulla tabella di conversione
    delle unita` di misura, presa dai sorgenti di dballe
    wreport/trunk/wreport/uniconv.gperf di Enrico Zini. La tabella e`
    incollata cosi` com'e` senza riformattazioni salvo convertire
    -273.15001f -> -273.1500 & c. e puo` essere riaggiornata quasi al
    suo posto"""

    uniconvtable = {}
    uniconvsrc = """
"K->C",				1,	-273.1500
"C->K",				1,	273.1500
"K->C/10",			10,	-2731.500
"C/10->K",			0.1,	273.1500
"C->C/10",			10,	0
"C/10->C",			0.1,	0
"YEARS->YEAR",			1,	0
"YEAR->YEARS",			1,	0
"MONTHS->MONTH",		1,	0
"MONTH->MONTHS",		1,	0
"DAYS->DAY",			1,	0
"DAY->DAYS",			1,	0
"HOURS->HOUR",			1,	0
"HOUR->HOURS",			1,	0
"MINUTES->MINUTE",		1,	0
"MINUTE->MINUTES",		1,	0
"SECONDS->SECOND",		1,	0
"SECOND->SECONDS",		1,	0
"SECOND->S",			1,	0
"S->SECOND",			1,	0
"sec->S",			1,	0
"S->sec",			1,	0
"minuti->S",                    60,     0
"G/G->KG/KG",			1,	0
"KG/KG->G/G",			1,	0
"G/M**3->KG/M**3",		0.001,	0
"KG/M**3->G/M**3",		1000,	0
"PA->KPA",			0.001,	0
"KPA->PA",			1000,	0
"m**(2/3)/S->M**(2/3)/S",	1,	0
"M**(2/3)/S->m**(2/3)/S",	1,	0
"M->MM",			1000,	0
"MM->M",			0.001,	0
"M->cm",			100,	0
"cm->M",			0.01,	0
"%->PERCENT",			1,	0
"PERCENT->%",			1,	0
"M->FT",			3.2808,	0
"FT->M",			0.3048,	0
"DEGREE**2->DEGREE2",		1,	0
"DEGREE2->DEGREE**2",		1,	0
"KG/M**2->KGM-2",		1,	0
"KGM-2->KG/M**2",		1,	0
"J/M**2->JM-2",			1,	0
"JM-2->J/M**2",			1,	0
"cal/cm**2->J/M**2",		41868,	0
"J/M**2->cal/cm**2",		0.000023885,	0
"Bq/L->BQ L-1",			1,		0
"BQ L-1->Bq/L",			1,		0
"DOBSON->DU",			1,		0
"DU->DOBSON",			1,		0
"PA->NBAR",			0.0001,		0		
"NBAR->PA",			10000,		0
"LOG(1/M**2)->LOG (M-2)",	1,		0
"LOG (M-2)->LOG(1/M**2)",	1,		0
"DEGREE TRUE->gsess",		1,		0
"gsess->DEGREE TRUE",		1,		0
"m/sec->M/S",  			1,		0
"m/s/10->M/S",			0.1,		0
"M/S->m/s/10",			10,		0
"nodi->M/S",			0.51444,	0
"M/S->nodi",			1.94384,	0
"PA->mBar",			0.01,		0
"mBar->PA",			100,		0
"PA->hPa",			0.01,		0
"hPa->PA",			100,		0
"PA->Bar",			0.00001,	0
"Bar->PA",			100000,		0
"m->M",				1,		0
"M->m",				1,		0
"hm->M",			100,		0
"M->hm",			0.01,		0
"mm->M",			0.001,		0
"M->mm",			1000,		0
"1/8->%",			12.5,		0
"%->1/8",			0.08,		0
"mm->KG/M**2",			1,		0
"mm/10->KG/M**2",		0.1,		0
"KG/M**2->mm",			1,		0
"degree true->DEGREE TRUE",	1,		0
"DEGREE TRUE->degree true",	1,		0
"GPM->MGP",			1,		0
"MGP->GPM",			1,		0
"GPM->m**2/s**2",		9.80665,	0
"GPM->M**2/S**2",		9.80665,	0
"MGP->m**2/s**2",		9.80665,	0
"MGP->M**2/S**2",		9.80665,	0
"m**2/s**2->GPM",		0.101971621,	0
"M**2/S**2->GPM",		0.101971621,	0
"m**2/s**2->MGP",		0.101971621,	0
"M**2/S**2->MGP",		0.101971621,	0
"cal/s/cm**2->W/M**2",		41868,		0
"cal/h/cm**2->W/M**2",		11.63,		0
"""

# problemi di dballe:
# (45.,-22.5?) 230,B11001,ottavi->DEGREE TRUE,1.0,0.0
# ci metto una pezza temporanea
    uniconvsrc = uniconvsrc + """
"ottavi->DEGREE TRUE", 45., -22.5
"""

    for row in csv.reader(uniconvsrc.split('\n'), delimiter=","):
        if len(row) > 0:
            uniconvtable[row[0]] = (float(row[1]), float(row[2]))
    return uniconvtable


def UnitFromOracle(hack=True, allvar=False):
    """Restituisce una lista (ordinata per oracleid crescente) di dizionari,
    estratti dal db oracle, con le chiavi:
    - "oracleid"
    - "blocal"
    - "umis_princ"
    - "umis_ausil"
    - "umis_abbr"
    - "description"
    """
    oratable = []
    pollprevhack = {}
    query = """
set lines 350
set pages 10000
set verify off
set feed off
set heading off
set colsep ','
col IDENT format 9999
--col BLOCAL format A6
--v.UMIS_CODICE_AUSILIARIO
SELECT v.IDENTNR IDENT, v.BLOCAL_NEW BLOCAL, v.UMIS_CODICE_PRINCIPALE,
       v.UMIS_CODICE_AUSILIARIO, u.ABBREVIAZIONE, v.DESCRIZIONE
FROM MET_VARIABILI_DEFINITE v, MET_UNITA_MISURA u
--WHERE v.BLOCAL_NEW != ' ' AND v.BLOCAL_NEW IS NOT NULL AND v.UMIS_CODICE_PRINCIPALE = u.CODICE
--WHERE ((v.BLOCAL_NEW != ' ' AND v.BLOCAL_NEW IS NOT NULL) OR (v.IDENTNR >= 1360 AND v.IDENTNR <= 1402)) AND v.UMIS_CODICE_PRINCIPALE = u.CODICE
WHERE v.UMIS_CODICE_PRINCIPALE = u.CODICE
ORDER BY IDENTNR;
EXIT;
"""
    sqlplus = subprocess.Popen(["sqlplus","-S","leggo/meteo@metw"], 
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    sqlplus.stdin.write(query)
    for row in csv.reader(sqlplus.stdout, delimiter=","):
        if len(row) != 6: continue
#        if row[1].rstrip() == '' and not allvar and \
#                (int(row[0]) < 1360 or int(row[0]) > 1402 or not hack): continue
        if row[1].rstrip() == '' and not allvar: continue
# sporca conversione per il transiente
        if hack:
            if row[1].rstrip() == 'B10061': row[1] = 'B10060'
            elif row[1].rstrip() == 'B12001': row[1] = 'B12101'
            elif row[1].rstrip() == 'B12003': row[1] = 'B12103'
            elif row[1].rstrip() == 'B22042': row[1] = 'B22043'
            elif row[1].rstrip() == 'B10009': row[1] = 'B10008'
        oratable.append({"oracleid": int(row[0]),
                         "blocal": row[1].rstrip(),
                         "umis_princ": row[2].rstrip(),
                         "umis_ausil": row[3].rstrip(),
                         "umis_abbr": row[4].rstrip(),
                         "description": row[5]})

        if oratable[-1]["oracleid"] >= 360 and oratable[-1]["oracleid"] <= 402:
            pollprevhack[oratable[-1]["oracleid"]] = oratable[-1]
    
    sqlplus.wait()
    # if hack:
    #     for row,ind in zip(oratable,range(len(oratable))):
    #         if row["blocal"] == "":
    #             if row["oracleid"] >= 1360 and row["oracleid"] <= 1402:
    #                 try:
    #                     tmp = pollprevhack[row["oracleid"]-1000]["blocal"]
    #                     oratable[ind]["blocal"] = tmp
    #                 except:
    #                     print "Error:, the table has changed, please modify",sys.argv[0]
    #                     raise
    return oratable


def UnitFromDballe(file="/usr/share/wreport/dballe.txt"):
    """Legge un file in formato dballe.txt e restituisce un
    dizionario (con chiave codice blocal) di dizionari con le chiavi:
    - "unit1"
    - "unit2"
    """
    dbatable = {}
    for line in open(file).readlines():
        dbatable["B"+line[2:7]] = {
            "unit1": line[73:97].rstrip(),
            "unit2": line[119:143].rstrip()}
    return dbatable


# main
fortran = False
hack = True
allvar = False
for arg in sys.argv[1:]:
    if arg == "-f": fortran = True # fortran format
    elif arg == "-n": hack = False # do not apply hacks
    elif arg == "-a": allvar = True # print all vars, including those w/o B

# riempi le tabelle
zinitable = UniConvTable()
oratable = UnitFromOracle(hack, allvar)
dbatable = UnitFromDballe()

if fortran:
    print "ALLOCATE(vartable_s(%d))" % (oratable[-1]["oracleid"],)
    fortran_lastind = 0
else:
    output = csv.writer(sys.stdout, delimiter=",")

# ciclo sulle variabili oracle
for oraentry in oratable:
    conv = (1.0, 0.0)
    convkey = ''
    if dbatable.has_key(oraentry["blocal"]):
        convkey = oraentry["umis_abbr"]+"->" + dbatable[oraentry["blocal"]]["unit1"]
        if zinitable.has_key(convkey):
            conv = zinitable[convkey]
        else: # secondo tentativo
            convkey = oraentry["umis_abbr"]+"->" + dbatable[oraentry["blocal"]]["unit2"]
            if zinitable.has_key(convkey):
                conv = zinitable[convkey]
    if fortran:
        for ind in range(fortran_lastind+1, oraentry["oracleid"]):
            print "vartable_s(%d) = ora_var_conv_static(cmiss, rmiss, rmiss)" % (ind, )
        print "vartable_s(%d) = ora_var_conv_static('%s', %f, %f)" % \
            (oraentry["oracleid"], oraentry["blocal"], conv[0], conv[1])
        fortran_lastind = oraentry["oracleid"]

    else:
        output.writerow([oraentry["oracleid"], oraentry["blocal"], convkey,
                         conv[0], conv[1],
                         oraentry["umis_abbr"],
                         oraentry["umis_princ"], oraentry["umis_ausil"],
                         oraentry["description"]])


# (1000000.?) 246,B14021,Mjd/m**2->J/M**2,1.0,0.0
# (1000000.?) 247,B14018,Mjd/m**2->W/M**2,1.0,0.0 (248?)

# problemi di Oracle:

# 107 109 110 (cal/cm**2->W/M**2,1.0,0.0)
# rete climat
# (MN / 60?) 249,B14031,,1.0,0.0
# (MM) 250,B13060,,1.0,0.0,
# (MN / 60?) 257,B13035,,1.0,0.0
# (KM / 1000.?) 326,B11192,,1.0,0.0

## (GC / 273.15?) 169,B13082,,1.0,0.0,
## (GC / 273.15?) 451,B12064,,1.0,0.0
## (GC / 273.15?) 633,B22049,,1.0,0.0
## (GC / 273.15?) 661,B13082,,1.0,0.0
