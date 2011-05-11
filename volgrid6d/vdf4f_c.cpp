 // Copyright (C) 2011  ARPA-SIM <urpsim@smr.arpa.emr.it>
 // authors:
 // Davide Cesari <dcesari@arpa.emr.it>
 // Paolo Patruno <ppatruno@arpa.emr.it>

 // This program is free software; you can redistribute it and/or
 // modify it under the terms of the GNU General Public License as
 // published by the Free Software Foundation; either version 2 of 
 // the License, or (at your option) any later version.

 // This program is distributed in the hope that it will be useful,
 // but WITHOUT ANY WARRANTY; without even the implied warranty of
 // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 // GNU General Public License for more details.

 // You should have received a copy of the GNU General Public License
 // along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "vdf4f_c.h"
#include <vapor/Metadata.h>
#include <vapor/WaveletBlock3DBufWriter.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

static VAPoR::MetadataVDC *__md = NULL;
static VAPoR::WaveletBlock3DBufWriter *__wr = NULL;

int set_variable_names(char *names, size_t len, size_t n)
{
	std::vector<std::string> v;
	size_t i;
	char *p;

	p=names;

	for (i = 0; i < n; i++) {
	  v.push_back(p);
	  p+=len;
	}
	if ((i = __md->SetVariableNames(v)) < 0) {
	  return i;
	}
	return 0;
}

// those are called by fortran

int vdf4f_set_comment_c(char *comment)
{

	size_t i;

	if ((i = __md->SetComment(comment)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_ts_comment_c( size_t ts, char *comment)
{

	size_t i;

	if ((i = __md->SetTSComment(ts,comment)) < 0) {
	  return -1;
	}
	return 0;
}

int vdf4f_set_v_comment_c( size_t ts, char *var, char *comment)
{

	size_t i;

	if ((i = __md->SetVComment(ts,var,comment)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_coord_system_type_c(char *coordsystemtype)
{

	size_t i;

	if ((i = __md->SetCoordSystemType(coordsystemtype)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_grid_type_c(char *gridtype)
{

	size_t i;

	if ((i = __md->SetGridType(gridtype)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_map_projection_c(char *mapprojection)
{

	size_t i;

	if ((i = __md->SetMapProjection(mapprojection)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_grid_extents_c(double extents[6])
{

	size_t i;
	std::vector<double> v;

	for (i = 0; i < 6; i++) {
		v.push_back(extents [i]);
	}


	if ((i = __md->SetExtents(v)) < 0) {
		return -1;
	}
	return 0;
}


/*
int vdf4f_set_grid_permutation_c(long permutation[3])
{

	char *errmsg = NULL;
	size_t i;
	std::vector<long> v;

	for (i = 0; i < 3; i++) {
		v.push_back(permutation [i]);
	}


	if ((i = __md->SetGridPermutation(v)) < 0) {
		errmsg = strdup(VAPoR::MetadataVDC::GetErrMsg());
		return -1;
	}
	return 0;
}

*/

int create_metadata_c(size_t xyzdim[3])
{

  size_t num_transforms = 2;
  size_t bs[3] = { 2, 2, 2};
  int nfiltercoef = 1;
  int nliftingcoef = 1;
  int mbsfirst = 1;
  
  /* creazione del metadata */

  __md = new VAPoR::MetadataVDC(xyzdim, num_transforms, bs, nfiltercoef,
				nliftingcoef, mbsfirst);
  if ((VAPoR::MetadataVDC::GetErrCode())) {
    __md = NULL;
    return -1;
  }
  return 0;

}

int destroy_metadata_c()
{
	delete __md;
        __md = NULL;
	return 0;
}

int destroy_writer_c()
{
	delete __wr;
        __wr = NULL;
	return 0;
}

int set_num_timesteps_c(size_t ntime )
{
	
  /* numero di timesteps */

  if ((__md->SetNumTimeSteps(ntime)) < 0) {
    return -1;
  }
  return 0;

}


int set_variables_names_c(size_t nvar, char varnames[], size_t len )
{

  /* inserisco i nomi delle variabili 3D */

  if (set_variable_names(varnames, len, nvar) != 0) {
    return -1;
  }
  return 0;
  
}


int write_metadata_c(char filename[])
{
  
  /* write on file metadata */

  //relpath:	use relative path names for all data paths (1=true)
  if (( __md->Write(filename, 1)) < 0) {
    return -1;
  }
  
  return 0;

}


int vdf4f_write_c(float *volume,
		  size_t xyzdim[3], size_t ntime , size_t nvar , 
		  char varnames[], size_t len ,
		  char filename[])
{
  float *slice;
  size_t i, j, k;
  
  /* creo il writer per i valori a partire dal metadata appena creato */
  __wr = new VAPoR::WaveletBlock3DBufWriter(filename);
  if ((VAPoR::MetadataVDC::GetErrCode())) {
    return -1;
  }
  
  slice = volume ;
  
  /* per ogni variabile: */
  for (i = 0; i < nvar; i++) {
    /* per ogni timestep */
    for (j = 0; j < ntime; j++) {
      /* preparo il writer per la scrittura della
       * i-esima variabile a j-esimo timestep */
      	    
      if ((__wr->OpenVariableWrite(j, varnames+(i*len), -1)) < 0) {
	return -1;
      }
      
      /* passo al writer le slice della i-esima variabile
       * al j-esimo timestep lungo tutto l'asse Z */
      for (k = 0 ; k < xyzdim[2] ; k++) {


	if (( __wr->WriteSlice(slice)) < 0) {
	  return -1;
	}
	
	slice += xyzdim[0] * xyzdim[1] ;
	
      }
      /* chiudo la scrittura sulla i-esima variabile
       * al j-esimo timestep */
      __wr->CloseVariable();
    }
  }

  return 0;
}


int get_err_msg_c(char *errmsg, size_t *len )
{
  const char *errmsgtmp = NULL;

  errmsgtmp = VAPoR::MetadataVDC::GetErrMsg();
  strncpy(errmsg, errmsgtmp, *len);

  *len = strlen(errmsgtmp);


  return 0;
}

