 // Copyright (C) 2011  ARPA-SIM <urpsim@smr.arpa.emr.it>
 // authors:

 // Emanuele Di Giacomo <edigiacomo@arpa.emr.it>
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
#include <vapor/WaveletBlock3DRegionWriter.h>
#include <vapor/WaveCodecIO.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

static VAPoR::MetadataVDC *__md = NULL;
//static VAPoR::WaveletBlock3DBufWriter *__wr = NULL;
static VAPoR::VDFIOBase *__wr = NULL;

int set_variable_names(char *names, size_t len, size_t n)
{
	std::vector<std::string> v;
	int i;
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


int set_variables_2d_xy(char *names, size_t len, size_t n)
{
	std::vector<std::string> v;
	int i;
	char *p;

	p=names;

	for (i = 0; i < n; i++) {
	  v.push_back(p);
	  p+=len;
	}
	if ((i = __md->SetVariables2DXY(v)) < 0) {
	  return i;
	}
	return 0;
}


int set_missing_value_c(double missingv )
{
	
  /* missing value */

  if ((__md->SetMissingValue(missingv)) < 0) {
    return -1;
  }
  return 0;

}


int get_missing_value_c(double* missingv)
{
	
  /* get missing value */

 if (__wr->GetMissingValue().size() == 1) {
   *missingv= __wr->GetMissingValue()[0] ;
  return 0;
  }
  return -1;
}



// those are called by fortran

int vdf4f_set_comment_c(char *comment)
{

	int i;

	if ((i = __md->SetComment(comment)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_ts_comment_c( size_t ts, char *comment)
{

	int i;

	if ((i = __md->SetTSComment(ts,comment)) < 0) {
	  return -1;
	}
	return 0;
}

int vdf4f_set_v_comment_c( size_t ts, char *var, char *comment)
{

	int i;

	if ((i = __md->SetVComment(ts,var,comment)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_coord_system_type_c(char *coordsystemtype)
{

	int i;

	if ((i = __md->SetCoordSystemType(coordsystemtype)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_grid_type_c(char *gridtype)
{

	int i;

	if ((i = __md->SetGridType(gridtype)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_map_projection_c(char *mapprojection)
{

	int i;

	if ((i = __md->SetMapProjection(mapprojection)) < 0) {
	  return -1;
	}
	return 0;
}


int vdf4f_set_grid_extents_c(double extents[6])
{

	int i;
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

int create_metadata_c(size_t xyzdim[3], int vdctype)

/*
VDC Type I

In a type I data collection each netCDF file contains the wavelet coefficients associated with a 
single wavelet transformation pass applied to a single field variable, at a single time step. 
For example, applying two transformation passes to the variable, vx, from the first time step in 
a data collection would result in the generation of three netCDF files with the extensions .nc0, .nc1, and .nc2.
 The first file, .nc0, contains the wavelet coefficients necessary to reconstruct vx at its coarsest 
approximation level (1/4 the original grid resolution along each coordinate axes). 
The .nc1 file provides the wavelet coefficients necessary to reconstruct the vx variable at ½ the original resolution, etc.

VDC Type II

A type II data collection is more complicated than a type I, but offers higher quality compression for 
a given storage budget. In a type II collection field data again undergo a wavelet transformation. 
The resulting wavelet coefficients are than sorted into a small number of ordered groups. 
The original data can be exactly reconstructed (up to floating point round-off) from the coefficients 
in all the groups, or an approximation of the data can be generated from a subset of the groups. 
As with type I collections the lowest order, but most information containing group, is stored in a 
netCDF file with the extention .nc0. The next most information containing group is stored in a netCDF
 file with the extension .nc1, and so on.

VDC Type I or II?

There are tradeoffs when deciding whether to use a type I or type II VDC encoding. 
Each available approximation in a type I VDC coresponds to a level in a hierarchical represenation 
of the sampling grid. Sample values at successively coarser grids are constructed by averaging 
neighboring grid points from parent grids. By virtue of containing fewer grid points a coarsened 
grid requires less space (both on disk and in when stored in memory), less IO bandwidth, and less 
computation when any data processing operators are applied. A type II collection examines each 
wavelet coefficient and groups it based on its information content in terms of the L2 error norm. 
Thus for a given number of wavelet coefficients the type II representation is guaranteed to provide 
the highest quality reconstruction based on the L2 error. However, type II collections require storage 
overhead to address the wavelet coefficients. Hence, a type II VDC will be larger than a type I VDC if 
all coefficients are kept.

A VDC, type I or II, is considered valid even if finer approximation levels are missing. 
In the above examples, the user may choose to store the .nc2 coefficients off line in order to save space. 
Furthermore, a VDC is valid even if entire time steps or variables are not present on disk. 
The goal of supporting incomplete VDCs is to provide the user the flexibility needed to manage very large data sets.  
Hence a minimal valid VDC consists only of a metadata .vdf file, and no field data.
*/

{

  size_t num_transforms = 0;
  const size_t bs[3] = { 64, 64, 64};
  int nfiltercoef = 1;
  int nliftingcoef = 1;
  int mbsfirst = 1;
  
  vector <size_t> cratios;
  cratios.push_back(1);
  cratios.push_back(10);
  cratios.push_back(100);
  //cratios.push_back(500);
  string wname;
  string wmode;

  wmode="symh";
  wname="bior3.3";
    
  /* create metadata */

  if (vdctype == 1) {

    __md = new VAPoR::MetadataVDC(xyzdim, num_transforms, bs, nfiltercoef,
  				nliftingcoef, mbsfirst);
  }
  else if  (vdctype == 2) {

    __md = new VAPoR::MetadataVDC(xyzdim, bs, cratios, wname,wmode);

  }
  else {

    __md = NULL;
    return -1;

  }

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


int create_writer_c(char filename[])
{

  // todo : manage type 1 vdc
/* create writer starting from metadata already done */
  //__wr = new VAPoR::WaveletBlock3DBufWriter(filename);
  __wr = new VAPoR::WaveCodecIO(filename);
 

 if ((VAPoR::MetadataVDC::GetErrCode())) {
    __wr = NULL;
    return -1;
  }

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
	
  /* timesteps number */

  if ((__md->SetNumTimeSteps(ntime)) < 0) {
    return -1;
  }
  return 0;

}


int set_variables_names_c(size_t nvar, char varnames[], size_t len )
{

  /* insert variable name 3D */

  if (set_variable_names(varnames, len, nvar) != 0) {
    return -1;
  }
  return 0;
  
}

int set_variables_2d_xy_c(size_t nvar, char varnames[], size_t len )
{

  /* insert variable name 2D */

  if (set_variables_2d_xy(varnames, len, nvar) != 0) {
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
		  char varnames[], size_t len, int rzscan)
{
  float *slice, *reverseslice, *myslice ;
  size_t i, j, k, xydim ;
  
  xydim = xyzdim[0] * xyzdim[1] ;

  slice = volume ;
  
  /* each variable */
  for (i = 0; i < nvar; i++) {
    /* each timestep */
    for (j = 0; j < ntime; j++) {
      /* prepare writer to write 
       * i variable at j timestep */
      //cerr << "check: " <<      	     varnames+(i*len) << endl;
      if ((__wr->OpenVariableWrite(j, varnames+(i*len), -1,-1)) < 0) {
	return -1;
      }
      

      reverseslice = slice + (xydim * (xyzdim[2] - 1)) ;

      /* slice for i variable
       * at j timestep */
      for (k = 0 ; k < xyzdim[2] ; k++) {

	if ( rzscan ) {
	  myslice = reverseslice ;
	    }
	else{
	  myslice = slice ;
	}

	if (( __wr->WriteSlice(myslice)) < 0) {
	  return -2;
	}
	
	slice += xydim ;
	reverseslice -= xydim ;
	
      }
      /* close */
      __wr->CloseVariable();
    }
  }

  return 0;
}



int vdf4f_write_2d_xy_c(float *volume,
		  size_t xydim[2], size_t ntime , size_t nvar , 
		  char varnames[], size_t len )
{
  float *slice ;
  size_t i, j, k, xydimtot ;
  
  xydimtot = xydim[0] * xydim[1] ;

  slice = volume ;
  
  /* each variable */
  for (i = 0; i < nvar; i++) {
    /* each timestep */
    for (j = 0; j < ntime; j++) {
      /* prepare writer to write 
       * i variable at j timestep */
      	    
      if ((__wr->OpenVariableWrite(j, varnames+(i*len), -1, -1)) < 0) {
	printf ("OpenVariableWrite error\n");
	return -1;
      }
      
      //printf ("%s\n",varnames+(i*len));

      /* slice for i variable
       * at j timestep */
      if (( __wr->WriteRegion(slice)) < 0) {
	printf("WriteRegion error\n");
	return -1;
      }
	
      slice += xydimtot ;
	
      /* close */
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

