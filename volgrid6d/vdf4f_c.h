#ifndef VDF4F_H
#define VDF4F_H

#include <sys/types.h>

/**
 * create_metadata() - create metadata
 * @xyzdim:		X, Y, and Z coordinate dimensions of all data volumes, 
 * 			specified in grid coordinates (voxels)
 * @num_transforms:	number of wavelet transforms to perform
 * @bs:			internal blocking factor for transformed data
 * @nfiltercoef:	number of filter coefficients
 * @nliftingcoef:	number of lifting coefficients
 * @mbsfirst:		if true storage order for volume data will be most 
 * 			significant byte fist
 * @errmsg:		on failure, human readable message, else NULL. free this
 * 			pointer.
 *
 * Creates a __static__ metadata (see destroy_metadata())
 */
int create_metadata(size_t xyzdim[3], size_t num_transforms, size_t bs[3],
		    int nfiltercoef, int nliftingcoef, int mbsfirst,
		    const char **errmsg);

/**
 * create_writer() - creates a static writer
 * @metapath:	metadata path
 * @errmsg:	on failure, human readable message, else NULL. free this pointer
 */
int create_writer(const char *metapath, const char **errmsg);

/**
 * open_variable_write() - open the named variable for writing
 * @timestep:	time step of the variable to read 
 * @varname:	name of the variable to read 
 * @reflevel:	refinement level of the variable (-1: max refinement)
 * @errmsg:	on failure, human readable message, else NULL. free this pointer
 */
int open_variable_write(size_t timestep, const char *varname,
			int reflevel, const char **errmsg);
/**
 * close_variable_write() - close the currently opened variable
 */
int close_variable_write();
/**
 * write_slice() - transform and write a single slices of voxels
 * @slice:	a slices of volume data
 * @errmsg:	on failure, human readable message, else NULL. free this pointer
 *
 * Note: this function should be called exactly NZ times for each opened
 * variable, where NZ is the dimension of the volume in voxels along the Z
 * axis. Each invocation should pass a successive slice of volume data
 */
int write_slice(const float *slice, const char **errmsg);


/**
fortran interface
 **/
#ifdef  __cplusplus
extern "C" {
#endif

  int create_metadata_c(size_t xyzdim[3]);
  
/**
 * set_num_timesteps() - set the number of time steps
 * @values:	number of time steps
 * @errmsg:	on failure, human readable message, else NULL. free this pointer
 */
  int set_num_timesteps_c(size_t ntime );

/**
 * set_variable_names() - set the names of the field variables
 * @nvar:	array dimension
 * @varnames:	array of names
 * @len:	len of varnames
 * @errmsg:		on failure, human readable message, else NULL. free this
 */
  int set_variables_names_c(size_t nvar, char varnames[], size_t len );

  int vdf4f_set_comment_c(char *comment);
  int vdf4f_set_ts_comment_c(size_t ts, char *comment);
  int vdf4f_set_v_comment_c(size_t ts, char *var, char *comment);

  int vdf4f_set_grid_extents_c(double extents[6]);

  int vdf4f_set_coord_system_type_c(char *coordsystemtype);

  int vdf4f_set_grid_type_c(char *gridtype);

  int vdf4f_set_map_projection_c(char *mapprojection);

  //  int vdf4f_set_grid_permutation(long permutation[3]);

/**
 * write_metadata() - write metadata object to a file
 * @path:	name of the file to write to 
 * @errmsg:	on failure, human readable message, else NULL. free this pointer
 */
  int write_metadata_c(char filename[]);

/**
 * destroy_metadata() - destroy metadata
 */
  int destroy_metadata_c();

  int vdf4f_write_c(float *volume,
		size_t xyzdim[3], size_t ntime ,size_t nvar , 
		  char varnames[], size_t len, char filename[]);

/**
 * destroy_writer() - destroy writer
 */
  int destroy_writer_c();


  int get_err_msg_c(char *errmsg, size_t *len );


#ifdef  __cplusplus
}
#endif

#endif
