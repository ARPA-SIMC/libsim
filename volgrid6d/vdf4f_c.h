#ifndef VDF4F_H
#define VDF4F_H

#include <sys/types.h>


/**
fortran interface
 **/
#ifdef  __cplusplus
extern "C" {
#endif


  int set_variable_names(char *names, size_t len, size_t n);
  int set_variables_2d_xy(char *names, size_t len, size_t n);

  int vdf4f_set_comment_c(char *comment);
  int vdf4f_set_ts_comment_c( size_t ts, char *comment);
  int vdf4f_set_v_comment_c( size_t ts, char *var, char *comment);
  int vdf4f_set_coord_system_type_c(char *coordsystemtype);
  int vdf4f_set_grid_type_c(char *gridtype);
  int vdf4f_set_map_projection_c(char *mapprojection);
  int vdf4f_set_grid_extents_c(double extents[6]);
  //int vdf4f_set_grid_permutation_c(long permutation[3]);
  int create_metadata_c(size_t xyzdim[3], int vdctype);
  int create_writer_c(char filename[]);
  int destroy_metadata_c();
  int destroy_writer_c();
  int set_num_timesteps_c(size_t ntime );
  int set_variables_names_c(size_t nvar, char varnames[], size_t len );
  int set_variables_2d_xy_c(size_t nvar, char varnames[], size_t len );
  int write_metadata_c(char filename[]);
  int vdf4f_write_c(float *volume,
		  size_t xyzdim[3], size_t ntime , size_t nvar , 
		    char varnames[], size_t len, int rzscan);
  int vdf4f_write_2d_xy_c(float *volume,
		  size_t xydim[2], size_t ntime , size_t nvar , 
			  char varnames[], size_t len );
  int set_missing_value_c(double missingv );
  int get_missing_value_c(double* missingv);
  int get_err_msg_c(char *errmsg, size_t *len );


#ifdef  __cplusplus
}
#endif

#endif
