! =============================================================
! FOR2R-TEST.F90     M.H.Prager   May 2005 - Aug 2006
! =============================================================
   ! This is a driver program for testing for2r.f90.
   ! Those routines write [parts of] an R-compatible output object.
   ! This program generates some dummy data & calls those routines.
! =============================================================
! Use subroutines to test any scoping issues  6/16/05
! For2R Routines renamed for similarity to C2R, A. Stephens, 1/2006
! Changed name of INFO object, M. Prager, 8/2006
! Changed KINDs to use default kinds, M. Prager, 8/2006
! =============================================================
   program for2rtest
      use for2r
      implicit none
      integer, parameter            :: r4 = kind(1.0)    !--real single precision
      integer, parameter            :: r8 = kind(1.0d0)  !--real double precision
      integer, parameter            :: nyr=6, nage=5
      integer                       :: i, iyr(nyr), iage(nage)
      real(r8), dimension(nyr)      :: v1, v2, v3
      real(r8), dimension(nyr,nage) :: m1, m2
      character(len=16)             :: colnames(nage), rownames(nyr)
      character(len=4)              :: adum
      logical, dimension(nyr,nage)  :: mv_mat
      logical, dimension(nyr)       :: mv_vec


      !========  The following are initialization tasks to fill Fortran
      !========  arrays with numbers, etc., to prepare for testing the
      !========  export functions to R.
      ! Note: variables m1, m2 are matrices
      ! Note: variables v1, v2, etc. are real vectors
      ! Note: variables iyr and iage are integer vectors

      ! Initialize missing-value matrix and vector:
      mv_mat(:,:) = .false.
      mv_mat(1,2) = .true.
      mv_mat(4,3) = .true.
      !
      mv_vec(:)   = .false.
      mv_vec(2)   = .true.
      mv_vec(3)   = .true.

      ! Pre-fill the real arrays with random numbers:
      call random_number(m1)
      m2 = 1.0e12 * m1
      call random_number(v1)
      call random_number(v2)
      call random_number(v3)

      ! Fill the YEAR array with integers:
      do i = 1, nyr
         iyr(i) = 1990 + i
      end do

      ! Generate column names:
      do i = 1, nage
         write(unit=adum, fmt="(i2.2)") i
         colnames(i) = "var" // adum
         iage(i) = i - 1
      end do

      ! Generate row names:
      do i = 1, nyr
         write(unit=adum, fmt="(i4.4)") iyr(i)
         rownames(i) = "yr" // trim(adum)
      end do

      !===== Now write a complicated R object that can be read with dget("filename")

call sub1()
call sub2(m1, m2, mv_mat, rownames, colnames, iyr, nyr, nage)
call sub3(v1, v2, v3, mv_vec,iyr, iage, m1, m2, mv_mat, rownames, nyr, nage)

end program for2rtest

subroutine sub1
      use for2r
      implicit none
      integer, parameter      :: r4 = kind(1.0)    !--real single precision
      integer, parameter      :: r8 = kind(1.0d0)  !--real double precision

      ! Open the file
      call open_r_file("for2r-test.rdat", digits=6)
      ! Write the INFO object:
      call open_r_info_list("info")
         call wrt_r_item("author",ax="Michael H. Prager")
         call wrt_r_item("run",ax="32b")
         call wrt_r_item("species",ax="yellow grouper")
         call wrt_r_item("model",ax="Statistical CAA")
         call wrt_r_item("units.len",ax="mm")
         call wrt_r_item("units.L",ax="mt", last=.TRUE.)
      ! Write an ELEMENTWISE VECTOR object:
      call open_r_vector("real.parms")
         call wrt_r_item("vb.k", x=0.2_r8)
         call wrt_r_item("vb.li", x=122.2_r8)
         call wrt_r_item("vb.k", x=0.2_r8)
         call wrt_r_item("vb.t0", na=.true., last=.TRUE.)
      return
end subroutine sub1

subroutine sub2(mat1, mat2, mvmat, rnames, cnames, id0, nr, nc)
      use for2r
      implicit none
      integer, parameter      :: r4 = kind(1.0)    !--real single precision
      integer, parameter      :: r8 = kind(1.0d0)  !--real double precision
      integer, intent(IN)     :: nr, nc
      real(r8), intent(IN)    :: mat1(nr,nc), mat2(nr,nc)
      logical, intent(IN)     :: mvmat(nr,nc)
      character(len=*), intent(IN) :: rnames(nr), cnames(nc)
      integer, intent(IN)     :: id0(nr)
      !
      ! Write a complete vector:
      call wrt_r_complete_vector("vector3", ax=cnames)
      ! Write a MATRIX object with character rownames and column names:
      call wrt_r_matrix("ran.mat",x=mat1,rownames=rnames,colnames=cnames)
      ! Write another MATRIX object with integer row names, no column names, and NAs
      call wrt_r_matrix("ran2.mat",x=mat2, na=mvmat, rowids=id0)
      ! Write a MATRIX object with no row or column names:
      call wrt_r_matrix("ran3.mat",x=mat2)
      return
end subroutine sub2

subroutine sub3(vec1, vec2, vec3, mvvec, id1, id2, mat1, mat2, mvmat, rnames, nr, nc)
      use for2r
      implicit none
      integer, parameter      :: r4 = kind(1.0)    !--real single precision
      integer, parameter      :: r8 = kind(1.0d0)  !--real double precision
      integer, parameter, dimension(2)  :: rbounds = (/ 5, 8 /)
      integer, intent(IN)     :: nr, nc
      real(r8), intent(IN)    :: mat1(nr,nc), mat2(nr,nc), vec1(nr), vec2(nr), vec3(nr)
      logical, intent(IN)     :: mvmat(nr,nc), mvvec(nr)
      integer, intent(IN)     :: id1(nr), id2(nc)
      character(len=*), intent(IN) :: rnames(nr)


      ! Write a DATA FRAME object with row names:
      call open_r_df("tseries")
         call wrt_r_df_col("id",ix=id1)
         call wrt_r_df_col("var1", x=vec1)
         call wrt_r_df_col("var2", x=vec2, na=mvvec)
         call wrt_r_df_col("var3", x=vec3, last=.TRUE., rownames=rnames)
      ! Write a DATA FRAME object without row names:
      call open_r_df("vseries")
         call wrt_r_df_col("id",ix=id1(1:4))
         call wrt_r_df_col("var.a", x=vec3(1:4), na=mvvec(1:4))
         call wrt_r_df_col("var.b", x=vec1(1:4), last=.TRUE.)
      ! Write a DATA FRAME object with row bounds:
      call open_r_df("xseries")
         call wrt_r_df_col("id",ix=id1(1:4))
         call wrt_r_df_col("var.a", x=vec3(1:4), na=mvvec(1:4))
         call wrt_r_df_col("var.b", x=vec1(1:4), last=.TRUE., rowbounds = rbounds)
      ! Write some complete vectors, with and without names
      call wrt_r_complete_vector("vector1", x=vec3)
      ! Write a list object containing two matrices and a complete vector:
      call open_r_list("lcomp.mats")
         call wrt_r_matrix("lcomp.a", x=mat1, na=mvmat, rowids=id1, colids=id2)
         call wrt_r_matrix("lcomp.b", x=mat2, rowids=id1, colids=id2)
         call wrt_r_complete_vector("vector2", x=vec3, el_names=rnames)
      call close_r_list
      !
      ! Close the file
      call wrt_r_comment("Calling close_r_file -- last call in program.")
      call close_r_file
      return
end subroutine sub3
! =============================================================
