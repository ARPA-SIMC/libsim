!==============================================================================
! FOR2R.F90    --    MODULE COMPRISING FOR2R PACKAGE
! Author:
!     Michael H. Prager
!     NOAA, Beaufort, NC
!     mike.prager@noaa.gov
! Date annotated:
!     June 7, 2005
! Date last revised:
!     See change log immediately below
! Language:
!     Fortran 95 (standard conforming)
! Purpose:
!     This module file has functions for writing R-compatible data output.
!     Output is written into a file that R can read with the dget() function.
!     Example from R prompt:
!        > myvar = dget("myfile.txt")
! Other files required:  none
! With thanks to the following for collaboration or bug reports:
!     Jennifer Martin
!     Andi Stephens
!     John Zedlewski
!==============================================================================
! Change log
! 07 Jun 2005: v.0.1  First functioning version (info, vector, matrix).
! 08 Jun 2005: v.0.2  Added data frames and lists.
! 09 Jun 2005: v.0.21 Tidied up code & documentation; added comment subroutine.
! 10 Jun 2005: v.0.22 Fixed bugs; added integer type to matrix and data frame.
!                     Added character type to data frame.
! 13 Jun 2005: v.0.23 Changed reals to double precision. Added missing value option
!                     to s_vec_wrt. Revised info item to include name & date options.
! 14 Jun 2005  v.0.25 Changed NA vars from integer to logical. Added NAs option
!                     with matrices. Put real format into var "realfmt"
! 16 June 2005 v.0.26 Added SAVE to several module variables in case this is
!                     used by a subroutine that goes out of scope.
! 23 Aug 2005 v.0.27  Added character type to s_vec_wrt.
! 15 Apr 2006 v.0.28  Routine names changed by A. Stephens
! 05 Jun 2006 v.0.90  Added missing "trim" to wrt_r_item.
! 08 Aug 2006 v.0.91  Changed name of "info" writing routine
! 09 Aug 2006 V.1.00  Updated version number to 1.00 for release
! 14 Aug 2006 V.1.01  Reduced to one file (eliminated two modules) for distribution.
! 08 Sep 2006 V.1.02  Changed error messages to reflect routine names (Andi).
! 08 Sep 2006 v.1.03  Added wrt_r_complete_vector.
! 12 Jan 2007 v.1.04  Fixed several routines to eliminate extra commas (first_element)
! 28 Feb 2007 v.1.05  Fixed bug (reported by John Zedlewski) in which a data frame
!                     without row names was not written correctly. Lines 887ff.
!  1 Mar 2007 v.1.06  Added argument "rowbounds" to wrt_r_df_col.
! 11 Mar 2007 v.1.07  No changes
! 20 Oct 2007 v.1.1   Revised info list functions to allow using it to write
!                     any list of scalars.
! 12 Sep 2008 v.1.2   Change to prevent extra comma for nested lists.
!==============================================================================
! Possible future improvements:
! *   Allow matrices of character data.
! *   Allow N-dimensional arrays for N > 2.
! *   More error checking for proper sequence of calls
! *   Optional auto reallocation of "names" matrix when full
!==============================================================================
MODULE FOR2R
   ! The following are module variables, available to all contained procedures
   implicit none
   integer, private, parameter            :: r4 = kind(1.0)    !--real single precision
   integer, private, parameter            :: r8 = kind(1.0d0)  !--real double precision
   integer, save, private                 :: iunit, dflen, level, prevlevel, maxlevel, maxcomp
   logical, save, private                 :: first_element = .false.
   character(len=32), allocatable, private, save :: names(:,:)
   integer, allocatable, save, private    :: nnames(:)
   character, parameter, private          :: comma=",", lparen="(", rparen=")", equals="=",quote=""""
   character(len=*),parameter, private    :: nachar="NA", version="1.2"  ! <==== VERSION
   ! Note--the following format determines default precision of the data transfer:
   character(len=12), private, save       :: realfmt="(es16.9,2A)"
   ! Declare one routine as private
   private                                :: reg_rnames, day_of_week, find_unit
   !------------------------------------------------------------------------------
   ! --- IMPORTANT VARIABLES ---
   ! NCOMP     Total number of components, subcomponents written
   ! LEVEL     Current nesting level. 1=master object, 2=subobject, etc.
   !           This should be incremented/decremented by any object
   !           that stores subobject names!
   ! MAXLEVEL  Maximumum number of levels for which storage of
   !           object names is allocated
   ! MAXCOMP   Maximumum number of components (per level) for which storage
   !           of object names is allocated
   ! NAMES     Array of character strings containing names of components
   ! DFLEN     Used by data-frame routines to store working column length

CONTAINS
!------------------------------------------------------------------------------
   SUBROUTINE OPEN_R_FILE(fname, mxlevel, mxcomp, digits)
      ! M.H. Prager,  March 2004; revised June, 2005
      ! mike.prager@noaa.gov
      !
      ! Open a file to hold an R data object and initialize the object
      ! Also allocate array to hold component levels
      ! ARGUMENTS
      ! fname - Name of file for output
      ! mxlevel - maximum nesting level of components within components
      ! mxcomp - maximum number of components within a level
      !    (e.g., cols within dataframe)
      !    (e.g., components within main outer object)
      ! digits - digits after decimal point in real format for writing

      implicit none
      ! Arguments
      character(len=*), intent(IN)  :: fname
      integer, optional, intent(IN) :: mxlevel, mxcomp, digits
      ! Local variables
      character(len=120)            :: string1, string2, string3
      integer                       :: dig

      string1 = "This file written with For2R version " // version //"."
      string2 = "Read this file into R or S with x=dget('" // trim(fname) // "')."
      string3 = "For2R written by Mike.Prager@noaa.gov. Please credit author and report bugs/improvements."

      ! Initialize level variables
      level = 1
      prevlevel = 0
      first_element = .true.
      ! Check arguments and set defaults if not given. Store values in local variables.
      if (present(mxlevel)) then
         maxlevel = mxlevel
      else
         maxlevel = 6
      endif
      if (present(mxcomp)) then
         maxcomp = mxcomp
      else
         maxcomp = 128
      endif
      if (present(digits)) then
         dig = digits
      else
         dig = 7
      endif
      write(realfmt, "(A, i0, A, i0, A)") "(es", dig+7, ".", dig, ",2a)"
      ! Allocate arrays to hold names & number of names
      allocate(names(maxcomp,maxlevel))
      allocate(nnames(maxlevel))
      names = ""
      nnames = 0
      ! Open the file for output
      call find_unit(iunit)

      open(file=fname, unit=iunit, action="WRITE")
      call wrt_r_comment(string1)
      call wrt_r_comment(string2)
      call wrt_r_comment(string3)
      write(iunit,*)
      ! Write the beginning of the structure
      write(unit=iunit,fmt=500)
      500 format("structure(list(")
      !
      return
   END SUBROUTINE OPEN_R_FILE
!==============================================================================
   SUBROUTINE REG_RNAMES(name0)
      ! M.H. Prager,  March 2004; revised June, 2005
      ! mike.prager@noaa.gov
      !
      ! Subroutine to keep track of names of the components in the R structure.

      implicit none
      character(len=*)  ::    name0

      ! Check for invalid nesting levels
      if (level > maxlevel) then
         write(*,500) level, maxlevel
500      format(" Error: Too many levels in reg_rnames. Level=", &
            i0," and max=",i0)
         stop
      elseif (level == 0) then
         write(*,*) "Error: Level can't be zero in reg_rnames."
         stop
      endif

      ! See if level has changed and if so, take appropriate action:
      if (level==prevlevel) then
         continue
      elseif (level < prevlevel) then
         prevlevel = level
      elseif (level == prevlevel + 1) then
         ! initialize new level
         names(:,level) = ""
         nnames(level) = 0
         prevlevel = level
      else
         write(*,510) level, prevlevel
510      format("Note: Level change unexpected in reg_rnames. Current level=",i0,", and previous level=",i0)
         prevlevel = level
!         stop
      endif

      ! Keep count of the number of names at this level:
      nnames(level) = nnames(level) + 1
      ! Store the current name in the NAMES array:
      names(nnames(level), level) = name0

      return
   END SUBROUTINE REG_RNAMES
!================================================================================
   SUBROUTINE OPEN_R_INFO_LIST(name, date)
      ! M.H. Prager,  December 2004; revised June, 2005
      ! mike.prager@noaa.gov
      !
      ! Initialize an INFO object and write its DATE subobject.
      ! All main R objects are assumed to begin with an INFO object.
      !
      ! The INFO object contains descriptive information about the data structure.
      ! It ALWAYS contains the date as the first item, and it MUST contain
      ! at least one other items
      !-----
      implicit none
      ! Arguments
      character(len=*), intent(IN)     :: name
      logical, intent(IN),optional     :: date
      ! Local variables
      character(len=3),parameter,dimension(12)  :: month = &
         (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
      character                        :: wkday*9, date_string*48
      integer                          :: datime(8)
      logical                          :: dte
      !-------
      call reg_rnames(name)
      level = level + 1

      if (present(date)) then
         dte = date
      else
         dte = .true.
      endif

      ! Write output to start the "info" subobject (a list)
      if (first_element) then
         first_element = .false.
      else
         write(iunit,510,advance='NO') comma
      endif

      write(iunit,500,advance="NO") name
500   format(/,A,'= structure(list',/,'(')
510   format(A)

      if (dte)  then
         !... Get date & time (F90 style) ***
         call date_and_time(values=datime)
         !... Get day of week from a function
         wkday = day_of_week(datime(1),datime(2),datime(3))
         ! Use Fortran's interal write to put the date and time information into
         ! a character variable named "date_string"
         write(date_string,400) trim(wkday), comma, datime(3), month(datime(2)), &
            datime(1), datime(5), datime(6), datime(7)
400      format (A,A,1x,i2.2,1x,a3,1x,i4," at ",i2.2,":",i2.2,":",i2.2)
         ! Now write the date to the INFO object:
         write(iunit,520) trim(date_string)
520      format('date ="' , A, '"' )
         ! Save the name:
         call reg_Rnames("date")
         first_element = .false.
      else
         first_element = .true.
      endif

      return
   end subroutine OPEN_R_INFO_LIST
!==============================================================================
   SUBROUTINE OPEN_R_VECTOR(name)
      ! M.H. Prager,  June, 2005
      ! mike.prager@noaa.gov
      !
      ! Initialize a vector object
      ! ARGUMENT
      ! name - name of the vector object (character)

      implicit none
      character(len=*), intent(IN)   :: name

      ! Register name of vector
      call reg_Rnames(trim(name))
      level = level + 1

      if (first_element) then
         first_element = .false.
      else
         write(iunit,510,advance='NO') comma
      endif
      write(iunit,500,advance="NO") name, equals
500   format(/,2A, "structure(",/,"c(")
510   format(A)

      first_element = .true.

      return
   END SUBROUTINE OPEN_R_VECTOR
!==============================================================================
   SUBROUTINE WRT_R_ITEM(name, x, ix, ax, na, last)
      ! M.H. Prager,   June, 2005
      ! mike.prager@noaa.gov

      ! Write one element of a numeric vector or list
      ! The element must have a name
      ! ARGUMENTS:
      ! name - name of the data item (character)
      ! x - the datum itself (if real)
      ! ix - the datum itself  (if integer)
      ! ax - the datum itself (if character)
      ! last - set to .TRUE. if this is the last item in this vector

      implicit none
      ! Arguments
      character(len=*), intent(IN)           :: name
      real(r8), intent(IN), optional         :: x
      integer, intent(IN), optional          :: ix
      character(len=*), intent(IN), optional :: ax
      logical, optional, intent(IN)          :: last, na
      ! Local variables
      integer                                :: i
      logical                                :: lst, isna
      character(len=16)                      :: xtype

      ! Initialize variable LST depending on argument LAST
      if (present(last)) then
         lst = last
      else
         lst = .false.
      endif

      ! Initialize variable ISNA depending on argument NA
      if (present(na)) then
         isna = na
      else
         isna = .false.
      endif

      ! Set type of input data
      xtype = "none"
      if (present(x)) then
         xtype = "real"
      elseif (present(ix)) then
         xtype = "integer"
      elseif (present(ax)) then
         xtype = "character"
      endif
      if (xtype=="none") then
         isna = .true.
      endif
      if (isna) xtype = "missing"

      ! Register (save) the name of the item.  This is done first so
      ! that reg_Rnames can initialize this level's name count.
      call reg_Rnames(name)

      ! Write the VALUE of the item.
      if (first_element) then
         first_element = .false.
      else
         write(iunit,510,advance='NO') comma
      endif
      select case(xtype)
      case("real")
         write(iunit, realfmt, advance="NO") x
      case("integer")
         write(iunit, 520, advance="NO") ix
      case("character")
         write(iunit,530, advance="NO") quote, trim(ax), quote
      case("missing")
         write(iunit, 540, advance="NO") nachar
      endselect

      if (lst) then
         ! Write the NAMES of the information items
         write(iunit,570, advance="NO") rparen, comma, lparen

         do i=1,nnames(level)
            write(iunit,530, advance="no") quote, trim(names(i,level)),quote
            if (i < nnames(level)) then
               write(iunit,510, advance="no") comma
            else
               write(iunit,510) rparen,rparen
            endif
         enddo

         ! Reset level since this is done.
         level = level - 1    ! Should now be 1
      endif

510   format(2A)
520   format(i0)
530   format(3A)
540   format(A)
570   format(2A,/,".Names=c", A)

   END SUBROUTINE WRT_R_ITEM
!==============================================================================
   SUBROUTINE WRT_R_MATRIX (name, x, ix, na, rownames, colnames, rowids, colids)
      ! M. H. Prager, June 2005
      ! mike.prager@noaa.gov
      !
      ! Write a matrix subobject to the R data object
      !
      ! ARGUMENTS
      ! x         : the matrix itself (if real)
      ! ix        : the matrix itself (if integer)
      ! na        : missing-value mask (missing if .true.)
      ! rownames  : array of row names (character)
      ! colnames  : same, for columns
      ! rowids    : array of row names as integers (e.g., years)
      ! colids    : same, for columns
      ! NOTE: Either rownames OR rowids OR neither can be given.
      !       [The same applies to columns.]

      implicit none
      ! Passed arguments:
      character(len=*), intent(IN)                          :: name
      real(r8), dimension(:,:), intent(IN), optional        :: x
      integer, dimension(:,:), intent(IN), optional         :: ix
      logical, dimension(:,:), intent(IN), optional         :: na
      character(len=*), dimension(:), intent(IN), optional  :: rownames, colnames
      integer, dimension(:), intent(IN), optional           :: rowids, colids
      ! Local variables:
      integer                                      :: nrow, ncol, irow, icol
      character(len=32), dimension(:), allocatable :: rname, cname
      character(len=16)                            :: xtype
      logical, dimension(:,:), allocatable         :: isna
      logical                                      :: wrtrownames, wrtcolnames

      ! Register (store) name of matrix
      call reg_Rnames(name)

      ! Set type of input data
      ! Get number of rows and columns
      xtype = "none"
      if (present(x)) then
         xtype = "real"
         nrow = size(x, dim=1)
         ncol = size(x, dim=2)
      elseif (present(ix)) then
         xtype = "integer"
         nrow = size(ix, dim=1)
         ncol = size(ix, dim=2)
      endif
      if (xtype=="none") then
         write(*,410) trim(name)
         stop
      endif
410   format(1x,"Error: no data supplied to wrt_r_matrix for object name", 1x, A)

      !...Check availability & compatibility of missing-value mask
      allocate(isna(nrow,ncol))
      if (present(na)) then
         if ((size(na,1) /= nrow) .or. (size(na,2) /= ncol)) then
            write(*,415) trim(name)
            stop
         else
            isna(:,:) = na(:,:)
         endif
      else  ! Argument NA was not present
         isna(:,:) = .false.
      endif
415   format(1x,"Error: Size of missing-values matrix does not match size of data", &
         " matrix in wrt_r_matrix for object name",1X,A)

      ! Allocate temporary storage for names:
      allocate(rname(nrow))
      allocate(cname(ncol))

      ! Check for presence of row names and copy into char vector:
      wrtrownames = .false.
      ! If character rownames are given, copy into "rname" array:
      if (present(rownames)) then
         wrtrownames = .true.
         rname(:) = rownames(:)
      elseif (present(rowids)) then
         ! If integer row IDs are given, convert to character with
         ! internal write and copy into "rname" array:
         wrtrownames = .true.
         do irow = 1, nrow
            write(rname(irow), '(I0)') rowids(irow)
         enddo
      endif

      ! Check for presence of col names and copy into char vector:
      wrtcolnames = .false.
      ! If colnames are given, copy into "cname" array
      if (present(colnames)) then
         wrtcolnames = .true.
         cname(:) = colnames(:)
      elseif (present(colids)) then
      ! If col IDs are given, convert to character with internal write:
         wrtcolnames = .true.
         do icol = 1, ncol
            write(cname(icol), '(I0)') colids(icol)
         enddo
      endif

      ! Write output to start the matrix:
      if (first_element) then
         first_element = .false.
      else
         write(iunit, 499, advance="NO") comma
      endif
      write(iunit,500)  name, equals
499   format(A)
500   format(/, 2A, "structure(c(")

      ! Write the data
      cols: do icol = 1, ncol
         rows: do irow = 1, nrow
            if (icol < ncol .or. irow < nrow) then
               if (isna(irow,icol)) then
                  write(iunit,505, advance="NO") nachar, comma
               else
                  select case(xtype)
                  case("real")
                     write(iunit,realfmt, advance="NO") x(irow,icol), comma
                  case("integer")
                     write(iunit,520, advance="NO") ix(irow,icol), comma
                  endselect
               endif
            else  ! final value of matrix
               if (isna(irow,icol)) then
                  write(iunit,505, advance="NO") nachar, rparen, comma
               else
                  select case(xtype)
                  case("real")
                     write(iunit,realfmt, advance="NO") x(irow,icol), rparen, comma
                  case("integer")
                     write(iunit,520, advance="NO") ix(irow,icol), rparen, comma
                  endselect
               endif
            endif
         enddo rows
         write(iunit,530,advance="NO")  ! Newline
      enddo cols
505   format(3A)
520   format(i0, 2A)
530   format(/)

      ! Write the dimensioning information:
      write(iunit, 600, advance = "NO") nrow, comma, ncol
600   format(".Dim = c(", i0, A, i0, "), ")

      ! Write heading for the dimnames:
      write(iunit, 620, advance = "NO")
620   format(".Dimnames = list(")

      ! Write the row names
      if (wrtrownames) then
         write(iunit, 630, advance="NO")
         write(iunit,640, advance="NO") &
            (quote, trim(rname(irow)), quote, comma, irow = 1, nrow-1)
         write(iunit, 650) quote, trim(rname(nrow)), quote
      else
         write(iunit,660)
      endif

630   format("c(")
640   format(40A)
650   format(3A, "),")
660   format("NULL,")

      ! Write the column names
      if (wrtcolnames) then
         write(iunit, 630, advance="NO")
         write(iunit,640, advance="NO") &
            (quote, trim(cname(icol)), quote, comma, icol = 1, ncol-1)
         write(iunit,680) quote, trim(cname(ncol)), quote
      else
         write(iunit,690)
      endif
680   format(3A, ")))")
690   format("NULL))")

      deallocate(rname, cname, isna)
      return
   END SUBROUTINE WRT_R_MATRIX
!==============================================================================
   SUBROUTINE WRT_R_COMPLETE_VECTOR (name, x, ix, ax, na, el_names, el_ids)
      ! M. H. Prager, Sept 2006
      ! mike.prager@noaa.gov
      !
      ! Write an R vector (all at once) to the R data object
      ! (Derived from wrt_r_matrix)
      !
      ! ARGUMENTS
      ! x         : the vector itself (if real)
      ! ix        : the vector itself (if integer)
      ! na        : missing-value mask (missing if .true.)
      ! el_names  : array of element names (character)
      ! el_ids    : array of element names as integers (e.g., years)
      ! NOTE: Either vnames OR vids OR neither can be given.

      implicit none
      ! Passed arguments:
      character(len=*), intent(IN)                          :: name
      real(r8), dimension(:), intent(IN), optional          :: x
      integer, dimension(:), intent(IN), optional           :: ix
      character(len=*), dimension(:), intent(IN), optional  :: ax
      logical, dimension(:), intent(IN), optional           :: na
      character(len=*), dimension(:), intent(IN), optional  :: el_names
      integer, dimension(:), intent(IN), optional           :: el_ids
      ! Local variables:
      integer                                      :: nrow, irow
      character(len=32), dimension(:), allocatable :: names
      character(len=16)                            :: xtype
      logical, dimension(:), allocatable           :: isna
      logical                                      :: wrtnames

      ! Register (store) name of matrix
      call reg_Rnames(name)

      ! Set type of input data
      ! Get number of elements in vector
      xtype = "none"
      if (present(x)) then
         xtype = "real"
         nrow = size(x)
      elseif (present(ix)) then
         xtype = "integer"
         nrow = size(ix)
      elseif (present(ax)) then
         xtype = "character"
         nrow = size(ax)
      endif
      if (xtype=="none") then
         write(*,410) trim(name)
         stop
      endif
410   format(1x,"Error: no data supplied to wrt_r_truevector for object name", 1x, A)

      !...Check availability & compatibility of missing-value mask array
      allocate(isna(nrow))
      if (present(na)) then
         if (size(na) /= nrow) then
            write(*,415) trim(name)
            stop
         else
            isna(:) = na(:)
         endif
      else  ! Argument NA was not present
         isna(:) = .false.
      endif
415   format(1x,"Error: Size of missing-values matrix does not match size of data", &
         " matrix in wrt_r_matrix for object name", 1X, A)

      ! Allocate temporary storage for names:
      allocate(names(nrow))

      ! Check for presence of row names and copy into char vector:
      wrtnames = .false.
      ! If character rownames are given, copy into "names" array:
      if (present(el_names)) then
         wrtnames = .true.
         names(:) = el_names(:)
      elseif (present(el_ids)) then
         ! If integer row IDs are given, convert to character with
         ! internal write and copy into "rname" array:
         wrtnames = .true.
         do irow = 1, nrow
            write(names(irow), '(I0)') el_ids(irow)
         enddo
      endif

      ! Write output to start the vector
      if (first_element) then
         first_element = .false.
      else
         write(iunit, fmt="(A)", advance="NO") comma
      endif
      write(iunit,500) name, equals
500   format(/, 2A, "structure(c(")

      ! Write the data
         do irow = 1, nrow
            if (irow < nrow) then
               if (isna(irow)) then
                  write(iunit,505, advance="NO") nachar, comma
               else
                  select case(xtype)
                  case("real")
                     write(iunit, realfmt, advance="NO") x(irow), comma
                  case("integer")
                     write(iunit, 520, advance="NO") ix(irow), comma
                  case("character")
                     write(iunit, 505, advance="NO") quote, trim(ax(irow)), quote, comma
                  endselect
               endif
            else  ! final value of matrix
               if (isna(irow)) then
                  write(iunit, 505, advance="NO") nachar, rparen, comma
               else
                  select case(xtype)
                  case("real")
                     write(iunit, realfmt, advance="NO") x(irow), rparen, comma
                  case("integer")
                     write(iunit, 520, advance="NO") ix(irow), rparen, comma
                  case("character")
                     write(iunit, 505, advance="NO") quote, trim(ax(irow)), quote, rparen, comma
                  endselect
               endif
            endif
         enddo
         write(iunit,530,advance="NO")  ! Newline
505   format(5A)
520   format(i0, 2A)
530   format(/)

      if (wrtnames) then
         ! Write the element names & close the vector
         write(iunit, 620)
         write(iunit, 640, advance="NO") (quote, trim(names(irow)), quote, comma, irow = 1, nrow-1)
         write(iunit, 650) quote, trim(names(nrow)), quote
      else
         ! Write NULL names & close the vector
         write(iunit, 660)
      endif

620   format(".Names = c(")
640   format(40A)
650   format(3A, "))")
660   format(".Names = NULL)")

      deallocate(names, isna)
      return
   END SUBROUTINE WRT_R_COMPLETE_VECTOR
!==============================================================================
   SUBROUTINE OPEN_R_DF(name)
      ! M.H. Prager,  June 2005
      ! mike.prager@noaa.gov
      !
      ! Initialize a data frame
      ! ARGUMENT:
      ! name - name of data frame (R compatible)

      implicit none
      character(len=*), intent(IN)   :: name

      call reg_Rnames(trim(name))      ! Register name of data frame
      dflen = 0                        ! Initialize number of rows in DF
      level = level + 1                ! We are up one level

      ! Write output to start the data frame subobject
      write(iunit,500) comma, name, equals, "structure(list"
500   format(/,4A)

      return
   END SUBROUTINE OPEN_R_DF
!==============================================================================
   SUBROUTINE WRT_R_DF_COL(name, x, ix, ax, na, last, rownames, rowids, rowbounds)
      ! M.H. Prager,  June 2005
      ! mike.prager@noaa.gov
      !
      ! Write a real, numeric column to a data frame.
      ! ARGUMENTS:
      ! name - name to use for this column of the data frame
      ! x    - Real; vector of real values to write to df column.
      ! na   - Logical; vector of same length as x.
      !        If .true., value in x is missing.
      ! last - Logical; set .TRUE. if last column to finalize data frame

      implicit none
      ! Arguments
      character(len=*), intent(IN)              :: name
      real(r8), intent(IN), optional            :: x(:)
      integer, intent(IN), optional             :: ix(:)
      logical, intent(IN), optional             :: na(:)
      character(len=*), intent(IN), optional    :: ax(:)
      logical, intent(IN), optional             :: last
      character(len=*), dimension(:), intent(IN), optional  :: rownames
      integer, dimension(:), intent(IN), optional           :: rowids
      integer, dimension(2), intent(IN), optional           :: rowbounds

      ! Local variables
      integer                                               :: i, nrow
      logical                                               :: lst
      character(len=16)                                     :: xtype
      !character(len=32), dimension(:), allocatable          :: rname
      character(len=9)                                      :: rntype
      logical, dimension(:), allocatable                    :: isna

      ! Set flag if user says this is the last variable
      lst = .false.                      ! Default is .false.
      if (present(last)) lst = last      ! Use user value if given

      ! Register (save) the name of the column
      if (len_trim(name) < 1) then
         write(*,400)
         stop
      else
         call reg_Rnames(name)
      endif
400   format(1x,"ERROR: Name must be specified in wrt_r_df_col.")

      ! Set type of input data and length of data column
      xtype = "none"
      if (present(x)) then
         xtype = "real"
         nrow = size(x)
      elseif (present(ix)) then
         xtype = "integer"
         nrow = size(ix)
      elseif (present(ax)) then
         xtype = "character"
         nrow = size(ax)
      endif

      if (xtype=="none") then
         write(*,410)
         stop
      endif
410   format(1x,"Error: no data supplied to wrt_r_df_col.")

      !...Check availability & compatibility of missing-value mask
      allocate(isna(nrow))
      if (present(na)) then
         if (size(na) /= nrow) then
            write(*,415) size(na), nrow
            stop
         else
            isna(1:nrow) = na(1:nrow)
         endif
      else  ! Argument NA was not present
         isna(:) = .false.
      endif
415   format(1x,"Error: Size of missing-values array does not match size of data", &
         " array in wrt_r_df_col.",/,t2, "Sizes are",1x,i0,1x,"and",1x,i0)

      !...Store column length if first col; otherwise check against first col
      if (nnames(level)==1) then
         dflen = nrow
      else
         if (nrow /= dflen) then
            write(*,420) dflen, nrow
            stop
         endif
      endif
420   format(1x,"Error: Column lengths do not match in wrt_r_df_col."/&
         t2, "Lengths of column 1 is",1x,i0,1x,"and current length is",1x,i0)

      ! If last column, check for passed row names & set indicator:
      if (lst) then
         if (present(rownames)) then
            rntype = "character"
         elseif (present(rowbounds)) then
            rntype = "bounds"
         elseif (present(rowids)) then
            rntype = "integer"
         else
            rntype = "none"
         endif
      endif

      !...If this is the first column, write a left paren; otherwise, a comma:
      if (nnames(level) == 1) then
         write(iunit,500, advance="no") lparen
      else
         write(iunit,500, advance="no") comma
      endif
500   format(4A)

      !...Initialize the column:
      write(iunit,500, advance="no") trim(name), equals, "c", lparen

      !...Write the VALUEs of the column
      do_wrtvals: do i = 1, nrow
         if (mod(i,10) == 0) write(iunit,500)       ! newline
         if (isna(i)) then
            write(iunit,500,advance="NO") nachar
            if (i < nrow) write(iunit,500,advance="NO") comma
         else
            select case (xtype)
            case("real")
               write(iunit,realfmt,advance="NO") x(i)
            case("integer")
               write(iunit,512,advance="NO") ix(i)
            case("character")
               write(iunit,514,advance="NO") quote, trim(ax(i)), quote
            endselect
            if (i < nrow) write(iunit,500,advance="NO") comma
         endif
      enddo do_wrtvals
      ! Write closing punctuation for column
      write(iunit,500) rparen
512   format(i0)
514   format(3A)

      !----- This section executes for last column only ------
      if_last: if (lst) then
         !...Write header for variable (column) names:
         write(iunit,520, advance="NO") rparen, comma,".Names = c("
         !...Write column names:
         do i=1, nnames(level)
            write(iunit,500, advance="no") quote, trim(names(i,level)), quote
            if (i<nnames(level)) then
               write(iunit,500, advance="no") comma
            else
               write(iunit,500) rparen,comma
            endif
         enddo
         !...Write header for row names:
         write(iunit,530, advance="NO")
         !...Write row names:
         select case (rntype)
         case ("none")
            write(iunit, 535, advance="NO") nrow, rparen, comma
         case ("character")
            write(iunit, 550, advance="NO")
            do i = 1, nrow
               write(iunit,500,advance="NO") quote, trim(rownames(i)), quote
               if (i < nrow) write(iunit, 500, advance="NO") comma
            enddo
            write(iunit, 500) rparen, comma
         case ("integer")
            write(iunit, 560, advance="NO")
            do i = 1, nrow
               write(iunit,560,advance="NO") rowids(i)
               if (i < nrow) write(iunit, 500, advance="NO") comma
            end do
         case ("bounds")
            write(iunit, 540, advance="NO") rowbounds(1), rowbounds(2), comma
         case default
            write(*,*) " Faulty value of 'rntype' in 'wrt_r_df_col'."
            stop
         endselect
         !...Write closing information:
         write(iunit,500) 'class="data.frame")'
         level = level - 1
      endif if_last
520   format(2A,/,A)
530   format("row.names=")
535   format("c(NA,", i0, 2A, 1x)
540   format(i0, ":", i0, A, 1x)
550   format("c(")
560   format(i0)

      deallocate(isna)
      return

   END SUBROUTINE WRT_R_DF_COL
!==============================================================================
   SUBROUTINE OPEN_R_LIST(name)
      ! M.H. Prager,   June, 2005
      ! mike.prager@noaa.gov
      !
      ! Initialize a LIST object
      !
      implicit none
      character(len=*), intent(IN)   :: name

      ! Register name of list
      call reg_Rnames(trim(name))
      level = level + 1

      ! Write output to start the list subobject:
      ! Write the VALUE of the item.
      if (first_element) then
         first_element = .false.
      else
         write(iunit,510,advance='NO') comma
      endif
      write(iunit,500) name, equals, "structure(list("
500   format(/,4A)
510   format(A)

      first_element = .true.

      return
   END SUBROUTINE OPEN_R_LIST
!==============================================================================
   SUBROUTINE CLOSE_R_LIST
      ! M.H. Prager,   June, 2005
      ! mike.prager@noaa.gov
      !
      ! Finalize a LIST object by writing names of components
      !
      implicit none
      integer     :: i, nn

      ! Write output to start the vector subobject:
      write(iunit,500,advance="NO") rparen, comma, ".Names = c("
500   format(A,/,2A)
      ! Write the names of the components of the list:
      nn = nnames(level)
      do i = 1, nn
         write(unit=iunit,fmt=510, advance="no") quote, &
            trim(names(i,level)), quote
         if (i<nn) then
            write(unit=iunit, fmt=510, advance="no") comma
         else
            write(unit=iunit, fmt=510) rparen, rparen
         endif
      end do
510   format(3A)

      level = level - 1
      return
   END SUBROUTINE CLOSE_R_LIST
!==============================================================================
   SUBROUTINE WRT_R_COMMENT(text)
      ! M.H. Prager,   June, 2005
      ! mike.prager@noaa.gov
      !
      ! Write a comment
      !
      implicit none
      character(len=*), intent(IN)   :: text

      ! Write comment to the output object:
      write(iunit,500) trim(text)
500   format("### ",A)

      return
   END SUBROUTINE WRT_R_COMMENT
!==============================================================================
   SUBROUTINE CLOSE_R_FILE
      ! M. H. Prager,  March 2004
      ! mike.prager@noaa.gov
      !
      ! Write the component names to finalize the object
      ! and close the file
      implicit none
      integer i, nn
      !
      write(unit=iunit,fmt=500, advance="no") rparen, comma, lparen
500   format(2A,//, " .Names = c", A)
      nn = nnames(1)
      do i = 1, nn
         write(unit=iunit,fmt=510, advance="no") quote, trim(names(i,1)), quote
         if (i<nn) then
            write(unit=iunit, fmt=510, advance="no") comma
         else
            write(unit=iunit, fmt=510) rparen, rparen
         endif
      end do
510   format(3A)

      close(unit=iunit)
      deallocate(names, nnames)
      return
   END SUBROUTINE CLOSE_R_FILE
!--------------------------------------------------------------------
   subroutine find_unit(iu)
      ! Finds and returns first unit number not already connected to a file
      ! Returns -999 if no unit number available
      implicit none
      integer i, iu
      logical used

      do i=10,1000
         inquire(unit=i,opened=used)
         if (.not. used) then
            iu = i
            exit  ! leave do loop
         endif
         iu = -999
      enddo
      return
   end subroutine find_unit
!--------------------------------------------------------------------
   FUNCTION day_of_week(year, month, day) RESULT(weekday)

      !  Function added to module by MHP.  Obtained from Alan J. Miller.
      !  Calculate day of week, allowing for leap years.
      !  Correct back to October 1752 (11 days were left out of September 1752).

      implicit none
      integer, intent(IN)        :: year, month, day
      character(len=9)           :: weekday
      INTEGER                    :: yr, mnth, hundreds, day_ptr
      INTEGER,PARAMETER          :: max_days(12) = (/31,29,31,30,31,30,31,31,30,31,30,31/)
      CHARACTER(LEN=9),parameter :: day_name(0:6) =  (/ 'Sunday   ', 'Monday   ','Tuesday  ', &
         'Wednesday', 'Thursday ', 'Friday   ','Saturday '/)

      !       Number the months starting from March; January & February are
      !       treated as months 11 & 12 of the previous year.

      mnth = month - 2
      IF (mnth <= 0) THEN
        mnth = mnth + 12
        yr = year - 1
      ELSE
        yr = year
      END IF

      !       Check for legal day of month.
      !       N.B. Allows 29 days in February even when not a leap year.

      IF (day < 1 .OR. day > max_days(month)) RETURN

      hundreds = yr/100
      yr = yr - 100*hundreds

      !   The days are numbered from Sunday (0) to Saturday (6).
      !   The function mod(n,7) returns the remainder after n is divided by 7.

      day_ptr = MOD(day + (26*mnth - 2)/10 + 5*hundreds + yr + (yr/4) +  &
                (hundreds/4), 7)
      weekday = day_name(day_ptr)

      RETURN
      END FUNCTION day_of_week
END MODULE FOR2R
!==============================================================================
