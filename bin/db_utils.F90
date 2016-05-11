! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>
! Enrico Minguzzi <eminguzzi@arpa.emr.it>

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License as
! published by the Free Software Foundation; either version 2 of 
! the License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
#include "config.h"
MODULE db_utils
USE missing_values
USE log4fortran
IMPLICIT NONE

CONTAINS

SUBROUTINE parse_dba_access_info(string, dsn, user, password)
CHARACTER(len=*),INTENT(in) :: string
CHARACTER(len=*),INTENT(out) :: dsn
CHARACTER(len=*),INTENT(out) :: user
CHARACTER(len=*),INTENT(out) :: password

INTEGER :: bar, at, colon

IF (string == '-' .OR. string == '') THEN
  dsn = cmiss
  user = cmiss
  password = cmiss
ELSE
! here we try to distinguish euristically between the old Oracle-style
! user/password@dataset and the URI-style
! scheme:[//[[user][:password]@][host[:port]]/]resource
! see also http://www.faqs.org/rfcs/rfc3986.html
  colon = INDEX(string, ':')
  bar = INDEX(string, '/')
  at = INDEX(string, '@')
  IF (bar > 0 .AND. at > bar .AND. &
   (colon == 0 .OR. colon > bar)) THEN ! Oracle-style
    user = string(:bar-1)
    password = string(bar+1:at-1)
    dsn = string(at+1:)
#ifdef DEBUG
    CALL l4f_log(L4F_DEBUG,'Oracle-style db access info: '// &
     TRIM(string))
#endif
  ELSE ! URI-style (unchecked for correctness)
    dsn = string
    user = cmiss
    password = cmiss
#ifdef DEBUG
    CALL l4f_log(L4F_DEBUG,'URI-style db access info: '// &
     TRIM(string))
#endif
!    CALL optionparser_printhelp(opt)
!    CALL l4f_category_log(category, L4F_ERROR, &
!     'error in command-line arguments, database access info '// &
!     TRIM(string)//' not valid.')
!    CALL raise_fatal_error()
  ENDIF
ENDIF

END SUBROUTINE parse_dba_access_info

END MODULE db_utils
