! Copyright (C) 2010  ARPA-SIM <urpsim@smr.arpa.emr.it>
! authors:
! Davide Cesari <dcesari@arpa.emr.it>
! Paolo Patruno <ppatruno@arpa.emr.it>

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
PROGRAM v7d_add_wmo_info
#include "config.h"

USE log4fortran
USE optionparser_class
USE io_units
USE vol7d_var_class
USE vol7d_class
#ifdef HAVE_DBALLE
USE vol7d_dballe_class
#endif

IMPLICIT NONE

TYPE(optionparser) :: opt
INTEGER :: optind, optstatus
CHARACTER(len=8) :: input_format
CHARACTER(len=512) :: input_file, output_file, output_format, output_template
INTEGER :: block_number
LOGICAL :: file

INTEGER :: iun, ier, i, n, ninput, iargc, bni, sni, sii
INTEGER,POINTER :: w_s(:), w_e(:)
TYPE(vol7d) :: v7d, v7dtmp
#ifdef HAVE_DBALLE
TYPE(vol7d_dballe) :: v7d_dba, v7d_dba_out
#endif
CHARACTER(len=512) :: dsn, user, password
LOGICAL :: version, ldisplay
CHARACTER(len=512):: a_name
INTEGER :: category


!questa chiamata prende dal launcher il nome univoco
CALL l4f_launcher(a_name,a_name_force="v7d_add_wmo_info")
!init di log4fortran
ier = l4f_init()
!imposta a_name
category = l4f_category_get(a_name//".main")

! define the option parser
opt = optionparser_new(description_msg= &
 'Vol7d application for adding WMO information, it imports a vol7d volume of sparse point data &
 &from a native vol7d file'&
#ifdef HAVE_DBALLE
 //', from a dbAll.e database or from a BUFR/CREX file'&
#endif
 //', it adds WMO block/station numbers according to an internal algorithm, &
 &taking into account polygons info if present, and exports it into a native v7d file'&
#ifdef HAVE_DBALLE
 //' or into a BUFR/CREX file'&
#endif
 //'. If input-format is of file type, inputfile ''-'' indicates stdin, &
 &if input-format or output-format is of database type, inputfile/outputfile specifies &
 &database access info in the form user/password@dsn, &
 &if empty or ''-'', a suitable default is used. &
 &If output-format is of file type, outputfile ''-'' indicates stdout.', &
 usage_msg='Usage: v7d_add_wmo_info [options] inputfile1 [inputfile2...] outputfile')

! options for defining input
CALL optionparser_add(opt, ' ', 'input-format', input_format, 'native', help= &
 'format of input, ''native'' for vol7d native binary file'&
#ifdef HAVE_DBALLE
 //', ''BUFR'' for BUFR file with generic template, ''CREX'' for CREX file&
 &, ''dba'' for dballe database'&
#endif
 )

! option for displaying/processing
CALL optionparser_add(opt, 'd', 'display', ldisplay, help= &
 'briefly display the data volume imported, warning: this option is incompatible &
 &with output on stdout.')
block_number = imiss
CALL optionparser_add(opt, ' ', 'block-number', block_number, help= &
 'set variable B01001, WMO block number, to this value in output volume, &
 &if not provided an automatic choice is done.')

! options for defining output
output_template = ''
CALL optionparser_add(opt, ' ', 'output-format', output_format, 'native', help= &
 'format of output file, in the form ''name[:template]''; ''native'' for vol7d &
 &native binary format (no template to be specified)'&
#ifdef HAVE_DBALLE
 //'; ''BUFR'' and ''CREX'' for corresponding formats, with template as an alias like ''synop'', ''metar'', &
 &''temp'', ''generic'', empty for ''generic'''&
#endif
 )

! help options
CALL optionparser_add_help(opt, 'h', 'help', help='show an help message and exit')
CALL optionparser_add_html(opt, ' ', 'html-form', help= &
 &'print the options as an html form')
CALL optionparser_add(opt, ' ', 'version', version, help='show version and exit')

! parse options and check for errors
CALL optionparser_parse(opt, optind, optstatus)

IF (optstatus == optionparser_help) THEN
  CALL exit(0) ! generate a clean manpage
ELSE IF (optstatus == optionparser_html) THEN
  CALL exit(0) ! generate a clean form
ELSE IF (optstatus == optionparser_err) THEN
  CALL l4f_category_log(category,L4F_ERROR,'in command-line parameters')
  CALL raise_fatal_error()
ENDIF
IF (version) THEN
  WRITE(*,'(A,1X,A)')'v7d_add_wmo_info',VERSION
  CALL exit(0)
ENDIF

! check input/output files
i = iargc() - optind
IF (i < 0) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'input file missing')
  CALL raise_fatal_error()
ELSE IF (i < 1) THEN
  CALL optionparser_printhelp(opt)
  CALL l4f_category_log(category,L4F_ERROR,'output file missing')
  CALL raise_fatal_error()
ENDIF
CALL getarg(iargc(), output_file)

IF (c_e(block_number)) THEN
  IF (block_number < 0 .OR. block_number > 99) THEN
    CALL l4f_category_log(category,L4F_ERROR, &
     'block-number must be between 0 and 99')
    CALL raise_fatal_error()
  ENDIF
ENDIF

! check output format/template
n = word_split(output_format, w_s, w_e, ':')
IF (n >= 2) THEN ! set output template if present
  output_template = output_format(w_s(2):w_e(2))
  output_format(w_e(1)+1:) = ' '
ENDIF
DEALLOCATE(w_s, w_e)

! import data looping on input files
CALL init(v7d)
DO ninput = optind, iargc()-1
  CALL getarg(ninput, input_file)

  IF (input_format == 'native') THEN

    IF (input_file == '-') THEN ! stdin_unit does not work with unformatted
      CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit.')
      input_file='/dev/stdin'
    ENDIF
    CALL import(v7dtmp, filename=input_file)

#ifdef HAVE_DBALLE
  ELSE IF (input_format == 'BUFR' .OR. input_format == 'CREX' .OR. input_format == 'dba') THEN

    IF (input_format == 'BUFR' .OR. input_format == 'CREX') then

      IF (input_file == '-') THEN
        CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdin as stdin unit.')
        input_file='/dev/stdin'
      ENDIF
      file=.TRUE.

    ELSE IF (input_format == 'dba') THEN
      CALL parse_dba_access_info(input_file, dsn, user, password)
      file=.FALSE.
    ENDIF
    
    CALL init(v7d_dba, filename=input_file, FORMAT=input_format, &
     dsn=dsn, user=user, password=password, file=file)

    CALL import(v7d_dba) !, vl, nl, &
    
    v7dtmp = v7d_dba%vol7d
    CALL init(v7d_dba%vol7d) ! nullify without deallocating
    CALL delete(v7d_dba)

#endif

  ELSE
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, format '// &
     TRIM(input_format)//' in --input-format not valid or not supported.')
    CALL raise_fatal_error()
  ENDIF

  CALL vol7d_merge(v7d, v7dtmp) ! smart merge in v7d
  CALL delete(v7dtmp)

ENDDO


CALL delete(opt) ! check whether I can already get rid of this stuff now

IF (ldisplay) THEN
  PRINT*," >>>>> Input Volume <<<<<"
  CALL display(v7d)
ENDIF

CALL add_block_station_var(v7d)
! search for block/station number variables and for station_id variable
bni = INDEX(v7d%anavar%i, vol7d_var_new('B01001'))
sni = INDEX(v7d%anavar%i, vol7d_var_new('B01002'))
sii = INDEX(v7d%anavar%i, vol7d_var_new('B01192'))

IF (bni <= 0 .OR. sni <= 0) THEN
  CALL display(v7d)
  CALL l4f_category_log(category,L4F_ERROR,'adding block/station number variables')
  CALL raise_fatal_error()
ENDIF

IF (sii <= 0) THEN
  DO i = 1, SIZE(v7d%network)
    CALL compute_block_station_number(block_number, &
     v7d%volanai(:,bni,i), v7d%volanai(:,sni,i))
  ENDDO
ELSE
  DO i = 1, SIZE(v7d%network)
    CALL compute_block_station_number(block_number, &
     v7d%volanai(:,bni,i), v7d%volanai(:,sni,i), v7d%volanai(:,sii,i))
  ENDDO
ENDIF

IF (ldisplay) THEN
  PRINT*," >>>>> Output Volume <<<<<"
  CALL display(v7d)
ENDIF

! output
IF (output_format == 'native') THEN
  IF (output_file == '-') THEN ! stdout_unit does not work with unformatted
    CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
    output_file='/dev/stdout'
  ENDIF
  iun = getunit()
  OPEN(iun, file=output_file, form='UNFORMATTED', access=stream_if_possible)
  CALL export(v7d, unit=iun)
  CLOSE(iun)
  CALL delete(v7d)

#ifdef HAVE_DBALLE
ELSE IF (output_format == 'BUFR' .OR. output_format == 'CREX' .OR. output_format == 'dba') THEN
  IF (output_format == 'BUFR' .OR. output_format == 'CREX') THEN
    IF (output_file == '-') THEN
      CALL l4f_category_log(category, L4F_INFO, 'trying /dev/stdout as stdout unit.')
      output_file='/dev/stdout'
    ENDIF
    file=.TRUE.

  ELSE IF (output_format == 'dba') THEN
    CALL parse_dba_access_info(output_file, dsn, user, password)
    file=.FALSE.
  ENDIF

  IF (output_template == '') output_template = 'generic'
! check whether wipe=file is reasonable
  CALL init(v7d_dba_out, filename=output_file, FORMAT=output_format, &
   dsn=dsn, user=user, password=password, file=file, WRITE=.TRUE., wipe=file)

  v7d_dba_out%vol7d = v7d
  CALL init(v7d) ! nullify without deallocating
  CALL export(v7d_dba_out, template=output_template)
  CALL delete(v7d_dba_out)
#endif

ELSE IF (output_format /= '') THEN
  CALL l4f_category_log(category, L4F_ERROR, &
   'error in command-line parameters, format '// &
   TRIM(output_format)//' in --output-format not valid or not supported.')
  CALL raise_fatal_error()
ENDIF

! cleanup
IF (input_format == 'native') THEN
  CALL delete(v7d) ! controllare? input native output bufr
ENDIF
ier = l4f_fini()

CONTAINS

SUBROUTINE parse_dba_access_info(string, dsn, user, password)
CHARACTER(len=*),INTENT(in) :: string
CHARACTER(len=*),INTENT(out) :: dsn
CHARACTER(len=*),INTENT(out) :: user
CHARACTER(len=*),INTENT(out) :: password

INTEGER :: bar, at

IF (string == '-' .OR. string == '') THEN
  dsn = cmiss
  user = cmiss
  password = cmiss
ELSE
  bar = INDEX(string, '/')
  at = INDEX(string, '@')
  IF (bar > 0 .AND. at > bar) THEN
    user = string(:bar-1)
    password = string(bar+1:at-1)
    dsn = string(at+1:)
  ELSE
    CALL optionparser_printhelp(opt)
    CALL l4f_category_log(category, L4F_ERROR, &
     'error in command-line parameters, database access info '// &
     TRIM(string)//' not valid.')
    CALL raise_fatal_error()
  ENDIF
ENDIF

END SUBROUTINE parse_dba_access_info


! make room for block/station numbers
SUBROUTINE add_block_station_var(v7d)
TYPE(vol7d),INTENT(inout) :: v7d

TYPE(vol7d) :: locana

CALL init(locana)
CALL vol7d_alloc(locana, nana=SIZE(v7d%ana), nnetwork=SIZE(v7d%network), nanavari=2)
CALL vol7d_alloc_vol(locana)
locana%ana(:) = v7d%ana(:)
locana%network(:) = v7d%network(:)
CALL init(locana%anavar%i(1), btable='B01001')
CALL init(locana%anavar%i(2), btable='B01002')

CALL vol7d_merge(v7d, locana)

END SUBROUTINE add_block_station_var


! add block/station number with an intelligent(!) algorithm
SUBROUTINE compute_block_station_number(block_number, out_block, out_station, &
 station_id)
INTEGER,INTENT(in) :: block_number
INTEGER,INTENT(out) :: out_block(:), out_station(:)
INTEGER,INTENT(in),OPTIONAL :: station_id(:)

INTEGER :: i

out_station(:) = imiss
out_block(:) = imiss

IF (c_e(block_number)) THEN ! block_number provided
  IF (PRESENT(station_id)) THEN ! station_id found, use it as station number
    WHERE(c_e(station_id))
      out_block(:) = block_number
    END WHERE
    IF (ANY(c_e(station_id) .AND. station_id > 999)) THEN
      CALL l4f_category_log(category, L4F_WARN, &
       'station_id > 999 encountered, stations may be duplicated')
      CALL l4f_category_log(category, L4F_WARN, &
       'try omitting --block-number option')
      WHERE(c_e(station_id))
        out_station(:) = MOD(station_id(:),1000)
      END WHERE
    ELSE
      WHERE(c_e(station_id))
        out_station(:) = station_id(:)
      END WHERE
    ENDIF
  ELSE ! station_id not found, invent station number
    out_block(:) = block_number
    IF (SIZE(out_station) > 999) THEN
      CALL l4f_category_log(category, L4F_WARN, &
       '> 999 stations imported, stations will be duplicated')
      CALL l4f_category_log(category, L4F_WARN, &
       'try omitting --block-number option')
      out_station(:) = (/(MOD(i,1000),i=0,SIZE(out_station)-1)/)
    ELSE
      out_station(:) = (/(i,i=0,SIZE(out_station)-1)/)
    ENDIF
  ENDIF
ELSE ! block_number not provided
  IF (PRESENT(station_id)) THEN ! station_id found, use it as block/station number
      WHERE(c_e(station_id))
        out_station(:) = MOD(station_id(:),1000)
        out_block(:) = MOD((station_id(:) - out_station(:))/1000,100)
      END WHERE
  ELSE ! station_id not found, invent block/station number
      out_station(:) = (/(MOD(i,1000),i=0,SIZE(out_station)-1)/)
      out_block(:) = MOD(((/(i,i=0,SIZE(out_station)-1)/))/1000,100)
  ENDIF
ENDIF

#ifdef DEBUG
DO i = 1, MIN(SIZE(out_block),10)
  CALL l4f_category_log(category, L4F_DEBUG, &
 'computed block/station numbers: '//t2c(out_block(i))//','//t2c(out_station(i)))
ENDDO
#endif

END SUBROUTINE compute_block_station_number

END PROGRAM v7d_add_wmo_info
