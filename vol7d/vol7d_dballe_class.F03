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

#include "config.h"

!> \brief  classe per import ed export di volumi da e in DB-All.e 
!!
!!Questo modulo definisce gli oggetti e i metodi per gestire
!!l'importazione e l'esportazione di volumi dal database per dati sparsi
!!DB-All.e
!!
!!Il tutto funziona intorno all'oggetto vol7d_dballe che aggiunge ad un
!!oggetto vol7d ulteriori informazioni.
!!
!!Con la chiamata init vengono definiti i parametri di accesso alla DSN
!!(database) di DB-All.e. oppure da file bufr/crex
!!
!!Con import è possibile acquisire nel prorio programma i dati presenti
!!nel DSN o su file; l'allocazione di memoria è automatica. Import è in grado di
!!importare dati senza nessuna ulteriore specificazione: vengono
!!acquisite tutte le stazioni senza limiti di tempo o di spazio, per
!!tutti i dati, compresi quelli di anagrafica; solo gli attributi
!!vengono tralasciati se non specificati. Se non specificato i dati sono
!!in forma character che permette di essere conservativi su tutti i tipi
!!di dato archiviabile in DB-all.e. Aumentando i parametri
!!all'intefaccia import è possibile specificare sottoinsiemi temporali e
!!spaziali, elenchi di variabili e attributi. E' possibile anche
!!specificare il tipo per ogni variabile o attributo richiesto. E'
!!fornito un set di routine per avere la possibilità di estrarre un
!!vettore di variabili e un vettore di reti. E' possibile anche specificare
!!una singola stazione. vol7d_dballe%%data_id
!!contiene un vettore di servizio con gli id interni del database che
!!indirizzano direttamente i dati. Una particolare opzione permette di
!!attivare l'opzione query di tipo best in DB-All.e per avere in una
!!unica rete i dati migliori presenti nel DB.
!!
!!Con export si riscrivono i dati nel DSN di DB-All.e potendo attivare
!!una serie di filtri.
!!
!!Con delete si elimina definitvamente l'oggetto vol7d_dballe.
!!
!!Mantenendo lo stesso oggetto nella sequenza init, import, export,
!!delete si sovrascrive lo stesso DSN di DB-All.e.  Il vettore
!!vol7d_dballe%%data_id in export permette di sovrascrivere solo gli
!!attributi; eventualmente possono essere sovrascritti i soli attributi
!!relativi agli elementi data_id non mancanti.
!!
!! E' da notare che se si attiva l'opzione "anaonly" solo ma tutte le stazioni e 
!! i dati di anagrafica vengono importati secondo i parametri di query selezionati.
!! Se l'opzione "anaonly" è disattivata solo le stazioni con i dati richiesti presenti 
!! saranno caricati nella sezione anagrafica
!!
!!Utilizzando due differenti oggetti uno per import e uno per export è
!!possibile tramite l'associazione del puntatore in essi contenuto
!!relativo al volume vol7d ricopiare contenuti in altri DSN senza
!!sprechi di memoria.
!!
!!Programma esempio
!!\include esempio_v7ddballe.f90
!!
!!\ingroup vol7d

MODULE vol7d_dballe_class

USE dballe_class
USE char_utilities
USE vol7d_class
USE array_utilities
use log4fortran
USE geo_coord_class
use list_mix
use list_character
use list_linkchar
use vol7d_serialize_dballe_class

IMPLICIT NONE

character (len=255),parameter:: subcategory="vol7d_dballe_class"

                                !>\brief Oggetto per import ed export da DB-All.e
!!
!!L'oggetto è costituito da un oggetto vol7d attorniato dalle 
!!informazioni necessarie per l'accesso al DSN di DB-All.e
!! e da una matrice necessaria per l'ottimizzazione della scrittura dei 
!!degli attributi dei dati in export

TYPE vol7d_dballe

  TYPE(vol7d) :: vol7d !< volume vol7d
  type(dbaconnection) :: idbhandle !< handle della connessioni al DSN DB-All.e
  type(dbasession) :: handle !< handle della sessione connessa al DSN DB-All.e
  !> memorizza gli id interni al database DB-All.e per 
  !!ottimizzare le riscritture degli attributi ai dati
  integer ,pointer :: data_id(:,:,:,:,:)
  integer :: time_definition
  integer :: category !< log4fortran
  logical :: file !<

END TYPE vol7d_dballe

INTEGER, PARAMETER, PRIVATE :: nftype = 2
CHARACTER(len=16), PARAMETER, PRIVATE :: &
 pathlist(2,nftype) = RESHAPE((/ &
 '/usr/share      ', '/usr/local/share', &
 '/etc            ', '/usr/local/etc  ' /), &
 (/2,nftype/))


type(vol7d_var),allocatable,private :: blocal(:) ! cache of dballe.txt

CHARACTER(len=20),PRIVATE :: dballe_name='wreport', dballe_name_env='DBA_TABLES'


                                !>\brief inizializza
INTERFACE init
  MODULE PROCEDURE vol7d_dballe_init
END INTERFACE init

                                !>\brief cancella
INTERFACE delete
  MODULE PROCEDURE vol7d_dballe_delete
END INTERFACE delete


                                !>\brief importa
INTERFACE import
  MODULE PROCEDURE  vol7d_dballe_importvvnv,vol7d_dballe_import, vol7d_dballe_import_old, dba2v7d
END INTERFACE import

                                !>\brief exporta
INTERFACE export
  MODULE PROCEDURE vol7d_dballe_export_old,vol7d_dballe_export, v7d2dba
END INTERFACE export


PRIVATE
PUBLIC vol7d_dballe, init, delete, import, export, vol7d_dballe_import_dballevar, vol7d_dballe_set_var_du

CONTAINS


                                !>\brief  inizializza l'oggetto
SUBROUTINE vol7d_dballe_init(this,dsn,user,password,write,wipe,repinfo,&
 filename,format,file,categoryappend,time_definition,idbhandle,template)


TYPE(vol7d_dballe),INTENT(out) :: this !< l'oggetto da inizializzare
character(len=*), INTENT(in),OPTIONAL :: dsn !< per l'accesso al DSN ( default="test" )
character(len=*), INTENT(in),OPTIONAL :: user !< per l'accesso al DSN ( default="test" )
character(len=*), INTENT(in),OPTIONAL :: password !< per l'accesso al DSN ( default="" )
logical,INTENT(in),OPTIONAL :: write !< abilita la scrittura sul DSN/file ( default=.false. )
logical,INTENT(in),OPTIONAL :: wipe !<  svuota il DSN/file e/o lo prepara per una scrittura ( default=.false. )
character(len=*), INTENT(in),OPTIONAL :: repinfo !< eventuale file repinfo.csv usato con wipe ( default="" )
character(len=*),intent(inout),optional :: filename !< nome dell'eventuale file da utilizzare in alternativa a dsn when file is true; se passato ="" ritorna un valore di deafult elaborato
character(len=*),intent(in),optional :: format !< the file format when file is true. It can be "BUFR" or "CREX". (default="BUFR")
logical,INTENT(in),OPTIONAL :: file !< switch to use file or data base ( default=.false )
character(len=*),INTENT(in),OPTIONAL :: categoryappend !< appennde questo suffisso al namespace category di log4fortran
integer,INTENT(in),OPTIONAL :: time_definition !< 0=time is reference time ; 1=time is validity time (default=1) 
integer,INTENT(in),OPTIONAL :: idbhandle !< dsn connection; if present it will be used
!> specificando category.subcategory.localcategory oppure un alias ("synop", "metar","temp","generic") forza l'exportazione ad uno specifico template BUFR/CREX"
!!  the special value "generic-frag is used to generate bufr on file where ana data is reported only once at beginning and data in other bufr after
character(len=*),intent(in),optional :: template !< default template to use exporting to file; can be overwritten by export

logical :: quiwrite,loadfile
character(len=512) :: a_name
character(len=254) :: arg,lfilename,lformat

quiwrite=.false.
if (present(write))then
  quiwrite=write
endif

if (present(categoryappend))then
  call l4f_launcher(a_name,a_name_append=trim(subcategory)//"."//trim(categoryappend))
else
  call l4f_launcher(a_name,a_name_append=trim(subcategory))
endif
this%category=l4f_category_get(a_name)

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'vol7d_dballe_init start')
#endif        

nullify(this%data_id)

if (optio_log(file)) then

  this%file=.true.

  lformat="BUFR"
  if (present(format))then
    lformat=format
  end if

  CALL getarg(0,arg)

  lfilename=trim(arg)//"."//trim(lformat)
  if (index(arg,'/',back=.true.) > 0) lfilename=lfilename(index(arg,'/',back=.true.)+1 : )

  if (present(filename))then
    if (filename /= "")then
      lfilename=filename
    end if
  end if

  if(quiwrite)then
                                !   this for write in memdb and write file on export
    loadfile=.false.
  else
    loadfile=.true.
  end if

  this%handle=dbasession(wipe=wipe,write=quiwrite,repinfo=repinfo,filename=lfilename,template=template,&
   memdb=.true.,loadfile=loadfile)

else

  this%file=.false.
  this%idbhandle=dbaconnection(dsn,user,password,idbhandle=idbhandle)
  this%handle=dbasession(this%idbhandle,wipe=wipe,write=quiwrite,repinfo=repinfo)

endif

this%time_definition = optio_i(time_definition)

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'vol7d_dballe_init end')
#endif        

END SUBROUTINE vol7d_dballe_init



                                !>\brief Identica a vol7d_dballe_importvsns con var e network vettore.
!!
!!import da DB-all.e

SUBROUTINE vol7d_dballe_importvvnv(this, var, network, coordmin,coordmax, timei, timef, level,timerange,set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,dataonly,ana)
TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in) :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(vol7d_ana),INTENT(inout),optional :: ana
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_network),INTENT(in) :: network(:)
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)
logical,intent(in),optional :: anaonly
logical,intent(in),optional :: dataonly

INTEGER :: i

if (size(network) == 0 )then
  CALL import(this,var, coordmin=coordmin, coordmax=coordmax, timei=timei, timef=timef, level=level,&
   timerange=timerange,set_network=set_network, attr=attr,anavar=anavar,anaattr=anaattr,&
   varkind=varkind,attrkind=attrkind,anavarkind=anavarkind,anaattrkind=anaattrkind,anaonly=anaonly,dataonly=dataonly,ana=ana)
else
  DO i = 1, SIZE(network)
    CALL import(this, var, network(i), coordmin, coordmax, timei, timef, level,timerange,set_network,&
     attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,dataonly,ana)
  ENDDO
end if

END SUBROUTINE vol7d_dballe_importvvnv

!>import da DB-all.e
SUBROUTINE vol7d_dballe_import_old(this, var, network, coordmin, coordmax, timei, timef,level,timerange, set_network,&
 attr,anavar,anaattr, varkind,attrkind,anavarkind,anaattrkind,anaonly,dataonly,ana)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:)
TYPE(geo_coord),INTENT(inout),optional :: coordmin,coordmax 
TYPE(vol7d_ana),INTENT(inout),optional :: ana
TYPE(datetime),INTENT(in),OPTIONAL :: timei, timef
TYPE(vol7d_network),INTENT(in),OPTIONAL :: network,set_network
TYPE(vol7d_level),INTENT(in),optional :: level
TYPE(vol7d_timerange),INTENT(in),optional :: timerange
CHARACTER(len=*),INTENT(in),OPTIONAL :: attr(:),anavar(:),anaattr(:)
CHARACTER(len=*),INTENT(in),OPTIONAL :: varkind(:),attrkind(:),anavarkind(:),anaattrkind(:)
logical,intent(in),optional :: anaonly
logical,intent(in),optional :: dataonly


INTEGER,PARAMETER :: maxvarlist=100
                                !TYPE(vol7d) :: v7d
                                ! da non fare (con gfortran?)!!!!!
                                !CHARACTER(len=SIZE(var)*7) :: varlist
                                !CHARACTER(len=SIZE(attr)*8) :: starvarlist

LOGICAL ::  ldegnet

INTEGER :: i
integer :: nvar 
integer :: nanavar

                                !CHARACTER(len=10),allocatable :: lvar(:), lanavar(:)
type(dbadcv)       :: vars,starvars,anavars,anastarvars
type(dbafilter)    :: filter
type(dbacoord)     :: mydbacoordmin, mydbacoordmax
type(dbaana)       :: mydbaana
type(dbadatetime)  :: mydatetimemin, mydatetimemax
type(dbatimerange) :: mydbatimerange
type(dbalevel)     :: mydbalevel
type(dbanetwork)   :: mydbanetwork

integer :: nanaattr,nattr

character(len=40) :: query

#ifdef DEBUG
CALL l4f_category_log(this%category,L4F_DEBUG,'inizio')
#endif        


IF (PRESENT(set_network)) THEN
  if (c_e(set_network)) then
    ldegnet = .TRUE.
  else
    ldegnet = .FALSE.
  end if
ELSE
  ldegnet = .FALSE.
ENDIF

if(ldegnet) then
  query = "best"
else
  query=cmiss
end if


                                !     uncommenti this if you want compatibility API with old import

!!$  if (allocated(starvars%dcv)) then
!!$    ldataonly=.false.
!!$  else
!!$    ldataonly=.true.
!!$  end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                ! ------------- prepare filter options

!!
!!           translate import option for dballe2003 api
!!

if (present(var)) then
  nvar=count(c_e(var))
  if (nvar > 0) then
    allocate (vars%dcv(nvar))
    do i=1,size(var)
      if (c_e(var(i)))then
        if (present(varkind))then
          select case (varkind(i))
          case("r")
            allocate (vars%dcv(i)%dat,source=dbadatar(var(i)))
          case("i")
            allocate (vars%dcv(i)%dat,source=dbadatai(var(i)))
          case("b")
            allocate (vars%dcv(i)%dat,source=dbadatab(var(i)))
          case("d")
            allocate (vars%dcv(i)%dat,source=dbadatad(var(i)))
          case("c")
            allocate (vars%dcv(i)%dat,source=dbadatac(var(i)))
          case default
            call l4f_category_log(this%category,L4F_ERROR,"var and varkind mismach")
            CALL raise_fatal_error()
          end select
        else 
          allocate (vars%dcv(i)%dat,source=dbadatac(var(i))) !char is default
        end if
      end if
    end do
  end if
end if

if (present(anavar)) then
  nanavar=count(c_e(anavar))
  if (nanavar > 0) then
    allocate (anavars%dcv(nanavar))
    do i=1,size(anavar)
      if (c_e(anavar(i)))then
        if (present(anavarkind))then
          select case (anavarkind(i))
          case("r")
            allocate (anavars%dcv(i)%dat,source=dbadatar(anavar(i)))
          case("i")
            allocate (anavars%dcv(i)%dat,source=dbadatai(anavar(i)))
          case("b")
            allocate (anavars%dcv(i)%dat,source=dbadatab(anavar(i)))
          case("d")
            allocate (anavars%dcv(i)%dat,source=dbadatad(anavar(i)))
          case("c")
            allocate (anavars%dcv(i)%dat,source=dbadatac(anavar(i)))
          case default
            call l4f_category_log(this%category,L4F_ERROR,"anavar and anavarkind mismach")
            CALL raise_fatal_error()
          end select
        else 
          allocate (anavars%dcv(i)%dat,source=dbadatac(anavar(i))) !char is default
        end if
      end if
    end do
  end if
end if

if (present(attr)) then
  nattr=size(attr)
  if (nattr == 0) then
    allocate (starvars%dcv(nattr))
  else
    nattr=count(c_e(attr))
    if (nattr > 0) then
      allocate (starvars%dcv(nattr))
      do i=1,size(attr)
        if (c_e(attr(i)))then
          if (present(attrkind))then
            select case (attrkind(i))
            case("r")
              allocate (starvars%dcv(i)%dat,source=dbadatar(attr(i)))
            case("i")
              allocate (starvars%dcv(i)%dat,source=dbadatai(attr(i)))
            case("b")
              allocate (starvars%dcv(i)%dat,source=dbadatab(attr(i)))
            case("d")
              allocate (starvars%dcv(i)%dat,source=dbadatad(attr(i)))
            case("c")
              allocate (starvars%dcv(i)%dat,source=dbadatac(attr(i)))
            case default
              call l4f_category_log(this%category,L4F_ERROR,"attr and attrkind mismach")
              CALL raise_fatal_error()
            end select
          else 
            allocate (starvars%dcv(i)%dat,source=dbadatac(attr(i))) !char is default
          end if
        end if
      end do
    end if
  endif
end if

if (present(anaattr)) then
  nanaattr=size(anaattr)
  if (nanaattr == 0) then
    allocate (anastarvars%dcv(nanaattr))
  else
    nanaattr=count(c_e(anaattr))
    if (nanaattr > 0) then
      allocate (anastarvars%dcv(nanaattr))
      do i=1,size(anaattr)
        if (c_e(anaattr(i)))then
          if (present(anaattrkind))then
            select case (anaattrkind(i))
            case("r")
              allocate (anastarvars%dcv(i)%dat,source=dbadatar(anaattr(i)))
            case("i")
              allocate (anastarvars%dcv(i)%dat,source=dbadatai(anaattr(i)))
            case("b")
              allocate (anastarvars%dcv(i)%dat,source=dbadatab(anaattr(i)))
            case("d")
              allocate (anastarvars%dcv(i)%dat,source=dbadatad(anaattr(i)))
            case("c")
              allocate (anastarvars%dcv(i)%dat,source=dbadatac(anaattr(i)))
            case default
              call l4f_category_log(this%category,L4F_ERROR,"attr and attrkind mismach")
              CALL raise_fatal_error()
            end select
          else 
            allocate (anastarvars%dcv(i)%dat,source=dbadatac(anaattr(i))) !char is default
          end if
        end if
      end do
    end if
  end if
end if


                                ! like a cast
mydbacoordmin=dbacoord()
if (present(coordmin)) mydbacoordmin%geo_coord=coordmin
mydbacoordmax=dbacoord()
if (present(coordmax)) mydbacoordmax%geo_coord=coordmax
mydbaana=dbaana()
if (present(ana)) mydbaana%vol7d_ana=ana
mydatetimemin=dbadatetime()
if (present(timei)) mydatetimemin%datetime=timei
mydatetimemax=dbadatetime()
if (present(timef)) mydatetimemax%datetime=timef
mydbatimerange=dbatimerange()
if (present(timerange)) mydbatimerange%vol7d_timerange=timerange
mydbalevel=dbalevel()
if (present(level)) mydbalevel%vol7d_level=level
mydbanetwork=dbanetwork()
if (present(network)) mydbanetwork%vol7d_network=network

!!
!!         here we have options ready for filter
!!
filter=dbafilter(coordmin=mydbacoordmin,coordmax=mydbacoordmax,ana=mydbaana, &
 datetimemin=mydatetimemin,datetimemax=mydatetimemax, &
 timerange=mydbatimerange,level=mydbalevel,network=mydbanetwork,query=query,&
 vars=vars,starvars=starvars,anavars=anavars,anastarvars=anastarvars,&
 dataonly=dataonly,anaonly=anaonly)
!!$  print *, "filter:"
!!$  call filter%display()

call import(this,filter,set_network)


END SUBROUTINE vol7d_dballe_import_old



!>import da DB-all.e
subroutine vol7d_dballe_import(this,filter,set_network)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto vol7d_dballe
type(dbafilter),INTENT(in)     :: filter
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network

TYPE(vol7d) :: vol7dtmp
type(dbametaanddata),allocatable :: metaanddatav(:)
type(dbafilter)    :: myfilter

CALL l4f_category_log(this%category,L4F_DEBUG,'start import vol7d_dballe')

if ( .not. filter%dataonly) then
                                ! ---------------->   constant station data
  myfilter=dbafilter(filter=filter,contextana=.true.)
!                                ! set filter
!  call this%handle%set(filter=myfilter)
                                ! estrude the data
  CALL l4f_category_log(this%category,L4F_DEBUG,'start import vol7d_dballe ingest')
  call this%handle%ingest(metaanddatav,filter=myfilter)
  CALL l4f_category_log(this%category,L4F_DEBUG,'end import vol7d_dballe ingest')
  CALL l4f_category_log(this%category,L4F_DEBUG,'start import vol7d_dballe dba2v7d')
  call dba2v7d(this%vol7d, metaanddatav,this%time_definition,set_network)
  CALL l4f_category_log(this%category,L4F_DEBUG,'end import vol7d_dballe dba2v7d')

  deallocate (metaanddatav)

else
                                ! empty volume
  call init(this%vol7d)
  call vol7d_alloc(this%vol7d)
  call vol7d_alloc_vol(this%vol7d)
end if
                                ! ---------------->   constant station data end

if ( .not. filter%anaonly) then
                                ! ---------------->   working on data
  myfilter=dbafilter(filter=filter,contextana=.false.)
!                                ! set filter
!  call this%handle%set(filter=myfilter)
                                ! estrude the data

  call this%handle%ingest(metaanddatav,filter=myfilter)

  call dba2v7d(vol7dtmp,metaanddatav,this%time_definition,set_network)

  deallocate (metaanddatav)

  CALL vol7d_merge(this%vol7d, vol7dtmp, sort=.TRUE.) ! Smart merge
!!$else
!!$                                ! should we sort separately in case no merge is done?
!!$  CALL vol7d_smart_sort(this%vol7d, lsort_time=.TRUE., lsort_timerange=.TRUE., lsort_level=.TRUE.)
end if

call vol7d_dballe_set_var_du(this%vol7d)


#ifdef NONE

!!$if (lattr) then
!!$
!!$  allocate  (this%data_id( nana, ntime, nlevel, ntimerange, nnetwork),stat=istat)
!!$  if (istat/= 0) THEN
!!$    CALL l4f_category_log(this%category,L4F_ERROR,'cannot allocate ' &
!!$     //TRIM(to_char(nana*ntime*nlevel*ntimerange*nnetwork))//' data_id elements')
!!$    CALL raise_fatal_error()
!!$    
!!$  ENDIF
!!$
!!$  this%data_id=DBA_MVI
!!$
!!$else

nullify(this%data_id)

!!$end if


                                !memorizzo data_id
#ifdef DEBUG
                                !CALL l4f_category_log(this%category,L4F_DEBUG,"data_id: "//trim(to_char(buffer(i)%data_id)))
#endif

this%data_id(indana,indtime,indlevel,indtimerange,indnetwork)=buffer(i)%data_id


ier=idba_set (this%handle,"*context_id",buffer(i)%data_id)
ier=idba_set (this%handle,"*var_related",buffer(i)%btable)
                                !per ogni dato ora lavoro sugli attributi
ier=idba_set(this%handle, "*varlist",starvarlist )
ier=idba_voglioancora (this%handle,nn)
                                !print*,buffer(i)%btable," numero attributi",nn

#endif

CALL l4f_category_log(this%category,L4F_DEBUG,'end import vol7d_dballe')

end subroutine vol7d_dballe_import



                                !>\brief Cancella l'oggetto

SUBROUTINE vol7d_dballe_delete(this, preserveidbhandle)
TYPE(vol7d_dballe) :: this !< oggetto da cancellare
logical,intent(in), optional :: preserveidbhandle !< do not close connection to dsn

call this%handle%delete()

if (.not. optio_log(preserveidbhandle)) call this%idbhandle%delete()

!!$if (associated(this%data_id)) then
!!$  deallocate (this%data_id)
!!$  nullify(this%data_id)
!!$end if

CALL delete(this%vol7d)

                                !chiudo il logger
call l4f_category_delete(this%category)
                                !ier=l4f_fini()

END SUBROUTINE vol7d_dballe_delete



                                !>\brief import dba objects in vol7d
!subroutine dba2v7d(this,metaanddatav,vars,starvars,anavars,anastarvars,time_definition, set_network)
subroutine dba2v7d(this,metaanddatav,time_definition, set_network)

type(dbametaanddata),intent(inout)      :: metaanddatav(:)  ! change value in datetime reguard timedefinition
TYPE(vol7d),INTENT(inout)               :: this
integer,INTENT(in),OPTIONAL             :: time_definition !< 0=time is reference time ; 1=time is validity time (default=1) 
TYPE(vol7d_network),INTENT(in),OPTIONAL :: set_network
type(dbadcv)                :: vars
type(dbadcv)                :: starvars
type(dbadcv)                :: anavars
type(dbadcv)                :: anastarvars


LOGICAL :: ldegnet
integer :: indana,indtime,indlevel,indtimerange,inddativar,indnetwork,indattrvar

integer :: nana,ntime,ntimerange,nlevel,nnetwork

INTEGER :: i, j, k, n
integer :: inddativarattr
integer :: nanavar, indanavar,indanavarattr,nanavarattr

integer :: ndativarr,     ndativari,     ndativarb,     ndativard,     ndativarc
integer :: ndatiattrr,    ndatiattri,    ndatiattrb,    ndatiattrd,    ndatiattrc 
integer :: ndativarattrr, ndativarattri, ndativarattrb, ndativarattrd, ndativarattrc

integer :: nanavarr,     nanavari,     nanavarb,     nanavard,     nanavarc
integer :: nanaattrr,    nanaattri,    nanaattrb,    nanaattrd,    nanaattrc 
integer :: nanavarattrr, nanavarattri, nanavarattrb, nanavarattrd, nanavarattrc

integer :: ndativar,ndativarattr

type(characterlist) :: dativarl,dativarattrl,anavarl,anavarattrl

character(len=listcharmaxlen),allocatable :: dativara(:),dativarattra(:),anavara(:),anavarattra(:)
logical :: status
integer :: ltime_definition

type(datetime),allocatable :: tmptime(:)
type(vol7d_network),allocatable :: tmpnetwork(:)
type(vol7d_level),allocatable :: tmplevel(:)
type(vol7d_timerange),allocatable :: tmptimerange(:)
type(vol7d_ana),allocatable :: tmpana(:)


ltime_definition=optio_i(time_definition)
if (.not. c_e(ltime_definition)) ltime_definition = 1

                                ! take in account time_definition
if (ltime_definition == 0) then
  do i =1,size(metaanddatav)
    metaanddatav(i)%metadata%datetime%datetime = &
     metaanddatav(i)%metadata%datetime%datetime - &
     timedelta_new(msec=metaanddatav(i)%metadata%timerange%vol7d_timerange%p1*1000)
  end do
end if


IF (PRESENT(set_network)) THEN
  if (c_e(set_network)) then
    ldegnet = .TRUE.
  else
    ldegnet = .FALSE.
  end if
ELSE
  ldegnet = .FALSE.
ENDIF



!!--------------------------------------------------------------------------
!! find vars, starvars, anavars, anastarvars
!!

! create lists of all
                                ! data
do i =1, size(metaanddatav)
  do j=1, size(metaanddatav(i)%dataattrv%dataattr)
    if (c_e(metaanddatav(i)%metadata%datetime%datetime)) then
                                !print *,"dativarl: ", metaanddatav(i)%dataattrv%dataattr(j)%dat%btable
      call dativarl%append(metaanddatav(i)%dataattrv%dataattr(j)%dat%btable)
    else
                                !print *,"anavarl: ", metaanddatav(i)%dataattrv%dataattr(j)%dat%btable
      call anavarl%append(metaanddatav(i)%dataattrv%dataattr(j)%dat%btable)
    end if
  end do
end do

!count and put in vector of unuique key
ndativar     = count_distinct (toarray_charl(dativarl)    , back=.TRUE.)
allocate(dativara(ndativar))
call pack_distinct_c (toarray_charl(dativarl)    , dativara    , back=.TRUE.)
status = dativarl%delete()
allocate (vars%dcv(ndativar))

nanavar      = count_distinct (toarray_charl(anavarl)     , back=.TRUE.)
allocate(anavara(nanavar))
call pack_distinct_c (toarray_charl(anavarl)     , anavara     , back=.TRUE.)
status = anavarl%delete()
allocate (anavars%dcv(nanavar))


an: do n=1,ndativar
  do i =1, size(metaanddatav)
    do j=1, size(metaanddatav(i)%dataattrv%dataattr)
      if (c_e(metaanddatav(i)%metadata%datetime%datetime)) then
        if (metaanddatav(i)%dataattrv%dataattr(j)%dat%btable == dativara(n)) then
          allocate(vars%dcv(n)%dat,source=metaanddatav(i)%dataattrv%dataattr(j)%dat)
          cycle an
        end if
      end if
    end do
  end do 
end do an

bn: do n=1,nanavar
  do i =1, size(metaanddatav)
    do j=1, size(metaanddatav(i)%dataattrv%dataattr)
      if (.not. c_e(metaanddatav(i)%metadata%datetime%datetime)) then
        if (metaanddatav(i)%dataattrv%dataattr(j)%dat%btable == anavara(n)) then
          allocate(anavars%dcv(n)%dat,source=metaanddatav(i)%dataattrv%dataattr(j)%dat)
          cycle bn
        end if
      end if
    end do
  end do
end do bn

                                ! attributes
do i =1, size(metaanddatav)
  do j=1, size(metaanddatav(i)%dataattrv%dataattr)
    do k=1, size(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
      if (c_e(metaanddatav(i)%metadata%datetime%datetime)) then
                                !print *,"dativarattrl: ", metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable
        call dativarattrl%append(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable)
      else
                                !print *,"anavarattrl: ", metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable
        call anavarattrl%append(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable)
      end if
    end do
  end do
end do


ndativarattr = count_distinct (toarray_charl(dativarattrl), back=.TRUE.)
allocate(dativarattra(ndativarattr))
call pack_distinct_c (toarray_charl(dativarattrl), dativarattra, back=.TRUE.)
status = dativarattrl%delete()
allocate(starvars%dcv(ndativarattr))

nanavarattr  = count_distinct (toarray_charl(anavarattrl) , back=.TRUE.)
allocate(anavarattra(nanavarattr))
call pack_distinct_c (toarray_charl(anavarattrl) , anavarattra , back=.TRUE.)
status = anavarattrl%delete()
allocate(anastarvars%dcv(nanavarattr))


cn: do n=1,ndativarattr
  do i =1, size(metaanddatav)
    do j=1, size(metaanddatav(i)%dataattrv%dataattr)
      do k=1, size(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
        if (c_e(metaanddatav(i)%metadata%datetime%datetime)) then
          if (metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable == dativarattra(n))then 
            allocate(starvars%dcv(n)%dat,source=metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat)
            cycle cn
          end if
        end if
      end do
    end do
  end do
end do cn


dn: do n=1,nanavarattr
  do i =1, size(metaanddatav)
    do j=1, size(metaanddatav(i)%dataattrv%dataattr)
      do k=1, size(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
        if (.not. c_e(metaanddatav(i)%metadata%datetime%datetime)) then
          if (metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat%btable == anavarattra(n))then 
            allocate(anastarvars%dcv(n)%dat,source=metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat)
            cycle dn
          end if
        end if
      end do
    end do
  end do
end do dn


!!--------------------------------------------------------------------------


!!
!!             count all unique metadata
!!

if(ldegnet) then
  nnetwork=1
else
  !nnetwork = count_distinct(metaanddatav(:)%metadata%network%vol7d_network, back=.TRUE.)
  allocate (tmpnetwork(size(metaanddatav(:)%metadata%network%vol7d_network)),&
   source=metaanddatav(:)%metadata%network%vol7d_network)
  call sort(tmpnetwork)
  nnetwork = count_distinct_sorted(tmpnetwork)
end if

!ntime      = count_distinct(metaanddatav(:)%metadata%datetime%datetime, &
! mask=c_e(metaanddatav(:)%metadata%datetime%datetime),back=.TRUE.)
allocate (tmptime(size(metaanddatav(:)%metadata%datetime%datetime)),&
 source=metaanddatav(:)%metadata%datetime%datetime)
call sort(tmptime)
ntime = count_distinct_sorted(tmptime,mask=c_e(tmptime))

!ntimerange = count_distinct(metaanddatav(:)%metadata%timerange%vol7d_timerange, &
! mask=c_e(metaanddatav(:)%metadata%timerange%vol7d_timerange), back=.TRUE.)
allocate (tmptimerange(size(metaanddatav(:)%metadata%timerange%vol7d_timerange)),&
 source=metaanddatav(:)%metadata%timerange%vol7d_timerange)
call sort(tmptimerange)
ntimerange = count_distinct_sorted(tmptimerange,mask=c_e(tmptimerange))

!nlevel     = count_distinct(metaanddatav(:)%metadata%level%vol7d_level, &
! mask=c_e(metaanddatav(:)%metadata%level%vol7d_level),back=.TRUE.)
allocate (tmplevel(size(metaanddatav(:)%metadata%level%vol7d_level)),&
 source=metaanddatav(:)%metadata%level%vol7d_level)
call sort(tmplevel)
nlevel = count_distinct_sorted(tmplevel,mask=c_e(tmplevel))

!nana   = count_distinct(metaanddatav(:)%metadata%ana%vol7d_ana, back=.TRUE.)
allocate (tmpana(size(metaanddatav(:)%metadata%ana%vol7d_ana)),&
 source=metaanddatav(:)%metadata%ana%vol7d_ana)
call sort(tmpana)
nana = count_distinct_sorted(tmpana)

!!$if(ldegnet) then
!!$  nnetwork=1
!!$else
!!$  nnetwork = size(metaanddatav(:)%metadata%network%vol7d_network)
!!$end if
!!$ntime      = size(metaanddatav(:)%metadata%datetime%datetime)
!!$ntimerange = size(metaanddatav(:)%metadata%timerange%vol7d_timerange)
!!$nlevel     = size(metaanddatav(:)%metadata%level%vol7d_level)
!!$nana       = size(metaanddatav(:)%metadata%ana%vol7d_ana)

                                ! var

ndativarr = 0
ndativari = 0
ndativarb = 0
ndativard = 0
ndativarc = 0

do i =1 ,size(vars%dcv)
  associate (dato => vars%dcv(i)%dat)
    select type (dato)
    type is (dbadatar)
      ndativarr = ndativarr + 1
    type is (dbadatai)
      ndativari = ndativari + 1
    type is (dbadatab)
      ndativarb = ndativarb + 1
    type is (dbadatad)
      ndativard = ndativard + 1
    type is (dbadatac)
      ndativarc = ndativarc + 1
    end select
  end associate
end do


                                !attr

ndatiattrr = 0
ndatiattri = 0
ndatiattrb = 0
ndatiattrd = 0
ndatiattrc = 0

do i =1 ,size(starvars%dcv)
  associate (dato => starvars%dcv(i)%dat)
    select type (dato)
    type is (dbadatar)
      ndatiattrr = ndatiattrr + 1
    type is (dbadatai)
      ndatiattri = ndatiattri + 1
    type is (dbadatab)
      ndatiattrb = ndatiattrb + 1
    type is (dbadatad)
      ndatiattrd = ndatiattrd + 1
    type is (dbadatac)
      ndatiattrc = ndatiattrc + 1
    end select
  end associate
end do


                                ! ana var

nanavarr = 0
nanavari = 0
nanavarb = 0
nanavard = 0
nanavarc = 0

do i =1 ,size(anavars%dcv)
  associate (dato => anavars%dcv(i)%dat)
    select type (dato)
    type is (dbadatar)
      nanavarr = nanavarr + 1
    type is (dbadatai)
      nanavari = nanavari + 1
    type is (dbadatab)
      nanavarb = nanavarb + 1
    type is (dbadatad)
      nanavard = nanavard + 1
    type is (dbadatac)
      nanavarc = nanavarc + 1
    end select
  end associate
end do


                                ! ana attr

nanaattrr = 0
nanaattri = 0
nanaattrb = 0
nanaattrd = 0
nanaattrc = 0

do i =1 ,size(anastarvars%dcv)
  associate (dato => anastarvars%dcv(i)%dat)
    select type (dato)
    type is (dbadatar)
      nanaattrr = nanaattrr + 1
    type is (dbadatai)
      nanaattri = nanaattri + 1
    type is (dbadatab)
      nanaattrb = nanaattrb + 1
    type is (dbadatad)
      nanaattrd = nanaattrd + 1
    type is (dbadatac)
      nanaattrc = nanaattrc + 1
    end select
  end associate
end do


                                !refine

ndativarattrr=0
ndativarattri=0
ndativarattrb=0
ndativarattrd=0
ndativarattrc=0

if (ndatiattrr > 0 ) ndativarattrr=ndativarr+ndativari+ndativarb+ndativard+ndativarc
if (ndatiattri > 0 ) ndativarattri=ndativarr+ndativari+ndativarb+ndativard+ndativarc
if (ndatiattrb > 0 ) ndativarattrb=ndativarr+ndativari+ndativarb+ndativard+ndativarc
if (ndatiattrd > 0 ) ndativarattrd=ndativarr+ndativari+ndativarb+ndativard+ndativarc
if (ndatiattrc > 0 ) ndativarattrc=ndativarr+ndativari+ndativarb+ndativard+ndativarc


nanavarattrr=0
nanavarattri=0
nanavarattrb=0
nanavarattrd=0
nanavarattrc=0

if (nanaattrr > 0 ) nanavarattrr=nanavarr+nanavari+nanavarb+nanavard+nanavarc
if (nanaattri > 0 ) nanavarattri=nanavarr+nanavari+nanavarb+nanavard+nanavarc
if (nanaattrb > 0 ) nanavarattrb=nanavarr+nanavari+nanavarb+nanavard+nanavarc
if (nanaattrd > 0 ) nanavarattrd=nanavarr+nanavari+nanavarb+nanavard+nanavarc
if (nanaattrc > 0 ) nanavarattrc=nanavarr+nanavari+nanavarb+nanavard+nanavarc


CALL init(this,time_definition=ltime_definition)

!!$print *, "nana=",nana, "ntime=",ntime, "ntimerange=",ntimerange, &
!!$ "nlevel=",nlevel, "nnetwork=",nnetwork, &
!!$ "ndativarr=",ndativarr, "ndativari=",ndativari, &
!!$ "ndativarb=",ndativarb, "ndativard=",ndativard, "ndativarc=",ndativarc,&
!!$ "ndatiattrr=",ndatiattrr, "ndatiattri=",ndatiattri, "ndatiattrb=",ndatiattrb,&
!!$ "ndatiattrd=",ndatiattrd, "ndatiattrc=",ndatiattrc,&
!!$ "ndativarattrr=",ndativarattrr, "ndativarattri=",ndativarattri, "ndativarattrb=",ndativarattrb,&
!!$ "ndativarattrd=",ndativarattrd, "ndativarattrc=",ndativarattrc
!!$
!!$print *,"nanaattrr,nanaattri,nanaattrb,nanaattrd,nanaattrc"
!!$print *,nanaattrr,nanaattri,nanaattrb,nanaattrd,nanaattrc


call vol7d_alloc (this, &
nana=nana, ntime=ntime, ntimerange=ntimerange, &
nlevel=nlevel, nnetwork=nnetwork, &
ndativarr=ndativarr, ndativari=ndativari, ndativarb=ndativarb, ndativard=ndativard, ndativarc=ndativarc,&
ndatiattrr=ndatiattrr, ndatiattri=ndatiattri, ndatiattrb=ndatiattrb, ndatiattrd=ndatiattrd, ndatiattrc=ndatiattrc,&
ndativarattrr=ndativarattrr, &
ndativarattri=ndativarattri, &
ndativarattrb=ndativarattrb, &
ndativarattrd=ndativarattrd, &
ndativarattrc=ndativarattrc,&
nanavarr=nanavarr, nanavari=nanavari, nanavarb=nanavarb, nanavard=nanavard, nanavarc=nanavarc,&
nanaattrr=nanaattrr, nanaattri=nanaattri, nanaattrb=nanaattrb, nanaattrd=nanaattrd, nanaattrc=nanaattrc,&
nanavarattrr=nanavarattrr, &
nanavarattri=nanavarattri, &
nanavarattrb=nanavarattrb, &
nanavarattrd=nanavarattrd, &
nanavarattrc=nanavarattrc)


! fill metadata removing contextana metadata

!nana=count_and_pack_distinct(metaanddatav(:)%metadata%ana%vol7d_ana,this%ana, back=.TRUE.)
!this%ana=pack_distinct(metaanddatav(:)%metadata%ana%vol7d_ana, nana, back=.TRUE.)
this%ana=pack_distinct_sorted(tmpana, nana)
deallocate(tmpana)
!call sort(this%ana)

!ntime=count_and_pack_distinct(metaanddatav(:)%metadata%datetime%datetime,this%time, &
! mask=c_e(metaanddatav(:)%metadata%datetime%datetime), back=.TRUE.)
!this%time=pack_distinct(metaanddatav(:)%metadata%datetime%datetime, ntime, &
! mask=c_e(metaanddatav(:)%metadata%datetime%datetime),back=.TRUE.)
this%time=pack_distinct_sorted(tmptime, ntime,mask=c_e(tmptime))
deallocate(tmptime)
!call sort(this%time)

!ntimerange=count_and_pack_distinct(metaanddatav(:)%metadata%timerange%vol7d_timerange,this%timerange, &
! mask=c_e(metaanddatav(:)%metadata%timerange%vol7d_timerange), back=.TRUE.)
!this%timerange=pack_distinct(metaanddatav(:)%metadata%timerange%vol7d_timerange, ntimerange, &
! mask=c_e(metaanddatav(:)%metadata%timerange%vol7d_timerange), back=.TRUE.)
this%timerange=pack_distinct_sorted(tmptimerange, ntimerange,mask=c_e(tmptimerange))
deallocate(tmptimerange)
!call sort(this%timerange)

!nlevel=count_and_pack_distinct(metaanddatav(:)%metadata%level%vol7d_level,this%level, &
! mask=c_e(metaanddatav(:)%metadata%level%vol7d_level), back=.TRUE.)
!this%level=pack_distinct(metaanddatav(:)%metadata%level%vol7d_level, nlevel, &
! mask=c_e(metaanddatav(:)%metadata%level%vol7d_level), back=.TRUE.)
this%level=pack_distinct_sorted(tmplevel, nlevel,mask=c_e(tmplevel))
deallocate(tmplevel)
!call sort(this%level)

if(ldegnet)then
  nnetwork=1
  ALLOCATE(this%network(1))
  this%network(1)=set_network
else
  !nnetwork=count_and_pack_distinct(metaanddatav(:)%metadata%network%vol7d_network,this%network, back=.TRUE.)
  !this%network=pack_distinct(metaanddatav(:)%metadata%network%vol7d_network, nnetwork, back=.TRUE.)
  this%network=pack_distinct_sorted(tmpnetwork, nnetwork)
  deallocate(tmpnetwork)
end if
!call sort(this%network)

                                ! var

ndativarr = 0
ndativari = 0
ndativarb = 0
ndativard = 0
ndativarc = 0

do i =1 ,size(vars%dcv)
  associate (dato => vars%dcv(i)%dat)
    select type (dato)
    type is (dbadatar)
      ndativarr = ndativarr + 1
      call init (this%dativar%r(ndativarr), btable=dato%btable)
    type is (dbadatai)
      ndativari = ndativari + 1
      call init (this%dativar%i(ndativari), btable=dato%btable)
    type is (dbadatab)
      ndativarb = ndativarb + 1
      call init (this%dativar%b(ndativarb), btable=dato%btable)
    type is (dbadatad)
      ndativard = ndativard + 1
      call init (this%dativar%d(ndativard), btable=dato%btable)
    type is (dbadatac)
      ndativarc = ndativarc + 1
      call init (this%dativar%c(ndativarc), btable=dato%btable)
    end select
  end associate
end do


                                !attr

ndatiattrr = 0
ndatiattri = 0
ndatiattrb = 0
ndatiattrd = 0
ndatiattrc = 0

do i =1 ,size(starvars%dcv)
  associate (dato => starvars%dcv(i)%dat)
    select type (dato)
    type is (dbadatar)
      ndatiattrr = ndatiattrr + 1
      call init (this%datiattr%r(ndatiattrr), btable=dato%btable)
    type is (dbadatai)
      ndatiattri = ndatiattri + 1
      call init (this%datiattr%i(ndatiattri), btable=dato%btable)
    type is (dbadatab)
      ndatiattrb = ndatiattrb + 1
      call init (this%datiattr%b(ndatiattrb), btable=dato%btable)
    type is (dbadatad)
      ndatiattrd = ndatiattrd + 1
      call init (this%datiattr%d(ndatiattrd), btable=dato%btable)
    type is (dbadatac)
      ndatiattrc = ndatiattrc + 1
      call init (this%datiattr%c(ndatiattrc), btable=dato%btable)
    end select
  end associate
end do


                                ! ana var

nanavarr = 0
nanavari = 0
nanavarb = 0
nanavard = 0
nanavarc = 0

do i =1 ,size(anavars%dcv)
  associate (dato => anavars%dcv(i)%dat)
    select type (dato)
    type is (dbadatar)
      nanavarr = nanavarr + 1
      call init (this%anavar%r(nanavarr), btable=dato%btable)
    type is (dbadatai)
      nanavari = nanavari + 1
      call init (this%anavar%i(nanavari), btable=dato%btable)
    type is (dbadatab)
      nanavarb = nanavarb + 1
      call init (this%anavar%b(nanavarb), btable=dato%btable)
    type is (dbadatad)
      nanavard = nanavard + 1
      call init (this%anavar%d(nanavard), btable=dato%btable)
    type is (dbadatac)
      nanavarc = nanavarc + 1
      call init (this%anavar%c(nanavarc), btable=dato%btable)
    end select
  end associate
end do


                                ! ana attr

nanaattrr = 0
nanaattri = 0
nanaattrb = 0
nanaattrd = 0
nanaattrc = 0

do i =1 ,size(anastarvars%dcv)
  associate (dato => anastarvars%dcv(i)%dat)
    select type (dato)
    type is (dbadatar)
      nanaattrr = nanaattrr + 1
      call init (this%anaattr%r(nanaattrr), btable=dato%btable)
    type is (dbadatai)
      nanaattri = nanaattri + 1
      call init (this%anaattr%i(nanaattri), btable=dato%btable)
    type is (dbadatab)
      nanaattrb = nanaattrb + 1
      call init (this%anaattr%b(nanaattrb), btable=dato%btable)
    type is (dbadatad)
      nanaattrd = nanaattrd + 1
      call init (this%anaattr%d(nanaattrd), btable=dato%btable)
    type is (dbadatac)
      nanaattrc = nanaattrc + 1
      call init (this%anaattr%c(nanaattrc), btable=dato%btable)
    end select
  end associate
end do


                                ! here we colcolate the link from attributes and vars
do i =1, size(vars%dcv)
  associate (dato => vars%dcv(i)%dat)
    if ( ndativarattri > 0 ) call init(this%dativarattr%i(i),btable=dato%btable)
    if ( ndativarattrr > 0 ) call init(this%dativarattr%r(i),btable=dato%btable)
    if ( ndativarattrd > 0 ) call init(this%dativarattr%d(i),btable=dato%btable)
    if ( ndativarattrb > 0 ) call init(this%dativarattr%b(i),btable=dato%btable)
    if ( ndativarattrc > 0 ) call init(this%dativarattr%c(i),btable=dato%btable)
  end associate
end do

do i =1, size(anavars%dcv)
  associate (dato => anavars%dcv(i)%dat)
    if ( nanavarattri > 0 ) call init(this%anavarattr%i(i),btable=dato%btable)
    if ( nanavarattrr > 0 ) call init(this%anavarattr%r(i),btable=dato%btable)
    if ( nanavarattrd > 0 ) call init(this%anavarattr%d(i),btable=dato%btable)
    if ( nanavarattrb > 0 ) call init(this%anavarattr%b(i),btable=dato%btable)
    if ( nanavarattrc > 0 ) call init(this%anavarattr%c(i),btable=dato%btable)
  end associate
end do

                                ! set index in dativaratt*
call vol7d_set_attr_ind(this)

call vol7d_alloc_vol (this)

                                ! Ora qui bisogna metterci dentro idati
indana = 0
indtime = 0
indnetwork = 0
indtime = 0
indtimerange = 0
indlevel = 0
do i =1, size(metaanddatav)

  indana       = INDEX(this%ana, metaanddatav(i)%metadata%ana%vol7d_ana, cache=indana)
  if (ldegnet)then
    indnetwork=1
  else
    indnetwork = INDEX(this%network, metaanddatav(i)%metadata%network%vol7d_network, cache=indnetwork)
  endif
  
  
  if (c_e(metaanddatav(i)%metadata%datetime%datetime)) then      ! dati

    indtime      = INDEX(this%time, metaanddatav(i)%metadata%datetime%datetime, cache=indtime)
    indtimerange = INDEX(this%timerange, metaanddatav(i)%metadata%timerange%vol7d_timerange, cache=indtimerange)
    indlevel     = INDEX(this%level, metaanddatav(i)%metadata%level%vol7d_level, cache=indlevel)
    
    do j=1, size(metaanddatav(i)%dataattrv%dataattr)
      
      associate (dato => metaanddatav(i)%dataattrv%dataattr(j)%dat)
        select type (dato)
        type is (dbadatai)
          inddativar = firsttrue(dato%btable == this%dativar%i%btable)
          this%voldatii( &
           indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
           ) = dato%value
          
        type is (dbadatar)
          inddativar = firsttrue(dato%btable == this%dativar%r%btable)
          this%voldatir( &
           indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
           ) = dato%value
          
        type is (dbadatad)
          inddativar = firsttrue(dato%btable == this%dativar%d%btable)
          this%voldatid( &
           indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
           ) = dato%value
          
        type is (dbadatab)
          inddativar = firsttrue(dato%btable == this%dativar%b%btable)
          this%voldatib( &
           indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
           ) = dato%value
          
        type is (dbadatac)
          inddativar = firsttrue(dato%btable == this%dativar%c%btable)
          this%voldatic( &
           indana,indtime,indlevel,indtimerange,inddativar,indnetwork &
           ) = dato%value
          
        end select
        
        
                                ! dati attributes
        do k=1, size(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
          associate (attr => metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat)
            select type (attr)
              
            type is (dbadatai)
              inddativarattr  = firsttrue(dato%btable == this%dativarattr%i%btable)
              indattrvar = firsttrue(attr%btable == this%datiattr%i%btable)
              this%voldatiattri( &
               indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
               ) = attr%value
            type is (dbadatar)
              inddativarattr  = firsttrue(dato%btable == this%dativarattr%r%btable)
              indattrvar = firsttrue(attr%btable == this%datiattr%r%btable)
              this%voldatiattrr( &
               indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
               ) = attr%value
            type is (dbadatad)
              inddativarattr  = firsttrue(dato%btable == this%dativarattr%d%btable)
              indattrvar = firsttrue(attr%btable == this%datiattr%d%btable)
              this%voldatiattrd( &
               indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
               ) = attr%value
            type is (dbadatab)
              inddativarattr  = firsttrue(dato%btable == this%dativarattr%b%btable)
              indattrvar = firsttrue(attr%btable == this%datiattr%b%btable)
              this%voldatiattrb( &
               indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
               ) = attr%value
            type is (dbadatac)
              inddativarattr  = firsttrue(dato%btable == this%dativarattr%c%btable)
              indattrvar = firsttrue(attr%btable == this%datiattr%c%btable)
              this%voldatiattrc( &
               indana,indtime,indlevel,indtimerange,inddativarattr,indnetwork,indattrvar &
               ) = attr%value
              
            end select
          end associate
        end do
      end associate
    end do

  else
                                ! ana
    do j=1, size(metaanddatav(i)%dataattrv%dataattr)
      
      associate (dato => metaanddatav(i)%dataattrv%dataattr(j)%dat)
        select type (dato)
        type is (dbadatai)
          indanavar = firsttrue(dato%btable == this%anavar%i%btable)
          this%volanai( &
           indana,indanavar,indnetwork &
           ) = dato%value
          
        type is (dbadatar)
          indanavar = firsttrue(dato%btable == this%anavar%r%btable)
          this%volanar( &
           indana,indanavar,indnetwork &
           ) = dato%value
          
        type is (dbadatad)
          indanavar = firsttrue(dato%btable == this%anavar%d%btable)
          this%volanad( &
           indana,indanavar,indnetwork &
           ) = dato%value
          
        type is (dbadatab)
          indanavar = firsttrue(dato%btable == this%anavar%b%btable)
          this%volanab( &
           indana,indanavar,indnetwork &
           ) = dato%value
          
        type is (dbadatac)
          indanavar = firsttrue(dato%btable == this%anavar%c%btable)
          this%volanac( &
           indana,indanavar,indnetwork &
           ) = dato%value
          
        end select
        
        
                                ! ana attributes
        do k=1, size(metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv)
          associate (attr => metaanddatav(i)%dataattrv%dataattr(j)%attrv%dcv(k)%dat)
            select type (attr)
              
            type is (dbadatai)
              indanavarattr  = firsttrue(dato%btable == this%anavarattr%i%btable)
              indattrvar = firsttrue(attr%btable == this%anaattr%i%btable)
              this%volanaattri( &
               indana,indanavarattr,indnetwork,indattrvar &
               ) = attr%value
            type is (dbadatar)
              indanavarattr  = firsttrue(dato%btable == this%anavarattr%r%btable)
              indattrvar = firsttrue(attr%btable == this%anaattr%r%btable)
              this%volanaattrr( &
               indana,indanavarattr,indnetwork,indattrvar &
               ) = attr%value
            type is (dbadatad)
              indanavarattr  = firsttrue(dato%btable == this%anavarattr%d%btable)
              indattrvar = firsttrue(attr%btable == this%anaattr%d%btable)
              this%volanaattrd( &
               indana,indanavarattr,indnetwork,indattrvar &
               ) = attr%value
            type is (dbadatab)
              indanavarattr  = firsttrue(dato%btable == this%anavarattr%b%btable)
              indattrvar = firsttrue(attr%btable == this%anaattr%b%btable)
              this%volanaattrb( &
               indana,indanavarattr,indnetwork,indattrvar &
               ) = attr%value
            type is (dbadatac)
              indanavarattr  = firsttrue(dato%btable == this%anavarattr%c%btable)
              indattrvar = firsttrue(attr%btable == this%anaattr%c%btable)
              this%volanaattrc( &
               indana,indanavarattr,indnetwork,indattrvar &
               ) = attr%value
              
            end select
          end associate
        end do
      end associate
    end do
  end if
end do

contains

!!$!> /brief Return an dbadcv from a mixlist with dbadata* type 
!!$function todcv_dbadat(this)
!!$type(dbadcv) :: todcv_dbadat !< array
!!$type(mixlist) :: this
!!$
!!$integer :: i
!!$
!!$allocate (todcv_dbadat%dcv(this%countelements()))
!!$
!!$call this%rewind()
!!$i=0
!!$do while(this%element())
!!$  i=i+1
!!$
!!$  associate (dato => this%current())
!!$    select type (dato)
!!$    type is (dbadatar)
!!$      allocate(todcv_dbadat%dcv(i)%dat,source=dato)
!!$    type is (dbadatai)
!!$      allocate(todcv_dbadat%dcv(i)%dat,source=dato)
!!$    type is (dbadatab)
!!$      allocate(todcv_dbadat%dcv(i)%dat,source=dato)
!!$    type is (dbadatad)
!!$      allocate(todcv_dbadat%dcv(i)%dat,source=dato)
!!$    type is (dbadatac)
!!$      allocate(todcv_dbadat%dcv(i)%dat,source=dato)
!!$    end select
!!$  end associate
!!$
!!$  call this%next()
!!$end do
!!$end function todcv_dbadat

!!$! Definisce le funzioni count_distinct e pack_distinct
!!$#define VOL7D_POLY_TYPE TYPE(dbadata)
!!$#define VOL7D_POLY_TYPES _dbadata
!!$#undef ENABLE_SORT
!!$#include "array_utilities_inc.F90"
!!$#undef VOL7D_POLY_TYPE
!!$#undef VOL7D_POLY_TYPES


end subroutine dba2v7d


subroutine vol7d_dballe_import_dballevar(this)

type(vol7d_var),pointer :: this(:)
INTEGER :: i,un,n

IF (associated(this)) return
IF (allocated(blocal)) then
  ALLOCATE(this(size(blocal)))
  this=blocal
  return
end if

un = open_dballe_file('dballe.txt', filetype_data)
IF (un < 0) then

  call l4f_log(L4F_ERROR,"error open_dballe_file: dballe.txt")
  CALL raise_error("error open_dballe_file: dballe.txt")
  return
end if

n = 0
DO WHILE(.TRUE.)
  READ(un,*,END=100)
  n = n + 1
ENDDO
100 CONTINUE

IF (n > 0) THEN
  ALLOCATE(this(n))
  ALLOCATE(blocal(n))
  REWIND(un)
  readline: do i = 1 ,n
    READ(un,'(1x,A6,1x,a65,a24,i4)')blocal(i)%btable,blocal(i)%description,blocal(i)%unit,&
     blocal(i)%scalefactor
    blocal(i)%btable(:1)="B"
    !print*,"B=",blocal(i)%btable
    !print*," D=",blocal(i)%description
    !PRINT*," U=",blocal(i)%unit
    !PRINT*," D=",blocal(i)%scalefactor
  ENDDO readline

  CALL l4f_log(L4F_INFO,'Found '//TRIM(to_char(i-1))//' variables in dballe master table')

  this=blocal

ENDIF
CLOSE(un)

END SUBROUTINE vol7d_dballe_import_dballevar



!> \brief Integra il vettore delle variabili in vol7d con le descrizioni e le unità di misura
!!eventualmente mancanti.

subroutine vol7d_dballe_set_var_du(this)

TYPE(vol7d) :: this !< oggetto vol7d con le variabili da completare
integer :: i,j
type(vol7d_var),pointer :: dballevar(:)


call vol7d_dballe_import_dballevar(dballevar)

#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME dativar


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V

#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME anavar


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V


#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME datiattr


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V


#undef VOL7D_POLY_NAME
#define VOL7D_POLY_NAME anaattr


#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V r
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V i
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V b
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V d
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V
#define VOL7D_POLY_TYPES_V c
#include "vol7d_dballe_class_var_du.F90"
#undef VOL7D_POLY_TYPES_V


deallocate(dballevar)

return

end subroutine vol7d_dballe_set_var_du



FUNCTION get_dballe_filepath(filename, filetype) RESULT(path)
CHARACTER(len=*), INTENT(in) :: filename
INTEGER, INTENT(in) :: filetype

INTEGER ::  j
CHARACTER(len=512) :: path
LOGICAL :: exist

IF (dballe_name == ' ') THEN
  CALL getarg(0, dballe_name)
  ! dballe_name_env
ENDIF

IF (filetype < 1 .OR. filetype > nftype) THEN
  path = ""
  CALL l4f_log(L4F_ERROR, 'dballe file type '//TRIM(to_char(filetype))// &
   ' not valid')
  CALL raise_error()
  RETURN
ENDIF

! try with environment variable
CALL getenv(TRIM(dballe_name_env), path)
IF (path /= ' ') THEN

  path=TRIM(path)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL l4f_log(L4F_INFO, 'dballe file '//TRIM(path)//' found')
    RETURN
  ENDIF
ENDIF
! try with pathlist
DO j = 1, SIZE(pathlist,1)
  IF (pathlist(j,filetype) == ' ') EXIT
  path=TRIM(pathlist(j,filetype))//'/'//TRIM(dballe_name)//'/'//filename
  INQUIRE(file=path, exist=exist)
  IF (exist) THEN
    CALL l4f_log(L4F_INFO, 'dballe file '//TRIM(path)//' found')
    RETURN
  ENDIF
ENDDO
CALL l4f_log(L4F_ERROR, 'dballe file '//TRIM(filename)//' not found')
CALL raise_error()
path = ""

END FUNCTION get_dballe_filepath


FUNCTION open_dballe_file(filename, filetype) RESULT(unit)
CHARACTER(len=*), INTENT(in) :: filename
INTEGER, INTENT(in) :: filetype
INTEGER :: unit,i

CHARACTER(len=512) :: path

unit = -1
path=get_dballe_filepath(filename, filetype)
IF (path == '') RETURN

unit = getunit()
IF (unit == -1) RETURN

OPEN(unit, file=path, status='old', iostat = i)
IF (i == 0) THEN
  CALL l4f_log(L4F_INFO, 'dballe file '//TRIM(path)//' opened')
  RETURN
ENDIF

CALL l4f_log(L4F_ERROR, 'dballe file '//TRIM(filename)//' not found')
CALL raise_error()
unit = -1

END FUNCTION open_dballe_file


!> \brief Exporta un volume dati a un DSN DB-all.e
!!
!! Riscrive i dati nel DSN di DB-All.e con la possibilità di attivare
!! una serie di filtri.


!!! TODO manage attr_only
!!! attention template migrated in init
!SUBROUTINE vol7d_dballe_export(this, network, coordmin, coordmax,&
! timei, timef,level,timerange,var,attr,anavar,anaattr,attr_only,ana,dataonly)

SUBROUTINE vol7d_dballe_export_old(this, network, coordmin, coordmax,&
 timei, timef,level,timerange,var,attr,anavar,anaattr,ana,dataonly,anaonly,template)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto contenente il volume e altre info per l'accesso al DSN
character(len=network_name_len),INTENT(in),optional :: network !< network da exportare
!> coordinate minime e massime che definiscono il 
!! rettangolo di estrazione per l'esportazione
TYPE(geo_coord),INTENT(in),optional :: coordmin,coordmax 
!>estremi temporali dei dati da esportare
TYPE(datetime),INTENT(in),optional :: timei, timef
TYPE(vol7d_level),INTENT(in),optional :: level !< livello selezionato per l'esportazione
TYPE(vol7d_timerange),INTENT(in),optional :: timerange !< timerange selezionato per l'esportazione
!> variabili da exportare secondo la tabella B locale o alias relative a dati, attributi,
!! anagrafica e attributi dell'anagrafica
CHARACTER(len=*),INTENT(in),OPTIONAL :: var(:),attr(:),anavar(:),anaattr(:)
!!$!> permette di riscrivere su un DSN letto precedentemente, modificando solo gli attributi ai dati,
!!$!! ottimizzando enormente le prestazioni: gli attributi riscritti saranno quelli con this%data_id definito
!!$!! (solitamente ricopiato dall'oggetto letto)
!!$logical,intent(in),optional :: attr_only 
TYPE(vol7d_ana),INTENT(inout),optional :: ana  !< identificativo della stazione da exportare
logical, intent(in),optional :: dataonly          !< set to .true. to export data only
logical, intent(in),optional :: anaonly           !< set to .true. to export ana only
!> specificando category.subcategory.localcategory oppure un alias ("synop", "metar","temp","generic") forza l'exportazione ad uno specifico template BUFR/CREX"
!!  the special value "generic-frag is used to generate bufr on file where ana data is reported only once at beginning and data in other bufr after
character(len=*),intent(in),optional :: template 


type(dbadcv)       :: vars,starvars,anavars,anastarvars
type(dbafilter)    :: filter
type(dbacoord)     :: mydbacoordmin, mydbacoordmax
type(dbaana)       :: mydbaana
type(dbadatetime)  :: mydatetimemin, mydatetimemax
type(dbatimerange) :: mydbatimerange
type(dbalevel)     :: mydbalevel
type(dbanetwork)   :: mydbanetwork

integer :: i
LOGICAL :: lattr, lanaattr
integer :: nanaattr,nattr,nanavar,nvar


                                ! ------------- prepare filter options

!!
!!           translate export option for dballe2003 api
!!

if (present(var)) then
  nvar=count(c_e(var))
  if (nvar > 0) then
    allocate (vars%dcv(nvar))
    do i=1,size(var)
      if (c_e(var(i)))then
        allocate (vars%dcv(i)%dat,source=dbadatac(var(i))) !char is default
      end if
    end do
  end if
end if

if (present(anavar)) then
  nanavar=count(c_e(anavar))
  if (nanavar > 0) then
    allocate (anavars%dcv(nanavar))
    do i=1,size(anavar)
      if (c_e(anavar(i)))then
        allocate (anavars%dcv(i)%dat,source=dbadatac(anavar(i))) !char is default
      end if
    end do
  end if
end if

lattr = .false.
if (present(attr)) then
  nattr=count(c_e(attr))
  if (nattr > 0) then
    lattr = .true.
    allocate (starvars%dcv(nattr))
    do i=1,size(attr)
      if (c_e(attr(i)))then
          allocate (starvars%dcv(i)%dat,source=dbadatac(attr(i))) !char is default
      end if
    end do
  end if
end if

lanaattr = .false.
if (present(anaattr)) then
  nanaattr=count(c_e(anaattr))
  if (nanaattr > 0) then
    lanaattr = .true.
    allocate (anastarvars%dcv(nanaattr))
    do i=1,size(anaattr)
      if (c_e(anaattr(i)))then
        allocate (anastarvars%dcv(i)%dat,source=dbadatac(anaattr(i))) !char is default
      end if
    end do
  end if
end if


                                ! like a cast
mydbacoordmin=dbacoord()
if (present(coordmin)) mydbacoordmin%geo_coord=coordmin
mydbacoordmax=dbacoord()
if (present(coordmax)) mydbacoordmax%geo_coord=coordmax
mydbaana=dbaana()
if (present(ana)) mydbaana%vol7d_ana=ana
mydatetimemin=dbadatetime()
if (present(timei)) mydatetimemin%datetime=timei
mydatetimemax=dbadatetime()
if (present(timef)) mydatetimemax%datetime=timef
mydbatimerange=dbatimerange()
if (present(timerange)) mydbatimerange%vol7d_timerange=timerange
mydbalevel=dbalevel()
if (present(level)) mydbalevel%vol7d_level=level
mydbanetwork=dbanetwork()
if (present(network)) call init(mydbanetwork%vol7d_network,name=network)

!!
!!         here we have options ready for filter
!!
filter=dbafilter(coordmin=mydbacoordmin,coordmax=mydbacoordmax,ana=mydbaana, &
 datetimemin=mydatetimemin,datetimemax=mydatetimemax, &
 timerange=mydbatimerange,level=mydbalevel,network=mydbanetwork,&
 vars=vars,starvars=starvars,anavars=anavars,anastarvars=anastarvars,&
 dataonly=dataonly,anaonly=anaonly)

!!$  print *, "filter:"
!!$  call filter%display()

call export (this, filter,template)

end SUBROUTINE vol7d_dballe_export_old


subroutine vol7d_dballe_export (this, filter, template)

TYPE(vol7d_dballe),INTENT(inout) :: this !< oggetto contenente il volume e altre info per l'accesso al DSN
type(dbafilter),intent(in)    :: filter !< filter o use
!> specificando category.subcategory.localcategory oppure un alias ("synop", "metar","temp","generic") forza l'exportazione ad uno specifico template BUFR/CREX"
!!  the special value "generic-frag is used to generate bufr on file where ana data is reported only once at beginning and data in other bufr after
character(len=*),intent(in),optional :: template 

character(len=40) :: ltemplate 

type(dbametaanddatalist) :: metaanddatal
logical :: stat

metaanddatal=dbametaanddatalist()

call v7d2dba(this%vol7d,metaanddatal)
!call metaanddatal%display()

!clean memdb
if (this%file) call this%handle%remove_all()

! using filter here can limit memory use for memdb
call metaanddatal%extrude(session=this%handle,filter=filter)

if (this%file) then
  !!!!!  this if we have written in memdb and now we have to write the file

  !filter is already in extrude
  !this%handle%set(filter=filter)

  ! export to file
  !! TODO : revert template from init to export  !!!!!!!!!!!!!!!!!!!!!
  !!call this%handle%messages_write_next(template)

  ! note that you can use unsetall hera because the filter was used in extrude
  call filter%dbaset(this%handle)

  ltemplate=this%handle%template
  if (present(template))then
    ltemplate=template
  end if

  call this%handle%messages_write_next(ltemplate)
  !clean memdb
  call this%handle%remove_all()

end if

stat = metaanddatal%delete()

end subroutine vol7d_dballe_export


subroutine v7d2dba(v7d,metaanddatal)
TYPE(vol7d),INTENT(in)   :: v7d  !!!!!! dovrebbe essere intent(in)
type(dbametaanddatalist),intent(inout) :: metaanddatal

TYPE(vol7d_serialize_dballe) :: serialize

serialize = vol7d_serialize_dballe_new()
serialize%anaonly=.true.
call serialize%vol7d_serialize_setup(v7d)
call serialize%vol7d_serialize_export(metaanddatal)

serialize = vol7d_serialize_dballe_new()
serialize%dataonly=.true.
call serialize%vol7d_serialize_setup(v7d)
call serialize%vol7d_serialize_export(metaanddatal)

end subroutine v7d2dba


end MODULE vol7d_dballe_class

!>\example esempio_v7ddballe.f90
!!/brief Programma esempio semplice per l'uso di vol7d con DB-All.e
!!

!>\example esempio_v7ddballe_multi.f90
!!/brief Programma esempio per l'uso di vol7d con DB-All.e
!!
!!Vengono estratte più reti

!>\example esempio_v7ddballe_import_export.f90
!!\brief Esempio di utilizzo della classe vol7d_dballe_class
!!
!! Vengono estratti i dati e riscritti in un nuovo DSN
