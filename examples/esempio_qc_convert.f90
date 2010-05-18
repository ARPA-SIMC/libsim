program leggi

!legge i dati alla Pavan e li mette unformatted


USE vol7d_class
USE vol7d_dballe_class
USE vol7d_utilities
USE modqccli

IMPLICIT NONE

!INTEGER(kind=int_b) :: clima(area,altezza,mese,per) 

!real :: clima(9,12,10,3,3,1)

character(len=2) :: minuti(3)
character(len=1) :: area(3)
integer :: iarea,iper
character(len=80) ::filename
!REAL, PARAMETER :: rmiss = HUGE(1.0)


integer :: nana, ntime ,nlevel, ntimerange, ndativarr, nnetwork !,nanavari
integer :: iana, itime, ilevel, itimerange, idativarr, inetwork
TYPE(vol7d) :: v7d
TYPE(vol7d_dballe) :: v7d_dballe
TYPE(vol7d_ana) :: ana(cli_nsuperarea)
TYPE(datetime) :: time
TYPE(vol7d_level) :: level(cli_nlevel)
TYPE(vol7d_timerange) :: timerange(3)
TYPE(vol7d_network) :: network
TYPE(vol7d_var) ::  dativar,anavar
CHARACTER(len=vol7d_ana_lenident) :: ident
REAL(kind=fp_geo) :: lat,lon
!integer :: thres_quota(11) = (/-100,100,250,500,750,1000,1250,1500,1750,2000,2250/)
!integer :: livello(10) = (/50,175,375,625,875,1125,1375,1625,1875,2125/)
!integer :: livello1(10) = (/-100,100,250,500,750,1000,1250,1500,1750,2000/)
!integer :: livello2(10) = (/100,250,500,750,1000,1250,1500,1750,2000,2250/)
integer :: scadenze(3) = (/900,1800,3600/)
real :: dato
integer :: iunit=1
integer :: category,ier
character(len=512):: a_name
!questa chiamata prende dal launcher il nome univoco
call l4f_launcher(a_name,a_name_force="qc_convert")

!init di log4fortran
ier=l4f_init()

!imposta a_name
category=l4f_category_get(a_name//".main")

call l4f_category_log(category,L4F_INFO,"inizio")


CALL init(v7d)

nana=9*3
ntime=12
ntimerange=3
nlevel=cli_nlevel
nnetwork=1
ndativarr=1
!nanavari=1

call vol7d_alloc (v7d, &
 nana=nana, ntime=ntime, ntimerange=ntimerange, &
 nlevel=nlevel, nnetwork=nnetwork, &
 ndativarr=ndativarr)
!, nanavari=nanavari)

call vol7d_alloc_vol (v7d)


area(1)="e"
area(2)="c"
area(3)="o"
minuti(1)="15"
minuti(2)="30"
minuti(3)="60"


inetwork=1
idativarr=1

call init(v7d%network(inetwork), 'climat')
call init(v7d%dativar%r(idativarr), btable="B13011")
!call init(v7d%anavar%i(1), btable="B07001")
call cli_level_generate(v7d%level)


do iarea = 1, cli_nsuperarea
  do ilevel = 1,cli_nlevel

!    call init(v7d%level(ilevel), 102,cli_level1(ilevel)*1000,102,cli_level2(ilevel)*1000)

    do itimerange=1,3

      call init(v7d%timerange(itimerange), 1, 0,scadenze(itimerange))


      filename = "pctl_prec_"//minuti(itimerange)//"m_all_"//area(iarea)
      write (filename(20:) ,"(i1.1)")ilevel-1
      filename =filename(:20)//".txt"
      filename="/autofs/nethomes/pavan/cq_emr/percentili/"//filename
      
      print *, "apro file=",filename
      open (unit=iunit,file=filename)
      
      do itime = 1,12

        call init(v7d%time(itime), year=1001, month=itime, day=1, hour=0, minute=0)

        do iper = 1,9
          
          lat=0.0
          lon=0.0
          write(ident,'("BOX-",2i2.2)')iarea,iper*10
          iana=iarea+(iper-1)*cli_nsuperarea
          call init(v7d%ana(iana),lat=lat,lon=lon,ident=ident)

          read (iunit,"(10x,f7.0)") dato
          if (dato == -999.9) dato=rmiss

          v7d%voldatir(iana,itime,ilevel,itimerange,idativarr,inetwork) = dato          
          
        end do
      end do
      
      close(unit=iunit)
    end do
  end do
end do

open (unit=iunit,file="climaprec.v7d", form='UNFORMATTED', access='STREAM')

call export(v7d,unit=iunit)

close (unit=iunit)

filename="climaprec.bufr"
call init(v7d_dballe,file=.true.,wipe=.true.,write=.true.,filename=filename,categoryappend="esporto")
call copy(v7d,v7d_dballe%vol7d)
call export(v7d_dballe,template="generic")
call delete(v7d_dballe)

call delete(v7d)

!chiudo il logger
call l4f_category_delete(category)
ier=l4f_fini()

end program leggi
