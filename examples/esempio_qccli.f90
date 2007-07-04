! Programma esempio di controllo dei dati per soglie

program esempio_qccli

  use modqccli

  implicit none

  character(len=10) :: btable,starbtable
  character(len=19) :: database,user,password

  integer :: rep_cod
  integer :: leveltype=1,l1=0,l2=0,pindicator=0,p1=0,p2=0
  integer :: year ,month,day,hour,min,sec

  !tipo derivato per il Q.C.
  type(qcclitype) :: qccli

  ! namelist utilizzata per definire il DSN
  namelist  /odbc/database,user,password

  ! lettura della namelist utilizzata per definire il DSN
  open(10,file='odbc.nml',status='old')
  read(10,nml=odbc,iostat=io)
  
  if (io /= 0 )then
     print *,"Errore durante la lettura della namelist odbc"
     call exit (1)
  end if
  close(10)

  ! chiamiamo il "costruttore" per il Q.C.
  call init(qccli,ier)
  if (ier /= 0 ) then
     print * , "errore qccliinit ier=",ier
     call exit(1)
  end if

  call create(qccli,ier)
  if (ier /= 0 ) then
     print * , "errore qcclialloc ier=",ier
     call exit(1)
  end if
  
  print*,"elaboro la stazione ",ist

  qccli%valminr(1)=0.31
  qccli%valmaxr(1)=1.12
  !qccli%qc%ana(:)=ana

  print*,"Controllo climatico"
  call quaconcli(qccli,ier)

  if( ier /= 0) then
     print *, "errore qccli= ",ier
     call exit(1)
  end if

  do i=1,N
     ! cancello tutte le flag di qualità che voglio rielaborare
     ! dico quale è il contesto dei dati
     call idba_set (handle_write,"*context_id" , data_id(i))
     ! in questo contesto quale variabile
     call idba_set (handle_write,"*var_related",'B22070'     )
     ! in questa variabile quale attributo
     call idba_set (handle_write,"*var"        ,"*B33192"    )
        ! e cancello gli attributi
     call idba_scusa(handle_write)
     
     ! attribuisco le nuove confidenze ai dati
     call idba_set(handle_write, "*B33192",qccli%v7d%volattrdatib(1,i,1,1,1,1,2))
     call idba_critica(handle_write)
  end do
     
  ! il "distruttore" del Q.C.
  call delete(qccli,ier)


  ! chiudo sessioni e connessione

  stop

end program esempio_qccli

