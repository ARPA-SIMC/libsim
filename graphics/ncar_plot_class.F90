module ncar_plot_class

use phys_const
use optional_values
use log4fortran
use vol7d_class
use termolib

implicit none

character(len=40) :: colname(0:99)=""

!> plot object
type ncar_plot

  integer :: wkid  !< work station id
  integer :: category !< log4fortran
  integer :: LX
  integer :: LY
  integer :: UX
  integer :: UY

end type ncar_plot

!> \brief Costructor
!!
!! create a new instance of object
INTERFACE init
  MODULE PROCEDURE init_ncar_plot
END INTERFACE

!> \brief destructor
!!
!! delete object 
INTERFACE delete
  MODULE PROCEDURE delete_ncar_plot
END INTERFACE


!> \brief plot herlofson diagrams
!!
!! plot herlofson diagrams
INTERFACE plot_herlofson
  MODULE PROCEDURE ncar_plot_herlofson
END INTERFACE


!> \brief plot soundings
!!
!! plot soundings vertical plofiles
INTERFACE plot_vertical_plofiles
  MODULE PROCEDURE ncar_plot_vertical_profiles
END INTERFACE


real,parameter  :: dim_x=0.8,dim_y=0.8,offset_x=0.05,offset_y=0.05,ptopu=300.

! herlofson
real  :: ptop=100.,pdown=1050.,tmin=-40.,tmax=40.,pdiag1=850.,pdiag2=700.
real  :: xrotation=-45.
!!$
!!$
!!$! herlofson basso
!!$real  :: ptop=500.,pdown=1050.,tmin=-20.,tmax=40.,pdiag1=850.,pdiag2=700.
!!$real  :: xrotation=-45.


!!$!emagramma
!!$real  :: ptop=100.,pdown=1050.,tmin=-70.,tmax=35.,pdiag1=850.,pdiag2=700.
!!$real  :: xrotation=0.



!> \brief plot soundings title
!!
!! plot soundings vertical plofiles titles
INTERFACE plot_vp_title
  MODULE PROCEDURE ncar_plot_vp_title
END INTERFACE



private
public ncar_plot,init,delete,plot_herlofson,plot_vertical_plofiles,plot_vp_title


contains


subroutine init_ncar_plot(this,wkid,wstype,conid,file,PSTYPE,ORIENT,COLOR)

type(ncar_plot),intent(out) :: this
integer,intent(in),optional :: wkid  !< (def= 1)
integer,intent(in),optional :: wstype!< (def= 8 PS)
integer,intent(in),optional :: conid !< (def= 2)
character(len=*),optional :: file    !< Names for the PostScript output files can be assigned to override the default names; The default file names of PostScript output from NCAR GKS are of the form gmetaXX.YY, where "XX" is the workstation ID and "YY" is either "ps", "eps", or" epsi", as appropriate
character(len=*),optional :: PSTYPE  !< can be one of 'PS', 'EPS', or 'EPSI'
character(len=*),optional :: ORIENT  !< can be one of 'PORTRAIT' or 'LANDSCAPE'
character(len=*),optional :: COLOR   !< can be one of 'COLOR' or 'MONOCHROME'


integer :: lwstype,lconid,NGPSWK

if(present(wkid))then
  this%wkid=wkid
else
  this%wkid=1
end if

if(present(wstype))then
  lwstype=wstype
else
  if (present(PSTYPE) .and. present(ORIENT) .and. present(COLOR)) then
    lwstype=NGPSWK(PSTYPE,ORIENT,COLOR)
  else
    lwstype=8    
  end if
end if

if(present(conid))then
  lconid=conid
else
  lconid=0
end if

! apertura gks e workstation
call GOPKS(6,0)
if(present(file)) call NGSETC('ME',trim(file))
call GOPWK(this%wkid,lconid,lwstype)
call GACWK(this%wkid)
call NGSETI('Workstation',this%wkid)
call NGSETI('Full background',1)

this%lx=0
this%ly=0
this%ux=840
this%uy=594

!landscape mode
if (lwstype == 12 .or.&
  lwstype == 26.or.&
  lwstype == 27.or.&
  lwstype == 29.or.&
  lwstype == 30.or.&
  lwstype == 31) then

  call NGSETI('LX',this%lx)
  call NGSETI('LY',this%ly)
  call NGSETI('UX',this%ux)
  call NGSETI('UY',this%uy)

end if

call GSTXFP (-7,2)   !hershey roman
call set_color_table(this%wkid)

end subroutine init_ncar_plot



subroutine delete_ncar_plot(this)

type(ncar_plot),intent(in) :: this

! chiusura Gks e Workstation
call  GCLRWK (this%wkid,0)
call  GDAWK (this%wkid)
call  GCLWK (this%wkid)
call  GCLKS ()


end subroutine delete_ncar_plot

!> \brief  plot herlofson (skew-T)
!! Programma grafico per la rappresentazione del sondaggio termodinamico 
!! dell'atmosfera su di un nomogramma di Herlofson ( detto anche skew-T) 
!! 
subroutine ncar_plot_herlofson (this,logo,nomogramma)


type(ncar_plot),intent(in)  :: this
character(len=*),intent(in),optional :: logo,nomogramma

real,parameter :: pressioni(11)=(/1050.,1000.,850.,700.,&
 500.,400.,300.,250.,200.,150.,100./)
real,parameter :: isoigrom(10)=(/0.1,0.2,0.5,1.,2.,3.,5.,&
 8.,15.,20./)

integer                     :: i,l,ij,k

real,dimension(5)           :: area_fill_x,area_fill_y
real,dimension(2)           :: riga_x,riga_y
real,allocatable            :: x(:),y(:),p(:)
real,dimension(300)         :: xx,yy
real,dimension(6)           :: x_clip,y_clip
real,dimension(7)           :: x_frame,y_frame
real                        :: ycord,xcord,ratio,x_min,x_max,x_minh,y_maxh,xtest
real                        :: tabs,temp,pres,tsad,tt0,t1,t2
real                        :: delta_icao,psat,htop

character(len=100)          :: label,lnomogramma


! ---------------------------------------------------------------------------------------------
! settaggi generali per il grafico
! ---------------------------------------------------------------------------------------------

call optio(nomogramma,lnomogramma)

select case(lnomogramma)

case ("herlofson" , cmiss )
! herlofson
  ptop=100.
  pdown=1050.
  tmin=-40.
  tmax=40.
  pdiag1=850.
  pdiag2=700.
  xrotation=-45.

case("herlofson-down")
! herlofson basso
  ptop=500.
  pdown=1050.
  tmin=-20.
  tmax=40.
  pdiag1=850.
  pdiag2=700.
  xrotation=-45.

case("emagramma")
!emagramma
  ptop=100.
  pdown=1050.
  tmin=-70.
  tmax=40.
  pdiag1=850.
  pdiag2=700.
  xrotation=0.

case("emagramma-down")
!emagramma
  ptop=500.
  pdown=1050.
  tmin=-60.
  tmax=40.
  pdiag1=850.
  pdiag2=700.
  xrotation=0.

case default

  call l4f_category_log(this%category,L4F_ERROR,"errore tipo nomogramma: "//trim(nomogramma))
  call raise_fatal_error("errore tipo nomogramma: "//trim(nomogramma))
  
end select

ratio=real(this%uy-this%ly)/real(this%ux-this%lx)

x_min=x_coord (tmin,tmax,tmin,ptop,pdown,pdown,dim_x,dim_y,ratio,&
 &offset_x) 

x_minh=coord_prh (tmin,tmax,100.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)+0.01

x_max=x_coord (tmin,tmax,tmax,ptop,pdown,pdown,dim_x,dim_y,ratio,&
 &offset_x)

psat=max(ptop,ptopu)
y_maxh=y_coord (psat,dim_y,offset_y)


! ---------------------------------------------------------------------------------------------
!Isoterme colorate a passi di 10 C
! ---------------------------------------------------------------------------------------------

                                !fasce colorate
call set_color("green")

area_fill_y(1)=y_coord (pdown,dim_y,offset_y)
area_fill_y(2)=y_coord (pdown,dim_y,offset_y)
area_fill_y(3)=y_coord (ptop,dim_y,offset_y)
area_fill_y(4)=y_coord (ptop,dim_y,offset_y)
area_fill_y(5)=area_fill_y(1)

do l = nint(tmax)-160,nint(tmax),20     
  t1=real(l)
  t2=t1+10
  area_fill_x(1)=x_coord (tmin,tmax,t1,ptop,pdown,pdown,dim_x,dim_y,&
   &ratio,offset_x)
  area_fill_x(2)=x_coord (tmin,tmax,t2,ptop,pdown,pdown,dim_x,dim_y,&
   &ratio,offset_x)
  area_fill_x(3)=x_coord (tmin,tmax,t2,ptop,pdown,ptop,dim_x,dim_y,&
   &ratio,offset_x)
  area_fill_x(4)=x_coord (tmin,tmax,t1,ptop,pdown,ptop,dim_x,dim_y,&
   &ratio,offset_x)
  area_fill_x(5)=area_fill_x(1)

  call GSFAIS(3)
  call GSFASI(6)
  call gfa (5,area_fill_x,area_fill_y)
end do


                                ! pulisco lo sporco prodotto disegnando le isoterme colorate
                                ! riquadro a sinistra

call GSFAIS(1)
!call GSFASI(6)

area_fill_x(1)=0.
area_fill_x(2)=x_coord (tmin,tmax,tmin,ptop,pdown,pdown,dim_x,dim_y,ratio,&
 &offset_x)
area_fill_x(3)=area_fill_x(2)
area_fill_x(4)=area_fill_x(1)
area_fill_x(5)=area_fill_x(1)
area_fill_y(1)=0.
area_fill_y(2)=area_fill_y(1)
area_fill_y(3)=1.
area_fill_y(4)=1.
area_fill_y(5)=area_fill_y(1)
call set_color("background")
call gfa (5,area_fill_x,area_fill_y)


                                ! pulisco lo sporco prodotto disegnando le isoterme colorate
                                ! riquadro a destra
area_fill_x(1)=x_coord (tmin,tmax,tmax,ptop,pdown,pdown,dim_x,dim_y,&
 &ratio,offset_x)
area_fill_x(2)=1.
area_fill_x(3)=area_fill_x(2)
area_fill_x(4)=area_fill_x(1)
area_fill_x(5)=area_fill_x(1)
area_fill_y(1)=0.
area_fill_y(2)=area_fill_y(1)
area_fill_y(3)=1.
area_fill_y(4)=1.
area_fill_y(5)=area_fill_y(1)
call set_color("background")
call gfa (5,area_fill_x,area_fill_y)



                                ! isoterma 0

ij=size((/(real(k),k=int(pdown),int(ptop),-5)/))
allocate(x(ij),y(ij),p(ij))

p=(/(real(k),k=int(pdown),int(ptop),-5)/)

y=y_coord (p,dim_y,offset_y)
x=x_coord (tmin,tmax, 0. ,ptop,pdown,p,dim_x,&
   &dim_y,ratio,offset_x)
  
ij=count(x >= x_min  .and. x <= x_max )

call GSLWSC(1.5)
call set_color("red")
call gpl (ij,pack(x,(x >= x_min  .and. x <= x_max )),pack(y,(x >= x_min  .and. x <= x_max )))
  

                                ! tick + Label Isoterme

CALL GSTXAL(2,1)
call GSCHUP (0.,1.)
call gschh(0.010)
call set_color("foreground")
call gsln (1)
call GSLWSC(1.)

riga_y(2)=y_coord (pdown,dim_y,offset_y)
riga_y(1)=riga_y(2)-0.01

do l =nint(tmin),nint(tmax),10
  riga_x(1)=x_coord (tmin,tmax,real(l),ptop,pdown,pdown,dim_x,dim_y,&
   &ratio,offset_x)
  riga_x(2)=riga_x(1)
  write(label,'(i0)')l
  call gpl(2,riga_x,riga_y)
  call gtx(riga_x(1),riga_y(1),trim(label))
end do

call gtx (0.45,0.02,'Temperature [C]')


! ---------------------------------------------------------------------------------------------
! Isobare 
! ---------------------------------------------------------------------------------------------

CALL GSTXAL(3,3)
call GSCHUP (0.,1.)
call gschh(0.010)
call set_color("foreground")
call gsln (1)
call GSLWSC(1.)

riga_x(1)=x_min-0.01
riga_x(2)=x_max
do i=1,size(pressioni)
  if (pressioni(i) >= ptop )then
    riga_y(1)=y_coord (pressioni(i),dim_y,offset_y)
    riga_y(2)=riga_y(1)
    write(label,'(i0)')int(pressioni(i))
    call gtx(riga_x(1),riga_y(1),trim(label))
    call gpl(2,riga_x,riga_y)
  end if
end do

call GSCHUP (-1.,0.)
call gtx ( 0.01,0.5,'Pressure [hPa]')


! ---------------------------------------------------------------------------------------------
! Adiabatiche Secche
! ---------------------------------------------------------------------------------------------

CALL GSTXAL(2,5)
call GSCHUP (0.,1.)
call gschh(0.010)
call set_color("foreground")
call gsln (1)
call GSLWSC(1.)

do l=nint(tmax)+140,nint(tmin)-10,-10
  ij=0
  do k=nint(pdown),nint(ptop)+50,-5
    temp=real(l)
    pres=real(k)
    tabs=tda(temp+t0c,pres)-t0c
    ycord=y_coord (real(k),dim_y,offset_y)
    xcord=x_coord (tmin,tmax,tabs,ptop,pdown,real(k),dim_x,dim_y,&
     &ratio,offset_x)
    if (ycord < y_maxh )then
      xtest=x_minh
    else
      xtest=x_min
    end if
    if (xcord >= xtest .and. xcord <= x_max)then
      ij=ij+1  
      xx(ij)=xcord
      yy(ij)=ycord
    end if
  end do
  if (ij > 1 )then
!    if ( l >= tmax-10 .and. l <= tmax+100 )then
    if (  l <= tmax+100 )then
      call gpl (ij,xx,yy)
      write(label,'(i0)')l
      call gtx(xx(ij),yy(ij),trim(label))
    end if
  end if
end do



! ---------------------------------------------------------------------------------------------
! Adiabatiche Sature
! ---------------------------------------------------------------------------------------------

CALL GSTXAL(2,5)
call GSCHUP (0.,1.)
call gschh(0.010)
call set_color("violet")
call gsln (1)
call GSLWSC(1.)


psat=ptop+75.

do l=nint(tmin)+20,nint(tmax)-10,5
  ij=0
  do k=nint(pdown),nint(psat),-5
    tt0 = oe(real(l)+t0c, real(l)+t0c,  1000.)	
    tsad = tsa(tt0,real(k))-t0c		 !teta al livello a 1000-delta P
    ycord=y_coord (real(k),dim_y,offset_y)
    xcord=x_coord (tmin,tmax,tsad,ptop,pdown,real(k),dim_x,dim_y,ratio,&
     &offset_x)
    if (ycord < y_maxh )then
      xtest=x_minh
    else
      xtest=x_min
    end if
    if (xcord >= xtest .and. xcord <= x_max )then
      ij=ij+1  
      xx(ij)=xcord
      yy(ij)=ycord
    end if
  end do
  if (ij > 1 )then
!    if ( l >= tmin+35 .and. l <= tmax-10)then
    if (  l <= tmax-10)then
      call gpl (ij,xx,yy)
      write(label,'(i0)')l
      call gtx(xx(ij),yy(ij),trim(label))
    end if
  end if
end do


! ---------------------------------------------------------------------------------------------
! Isoigrometriche
! ---------------------------------------------------------------------------------------------

CALL GSTXAL(2,5)
call GSCHUP (0.,1.)
call gschh(0.010)
call set_color("brown")
call gsln (3)
call GSLWSC(1.)

psat=ptop+85.

call gschh(0.008)
do i = 1,size(isoigrom)
  ij=0
  do k = nint(pdown),nint(psat),-10
    tt0=tmr(isoigrom(i),real(k))-t0c
    ycord=y_coord (real(k),dim_y,offset_y)
    xcord=x_coord (tmin,tmax,tt0,ptop,pdown,real(k),dim_x,dim_y,&
     &ratio,offset_x)
    if (xcord >= x_min .and. xcord <= x_max-.15 )then
      ij=ij+1  
      xx(ij)=xcord
      yy(ij)=ycord
    end if
  end do
  if (ij > 0)then
    call gpl (ij,xx,yy)
    write(label,'(f4.1)')isoigrom(i)
    call gtx(xx(ij),yy(ij),trim(label))
  end if
end do

! ---------------------------------------------------------------------------------------------
! Umidit‡ relativa
! ---------------------------------------------------------------------------------------------

psat=max(ptop,ptopu)


!ripulisco l'area per l'umidita'

xx(1)=coord_prh (tmin,tmax,0.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)
yy(1)=y_coord (pdown,dim_y,offset_y)

xx(2)=xx(1)
yy(2)=y_coord (psat,dim_y,offset_y)

xx(3)=coord_prh (tmin,tmax,100.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)
yy(3)=yy(2)

xx(4)=xx(3)
yy(4)=yy(1)

call set_color("background")
call GSFAIS(1)

call gfa (4,xx(1:4),yy(1:4))


!disegno gli assi
call set_color("ever-green1")
call GSLWSC(1.5)
call gsln (1)

xx(1)=coord_prh (tmin,tmax,50.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)
xx(2)=xx(1)

call gpl (2,xx(1:2),yy(1:2))

xx(3)=coord_prh (tmin,tmax,100.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)
xx(4)=xx(3)

call gpl (2,xx(3:4),yy(3:4))

call gsln (3)

xx(1)=coord_prh (tmin,tmax,25.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)
xx(2)=coord_prh (tmin,tmax,25.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)
call gpl (2,xx,yy)

xx(1)=coord_prh (tmin,tmax,75.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)
xx(2)=coord_prh (tmin,tmax,75.,ptop,pdown,&
 &dim_x,dim_y,ratio,offset_x)
call gpl (2,xx,yy)


! ---------------------------------------------------------------------------------------------
! frame diagramma
! ---------------------------------------------------------------------------------------------


CALL GSTXAL(2,5)
call GSCHUP (0.,1.)
call gschh(0.010)
call gsln (1)
call GSLWSC(3.)

call herlo_frame (tmin,tmax,ptop,pdown,pdiag1,pdiag2,dim_x,dim_y,ratio,&
 &offset_x,offset_y,x_frame,x_clip,y_frame,y_clip)

                                !clipping forma strana diagramma
call set_color("background")
call gfa (6,x_clip,y_clip)

call set_color("foreground")
call gpl (7,x_frame,y_frame)


! ---------------------------------------------------------------------------------------------
! I.C.A.O. standard
! Atmosfera I.C.A.O. :
! gradiente verticale di 6.5 gradi ogni 1000 metri da 1013 hpa definita come
! altezza 0 fino a 11 km dove la temperatura e' di  -56.5, e isoterma 
! successivamente 
! ---------------------------------------------------------------------------------------------


CALL GSTXAL(1,3)
call GSCHUP (0.,1.)
call gschh(0.010)
call set_color("foreground")
call gsln (1)
call GSLWSC(2.)

! disegno asse verticale
yy(1)=y_coord (pdown,dim_y,offset_y)
yy(2)=y_coord (ptop,dim_y,offset_y)
xx(1)=x_coord (tmin,tmax,tmax,ptop,pdown,pdown,dim_x,dim_y,&
 &ratio,offset_x)
xx(2)=xx(1)
call gpl (2,xx,yy)


! altezze standard
yy(1)=y_coord (1013.25,dim_y,offset_y)
htop=y_coord (103.,dim_y,offset_y)
delta_icao=(htop-yy(1))/16.

htop=y_coord (ptop,dim_y,offset_y)

xx(2)=xx(2)+0.005

call GSLWSC(1.)

do l =0,16
  if ( yy(1) > htop ) exit
  yy(2)=yy(1)
  call gpl (2,xx,yy)
  write(label,'(i0)')l
  call gtx (xx(2),yy(1),trim(label))
  yy(1)=yy(1)+delta_icao
end do


xx(1)=x_coord (tmin,tmax,tmax-0.5,ptop,pdown,pdown,dim_x,dim_y,&
 &ratio,offset_x)
CALL GSTXAL(2,5)
call GSCHUP (-1.,0.)
call gtx ( xx(1),0.5,'HEIGHT [Km] - standard I.C.A.O. -')


!scrivo logo

CALL GSTXAL(3,5)
call GSCHUP (0.,1.)
call gschh(0.010)
call set_color("blue")

if (present(logo)) call gtx ( 0.9,0.01,trim(logo))

return

contains




!------------------------------------------------------------
!
!             S O T T O P R O G R A M M I 
!
!------------------------------------------------------------
!------------------------------------------------------------------------

subroutine herlo_frame (tmin,tmax,ptop,pdown,pdiag1,pdiag2,&
     &dim_x,dim_y,ratio,offset_x,offset_y,frame_x,&
     &x_clip,frame_y,y_clip)
 
! Subroutine per il plottaggio del frame del nomogramma di Herlofson
! si puo' parametrizzare dal main
! 
!----------------------------------------------------------------------


!  calcolo coordinate del frame del nomogramma :

real,intent(in)::tmin,tmax,ptop,pdown,pdiag1,pdiag2,dim_x,dim_y,ratio,&
 &offset_x,offset_y
real,dimension(7),intent(out)::frame_x,frame_y
real,dimension(6),intent(out)::x_clip,y_clip

frame_x(1)=x_coord (tmin,tmax,tmin,ptop,pdown,pdown,dim_x,dim_y,&
 &ratio,offset_x)
frame_y(1)=y_coord (pdown,dim_y,offset_y)
frame_x(2)=x_coord (tmin,tmax,tmax,ptop,pdown,pdown,dim_x,dim_y,&
 &ratio,offset_x)
frame_y(2)=y_coord (pdown,dim_y,offset_y)
frame_x(3)=frame_x(2)
frame_y(3)=y_coord (pdiag1,dim_y,offset_y)   
frame_x(4)=x_coord (tmin,tmax,tmax-14,ptop,pdown,pdown,dim_x,dim_y,ratio,&
       &offset_x)
frame_y(4)=y_coord (pdiag2,dim_y,offset_y)   
frame_x(5)=frame_x(4)
frame_y(5)=y_coord (ptop,dim_y,offset_y)   
frame_x(6)=x_coord (tmin,tmax,tmin,ptop,pdown,pdown,dim_x,dim_y,&
 &ratio,offset_x)
frame_y(6)=frame_y(5)
frame_y(7)=frame_y(1)
frame_x(7)=frame_x(1)

x_clip(1)=frame_x(3)
y_clip(1)=frame_y(3)

x_clip(2)=frame_x(4)
y_clip(2)=frame_y(4)

x_clip(3)=frame_x(5)
y_clip(3)=frame_y(5)

x_clip(4)=1.
y_clip(4)=1.

x_clip(5)=1.
y_clip(5)=0.

x_clip(6)=x_clip(1)
y_clip(6)=y_clip(1)
  
return

end subroutine herlo_frame

end subroutine ncar_plot_herlofson



!> \brief  plot herlofson (skew-T)
!! Programma grafico per la rappresentazione del sondaggio termodinamico 
!! dell'atmosfera su di un nomogramma di Herlofson ( detto anche skew-T) 
!! 
subroutine ncar_plot_vertical_profiles (this,v7d,ana,time,timerange,network,&
 tcolor,tdcolor,ucolor,wcolor)


type(ncar_plot),intent(in)  :: this
type(vol7d),intent(in)      :: v7d
integer,intent(in)          :: ana
integer,intent(in)          :: time
integer,intent(in)          :: timerange
integer,intent(in)          :: network
character (len=*),intent(in),optional :: tcolor,tdcolor,ucolor,wcolor

TYPE(vol7d_var) ::  var

integer                     :: l,levP,levT,ind

real,dimension(300)         :: xx,yy
real,allocatable            :: r_tt(:),r_pr(:),r_tr(:),r_ur(:),r_dd(:),r_ff(:),&
                               u(:),v(:),r_uq(:)
real                        :: ratio,xww,yww
real                        :: psat
character(len=1)            :: type
character(len=40)           :: color


ratio=real(this%uy-this%ly)/real(this%ux-this%lx)

! ---------------------------------------------------------------------------------------------
! CURVE DI STATO
! ---------------------------------------------------------------------------------------------


!!$ dimensioni di vol7d
!!$    * anagrafica
!!$    * tempo
!!$    * livello verticale
!!$    * intervallo temporale (timerange)
!!$    * variabile di dati
!!$    * network

!vettore pressioni

levP=count(v7d%level%level1 == 100 .and. v7d%level%level2 == imiss)
allocate (r_pr(levP))
r_pr=pack(v7d%level%l1,v7d%level%level1 == 100 .and. v7d%level%level2 == imiss)/100.

!vettore temperature

call init(var, btable="B12001")    ! temperatura
allocate(r_tt(levP))
r_tt=rmiss

type=cmiss
!type="i"
call vol7d_varvect_index(v7d%dativar,var , type=type,index_v=ind)

!if( ind /= 0 ) then
  select case (type)

  case("d")
    r_tt=pack(realdat(v7d%voldatid(ana,time,:,timerange,ind,network),v7d%dativar%d(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("r")
    r_tt=pack(realdat(v7d%voldatir(ana,time,:,timerange,ind,network),v7d%dativar%r(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("i")
    r_tt=pack(realdat(v7d%voldatii(ana,time,:,timerange,ind,network),v7d%dativar%i(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))
    
  case("b")
    r_tt=pack(realdat(v7d%voldatib(ana,time,:,timerange,ind,network),v7d%dativar%b(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("c")
    r_tt=pack(realdat(v7d%voldatic(ana,time,:,timerange,ind,network),v7d%dativar%c(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case default
    
    r_tt=rmiss

  end select
!end if

call init(var, btable="B12003")    ! temperatura rugiada

allocate(r_tr(levP))
r_tr=rmiss

type=cmiss
!type="i"
call vol7d_varvect_index(v7d%dativar,var , type=type,index_v=ind)

!if( ind /= 0 ) then
  select case (type)

  case("d")
    r_tr=pack(realdat(v7d%voldatid(ana,time,:,timerange,ind,network),v7d%dativar%d(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))
    
  case("r")
    r_tr=pack(realdat(v7d%voldatir(ana,time,:,timerange,ind,network),v7d%dativar%r(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))
  
  case("i")
    r_tr=pack(realdat(v7d%voldatii(ana,time,:,timerange,ind,network),v7d%dativar%i(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))
    
  case("b")
    r_tr=pack(realdat(v7d%voldatib(ana,time,:,timerange,ind,network),v7d%dativar%b(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("c")
    r_tr=pack(realdat(v7d%voldatic(ana,time,:,timerange,ind,network),v7d%dativar%c(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case default

    r_tr=rmiss

  end select
!end if

!umidit‡ relativa

allocate(r_ur(levP))

r_ur=fr(r_tt,r_tr)


!umidit‡ specifica

if (.not. any(c_e(r_ur))) then

  call init(var, btable="B13001")    ! Specific humidity,KG/KG
  allocate(r_uq(levP))
  r_uq=rmiss

  type=cmiss
  !type="i"
  call vol7d_varvect_index(v7d%dativar,var , type=type,index_v=ind)

  !if( ind /= 0 ) then
  select case (type)

  case("d")
    r_uq=pack(realdat(v7d%voldatid(ana,time,:,timerange,ind,network),v7d%dativar%d(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))
    
  case("r")
    r_uq=pack(realdat(v7d%voldatir(ana,time,:,timerange,ind,network),v7d%dativar%r(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))
  
  case("i")
    r_uq=pack(realdat(v7d%voldatii(ana,time,:,timerange,ind,network),v7d%dativar%i(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))
    
  case("b")
    r_uq=pack(realdat(v7d%voldatib(ana,time,:,timerange,ind,network),v7d%dativar%b(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("c")
    r_uq=pack(realdat(v7d%voldatic(ana,time,:,timerange,ind,network),v7d%dativar%c(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case default

    r_uq=rmiss

  end select
!end if

r_ur=QTORELHUM(r_uq,r_pr,r_tt)

! delete very dry points for truncation problem
where (r_ur < 10. )
  r_ur=rmiss
end where
r_tr=TRUG(r_ur,r_tt)


end if


!vento


call init(var, btable="B11003")    ! U-COMPONENT : M/S 
allocate(u(levP))
u=rmiss

type=cmiss
!type="i"
call vol7d_varvect_index(v7d%dativar,var , type=type,index_v=ind)

!if( ind /= 0 ) then
  select case (type)

  case("d")
    u=pack(realdat(v7d%voldatid(ana,time,:,timerange,ind,network),v7d%dativar%d(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("r")
    u=pack(realdat(v7d%voldatir(ana,time,:,timerange,ind,network),v7d%dativar%r(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("i")
    u=pack(realdat(v7d%voldatii(ana,time,:,timerange,ind,network),v7d%dativar%i(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("b")
    u=pack(realdat(v7d%voldatib(ana,time,:,timerange,ind,network),v7d%dativar%b(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("c")
    u=pack(realdat(v7d%voldatic(ana,time,:,timerange,ind,network),v7d%dativar%c(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case default

    u=rmiss

  end select
!end if


call init(var, btable="B11004")    ! V-COMPONENT : M/S
allocate(v(levP))
v=rmiss

type=cmiss
!type="i"
call vol7d_varvect_index(v7d%dativar,var , type=type,index_v=ind)

!if( ind /= 0 ) then
  select case (type)

  case("d")
    v=pack(realdat(v7d%voldatid(ana,time,:,timerange,ind,network),v7d%dativar%d(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("r")
    v=pack(realdat(v7d%voldatir(ana,time,:,timerange,ind,network),v7d%dativar%r(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("i")
    v=pack(realdat(v7d%voldatii(ana,time,:,timerange,ind,network),v7d%dativar%i(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("b")
    v=pack(realdat(v7d%voldatib(ana,time,:,timerange,ind,network),v7d%dativar%b(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("c")
    v=pack(realdat(v7d%voldatic(ana,time,:,timerange,ind,network),v7d%dativar%c(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case default

    v=rmiss

  end select

!end if


! test su tutti U e V mancanti
! cerco DD FF
if (.not. any(c_e(u)) .and. .not. any(c_e(v))) then

  call init(var, btable="B11001")    ! WIND DIRECTION : DEGREE TRUE 
  allocate(r_dd(levP))
  r_dd=rmiss

  type=cmiss
  !type="i"
  call vol7d_varvect_index(v7d%dativar,var , type=type,index_v=ind)
  
  !if( ind /= 0 ) then
  select case (type)

  case("d")
    r_dd=pack(realdat(v7d%voldatid(ana,time,:,timerange,ind,network),v7d%dativar%d(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("r")
    r_dd=pack(realdat(v7d%voldatir(ana,time,:,timerange,ind,network),v7d%dativar%r(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("i")
    r_dd=pack(realdat(v7d%voldatii(ana,time,:,timerange,ind,network),v7d%dativar%i(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("b")
    r_dd=pack(realdat(v7d%voldatib(ana,time,:,timerange,ind,network),v7d%dativar%b(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("c")
    r_dd=pack(realdat(v7d%voldatic(ana,time,:,timerange,ind,network),v7d%dativar%c(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case default

    r_dd=rmiss

  end select
  !end if

  call init(var, btable="B11002")    ! WIND SPEED : M/S 
  allocate(r_ff(levP))
  r_ff=rmiss

  type=cmiss
  !type="i"
  call vol7d_varvect_index(v7d%dativar,var , type=type,index_v=ind)

  !if( ind /= 0 ) then
  select case (type)

  case("d")
    r_ff=pack(realdat(v7d%voldatid(ana,time,:,timerange,ind,network),v7d%dativar%d(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("r")
    r_ff=pack(realdat(v7d%voldatir(ana,time,:,timerange,ind,network),v7d%dativar%r(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("i")
    r_ff=pack(realdat(v7d%voldatii(ana,time,:,timerange,ind,network),v7d%dativar%i(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("b")
    r_ff=pack(realdat(v7d%voldatib(ana,time,:,timerange,ind,network),v7d%dativar%b(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case("c")
    r_ff=pack(realdat(v7d%voldatic(ana,time,:,timerange,ind,network),v7d%dativar%c(ind)),&
     (v7d%level%level1 == 100.and.v7d%level%level2 == imiss))

  case default

    r_ff=rmiss

  end select

  !end if

  !conversione in u,v
  CALL UV(r_dd,r_ff,u,v)

end if


! disegno la temperatura
if (present (tcolor))then
  call set_color(trim(optio_c(tcolor,40)))
else
  call set_color("blue")
end if
call GSLWSC(3.)
call gsln (1)

levT=count(c_e(r_tt).and. r_pr >= ptop)
if(levT > 1) then
  
  call gpl(levT,&
   pack(x_coord(tmin,tmax,r_tt-t0c,ptop,pdown,r_pr,dim_x,dim_y,ratio,offset_x),&
   (c_e(r_tt).and. r_pr >= ptop)),&
   pack(y_coord(r_pr,dim_y,offset_y),(c_e(r_tt).and. r_pr >= ptop)))
  
end if


! disegno la TD

if (present (tdcolor))then
  call set_color(trim(optio_c(tdcolor,40)))
else
  call set_color("sky blue")
end if
call GSLWSC(2.5)
call gsln (1)

levT=count(c_e(r_tr).and. r_pr >= ptop)
if(levT > 1) then
  
  call gpl(levT,&
   pack(x_coord(tmin,tmax,r_tr-t0c,ptop,pdown,r_pr,dim_x,dim_y,ratio,offset_x),&
   (c_e(r_tr).and. r_pr >= ptop)),&
   pack(y_coord(r_pr,dim_y,offset_y),(c_e(r_tr).and. r_pr >= ptop)))
  
end if


! disegno  umidita relativa
if (present (ucolor))then
  call set_color(trim(optio_c(ucolor,40)))
else
  call set_color("ever-green1")
end if
call GSLWSC(2.)
call gsln (1)


psat=max(ptop,300.)

levT=count(c_e(r_ur).and. r_pr >= psat)
if(levT > 1) then
  
  call gpl(levT,&
   pack(coord_prh(tmin,tmax,r_ur,ptop,pdown,dim_x,dim_y,ratio,offset_x),&
   (c_e(r_ur).and. r_pr >= psat)),&
   pack(y_coord(r_pr,dim_y,offset_y),(c_e(r_ur).and. r_pr >= psat)))

end if


                                ! Vento forza e direzione 
call set_color("foreground")
call gsln (1)
call GSLWSC(2.5)

! asse verticale
yy(1)=y_coord (pdown,dim_y,offset_y)
yy(2)=y_coord (ptop,dim_y,offset_y)
xx(1)=x_coord (tmin,tmax,tmax,ptop,pdown,pdown,&
 &dim_x,dim_y,ratio,offset_x)+0.09
xx(2)=xx(1)
call gpl (2,xx,yy)

call GSCHUP (-1.,0.)
call gtx ( xx(1)+0.01,0.8,'wind')


! simbologia synottica

if (present (wcolor))then
  color=trim(optio_c(wcolor,40))
else
  color="foreground"
end if
call GSLWSC(1.5)

xww=x_coord (tmin,tmax,tmax,ptop,pdown,pdown,&
 dim_x,dim_y,ratio,offset_x)+0.09

!imposta lo standar synottico per la provenienza del vento
CALL WMSETI("col",color_index(color))
CALL WMSETI("wdf",1)

do l=1,levP
  if (c_e(u(l)).and. c_e(v(l)) .and. r_pr(l) >= ptop) then
    yww=y_coord (r_pr(l),dim_y,offset_y)  
                                !con conversione unit‡ di misura
    CALL WMBARB(xww,yww,u(l)*convff,v(l)*convff)
  end if
end do



return

end subroutine ncar_plot_vertical_profiles


subroutine ncar_plot_vp_title (this,v7d,ana,time,timerange,network,color)


type(ncar_plot),intent(in)  :: this
type(vol7d),intent(in)      :: v7d
integer,intent(in)          :: ana
integer,intent(in)          :: time
integer,intent(in)          :: timerange
integer,intent(in)          :: network
character(len=*),optional   :: color

real                        :: wmob,wmos,ht
character(len=100)          :: label,title,nome,tmpc,lcolor
character(len=1)            :: type

integer                     :: ind
TYPE(vol7d_var) ::  var
doubleprecision :: lon,lat


if (present(color))then
  lcolor=color
else
  lcolor='blue'
end if

! informazioni di anagrafica etc

!!$ dimensioni di vol7d
!!$    * anagrafica
!!$    * livello verticale
!!$    * variabile di dati
!!$    * network

call init(var, btable="B01001")    ! WMO BLOCK NUMBER 
type=cmiss
call vol7d_varvect_index(v7d%anavar,var , type=type,index_v=ind)
!if( ind /= 0 ) then
  select case (type)
  case("d")
    wmob=realdat(v7d%volanad(ana,ind,network),v7d%anavar%d(ind))
  case("r")
    wmob=realdat(v7d%volanar(ana,ind,network),v7d%anavar%r(ind))
  case("i")
    wmob=realdat(v7d%volanai(ana,ind,network),v7d%anavar%i(ind))
  case("b")
    wmob=realdat(v7d%volanab(ana,ind,network),v7d%anavar%b(ind))
  case("c")
    wmob=realdat(v7d%volanac(ana,ind,network),v7d%anavar%c(ind))
  case default
    wmob=rmiss
  end select
!end if

call init(var, btable="B01002")    ! WMO STATION NUMBER
type=cmiss
call vol7d_varvect_index(v7d%anavar,var , type=type,index_v=ind)
!if( ind /= 0 ) then
  select case (type)
  case("d")
    wmos=realdat(v7d%volanad(ana,ind,network),v7d%anavar%d(ind))
  case("r")
    wmos=realdat(v7d%volanar(ana,ind,network),v7d%anavar%r(ind))
  case("i")
    wmos=realdat(v7d%volanai(ana,ind,network),v7d%anavar%i(ind))
  case("b")
    wmos=realdat(v7d%volanab(ana,ind,network),v7d%anavar%b(ind))
  case("c")
    wmos=realdat(v7d%volanac(ana,ind,network),v7d%anavar%c(ind))
  case default
    wmos=rmiss
  end select
!end if

title=""
if (c_e(wmob)) title=trim(to_char(int(wmob)))
if (c_e(wmos)) then
  write (tmpc,'(i3.3)')int(wmos)
else
  tmpc="///"
end if

title=trim(title)//trim(tmpc)
if (title /= "") title="Station: "//title

call init(var, btable="B07001")    ! HEIGHT OF STATION
type=cmiss
call vol7d_varvect_index(v7d%anavar,var , type=type,index_v=ind)
!if( ind /= 0 ) then
  select case (type)
  case("d")
    ht=realdat(v7d%volanad(ana,ind,network),v7d%anavar%d(ind))
  case("r")
    ht=realdat(v7d%volanar(ana,ind,network),v7d%anavar%r(ind))
  case("i")
    ht=realdat(v7d%volanai(ana,ind,network),v7d%anavar%i(ind))
  case("b")
    ht=realdat(v7d%volanab(ana,ind,network),v7d%anavar%b(ind))
  case("c")
    ht=realdat(v7d%volanac(ana,ind,network),v7d%anavar%c(ind))
  case default
    ht=rmiss
  end select
!end if

call getval(v7d%ana(ana)%coord,lon=lon,lat=lat)
!title=trim(title)//" lat:"//trim(to_char(lat)(1:5))//" lon:"//trim(to_char(lon)(1:5))
write(title,'(a,2(a,f7.2))') trim(title)," lat:",lat," lon:",lon

if (c_e(ht)) title=trim(title)//"  height:"//trim(to_char(int(ht)))//" m. "

nome=""

call init(var, btable="B01063")    ! ICAO LOCATION INDICATOR
type="c"
call vol7d_varvect_index(v7d%anavar,var , type=type,index_v=ind)
if( ind /= 0 ) nome=v7d%volanac(ana,ind,network)

call init(var, btable="B01019")    ! LONG STATION OR SITE NAME
type="c"
call vol7d_varvect_index(v7d%anavar,var , type=type,index_v=ind)
if( ind /= 0 ) nome=v7d%volanac(ana,ind,network)


!time
label=""
call getval(v7d%time(time),isodate=label(:16))

label=trim(label)//" "//trim(to_char(v7d%timerange(timerange)))//&
 " "//trim(to_char(v7d%network(network)))

!scrivo header

CALL GSTXAL(1,5)
call GSCHUP (0.,1.)
call gschh(0.010)
call set_color(trim(lcolor))


call gtx (0.1,0.98,trim(nome))
call gtx (0.1,0.96,trim(title))
call gtx (0.1,0.94,trim(label))

return

end subroutine ncar_plot_vp_title


subroutine set_color(color)

character(len=*),intent(in) :: color

integer :: coli

COLI=color_index(color)

! set the color for all the world
CALL GSPLCI  (COLI)
CALL GSPMCI  (COLI)
CALL GSTXCI  (COLI) 
CALL GSFACI  (COLI) 

end subroutine set_color



integer function color_index(color)

character(len=*),intent(in) :: color

do color_index=0,size(colname)-1
  if (colname(color_index) == color )exit
end do

if (color_index == size(colname)) color_index=1

end function color_index


elemental real function y_coord (p,dim_y,offset_y)

!
! Funzione ancillare nomogramma di Herlofson
!
! Computa il valore dell'ordinata sul nomogramma di Herlofson 
!   - pdown > ptop -
!
!----------------------------------------------------------------------------

real,intent(in)  :: p,dim_y,offset_y

real::porig,pmax,pres,yy
  
!!$  work=ptop_herlo+delta_herlo*(p-ptop)/(pdown-ptop)

if (c_e(p))then
  porig=(log(pdown/pdown))
  pmax=(log(pdown/ptop))
  pres=(log(pdown/P))
  
  yy=((pres-porig)/(pmax-porig))*dim_y
  
  y_coord=yy+offset_y

  !!$  porig=32.182-44.061*ALOG10(pdown) 

else

  y_coord=rmiss

end if

return 

end function y_coord


!------------------------------------------------------------------------------

elemental real function x_coord (tmin,tmax,t,ptop,pdown,p,dim_x,dim_y,ratio,&
     &offset_x)

real,intent(in) :: tmin,tmax,t,ptop,pdown,p,dim_x,dim_y,ratio,offset_x

real :: xx,porig,pmax,pres,yy
real :: incli

incli=xrotation*(pi/180.) ! gradi in radianti

if(c_e(t).and.c_e(p))then
  porig=(log(pdown/pdown))
  pmax=(log(pdown/ptop))
  pres=(log(pdown/p))
    
  yy=((pres-porig)/(pmax-porig))*dim_y

  xx=(t-tmin)/(tmax-tmin)*dim_x/ratio

 ! devo riscalare per adattarmi alle espansioni dovute alla rotazione dell'asse
  xx=xx*cos(45.*(pi/180.)+incli)

  x_coord = xx * cos(incli) - yy * sin(incli) !rotazione dell'angolo incli

  x_coord=x_coord+offset_x
  
else

  x_coord=rmiss

end if
return 

end function x_coord



elemental real function x_coord_old (tmin,tmax,t,ptop,pdown,p,dim_x,dim_y,ratio,&
     &offset_x,spazio)

real,intent(in) :: tmin,tmax,t,ptop,pdown,p,dim_x,dim_y,ratio,offset_x,spazio

real::xx,porig,pmax,pres,yy,y
real::torig,t_max,temp
real,parameter :: incli=45.*(pi/180.) ! 45 gradi in radianti

if(c_e(t))then

  torig=sin(incli)*tmin
  t_max=sin(incli)*tmax
  temp=sin(incli)*t

  xx=(temp-torig)/(t_max-torig)
  xx=xx*dim_x                   !coordinata grafico a pdown

  porig=(log(pdown/pdown))/cpd
  pmax=(log(pdown/ptop))/cpd
  pres=(log(pdown/p))/cpd
    
  yy=((pres-porig)/(pmax-porig))
  y=yy*dim_y
  x_coord_old=(xx+y)*ratio+offset_x+spazio
  
else

  x_coord_old=rmiss

end if
return 

end function x_coord_old

!---------------------------------------------------------------

elemental real function coord_prh (tmin,tmax,rh,ptop,pdown,dim_x,dim_y,&
     &ratio,offset_x)

! funzione ancillare nomogramam di herlofson
!
! Calcola le coordinate grafico per la curva dell'umidit√† relativa
!
!-----------------------------------------------------------------------

real, intent(in) ::tmin,tmax,ptop,pdown,dim_x,dim_y,&
 ratio,offset_x,rh

real::fine,orig,delta_x,xx  !variabili locali


if (c_e(rh))then

  orig= x_coord (tmin,tmax,tmin,ptop,pdown,pdown,dim_x,dim_y,ratio,&
   &offset_x)
  fine= x_coord (tmin,tmax,tmin+15,ptop,pdown,pdown,dim_x,dim_y,&
   &ratio,offset_x)
  delta_x=fine-orig
    
  xx=(rh/100.)*delta_x
    
  coord_prh=xx*ratio+offset_x

else

  coord_prh=rmiss

end if


return
end function coord_prh


subroutine set_color_table(iwkid)

integer,intent(in) :: iwkid

colname(0)='background'
colname(1)='foreground'
colname(2)='tan'
colname(3)='brown'
colname(4)='orange'
colname(5)='red'
colname(6)='yellow'
colname(7)='green'
colname(8)='forest Green'
colname(9)='cyan'
colname(10)='sky blue'
colname(11)='blue'
colname(12)='blue magenta'
colname(13)='magenta'
colname(14)='white'
colname(15)='light gray'
colname(16)='dark gray'
colname(17)='black'
colname(18)='violet'
colname(19)='ever-green1'
colname(20)='ever-green2'
colname(21)='sconosciuto'


CALL GSCR(IWKID,0,1.,1.,1.)
CALL GSCR(IWKID,1,0.,0.,0.)
CALL GSCR(IWKID,2,  0.86, 0.58, 0.44)
CALL GSCR(IWKID,3,  0.65, 0.16, 0.16)
CALL GSCR(IWKID,4,  1.00, 0.50, 0.00)
CALL GSCR(IWKID,5,  1.00, 0.00, 0.00)
CALL GSCR(IWKID,6,  1.00, 1.00, 0.00)
CALL GSCR(IWKID,7,  0.00, 1.00, 0.00)
CALL GSCR(IWKID,8,  0.14, 0.56, 0.14)
CALL GSCR(IWKID,9,  0.00, 1.00, 1.00)
CALL GSCR(IWKID,10,  0.20, 0.56, 0.80)
CALL GSCR(IWKID,11,  0.00, 0.00, 1.00)
CALL GSCR(IWKID,12,  0.50, 0.00, 1.00)
CALL GSCR(IWKID,13,  1.00, 0.00, 1.00)
CALL GSCR(IWKID,14,  1.00, 1.00, 1.00)
CALL GSCR(IWKID,15,  0.66, 0.66, 0.66)
CALL GSCR(IWKID,16,  0.40, 0.40, 0.40)
CALL GSCR(IWKID,17,  0.00, 0.00, 0.00)
CALL GSCR(IWKID,18,  1.0,  0.0,  1.0 )
CALL GSCR(IWKID,19    ,0., 0.44, 0.29)
CALL GSCR(IWKID,20,    0., 0.44, 1.)
CALL GSCR(IWKID,21,    0.59,0.39,0.)

!!$!supersintesi
!!$CALL GSCR(IWK, 0, 0.0, 0.0, 0.0)
!!$CALL GSCR(IWK, 1, 1.0, 1.0, 1.0)
!!$CALL GSCR(IWK, 2, 1.0, 0.0, 0.0)
!!$CALL GSCR(IWK, 3, 0.0, 1.0, 0.0)
!!$CALL GSCR(IWK, 4, 1.0, 1.0, 0.0)
!!$CALL GSCR(IWK, 5, 0.0, 1.0, 1.0)
!!$CALL GSCR(IWK, 5, 1.0, 0.0, 1.0)


!! daimplementare

!!$aliceblue                         0.9412    0.9725    1.0000
!!$antiquewhite                      0.9804    0.9216    0.8431
!!$antiquewhite1                     1.0000    0.9373    0.8588
!!$antiquewhite2                     0.9333    0.8745    0.8000
!!$antiquewhite3                     0.8039    0.7529    0.6902
!!$antiquewhite4                     0.5451    0.5137    0.4706
!!$aquamarine                        0.4980    1.0000    0.8314
!!$aquamarine1                       0.4980    1.0000    0.8314
!!$aquamarine2                       0.4627    0.9333    0.7765
!!$aquamarine3                       0.4000    0.8039    0.6667
!!$aquamarine4                       0.2706    0.5451    0.4549
!!$azure                             0.9412    1.0000    1.0000
!!$azure1                            0.9412    1.0000    1.0000
!!$azure2                            0.8784    0.9333    0.9333
!!$azure3                            0.7569    0.8039    0.8039
!!$azure4                            0.5137    0.5451    0.5451
!!$beige                             0.9608    0.9608    0.8627
!!$bisque                            1.0000    0.8941    0.7686
!!$bisque1                           1.0000    0.8941    0.7686
!!$bisque2                           0.9333    0.8353    0.7176
!!$bisque3                           0.8039    0.7176    0.6196
!!$bisque4                           0.5451    0.4902    0.4196
!!$black                             0.0000    0.0000    0.0000
!!$blanchedalmond                    1.0000    0.9216    0.8039
!!$blue                              0.0000    0.0000    1.0000
!!$blue1                             0.0000    0.0000    1.0000
!!$blue2                             0.0000    0.0000    0.9333
!!$blue3                             0.0000    0.0000    0.8039
!!$blue4                             0.0000    0.0000    0.5451
!!$blueviolet                        0.5412    0.1686    0.8863
!!$brown                             0.6471    0.1647    0.1647
!!$brown1                            1.0000    0.2510    0.2510
!!$brown2                            0.9333    0.2314    0.2314
!!$brown3                            0.8039    0.2000    0.2000
!!$brown4                            0.5451    0.1373    0.1373
!!$burlywood                         0.8706    0.7216    0.5294
!!$burlywood1                        1.0000    0.8275    0.6078
!!$burlywood2                        0.9333    0.7725    0.5686
!!$burlywood3                        0.8039    0.6667    0.4902
!!$burlywood4                        0.5451    0.4510    0.3333
!!$cadetblue                         0.3725    0.6196    0.6275
!!$cadetblue1                        0.5961    0.9608    1.0000
!!$cadetblue2                        0.5569    0.8980    0.9333
!!$cadetblue3                        0.4784    0.7725    0.8039
!!$cadetblue4                        0.3255    0.5255    0.5451
!!$chartreuse                        0.4980    1.0000    0.0000
!!$chartreuse1                       0.4980    1.0000    0.0000
!!$chartreuse2                       0.4627    0.9333    0.0000
!!$chartreuse3                       0.4000    0.8039    0.0000
!!$chartreuse4                       0.2706    0.5451    0.0000
!!$chocolate                         0.8235    0.4118    0.1176
!!$chocolate1                        1.0000    0.4980    0.1412
!!$chocolate2                        0.9333    0.4627    0.1294
!!$chocolate3                        0.8039    0.4000    0.1137
!!$chocolate4                        0.5451    0.2706    0.0745
!!$coral                             1.0000    0.4980    0.3137
!!$coral1                            1.0000    0.4471    0.3373
!!$coral2                            0.9333    0.4157    0.3137
!!$coral3                            0.8039    0.3569    0.2706
!!$coral4                            0.5451    0.2431    0.1843
!!$cornflowerblue                    0.3922    0.5843    0.9294
!!$cornsilk                          1.0000    0.9725    0.8627
!!$cornsilk1                         1.0000    0.9725    0.8627
!!$cornsilk2                         0.9333    0.9098    0.8039
!!$cornsilk3                         0.8039    0.7843    0.6941
!!$cornsilk4                         0.5451    0.5333    0.4706
!!$cyan                              0.0000    1.0000    1.0000
!!$cyan1                             0.0000    1.0000    1.0000
!!$cyan2                             0.0000    0.9333    0.9333
!!$cyan3                             0.0000    0.8039    0.8039
!!$cyan4                             0.0000    0.5451    0.5451
!!$darkblue                          0.0000    0.0000    0.5451
!!$darkcyan                          0.0000    0.5451    0.5451
!!$darkgoldenrod                     0.7216    0.5255    0.0431
!!$darkgoldenrod1                    1.0000    0.7255    0.0588
!!$darkgoldenrod2                    0.9333    0.6784    0.0549
!!$darkgoldenrod3                    0.8039    0.5843    0.0471
!!$darkgoldenrod4                    0.5451    0.3961    0.0314
!!$darkgray                          0.6627    0.6627    0.6627
!!$darkgreen                         0.0000    0.3922    0.0000
!!$darkgrey                          0.6627    0.6627    0.6627
!!$darkkhaki                         0.7412    0.7176    0.4196
!!$darkmagenta                       0.5451    0.0000    0.5451
!!$darkolivegreen                    0.3333    0.4196    0.1843
!!$darkolivegreen1                   0.7922    1.0000    0.4392
!!$darkolivegreen2                   0.7373    0.9333    0.4078
!!$darkolivegreen3                   0.6353    0.8039    0.3529
!!$darkolivegreen4                   0.4314    0.5451    0.2392
!!$darkorange                        1.0000    0.5490    0.0000
!!$darkorange1                       1.0000    0.4980    0.0000
!!$darkorange2                       0.9333    0.4627    0.0000
!!$darkorange3                       0.8039    0.4000    0.0000
!!$darkorange4                       0.5451    0.2706    0.0000
!!$darkorchid                        0.6000    0.1961    0.8000
!!$darkorchid1                       0.7490    0.2431    1.0000
!!$darkorchid2                       0.6980    0.2275    0.9333
!!$darkorchid3                       0.6039    0.1961    0.8039
!!$darkorchid4                       0.4078    0.1333    0.5451
!!$darkred                           0.5451    0.0000    0.0000
!!$darksalmon                        0.9137    0.5882    0.4784
!!$darksea green                     0.5608    0.7373    0.5608
!!$darkseagreen                      0.5608    0.7373    0.5608
!!$darkseagreen1                     0.7569    1.0000    0.7569
!!$darkseagreen2                     0.7059    0.9333    0.7059
!!$darkseagreen3                     0.6078    0.8039    0.6078
!!$darkseagreen4                     0.4118    0.5451    0.4118
!!$darkslate blue                    0.2824    0.2392    0.5451
!!$darkslate gray                    0.1843    0.3098    0.3098
!!$darkslate grey                    0.1843    0.3098    0.3098
!!$darkslateblue                     0.2824    0.2392    0.5451
!!$darkslategray                     0.1843    0.3098    0.3098
!!$darkslategray1                    0.5922    1.0000    1.0000
!!$darkslategray2                    0.5529    0.9333    0.9333
!!$darkslategray3                    0.4745    0.8039    0.8039
!!$darkslategray4                    0.3216    0.5451    0.5451
!!$darkslategrey                     0.1843    0.3098    0.3098
!!$darkturquoise                     0.0000    0.8078    0.8196
!!$darkviolet                        0.5804    0.0000    0.8275
!!$deeppink                          1.0000    0.0784    0.5765
!!$deeppink1                         1.0000    0.0784    0.5765
!!$deeppink2                         0.9333    0.0706    0.5373
!!$deeppink3                         0.8039    0.0627    0.4627
!!$deeppink4                         0.5451    0.0392    0.3137
!!$deepskyblue                       0.0000    0.7490    1.0000
!!$deepskyblue1                      0.0000    0.7490    1.0000
!!$deepskyblue2                      0.0000    0.6980    0.9333
!!$deepskyblue3                      0.0000    0.6039    0.8039
!!$deepskyblue4                      0.0000    0.4078    0.5451
!!$dimgray                           0.4118    0.4118    0.4118
!!$dimgrey                           0.4118    0.4118    0.4118
!!$dodger blue                       0.1176    0.5647    1.0000
!!$dodgerblue                        0.1176    0.5647    1.0000
!!$dodgerblue1                       0.1176    0.5647    1.0000
!!$dodgerblue2                       0.1098    0.5255    0.9333
!!$dodgerblue3                       0.0941    0.4549    0.8039
!!$dodgerblue4                       0.0627    0.3059    0.5451
!!$firebrick                         0.6980    0.1333    0.1333
!!$firebrick1                        1.0000    0.1882    0.1882
!!$firebrick2                        0.9333    0.1725    0.1725
!!$firebrick3                        0.8039    0.1490    0.1490
!!$firebrick4                        0.5451    0.1020    0.1020
!!$floralwhite                       1.0000    0.9804    0.9412
!!$forestgreen                       0.1333    0.5451    0.1333
!!$gainsboro                         0.8627    0.8627    0.8627
!!$ghostwhite                        0.9725    0.9725    1.0000
!!$gold                              1.0000    0.8431    0.0000
!!$gold1                             1.0000    0.8431    0.0000
!!$gold2                             0.9333    0.7882    0.0000
!!$gold3                             0.8039    0.6784    0.0000
!!$gold4                             0.5451    0.4588    0.0000
!!$goldenrod                         0.8549    0.6471    0.1255
!!$goldenrod1                        1.0000    0.7569    0.1451
!!$goldenrod2                        0.9333    0.7059    0.1333
!!$goldenrod3                        0.8039    0.6078    0.1137
!!$goldenrod4                        0.5451    0.4118    0.0784
!!$gray                              0.7451    0.7451    0.7451
!!$gray0                             0.0000    0.0000    0.0000
!!$gray1                             0.0118    0.0118    0.0118
!!$gray10                            0.1020    0.1020    0.1020
!!$gray100                           1.0000    1.0000    1.0000
!!$gray11                            0.1098    0.1098    0.1098
!!$gray12                            0.1216    0.1216    0.1216
!!$gray13                            0.1294    0.1294    0.1294
!!$gray14                            0.1412    0.1412    0.1412
!!$gray15                            0.1490    0.1490    0.1490
!!$gray16                            0.1608    0.1608    0.1608
!!$gray17                            0.1686    0.1686    0.1686
!!$gray18                            0.1804    0.1804    0.1804
!!$gray19                            0.1882    0.1882    0.1882
!!$gray2                             0.0196    0.0196    0.0196
!!$gray20                            0.2000    0.2000    0.2000
!!$gray21                            0.2118    0.2118    0.2118
!!$gray22                            0.2196    0.2196    0.2196
!!$gray23                            0.2314    0.2314    0.2314
!!$gray24                            0.2392    0.2392    0.2392
!!$gray25                            0.2510    0.2510    0.2510
!!$gray26                            0.2588    0.2588    0.2588
!!$gray27                            0.2706    0.2706    0.2706
!!$gray28                            0.2784    0.2784    0.2784
!!$gray29                            0.2902    0.2902    0.2902
!!$gray3                             0.0314    0.0314    0.0314
!!$gray30                            0.3020    0.3020    0.3020
!!$gray31                            0.3098    0.3098    0.3098
!!$gray32                            0.3216    0.3216    0.3216
!!$gray33                            0.3294    0.3294    0.3294
!!$gray34                            0.3412    0.3412    0.3412
!!$gray35                            0.3490    0.3490    0.3490
!!$gray36                            0.3608    0.3608    0.3608
!!$gray37                            0.3686    0.3686    0.3686
!!$gray38                            0.3804    0.3804    0.3804
!!$gray39                            0.3882    0.3882    0.3882
!!$gray4                             0.0392    0.0392    0.0392
!!$gray40                            0.4000    0.4000    0.4000
!!$gray41                            0.4118    0.4118    0.4118
!!$gray42                            0.4196    0.4196    0.4196
!!$gray43                            0.4314    0.4314    0.4314
!!$gray44                            0.4392    0.4392    0.4392
!!$gray45                            0.4510    0.4510    0.4510
!!$gray46                            0.4588    0.4588    0.4588
!!$gray47                            0.4706    0.4706    0.4706
!!$gray48                            0.4784    0.4784    0.4784
!!$gray49                            0.4902    0.4902    0.4902
!!$gray5                             0.0510    0.0510    0.0510
!!$gray50                            0.4980    0.4980    0.4980
!!$gray51                            0.5098    0.5098    0.5098
!!$gray52                            0.5216    0.5216    0.5216
!!$gray53                            0.5294    0.5294    0.5294
!!$gray54                            0.5412    0.5412    0.5412
!!$gray55                            0.5490    0.5490    0.5490
!!$gray56                            0.5608    0.5608    0.5608
!!$gray57                            0.5686    0.5686    0.5686
!!$gray58                            0.5804    0.5804    0.5804
!!$gray59                            0.5882    0.5882    0.5882
!!$gray6                             0.0588    0.0588    0.0588
!!$gray60                            0.6000    0.6000    0.6000
!!$gray61                            0.6118    0.6118    0.6118
!!$gray62                            0.6196    0.6196    0.6196
!!$gray63                            0.6314    0.6314    0.6314
!!$gray64                            0.6392    0.6392    0.6392
!!$gray65                            0.6510    0.6510    0.6510
!!$gray66                            0.6588    0.6588    0.6588
!!$gray67                            0.6706    0.6706    0.6706
!!$gray68                            0.6784    0.6784    0.6784
!!$gray69                            0.6902    0.6902    0.6902
!!$gray7                             0.0706    0.0706    0.0706
!!$gray70                            0.7020    0.7020    0.7020
!!$gray71                            0.7098    0.7098    0.7098
!!$gray72                            0.7216    0.7216    0.7216
!!$gray73                            0.7294    0.7294    0.7294
!!$gray74                            0.7412    0.7412    0.7412
!!$gray75                            0.7490    0.7490    0.7490
!!$gray76                            0.7608    0.7608    0.7608
!!$gray77                            0.7686    0.7686    0.7686
!!$gray78                            0.7804    0.7804    0.7804
!!$gray79                            0.7882    0.7882    0.7882
!!$gray8                             0.0784    0.0784    0.0784
!!$gray80                            0.8000    0.8000    0.8000
!!$gray81                            0.8118    0.8118    0.8118
!!$gray82                            0.8196    0.8196    0.8196
!!$gray83                            0.8314    0.8314    0.8314
!!$gray84                            0.8392    0.8392    0.8392
!!$gray85                            0.8510    0.8510    0.8510
!!$gray86                            0.8588    0.8588    0.8588
!!$gray87                            0.8706    0.8706    0.8706
!!$gray88                            0.8784    0.8784    0.8784
!!$gray89                            0.8902    0.8902    0.8902
!!$gray9                             0.0902    0.0902    0.0902
!!$gray90                            0.8980    0.8980    0.8980
!!$gray91                            0.9098    0.9098    0.9098
!!$gray92                            0.9216    0.9216    0.9216
!!$gray93                            0.9294    0.9294    0.9294
!!$gray94                            0.9412    0.9412    0.9412
!!$gray95                            0.9490    0.9490    0.9490
!!$gray96                            0.9608    0.9608    0.9608
!!$gray97                            0.9686    0.9686    0.9686
!!$gray98                            0.9804    0.9804    0.9804
!!$gray99                            0.9882    0.9882    0.9882
!!$green                             0.0000    1.0000    0.0000
!!$green1                            0.0000    1.0000    0.0000
!!$green2                            0.0000    0.9333    0.0000
!!$green3                            0.0000    0.8039    0.0000
!!$green4                            0.0000    0.5451    0.0000
!!$greenyellow                       0.6784    1.0000    0.1843
!!$grey                              0.7451    0.7451    0.7451
!!$grey0                             0.0000    0.0000    0.0000
!!$grey1                             0.0118    0.0118    0.0118
!!$grey10                            0.1020    0.1020    0.1020
!!$grey100                           1.0000    1.0000    1.0000
!!$grey11                            0.1098    0.1098    0.1098
!!$grey12                            0.1216    0.1216    0.1216
!!$grey13                            0.1294    0.1294    0.1294
!!$grey14                            0.1412    0.1412    0.1412
!!$grey15                            0.1490    0.1490    0.1490
!!$grey16                            0.1608    0.1608    0.1608
!!$grey17                            0.1686    0.1686    0.1686
!!$grey18                            0.1804    0.1804    0.1804
!!$grey19                            0.1882    0.1882    0.1882
!!$grey2                             0.0196    0.0196    0.0196
!!$grey20                            0.2000    0.2000    0.2000
!!$grey21                            0.2118    0.2118    0.2118
!!$grey22                            0.2196    0.2196    0.2196
!!$grey23                            0.2314    0.2314    0.2314
!!$grey24                            0.2392    0.2392    0.2392
!!$grey25                            0.2510    0.2510    0.2510
!!$grey26                            0.2588    0.2588    0.2588
!!$grey27                            0.2706    0.2706    0.2706
!!$grey28                            0.2784    0.2784    0.2784
!!$grey29                            0.2902    0.2902    0.2902
!!$grey3                             0.0314    0.0314    0.0314
!!$grey30                            0.3020    0.3020    0.3020
!!$grey31                            0.3098    0.3098    0.3098
!!$grey32                            0.3216    0.3216    0.3216
!!$grey33                            0.3294    0.3294    0.3294
!!$grey34                            0.3412    0.3412    0.3412
!!$grey35                            0.3490    0.3490    0.3490
!!$grey36                            0.3608    0.3608    0.3608
!!$grey37                            0.3686    0.3686    0.3686
!!$grey38                            0.3804    0.3804    0.3804
!!$grey39                            0.3882    0.3882    0.3882
!!$grey4                             0.0392    0.0392    0.0392
!!$grey40                            0.4000    0.4000    0.4000
!!$grey41                            0.4118    0.4118    0.4118
!!$grey42                            0.4196    0.4196    0.4196
!!$grey43                            0.4314    0.4314    0.4314
!!$grey44                            0.4392    0.4392    0.4392
!!$grey45                            0.4510    0.4510    0.4510
!!$grey46                            0.4588    0.4588    0.4588
!!$grey47                            0.4706    0.4706    0.4706
!!$grey48                            0.4784    0.4784    0.4784
!!$grey49                            0.4902    0.4902    0.4902
!!$grey5                             0.0510    0.0510    0.0510
!!$grey50                            0.4980    0.4980    0.4980
!!$grey51                            0.5098    0.5098    0.5098
!!$grey52                            0.5216    0.5216    0.5216
!!$grey53                            0.5294    0.5294    0.5294
!!$grey54                            0.5412    0.5412    0.5412
!!$grey55                            0.5490    0.5490    0.5490
!!$grey56                            0.5608    0.5608    0.5608
!!$grey57                            0.5686    0.5686    0.5686
!!$grey58                            0.5804    0.5804    0.5804
!!$grey59                            0.5882    0.5882    0.5882
!!$grey6                             0.0588    0.0588    0.0588
!!$grey60                            0.6000    0.6000    0.6000
!!$grey61                            0.6118    0.6118    0.6118
!!$grey62                            0.6196    0.6196    0.6196
!!$grey63                            0.6314    0.6314    0.6314
!!$grey64                            0.6392    0.6392    0.6392
!!$grey65                            0.6510    0.6510    0.6510
!!$grey66                            0.6588    0.6588    0.6588
!!$grey67                            0.6706    0.6706    0.6706
!!$grey68                            0.6784    0.6784    0.6784
!!$grey69                            0.6902    0.6902    0.6902
!!$grey7                             0.0706    0.0706    0.0706
!!$grey70                            0.7020    0.7020    0.7020
!!$grey71                            0.7098    0.7098    0.7098
!!$grey72                            0.7216    0.7216    0.7216
!!$grey73                            0.7294    0.7294    0.7294
!!$grey74                            0.7412    0.7412    0.7412
!!$grey75                            0.7490    0.7490    0.7490
!!$grey76                            0.7608    0.7608    0.7608
!!$grey77                            0.7686    0.7686    0.7686
!!$grey78                            0.7804    0.7804    0.7804
!!$grey79                            0.7882    0.7882    0.7882
!!$grey8                             0.0784    0.0784    0.0784
!!$grey80                            0.8000    0.8000    0.8000
!!$grey81                            0.8118    0.8118    0.8118
!!$grey82                            0.8196    0.8196    0.8196
!!$grey83                            0.8314    0.8314    0.8314
!!$grey84                            0.8392    0.8392    0.8392
!!$grey85                            0.8510    0.8510    0.8510
!!$grey86                            0.8588    0.8588    0.8588
!!$grey87                            0.8706    0.8706    0.8706
!!$grey88                            0.8784    0.8784    0.8784
!!$grey89                            0.8902    0.8902    0.8902
!!$grey9                             0.0902    0.0902    0.0902
!!$grey90                            0.8980    0.8980    0.8980
!!$grey91                            0.9098    0.9098    0.9098
!!$grey92                            0.9216    0.9216    0.9216
!!$grey93                            0.9294    0.9294    0.9294
!!$grey94                            0.9412    0.9412    0.9412
!!$grey95                            0.9490    0.9490    0.9490
!!$grey96                            0.9608    0.9608    0.9608
!!$grey97                            0.9686    0.9686    0.9686
!!$grey98                            0.9804    0.9804    0.9804
!!$grey99                            0.9882    0.9882    0.9882
!!$honeydew                          0.9412    1.0000    0.9412
!!$honeydew1                         0.9412    1.0000    0.9412
!!$honeydew2                         0.8784    0.9333    0.8784
!!$honeydew3                         0.7569    0.8039    0.7569
!!$honeydew4                         0.5137    0.5451    0.5137
!!$hotpink                           1.0000    0.4118    0.7059
!!$hotpink1                          1.0000    0.4314    0.7059
!!$hotpink2                          0.9333    0.4157    0.6549
!!$hotpink3                          0.8039    0.3765    0.5647
!!$hotpink4                          0.5451    0.2275    0.3843
!!$indianred                         0.8039    0.3608    0.3608
!!$indianred1                        1.0000    0.4157    0.4157
!!$indianred2                        0.9333    0.3882    0.3882
!!$indianred3                        0.8039    0.3333    0.3333
!!$indianred4                        0.5451    0.2275    0.2275
!!$ivory                             1.0000    1.0000    0.9412
!!$ivory1                            1.0000    1.0000    0.9412
!!$ivory2                            0.9333    0.9333    0.8784
!!$ivory3                            0.8039    0.8039    0.7569
!!$ivory4                            0.5451    0.5451    0.5137
!!$khaki                             0.9412    0.9020    0.5490
!!$khaki1                            1.0000    0.9647    0.5608
!!$khaki2                            0.9333    0.9020    0.5216
!!$khaki3                            0.8039    0.7765    0.4510
!!$khaki4                            0.5451    0.5255    0.3059
!!$lavender                          0.9020    0.9020    0.9804
!!$lavenderblush                     1.0000    0.9412    0.9608
!!$lavenderblush1                    1.0000    0.9412    0.9608
!!$lavenderblush2                    0.9333    0.8784    0.8980
!!$lavenderblush3                    0.8039    0.7569    0.7725
!!$lavenderblush4                    0.5451    0.5137    0.5255
!!$lawngreen                         0.4863    0.9882    0.0000
!!$lemonchiffon                      1.0000    0.9804    0.8039
!!$lemonchiffon1                     1.0000    0.9804    0.8039
!!$lemonchiffon2                     0.9333    0.9137    0.7490
!!$lemonchiffon3                     0.8039    0.7882    0.6471
!!$lemonchiffon4                     0.5451    0.5373    0.4392
!!$lightblue                         0.6784    0.8471    0.9020
!!$lightblue1                        0.7490    0.9373    1.0000
!!$lightblue2                        0.6980    0.8745    0.9333
!!$lightblue3                        0.6039    0.7529    0.8039
!!$lightblue4                        0.4078    0.5137    0.5451
!!$lightcoral                        0.9412    0.5020    0.5020
!!$lightcyan                         0.8784    1.0000    1.0000
!!$lightcyan1                        0.8784    1.0000    1.0000
!!$lightcyan2                        0.8196    0.9333    0.9333
!!$lightcyan3                        0.7059    0.8039    0.8039
!!$lightcyan4                        0.4784    0.5451    0.5451
!!$lightgoldenrod                    0.9333    0.8667    0.5098
!!$lightgoldenrod1                   1.0000    0.9255    0.5451
!!$lightgoldenrod2                   0.9333    0.8627    0.5098
!!$lightgoldenrod3                   0.8039    0.7451    0.4392
!!$lightgoldenrod4                   0.5451    0.5059    0.2980
!!$lightgoldenrodyellow              0.9804    0.9804    0.8235
!!$lightgray                         0.8275    0.8275    0.8275
!!$lightgreen                        0.5647    0.9333    0.5647
!!$lightgrey                         0.8275    0.8275    0.8275
!!$lightpink                         1.0000    0.7137    0.7569
!!$lightpink1                        1.0000    0.6824    0.7255
!!$lightpink2                        0.9333    0.6353    0.6784
!!$lightpink3                        0.8039    0.5490    0.5843
!!$lightpink4                        0.5451    0.3725    0.3961
!!$lightsalmon                       1.0000    0.6275    0.4784
!!$lightsalmon1                      1.0000    0.6275    0.4784
!!$lightsalmon2                      0.9333    0.5843    0.4471
!!$lightsalmon3                      0.8039    0.5059    0.3843
!!$lightsalmon4                      0.5451    0.3412    0.2588
!!$lightseagreen                     0.1255    0.6980    0.6667
!!$lightskyblue                      0.5294    0.8078    0.9804
!!$lightskyblue1                     0.6902    0.8863    1.0000
!!$lightskyblue2                     0.6431    0.8275    0.9333
!!$lightskyblue3                     0.5529    0.7137    0.8039
!!$lightskyblue4                     0.3765    0.4824    0.5451
!!$lightslateblue                    0.5176    0.4392    1.0000
!!$lightslategray                    0.4667    0.5333    0.6000
!!$lightslategrey                    0.4667    0.5333    0.6000
!!$lightsteelblue                    0.6902    0.7686    0.8706
!!$lightsteelblue1                   0.7922    0.8824    1.0000
!!$lightsteelblue2                   0.7373    0.8235    0.9333
!!$lightsteelblue3                   0.6353    0.7098    0.8039
!!$lightsteelblue4                   0.4314    0.4824    0.5451
!!$lightyellow                       1.0000    1.0000    0.8784
!!$lightyellow1                      1.0000    1.0000    0.8784
!!$lightyellow2                      0.9333    0.9333    0.8196
!!$lightyellow3                      0.8039    0.8039    0.7059
!!$lightyellow4                      0.5451    0.5451    0.4784
!!$limegreen                         0.1961    0.8039    0.1961
!!$linen                             0.9804    0.9412    0.9020
!!$magenta                           1.0000    0.0000    1.0000
!!$magenta1                          1.0000    0.0000    1.0000
!!$magenta2                          0.9333    0.0000    0.9333
!!$magenta3                          0.8039    0.0000    0.8039
!!$magenta4                          0.5451    0.0000    0.5451
!!$maroon                            0.6902    0.1882    0.3765
!!$maroon1                           1.0000    0.2039    0.7020
!!$maroon2                           0.9333    0.1882    0.6549
!!$maroon3                           0.8039    0.1608    0.5647
!!$maroon4                           0.5451    0.1098    0.3843
!!$mediumaquamarine                  0.4000    0.8039    0.6667
!!$mediumblue                        0.0000    0.0000    0.8039
!!$mediumorchid                      0.7294    0.3333    0.8275
!!$mediumorchid1                     0.8784    0.4000    1.0000
!!$mediumorchid2                     0.8196    0.3725    0.9333
!!$mediumorchid3                     0.7059    0.3216    0.8039
!!$mediumorchid4                     0.4784    0.2157    0.5451
!!$mediumpurple                      0.5765    0.4392    0.8588
!!$mediumpurple1                     0.6706    0.5098    1.0000
!!$mediumpurple2                     0.6235    0.4745    0.9333
!!$mediumpurple3                     0.5373    0.4078    0.8039
!!$mediumpurple4                     0.3647    0.2784    0.5451
!!$mediumseagreen                    0.2353    0.7020    0.4431
!!$mediumslateblue                   0.4824    0.4078    0.9333
!!$mediumspringgreen                 0.0000    0.9804    0.6039
!!$mediumturquoise                   0.2824    0.8196    0.8000
!!$mediumvioletred                   0.7804    0.0824    0.5216
!!$midnightblue                      0.0980    0.0980    0.4392
!!$mintcream                         0.9608    1.0000    0.9804
!!$mistyrose                         1.0000    0.8941    0.8824
!!$mistyrose1                        1.0000    0.8941    0.8824
!!$mistyrose2                        0.9333    0.8353    0.8235
!!$mistyrose3                        0.8039    0.7176    0.7098
!!$mistyrose4                        0.5451    0.4902    0.4824
!!$moccasin                          1.0000    0.8941    0.7098
!!$navahowhite                       1.0000    0.8706    0.6784
!!$navahowhite1                      1.0000    0.8706    0.6784
!!$navahowhite2                      0.9333    0.8118    0.6314
!!$navahowhite3                      0.8039    0.7020    0.5451
!!$navahowhite4                      0.5451    0.4745    0.3686
!!$navajowhite                       1.0000    0.8706    0.6784
!!$navajowhite1                      1.0000    0.8706    0.6784
!!$navajowhite2                      0.9333    0.8118    0.6314
!!$navajowhite3                      0.8039    0.7020    0.5451
!!$navajowhite4                      0.5451    0.4745    0.3686
!!$navy                              0.0000    0.0000    0.5020
!!$navyblue                          0.0000    0.0000    0.5020
!!$oldlace                           0.9922    0.9608    0.9020
!!$olivedrab                         0.4196    0.5569    0.1373
!!$olivedrab1                        0.7529    1.0000    0.2431
!!$olivedrab2                        0.7020    0.9333    0.2275
!!$olivedrab3                        0.6039    0.8039    0.1961
!!$olivedrab4                        0.4118    0.5451    0.1333
!!$orange                            1.0000    0.6471    0.0000
!!$orange1                           1.0000    0.6471    0.0000
!!$orange2                           0.9333    0.6039    0.0000
!!$orange3                           0.8039    0.5216    0.0000
!!$orange4                           0.5451    0.3529    0.0000
!!$orangered                         1.0000    0.2706    0.0000
!!$orangered1                        1.0000    0.2706    0.0000
!!$orangered2                        0.9333    0.2510    0.0000
!!$orangered3                        0.8039    0.2157    0.0000
!!$orangered4                        0.5451    0.1451    0.0000
!!$orchid                            0.8549    0.4392    0.8392
!!$orchid1                           1.0000    0.5137    0.9804
!!$orchid2                           0.9333    0.4784    0.9137
!!$orchid3                           0.8039    0.4118    0.7882
!!$orchid4                           0.5451    0.2784    0.5373
!!$palegoldenrod                     0.9333    0.9098    0.6667
!!$palegreen                         0.5961    0.9843    0.5961
!!$palegreen1                        0.6039    1.0000    0.6039
!!$palegreen2                        0.5647    0.9333    0.5647
!!$palegreen3                        0.4863    0.8039    0.4863
!!$palegreen4                        0.3294    0.5451    0.3294
!!$paleturquoise                     0.6863    0.9333    0.9333
!!$paleturquoise1                    0.7333    1.0000    1.0000
!!$paleturquoise2                    0.6824    0.9333    0.9333
!!$paleturquoise3                    0.5882    0.8039    0.8039
!!$paleturquoise4                    0.4000    0.5451    0.5451
!!$palevioletred                     0.8588    0.4392    0.5765
!!$palevioletred1                    1.0000    0.5098    0.6706
!!$palevioletred2                    0.9333    0.4745    0.6235
!!$palevioletred3                    0.8039    0.4078    0.5373
!!$palevioletred4                    0.5451    0.2784    0.3647
!!$papayawhip                        1.0000    0.9373    0.8353
!!$peachpuff                         1.0000    0.8549    0.7255
!!$peachpuff1                        1.0000    0.8549    0.7255
!!$peachpuff2                        0.9333    0.7961    0.6784
!!$peachpuff3                        0.8039    0.6863    0.5843
!!$peachpuff4                        0.5451    0.4667    0.3961
!!$peru                              0.8039    0.5216    0.2471
!!$pink                              1.0000    0.7529    0.7961
!!$pink1                             1.0000    0.7098    0.7725
!!$pink2                             0.9333    0.6627    0.7216
!!$pink3                             0.8039    0.5686    0.6196
!!$pink4                             0.5451    0.3882    0.4235
!!$plum                              0.8667    0.6275    0.8667
!!$plum1                             1.0000    0.7333    1.0000
!!$plum2                             0.9333    0.6824    0.9333
!!$plum3                             0.8039    0.5882    0.8039
!!$plum4                             0.5451    0.4000    0.5451
!!$powderblue                        0.6902    0.8784    0.9020
!!$purple                            0.6275    0.1255    0.9412
!!$purple1                           0.6078    0.1882    1.0000
!!$purple2                           0.5686    0.1725    0.9333
!!$purple3                           0.4902    0.1490    0.8039
!!$purple4                           0.3333    0.1020    0.5451
!!$red                               1.0000    0.0000    0.0000
!!$red1                              1.0000    0.0000    0.0000
!!$red2                              0.9333    0.0000    0.0000
!!$red3                              0.8039    0.0000    0.0000
!!$red4                              0.5451    0.0000    0.0000
!!$rosybrown                         0.7373    0.5608    0.5608
!!$rosybrown1                        1.0000    0.7569    0.7569
!!$rosybrown2                        0.9333    0.7059    0.7059
!!$rosybrown3                        0.8039    0.6078    0.6078
!!$rosybrown4                        0.5451    0.4118    0.4118
!!$royalblue                         0.2549    0.4118    0.8824
!!$royalblue1                        0.2824    0.4627    1.0000
!!$royalblue2                        0.2627    0.4314    0.9333
!!$royalblue3                        0.2275    0.3725    0.8039
!!$royalblue4                        0.1529    0.2510    0.5451
!!$saddlebrown                       0.5451    0.2706    0.0745
!!$salmon                            0.9804    0.5020    0.4471
!!$salmon1                           1.0000    0.5490    0.4118
!!$salmon2                           0.9333    0.5098    0.3843
!!$salmon3                           0.8039    0.4392    0.3294
!!$salmon4                           0.5451    0.2980    0.2235
!!$sandybrown                        0.9569    0.6431    0.3765
!!$seagreen                          0.1804    0.5451    0.3412
!!$seagreen1                         0.3294    1.0000    0.6235
!!$seagreen2                         0.3059    0.9333    0.5804
!!$seagreen3                         0.2627    0.8039    0.5020
!!$seagreen4                         0.1804    0.5451    0.3412
!!$seashell                          1.0000    0.9608    0.9333
!!$seashell1                         1.0000    0.9608    0.9333
!!$seashell2                         0.9333    0.8980    0.8706
!!$seashell3                         0.8039    0.7725    0.7490
!!$seashell4                         0.5451    0.5255    0.5098
!!$sienna                            0.6275    0.3216    0.1765
!!$sienna1                           1.0000    0.5098    0.2784
!!$sienna2                           0.9333    0.4745    0.2588
!!$sienna3                           0.8039    0.4078    0.2235
!!$sienna4                           0.5451    0.2784    0.1490
!!$skyblue                           0.5294    0.8078    0.9216
!!$skyblue1                          0.5294    0.8078    1.0000
!!$skyblue2                          0.4941    0.7529    0.9333
!!$skyblue3                          0.4235    0.6510    0.8039
!!$skyblue4                          0.2902    0.4392    0.5451
!!$slateblue                         0.4157    0.3529    0.8039
!!$slateblue1                        0.5137    0.4353    1.0000
!!$slateblue2                        0.4784    0.4039    0.9333
!!$slateblue3                        0.4118    0.3490    0.8039
!!$slateblue4                        0.2784    0.2353    0.5451
!!$slategray                         0.4392    0.5020    0.5647
!!$slategray1                        0.7765    0.8863    1.0000
!!$slategray2                        0.7255    0.8275    0.9333
!!$slategray3                        0.6235    0.7137    0.8039
!!$slategray4                        0.4235    0.4824    0.5451
!!$slategrey                         0.4392    0.5020    0.5647
!!$slategrey1                        0.7765    0.8863    1.0000
!!$slategrey2                        0.7255    0.8275    0.9333
!!$slategrey3                        0.6235    0.7137    0.8039
!!$slategrey4                        0.4235    0.4824    0.5451
!!$snow                              1.0000    0.9804    0.9804
!!$snow1                             1.0000    0.9804    0.9804
!!$snow2                             0.9333    0.9137    0.9137
!!$snow3                             0.8039    0.7882    0.7882
!!$snow4                             0.5451    0.5373    0.5373
!!$springgreen                       0.0000    1.0000    0.4980
!!$springgreen1                      0.0000    1.0000    0.4980
!!$springgreen2                      0.0000    0.9333    0.4627
!!$springgreen3                      0.0000    0.8039    0.4000
!!$springgreen4                      0.0000    0.5451    0.2706
!!$steelblue                         0.2745    0.5098    0.7059
!!$steelblue1                        0.3882    0.7216    1.0000
!!$steelblue2                        0.3608    0.6745    0.9333
!!$steelblue3                        0.3098    0.5804    0.8039
!!$steelblue4                        0.2118    0.3922    0.5451
!!$tan                               0.8235    0.7059    0.5490
!!$tan1                              1.0000    0.6471    0.3098
!!$tan2                              0.9333    0.6039    0.2863
!!$tan3                              0.8039    0.5216    0.2471
!!$tan4                              0.5451    0.3529    0.1686
!!$thistle                           0.8471    0.7490    0.8471
!!$thistle1                          1.0000    0.8824    1.0000
!!$thistle2                          0.9333    0.8235    0.9333
!!$thistle3                          0.8039    0.7098    0.8039
!!$thistle4                          0.5451    0.4824    0.5451
!!$tomato                            1.0000    0.3882    0.2784
!!$tomato1                           1.0000    0.3882    0.2784
!!$tomato2                           0.9333    0.3608    0.2588
!!$tomato3                           0.8039    0.3098    0.2235
!!$tomato4                           0.5451    0.2118    0.1490
!!$turquoise                         0.2510    0.8784    0.8157
!!$turquoise1                        0.0000    0.9608    1.0000
!!$turquoise2                        0.0000    0.8980    0.9333
!!$turquoise3                        0.0000    0.7725    0.8039
!!$turquoise4                        0.0000    0.5255    0.5451
!!$violet                            0.9333    0.5098    0.9333
!!$violetred                         0.8157    0.1255    0.5647
!!$violetred1                        1.0000    0.2431    0.5882
!!$violetred2                        0.9333    0.2275    0.5490
!!$violetred3                        0.8039    0.1961    0.4706
!!$violetred4                        0.5451    0.1333    0.3216
!!$wheat                             0.9608    0.8706    0.7020
!!$wheat1                            1.0000    0.9059    0.7294
!!$wheat2                            0.9333    0.8471    0.6824
!!$wheat3                            0.8039    0.7294    0.5882
!!$wheat4                            0.5451    0.4941    0.4000
!!$white                             1.0000    1.0000    1.0000
!!$whitesmoke                        0.9608    0.9608    0.9608
!!$yellow                            1.0000    1.0000    0.0000
!!$yellow1                           1.0000    1.0000    0.0000
!!$yellow2                           0.9333    0.9333    0.0000
!!$yellow3                           0.8039    0.8039    0.0000
!!$yellow4                           0.5451    0.5451    0.0000
!!$yellowgreen                       0.6039    0.8039    0.1961


end subroutine set_color_table

end module ncar_plot_class
