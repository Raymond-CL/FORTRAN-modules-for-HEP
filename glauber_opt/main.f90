program main
  use time
  use vint  ! uses vegas integration module
  use glauber_opt
  implicit none
  call setgeo(208)
  call time_start
  ! set vegas parameters
  ndim = 2
  ncall = 1000000
  itmax = 1
  nprn = -1
  region(1) = -20d0
  region(2) = -20d0
  !region(3) = 0d0
  region(3) = +20d0
  region(4) = +20d0
  !region(6) = 8d0*atan(1d0)
  init = -1
  call vegas(region(1:2*ndim),fxn,init,ncall,10*itmax,nprn,avgi,sd,chi2a)
  init = +1
  call vegas(region(1:2*ndim),fxn,init,10*ncall,itmax,nprn,avgi,sd,chi2a)
  write(*,*) "integrated result :",avgi!/(8d0*atan(1d0))
  !write(*,*) "theoretical result:",207d0*207d0
  call time_stop
  call print_time
end program main



function fxn(pt,wgt)
  use glauber_opt
  implicit none
  double precision, dimension(:), intent(in) :: pt
  double precision, intent(in) :: wgt
  double precision :: fxn
  double precision :: x,y,th
  fxn = 0d0
  x = pt(1)
  y = pt(2)
  !th= pt(3)
  fxn = TAB(x,y,0d0)
  return
end function fxn
