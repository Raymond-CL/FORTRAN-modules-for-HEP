! define a variable in global scope
module radius
  use nrtype
  implicit none
  real(sp) :: rad
end module radius



! main program
program main
  use nr, only:vegas
  use radius
  implicit none
  ! vegas necessary variables
  integer :: init,itmax,ncall,ndim,nprn
  real(sp) :: avgi,chi2a,sd
  real(sp), dimension(20) :: region
  ! interface for the integrand function
  interface
  function fxn(pt,wgt)
  use nrtype
  implicit none
  real(sp), dimension(:), intent(in) :: pt
  real(sp), intent(in) :: wgt
  real(sp) :: fxn
  end function fxn
  end interface
  ! set coefficients
  ndim = 3
  rad = 1.0
  ncall = 100000
  itmax = 1
  nprn = -1
  region(1:ndim) = 0.0
  region(1+ndim:2*ndim) = rad
  ! warm-up the vegas adaptive grid
  init = -1
  call vegas(region(1:2*ndim),fxn,init,ncall,10*itmax,nprn,avgi,sd,chi2a)
  ! actual vegas run
  init = +1
  call vegas(region(1:2*ndim),fxn,init,10*ncall,itmax,nprn,avgi,sd,chi2a)
  ! integrated result (multiplied by symmetry factor)
  write(*,*) "integrated result :",avgi * 2**ndim
  ! actual result
  write(*,*) "theoretical result:",PI**(ndim/2.0) / gamma(ndim/2.0+1.0) * rad**ndim  
end program main



! integrand function
function fxn(pt,wgt)
  use nrtype
  use radius
  implicit none
  real(sp), dimension(:), intent(in) :: pt
  real(sp), intent(in) :: wgt
  real(sp) :: fxn
  fxn = 0.0
  ! using rejection sampling (Euclidean norm)
  if( sum(pt(:)**2) .lt. rad**2 ) fxn = 1.0
  return
end function fxn
