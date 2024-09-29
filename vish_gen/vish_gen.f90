module vish_gen
  
  implicit none
  private
  double precision, parameter :: PI = 4d0*atan(1d0)
  ! output data file name
  character(20), parameter :: ofile = "vish.dat"
  ! define hydro boundaries
  double precision, parameter :: tau0 = 0.6d0
  double precision, parameter :: taumax = 40d0
  double precision, parameter :: xymax = 15d0
  ! hydro lattice number (set this)
  integer, parameter :: xyn = 15
  integer, parameter :: thn = 36
  ! lattice size definition
  double precision, parameter :: xybin = xymax/xyn
  double precision, parameter :: thbin = 2d0*PI/thn
  ! freezeout temperature PRD 90 (2014) 094503
  double precision, parameter :: Tc = 0.154d0
  ! loop variable
  double precision :: x,y,th
  ! highest temperature at core
  double precision :: T0
  ! some other dummy variables
  double precision :: e,s,Bd,vx,vy
  public :: ofile,Hydro_Set, Gen_Table

contains

  ! hydro set subroutine
  subroutine Hydro_Set
  ! call internal data file reader
  call hydroSetFiles()
  ! read profile first to get T0
  call hydroReadInfo2DZeroOnErr(tau0,0d0,0d0,e,s,T0,Bd,vx,vy)
  end subroutine Hydro_Set

  ! table generation subroutine
  subroutine Gen_table(u)
  use vint
  integer, intent(in) :: u
  integer :: i,j,k
  ! set vegas parameters
  ndim = 1
  ncall = 100000
  itmax = 1
  nprn = -1
  region(1) = 0d0
  region(2) = taumax
  ! begin loop
  do i = -xyn,+xyn
    do j = -xyn,+xyn
      !write(*,*) "working on:",real(i*xybin),real(j*xybin)
      open(u,file=ofile,status="old",position="append")
      do k = 1,thn
        ! define loop variables
        x = i*xybin
        y = j*xybin
        th = (k-0.5d0)*thbin
        ! call vegas integration
        init = -1
        call vegas(region(1:2),func,init,ncall,10*itmax,nprn,avgi,sd,chi2a)
        init = +1
        call vegas(region(1:2),func,init,10*ncall,itmax,nprn,avgi,sd,chi2a)
        ! write to output file
        write(u,'(2(f10.4),f10.6,2(e15.5))') x,y,th,avgi/T0**3
      enddo
      close(u)
    enddo
  enddo
  end subroutine Gen_table

  ! integrand function
  function func(pt,wgt)
  implicit none
  double precision, dimension(:), intent(in) :: pt
  double precision, intent(in) :: wgt
  double precision :: func
  double precision :: tau,xp,yp,T
  func = 0d0
  tau = pt(1)
  ! no quenching before tau0
  if(tau.lt.tau0) return
  ! free-streaming (eikonal approximation)
  xp = x + tau*cos(th)
  yp = y + tau*sin(th)
  ! read temperature info, no need for the rest
  call hydroReadInfo2DZeroOnErr(tau,xp,yp,e,s,T,Bd,vx,vy)
  ! no quenching after freezeout
  if(T.lt.Tc) return
  func = T**3 * tau
  return
  end function func

end module vish_gen
