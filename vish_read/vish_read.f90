module vish_read
  
  implicit none
  private
  double precision, parameter :: PI = 4d0*atan(1d0)
  double precision, parameter :: twoPI = 2d0*PI
  ! output data file name
  character(20), parameter :: ifile = "output.dat"
  ! define hydro boundaries
  double precision, parameter :: xymax = 15d0
  ! hydro lattice number (set this)
  integer, parameter :: xyn = 15
  integer, parameter :: thn = 36
  ! lattice size definition
  double precision, parameter :: xybin = xymax/xyn
  double precision, parameter :: thbin = 2d0*PI/thn
  ! 3d table to store data
  double precision, dimension(1:thn,-xyn:+xyn,-xyn:+xyn) :: vish_table
  public :: Read_data,get_val

contains
  
  ! read data from file and populate vish_table
  subroutine Read_data
  implicit none
  integer :: u,st,i,j,k
  double precision :: x,y,th
  u = 30
  open(unit=u,file=ifile,status='old',action='read',iostat=st)
  if(st /= 0) then
    write(*,*) "file read error!";  call exit(1)
  endif
  do i = -xyn,+xyn
    do j = -xyn,+xyn
      do k = 1,thn
        read(u,*) x,y,th,vish_table(k,j,i)
      enddo
    enddo
  enddo
  close(u)
  end subroutine Read_data

  function get_val(x,y,th) result(res)
  double precision, intent(in) :: x,y,th
  double precision :: res
  double precision :: theta
  integer :: xL,xR,yL,yR,thL,thR
  double precision :: x0,x1,y0,y1,th0,th1
  double precision :: t000,t001,t010,t011,t100,t101,t110,t111
  double precision :: xd,yd,thd
  double precision :: t00x,t01x,t10x,t11x,t0yx,t1yx,ttyx
  ! check for boundary condition
  if (x.le.-xymax .or. x.ge.+xymax .or. &
      y.le.-xymax .or. y.ge.+xymax) then 
    res = 0d0
    return
  endif
  theta = th
  do while (theta.lt.0d0)
    theta = theta + twoPI
  enddo
  do while (theta.gt.twoPI)
    theta = theta - twoPI
  enddo
  ! get lower and upper index
  xL = floor(x/xybin)
  xR = ceiling(x/xybin)
  yL = floor(y/xybin)
  yR = ceiling(y/xybin)
  thL = floor(theta/thbin-0.5d0)+1
  thR = ceiling(theta/thbin-0.5d0)+1
  if(thL.eq.0) thL = thn
  if(thR.eq.thn+1) thR = 1
  ! get lower and upper coordinate
  x0 = dble(xL)*xybin
  x1 = dble(xR)*xybin
  y0 = dble(yL)*xybin
  y1 = dble(yR)*xybin
  th0 = dble(thL-0.5d0)*thbin
  th1 = dble(thR-0.5d0)*thbin
  ! get coordinate values
  t000 = vish_table(thL,yL,xL)
  t001 = vish_table(thL,yL,xR)
  t010 = vish_table(thL,yR,xL)
  t011 = vish_table(thL,yR,xR)
  t100 = vish_table(thR,yL,xL)
  t101 = vish_table(thR,yL,xR)
  t110 = vish_table(thR,yR,xL)
  t111 = vish_table(thR,yR,xR)
  ! evaluate differences
  xd = (x-x0)/xybin
  yd = (y-y0)/xybin
  if(theta.le.thbin/2d0) then
    thd = theta/thbin+0.5d0
  else
    thd = (theta-th0)/thbin
  endif
  ! 3D linear interpolate 
  t00x = t000*(1d0-xd) + t001*xd
  t01x = t010*(1d0-xd) + t011*xd
  t10x = t100*(1d0-xd) + t101*xd
  t11x = t110*(1d0-xd) + t111*xd
  t0yx = t00x*(1d0-yd) + t01x*xd
  t1yx = t10x*(1d0-yd) + t11x*xd
  ttyx = t0yx*(1d0-thd) + t1yx*thd
  res = ttyx
  return
  end function get_val

end module vish_read
