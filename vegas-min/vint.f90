module vint
  use, intrinsic :: iso_fortran_env, only: int32
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private
  integer, parameter :: wp = real64
  real(wp), parameter :: PI=3.141592653589793238462643383279502884197_wp
  INTEGER(int32), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
  INTEGER(int32), PARAMETER :: hg=huge(1_int32), hgm=-hg, hgng=hgm-1
  INTEGER(int32), SAVE :: lenran=0, seq=0
  INTEGER(int32), SAVE :: iran0,jran0,kran0,nran0,mran0,rans
  INTEGER(int32), DIMENSION(:,:), POINTER, SAVE :: ranseeds
  INTEGER(int32), DIMENSION(:), POINTER, SAVE :: iran,jran,kran,nran,mran,ranv
  REAL(wp), SAVE :: amm
  INTERFACE reallocate
    MODULE PROCEDURE reallocate_iv,reallocate_im
  END INTERFACE
  interface
    function fxn(pt,weight)
    implicit none
    double precision, dimension(:), intent(in) :: pt
    double precision, intent(in) :: weight
    double precision :: fxn
    end function fxn
  end interface
  public :: wp,PI,vegas,fxn

contains

  FUNCTION reallocate_iv(p,n)
    INTEGER(int32), DIMENSION(:), POINTER :: p, reallocate_iv
    INTEGER(int32), INTENT(IN) :: n
    INTEGER(int32) :: nold,ierr
    allocate(reallocate_iv(n),stat=ierr)
    if (ierr /= 0) call nrerror('reallocate_iv: problem in attempt to allocate memory')
    if (.not. associated(p)) RETURN
    nold=size(p)
    reallocate_iv(1:min(nold,n))=p(1:min(nold,n))
    deallocate(p)
  END FUNCTION reallocate_iv

  FUNCTION reallocate_im(p,n,m)
    INTEGER(int32), DIMENSION(:,:), POINTER :: p, reallocate_im
    INTEGER(int32), INTENT(IN) :: n,m
    INTEGER(int32) :: nold,mold,ierr
    allocate(reallocate_im(n,m),stat=ierr)
    if (ierr /= 0) call nrerror('reallocate_im: problem in attempt to allocate memory')
    if (.not. associated(p)) RETURN
    nold=size(p,1)
    mold=size(p,2)
    reallocate_im(1:min(nold,n),1:min(mold,m))=p(1:min(nold,n),1:min(mold,m))
    deallocate(p)
  END FUNCTION reallocate_im

  SUBROUTINE nrerror(string)
    CHARACTER(LEN=*), INTENT(IN) :: string
    write (*,*) 'nrerror: ',string
    STOP 'program terminated by nrerror'
  END SUBROUTINE nrerror

  FUNCTION arth(first,increment,n)
    INTEGER(int32), INTENT(IN) :: first,increment,n
    INTEGER(int32), DIMENSION(n) :: arth
    INTEGER(int32) :: k,k2,temp
    if (n > 0) arth(1)=first
    if (n <= NPAR_ARTH) then
      do k=2,n
        arth(k)=arth(k-1)+increment
      end do
    else
      do k=2,NPAR2_ARTH
        arth(k)=arth(k-1)+increment
      end do
      temp=increment*NPAR2_ARTH
      k=NPAR2_ARTH
      do
        if (k >= n) exit
        k2=k+k
        arth(k+1:min(k2,n))=temp+arth(1:min(k,n-k))
        temp=temp+temp
        k=k2
      end do
    end if
  END FUNCTION arth

  SUBROUTINE ran_init(length)
    INTEGER(int32), INTENT(IN) :: length
    INTEGER(int32) :: new,j,hgt
    if (length < lenran) RETURN
    hgt=hg
    if (hg /= 2147483647) call nrerror('ran_init: arith assump 1 fails')
    if (hgng >= 0)        call nrerror('ran_init: arith assump 2 fails')
    if (hgt+1 /= hgng)    call nrerror('ran_init: arith assump 3 fails')
    if (not(hg) >= 0)     call nrerror('ran_init: arith assump 4 fails')
    if (not(hgng) < 0)    call nrerror('ran_init: arith assump 5 fails')
    if (hg+hgng >= 0)     call nrerror('ran_init: arith assump 6 fails')
    if (not(-1_int32) < 0)  call nrerror('ran_init: arith assump 7 fails')
    if (not(0_int32) >= 0)  call nrerror('ran_init: arith assump 8 fails')
    if (not(1_int32) >= 0)  call nrerror('ran_init: arith assump 9 fails')
    if (lenran > 0) then
      ranseeds=>reallocate(ranseeds,length,5)
      ranv=>reallocate(ranv,length-1)
      new=lenran+1
    else
      allocate(ranseeds(length,5))
      allocate(ranv(length-1))
      new=1
      amm=nearest(1.0_wp,-1.0_wp)/hgng
      if (amm*hgng >= 1.0 .or. amm*hgng <= 0.0) call nrerror('ran_init: arth assump 10 fails')
    end if
    ranseeds(new:,1)=seq
    ranseeds(new:,2:5)=spread(arth(new,1,size(ranseeds(new:,1))),2,4)
    do j=1,4
      call ran_hash(ranseeds(new:,j),ranseeds(new:,j+1))
    end do
    where (ranseeds(new:,1:3) < 0) ranseeds(new:,1:3)=not(ranseeds(new:,1:3))
    where (ranseeds(new:,4:5) == 0) ranseeds(new:,4:5)=1
    if (new == 1) then
      iran0=ranseeds(1,1)
      jran0=ranseeds(1,2)
      kran0=ranseeds(1,3)
      mran0=ranseeds(1,4)
      nran0=ranseeds(1,5)
      rans=nran0
    end if
    if (length > 1) then
      iran => ranseeds(2:,1)
      jran => ranseeds(2:,2)
      kran => ranseeds(2:,3)
      mran => ranseeds(2:,4)
      nran => ranseeds(2:,5)
      ranv = nran
    end if
    lenran=length
  END SUBROUTINE ran_init

  SUBROUTINE ran_deallocate
    if (lenran > 0) then
      deallocate(ranseeds,ranv)
      nullify(ranseeds,ranv,iran,jran,kran,mran,nran)
      lenran = 0
    end if
  END SUBROUTINE ran_deallocate

  SUBROUTINE ran_seed(sequence,size,put,get)
    INTEGER, OPTIONAL, INTENT(IN) :: sequence
    INTEGER, OPTIONAL, INTENT(OUT) :: size
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: put
    INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: get
    if (present(size)) then
      size=5*lenran
    else if (present(put)) then
      if (lenran == 0) RETURN
      ranseeds=reshape(put,shape(ranseeds))
      where (ranseeds(:,1:3) < 0) ranseeds(:,1:3)=not(ranseeds(:,1:3))
      where (ranseeds(:,4:5) == 0) ranseeds(:,4:5)=1
      iran0=ranseeds(1,1)
      jran0=ranseeds(1,2)
      kran0=ranseeds(1,3)
      mran0=ranseeds(1,4)
      nran0=ranseeds(1,5)
    else if (present(get)) then
      if (lenran == 0) RETURN
      ranseeds(1,1:5)=(/ iran0,jran0,kran0,mran0,nran0 /)
      get=reshape(ranseeds,shape(get))
    else if (present(sequence)) then
      call ran_deallocate
      seq=sequence
    end if
  END SUBROUTINE ran_seed

  SUBROUTINE ran_hash(il,ir)
    IMPLICIT NONE
    INTEGER(int32), DIMENSION(:), INTENT(INOUT) :: il,ir
    INTEGER(int32), DIMENSION(size(il)) :: is
    INTEGER(int32) :: j
    do j=1,4
      is=ir
      ir=ieor(ir,ishft(ir,5))+1422217823
      ir=ieor(ir,ishft(ir,-16))+1842055030
      ir=ieor(ir,ishft(ir,9))+80567781
      ir=ieor(il,ir)
      il=is
    end do
  END SUBROUTINE ran_hash

  SUBROUTINE ran1(harvest)
    REAL(wp), INTENT(OUT) :: harvest
    if (lenran < 1) call ran_init(1)
    rans=iran0-kran0
    if (rans < 0) rans=rans+2147483579_int32
    iran0=jran0
    jran0=kran0
    kran0=rans
    nran0=ieor(nran0,ishft(nran0,13))
    nran0=ieor(nran0,ishft(nran0,-17))
    nran0=ieor(nran0,ishft(nran0,5))
    if (nran0 == 1) nran0=270369_int32
    mran0=ieor(mran0,ishft(mran0,5))
    mran0=ieor(mran0,ishft(mran0,-13))
    mran0=ieor(mran0,ishft(mran0,6))
    rans=ieor(nran0,rans)+mran0
    harvest=amm*merge(rans,not(rans), rans<0 )
  END SUBROUTINE ran1

  SUBROUTINE vegas(region,func,init,ncall,itmx,nprn,tgral,sd,chi2a)
    REAL(wp), DIMENSION(:), INTENT(IN) :: region
    INTEGER(int32), INTENT(IN) :: init,ncall,itmx,nprn
    REAL(wp), INTENT(OUT) :: tgral,sd,chi2a
    INTERFACE
      FUNCTION func(pt,weight)
      IMPLICIT NONE
      double precision, DIMENSION(:), INTENT(IN) :: pt
      double precision, INTENT(IN) :: weight
      double precision :: func
      END FUNCTION func
    END INTERFACE
    REAL(wp), PARAMETER :: ALPH=1.5_wp,TINY=1.0e-30_wp
    INTEGER(int32), PARAMETER :: MXDIM=10,NDMX=50
    INTEGER(int32), SAVE :: i,it,j,k,mds,nd,ndim,ndo,ng,npg
    INTEGER(int32), DIMENSION(MXDIM), SAVE :: ia,kg
    REAL(wp), SAVE :: calls,dv2g,dxg,f,f2,f2b,fb,rc,ti,tsi,wgt,xjac,xn,xnd,xo,harvest
    REAL(wp), DIMENSION(NDMX,MXDIM), SAVE :: d,di,xi
    REAL(wp), DIMENSION(MXDIM), SAVE :: dt,dx,x
    REAL(wp), DIMENSION(NDMX), SAVE :: r,xin
    REAL(wp), SAVE :: schi,si,swgt
    ndim=size(region)/2
    if (init <= 0) then
      mds=1
      ndo=1
      xi(1,:)=1.0
    end if
    if (init <= 1) then
      si=0.0
      swgt=0.0
      schi=0.0
    end if
    if (init <= 2) then
      nd=NDMX
      ng=1
      if (mds /= 0) then
        ng=int( (ncall/2.0_wp+0.25_wp)**(1.0_wp/ndim) )
        mds=1
        if ((2*ng-NDMX) >= 0) then
          mds=-1
          npg=ng/NDMX+1
          nd=ng/npg
          ng=npg*nd
        end if
      end if
      k=ng**ndim
      npg=max(ncall/k,2)
      calls=real(npg,wp)*real(k,wp)
      dxg=1.0_wp/ng
      dv2g=(calls*dxg**ndim)**2/npg/npg/(npg-1.0_wp)
      xnd=nd
      dxg=dxg*xnd
      dx(1:ndim)=region(1+ndim:2*ndim)-region(1:ndim)
      xjac=1.0_wp/calls*product(dx(1:ndim))
      if (nd /= ndo) then
        r(1:max(nd,ndo))=1.0
        do j=1,ndim
          call rebin(ndo/xnd,nd,r,xin,xi(:,j))
        end do
        ndo=nd
      end if
      if (nprn >= 0) write(*,200) ndim,calls,it,itmx,nprn,ALPH,mds,nd,(j,region(j),j,region(j+ndim),j=1,ndim)
    end if
    do it=1,itmx
      ti=0.0
      tsi=0.0
      kg(:)=1
      d(1:nd,:)=0.0
      di(1:nd,:)=0.0
      iterate: do
        fb=0.0
        f2b=0.0
        do k=1,npg
          wgt=xjac
          do j=1,ndim
            call ran1(harvest)
            xn=(kg(j)-harvest)*dxg+1.0_wp
            ia(j)=max(min(int(xn),NDMX),1)
            if (ia(j) > 1) then
              xo=xi(ia(j),j)-xi(ia(j)-1,j)
              rc=xi(ia(j)-1,j)+(xn-ia(j))*xo
            else
              xo=xi(ia(j),j)
              rc=(xn-ia(j))*xo
            end if
            x(j)=region(j)+rc*dx(j)
            wgt=wgt*xo*xnd
          end do
          f=wgt*func(x(1:ndim),wgt)
          f2=f*f
          fb=fb+f
          f2b=f2b+f2
          do j=1,ndim
            di(ia(j),j)=di(ia(j),j)+f
            if (mds >= 0) d(ia(j),j)=d(ia(j),j)+f2
          end do
        end do
        f2b=sqrt(f2b*npg)
        f2b=(f2b-fb)*(f2b+fb)
        if (f2b <= 0.0) f2b=TINY
        ti=ti+fb
        tsi=tsi+f2b
        if (mds < 0) then
          do j=1,ndim
            d(ia(j),j)=d(ia(j),j)+f2b
          end do
        end if
        do k=ndim,1,-1
          kg(k)=mod(kg(k),ng)+1
          if (kg(k) /= 1) cycle iterate
        end do
        exit iterate
      end do iterate
      tsi=tsi*dv2g
      wgt=1.0_wp/tsi
      si=si+real(wgt,wp)*real(ti,wp)
      schi=schi+real(wgt,wp)*real(ti,wp)**2
      swgt=swgt+real(wgt,wp)
      tgral=si/swgt
      chi2a=max((schi-si*tgral)/(it-0.99_wp),0.0_wp)
      sd=sqrt(1.0_wp/swgt)
      tsi=sqrt(tsi)
      if (nprn >= 0) then
        write(*,201) it,ti,tsi,tgral,sd,chi2a
        if (nprn /= 0) then
          do j=1,ndim
            write(*,202) j,(xi(i,j),di(i,j),i=1+nprn/2,nd,nprn)
          end do
        end if
      end if
      do j=1,ndim
        xo=d(1,j)
        xn=d(2,j)
        d(1,j)=(xo+xn)/2.0_wp
        dt(j)=d(1,j)
        do i=2,nd-1
          rc=xo+xn
          xo=xn
          xn=d(i+1,j)
          d(i,j)=(rc+xn)/3.0_wp
          dt(j)=dt(j)+d(i,j)
        end do
        d(nd,j)=(xo+xn)/2.0_wp
        dt(j)=dt(j)+d(nd,j)
      end do
      where (d(1:nd,:) < TINY) d(1:nd,:)=TINY
      do j=1,ndim
        r(1:nd)=((1.0_wp-d(1:nd,j)/dt(j))/(log(dt(j))-log(d(1:nd,j))))**ALPH
        rc=sum(r(1:nd))
        call rebin(rc/xnd,nd,r,xin,xi(:,j))
      end do
    end do
  200 format(/' input parameters for vegas:  ndim=',i3,'  ncall=',f8.0&
      /28x,'  it=',i5,'  itmx=',i5&
      /28x,'  nprn=',i3,'  alph=',f5.2/28x,'  mds=',i3,'   nd=',i4&
      /(30x,'xl(',i2,')= ',g11.4,' xu(',i2,')= ',g11.4))
  201 format(/' iteration no.',I3,': ','integral =',g14.7,' +/- ',g9.2,&
      /' all iterations:   integral =',g14.7,' +/- ',g9.2,&
      ' chi**2/it''n =',g9.2)
  202 format(/' data for axis ',I2/'    X       delta i       ',&
      '   x       delta i       ','    x       delta i       ',&
      /(1x,f7.5,1x,g11.4,5x,f7.5,1x,g11.4,5x,f7.5,1x,g11.4))

  END SUBROUTINE vegas
  
  SUBROUTINE rebin(rc,nd,r,xin,xi)
    REAL(wp), INTENT(IN) :: rc
    INTEGER(int32), INTENT(IN) :: nd
    REAL(wp), DIMENSION(:), INTENT(IN) :: r
    REAL(wp), DIMENSION(:), INTENT(OUT) :: xin
    REAL(wp), DIMENSION(:), INTENT(INOUT) :: xi
    INTEGER(int32) :: i,k
    REAL(wp) :: dr,xn,xo
    k=0
    xo=0.0
    dr=0.0
    do i=1,nd-1
      do
        if (rc <= dr) exit
        k=k+1
        dr=dr+r(k)
      end do
      if (k > 1) xo=xi(k-1)
      xn=xi(k)
      dr=dr-rc
      xin(i)=xn-(xn-xo)*dr/r(k)
    end do
    xi(1:nd-1)=xin(1:nd-1)
    xi(nd)=1.0
  END SUBROUTINE rebin

end module vint
