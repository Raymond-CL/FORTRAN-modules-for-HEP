program main
  use, intrinsic :: iso_fortran_env, only : int32
  use time
  implicit none
  logical :: isprime
  integer(int32) :: n,i,p
  integer(int32), allocatable :: primes(:)
  ! primes(:) is dynamic array that stores prime numbers

  ! note that n > 50000 might take a long time
  n = 50000

  ! start stopwatch
  call time_start

  ! set the first 2 easy primes
  allocate(primes(0))
  primes = [primes,2]
  primes = [primes,3]
  p = 3

  if(n < 1) then
    write(*,*) "Please enter correct n"
  elseif(n == 1) then
    write(*,*) "The",n,"th prime is:",primes(n)
  elseif(n == 2) then
    write(*,*) "The",n,"th prime is:",primes(n)
  else
    ! prime search algorithm
    ! this algorithm check each odd number against existing primes
    ! not very efficient because it does not uilize the prime number theorem
    do while ( size(primes) < n )
      isprime = .false.
      checkp: do while ( .not.(isprime) )
        p = p + 2
        do i = 2,size(primes)
          if( mod(p,primes(i)) == 0 ) cycle checkp
        enddo
        isprime = .true.
      enddo checkp
      primes = [primes,p]
    enddo
    write(*,*) "The",n,"th prime is:",primes(n)
  endif

  ! stop stopwatch
  call time_stop

  ! print duration
  call print_time

end program main
