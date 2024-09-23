module time

  use, intrinsic :: iso_fortran_env, only: int32,real64,stdout=>output_unit
  implicit none
  private
  real(real64) :: cpu_start,cpu_stop
  integer(int32) :: count_start,count_stop,count_rate
  real(real64) :: cputime,walltime
  integer(int32), dimension(8) :: v0,v1
  public :: time_start,time_stop,print_time

contains

  subroutine time_start
    call cpu_time(cpu_start)
    call system_clock(count_start,count_rate)
    call date_and_time(VALUES=v0)
  end subroutine

  subroutine time_stop
    call cpu_time(cpu_stop)
    call system_clock(count_stop ,count_rate)
    call date_and_time(VALUES=v1)
  end subroutine time_stop

  subroutine print_time
    cputime = cpu_stop-cpu_start
    walltime = real(count_stop-count_start,real64)/count_rate
    write(stdout,99)
    write(stdout,11) v0(1:3),v0(5:8)
    write(stdout,12) v1(1:3),v1(5:8)
    write(stdout,13) cputime
    write(stdout,14) walltime
    write(stdout,99)
11  format(' time started at: ',i4,'/',i2,'/',i2,' - ',i2,':',i2,':',i2,':',i3)
12  format(' time stopped at: ',i4,'/',i2,'/',i2,' - ',i2,':',i2,':',i2,':',i3)
13  format(' CPU  time elapsed: ',f10.2, ' seconds.')
14  format(' Wall time elapsed: ',f10.2, ' seconds.')
99  format(72('*'))
  end subroutine print_time

end module time
