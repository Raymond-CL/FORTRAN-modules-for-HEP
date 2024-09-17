## time

### description

A module with convenient subroutines that records program starting and ending times and displays the duration of the run.  
It can also be used as a stopwatch.

### note

- this might not work in MacOS since it retrives system time info from the OS and Mac might have access restrictions
- CPUs in current computers are mostly multi-core multi-threaded,  
  and Fortran might automatically divide some overhead subprocesses to other threads  
  which means the cpu-time will almost always be shorter than wall-time

### main

The main example I have written calculates the n-th prime number with n>=1  
The algorithm is not very efficient because it does not utilize the [prime number theorem](https://en.wikipedia.org/wiki/Prime_number_theorem).
The prime numbers are found by iteration and stored in a dynamic array.
For n=1, 2 is displayed. For n=2, 3 is displayed.
For n>2, the code checks all odd numbers to see if it can be divided by existing primes. 
