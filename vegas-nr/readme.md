## VEGAS 

### description

VEGAS: adaptive multi-dimensional Monte Carlo integration  
derived from [Numerical Recipes](https://numerical.recipes/)  
please give credit to NR if you are using their code:  
W. H. Press, S. A. Teukolsky, W. T. Vetterling and B. P. Flannery,  
*Numerical Recipes in FORTRAN: The Art of Scientific Computing*

### note

- this module is no different than the codes provided by NR  
- the usage in `main.f90` is also employed from the examples provided in NR  
- need to use compiler flag `-fno-strict-overflow`  
- currently, vegas (NR) can perform integration of up to 10 dimensions
- this can be modified by increasing `MXDIM` in `vegas.f90`
- correspondingly, we increase the dimension for `region` in `main.f90` to set the limits

### dependencies:

    nrtype.o :
    nr.o : nrtype.o
    nrutil.o : nrtype.o
    ran_state.o : nrtype.o nrutil.o
    ran1.o : nrtype.o ran_state.o
    vegas.o : nrtype.o nr.o
    main.o : nrtype.o nr.o

Therefore, one should compile with the following:

```
gfortran -c -fno-strict-overflow nrtype.f90 nr.f90 nrutil.f90 ran_state.f90 ran1.f90 vegas.f90 main.f90
```
```
gfortran -o vegas.exe *.o
```

### main

The main example program that I have written calculates the volume of a n-sphere using acceptance/rejection method  
- `ndim` sets the dimension $n$
- `rad` sets the radius $R$
- `ncall` sets the (approximate) number of calls/points for each iteration
- `itmax` sets the number of iterations
- `init=-1` runs vegas with low `ncall` and high `itmax` to let vegas adapt to the integrand function
- `init=+1` then runs vegas with high `ncall` and low `itmax` to achieve high precision
- the calculation takes into account the symmetry in cartesian coordinates and integrates from $[0,R]$, a final symmetry factor is multiplied

The actual volume is 

```math
\frac{\pi^{n/2}}{\Gamma(\frac{n}{2}+1)}~R^{n}
```
