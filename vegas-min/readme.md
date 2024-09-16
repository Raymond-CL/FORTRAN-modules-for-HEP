## VEGAS-min

### description

VEGAS: adaptive multi-dimensional Monte Carlo integration  
minimized from [Numerical Recipes](https://numerical.recipes/)  
to a single module file `vint.f90`  
please give credit to NR if you are using their code:  
W. H. Press, S. A. Teukolsky, W. T. Vetterling and B. P. Flannery,  
*Numerical Recipes in FORTRAN: The Art of Scientific Computing*

### note 

- this module is similar to `vegas-nr` in the same repository
- the example usage is also similar except for some minor changes
- need to use compiler flag `-fno-strict-overflow`  

The integrand function `fxn` need to be declared before compiling `vegas`.
Since it will be called by `vegas` with its inputs supplied be `vegas`, and the return value will also be used by `vegas`.
This means that `fxn` can be considered an internal function of the `vegas` subroutine.
A predefined `vegas` module will restrict some of the freedoms of `fxn`.

There are two ways of writing the vegas module:  
1. with a flexible variable precision  
This requires an extra module used both by `vegas` and `fxn` that act as a global scope which contains a set working-precision.
this is what was done in NR using `nrtype.f90`.
2. with a fixed predefined precision  
The makes the `vegas` module more standalone and fixes the `fxn` precision to be the system-defined `real` or `double precision`.  
this is what we have done in this module where the working-precision is `double precision`

If you wish to change everything to single precision, follow the steps:
1. change `wp` in `vint.f90` to `real32`
2. swap all `double precision` to `real` in both `vint.f90` and `mainf90`

compile with the following:
```
gfortran -c -fno-strict-overflow vint.f90 main.f90
```
```
gfortran -o vint.exe *.o
```

### main

The main example program is similar to the one in `vegas-nr` that calculates the volume of a n-sphere.
note that we no longer need to define `fxn` interface in main because it is defined in the `vint` module.
