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

because the integrand function `fxn` will need to declared before compiling vegas.
There are two ways of writing the vegas module:
1. declare an extra module that act as a global scope that contains a set precision
  this is what was done in NR using `nrtype.f90`
  
2. use an implicit system defined precision such as `real` or `double precision`
  this is what we have done in this module
