## gauss

### description

Performs [Gauss-Legendre quadrature](https://en.wikipedia.org/wiki/Gaussian_quadrature) 1D integration.  
The data used in the quadrature is extracted from this [website](https://pomax.github.io/bezierinfo/legendre-gauss.html), please give them credit.  
The Gauss-Legendre (GL) integration is commonly found in many Fortran libraries such as [stdlib](https://stdlib.fortran-lang.org/sourcefile/stdlib_quadrature_gauss.f90.html) and [Numerical Recipes](https://numerical.recipes/book.html).  
This module is an simplified version that is standalone and does not depend on external libraries (although it does use a Fortran intrinsic to set working precision).  
The number of Gaussian points are 4, 8, 16, 32, and 64.

### usage

For a definite integral of a function $f(x)$ in the range $x\in[a,b]$, the Gaussian quadrature approximates the integral by the following:
```math
\int_a^b~f(x)~dx \approx \sum_i^n~f(x_i)*w_i
```
where the *abscissae* $x_i$ and their corresponding *weights* $w_i$ are pre-determined in the module depending on the number of Gaussian points $n$.
We first call the GL quadrature to get the abscissae and weights by providing the information of the integration range, assuming we use `n=64` points with the arrays `xi(1:n)` and `wi(1:n)` as output.
```
call GL64(a,b,xi,wi)
```
we then perform the integral by summing the product of function calls `f(x)` and their corresponding weights.
```
result = 0.0
do i = 1, n
  result = result + f( xi(i) ) * wi(i)
enddo
```


### dependencies

This module is standalone

### example

The example in the main program performs the following integral which gives an analytic solution
```math
\int_{x_{\min}}^{x_{\max}}~J_0(x)*J_1(x)~dx = \frac{1}{2}\left(J_0(x_\min)^2-J_0(x_\max)^2\right)
```
The program performs GL integration using 4, 8, 16, 32, 64 points to test for accuracy.
Note that the integrand function is oscillatory, so the number of points used should be twice larger than the number of oscillations in the range $[a,b]$.
