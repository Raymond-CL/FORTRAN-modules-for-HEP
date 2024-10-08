## FORTRAN modules for HEP

### description

Fortran modules for High Energy Physics  
Some fun and useful modules I have written that are commonly used in [HEP](https://inspirehep.net/).  
The modules are separated in folders, written using [Fortran 90](https://fortranwiki.org/fortran/show/Fortran+90) standard, most of them with corresponding main program.

### installation

If you haven't already got Fortran setup on your PC, please read through the setup guide in the [documentation](https://fortran-lang.org/learn/os_setup/).  
If you have fortran [compiler](https://fortranwiki.org/fortran/show/Compilers) setup, just create a folder and copy the module file `<module_name>.f90` and main program `main.f90` into it to start playing.

### usage

copy the module file `<module_name>.f90` into the same directory as your main program `main.f90`.  
It can then be compiled with the following code (assuming `gfortran` is your favourite compiler):  
```
gfortran -c <module_name>.f90 main.f90
```
```
gfortran -o <my_program> *.o
```
*note that some modules might need extra compiler flags in order for it to work.*

### dependencies

note that module dependencies affect the order of compilation.  
One can keep track of dependency with a `makefile` using `make`.  
Or handle dependency using `makedepf90` for linux based OS.  
Or simply use `cmake`.  
The following [Link](https://fortranwiki.org/fortran/show/Build+tools) provides some info on build tools.

---

### modules

Listed below are some common modules that are useful in high energy physics calculations: 

- `color`

  a simple module that allow colored output to the terminal

- `gauss`

  a standalone Gauss-Legendre quadrature subroutine with 4, 8, 16, 32, and 64 points.
 
- `glauber_mc`

  Monte Carlo Glauber subroutine

- `glauber_opt`

  optical Glauber subroutine

- `time`

  a module with subroutines that can keep track of program start/stop time and run durations
  
- `vec_2D`

  a 2D vector class with vector opeartion methods

- `vegas_nr`

  VEGAS multi-dimensional Monte Carlo integration subroutine developed by "Numerical Recipe"
  
- `vint`

  a mininum VEGAS module based on `vegas-nr` that is self-contained in a single module file

- `vish_gen`

  generates a 3d table of parton quenching paths using Viscous Israel-Stewart Hydrodynamics

- `vish_read`

  reads the 3d table generated by `vish_gen` and provides an interpolated value
