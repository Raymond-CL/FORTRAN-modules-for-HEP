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
- `vegas-nr`

  VEGAS multi-dimensional Monte Carlo integration subroutine developed by "Numerical Recipe"
  
- `vegas-min`

  a mininum VEGAS module based on `vegas-nr` that is self-contained in a single module file

- `glauber-opt`
- `glauber-mc`
- `time`
- `vec2D`
- `VISH-wc`
