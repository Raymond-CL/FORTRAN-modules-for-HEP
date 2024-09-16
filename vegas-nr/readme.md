## VEGAS 

### description
VEGAS: adaptive multi-dimensional Monte Carlo integration  
derived from numerical recipes  
please cite NR if you are using their code:  
W. H. Press, S. A. Teukolsky, W. T. Vetterling and B. P. Flannery,  
Numerical Recipes in FORTRAN: The Art of Scientific Computing

### note
this module is no different than the code provided by NR  
the usage in `main.f90` is also employed from the examples provided in NR  
need to use compiler flag `-fno-strict-overflow`

### dependencies:

    nrtype.o :
    nr.o : nrtype.o
    nrutil.o : nrtype.o
    ran_state.o : nrtype.o nrutil.o
    ran1.o : nrtype.o ran_state.o
    vegas.o : nrtype.o nr.o
    main.o : nrtype.o nr.o

Therefore, one should compile with the following:  
`gfortran -c -fno-strict-overflow nrtype.f90 nr.f90 nrutil.f90 ran_state.f90 ran1.f90 vegas.f90 main.f90`  
`gfortran -o vegas.exe *.o`

### main
The main program calculates the volume of a n-sphere using VEGAS

