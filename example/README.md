A Parallel 2D Unsteady Heat Equation Solver
===========================================

This subdirectory's [heat-equation.f90] file contains a parallel solver for the
unsteady heat equation written in Fortran 2018.  The numerical algorithm uses
2nd-order-accurate central, finite differencing in space and first-order-accurate
explicit Euler advancement in time.

Downloading, Building, and Running
----------------------------------
### Prerequisities
* The [GCC] Fortran compiler (`gfortran`)
* The [OpenCoarrays] compiler wrapper (`caf`) and program launcher (`cafrun`)
* The [Fortran Package Manager] (`fpm`)

### Parallel execution with GCC and OpenCoarrays
With the [GCC](https://gcc.gnu.org) Fortran compiler (`gfortran`) and the
[OpenCoarrays] parallel runtime library installed,
```
git clone https://github.com/BerkeleyLab/matcha
cd matcha
fpm run --example heat-equation --compiler caf --runner "cafrun -n 2"
```
where you can replace `2` in the above line with the desired number of 
images 

### Parallel execution with the Intel `ifx` compiler
With the Intel `ifx` Fortran compiler installed, replace the above
`fpm` command with
```
export FOR_COARRAY_NUM_IMAGES=2
fpm run --example heat-equation --compiler ifx --flag "-coarray"
```

### Serial execution with `gfortran` *without* requiring OpenCoarrays
With `gfortran` installed, replace the above `fpm` commands with
```
fpm run --example heat-equation
```

Exercise
-------
Try modifing the main program to use 2nd-order Runge-Kutta time advancement:
```
T_half = T + 0.5*dt*alpha* .laplacian. T
T = T + dt*alpha* .laplacian. T_half
```
You'll need to append `, real T_half` to the line `type(subdomain_2D_t) T`
declaration.

[heat-equation.f90]: ./heat-equation.f90
[GCC]: https://gcc.gnu.org
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[Fortran Package Manager]: https://github.com/fortran-lang/fpm
