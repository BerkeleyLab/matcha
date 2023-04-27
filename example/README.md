A Parallel 2D Unsteady Heat Equation Solver
===========================================

This subdirectory's [heat-equation.f90] file contains a parallel solver for the
unsteady 2D heat equation written in Fortran 2018.  The numerical algorithm uses
2nd-order-accurate central finite differencing in space and 1st-order-accurate
explicit Euler advancement in time.

Downloading, Building, and Running
----------------------------------
### Prerequisites
* The [GCC], NAG, HPE, or Intel Fortran 2018 compilers.
* _Only if using [GCC]_: The [OpenCoarrays] compiler wrapper (`caf`) and program launcher (`cafrun`)

### Parallel execution with GCC and OpenCoarrays
With the [GCC](https://gcc.gnu.org) Fortran compiler (`gfortran`) and the
[OpenCoarrays] parallel runtime library installed,
```
git clone https://github.com/BerkeleyLab/matcha
cd matcha/example
caf -o heat heat-conduction.f90
cafrun -n 2 ./heat
```
where you can replace `2` in the above line with the desired number of 
images.

### Parallel execution with the Intel `ifx` compiler
With the Intel `ifx` Fortran compiler installed, 
```
ifx -o heat -coarray heat-equation.f90 
export FOR_COARRAY_NUM_IMAGES=2
./heat
```

### Serial execution with `gfortran` *without* requiring OpenCoarrays
With `gfortran` installed, replace the above `fpm` commands with
```
gfortran -o heat -fcoarray=single heat-equation.f90
./a.out
```

Exercise
--------
In addition to demonstrating parallel features of Fortran 2018, this example
shows an object-oriented, functional programming style based on Fortran's
user-defined operators such as the `.laplacian.` operator defined in this
example.  To demonstrate the expressive power and flexibility of this
approach, try modifying the modifying the main program to use 2nd-order
Runge-Kutta time advancement:
```
T_half = T + 0.5*dt*alpha* .laplacian. T
call T%exchange_halo
sync all
T = T + dt*alpha* .laplacian. T_half
call T%exchange_halo
sync all
```
You'll need to append `, T_half` to the declaration `type(subdomain_2D_t) T`.
With some care, you could modify the main program to use any desired order of
Runge-Kutta algorithm without changing any of the supporting code.

This example also demonstrates a benefit of Fortran's facility for declaring a
procedure to be `pure`: the semantics of `pure` procedures essentially
guarantees that the above right-hand-side expressions can be evaluated fully
asynchronously across all images.  No operator can modify state that would be
observable by another operator other than via the first operator's result. This
would be true even if an operator executing on one image performs communication
to _get_ data from another image via a coarray.  To reduce communication waiting
times, however, each image in our example proactively _puts_ data onto
neighboring images.  Puts generally outperform gets because the data can be
shipped off as soon the data are ready.  With the exception of one coarray
allocation in the `define` procedure, all procedures are asynchronous and all
image control is exposed in the main program.

[heat-equation.f90]: ./heat-equation.f90
[GCC]: https://gcc.gnu.org
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[Fortran Package Manager]: https://github.com/fortran-lang/fpm
