Matcha
======
Motility Analysis of T Cell Histories in Activation

Overview
--------
Despite advances in microscopy, imaging techniques only capture a very small spatial and temporal fraction of T cell activity within a tissue. For this reason, we have pursued the development of a virtual T cell model. Matcha's objective is to design virtual T cells that move like biological T cells. The virtual T cells will match the speed and turning angle distributions of biological cells. In addition, Matcha will also attempt to match the volume patrolled by the biological cells and the dependence on speed and turning angle on the previous speed.


Prerequisites
-------------
  1. A Fortran 2018 compiler
  2. The Fortran Package Manager
  3. *Optional:* [OpenCoarrays] for parrallel execution with the GNU Fortran compiler
Downloading
-----------
```
git clone https://github.com/rouson/matcha
cd matcha
```
Building and Testing
--------------------
### Single-image (serial) execution
With `gfortran` installed, build, run and test Matcha in a single image by entering the following commands in a `bash`-like shell:
```
fpm run
fpm test
```
### Multi-image (parallel) execution
With `gfortran` and OpenCoarrays installed, build, run and test Matcha in multiple images by entering the following commands in a `bash`-like shell:
```
fpm run --compiler caf --runner "cafrun -n 2
fpm test --compiler caf --runner "cafrun -n 2
```
Change '2' above to the number of images that you would like to launch in parallel.

## Documentation
Please visit the Matcha Github Pages site to see HTML Documentation generated with ford.

