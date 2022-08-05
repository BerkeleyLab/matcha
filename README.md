Matcha
======
Motility Analysis of T-Cell Histories in Activation

Overview
--------
Despite advances in microscopy, imaging techniques only capture a very small spatial and temporal fraction of T-cell activity within a tissue. For this reason, we have pursued the development of a virtual T-cell model. Matcha's objective is to design virtual T cells that move like biological T cells. The virtual T cells will match the speed and turning angle distributions of biological cells. In addition, Matcha will also attempt to match the volume patrolled by the biological cells and the dependence on speed and turning angle on the previous speed.


Prerequisites
-------------
1. A Fortran 2018 compiler
2. The Fortran Package Manager
3. *Optional:* a parallel runtime library for multi-image execution:
    - [OpenCoarrays] or
    - [Caffeine].

Downloading
-----------
```
git clone https://github.com/rouson/matcha
cd matcha
```
Building
--------
With `gfortran` build Matcha in a single image by entering the following commands in a `bash`-like shell:
```
./install.sh
```
or execute `.install.sh -h` to see a list of options the installer accepts.

Running and Testing
-------------------
### Single-image (serial) execution
With `gfortran` installed, build, run and test Matcha in a single image by entering the following commands in a `bash`-like shell:
```
./install.sh
./build/run-fpm.sh run
./build/run-fpm.sh test
```
### Multi-image (parallel) execution
With `gfortran` and OpenCoarrays installed, build, run and test Matcha in multiple images by entering the following commands in a `bash`-like shell:
```
./install.sh
./build/run-fpm.sh run --compiler caf --runner "cafrun -n 2"
./build/run-fpm.sh test --compiler caf --runner "cafrun -n 2"
```
Change '2' above to the number of images that you would like to launch in parallel.

### Parallel Execution with Caffeine
```
./install.sh
GASNET_PSHM_NODES=2 ./build/run-fpm.sh run --flag "-DUSE_CAFFEINE"
GASNET_PSHM_NODES=2 ./build/run-fpm.sh test --flag "-DUSE_CAFFEINE"
```
Change '2' above to the number of images that you would like to launch in parallel.

## Documentation
Please visit the Matcha Github Pages site to see HTML Documentation generated with ford.

[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[Caffeine]: https://go.lbl.gov/caffeine
