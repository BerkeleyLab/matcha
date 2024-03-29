Matcha
======
Motility Analysis of T-Cell Histories in Activation

Overview
--------
Despite advances in microscopy, imaging techniques only capture a very small spatial and temporal fraction of T-cell activity within a tissue. For this reason, we have pursued the development of a virtual T-cell model. Matcha's objective is to design virtual T cells that move like biological T cells. The virtual T cells will match the speed and turning angle distributions of biological cells. In addition, Matcha will also attempt to match the volume patrolled by the biological cells and the dependence on speed and turning angle on the previous speed.


Prerequisites
-------------
1. A Fortran 2018 compiler (`gfortran` + [OpenCoarrays])
2. The Fortran Package Manager

Downloading, Building, and Testing
---------------------------------

### Single-image (serial) execution
With `gfortran` installed, download, build, run and test Matcha in a single image by entering the following commands in a `bash`-like shell:
```
git clone https://github.com/rouson/matcha
cd matcha
fpm test
```

### Multi-image (parallel) execution
With `gfortran` and OpenCoarrays installed, build, test, and run Matcha in multiple images by entering the following commands in a `bash`-like shell:
```
fpm test --compiler caf --runner "cafrun -n 2"
fpm run --compiler caf --runner "cafrun -n 2"
```
Change '2' above to the number of images that you would like to launch in parallel.

## Documentation
Please visit the [Matcha Github Pages](https://berkeleylab.github.io/matcha) site to see HTML Documentation generated with ford.

[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
