Matcha
======
Motility Analysis of T-Cell Histories in Activation

Overview
--------
Despite advances in microscopy, imaging techniques only capture a very small spatial and temporal fraction of T-cell activity within a tissue. For this reason, we have pursued the development of a virtual T-cell model. Matcha's objective is to design virtual T cells that move like biological T cells. The virtual T cells will match the speed and turning angle distributions of biological cells. In addition, Matcha will also attempt to match the volume patrolled by the biological cells and the dependence on speed and turning angle on the previous speed.


Prerequisites
-------------
1. A Fortran 2018 compiler with multi-image/coarray support (e.g. `gfortran` + [OpenCoarrays])
2. The Fortran Package Manager

Downloading, Building, and Testing
---------------------------------

### GNU Compiler Collection (GCC) `gfortran` Compiler

#### Single-image (serial) execution
With `gfortran` installed, download, build, run and test Matcha in a single image by entering the following commands in a `bash`-like shell:
```
git clone https://github.com/rouson/matcha
cd matcha
fpm test
```

#### Multi-image (parallel) execution
With `gfortran` and OpenCoarrays installed, build, test, and run Matcha in multiple images by entering the following commands in a `bash`-like shell:
```
fpm test --compiler caf --runner "cafrun -n 2"
fpm run --compiler caf --runner "cafrun -n 2"
```
Change '2' above to the number of images that you would like to launch in parallel.

### Intel OneAPI `ifx` Compiler
#### Multi-image (parallel) execution
The unit test suite uses small grids to minimize runtime, which limits the scalability of the test suite only.
Hence, the test suite is _not_ designed for execution in a large number of images.

All tests pass when running the test suite in 1-4 images:
```
export FOR_COARRAY_NUM_IMAGES=4
fpm test --compiler ifx --profile release --flag "-heap-arrays -coarray"
```

Note this requires a working install of Intel MPI.

#### Single-image (serial) execution
Same as multi-image execution except `FOR_COARRAY_NUM_IMAGES=1`.
Also requires a working install of Intel MPI.

#### Automatic GPU-based acceleration: _Experimental_
The following command is listed here for reference while we investigate what appear to be platform-specific compiler issues causing a crashe when automatic GPU-based acceleration is enabled with the required coarray features:
```
fpm test \
  --compiler ifx \
  --profile release \
  --flag "-heap-arrays -coarray -fopenmp-target-do-concurrent -qopenmp -fopenmp-targets=spir64"
```

## Documentation
Please visit the [Matcha Github Pages](https://berkeleylab.github.io/matcha) site to see HTML Documentation generated with ford.

[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
