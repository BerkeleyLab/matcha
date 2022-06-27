# Matcha
Motility Analysis of T Cell Histories in Activation
## Overview
Matcha tracks and simulates the activation of tcells going through lung tissue.
## Downloading, Building, and Running
###Prerequisites
  1. A Fortran 2018 compiler
  2. The Fortran Package Manager
  3. *Optional:* =OpenCoarrays= for parrallel execution with the GNU Fortran compiler

###Downloading, building, and Testing
**Downloading Matcha**
'
git clone https://github.com/rouson/matcha
cd matcha
'
**Building and testing: single-image (serial) execution**
The following command builds Matcha and runs the full test suite in a single image:
'fpm test'
**Building and testing: multi-image (parallel) execution**
With 'fpm', the GNU Fortran compiler (==gfortran==), and OpenCoarrays installed, build Matcha and run the program by executing the following command in a 'bash'-like shell:
'
fpm run --compiler caf --runner "cafrun -n 2"
'
Change '2' above to the number of images that you would like to launch in parallel.
## Documentation
Please visit the Matcha Github Pages site to see HTML Documentation generated with ford
## License
