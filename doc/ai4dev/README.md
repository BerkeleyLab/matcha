Set Up
-----------
With your present working directory set to `doc/ai4dev`, execute
```bash
cd  ../../scripts/
./create-single-source-file-programs.sh 
cd ../build/single-file-programs
```

Vibe Coding
-----------
1. Compile and execute `diffusion-app.F90` to ensure that the program
   terminates normally.  For example, with the `gfortran` compiler,
   run `gfortran -fcoarray=single -o diffusion-app diffusion-app.F90` 
   followed by `./diffusion-app`.
2. Edit `diffusion-app.F90` to remove the lines beginning and ending with 
   `module procedure laplacian` and `end procedure laplacian`, respectively.
3. Enter the file `doc/ai4dev/vibe-coding/README.md` as your prompt and
   attach the edited `diffusion-app.F90` file. You might need to rename
   the file to something like `diffusion-app.txt` for the file type to be
   acccepted.
4. Insert the resulting function into the `subdomain_s` submodule and
   try compiling and executing again to check the result.

Idiomtic Vibe Coding
--------------------
1. Compile and execute `test-suite.F90` to and check that the output 
   reports that all tests pass.  For example, with the `gfortran` compiler,
   run `gfortran -fcoarray=single -o test-suite test-suite.F90` followed
   by `./test-suite`.
2. Edit `test-suite.F90` to remove the lines beginning and ending with 
   `module procedure laplacian` and `end procedure laplacian`, respectively.
3. Enter the file `doc/ai4dev/idiomaic-vibe-coding/README.md` as your prompt 
   and attach the edited `test-suite.F90` file.  You might need to rename
   the file to something like `test-suite.txt` for the file type to be
   acccepted.
4. Insert the resulting function into the `subdomain_s` submodule and
   try compiling and executing again to check the result.
