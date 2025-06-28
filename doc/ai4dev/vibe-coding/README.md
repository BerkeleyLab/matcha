You are a research software engineer who is familiar with Fortran 2018, experienced in parallel and object-oriented programming, and working on a program named "diffusion". All source code that you need is in the attached file. The main program uses a second-order Runge-Kutta time advancement algorithm to solve the partial differential equation of unsteady molecular diffusion of a species with a density phi that varies with space and time. The program uses the subdomain_t derived type defined in the module subdomain_m.  Th subdomain_m module also contains a module function interface body named "laplacian".  Everything in the source code is complete except that there is no defintion for the "laplacian" function. Provide a defintion for the laplacian function definition using the following steps:

1. Write an empty module procecure named "laplacian" into which you will insert executable statements and comments that explain what your code is doing.
2. Allocate the laplacian_rhs% component to a shape [my_nx, ny, nz]
3. Use one or more "do concurrent" constructs, each with "default(none)" and any othe required locality specifiers, to compute a 2nd-order central difference approximation to the Laplacian of the "s_" array component of the "laplacian" function's "rhs" dummy argument.
4. Store the Laplacian approximation in the s_ component of the laplacian_rhs function result.
5. When an expresssion requires the value rhs%s_(1,:,:) and me==1, instead use halo_x(west,:,:) for the required value.
5. When an expresssion requires the value rhs%s_(ubound(rhs%s_,1),:,:) and me==num_subdomains, instead use halo_x(east,:,:) for the required value.
6. Set result component laplacian_rhs%s_ to zero for the subscript values (:, 1,:), (:,ny,:), (:,:, 1), (:,:,nz). 
7. Also set laplacian_rhs%s_ to zero for the subscript values (1,:,:) if me==1
8. Also set laplacian_rhs%s_ to zero for the subscript values (my_nx,:,:) if me==num_subdomains.
