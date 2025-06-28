You are a research software engineer who is familiar with Fortran 2018 and
experienced in parallel and object-oriented programming.

1. Write a separate module procedure corresponding to a pure module function
   interface body with the function name "laplacian", a polymorphic dummy
   argument "rhs" with the declared type "subdomain_t", and a result
   "laplacian_rhs" of type "subdomain_t".
2. The "subdomain_t" type has a real, allocatable 3D array component named "s_".
3. Use intrinsic functions and array statements for compactness.  For example,
   replace "if" constructs with "merge" where possible.
4. Your procedure must define a result with values that correspond to a
   2nd-order accurate, central-difference approximation to the 3D form of the
   Laplacian differential operator.
5. Inside the procedure,
   a. Allocate the laplacian_rhs%s_ to have a shape [my_nx, ny, nz].
   b. Use one or more "do concurrent" constructs, each with "default(none)" and
      any other required locality specifiers.
   c. If an expresssion requires any s_ elements for which the first subscript
      equals lbound(rhs%s_,1) and if the integer "me" is 1, replace the elements
      with the corresponding elements of halo_x(west,:,:).
   d. If an expresssion requires any s_ elements for which the first subscript
      equals ubound(rhs%s_,1) and if the integer "me" equals "num_subdomains",
      replace the elements with the corresponding elements of halo_x(east,:,:).
   e. Your result component must vanish for subscript values (:, 1,:), (:,ny,:),
      (:,:, 1), and s_(:,:,nz). 
   f. If "me" is 1, the result component must vanish for subscripts (1,:,:).
   g. If "me" equals "num_subdomains", the result component must for subscripts
      (my_nx,:,:).
