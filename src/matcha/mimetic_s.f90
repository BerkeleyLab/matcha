! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(mimetic_m) mimetic_s
  implicit none
 
contains
 
    module procedure divergence
      call divergence_rhs%subdomain_t%define(side=1., boundary_val=0., internal_val=0., n=21)
    end procedure
    
    module procedure gradient
      integer i
      allocate(gradient_rhs(3))
      call gradient_rhs%subdomain_t%define(side=1., boundary_val=0., internal_val=0., n=21)
    end procedure

    module procedure setOrder
      self%mimetic_k_ = my_k
    end procedure

    module procedure mimetic_k
      my_k = self%mimetic_k_
    end procedure

end submodule mimetic_s
