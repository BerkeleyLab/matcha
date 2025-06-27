! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

program diffusion
  !! Solve the partial differential equation governing unsteady 3D homogeneous, isotropic molecular diffusion
  !! using 2nd-order-accurate central differences in space and 2nd-order Runge-Kutta time advancement.
  use matcha_m, only : subdomain_t
  implicit none
  type(subdomain_t) phi
  integer, parameter :: steps = 1000
  integer step
  real, parameter :: D = 1.
  real dt

  call phi%define(side=1., boundary_val=0., internal_val=1., n=11) ! const. internally with a step down at boundaries

  associate(dt => phi%dt_stable(D))
    do step = 1, steps
      phi_half =  phi +  D * .laplacian. phi * (dt/2.)
      phi =  phi + D * .laplacian. phi_half * dt
    end do
  end associate

end program