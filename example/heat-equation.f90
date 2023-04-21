program main
  use subdomain_m, only : subdomain_t
  implicit none

  integer, parameter :: steps = 50
  real, parameter :: alpha = 0.25
  type(subdomain_t) T
  integer step

  call T%define(side=1., boundary_val=0., internal_val=1., n=11) ! internally constant subdomain with a step down at the edges

  associate(dx => T%dx(), dy => T%dy())
    associate(dt => 0.1 * alpha * dx * dy / 2)
      do step = 1, steps
        T =  T + dt * alpha * .laplacian. T
      end do
    end associate
  end associate

end program
