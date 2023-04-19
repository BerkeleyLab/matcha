program main
  use subdomain_m, only : subdomain_t
  implicit none

  integer, parameter :: nt = 50
  real, parameter :: sigma = 0.25, nu = 0.05
  type(subdomain_t) u

  call u%define(side=2., boundary_val=1., internal_val=2., n=31)

  associate(dx => u%dx(), dy => u%dy())
    associate(dt => sigma * dx * dy / nu)
      block
        integer i

        do i = 1, nt
          u =  u + dt * nu * .laplacian. u
        end do
      end block
    end associate
  end associate
end program
