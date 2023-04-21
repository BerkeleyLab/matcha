program main
  use subdomain_m, only : subdomain_t
  implicit none

  type(subdomain_t) T
  real, parameter :: T_initial = 2., T_boundary = 1.
  integer, parameter :: nx = 51, ny = nx

  call T%define(side=1., boundary_val=T_boundary, internal_val=T_initial, n=nx) 
    ! spatially constant internal temperatuers with a step change at the boundaries

  block
    integer, parameter :: steps = 5000
    real, parameter :: alpha = 1.
    integer step
    real t_start, t_finish
    associate(dt => T%dx()*T%dy()/(4*alpha))
      call cpu_time(t_start)
      do step = 1, steps
        T =  T + dt * alpha * .laplacian. T
      end do
      call cpu_time(t_finish)
      print *, "cpu_time: ", t_finish - t_start
    end associate
    associate(T_values => T%values())
      print *,"T_initial, T_boundary, T_min, T_max: ", T_initial, T_boundary, minval(T_values(:,2:ny-1)), maxval(T_values(:,2:ny-1))
    end associate
  end block

end program
