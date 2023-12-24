! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
program time_paradigm_m
  !! Time various alternative programming paradigms
  use subdomain_m, only : subdomain_t
  use assert_m, only : assert
  use sourcery_m, only : string_t, file_t, command_line_t, bin_t, csv 
  use iso_fortran_env, only : int64
  implicit none

  real, parameter :: alpha=1., T_internal_initial=1., T_boundary=0., T_steady=T_boundary, tolerance = 1.E-03
  character(len=:), allocatable :: steps_string, resolution_string
  type(command_line_t) command_line
  integer(int64) counter_start, counter_end, clock_rate
  integer :: steps=200, resolution=64

  associate(me => this_image())
  if (command_line%argument_present(["--help"])) then
    print *, &
      new_line('a') // new_line('a') // &
      'Usage: fpm run --example time-paradigm -- [--steps <integer>] [--resolution <integer>]' // &
      new_line('a') // new_line('a') // &
      'where square brackets indicate optional arguments'
    stop
  end if

  steps_string = string_t(command_line%flag_value("--steps"))
  resolution_string = string_t(command_line%flag_value("--resolution"))
  if (len(steps_string)/=0) read(steps_string,*) steps
  if (len(resolution_string)/=0) read(resolution_string,*) resolution

  if (me==1) print *,"Number of steps to execute: ",steps
  if (me==1) print *,"Number of grid points in each coordinate direction: ",resolution

    if (me==1) print *,"Starting functional solver."
    associate(t_functional => functional_programming_time())
      if (me==1) print *,"Starting procedural solver."
      associate(t_procedural => functional_programming_time())
        if (me==1) then 
          print *,"Functional program time: ", t_functional
          print *,"Procedural program time: ", t_procedural
          print *,"Procedural speedup: ", (t_functional - t_procedural)/t_functional
        end if
      end associate
    end associate
  end associate
  
contains

  function functional_programming_time() result(system_time)
    integer(int64) t_start_functional, t_end_functional, clock_rate
    integer step
    real system_time
    type(subdomain_t) T

    call T%define(side=1., boundary_val=T_boundary, internal_val=T_internal_initial, n=resolution)

    call system_clock(t_start_functional)

    associate(dt => T%dx()*T%dy()/(4*alpha))
      functional_programming: &
      do step = 1, steps
        T =  T + dt * alpha * .laplacian. T
      end do functional_programming
    end associate

    call system_clock(t_end_functional, clock_rate)
    system_time = real(t_end_functional - t_start_functional)/real(clock_rate)

    associate(L_infinity_norm => maxval(abs(T%values() - T_steady)))
      call assert(L_infinity_norm < tolerance, "functional programming reaches steady state", L_infinity_norm)
    end associate

  end function

  function procedural_programming_time() result(system_time)
    integer(int64) t_start_procedural, t_end_procedural, clock_rate
    integer step
    real system_time
    type(subdomain_t) T

    associate(dt => T%dx()*T%dy()/(4*alpha))
      call T%define(side=1., boundary_val=0., internal_val=1., n=resolution)
      call system_clock(t_start_procedural)
      procedural_programming: &
      do step = 1, steps
        call T%step(alpha*dt)
      end do procedural_programming
    end associate

    call system_clock(t_end_procedural, clock_rate)
    system_time = real(t_end_procedural - t_start_procedural)/real(clock_rate)

    associate(L_infinity_norm => maxval(abs(T%values() - T_steady)))
      call assert(L_infinity_norm < tolerance, "procedurall programming reaches steady state", L_infinity_norm)
    end associate

  end function

  subroutine output(v)
    real, intent(in) :: v(:,:,:)
    integer j, k
    sync all
    critical
      do j = 1, size(v,2)
        do k = 1, size(v,3)
          print *,"image ",this_image(),": ",j,k,v(:,j,k)
        end do
      end do
    end critical
    sync all
  end subroutine

end program