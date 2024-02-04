submodule(do_concurrent_m) do_concurrent_s
  use assert_m, only : assert
  use iso_c_binding, only : c_f_pointer
  implicit none

contains
  
  pure module subroutine do_concurrent_sampled_speeds(speeds, vel, cumulative_distribution, sampled_speeds) bind(C)
    real(c_double), intent(in) :: speeds(:,:), vel(:), cumulative_distribution(:)
    real(c_double), intent(out) :: sampled_speeds(:,:)
    integer cell, step
   
    call assert(all(shape(sampled_speeds)==shape(speeds)), "do_concurrent_sampled_speeds: {sampled_,}speeds shape match")
    
    associate(ncells => size(speeds,1), nsteps => size(speeds,2))
      do concurrent(cell = 1:ncells, step = 1:nsteps)
        associate(k => findloc(speeds(cell,step) >= cumulative_distribution, value=.false., dim=1)-1)
          sampled_speeds(cell,step) = vel(k)
        end associate
      end do
    end associate
    
  end subroutine  

  pure module subroutine do_concurrent_my_velocities(nsteps, dir, sampled_speeds, my_velocities) bind(C)
    integer(c_int), intent(in) :: nsteps
    real(c_double), intent(in) :: dir(:,:,:), sampled_speeds(:,:)
    real(c_double), intent(out) :: my_velocities(:,:,:)
    integer step
    
    call assert(all([size(my_velocities,1),size(sampled_speeds,2)] == shape(sampled_speeds)), &
      "do_concurrent_my_velocities: argument size match")
    call assert(all(size(my_velocities,1)==shape(dir)), "do_concurrent_my_velocities: argument shape match")

    do concurrent(step=1:nsteps)
      my_velocities(:,step,1) = sampled_speeds(:,step)*dir(:,step,1)
      my_velocities(:,step,2) = sampled_speeds(:,step)*dir(:,step,2)
      my_velocities(:,step,3) = sampled_speeds(:,step)*dir(:,step,3)
    end do
  end subroutine  
  
  pure module subroutine do_concurrent_k(speeds, vel, k) bind(C)
    real(c_double), intent(in) :: speeds(:), vel(:)
    integer(c_int), intent(out) :: k(:)  
    integer i
  
    associate(nspeeds => size(speeds))
        do concurrent(i = 1:nspeeds)
          k(i) = findloc(speeds(i) >= vel, value=.false., dim=1)-1
        end do
    end associate
  end subroutine
  
  pure module subroutine do_concurrent_output_distribution(speed, freq, emp_distribution, k, output_distribution) bind(C)
    integer(c_int), intent(in) :: speed, freq, k(:)
    real(c_double), intent(in) :: emp_distribution(:,:)
    real(c_double), intent(out) :: output_distribution(:,:)
    integer i
    
    output_distribution(:,freq) = 0.d0
    output_distribution(:,speed) = emp_distribution(:,speed)
    do concurrent(i = 1:size(output_distribution,1))
      output_distribution(i,freq) = count(k==i)
    end do
  end subroutine
  
  module subroutine do_concurrent_speeds(history, speeds) bind(C)
    type(t_cell_collection_bind_C_t), intent(in) :: history(:)
    real(c_double), intent(out) :: speeds(:)  
    integer i, j, k
    integer, parameter :: nspacedims=3
    
    real(c_double), pointer :: positions(:,:)
    real(c_double), allocatable :: x(:,:,:)
  
    associate(npositions => size(history), ncells => history(1)%positions_shape(1))

      allocate(x(npositions,ncells,nspacedims))

      do i=1,npositions
         call c_f_pointer(history(i)%positions_ptr, positions, history(1)%positions_shape)
         x(i,:,:) = positions
      end do
  
      do concurrent(i = 1:npositions-1, j = 1:ncells)
        associate( &
          u => (x(i+1,j,:) - x(i,j,:))/(history(i+1)%time - history(i)%time), &
          ij => i + (j-1)*(npositions-1) &
         )   
          speeds(ij) = sqrt(sum([(u(k)**2, k=1,nspacedims)]))
        end associate
      end do
    end associate
    
  end subroutine
  
end submodule do_concurrent_s