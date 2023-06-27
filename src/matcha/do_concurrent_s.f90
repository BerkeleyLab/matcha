submodule(do_concurrent_m) do_concurrent_s
  use iso_c_binding, only : c_f_pointer
  implicit none

contains

 module procedure do_concurrent_sample

    integer cell, step

    associate(ncells => size(rvalues,1), nsteps => size(rvalues,2))
      allocate(sampled_values(ncells,nsteps))
      do concurrent(cell = 1:ncells, step = 1:nsteps)
        associate(k => findloc(rvalues(cell,step) >= cumulative_distribution, value=.false., dim=1)-1)
          sampled_values(cell,step) = val(k)
        end associate
      end do
    end associate

  end procedure

  module procedure do_concurrent_sampled_speeds
  
    integer cell, step
    
    associate(ncells => size(speeds,1), nsteps => size(speeds,2))
      allocate(sampled_speeds(ncells,nsteps))
      do concurrent(cell = 1:ncells, step = 1:nsteps)
        associate(k => findloc(speeds(cell,step) >= cumulative_distribution, value=.false., dim=1)-1)
          sampled_speeds(cell,step) = vel(k)
        end associate
      end do
    end associate
    
  end procedure
  
  module procedure do_concurrent_my_velocities
  
    integer step
    
    if(allocated(my_velocities)) deallocate(my_velocities)
    allocate(my_velocities, mold=dir)
    
    do concurrent(step=1:nsteps)
      my_velocities(:,step,1) = sampled_speeds(:,step)*dir(:,step,1)
      my_velocities(:,step,2) = sampled_speeds(:,step)*dir(:,step,2)
      my_velocities(:,step,3) = sampled_speeds(:,step)*dir(:,step,3)
    end do
    
  end procedure
  
  module procedure do_concurrent_k
  
  integer i
  
    associate(nspeeds => size(speeds))
      if(allocated(k)) deallocate(k)
      allocate(k(nspeeds))
        do concurrent(i = 1:nspeeds)
          k(i) = findloc(speeds(i) >= vel, value=.false., dim=1)-1
        end do
    end associate
  end procedure
  
  module procedure do_concurrent_output_distribution
  
    integer i
    
    if(allocated(output_distribution)) deallocate(output_distribution)
    allocate(output_distribution(nintervals,2))
    output_distribution(:,freq) = 0.d0
    output_distribution(:,speed) = emp_distribution(:,speed)
    do concurrent(i = 1:size(output_distribution,1))
      output_distribution(i,freq) = count(k==i)
    end do
    
  end procedure
  
  module procedure do_concurrent_speeds
  
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
  
      associate(t => history%time)
        allocate(speeds(ncells*(npositions-1)))
        do concurrent(i = 1:npositions-1, j = 1:ncells)
          associate( &
            u => (x(i+1,j,:) - x(i,j,:))/(t(i+1) - t(i)), &
            ij => i + (j-1)*(npositions-1) &
           )   
            speeds(ij) = sqrt(sum([(u(k)**2, k=1,nspacedims)]))
          end associate
        end do
      end associate

    end associate
    
  end procedure

  module procedure do_concurrent_angles
  
    integer i, j, k
    integer, parameter :: nspacedims=3
    
    real(c_double), pointer :: positions(:,:)
    real(c_double), allocatable :: x(:,:,:)
    real(c_double) v1mag,v2mag,eps

    eps = 1.d-16
    associate(npositions => size(history), ncells => history(1)%positions_shape(1))

      allocate(x(npositions,ncells,nspacedims))

      do i=1,npositions
        call c_f_pointer(history(i)%positions_ptr, positions, history(1)%positions_shape)
        x(i,:,:) = positions
      end do

      allocate(angles(ncells*(npositions-2)) )      
      do concurrent(i = 1:npositions-2, j = 1:ncells)
        associate( &
          v1 => x(i+1,j,:) - x(i,j,:), &
          v2 => x(i+2,j,:) - x(i+1,j,:), &
          ij => i + (j-1)*(npositions-2) &
         )
          v1mag = sqrt(sum([(v1(k)**2, k=1,nspacedims)]))
          v2mag = sqrt(sum([(v2(k)**2, k=1,nspacedims)]))
          angles(ij) = acos( (v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3))/(v1mag*v2mag+eps) )
        end associate
      end do

    end associate
    
  end procedure
  
  
end submodule do_concurrent_s
