submodule(do_concurrent_m) do_concurrent_s
  implicit none

contains
  
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
    double precision, allocatable :: dir(:,:,:)
  
    associate(nsteps => size(speeds,2))

       ! Create unit vectors
      dir = directions(:,1:nsteps,:)

      associate(dir_mag => sqrt(dir(:,:,1)**2 +dir(:,:,2)**2 + dir(:,:,3)**2))
        associate(dir_mag_ => merge(dir_mag, epsilon(dir_mag), dir_mag/=0.))
          dir(:,:,1) = dir(:,:,1)/dir_mag_
          dir(:,:,2) = dir(:,:,2)/dir_mag_
          dir(:,:,3) = dir(:,:,3)/dir_mag_
        end associate
      end associate

      allocate(my_velocities, mold=dir)
      
      do concurrent(step=1:nsteps)
        my_velocities(:,step,1) = sampled_speeds(:,step)*dir(:,step,1)
        my_velocities(:,step,2) = sampled_speeds(:,step)*dir(:,step,2)
        my_velocities(:,step,3) = sampled_speeds(:,step)*dir(:,step,3)
      end do
    end associate
    
  end procedure
  
  module procedure do_concurrent_k
  
  integer i
  
    associate(nspeeds => size(speeds))
      allocate(k(nspeeds))
        do concurrent(i = 1:nspeeds)
          k(i) = findloc(speeds(i) >= vel, value=.false., dim=1)-1
        end do
    end associate
  end procedure
  
  module procedure do_concurrent_x
  
    integer i
  
    allocate(x(npositions,ncells,nspacedims))
    do concurrent(i=1:npositions)
      x(i,:,:) = history(i)%positions()
    end do
    
  end procedure
  
  module procedure do_concurrent_speeds
  
    integer i, j, k
    
    associate(t => history%time())
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
    
  end procedure
  
  
  
end submodule do_concurrent_s
