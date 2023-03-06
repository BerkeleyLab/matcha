! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(matcha_m) matcha_s
  use t_cell_collection_m, only : t_cell_collection_t
  use distribution_m, only : distribution_t
  use data_partition_m, only : data_partition_t
  
#ifdef USE_CAFFEINE
   use caffeine_m, only : this_image => caf_this_image
#endif

  implicit none
  
contains

  module procedure matcha

    associate( &
      ncells => input%num_cells(), &
      npositions => input%num_positions(), &
      ndim => input%num_dimensions(), &
      nintervals => input%num_intervals(), &
      dt => input%time_step(), &
      empirical_distribution => input%sample_distribution() &
    )

      block
        double precision, parameter :: scale = 100.D0
        double precision, allocatable :: random_positions(:,:), random_4vectors(:,:,:), v(:,:,:), x(:,:)
        type(distribution_t) distribution
        integer, parameter :: nveldim = 4
        integer step
        type(data_partition_t) data_partition
        double precision t
        
        call data_partition%define_partitions(cardinality=ncells)
    
        associate(me => this_image())
          associate(my_num_cells => data_partition%last(me) - data_partition%first(me) + 1)
          
            call random_init(repeatable=.true., image_distinct=.true.)
            
            allocate(random_positions(my_num_cells,ndim))
            call random_number(random_positions)  
          
            associate(nsteps => npositions -1)
              allocate(random_4vectors(my_num_cells,nsteps,nveldim))
              call random_number(random_4vectors)  
              distribution = distribution_t(empirical_distribution)
          
              associate(random_speeds => random_4vectors(:,:,1), random_directions => random_4vectors(:,:,2:4))
              	v = distribution%velocities(random_speeds, random_directions)
                
		allocate(history(nsteps))
		history(1) = t_cell_collection_t(scale*random_positions, time=0.D0)
		do step = 2, nsteps
		    x = history(step-1)%positions() 
		    t = history(step-1)%time()
		    history(step) = t_cell_collection_t(x + v(:,step-1,:)*dt, t + dt)
                end do
              end associate
            end associate
          end associate  
        end associate    
      end block
    end associate

  end procedure

end submodule matcha_s
