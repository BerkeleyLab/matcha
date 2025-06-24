! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(matcha_m) matcha_s
  use t_cell_collection_m, only : t_cell_collection_t
  use distribution_m, only : distribution_t
  use julienne_m, only : bin_t
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
        double precision, allocatable :: random_positions(:,:), random_4vectors(:,:,:)
        type(distribution_t) distribution
        integer, parameter :: nveldim = 4
        integer step
        
        associate(me => this_image())
          associate(bin => bin_t(num_items=ncells, num_bins=num_images(), bin_number=me))
          associate(my_num_cells => bin%last() - bin%first() + 1)
          
            call random_init(repeatable=.true., image_distinct=.true.)
            
            allocate(random_positions(my_num_cells,ndim))
            call random_number(random_positions)  
          
            associate(nsteps => npositions -1)
              allocate(random_4vectors(my_num_cells,nsteps,nveldim))
              call random_number(random_4vectors)  
              distribution = distribution_t(empirical_distribution)
          
              associate(random_speeds => random_4vectors(:,:,1), random_directions => random_4vectors(:,:,2:4))
                associate(v => distribution%velocities(random_speeds, random_directions))
                  allocate(history(nsteps))
                  history(1) = t_cell_collection_t(scale*random_positions, time=0.D0)
                  do step = 2, nsteps
                    associate(x => history(step-1)%positions(), t => history(step-1)%time())
                      history(step) = t_cell_collection_t(x + v(:,step-1,:)*dt, t + dt)
                    end associate
                  end do
                end associate
              end associate
            end associate
          end associate  
          end associate  
        end associate    
      end block
    end associate

  end procedure

end submodule matcha_s
