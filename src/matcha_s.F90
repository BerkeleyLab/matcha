submodule(matcha_m) matcha_s
  use distribution_m, only : distribution_t
  use sourcery_m, only : data_partition_t
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
        type(data_partition_t) data_partition
        
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
                associate(v => distribution%velocities(random_speeds, random_directions))
                  run_completed = .true.
                end associate
              end associate
            end associate
          end associate  
        end associate    
      end block
    end associate

  end procedure

end submodule matcha_s
