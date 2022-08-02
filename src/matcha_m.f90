! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module matcha_m 
  use t_cell_collection_m, only : t_cell_collection_t
  use distribution_m, only : distribution_t
  use input_m, only : input_t
  use output_m, only : output_t
  use data_partition_m, only : data_partition_t
  implicit none

  interface matcha

    module function matcha(input) result(history)
      implicit none
      type(input_t), intent(in) :: input
      type(t_cell_collection_t), allocatable :: history(:)
    end function

  end interface
  
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
                  history = [t_cell_collection_t(scale*random_positions, time=0.D0)]
                  do step = 1, nsteps
                    associate(x => history(step)%positions(), t => history(step)%time())
                      history = [history, t_cell_collection_t(x + v(:,step,:)*dt, t + dt)]
                    end associate
                  end do
                end associate
              end associate
            end associate
          end associate  
        end associate    
      end block
    end associate

  end procedure

end module
