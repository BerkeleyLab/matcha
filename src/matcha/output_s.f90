! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(output_m) output_s
  use do_concurrent_m, only : do_concurrent_k, do_concurrent_output_distribution,&
  do_concurrent_x, do_concurrent_speeds
  implicit none
  
contains

  module procedure construct
    output%input_ = input
    output%history_ = history
  end procedure
  
  module procedure my_num_cells
    num_cells = size(self%history_(1)%positions(), 1)
  end procedure

  module procedure simulated_distribution
    integer i
    integer, allocatable :: k(:)
    double precision, allocatable :: vel(:)
    
    integer, parameter :: speed=1, freq=2 ! subscripts for speeds and frequencies

    associate(speeds => sim_speeds(self%history_))
      associate(emp_distribution => self%input_%sample_distribution())
        associate(nintervals => size(emp_distribution(:,1)))
          allocate(output_distribution(nintervals,2))
          output_distribution(:,freq) = 0.d0
          output_distribution(:,speed) = emp_distribution(:,speed)
          associate(dvel_half => (emp_distribution(2,speed)-emp_distribution(1,speed))/2.d0)
            vel = [emp_distribution(1,speed) - dvel_half, [(emp_distribution(i,speed) + dvel_half, i=1,nintervals)]]
            k = do_concurrent_k(speeds, vel)
            output_distribution = do_concurrent_output_distribution(k)
            output_distribution(:,freq) = output_distribution(:,freq)/sum(output_distribution(:,freq))
          end associate
        end associate
      end associate
    end associate

  contains

    pure function sim_speeds(history) result(speeds)
      type(t_cell_collection_t), intent(in) :: history(:)
      double precision, allocatable :: speeds(:)

      integer, parameter :: nspacedims=3
      integer i, j, k
      double precision, allocatable :: x(:,:,:)
  
      associate( &
        npositions => size(history,1), &
        ncells => size(history(1)%positions(),1) &
      )   
        allocate(x(npositions,ncells,nspacedims))
        x = do_concurrent_x(npositions, ncells, nspacedims, history)
        speeds = do_concurrent_speeds(ncells, npositions, nspacedims, x, history)
      end associate
    end function

  end procedure

end submodule output_s
