! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(output_m) output_s
  use do_concurrent_m, only : do_concurrent_k, do_concurrent_output_distribution, &
    do_concurrent_x, do_concurrent_speeds
  use t_cell_collection_m, only : t_cell_collection_bind_C_t
  use iso_c_binding, only : c_loc 
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

    associate(speeds => sim_speeds(self%history_), emp_distribution => self%input_%sample_distribution())
      associate(nintervals => size(emp_distribution(:,1)), dvel_half => (emp_distribution(2,speed)-emp_distribution(1,speed))/2.d0)
        vel = [emp_distribution(1,speed) - dvel_half, [(emp_distribution(i,speed) + dvel_half, i=1,nintervals)]]
        call do_concurrent_k(speeds, vel, k)
        call do_concurrent_output_distribution(nintervals, speed, freq, emp_distribution, k, output_distribution)
        output_distribution(:,freq) = output_distribution(:,freq)/sum(output_distribution(:,freq))
      end associate
    end associate

  contains

    function sim_speeds(history) result(speeds)
      type(t_cell_collection_t), intent(in) :: history(:)
      double precision, allocatable :: speeds(:)
      double precision, allocatable :: x(:,:,:)
      
      associate(bind_C_history => t_cell_collection_bind_C_t(history))
        call do_concurrent_x(bind_C_history, x)
        call do_concurrent_speeds(x, bind_C_history, speeds)
      end associate
 
    end function

  end procedure

end submodule output_s
