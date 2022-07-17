submodule(input_m) input_s
  implicit none

contains
      
  module procedure num_cells
      n = self%num_cells_
  end procedure
   
  module procedure num_positions
      n = self%num_positions_
  end procedure
  
  module procedure num_dimensions
      n = self%num_dimensions_
  end procedure

  module procedure num_intervals
      n = self%num_intervals_
  end procedure

  module procedure time_step
      dt = self%time_step_
  end procedure

  module procedure sample_distribution
     integer i,nintervals
     double precision speed_lower,speed_upper
     double precision range,pi,dspeed,sumy
     double precision speed_lower_bin
     double precision speed_upper_bin
     double precision speed_middle_bin
     double precision, allocatable :: x(:),y(:)
     nintervals = self%num_intervals_

     speed_lower = 0.d0
     speed_upper = 6.d0
     range = speed_upper - speed_lower
     pi = acos(-1.d0)
     dspeed = range/dble(nintervals)
     allocate(x(nintervals),y(nintervals))
     x(1) = 1.d0
     x(2) = 2.d0
     x(3) = 3.d0
     x(4) = 4.d0

     y(1) = .2d0
     y(2) = .3d0
     y(3) = .3d0
     y(4) = .2d0

!    Create normal distribution     
     sumy = 0.d0
     do i = 1,nintervals
        speed_lower_bin = speed_lower + dble(i-1)*dspeed
        speed_upper_bin = speed_lower + dble(i)*dspeed
        speed_middle_bin = 0.5d0*(speed_lower_bin + speed_upper_bin)
        x(i) = speed_middle_bin
        y(i) = exp(-(x(i)-3.d0)**2/2.d0)/dsqrt(2.d0*pi)
        sumy = sumy + y(i)
     end do

     do i = 1,nintervals
        y(i) = y(i)/sumy
     end do
      
     allocate(empirical_distribution(nintervals,2))
      
     do i = 1,nintervals
         empirical_distribution(i,1) = x(i)
         empirical_distribution(i,2) = y(i)
     end do
      
  end procedure    
 
end submodule input_s
