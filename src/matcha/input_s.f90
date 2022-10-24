! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
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

  module procedure num_angle_intervals
      n = self%num_angle_intervals_
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
     double precision, allocatable :: speeds(:),probability(:)
     nintervals = self%num_intervals_

     speed_lower = 0.d0
     speed_upper = 6.d0
     range = speed_upper - speed_lower
     pi = acos(-1.d0)
     dspeed = range/dble(nintervals)
     allocate(speeds(nintervals),probability(nintervals))
!    Create normal distribution     
     sumy = 0.d0
     do i = 1,nintervals
        speed_lower_bin = speed_lower + dble(i-1)*dspeed
        speed_upper_bin = speed_lower + dble(i)*dspeed
        speed_middle_bin = 0.5d0*(speed_lower_bin + speed_upper_bin)
        speeds(i) = speed_middle_bin
        probability(i) = exp(-(speeds(i)-3.d0)**2/2.d0)/dsqrt(2.d0*pi) ! Use normal distribution
        sumy = sumy + probability(i)
     end do

     do i = 1,nintervals
        probability(i) = probability(i)/sumy
     end do
      
     allocate(empirical_distribution(nintervals,2))
      
     do i = 1,nintervals
         empirical_distribution(i,1) = speeds(i)
         empirical_distribution(i,2) = probability(i)
     end do
      
  end procedure    

  module procedure sample_angle_distribution
     integer i,nintervals
     double precision angle_lower,angle_upper
     double precision range,pi,dangle,sumy
     double precision angle_lower_bin
     double precision angle_upper_bin
     double precision angle_middle_bin
     double precision pi
     double precision, allocatable :: angles(:),probability(:)
     nintervals = self%num_angle_intervals_

     pi = acos(-1.d0)
     angle_lower = 0.d0
     angle_upper = pi
     range = angle_upper - angle_lower
     pi = acos(-1.d0)
     dangle = range/dble(nintervals)
     allocate(angles(nintervals),probability(nintervals))
!    Create normal distribution     
     sumy = 0.d0
     do i = 1,nintervals
        angle_lower_bin = angle_lower + dble(i-1)*dangle
        angle_upper_bin = angle_lower + dble(i)*dangle
        angle_middle_bin = 0.5d0*(angle_lower_bin + angle_upper_bin)
        angles(i) = angle_middle_bin
        probability(i) = 1.d0/dble(nintervals) ! Use uniform distribution
        sumy = sumy + probability(i)
     end do

     do i = 1,nintervals
        probability(i) = probability(i)/sumy
     end do
      
     allocate(empirical_angle_distribution(nintervals,2))
      
     do i = 1,nintervals
         empirical_angle_distribution(i,1) = angles(i)
         empirical_angle_distribution(i,2) = probability(i)
     end do
      
  end procedure    
 
end submodule input_s
