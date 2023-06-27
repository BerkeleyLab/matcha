! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.tx
submodule(distribution_m) distribution_s
  use intrinsic_array_m, only : intrinsic_array_t
!  use do_concurrent_m, only : do_concurrent_sampled_speeds, do_concurrent_my_velocities
  use do_concurrent_m, only : do_concurrent_sample, do_concurrent_my_velocities  
  use assert_m, only : assert
  implicit none

contains
  
  pure function monotonically_increasing(f) result(monotonic)
    double precision, intent(in) :: f(:)
    logical monotonic
    integer i
    monotonic = all([(f(i+1) >= f(i), i=1, size(f)-1)])
  end function

  module procedure construct
    integer i

    call assert(all(sample_distribution(:,2)>=0.D0), "distribution_t%construct: sample_distribution>=0.", &
      intrinsic_array_t(sample_distribution))

    associate(nintervals => size(sample_distribution,1))      
      distribution%vel_ = [(sample_distribution(i,1), i =1, nintervals)]  ! Assign speeds to each distribution bin         
      distribution%cumulative_distribution_ = [0.D0, [(sum(sample_distribution(1:i,2)), i=1, nintervals)]]

      call assert(monotonically_increasing(distribution%cumulative_distribution_), &
        "distribution_t: monotonically_increasing(distribution%cumulative_distribution_)", &
        intrinsic_array_t(distribution%cumulative_distribution_))
    end associate


    call assert(all(sample_angle_distribution(:,2)>=0.D0), "distribution_t%construct: sample_distribution>=0.", &
      intrinsic_array_t(sample_angle_distribution))

    associate(nintervals => size(sample_angle_distribution,1))
      distribution%angle_ = [(sample_angle_distribution(i,1), i =1, nintervals)]  ! Assign speeds to each distribution bin                    
      distribution%cumulative_angle_distribution_ = [0.D0, [(sum(sample_angle_distribution(1:i,2)), i=1, nintervals)]]

      call assert(monotonically_increasing(distribution%cumulative_angle_distribution_), &
        "distribution_t: monotonically_increasing(distribution%cumulative_distribution_)", &
        intrinsic_array_t(distribution%cumulative_angle_distribution_))
    end associate


  end procedure construct

  module procedure cumulative_distribution
    call assert(allocated(self%cumulative_distribution_), &
      "distribution_t%cumulative_distribution: allocated(cumulative_distribution_)")
    my_cumulative_distribution = self%cumulative_distribution_
  end procedure 

  module procedure cumulative_angle_distribution
    call assert(allocated(self%cumulative_angle_distribution_), &
      "distribution_t%cumulative_angle_distribution: allocated(cumulative_angle_distribution_)")
    my_cumulative_angle_distribution = self%cumulative_angle_distribution_
  end procedure

  
  
  module procedure velocities
    
    double precision, allocatable :: sampled_speeds(:,:), sampled_angles(:,:),  dir(:,:,:)
    integer cell,step

    double precision dir_mag_

    double precision pi,twopi
    double precision a,b,c
    double precision x1,y1,z1,x2,y2,z2,x3,y3,z3,vec1mag
    double precision vxp,vyp,vzp,eps,vec3mag
    double precision random_phi
    double precision dot,adot,magv

    
    call assert(allocated(self%cumulative_distribution_), &
      "distribution_t%cumulative_distribution: allocated(cumulative_distribution_)")
    call assert(allocated(self%vel_), "distribution_t%cumulative_distribution: allocated(vel_)")

    call assert(allocated(self%cumulative_angle_distribution_), &
      "distribution_t%cumulative_angle_distribution: allocated(cumulative_angle_distribution_)")
    call assert(allocated(self%angle_), "distribution_t%cumulative_angle_distribution: allocated(angle_)")
    
     ! Sample from the distribution
     !call do_concurrent_sampled_speeds(speeds, self%vel_, self%cumulative_distribution(), sampled_speeds)
     call do_concurrent_sample(speeds, self%vel_, self%cumulative_distribution(), sampled_speeds)
     call do_concurrent_sample(angles, self%angle_, self%cumulative_angle_distribution(), sampled_angles)

     !print*,'sampled_angles = ',sampled_angles
!     associate(nsteps => size(speeds,2))
!
!       ! Create unit vectors
!       dir = directions(:,1:nsteps,:)
!
!       associate(dir_mag => sqrt(dir(:,:,1)**2 +dir(:,:,2)**2 + dir(:,:,3)**2))
!         associate(dir_mag_ => merge(dir_mag, epsilon(dir_mag), dir_mag/=0.))
!           dir(:,:,1) = dir(:,:,1)/dir_mag_
!           dir(:,:,2) = dir(:,:,2)/dir_mag_
!           dir(:,:,3) = dir(:,:,3)/dir_mag_
!         end associate
!       end associate
!       
!       call do_concurrent_my_velocities(nsteps, dir, sampled_speeds, my_velocities)
!       
!     end associate

       associate(ncells => size(speeds,1), nsteps => size(speeds,2))

       allocate(my_velocities(ncells,nsteps,3))

       pi = acos(-1.d0)
       twopi = 2.d0*pi
       eps = 1.d-12

!       do cell = 1,ncells                                                                                                                    
       do concurrent(cell = 1:ncells)
         do step = 1,nsteps
            random_phi = 2.d0*pi*directions(cell,step,1)
            vxp = sampled_speeds(cell,step)*sin(sampled_angles(cell,step))*cos(random_phi)
            vyp = sampled_speeds(cell,step)*sin(sampled_angles(cell,step))*sin(random_phi)
            vzp = sampled_speeds(cell,step)*cos(sampled_angles(cell,step))
            if (step .eq. 1) then
               my_velocities(cell,step,1) = vxp
               my_velocities(cell,step,2) = vyp
               my_velocities(cell,step,3) = vzp
            else
               x3 = my_velocities(cell,step-1,1)
               y3 = my_velocities(cell,step-1,2)
               z3 = my_velocities(cell,step-1,3)
               vec3mag = sqrt(x3**2 + y3**2 + z3**2)+eps
               x3 = x3/vec3mag
               y3 = y3/vec3mag
               z3 = z3/vec3mag

               if (abs(z3) .gt. eps) then
                  a = 0.d0
                  b = 1.d0
                  c = 0.d0
               else
                  a = 0.d0
                  b = 0.d0
                  c = 1.d0
               end if

               !curl(x3,y3,z3,a,b,c,x1,y1,z1)                                                                                                 
               x1 = y3*c - z3*b
               y1 = z3*a - x3*c
               z1 = x3*b - y3*a
               vec1mag = sqrt(x1**2 + y1**2 + z1**2) + eps
               x1 = x1/vec1mag
               y1 = y1/vec1mag
               z1 = z1/vec1mag
              ! curl(x3,y3,z3,x1,y1,z1,x2,y2,z2)                                                                                             
               x2 = y3*z1 - z3*y1
               y2 = z3*x1 - x3*z1
               z2 = x3*y1 - y3*x1


               my_velocities(cell,step,1) = vxp*x1 + vyp*x2 + vzp*x3
               my_velocities(cell,step,2) = vxp*y1 + vyp*y2 + vzp*y3
               my_velocities(cell,step,3) = vxp*z1 + vyp*z2 + vzp*z3

               !magv = dsqrt(my_velocities(cell,step,1)**2 + &
               !             my_velocities(cell,step,2)**2 + &
               !             my_velocities(cell,step,3)**2)
               !dot = (my_velocities(cell,step,1)*x3 + &
               !       my_velocities(cell,step,2)*y3 + &
               !       my_velocities(cell,step,3)*z3)/magv
               !adot = acos(dot)
               !print*,'adot = ',adot,sampled_angles(cell,step)

            end if

         end do
       end do

     end associate


     
  end procedure

end submodule distribution_s
