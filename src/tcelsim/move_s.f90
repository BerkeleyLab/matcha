submodule(move_m) move_s
  implicit none

contains
  
  module procedure move_tcells
      ! Local variables
      integer i,j,k
      double precision speed,dt
      double precision rr1,rr2,rr3,rr4,sum

      ! Time step
      dt = .1

      do i = 1,ncells
        do j = 2,npositions
          
          rr1 = random_number_table(i,j,1)
          rr2 = random_number_table(i,j,2)
          rr3 = random_number_table(i,j,3)
          rr4 = random_number_table(i,j,4)

          ! Sample from the distribution
          do k = 1,nintervals
            if (rr1 .ge. cumulative_distribution(k) .and. &
                rr1 .lt. cumulative_distribution(k+1)) then
               speed = vel(k)
            end if
          end do

          ! Create a random unit vector

          sum = rr2 + rr3 + rr4
          rr2 = rr2/sum
          rr3 = rr3/sum
          rr4 = rr4/sum

          !     Use a forward Euler to advance the cell position
          x(i,j) = x(i,j-1) + dt*speed*rr2
          y(i,j) = y(i,j-1) + dt*speed*rr3
          z(i,j) = z(i,j-1) + dt*speed*rr4
        end do
      end do

    end procedure move_tcells

end submodule move_s
