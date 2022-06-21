submodule(move_m) move_s
  implicit none

contains
  
  module procedure move_tcells
      ! Local variables
      integer i,j,k
      double precision dt
      double precision, dimension(ncells,npositions-1) :: speed,rr2,rr3,rr4,sum

      ! Time step
      dt = .1
      
      rr2(:,:) = random_number_table(:,2:,2)
      rr3(:,:) = random_number_table(:,2:,3)
      rr4(:,:) = random_number_table(:,2:,4)

      do i = 1,ncells
        do j = 2,npositions
          ! Sample from the distribution
          do k = 1,nintervals
            if (random_number_table(i,j,1) .ge. cumulative_distribution(k) .and. &
                random_number_table(i,j,1) .lt. cumulative_distribution(k+1)) then
                speed(i,j) = vel(k)
            end if
          end do
        end do
      end do
      
      ! Create a random unit vector

      sum(:,:) = rr2(:,:) + rr3(:,:) + rr4(:,:)
      rr2(:,:) = rr2(:,:)/sum(:,:)
      rr3(:,:) = rr3(:,:)/sum(:,:)
      rr4(:,:) = rr4(:,:)/sum(:,:)

      !     Use a forward Euler to advance the cell position
      x(:,2:) = x(:,:) + dt*speed(:,:)*rr2(:,:)
      y(:,2:) = y(:,:) + dt*speed(:,:)*rr3(:,:)
      z(:,2:) = z(:,:) + dt*speed(:,:)*rr4(:,:)

  end procedure move_tcells

end submodule move_s
