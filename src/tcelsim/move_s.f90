submodule(move_m) move_s
  implicit none

contains
  
  module procedure move_tcells
      ! Local variables
      integer i, j
      double precision, allocatable, dimension(:,:) :: speed
      
      associate(ncells => size(random_number_table,1), npositions => size(random_number_table,2) )
        allocate(speed(ncells, npositions-1))

        ! Sample from the distribution
        do concurrent(i = 1:ncells, j = 1:npositions-1)
          associate(k => findloc(random_number_table(i,j,1) >= cumulative_distribution, value=.true., dim=1))
            speed(i,j) = vel(k)
          end associate
        end do
      
        block
          ! Time step
          double precision, allocatable, dimension(:,:,:) :: dir
          double precision, parameter :: dt = .1
          
          ! Create a random unit vector
          dir = random_number_table(:, 1:npositions-1, 2:4)

          associate(dir_mag => sqrt(dir(:,:,1)**2 +dir(:,:,2)**2 + dir(:,:,3)**2))
            associate(dir_mag_ => merge(dir_mag, epsilon(dir_mag), dir_mag/=0.))
              dir(:,:,1) = dir(:,:,1)/dir_mag_
              dir(:,:,2) = dir(:,:,2)/dir_mag_
              dir(:,:,3) = dir(:,:,3)/dir_mag_
            end associate
          end associate

          !     Use a forward Euler to advance the cell position
          do i=2,npositions
            x(:,i) = x(:,i-1) + dt*speed(:,i-1)*dir(:,i-1,1)
            y(:,i) = y(:,i-1) + dt*speed(:,i-1)*dir(:,i-1,2)
            z(:,i) = z(:,i-1) + dt*speed(:,i-1)*dir(:,i-1,3)
          end do
        end block
      end associate

  end procedure move_tcells

end submodule move_s
