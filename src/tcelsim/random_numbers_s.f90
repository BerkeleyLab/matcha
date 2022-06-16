submodule(random_numbers_m) random_numbers_s
  implicit none
  
contains

  module procedure create_rand_num_table
  
    ! Local variables
    integer i,j,k
    
    do i = 1,ncells
      do j = 1,npositions
        do k = 1,nveldim
          call random_number(random_number_table(i,j,k))
        end do
      end do
    end do
    
  end procedure create_rand_num_table
  
end submodule random_numbers_s
