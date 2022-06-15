module random_numbers_m
  implicit none
  
  interface
  
    module subroutine create_rand_num_table(ncells,npositions,random_number_table)
    
      implicit none
      integer, intent(in) :: ncells,npositions
      double precision, intent(out) :: random_number_table(ncells,npositions,4)
      
    end subroutine
    
  end interface
  
end module random_numbers_m
      
