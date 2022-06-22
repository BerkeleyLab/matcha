module distribution_m
  implicit none
    

  interface
  
    module subroutine create_distribution(vel,cumulative_distribution)
  
      implicit none
      double precision, intent(out) :: cumulative_distribution(:)
      double precision, intent(out) :: vel(:)

    end subroutine create_distribution
    
  end interface
  
end module distribution_m
