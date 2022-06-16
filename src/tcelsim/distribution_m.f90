module distribution_m
  implicit none
    

  interface
  
    module subroutine create_distribution(vel,cumulative_distribution,nintervals)
  
      implicit none
      integer, intent(in) :: nintervals
      double precision, intent(out) :: cumulative_distribution(nintervals+1)
      double precision, intent(out) :: vel(nintervals)

    end subroutine create_distribution
    
  end interface
  
end module distribution_m
