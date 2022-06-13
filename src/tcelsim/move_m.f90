module move_m
  implicit none

  interface

    module subroutine move_tcells(x,y,z,vel,cumulative_distribution,ncells,npositions,nintervals)
      implicit none
      integer, intent(in) :: ncells,npositions,nintervals
      double precision, intent(inout) :: x(ncells,npositions)
      double precision, intent(inout) :: y(ncells,npositions)
      double precision, intent(inout) :: z(ncells,npositions)
      double precision, intent(in) :: cumulative_distribution(0:nintervals)
      double precision, intent(in) :: vel(nintervals)
    end subroutine

  end interface

end module move_m
