module move_m
  implicit none

  interface

    module subroutine move_tcells(x,y,z,vel,cumulative_distribution,random_number_table,ncells,npositions,nintervals,nveldim)
      implicit none
      integer, intent(in) :: ncells,npositions,nintervals,nveldim
      double precision, intent(in) :: random_number_table(ncells,npositions,nveldim)
      double precision, intent(inout) :: x(ncells,npositions)
      double precision, intent(inout) :: y(ncells,npositions)
      double precision, intent(inout) :: z(ncells,npositions)
      double precision, intent(in) :: cumulative_distribution(nintervals+1)
      double precision, intent(in) :: vel(nintervals)
    end subroutine

  end interface

end module move_m
