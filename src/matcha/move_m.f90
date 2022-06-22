module move_m
  implicit none

  interface

    module subroutine move_tcells(x,y,z,vel,cumulative_distribution,random_number_table)
      implicit none
      double precision, intent(in) :: random_number_table(:,:,:)
      double precision, intent(inout) :: x(:,:)
      double precision, intent(inout) :: y(:,:)
      double precision, intent(inout) :: z(:,:)
      double precision, intent(in) :: cumulative_distribution(:)
      double precision, intent(in) :: vel(:)
    end subroutine

  end interface

end module move_m
