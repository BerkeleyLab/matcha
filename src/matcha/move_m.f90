module move_m
  use distribution_m, only : distribution_t
  implicit none

  interface

    module subroutine move_tcells(distribution, random_speeds, random_directions, x,y,z)
      implicit none
      type(distribution_t), intent(in) :: distribution
      double precision, intent(in) :: random_speeds(:,:), random_directions(:,:,:)
      double precision, intent(inout), dimension(:,:) :: x, y, z
    end subroutine

  end interface

end module move_m
