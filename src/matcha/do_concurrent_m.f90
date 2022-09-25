module do_concurrent_m
  use t_cell_collection_m, only : t_cell_collection_t
  implicit none
  private
  public :: do_concurrent_sampled_speeds, do_concurrent_my_velocities, do_concurrent_k, do_concurrent_x
  
  interface
    
    pure module function do_concurrent_sampled_speeds(speeds, vel, cumulative_distribution) result(sampled_speeds)
      implicit none
      double precision, intent(in) :: speeds(:,:), vel(:), cumulative_distribution(:)
      double precision, allocatable :: sampled_speeds(:,:)
    end function
    
    pure module function do_concurrent_my_velocities(speeds, directions, sampled_speeds) result(my_velocities)
      implicit none
      double precision, intent(in) :: speeds(:,:), directions(:,:,:), sampled_speeds(:,:)
      double precision, allocatable :: my_velocities(:,:,:)
    end function
    
    pure module function do_concurrent_k(speeds, vel) result (k)
      implicit none
      double precision, intent(in) :: speeds(:), vel(:)
      integer, allocatable :: k(:)
    end function
    
    pure module function do_concurrent_x(npositions, ncells, nspacedims, history) result(x)
      implicit none
      integer, intent(in) :: npositions, ncells, nspacedims
      type(t_cell_collection_t), intent(in) :: history(:)
      double precision, allocatable :: x(:,:,:)
    end function  
      

  end interface
  
end module do_concurrent_m
