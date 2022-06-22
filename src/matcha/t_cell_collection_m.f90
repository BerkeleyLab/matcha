module t_cell_collection_m
  !! Define a T-cell abstraction for motility simulations
  implicit none
  
  private
  public :: t_cell_collection_t
  
  type t_cell_collection_t
    !! Encapsulate the state of a collection of T cells
    private
    double precision, allocatable :: positions_(:,:) !! position vectors
    double precision time_ !! time stample
  contains
    procedure :: positions
  end type
  
  interface t_cell_collection_t
    
    pure module function construct(positions, scale, time) result(t_cell_collection)
      !! Return a t_cell_collection_t object rescaled position vectors and the provided time stamp
      implicit none
      double precision, intent(in) :: positions(:,:), scale, time
      type(t_cell_collection_t) t_cell_collection
    end function 
    
  end interface
  
  interface
    
    pure module function positions(self) result(my_positions)
      !! Return the t_cell_collection_t object's position vectors
      implicit none
      class(t_cell_collection_t), intent(in) :: self
      double precision, allocatable :: my_positions(:,:)
    end function
    
  end interface 
  
end module t_cell_collection_m
