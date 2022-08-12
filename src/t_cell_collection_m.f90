! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module t_cell_collection_m
  !! Define a T-cell abstraction for motility simulations
  use distribution_m, only : distribution_t
  implicit none
  
  private
  public :: t_cell_collection_t
  
  type t_cell_collection_t
    !! Encapsulate the state of a collection of T cells
    private
    double precision, allocatable :: positions_(:,:) !! position vectors
    double precision time_ !! time stamp
  contains
    procedure :: positions
    procedure :: time
  end type
  
  interface t_cell_collection_t
    
    pure module function construct(positions, time) result(t_cell_collection)
      !! Return a t_cell_collection_t object rescaled position vectors and the provided time stamp
      implicit none
      double precision, intent(in) :: positions(:,:), time
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
    
    
    elemental module function time(self) result(my_time)
      !! Return the t_cell_collection_t object's time stamp
      implicit none
      class(t_cell_collection_t), intent(in) :: self
      double precision my_time
    end function
    
  end interface 
  
end module t_cell_collection_m
