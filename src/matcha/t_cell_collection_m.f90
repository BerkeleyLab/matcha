! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module t_cell_collection_m
  !! Define a T-cell abstraction for motility simulations
  use distribution_m, only : distribution_t
  use iso_c_binding, only : c_ptr, c_double, c_int
  implicit none
  
  private
  public :: t_cell_collection_t
  public :: t_cell_collection_bind_C_t
  
  type t_cell_collection_t
    !! Encapsulate the state of a collection of T cells
    private
    double precision, allocatable :: positions_(:,:) !! position vectors
    double precision time_ !! time stamp
  contains
    procedure :: positions
    procedure :: time
  end type

  integer, parameter :: positions_dimension = 2

  type, bind(C) :: t_cell_collection_bind_C_t
    type(c_ptr) positions_ptr
    integer(c_int) positions_shape(positions_dimension)
    real(c_double) time
  end type
  
  interface t_cell_collection_t
    
    pure module function construct(positions, time) result(t_cell_collection)
      !! Return a t_cell_collection_t object with rescaled position vectors and the provided time stamp
      implicit none
      double precision, intent(in) :: positions(:,:), time
      type(t_cell_collection_t) t_cell_collection
    end function 
    
  end interface

    
  interface t_cell_collection_bind_C_t
    
    impure elemental module function construct_bind_C(t_cell_collection) result(t_cell_collection_bind_C)
      !! Result is bind(C) representation of the data inside a t_cell_collection_t object
      !! This function is impure because it invokes c_loc. Fortran 2023 compliance will allow this function to be pure.
      implicit none
      type(t_cell_collection_t), intent(in), target :: t_cell_collection
      type(t_cell_collection_bind_C_t) t_cell_collection_bind_C
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
