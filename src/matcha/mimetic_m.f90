! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module mimetic_m
  use subdomain_m, only : subdomain_t
  implicit none

  private
  public :: mimetic_t
  public :: operator(.div.)
 

  type, extends(subdomain_t) :: mimetic_t
    private
  ! Interpolating weights
    real, allocatable :: Q_(:)
    real, allocatable :: P_(:)
    integer :: mimetic_k_ = 2 ! Order 2, 4 or 6
  contains
    procedure, pass(self) :: setOrder
    generic :: operator(.grad.) => gradient
    procedure, private :: gradient
    procedure mimetic_k
 end type
 
  interface operator(.div.)
 
    module function divergence(rhs) result(divergence_rhs)
      implicit none
      class(mimetic_t), intent(in) :: rhs(:)
      type(mimetic_t) divergence_rhs
    end function

  end interface

  interface
 
    module function gradient(rhs) result(gradient_rhs)
      implicit none
      class(mimetic_t), intent(in) :: rhs
      type(mimetic_t), allocatable :: gradient_rhs(:)
    end function
 
    module subroutine setOrder(my_k, self)
      implicit none
      integer, intent(in) :: my_k
      class(mimetic_t), intent(out) :: self
    end subroutine
 
    pure module function mimetic_k(self) result(my_k)
      implicit none
      class(mimetic_t), intent(in) :: self
      real my_k
    end function

  end interface
 
end module
