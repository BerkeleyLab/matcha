! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module mimetic_m
  use subdomain_m, only : subdomain_t
  implicit none

  private
  public :: mimetic_t
  public :: operator(.div.)
 
  type, extends(subdomain_t) :: mimetic_t
  contains
    generic :: operator(.grad.) => gradient
    procedure, private :: gradient
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
 
  end interface
 
end module
