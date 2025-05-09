! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module subdomain_m
  implicit none

  private
  public :: subdomain_t

  type subdomain_t 
    private
    real, allocatable :: s_(:,:,:)
  contains
    procedure, pass(self) :: define
    procedure, pass(self) :: step
    procedure, pass(rhs) :: multiply
    generic :: operator(.laplacian.) => laplacian
    generic :: operator(*) => multiply
    generic :: operator(+) => add
    generic :: assignment(=) => assign_and_sync
    procedure dt_stable
    procedure dx
    procedure dy
    procedure dz
    procedure values
    procedure, private :: laplacian
    procedure, private :: add
    procedure, private :: assign_and_sync
  end type

  interface

    module subroutine define(side, boundary_val, internal_val, n, self)
      implicit none
      real, intent(in) :: side, boundary_val, internal_val
      integer, intent(in) :: n !! number of grid points in each coordinate direction
      class(subdomain_t), intent(out) :: self
    end subroutine

    module subroutine step(alpha_dt, self)
      implicit none
      real, intent(in) :: alpha_dt
      class(subdomain_t), intent(inout) :: self
    end subroutine

    pure module function values(self) result(my_values)
      implicit none
      class(subdomain_t), intent(in) :: self
      real, allocatable :: my_values(:,:,:)
    end function

    pure module function dx(self) result(my_dx)
      implicit none
      class(subdomain_t), intent(in) :: self
      real my_dx
    end function

    pure module function dy(self) result(my_dy)
      implicit none
      class(subdomain_t), intent(in) :: self
      real my_dy
    end function

    pure module function dz(self) result(my_dz)
      implicit none
      class(subdomain_t), intent(in) :: self
      real my_dz
    end function

    pure module function dt_stable(self, alpha) result(my_dt)
      !! Set the time step at 90% of the stability limit obtained generalizing to 3D the value provided for 2D by
      !! Kassinos, S., & Alexiadis, A. (2024). Beyond Language: Applying MLX Transformers to Engineering Physics. 
      !! arXiv preprint arXiv:2410.04167.
      implicit none
      class(subdomain_t), intent(in) :: self
      real, intent(in) :: alpha
      real my_dt
    end function

    pure module function laplacian(rhs) result(laplacian_rhs)
      implicit none
      class(subdomain_t), intent(in) :: rhs
      type(subdomain_t) laplacian_rhs
    end function

    pure module function multiply(lhs, rhs) result(product)
      implicit none
      class(subdomain_t), intent(in) :: rhs
      real, intent(in) :: lhs
      type(subdomain_t) product
    end function

    pure module function add(lhs, rhs) result(total)
      implicit none
      class(subdomain_t), intent(in) :: lhs
      type(subdomain_t), intent(in) :: rhs
      type(subdomain_t) total
    end function

    module subroutine assign_and_sync(lhs, rhs)
      implicit none
      class(subdomain_t), intent(out) :: lhs
      type(subdomain_t), intent(in) :: rhs
    end subroutine

  end interface

end module
