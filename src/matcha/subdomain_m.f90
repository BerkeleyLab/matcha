module subdomain_m
  implicit none

  private
  public :: subdomain_t

  type subdomain_t 
    private
    real, allocatable :: s_(:,:)
  contains
    procedure, pass(self) :: define
    procedure, pass(self) :: step
    procedure :: laplacian
    generic :: operator(.laplacian.) => laplacian
    procedure, pass(rhs) :: multiply
    generic :: operator(*) => multiply
    procedure :: add
    generic :: operator(+) => add
    procedure :: assign_wait_post
    generic :: assignment(=) => assign_wait_post
    procedure dx
    procedure dy
    procedure values
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
      real, allocatable :: my_values(:,:)
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

    pure module function laplacian(rhs) result(laplacian_rhs)
      implicit none
      class(subdomain_t), intent(in), target :: rhs
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

    module subroutine assign_wait_post(lhs, rhs)
      implicit none
      class(subdomain_t), intent(out) :: lhs
      type(subdomain_t), intent(in) :: rhs
    end subroutine

  end interface

end module
