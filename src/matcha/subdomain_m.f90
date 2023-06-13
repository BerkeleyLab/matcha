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
    procedure :: laplacian
    generic :: operator(.laplacian.) => laplacian
    procedure, pass(rhs) :: multiply
    generic :: operator(*) => multiply
    procedure :: add
    generic :: operator(+) => add
    procedure :: assign_and_sync
    generic :: assignment(=) => assign_and_sync
    procedure dx
    procedure dy
    procedure dz
    procedure mynx
    procedure myny
    procedure mynz
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

    pure module function mynx(self) result(mynx_)
      implicit none
      class(subdomain_t), intent(in) :: self
      integer mynx_
    end function

    pure module function myny(self) result(myny_)
      implicit none
      class(subdomain_t), intent(in) :: self
      integer myny_
    end function

    pure module function mynz(self) result(mynz_)
      implicit none
      class(subdomain_t), intent(in) :: self
      integer mynz_
    end function

    module function laplacian(rhs) result(laplacian_rhs)
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
