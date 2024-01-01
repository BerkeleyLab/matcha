module subdomain_m
  use assert_m, only : assert
  implicit none

  private
  public :: subdomain_t
  public :: operator(.laplacian.)
  public :: step

  type subdomain_t 
    private
    real, allocatable :: s_(:,:,:)
  contains
    procedure, pass(self) :: define
    procedure, pass(rhs) :: multiply
    generic :: operator(*) => multiply
    generic :: operator(+) => add
    generic :: assignment(=) => assign_
    procedure dx
    procedure dy
    procedure dz
    procedure values
    procedure, private :: add
    procedure, private :: assign_
  end type

  interface operator(.laplacian.)

    module procedure laplacian
    !pure module function laplacian(rhs) result(laplacian_rhs)
    !  implicit none
    !  type(subdomain_t), intent(in) :: rhs[*]
    !  type(subdomain_t) laplacian_rhs
    !end function

  end interface

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
      type(subdomain_t), intent(inout) :: self[*]
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

    module subroutine assign_(lhs, rhs)
      implicit none
      class(subdomain_t), intent(out) :: lhs
      type(subdomain_t), intent(in) :: rhs
    end subroutine

  end interface

  real dx_, dy_, dz_
  integer my_nx, nx, ny, nz, me, num_subdomains, my_internal_west, my_internal_east

contains

  pure module function laplacian(rhs) result(laplacian_rhs)
    type(subdomain_t), intent(in) :: rhs[*]
    type(subdomain_t) laplacian_rhs

    integer i, j, k
    real, allocatable :: halo_west(:,:), halo_east(:,:)

    call assert(allocated(rhs%s_), "subdomain_t%laplacian: allocated(rhs%s_)")

    allocate(laplacian_rhs%s_, mold=rhs%s_)

    if (me==1) then
      halo_west = rhs%s_(1,:,:)
    else
      halo_west = rhs[me-1]%s_(ubound(rhs[me-1]%s_,1),:,:)
    end if
    i = my_internal_west
    call assert(i+1<=my_nx,"laplacian: westernmost subdomain too small")
    do concurrent(j=2:ny-1, k=2:nz-1)
      laplacian_rhs%s_(i,j,k) = ( halo_west(j,k  ) - 2*rhs%s_(i,j,k) + rhs%s_(i+1,j  ,k  ))/dx_**2 + &
                                (rhs%s_(i,j-1,k  ) - 2*rhs%s_(i,j,k) + rhs%s_(i  ,j+1,k  ))/dy_**2 + &
                                (rhs%s_(i,j  ,k-1) - 2*rhs%s_(i,j,k) + rhs%s_(i  ,j  ,k+1))/dz_**2
    end do

    do concurrent(i=my_internal_west+1:my_internal_east-1, j=2:ny-1, k=2:nz-1)
      laplacian_rhs%s_(i,j,k) = (rhs%s_(i-1,j  ,k  ) - 2*rhs%s_(i,j,k) + rhs%s_(i+1,j  ,k  ))/dx_**2 + &
                                (rhs%s_(i  ,j-1,k  ) - 2*rhs%s_(i,j,k) + rhs%s_(i  ,j+1,k  ))/dy_**2 + &
                                (rhs%s_(i  ,j  ,k-1) - 2*rhs%s_(i,j,k) + rhs%s_(i  ,j  ,k+1))/dz_**2
    end do

    if (me==1) then
      halo_east = rhs%s_(1,:,:)
    else
      halo_east = rhs[me+1]%s_(lbound(rhs[me+1]%s_,1),:,:)
    end if
    i = my_internal_east
    call assert(i-1>0,"laplacian: easternmost subdomain too small")
    do concurrent(j=2:ny-1, k=2:nz-1)
      laplacian_rhs%s_(i,j,k) = (rhs%s_(i-1,j  ,k  ) - 2*rhs%s_(i,j,k) +  halo_east(j  ,k  ))/dx_**2 + &
                                (rhs%s_(i  ,j-1,k  ) - 2*rhs%s_(i,j,k) + rhs%s_(i  ,j+1,k  ))/dy_**2 + &
                                (rhs%s_(i  ,j  ,k-1) - 2*rhs%s_(i,j,k) + rhs%s_(i  ,j  ,k+1))/dz_**2
    end do

    laplacian_rhs%s_(:, 1,:) = 0.
    laplacian_rhs%s_(:,ny,:) = 0.
    laplacian_rhs%s_(:,:, 1) = 0.
    laplacian_rhs%s_(:,:,nz) = 0.
    if (me==1) laplacian_rhs%s_(1,:,:) = 0.
    if (me==num_subdomains) laplacian_rhs%s_(my_nx,:,:) = 0.

  end function


end module
