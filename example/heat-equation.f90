module assertions_m
  implicit none
contains
  pure subroutine assert(assertion, description)
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) error stop description
  end subroutine
end module

module subdomain_2D_m
  implicit none

  private
  public :: subdomain_2D_t

  type subdomain_2D_t 
    private
    real, allocatable :: s_(:,:)
  contains
    procedure define
    procedure dx
    procedure dy
    procedure values
    procedure exchange_halo
    generic :: operator(.laplacian.) => laplacian
    generic :: operator(+) => add
    procedure, private :: add
    procedure, private :: laplacian
  end type

  interface

    module subroutine define(self, side, boundary_val, internal_val, n)
      implicit none
      real, intent(in) :: side, boundary_val, internal_val
      integer, intent(in) :: n !! number of grid points in each coordinate direction
      class(subdomain_2D_t), intent(out) :: self
    end subroutine

    pure module function values(self) result(my_values)
      implicit none
      class(subdomain_2D_t), intent(in) :: self
      real, allocatable :: my_values(:,:)
    end function

    pure module function dx(self) result(my_dx)
      implicit none
      class(subdomain_2D_t), intent(in) :: self
      real my_dx
    end function

    pure module function dy(self) result(my_dy)
      implicit none
      class(subdomain_2D_t), intent(in) :: self
      real my_dy
    end function

    pure module function laplacian(rhs) result(laplacian_rhs)
      implicit none
      class(subdomain_2D_t), intent(in) :: rhs
      real, allocatable :: laplacian_rhs(:,:)
    end function

    pure module function add(lhs, rhs) result(total)
      implicit none
      class(subdomain_2D_t), intent(in) :: lhs
      real, intent(in) :: rhs(:,:)
      type(subdomain_2D_t) total
    end function

    module subroutine exchange_halo(self)
      implicit none
      class(subdomain_2D_t), intent(in) :: self
    end subroutine

  end interface

end module

submodule(subdomain_2D_m) subdomain_2D_s
  use assertions_m, only : assert
  implicit none

  real, allocatable :: halo_x(:,:)[:]
  real dx_, dy_
  integer, parameter :: west=1, east=2
  integer my_nx, nx, ny, me, num_subdomains, my_internal_west, my_internal_east

contains

  module procedure define

    integer, parameter :: nx_boundaries = 2
    call assert(num_subdomains <= n-nx_boundaries, "subdomain_2D_t%define: num_subdomains <= n-nx_boundaries")

    nx = n
    ny = nx
    dx_ = side/(nx-1)
    dy_ = dx_
    me = this_image()
    num_subdomains = num_images()
    my_nx = nx/num_subdomains + merge(1, 0, me <= mod(nx, num_subdomains))
    my_internal_west = merge(2, 1, me==1)
    my_internal_east = merge(my_nx-1, my_nx, me==num_subdomains)

    if (allocated(self%s_)) deallocate(self%s_)
    allocate(self%s_(my_nx, ny))

    self%s_(my_internal_west:my_internal_east, 1) = boundary_val ! bottom subdomain boundary
    self%s_(my_internal_west:my_internal_east, ny) = boundary_val ! top subdomain boundary
    self%s_(my_internal_west:my_internal_east, 2:ny-1) = internal_val ! internal points
    self%s_(1, 2:ny-1) = merge(boundary_val, internal_val, me==1) ! west subdomain boundary
    self%s_(my_nx, 2:ny-1) = merge(boundary_val, internal_val, me==num_subdomains) ! east subdomain boundary

    if (allocated(halo_x)) deallocate(halo_x)
    allocate(halo_x(west:east, ny)[*])
    call self%exchange_halo
  end procedure

  module procedure dx
    my_dx = dx_
  end procedure

  module procedure dy
    my_dy = dy_
  end procedure

  module procedure laplacian

    integer i, j
    real, allocatable :: halo_west(:), halo_east(:)

    call assert(allocated(rhs%s_), "subdomain_2D_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_2D_t%laplacian: allocated(halo_x)")

    allocate(laplacian_rhs(my_nx, ny))

    halo_west = merge(halo_x(west,:), rhs%s_(1,:), me/=1)
    i = my_internal_west
    call assert(i+1<=my_nx,"laplacian: westmost subdomain too small")
    do concurrent(j=2:ny-1)
      laplacian_rhs(i,j) = (halo_west(j)   - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                           (rhs%s_(i,j-1)  - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

    do concurrent(i=my_internal_west+1:my_internal_east-1, j=2:ny-1)
      laplacian_rhs(i,j) = (rhs%s_(i-1,j) - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                           (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

    halo_east = merge(halo_x(east,:), rhs%s_(my_nx,:), me/=num_subdomains)
    i = my_internal_east
    call assert(i-1>0,"laplacian: eastmost subdomain too small")
    do concurrent(j=2:ny-1)
      laplacian_rhs(i,j) = (rhs%s_(i-1,j)  - 2*rhs%s_(i,j) + halo_east(j))/dx_**2 + &
                           (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

    laplacian_rhs(:, 1) = 0.
    laplacian_rhs(:,ny) = 0.
    if (me==1) laplacian_rhs(1,:) = 0.
    if (me==num_subdomains) laplacian_rhs(my_nx,:) = 0.
  end procedure

  module procedure add
    total%s_ =  lhs%s_ + rhs
  end procedure
   
  module procedure exchange_halo
    if (me>1) halo_x(east,:)[me-1] = self%s_(1,:)
    if (me<num_subdomains) halo_x(west,:)[me+1] = self%s_(my_nx,:)
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_2D_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

end submodule subdomain_2D_s

program main
  use subdomain_2D_m, only : subdomain_2D_t
  implicit none

  integer step
  integer, parameter :: nx = 51, ny = nx, steps = 5000
  real, parameter :: T_initial = 2., T_boundary = 1., alpha = 1.
  real t_start, t_finish
  type(subdomain_2D_t) T

  call T%define(side=1., boundary_val=T_boundary, internal_val=T_initial, n=nx) ! 2D step function
  sync all

  associate(dt => T%dx()*T%dy()/(4*alpha))
    call cpu_time(t_start)
    do step = 1, steps
      T =  T + dt * alpha * .laplacian. T
      call T%exchange_halo
      sync all
    end do
    call cpu_time(t_finish)
    print *, "cpu_time: ", t_finish - t_start
  end associate
  associate(T_values => T%values())
    print *,"T_initial, T_boundary, T_min, T_max: ", T_initial, T_boundary, minval(T_values(:,2:ny-1)), maxval(T_values(:,2:ny-1))
  end associate
end program
