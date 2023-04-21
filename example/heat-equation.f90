module subdomain_2D_m
  implicit none

  private
  public :: subdomain_2D_t

  type subdomain_2D_t 
    private
    real, allocatable :: s_(:,:)
  contains
    procedure, pass(self) :: define
    procedure laplacian
    generic :: operator(.laplacian.) => laplacian
    procedure, pass(rhs) :: multiply
    generic :: operator(*) => multiply
    procedure add
    generic :: operator(+) => add
    procedure assign_and_sync
    generic :: assignment(=) => assign_and_sync
    procedure dx
    procedure dy
    procedure values
  end type

  interface

    module subroutine define(side, boundary_val, internal_val, n, self)
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
      type(subdomain_2D_t) laplacian_rhs
    end function

    pure module function multiply(lhs, rhs) result(product)
      implicit none
      class(subdomain_2D_t), intent(in) :: rhs
      real, intent(in) :: lhs
      type(subdomain_2D_t) product
    end function

    pure module function add(lhs, rhs) result(total)
      implicit none
      class(subdomain_2D_t), intent(in) :: lhs
      type(subdomain_2D_t), intent(in) :: rhs
      type(subdomain_2D_t) total
    end function

    module subroutine assign_and_sync(lhs, rhs)
      implicit none
      class(subdomain_2D_t), intent(out) :: lhs
      type(subdomain_2D_t), intent(in) :: rhs
    end subroutine

  end interface

end module

submodule(subdomain_2D_m) subdomain_2D_s
  use data_partition_m, only : data_partition_t
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

  real, allocatable :: halo_x(:,:)[:]
  integer, parameter :: west=1, east=2

  type(data_partition_t) data_partition

  real dx_, dy_
  integer my_nx, nx, ny, me, num_subdomains, my_internal_left, my_internal_right

contains

  module procedure define

    integer, parameter :: nx_boundaries = 2

    nx = n
    ny = nx
    dx_ = side/(nx-1)
    dy_ = dx_
    call assert(num_subdomains <= nx-nx_boundaries, &
      "subdomain_2D_t%define: num_subdomains <= nx-nx_boundaries", intrinsic_array_t([nx, num_subdomains]))
    me = this_image()
    num_subdomains = num_images()

    call data_partition%define_partitions(nx)
    my_nx = data_partition%last(me) - data_partition%first(me) + 1

    if (allocated(self%s_)) deallocate(self%s_)
    allocate(self%s_(my_nx, ny))

    my_internal_left = merge(2, 1, me==1)
    my_internal_right = merge(my_nx-1, my_nx, me==num_subdomains)

    self%s_(my_internal_left:my_internal_right, 1) = boundary_val ! bottom subdomain boundary
    self%s_(my_internal_left:my_internal_right, ny) = boundary_val ! top subdomain boundary
    self%s_(my_internal_left:my_internal_right, 2:ny-1) = internal_val ! internal points
    self%s_(1, 2:ny-1) = merge(boundary_val, internal_val, me==1) ! left subdomain boundary
    self%s_(my_nx, 2:ny-1) = merge(boundary_val, internal_val, me==num_subdomains) ! right subdomain boundary

    if (allocated(halo_x)) deallocate(halo_x)
    allocate(halo_x(west:east, ny)[*])
    if (me>1) halo_x(east,:)[me-1] = self%s_(1,:)
    if (me<num_subdomains) halo_x(west,:)[me+1] = self%s_(my_nx,:)
    sync all

  end procedure

  module procedure dx
    my_dx = dx_
  end procedure

  module procedure dy
    my_dy = dy_
  end procedure

  module procedure laplacian

    integer i, j
    real, allocatable :: halo_left(:), halo_right(:)

    call assert(allocated(rhs%s_), "subdomain_2D_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_2D_t%laplacian: allocated(halo_x)")

    allocate(laplacian_rhs%s_(my_nx, ny))

    halo_left = merge(halo_x(west,:), rhs%s_(1,:), me/=1)
    i = my_internal_left
    call assert(i+1<=my_nx,"laplacian: leftmost subdomain too small")
    do concurrent(j=2:ny-1)
      laplacian_rhs%s_(i,j) = (halo_left(j)   - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                              (rhs%s_(i,j-1)  - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

    do concurrent(i=my_internal_left+1:my_internal_right-1, j=2:ny-1)
      laplacian_rhs%s_(i,j) = (rhs%s_(i-1,j) - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                              (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

    halo_right = merge(halo_x(east,:), rhs%s_(my_nx,:), me/=num_subdomains)
    i = my_internal_right
    call assert(i-1>0,"laplacian: rightmost subdomain too small")
    do concurrent(j=2:ny-1)
      laplacian_rhs%s_(i,j) = (rhs%s_(i-1,j)  - 2*rhs%s_(i,j) + halo_right(j))/dx_**2 + &
                              (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

    laplacian_rhs%s_(:, 1) = 0.
    laplacian_rhs%s_(:,ny) = 0.
    if (me==1) laplacian_rhs%s_(1,:) = 0.
    if (me==num_subdomains) laplacian_rhs%s_(my_nx,:) = 0.
  end procedure

  module procedure multiply
    call assert(allocated(rhs%s_), "subdomain_2D_t%multiply: allocated(rhs%s_)")
    product%s_ =  lhs * rhs%s_
  end procedure

  module procedure add
    call assert(allocated(rhs%s_), "subdomain_2D_t%add: allocated(rhs%s_)")
    total%s_ =  lhs%s_ + rhs%s_
  end procedure

  module procedure assign_and_sync
    call assert(allocated(rhs%s_), "subdomain_2D_t%assign_and_sync: allocated(rhs%s_)")
    sync all
    lhs%s_ =  rhs%s_
    if (me>1) halo_x(east,:)[me-1] = rhs%s_(1,:)
    if (me<num_subdomains) halo_x(west,:)[me+1] = rhs%s_(my_nx,:)
    sync all
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_2D_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

end submodule subdomain_2D_s

program main
  use subdomain_2D_m, only : subdomain_2D_t
  implicit none

  type(subdomain_2D_t) T
  real, parameter :: T_initial = 2., T_boundary = 1.
  integer, parameter :: nx = 51, ny = nx

  call T%define(side=1., boundary_val=T_boundary, internal_val=T_initial, n=nx) 
    ! spatially constant internal temperatuers with a step change at the boundaries

  block
    integer, parameter :: steps = 5000
    real, parameter :: alpha = 1.
    integer step
    real t_start, t_finish
    associate(dt => T%dx()*T%dy()/(4*alpha))
      call cpu_time(t_start)
      do step = 1, steps
        T =  T + dt * alpha * .laplacian. T
      end do
      call cpu_time(t_finish)
      print *, "cpu_time: ", t_finish - t_start
    end associate
    associate(T_values => T%values())
      print *,"T_initial, T_boundary, T_min, T_max: ", T_initial, T_boundary, minval(T_values(:,2:ny-1)), maxval(T_values(:,2:ny-1))
    end associate
  end block

end program
