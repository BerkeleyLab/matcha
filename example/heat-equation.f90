! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module assertions_m
  implicit none
contains
  pure subroutine assert(assertion, description)
     logical, intent(in) :: assertion
     character(len=*), intent(in) :: description
     if (.not. assertion) error stop description
  end subroutine
end module assertions_m

module subdomain_2D_m
  implicit none

  private
  public :: subdomain_2D_t

  type subdomain_2D_t 
    private
    real, allocatable :: s_(:,:)
  contains
    procedure, pass(self) :: define
    procedure :: laplacian
    generic :: operator(.laplacian.) => laplacian
    procedure, pass(rhs) :: multiply
    generic :: operator(*) => multiply
    procedure :: add
    generic :: operator(+) => add
    procedure :: copy
    generic :: assignment(=) => copy
    procedure dx
    procedure dy
    procedure values
    procedure exchange_halo
    procedure, nopass :: allocate_halo_coarray
  end type

  interface

    module subroutine define(side, boundary_val, internal_val, n, self)
      implicit none
      real, intent(in) :: side, boundary_val, internal_val
      integer, intent(in) :: n !! number of grid points in each coordinate direction
      class(subdomain_2D_t), intent(out) :: self
    end subroutine

    module subroutine exchange_halo(self)
      implicit none
      class(subdomain_2D_t), intent(in) :: self
    end subroutine

    module subroutine allocate_halo_coarray()
      implicit none
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

    module subroutine copy(lhs, rhs)
      implicit none
      class(subdomain_2D_t), intent(out) :: lhs
      type(subdomain_2D_t), intent(in) :: rhs
    end subroutine

  end interface

end module

submodule(subdomain_2D_m) subdomain_2D_s
  use assertions_m, only : assert
  implicit none

  real, allocatable :: halo_x(:,:)[:]
  integer, parameter :: west=1, east=2

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
      "subdomain_2D_t%define: num_subdomains <= nx-nx_boundaries")
    me = this_image()
    num_subdomains = num_images()

    my_nx = nx/num_subdomains + merge(1, 0, me <= mod(nx, num_subdomains))

    if (allocated(self%s_)) deallocate(self%s_)
    allocate(self%s_(my_nx, ny))

    my_internal_left = merge(2, 1, me==1)
    my_internal_right = merge(my_nx-1, my_nx, me==num_subdomains)

    self%s_(my_internal_left:my_internal_right, 1) = boundary_val ! bottom subdomain_2D boundary
    self%s_(my_internal_left:my_internal_right, ny) = boundary_val ! top subdomain_2D boundary
    self%s_(my_internal_left:my_internal_right, 2:ny-1) = internal_val ! internal points

    if (me == 1) then
      self%s_(1, 1:ny) = boundary_val ! left domain boundary
    else
      self%s_(1, 2:ny-1) = internal_val ! left subdomain_2D boundary
    end if

    if (me == num_subdomains) then
      self%s_(my_nx, 1:ny) = boundary_val ! right domain boundary
    else
      self%s_(my_nx, 2:ny-1) = internal_val ! right subdomain_2D boundary
    end if

  end procedure

  module procedure allocate_halo_coarray
    if (allocated(halo_x)) deallocate(halo_x)
    allocate(halo_x(west:east, ny)[*])
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
    call assert(i+1<=my_nx,"laplacian: leftmost subdomain_2D too small")
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
    call assert(i-1>0,"laplacian: rightmost subdomain_2D too small")
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

  module procedure copy
    call assert(allocated(rhs%s_), "subdomain_2D_t%copy: allocated(rhs%s_)")
    lhs%s_ =  rhs%s_
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_2D_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

  subroutine exchange_halo(self)
    class(subdomain_2D_t), intent(in) :: self
    if (me>1) halo_x(east,:)[me-1] = self%s_(1,:)
    if (me<num_subdomains) halo_x(west,:)[me+1] = self%s_(my_nx,:)
  end subroutine

end submodule subdomain_2D_s

program heat_equation
  use subdomain_2D_m, only : subdomain_2D_t
  use iso_fortran_env, only : int64
  implicit none
  type(subdomain_2D_t) T
  integer, parameter :: nx = 4096, ny = nx, steps = 50
  real, parameter :: alpha = 1.
  real T_sum
  integer(int64) t_start, t_finish, clock_rate 
  integer step

  call T%define(side=1., boundary_val=1., internal_val=2., n=nx)
  call T%allocate_halo_coarray

  associate(dt => T%dx()*T%dy()/(4*alpha))

    call system_clock(t_start)

    do step = 1, steps
      call T%exchange_halo
      sync all
      T =  T + dt * alpha * .laplacian. T
      sync all
    end do

  end associate

  T_sum = sum(T%values())
  call co_sum(T_sum, result_image=1)

  call system_clock(t_finish, clock_rate)
  if (this_image()==1) then
    print *, "walltime: ", real(t_finish - t_start) / real(clock_rate)
    print *,"T_avg = ", T_sum/(nx*ny)
  end if
end program
