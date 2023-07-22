! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module assert_m
  implicit none
contains
  pure subroutine assert(assertion, description)
     logical, intent(in) :: assertion
     character(len=*), intent(in) :: description
     if (.not. assertion) error stop description
  end subroutine
end module assert_m

module data_partition_m
  !! distribute data identification numbers across images such that the number of
  !! items differs by at most 1 between any two images.
  implicit none

  private
  public :: data_partition_t

  type data_partition_t
    !! encapsulate a description of the data subset the executing image owns
    private
  contains
    procedure, nopass :: define_partitions
    procedure, nopass :: first
    procedure, nopass :: last
  end type

  integer, allocatable :: first_datum(:), last_datum(:)

  interface

    module subroutine define_partitions(cardinality)
      !! define the range of data identification numbers owned by the executing image
      integer, intent(in) :: cardinality
    end subroutine

    pure module function first(image_number) result(first_index)
      !! the result is the first identification number owned by the executing image
      implicit none
      integer, intent(in) :: image_number
      integer first_index
    end function

    pure module function last(image_number) result(last_index)
      !! the result is the last identification number owned by the executing image
      implicit none
      integer, intent(in) :: image_number
      integer last_index
    end function

  end interface

end module data_partition_m

submodule(data_partition_m) data_partition_s
  use assert_m, only : assert
  implicit none

  logical, parameter :: verbose=.false.

contains

  module procedure define_partitions

    if (allocated(first_datum)) deallocate(first_datum)
    if (allocated(last_datum)) deallocate(last_datum)

    associate( ni => num_images() )

      call assert( ni<=cardinality, "sufficient data for distribution across images")

      allocate(first_datum(ni), last_datum(ni))

      block
        integer i, image
        do image=1,ni
          associate( remainder => mod(cardinality, ni), quotient => cardinality/ni )
            first_datum(image) = sum([(quotient+overflow(i, remainder), i=1, image-1)]) + 1
            last_datum(image) = first_datum(image) + quotient + overflow(image, remainder) - 1
          end associate
        end do
      end block
    end associate

  contains

    pure function overflow(im, excess) result(extra_datum)
      integer, intent(in) :: im, excess
      integer extra_datum
      extra_datum= merge(1,0,im<=excess)
    end function

  end procedure

  module procedure first
    call assert( allocated(first_datum), "allocated(first_datum)")
    first_index= first_datum( image_number )
  end procedure

  module procedure last
    call assert( allocated(last_datum), "allocated(last_datum)")
    last_index = last_datum( image_number )
  end procedure

end submodule data_partition_s

module subdomain_m
  implicit none

  private
  public :: subdomain_t

  type subdomain_t 
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
      class(subdomain_t), intent(out) :: self
    end subroutine

    module subroutine exchange_halo(self)
      implicit none
      class(subdomain_t), intent(in) :: self
    end subroutine

    module subroutine allocate_halo_coarray()
      implicit none
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

    module subroutine copy(lhs, rhs)
      implicit none
      class(subdomain_t), intent(out) :: lhs
      type(subdomain_t), intent(in) :: rhs
    end subroutine

  end interface

end module

submodule(subdomain_m) subdomain_s
  use data_partition_m, only : data_partition_t
  use assert_m, only : assert
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
      "subdomain_t%define: num_subdomains <= nx-nx_boundaries")
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

    if (me == 1) then
      self%s_(1, 1:ny) = boundary_val ! left domain boundary
    else
      self%s_(1, 2:ny-1) = internal_val ! left subdomain boundary
    end if
    if (me == num_subdomains) then
      self%s_(my_nx, 1:ny) = boundary_val ! right domain boundary
    else
      self%s_(my_nx, 2:ny-1) = internal_val ! right subdomain boundary
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

    call assert(allocated(rhs%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")

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
    call assert(allocated(rhs%s_), "subdomain_t%multiply: allocated(rhs%s_)")
    product%s_ =  lhs * rhs%s_
  end procedure

  module procedure add
    call assert(allocated(rhs%s_), "subdomain_t%add: allocated(rhs%s_)")
    total%s_ =  lhs%s_ + rhs%s_
  end procedure

  module procedure copy
    call assert(allocated(rhs%s_), "subdomain_t%copy: allocated(rhs%s_)")
    lhs%s_ =  rhs%s_
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

  subroutine exchange_halo(self)
    class(subdomain_t), intent(in) :: self
    if (me>1) halo_x(east,:)[me-1] = self%s_(1,:)
    if (me<num_subdomains) halo_x(west,:)[me+1] = self%s_(my_nx,:)
  end subroutine

end submodule subdomain_s

program heat_equation
  use subdomain_m, only : subdomain_t
  implicit none
  type(subdomain_t) T
  integer, parameter :: nx = 4096, ny = nx, steps = 5
  real, parameter :: alpha = 1.
  real T_sum
  integer step

  call T%define(side=1., boundary_val=1., internal_val=2., n=nx)
  call T%allocate_halo_coarray

  associate(dt => T%dx()*T%dy()/(4*alpha))
    do step = 1, steps
      call T%exchange_halo
      sync all
      T =  T + dt * alpha * .laplacian. T
      sync all
    end do
  end associate

  T_sum = sum(T%values())
  call co_sum(T_sum, result_image=1)
  if (this_image()==1) print *,"T_avg = ", T_sum/(nx*ny)
end program
