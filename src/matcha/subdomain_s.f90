submodule(subdomain_m) subdomain_s
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
      "subdomain_t%define: num_subdomains <= nx-nx_boundaries", intrinsic_array_t([nx, num_subdomains]))
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

    call assert(allocated(rhs%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")

    allocate(laplacian_rhs%s_(my_nx, ny))

    if (me==1) then
      halo_left = rhs%s_(1,:)
    else
      halo_left = halo_x(west,:)
    end if
    i=my_internal_left
    do concurrent(j=2:ny-1)
      laplacian_rhs%s_(i,j) = (halo_left(j)   - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                              (rhs%s_(i,j-1)  - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

    do concurrent(i=my_internal_left+1:my_internal_right-1, j=2:ny-1)
      laplacian_rhs%s_(i,j) = (rhs%s_(i-1,j) - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                              (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

    if (me==num_subdomains) then
      halo_right = rhs%s_(my_nx,:)
    else
      halo_right = halo_x(east,:)
    end if 
    i=my_internal_right
    do concurrent(j=2:ny-1) ! j = 1 or ny is always a boundary
      laplacian_rhs%s_(i,j) = (rhs%s_(i-1,j) - 2*rhs%s_(i,j) + halo_right(j))/dx_**2 + &
                              (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do

  end procedure

  module procedure multiply
    call assert(allocated(rhs%s_), "subdomain_t%multiply: allocated(rhs%s_)")
    product%s_ =  lhs * rhs%s_
  end procedure

  module procedure add
    call assert(allocated(rhs%s_), "subdomain_t%add: allocated(rhs%s_)")
    total%s_ =  lhs%s_ + rhs%s_
  end procedure

  module procedure assign_and_sync
    call assert(allocated(rhs%s_), "subdomain_t%assign_and_sync: allocated(rhs%s_)")
    sync all
    lhs%s_ =  rhs%s_
    sync all
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

end submodule subdomain_s
