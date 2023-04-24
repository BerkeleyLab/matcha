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

  module procedure assign_and_sync
    call assert(allocated(rhs%s_), "subdomain_t%assign_and_sync: allocated(rhs%s_)")
    sync all
    lhs%s_ =  rhs%s_
    if (me>1) halo_x(east,:)[me-1] = rhs%s_(1,:)
    if (me<num_subdomains) halo_x(west,:)[me+1] = rhs%s_(my_nx,:)
    sync all
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

  module procedure step

    real, allocatable :: increment(:,:)

    call assert(allocated(self%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")
    call assert(my_internal_left+1<=my_nx,"laplacian: leftmost subdomain too small")
    call assert(my_internal_right-1>0,"laplacian: rightmost subdomain too small")

    allocate(increment(my_nx,ny))
 
    call internal_points(increment)
    call edge_points(increment)
    call apply_boundary_condition(increment)

    sync all
    self%s_ = self%s_ + increment
    sync all
    call exchange_halo(self%s_)

  contains

    subroutine internal_points(ds)
      real, intent(inout) :: ds(:,:)
      integer i, j

      do concurrent(i=my_internal_left+1:my_internal_right-1, j=2:ny-1)
        ds(i,j) = alpha_dt*( &
          (self%s_(i-1,j) - 2*self%s_(i,j) + self%s_(i+1,j))/dx_**2 + &
          (self%s_(i,j-1) - 2*self%s_(i,j) + self%s_(i,j+1))/dy_**2 &
        )
      end do
    end subroutine

    subroutine edge_points(ds)
      real, intent(inout) :: ds(:,:)
      real, allocatable :: halo_left(:), halo_right(:)
      integer i, j

      halo_left = merge(halo_x(west,:), self%s_(1,:), me/=1)
      halo_right = merge(halo_x(east,:), self%s_(my_nx,:), me/=num_subdomains)

      i = my_internal_left
      do concurrent(j=2:ny-1)
        ds(i,j) = alpha_dt*( &
          (halo_left(j)   - 2*self%s_(i,j) + self%s_(i+1,j))/dx_**2 + &
          (self%s_(i,j-1)  - 2*self%s_(i,j) + self%s_(i,j+1))/dy_**2 &
        )
      end do

      i = my_internal_right
      do concurrent(j=2:ny-1)
        ds(i,j) = alpha_dt*( &
          (self%s_(i-1,j)  - 2*self%s_(i,j) + halo_right(j))/dx_**2 + &
          (self%s_(i,j-1) - 2*self%s_(i,j) + self%s_(i,j+1))/dy_**2 &
        )
      end do
    end subroutine

    subroutine apply_boundary_condition(ds)
      real, intent(inout) :: ds(:,:)
      integer i, j

      ds(:, 1) = 0.
      ds(:,ny) = 0.
      if (me==1) ds(1,:) = 0.
      if (me==num_subdomains) ds(my_nx,:) = 0.
    end subroutine

    subroutine exchange_halo(s)
      real, intent(in) :: s(:,:)
      if (me>1) halo_x(east,:)[me-1] = s(1,:)
      if (me<num_subdomains) halo_x(west,:)[me+1] = s(my_nx,:)
    end subroutine

  end procedure

end submodule subdomain_s
