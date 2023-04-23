submodule(subdomain_m) subdomain_s
  use data_partition_m, only : data_partition_t
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use iso_fortran_env, only : event_type
  implicit none

  real, allocatable, target :: halo_x(:,:)[:]
  real, allocatable :: halo_west(:), halo_east(:)
  integer, parameter :: west=1, east=2

  type(data_partition_t) data_partition
  type(event_type) halo_x_ready(west:east)[*], halo_x_picked_up(west:east)[*]

  real dx_, dy_
  integer my_nx, nx, ny, me, num_subdomains, my_internal_west, my_internal_east

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

    my_internal_west = merge(2, 1, me==1)
    my_internal_east = merge(my_nx-1, my_nx, me==num_subdomains)

    self%s_(my_internal_west:my_internal_east, 1) = boundary_val ! bottom subdomain boundary
    self%s_(my_internal_west:my_internal_east, ny) = boundary_val ! top subdomain boundary
    self%s_(my_internal_west:my_internal_east, 2:ny-1) = internal_val ! internal points
    self%s_(1, 2:ny-1) = merge(boundary_val, internal_val, me==1) ! left subdomain boundary
    self%s_(my_nx, 2:ny-1) = merge(boundary_val, internal_val, me==num_subdomains) ! right subdomain boundary

    if (allocated(halo_x)) deallocate(halo_x)
    allocate(halo_x(west:east, ny)[*])

    send_east: &
    if (me>1) then
      halo_x(east,:)[me-1] = self%s_(1,:)
      event post(halo_x_ready(east)[me-1])
    end if send_east

    send_west: &
    if (me<num_subdomains) then
      halo_x(west,:)[me+1] = self%s_(my_nx,:)
      event post(halo_x_ready(west)[me+1])
    end if send_west

    pick_up_east: &
    if (me>1) then
      event wait(halo_x_ready(west))
      halo_west = halo_x(west,:)
      event post(halo_x_picked_up(west)[me-1])
    else
      halo_west = self%s_(1,:)
    end if pick_up_east

    pick_up_west: &
    if (me < num_subdomains) then
      event wait(halo_x_ready(east))
      halo_east = halo_x(east,:)
      event post(halo_x_picked_up(east)[me+1])
    else
      halo_east = self%s_(my_nx,:)
    end if pick_up_west
    
  end procedure

  module procedure dx
    my_dx = dx_
  end procedure

  module procedure dy
    my_dy = dy_
  end procedure

  module procedure laplacian

    integer i, j

    call assert(allocated(rhs%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")
    call assert(allocated(halo_west), "subdomain_t%laplacian: allocated(halo_west)")
    call assert(allocated(halo_east), "subdomain_t%laplacian: allocated(halo_east)")
    call assert(my_internal_west+1<=my_nx, "laplacian: westernmost subdomain too small")
    call assert(my_internal_east-1>0, "laplacian: easternmost subdomain too small")

    allocate(laplacian_rhs%s_(my_nx, ny))

    ! preserve Dirichlet boundary conditions
    laplacian_rhs%s_(:, 1) = 0. ! bottom boundary
    laplacian_rhs%s_(:,ny) = 0. ! top boundary
    if (me==1) laplacian_rhs%s_(1,:) = 0. ! west boundary
    if (me==num_subdomains) laplacian_rhs%s_(my_nx,:) = 0. ! east boundary

    compute_internal_points: &
    do concurrent(i=my_internal_west+1:my_internal_east-1, j=2:ny-1)
      laplacian_rhs%s_(i,j) = (rhs%s_(i-1,j) - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                              (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do compute_internal_points

    i = my_internal_west
    compute_westernmost_points: &
    do concurrent(j=2:ny-1)
      laplacian_rhs%s_(i,j) = (halo_west(j)   - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                              (rhs%s_(i,j-1)  - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do compute_westernmost_points

    i = my_internal_east
    compute_easternmost_points: &
    do concurrent(j=2:ny-1)
      laplacian_rhs%s_(i,j) = (rhs%s_(i-1,j)  - 2*rhs%s_(i,j) + halo_east(j))/dx_**2 + &
                              (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
    end do compute_easternmost_points
   
  end procedure

  module procedure multiply
    call assert(allocated(rhs%s_), "subdomain_t%multiply: allocated(rhs%s_)")
    product%s_ =  lhs * rhs%s_
  end procedure

  module procedure add
    call assert(allocated(rhs%s_), "subdomain_t%add: allocated(rhs%s_)")
    total%s_ =  lhs%s_ + rhs%s_
  end procedure

  module procedure assign_wait_post

    call assert(allocated(rhs%s_), "subdomain_t%wait_assign_post: allocated(rhs%s_)")

    lhs%s_ =  rhs%s_

    send_east: &
    if (me>1) then
      event wait(halo_x_picked_up(east))
      halo_x(east,:)[me-1] = rhs%s_(1,:)
      event post(halo_x_ready(east)[me-1])
    end if send_east

    send_west: &
    if (me<num_subdomains) then
      event wait(halo_x_picked_up(west))
      halo_x(west,:)[me+1] = rhs%s_(my_nx,:)
      event post(halo_x_ready(west)[me+1])
    end if send_west

    pick_up_east: &
    if (me>1) then
      event wait(halo_x_ready(west))
      halo_west = halo_x(west,:)
      event post(halo_x_picked_up(west)[me-1])
    else
      halo_west = rhs%s_(1,:)
    end if pick_up_east

    pick_up_west: &
    if (me < num_subdomains) then
      event wait(halo_x_ready(east))
      halo_east = halo_x(east,:)
      event post(halo_x_picked_up(east)[me+1])
    else
      halo_east = rhs%s_(my_nx,:)
    end if pick_up_west

  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

end submodule subdomain_s
