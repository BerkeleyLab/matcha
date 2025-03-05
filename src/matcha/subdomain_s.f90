submodule(subdomain_m) subdomain_s
  use sourcery_m, only : data_partition_t
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

  real, allocatable :: halo_x(:,:,:)[:]
  integer, parameter :: west=1, east=2

  type(data_partition_t) data_partition

  real dx_, dy_, dz_
  integer my_nx, nx, ny, nz, me, num_subdomains, my_internal_west, my_internal_east
  real, allocatable :: increment(:,:,:)

contains

  module procedure dt_stable
    !! Set the time step at 90% of the stability limit obtained generalizing to 3D the value provided for 2D by
    !! Kassinos, S., & Alexiadis, A. (2024). Beyond Language: Applying MLX Transformers to Engineering Physics. 
    !! arXiv preprint arXiv:2410.04167.
    my_dt = 0.9 * (1./(1./dx_**2 + 1./dy_**2 + 1./dz_**2)) *  (1./(2.*alpha))
  end procedure

  module procedure define

    integer, parameter :: nx_boundaries = 2

    nx = n
    ny = nx
    nz = nx

    dx_ = side/(nx-1)
    dy_ = dx_
    dz_ = dx_

    call assert(num_subdomains <= nx-nx_boundaries, &
      "subdomain_t%define: num_subdomains <= nx-nx_boundaries", intrinsic_array_t([nx, num_subdomains]))
    me = this_image()
    num_subdomains = num_images()

    call data_partition%define_partitions(nx)
    my_nx = data_partition%last(me) - data_partition%first(me) + 1

    if (allocated(self%s_)) deallocate(self%s_)
    allocate(self%s_(my_nx, ny, nz))

    my_internal_west = merge(2, 1, me==1)
    my_internal_east = merge(my_nx-1, my_nx, me==num_subdomains)

    self%s_(my_internal_west:my_internal_east, 2:ny-1, 2:nz-1) = internal_val ! internal points

    self%s_(:, : , 1 ) = boundary_val ! minimum z boundary
    self%s_(:, : , nz) = boundary_val ! maximum z boundary
    self%s_(:, 1 , : ) = boundary_val ! minimum y boundary
    self%s_(:, ny, : ) = boundary_val ! maximum y boundary

    if (me == 1)              self%s_(1    , :, :) = boundary_val ! minimum x boundary
    if (me == num_subdomains) self%s_(my_nx, :, :) = boundary_val ! maximum x boundary

    if (allocated(halo_x)) deallocate(halo_x)
    allocate(halo_x(west:east, ny, nz)[*])
    if (me>1) halo_x(east,:,:)[me-1] = self%s_(1,:,:)
    if (me<num_subdomains) halo_x(west,:,:)[me+1] = self%s_(my_nx,:,:)
    sync all
  end procedure

  module procedure dx
    my_dx = dx_
  end procedure

  module procedure dy
    my_dy = dy_
  end procedure

  module procedure dz
    my_dz = dz_
  end procedure

  module procedure laplacian

    integer i, j, k
    real, allocatable :: halo_west(:,:), halo_east(:,:)

    call assert(allocated(rhs%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")

    allocate(laplacian_rhs%s_(my_nx, ny, nz))

    halo_west = merge(halo_x(west,:,:), rhs%s_(1,:,:), me/=1)
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

    halo_east = merge(halo_x(east,:,:), rhs%s_(my_nx,:,:), me/=num_subdomains)
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
    if (me>1) halo_x(east,:,:)[me-1] = rhs%s_(1,:,:)
    if (me<num_subdomains) halo_x(west,:,:)[me+1] = rhs%s_(my_nx,:,:)
    sync all
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

  module procedure step

    call assert(allocated(self%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")
    call assert(my_internal_west+1<=my_nx,"laplacian: westernmost subdomain too small")
    call assert(my_internal_east-1>0,"laplacian: easternmost subdomain too small")

    if (.not. allocated(increment)) allocate(increment(my_nx,ny,nz))
 
    call internal_points(increment)
    call edge_points(increment)
    call apply_boundary_condition(increment)

    sync all
    self%s_ = self%s_ + increment
    sync all
    call exchange_halo(self%s_)

  contains

    subroutine internal_points(ds)
      real, intent(inout) :: ds(:,:,:)
      integer i, j, k

      do concurrent(i=my_internal_west+1:my_internal_east-1, j=2:ny-1, k=2:nz-1)
        ds(i,j,k) = alpha_dt*( &
          (self%s_(i-1,j  ,k  ) - 2*self%s_(i,j,k) + self%s_(i+1,j  ,k  ))/dx_**2 + &
          (self%s_(i  ,j-1,k  ) - 2*self%s_(i,j,k) + self%s_(i  ,j+1,k  ))/dy_**2 + &
          (self%s_(i  ,j  ,k-1) - 2*self%s_(i,j,k) + self%s_(i  ,j  ,k+1))/dz_**2 &
        )
      end do
    end subroutine

    subroutine edge_points(ds)
      real, intent(inout) :: ds(:,:,:)
      real, allocatable :: halo_west(:,:), halo_east(:,:)
      integer i, j, k

      halo_west = merge(halo_x(west,:,:), self%s_(1,    :,:), me/=1)
      halo_east = merge(halo_x(east,:,:), self%s_(my_nx,:,:), me/=num_subdomains)

      i = my_internal_west
      do concurrent(j=2:ny-1,k=2:nz-1)
        ds(i,j,k) = alpha_dt*( &
          (halo_west(j  ,k  ) - 2*self%s_(i,j,k) + self%s_(i+1,j  ,k  ))/dx_**2 + &
          (self%s_(i,j-1,k  ) - 2*self%s_(i,j,k) + self%s_(i  ,j+1,k  ))/dy_**2 + &
          (self%s_(i,j  ,k-1) - 2*self%s_(i,j,k) + self%s_(i  ,j  ,k+1))/dz_**2 &
        )
      end do

      i = my_internal_east
      do concurrent(j=2:ny-1, k=2:nz-1)
        ds(i,j,k) = alpha_dt*( &
          (self%s_(i-1,j  ,k  )  - 2*self%s_(i,j,k) + halo_east(j  ,k  ))/dx_**2 + &
          (self%s_(i  ,j-1,k  )  - 2*self%s_(i,j,k) + self%s_(i,j+1,k  ))/dy_**2 + &
          (self%s_(i  ,j  ,k-1)  - 2*self%s_(i,j,k) + self%s_(i,j  ,k+1))/dz_**2 &
        )
      end do
    end subroutine

    subroutine apply_boundary_condition(ds)
      real, intent(inout) :: ds(:,:,:)
      integer i, j

      ds(:,1:ny:ny-1, :       ) = 0.
      ds(:, :       ,1:nz:nz-1) = 0.
      if (me==1) ds(1,:,:) = 0.
      if (me==num_subdomains) ds(my_nx,:,:) = 0.
    end subroutine

    subroutine exchange_halo(s)
      real, intent(in) :: s(:,:,:)
      if (me>1) halo_x(east,:,:)[me-1] = s(1,:,:)
      if (me<num_subdomains) halo_x(west,:,:)[me+1] = s(my_nx,:,:)
    end subroutine

  end procedure

end submodule subdomain_s