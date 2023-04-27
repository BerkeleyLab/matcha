program heat
  implicit none

  real dx, dy, dt
  real, parameter :: alpha = 1.
  real, allocatable :: T(:,:), halo_x(:,:)[:]
  integer, parameter :: west=1, east=2, nx=11, ny=nx, steps = 1000
  integer me, my_nx, num_subdomains, my_internal_west, my_internal_east, i

  me = this_image()
  num_subdomains = num_images()

  call define(side=1., boundary_val=1., internal_val=2., n=nx) ! 2D step function
  sync all

  dt = dx*dy/(4*alpha)

  do i = 1, steps
    call step(T, dt)
  end do

  critical
    do i = 1, size(T,2)
      print *,"image ",me,": ", T(:,i)
    end do
  end critical

contains

  pure subroutine assert(assertion, description)
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) error stop description
  end subroutine

  subroutine exchange_halo(s)
    real, intent(in) :: s(:,:)
    if (me>1) halo_x(east,:)[me-1] = s(1,:)
    if (me<num_subdomains) halo_x(west,:)[me+1] = s(my_nx,:)
  end subroutine

  subroutine define(side, boundary_val, internal_val, n)
    real, intent(in) :: side, boundary_val, internal_val
    integer, intent(in) :: n !! number of grid points in each coordinate direction
    integer, parameter :: nx_boundaries = 2 

    call assert(num_subdomains <= n-nx_boundaries, "define: num_subdomains <= n-nx_boundaries")

    dx = side/(nx-1)
    dy = dx 
    my_nx = nx/num_subdomains + merge(1, 0, me <= mod(nx, num_subdomains))
    my_internal_west = merge(2, 1, me==1)
    my_internal_east = merge(my_nx-1, my_nx, me==num_subdomains)

    allocate(T(my_nx, ny))

    T(my_internal_west:my_internal_east, 1) = boundary_val ! bottom subdomain boundary
    T(my_internal_west:my_internal_east, ny) = boundary_val ! top subdomain boundary
    T(my_internal_west:my_internal_east, 2:ny-1) = internal_val ! internal points
    T(1, 2:ny-1) = merge(boundary_val, internal_val, me==1) ! west subdomain boundary
    T(my_nx, 2:ny-1) = merge(boundary_val, internal_val, me==num_subdomains) ! east subdomain boundary

    if (allocated(halo_x)) deallocate(halo_x)
    allocate(halo_x(west:east, ny)[*])
    call exchange_halo(T)
  end subroutine

  subroutine internal_points(alpha_dt_Laplacian_T)
    real, intent(inout) :: alpha_dt_Laplacian_T(:,:)
    integer i, j

    do concurrent(i=my_internal_west+1:my_internal_east-1, j=2:ny-1)
      alpha_dt_Laplacian_T(i,j) = alpha*dt*( &
        (T(i-1,j) - 2*T(i,j) + T(i+1,j))/dx**2 + &
        (T(i,j-1) - 2*T(i,j) + T(i,j+1))/dy**2 &
      )
    end do

  end subroutine

  subroutine edge_points(ds)
    real, intent(inout) :: ds(:,:)
    real, allocatable :: halo_west(:), halo_east(:)
    integer i, j

    halo_west = merge(halo_x(west,:), T(1,:), me/=1)
    halo_east = merge(halo_x(east,:), T(my_nx,:), me/=num_subdomains)

    i = my_internal_west
    do concurrent(j=2:ny-1)
      ds(i,j) = alpha*dt*( &
        (halo_west(j)   - 2*T(i,j) + T(i+1,j))/dx**2 + &
        (T(i,j-1)  - 2*T(i,j) + T(i,j+1))/dy**2 &
      )
    end do

    i = my_internal_east
    do concurrent(j=2:ny-1)
      ds(i,j) = alpha*dt*( &
        (T(i-1,j)  - 2*T(i,j) + halo_east(j))/dx**2 + &
        (T(i,j-1) - 2*T(i,j) + T(i,j+1))/dy**2 &
      )
    end do
  end subroutine

  subroutine apply_boundary_condition(ds)
    real, intent(inout) :: ds(:,:)
    ds(:, 1) = 0.
    ds(:,ny) = 0.
    if (me==1) ds(1,:) = 0.
    if (me==num_subdomains) ds(my_nx,:) = 0.
  end subroutine

  subroutine step(T, dt)
    real, intent(inout), allocatable :: T(:,:)
    real, intent(in) :: dt
    real, allocatable :: increment(:,:)

    call assert(allocated(T), "laplacian: allocated(T)")
    call assert(allocated(halo_x), "laplacian: allocated(halo_x)")
    call assert(my_internal_west+1<=my_nx, "laplacian: westernmost size")
    call assert(my_internal_east-1>0, "laplacian: subdomain size")

    allocate(increment(my_nx,ny))
 
    call internal_points(increment)
    call edge_points(increment)
    call apply_boundary_condition(increment)

    sync all
    T = T + increment
    sync all
    call exchange_halo(T)

  end subroutine

end program
