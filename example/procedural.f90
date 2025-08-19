program main
  implicit none

  real dx, dy, dt, t_start, t_end, T_min, T_max
  real, parameter :: alpha = 1., T_boundary = 1., T_initial = 2.
  real, allocatable :: T(:,:), halo_x(:,:)[:], laplacian(:,:)
  integer, parameter :: west=1, east=2, nx=51, ny=nx, steps = 3000
  integer me, my_nx, num_subdomains, my_internal_west, my_internal_east, i

  me = this_image()
  num_subdomains = num_images()
  call initialize(T, side=1., boundary_val=T_boundary, internal_val=T_initial, n=nx) ! 2D step function
  allocate(halo_x(west:east, ny)[*]) ! implicit synchronization
  call exchange_halo(T)
  sync all
  dt = dx*dy/(4*alpha)

  call cpu_time(t_start)
  do i = 1, steps
    call step(T, dt)
    call exchange_halo(T)
    sync all
  end do
  call cpu_time(t_end)

  T_min = minval(T)
  T_max = maxval(T)
  call co_min(T_min, result_image=1)
  call co_min(T_max, result_image=1)
  if (me==1) print *,"T_boundary, T_ininitial, T_min, T_max: ", T_boundary, T_initial, T_min, T_max
  print *,"cpu_time: ", t_end - t_start

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

  subroutine initialize(T, side, boundary_val, internal_val, n)
    real, intent(out), allocatable :: T(:,:)
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
  end subroutine

  subroutine step(T, dt)
    real, intent(inout), allocatable :: T(:,:)
    real, intent(in) :: dt
    real, allocatable :: halo_west(:), halo_east(:)
    integer i, j

    call assert(allocated(T), "laplacian: allocated(T)")
    call assert(allocated(halo_x), "laplacian: allocated(halo_x)")
    call assert(my_internal_west+1<=my_nx, "laplacian: westernmost size")
    call assert(my_internal_east-1>0, "laplacian: subdomain size")
    if (.not. allocated(laplacian)) allocate(laplacian(my_nx,ny))

    internal_points: &
    do concurrent(i=my_internal_west+1:my_internal_east-1, j=2:ny-1)
      laplacian(i,j) = ((T(i-1,j) - 2*T(i,j) + T(i+1,j))/dx**2 + &
                        (T(i,j-1) - 2*T(i,j) + T(i,j+1))/dy**2 )
    end do internal_points

    i = my_internal_west
    halo_west = merge(halo_x(west,:), T(1,:), me/=1)

    west_boundary_adjacent_points: &
    do concurrent(j=2:ny-1)
      laplacian(i,j) = ( (halo_west(j) - 2*T(i,j) + T(i+1,j))/dx**2 + &
                         (T(i,j-1)     - 2*T(i,j) + T(i,j+1))/dy**2 )
    end do west_boundary_adjacent_points
    
    i = my_internal_east
    halo_east = merge(halo_x(east,:), T(my_nx,:), me/=num_subdomains)

    east_boundary_adjacent_points: &
    do concurrent(j=2:ny-1)
      laplacian(i,j) = ( (T(i-1,j) - 2*T(i,j) + halo_east(j))/dx**2 + &
                         (T(i,j-1) - 2*T(i,j) + T(i,j+1)    )/dy**2 )
    end do east_boundary_adjacent_points

    T = T + alpha*dt*laplacian
  end subroutine

end program
