module procedure laplacian
  ! Separate module procedure for pure function laplacian
  ! Computes 2nd-order central-difference Laplacian operator
  
  integer :: i, j, k
  real, allocatable :: s_west(:,:), s_east(:,:)
  
  ! Allocate result array
  allocate(laplacian_rhs%s_(my_nx, ny, nz))
  
  ! Initialize result to zero
  laplacian_rhs%s_ = 0.0
  
  ! Prepare halo data for boundary conditions
  allocate(s_west(ny, nz), s_east(ny, nz))
  s_west = merge(halo_x(west,:,:), 0.0, me == 1)
  s_east = merge(halo_x(east,:,:), 0.0, me == num_subdomains)
  
  ! Compute Laplacian using do concurrent with proper boundary handling
  do concurrent (i = 1:my_nx, j = 1:ny, k = 1:nz) &
    default(none) shared(laplacian_rhs, rhs, s_west, s_east) &
    local(i, j, k)
    
    ! Apply boundary conditions - result vanishes at boundaries
    laplacian_rhs%s_(i,j,k) = merge(0.0, &
      ! X-direction second derivative
      merge(s_west(j,k), rhs%s_(i-1,j,k), i == 1 .and. me == 1) + &
      merge(s_east(j,k), rhs%s_(i+1,j,k), i == my_nx .and. me == num_subdomains) + &
      merge(rhs%s_(i-1,j,k), 0.0, i > 1 .or. me > 1) + &
      merge(rhs%s_(i+1,j,k), 0.0, i < my_nx .or. me < num_subdomains) - &
      2.0 * rhs%s_(i,j,k) + &
      ! Y-direction second derivative  
      merge(rhs%s_(i,j-1,k) + rhs%s_(i,j+1,k) - 2.0 * rhs%s_(i,j,k), 0.0, &
            j > 1 .and. j < ny) + &
      ! Z-direction second derivative
      merge(rhs%s_(i,j,k-1) + rhs%s_(i,j,k+1) - 2.0 * rhs%s_(i,j,k), 0.0, &
            k > 1 .and. k < nz), &
      ! Boundary condition mask
      j == 1 .or. j == ny .or. k == 1 .or. k == nz .or. &
      (i == 1 .and. me == 1) .or. (i == my_nx .and. me == num_subdomains))
      
  end do
  
end procedure laplacian
