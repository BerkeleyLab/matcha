pure module function laplacian(rhs) result(laplacian_rhs)
  implicit none
  class(subdomain_t), intent(in) :: rhs
  type(subdomain_t) :: laplacian_rhs
  
  ! Local variables for array bounds and grid spacing
  integer :: my_nx, ny, nz, me, num_subdomains
  real :: dx_val, dy_val, dz_val, dx2_inv, dy2_inv, dz2_inv
  
  ! Get array dimensions and parallel image information
  my_nx = size(rhs%s_, 1)
  ny = size(rhs%s_, 2) 
  nz = size(rhs%s_, 3)
  me = this_image()
  num_subdomains = num_images()
  
  ! Get grid spacing values
  dx_val = rhs%dx()
  dy_val = rhs%dy() 
  dz_val = rhs%dz()
  
  ! Compute inverse squared grid spacings for efficiency
  dx2_inv = 1.0 / (dx_val * dx_val)
  dy2_inv = 1.0 / (dy_val * dy_val)
  dz2_inv = 1.0 / (dz_val * dz_val)
  
  ! Allocate result array with same shape as input
  allocate(laplacian_rhs%s_(my_nx, ny, nz))
  
  ! Initialize result to zero
  laplacian_rhs%s_ = 0.0
  
  ! Compute 2nd-order central difference Laplacian in interior points
  ! Laplacian = d²/dx² + d²/dy² + d²/dz²
  do concurrent (i = 2:my_nx-1, j = 2:ny-1, k = 2:nz-1) default(none) &
    shared(laplacian_rhs, rhs, dx2_inv, dy2_inv, dz2_inv, my_nx, ny, nz, me, num_subdomains)
    
    ! Second derivative in x-direction: (f(i+1) - 2*f(i) + f(i-1))/dx²
    ! Second derivative in y-direction: (f(j+1) - 2*f(j) + f(j-1))/dy²  
    ! Second derivative in z-direction: (f(k+1) - 2*f(k) + f(k-1))/dz²
    laplacian_rhs%s_(i,j,k) = &
      dx2_inv * (rhs%s_(i+1,j,k) - 2.0*rhs%s_(i,j,k) + rhs%s_(i-1,j,k)) + &
      dy2_inv * (rhs%s_(i,j+1,k) - 2.0*rhs%s_(i,j,k) + rhs%s_(i,j-1,k)) + &
      dz2_inv * (rhs%s_(i,j,k+1) - 2.0*rhs%s_(i,j,k) + rhs%s_(i,j,k-1))
  end do
  
  ! Handle boundary conditions at subdomain interfaces for x-direction
  ! Left boundary (i=1) - only compute if not the first subdomain
  if (me > 1) then
    do concurrent (j = 2:ny-1, k = 2:nz-1) default(none) &
      shared(laplacian_rhs, rhs, dx2_inv, dy2_inv, dz2_inv, ny, nz, halo_x)
      
      laplacian_rhs%s_(1,j,k) = &
        dx2_inv * (rhs%s_(2,j,k) - 2.0*rhs%s_(1,j,k) + halo_x(west,j,k)) + &
        dy2_inv * (rhs%s_(1,j+1,k) - 2.0*rhs%s_(1,j,k) + rhs%s_(1,j-1,k)) + &
        dz2_inv * (rhs%s_(1,j,k+1) - 2.0*rhs%s_(1,j,k) + rhs%s_(1,j,k-1))
    end do
  end if
  
  ! Right boundary (i=my_nx) - only compute if not the last subdomain  
  if (me < num_subdomains) then
    do concurrent (j = 2:ny-1, k = 2:nz-1) default(none) &
      shared(laplacian_rhs, rhs, dx2_inv, dy2_inv, dz2_inv, my_nx, ny, nz, halo_x, num_subdomains)
      
      laplacian_rhs%s_(my_nx,j,k) = &
        dx2_inv * (halo_x(east,j,k) - 2.0*rhs%s_(my_nx,j,k) + rhs%s_(my_nx-1,j,k)) + &
        dy2_inv * (rhs%s_(my_nx,j+1,k) - 2.0*rhs%s_(my_nx,j,k) + rhs%s_(my_nx,j-1,k)) + &
        dz2_inv * (rhs%s_(my_nx,j,k+1) - 2.0*rhs%s_(my_nx,j,k) + rhs%s_(my_nx,j,k-1))
    end do
  end if
  
  ! Set boundary values to zero as specified
  ! Y boundaries
  laplacian_rhs%s_(:, 1, :) = 0.0
  laplacian_rhs%s_(:, ny, :) = 0.0
  
  ! Z boundaries  
  laplacian_rhs%s_(:, :, 1) = 0.0
  laplacian_rhs%s_(:, :, nz) = 0.0
  
  ! X boundaries for first and last subdomains
  if (me == 1) then
    laplacian_rhs%s_(1, :, :) = 0.0
  end if
  
  if (me == num_subdomains) then
    laplacian_rhs%s_(my_nx, :, :) = 0.0
  end if

end function laplacian
