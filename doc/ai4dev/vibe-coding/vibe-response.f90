pure module function laplacian(rhs) result(laplacian_rhs)
  implicit none
  class(subdomain_t), intent(in) :: rhs
  type(subdomain_t) :: laplacian_rhs
  integer :: i, j, k

  ! Step 1: Empty module procedure with comments explaining the computation
  ! This function computes the 2nd-order central difference approximation 
  ! to the Laplacian operator applied to the s_ component of the input subdomain

  ! Step 2: Allocate the result's s_ component to shape [my_nx, ny, nz]
  allocate(laplacian_rhs%s_(my_nx, ny, nz))

  ! Step 3: Compute 2nd-order central difference Laplacian using do concurrent
  ! Handle interior points (excluding boundaries and halo regions)
  do concurrent(i=2:my_nx-1, j=2:ny-1, k=2:nz-1) default(none) shared(laplacian_rhs, rhs)
    ! Step 4: Store Laplacian approximation in s_ component of result
    ! Laplacian = d²/dx² + d²/dy² + d²/dz²
    laplacian_rhs%s_(i,j,k) = &
      (rhs%s_(i-1,j,k) - 2*rhs%s_(i,j,k) + rhs%s_(i+1,j,k))/dx_**2 + &
      (rhs%s_(i,j-1,k) - 2*rhs%s_(i,j,k) + rhs%s_(i,j+1,k))/dy_**2 + &
      (rhs%s_(i,j,k-1) - 2*rhs%s_(i,j,k) + rhs%s_(i,j,k+1))/dz_**2
  end do

  ! Handle western boundary points (i=1) with halo exchange
  if (me /= 1) then
    ! Step 5: Use halo_x(west,:,:) when me==1 and accessing rhs%s_(1,:,:)
    do concurrent(j=2:ny-1, k=2:nz-1) default(none) shared(laplacian_rhs, rhs)
      laplacian_rhs%s_(1,j,k) = &
        (halo_x(west,j,k) - 2*rhs%s_(1,j,k) + rhs%s_(2,j,k))/dx_**2 + &
        (rhs%s_(1,j-1,k) - 2*rhs%s_(1,j,k) + rhs%s_(1,j+1,k))/dy_**2 + &
        (rhs%s_(1,j,k-1) - 2*rhs%s_(1,j,k) + rhs%s_(1,j,k+1))/dz_**2
    end do
  end if

  ! Handle eastern boundary points (i=my_nx) with halo exchange
  if (me /= num_subdomains) then
    ! Step 5: Use halo_x(east,:,:) when me==num_subdomains and accessing rhs%s_(my_nx,:,:)
    do concurrent(j=2:ny-1, k=2:nz-1) default(none) shared(laplacian_rhs, rhs)
      laplacian_rhs%s_(my_nx,j,k) = &
        (rhs%s_(my_nx-1,j,k) - 2*rhs%s_(my_nx,j,k) + halo_x(east,j,k))/dx_**2 + &
        (rhs%s_(my_nx,j-1,k) - 2*rhs%s_(my_nx,j,k) + rhs%s_(my_nx,j+1,k))/dy_**2 + &
        (rhs%s_(my_nx,j,k-1) - 2*rhs%s_(my_nx,j,k) + rhs%s_(my_nx,j,k+1))/dz_**2
    end do
  end if

  ! Step 6: Set boundary conditions - zero Laplacian at domain boundaries
  laplacian_rhs%s_(:, 1, :) = 0.  ! j=1 boundary
  laplacian_rhs%s_(:, ny, :) = 0. ! j=ny boundary
  laplacian_rhs%s_(:, :, 1) = 0.  ! k=1 boundary
  laplacian_rhs%s_(:, :, nz) = 0. ! k=nz boundary

  ! Step 7: Set western domain boundary to zero if this is the first subdomain
  if (me == 1) then
    laplacian_rhs%s_(1, :, :) = 0.
  end if

  ! Step 8: Set eastern domain boundary to zero if this is the last subdomain
  if (me == num_subdomains) then
    laplacian_rhs%s_(my_nx, :, :) = 0.
  end if

end function
