! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.tx
submodule(gridr_m) gridr_s
  use intrinsic_array_m, only : intrinsic_array_t
  
!#ifdef USE_CAFFEINE
!   use caffeine_assert_m, only : assert
!#else
!   use assert_m, only : assert
!#endif
  
  implicit none

contains
  
  module procedure construct
  end procedure construct

  module procedure gridparameters

    gridp(1) = (ge - gb)/dble(ng(1))
    gridp(2) = (ge - gb)/dble(ng(2))
    gridp(3) = (ge - gb)/dble(ng(3))

    gridp(4) = 1.d0/(2.d0*gridp(1))
    gridp(5) = 1.d0/(2.d0*gridp(2))
    gridp(6) = 1.d0/(2.d0*gridp(3))

    gridp(7) = 1.d0/(gridp(1)*gridp(2)*gridp(3))
    
  end procedure gridparameters
  

  module procedure gradient
    integer i,j,k,ii,jj,kk
    double precision, allocatable :: concentration(:,:,:)
    double precision, allocatable :: grad(:,:,:,:)

    allocate(concentration(ng(1),ng(2),ng(3)))
    allocate(grad(ng(1),ng(2),ng(3),3))    

    concentration = 0.d0
    grad = 0.d0
    
    do i = 1,my_num_cells
       ii = int((x(i,1) - gb)/dx(1)) + 1
       jj = int((x(i,2) - gb)/dx(2)) + 1
       kk = int((x(i,3) - gb)/dx(3)) + 1
       concentration(ii,jj,kk) = concentration(ii,jj,kk) + tconc
    end do

    call co_sum(concentration)

    do concurrent(i = 2:ng(1)-1, j = 2:ng(2)-1, k = 2:ng(3)-1)  
       grad(i,j,k,1) = dx(4)*(concentration(i+1,j,k)-concentration(i-1,j,k))
       grad(i,j,k,2) = dx(5)*(concentration(i,j+1,k)-concentration(i,j-1,k))
       grad(i,j,k,3) = dx(6)*(concentration(i,j,k+1)-concentration(i,j,k-1))
    end do

    allocate(gx(my_num_cells,3))
    do concurrent(i = 1:my_num_cells)
       ii = int((x(i,1) - gb)/dx(1)) + 1
       jj = int((x(i,2) - gb)/dx(2)) + 1
       kk = int((x(i,3) - gb)/dx(3)) + 1
       gx(i,:) = grad(ii,jj,kk,:)
    end do
    
  end procedure gradient

end submodule gridr_s
