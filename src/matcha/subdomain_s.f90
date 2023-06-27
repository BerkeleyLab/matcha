submodule(subdomain_m) subdomain_s
  use data_partition_m, only : data_partition_t
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

  double precision, allocatable :: halo_x(:,:,:)[:]
  integer mynxl,mynyl,mynzl
  integer, parameter :: west=1, east=2

  type(data_partition_t) data_partition

  double precision dx_, dy_, dz_
  integer mynx_,myny_,mynz_
  integer nxfirst_, nxlast_
  integer nx_cumulative_

  integer :: nxbefore[*]  
  integer nx, me, num_subdomains, my_internal_left, my_internal_right  

contains

  module procedure define

    integer i
    integer, parameter :: nx_boundaries = 2

    nx = n
    myny_ = n
    mynz_ = n


    dx_ = side/dble(nx-1)
    dy_ = side/dble(myny_-1)
    dz_ = side/dble(mynz_-1)    

    call assert(num_subdomains <= nx-nx_boundaries, &
      "subdomain_t%define: num_subdomains <= nx-nx_boundaries", intrinsic_array_t([nx, num_subdomains]))
    me = this_image()
    num_subdomains = num_images()

    call data_partition%define_partitions(nx)
    mynx_ = data_partition%last(me) - data_partition%first(me) + 1

    nxbefore[me] = mynx_
    sync all

    nx_cumulative_ = 0
    do i = 1,me-1
       nx_cumulative_ = nx_cumulative_ + nxbefore[i]
    end do

    if (allocated(self%s_)) deallocate(self%s_)
    allocate(self%s_(mynx_, myny_, mynz_))    

    my_internal_left = merge(2, 1, me==1)
    my_internal_right = merge(mynx_ - 1, mynx_, me==num_subdomains)    

    nxfirst_ = my_internal_left
    nxlast_ = my_internal_right
    
    self%s_(my_internal_left:my_internal_right, 1,:) = boundary_val ! bottom subdomain boundary
    self%s_(my_internal_left:my_internal_right, myny_,:) = boundary_val ! top subdomain boundary    
    
    self%s_(my_internal_left:my_internal_right,:,1) = boundary_val ! bottom subdomain boundary
    self%s_(my_internal_left:my_internal_right,:,mynz_) = boundary_val ! top subdomain boundary    

    self%s_(my_internal_left:my_internal_right, 2:myny_-1, 2:mynz_-1) = internal_val ! internal points  

    if (me == 1) then
      self%s_(1, 1:myny_, 1:mynz_) = boundary_val ! left domain boundary
    else
      self%s_(1, 2:myny_ - 1, 2:mynz_ - 1) = internal_val ! left subdomain boundary
    end if
    if (me == num_subdomains) then
      self%s_(mynx_, 1:myny_, 1:mynz_) = boundary_val ! right domain boundary
    else
      self%s_(mynx_, 2:myny_ - 1, 2:mynz_ - 1) = internal_val ! right subdomain boundary
    end if

    if (allocated(halo_x)) deallocate(halo_x)
    allocate(halo_x(west:east, myny_, mynz_)[*])
    if (me>1) halo_x(east,:,:)[me-1] = self%s_(1,:,:)
    if (me<num_subdomains) halo_x(west,:,:)[me+1] = self%s_(mynx_,:,:)
    
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

  module procedure mynx
    my_mynx = mynx_
  end procedure

  module procedure myny
    my_myny = myny_
  end procedure

  module procedure mynz
    my_mynz = mynz_
  end procedure

  module procedure nxfirst
    my_nxfirst = nxfirst_
  end procedure

  module procedure nxlast
    my_nxlast = nxlast_
  end procedure

  module procedure nx_cumulative
    my_nx_cumulative = nx_cumulative_
  end procedure
  
  module procedure laplacian

    integer i, j, k
    integer ny,nz
    double precision, allocatable :: halo_left(:,:), halo_right(:,:)

    call assert(allocated(rhs%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")

    allocate(laplacian_rhs%s_(mynx_, myny_, mynz_))

    halo_left = merge(halo_x(west,:,:), rhs%s_(1,:,:), me/=1)
    i = my_internal_left
    call assert(i+1<=mynx_,"laplacian: leftmost subdomain too small")
    do concurrent(j=2:myny_-1, k=2:mynz_-1)
      laplacian_rhs%s_(i,j,k) = (halo_left(j,k)   - 2*rhs%s_(i,j,k) + rhs%s_(i+1,j,k))/dx_**2 + &
                                (rhs%s_(i,j-1,k)  - 2*rhs%s_(i,j,k) + rhs%s_(i,j+1,k))/dy_**2 + &
                                (rhs%s_(i,j,k-1)  - 2*rhs%s_(i,j,k) + rhs%s_(i,j,k+1))/dz_**2      
    end do

    do concurrent(i=my_internal_left+1:my_internal_right-1, j=2:myny_-1, k=2:mynz_-1)
      laplacian_rhs%s_(i,j,k) = (rhs%s_(i-1,j,k) - 2*rhs%s_(i,j,k) + rhs%s_(i+1,j,k))/dx_**2 + &
                                (rhs%s_(i,j-1,k) - 2*rhs%s_(i,j,k) + rhs%s_(i,j+1,k))/dy_**2 + &
                                (rhs%s_(i,j,k-1) - 2*rhs%s_(i,j,k) + rhs%s_(i,j,k+1))/dz_**2           
    end do

    halo_right = merge(halo_x(east,:,:), rhs%s_(mynx_,:,:), me/=num_subdomains)
    i = my_internal_right
    call assert(i-1>0,"laplacian: rightmost subdomain too small")
    do concurrent(j=2:myny_-1, k=2:mynz_-1)
      laplacian_rhs%s_(i,j,k) = (rhs%s_(i-1,j,k) - 2*rhs%s_(i,j,k) + halo_right(j,k))/dx_**2 + &
                                (rhs%s_(i,j-1,k) - 2*rhs%s_(i,j,k) + rhs%s_(i,j+1,k))/dy_**2 + &
                                (rhs%s_(i,j,k-1) - 2*rhs%s_(i,j,k) + rhs%s_(i,j,k+1))/dz_**2      
    end do

    laplacian_rhs%s_(:, 1, :) = 0.
    laplacian_rhs%s_(:,myny_, :) = 0.
    laplacian_rhs%s_(:, :, 1) = 0.
    laplacian_rhs%s_(:, :, mynz_) = 0.
    if (me==1) laplacian_rhs%s_(1,:,:) = 0.
    if (me==num_subdomains) laplacian_rhs%s_(mynx_,:,:) = 0.
    
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
    if (me<num_subdomains) halo_x(west,:,:)[me+1] = rhs%s_(mynx_,:,:)    
    sync all
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

  module procedure step

    double precision, allocatable :: increment(:,:,:)

    call assert(allocated(self%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")
    call assert(my_internal_left+1<=mynx_,"laplacian: leftmost subdomain too small")    
    call assert(my_internal_right-1>0,"laplacian: rightmost subdomain too small")

    allocate(increment(mynx_,myny_,mynz_))    
 
    call internal_points(increment)
    call edge_points(increment)
    call apply_boundary_condition(increment)

    sync all
    self%s_ = self%s_ + increment
    sync all
    call exchange_halo(self%s_)

  contains

    subroutine internal_points(ds)
      double precision, intent(inout) :: ds(:,:,:)
      integer i, j, k

      do concurrent(i=my_internal_left+1:my_internal_right-1, j=2:myny_-1, k=2:mynz_-1)
        ds(i,j,k) = alpha_dt*( &
          (self%s_(i-1,j,k) - 2*self%s_(i,j,k) + self%s_(i+1,j,k))/dx_**2 + &
          (self%s_(i,j-1,k) - 2*self%s_(i,j,k) + self%s_(i,j+1,k))/dy_**2 + &
          (self%s_(i,j,k-1) - 2*self%s_(i,j,k) + self%s_(i,j,k+1))/dz_**2   &       
        )
      end do
     
    end subroutine

    subroutine edge_points(ds)
      double precision, intent(inout) :: ds(:,:,:)
      double precision, allocatable :: halo_left(:,:), halo_right(:,:)
      integer i, j, k

      halo_left = merge(halo_x(west,:,:), self%s_(1,:,:), me/=1)
      halo_right = merge(halo_x(east,:,:), self%s_(mynx_,:,:), me/=num_subdomains)      

      i = my_internal_left
      do concurrent(j=2:myny_-1,k=2:mynz_-1)
        ds(i,j,k) = alpha_dt*( &
          (halo_left(j,k)   - 2*self%s_(i,j,k) + self%s_(i+1,j,k))/dx_**2 + &
          (self%s_(i,j-1,k) - 2*self%s_(i,j,k) + self%s_(i,j+1,k))/dy_**2 + &
          (self%s_(i,j,k-1) - 2*self%s_(i,j,k) + self%s_(i,j,k+1))/dz_**2   &       
        )
      end do

      i = my_internal_right
      do concurrent(j=2:myny_-1,k=2:mynz_-1)
        ds(i,j,k) = alpha_dt*( &
          (self%s_(i-1,j,k) - 2*self%s_(i,j,k) +  halo_right(j,k))/dx_**2  + &
          (self%s_(i,j-1,k) - 2*self%s_(i,j,k) + self%s_(i,j+1,k))/dy_**2 + &
          (self%s_(i,j,k-1) - 2*self%s_(i,j,k) + self%s_(i,j,k+1))/dz_**2 &          
        )
      end do

     
    end subroutine

    subroutine apply_boundary_condition(ds)
      double precision, intent(inout) :: ds(:,:,:)
      integer i, j

      ds(:,1,:) = 0.
      ds(:,myny_,:) = 0.
      ds(:,:,1) = 0.
      ds(:,:,mynz_) = 0.      
      if (me==1) ds(1,:,:) = 0.
      if (me==num_subdomains) ds(mynx_,:,:) = 0.
      
    end subroutine

    subroutine exchange_halo(s)
      double precision, intent(in) :: s(:,:,:)
      if (me>1) halo_x(east,:,:)[me-1] = s(1,:,:)
      if (me<num_subdomains) halo_x(west,:,:)[me+1] = s(mynx_,:,:)
      
    end subroutine

  end procedure

end submodule subdomain_s
