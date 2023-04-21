submodule(subdomain_m) subdomain_s
  use data_partition_m, only : data_partition_t
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  implicit none

  real, allocatable :: halo_x(:,:)[:]
  integer, parameter :: west=1, east=2

  type(data_partition_t) data_partition

  real dx_, dy_
  integer ny

contains

    module procedure define

      integer, parameter :: nx_boundaries = 2

      associate(nx => (n), me => this_image(), num_subdomains => num_images())
        ny = nx
        dx_ = side/(nx-1)
        dy_ = dx_

        call assert(num_subdomains <= nx-nx_boundaries, &
          "subdomain_t%define: num_subdomains <= nx-nx_boundaries", intrinsic_array_t([nx, num_subdomains]))

        call data_partition%define_partitions(nx)

        associate(my_first => data_partition%first(me), my_last => data_partition%last(me))
          allocate(self%s_(my_first:my_last, ny))
          block
            integer i

            self%s_(my_first:my_last, 1) = boundary_val
            do concurrent(i=2:ny-1)
              self%s_(my_first, i) = merge(boundary_val, internal_val, my_first==1)
              self%s_(my_first+1:my_last-1, i) = internal_val
              self%s_(my_last, i) = merge(boundary_val, internal_val, my_last==nx)
            end do
            self%s_(my_first:my_last, ny) = boundary_val

            allocate(halo_x(west:east, ny)[*])
            if (me>1) halo_x(east,:)[me-1] = self%s_(my_first,:)
            if (me<num_subdomains) halo_x(west,:)[me+1] = self%s_(my_last,:)
            sync all
          end block
        end associate
      end associate

    end procedure

    module procedure dx
      my_dx = dx_
    end procedure

    module procedure dy
      my_dy = dy_
    end procedure

    module procedure laplacian

       call assert(allocated(rhs%s_), "subdomain_t%laplacian: allocated(rhs%s_)")
       call assert(allocated(halo_x), "subdomain_t%laplacian: allocated(halo_x)")

       associate(me => this_image(), num_subdomains => num_images())
         associate(my_first => data_partition%first(me), my_last => data_partition%last(me))
           allocate(laplacian_rhs%s_(my_first:my_last, ny))
           block
             integer i, j
   
             compute_west_boundary: &
             if (me>1) then
               i=my_first
               do concurrent(j=2:ny-1) ! j = 1 or ny is always a boundary
                 laplacian_rhs%s_(i,j) = (halo_x(west,j) - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                                         (rhs%s_(i,j-1)  - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
               end do
             end if compute_west_boundary

             compute_internal_points: &
             do concurrent(i=my_first+1:my_last-1, j=2:ny-1) ! j = 1 or ny is always a boundary
               laplacian_rhs%s_(i,j) = (rhs%s_(i-1,j) - 2*rhs%s_(i,j) + rhs%s_(i+1,j))/dx_**2 + &
                                       (rhs%s_(i,j-1) - 2*rhs%s_(i,j) + rhs%s_(i,j+1))/dy_**2
             end do compute_internal_points

             compute_east_boundary: &
             if (me<num_subdomains) then
               i=my_last
               do concurrent(j=2:ny-1) ! j = 1 or ny is always a boundary
                 laplacian_rhs%s_(i,j) = (rhs%s_(i-1,j) - 2*rhs%s_(i,j) + halo_x(east,j))/dx_**2 + &
                                         (rhs%s_(i,j-1) - 2*rhs%s_(i,j) +  rhs%s_(i,j+1))/dy_**2
               end do
             end if compute_east_boundary 
           end block
         end associate
       end associate
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
      sync all
    end procedure

    module procedure values
      call assert(allocated(self%s_), "subdomain_t%values: allocated(self%s_)")
      my_values =  self%s_
    end procedure

end submodule subdomain_s
