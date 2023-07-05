module assertions_m
  implicit none
contains
  pure subroutine assert(assertion, description)
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) error stop description
  end subroutine
end module

module subdomain_3D_m
  implicit none

  private
  public :: subdomain_3D_t

  type subdomain_3D_t 
    private
    double precision, allocatable :: s_(:,:,:)
  contains
    procedure define
    procedure dx
    procedure dy
    procedure dz
    procedure mynx
    procedure myny
    procedure mynz
    procedure values
    procedure exchange_halo
    generic :: operator(.laplacian.) => laplacian
    generic :: operator(+) => add
    procedure, private :: add
    procedure, private :: laplacian
  end type

  interface

    module subroutine define(self, side, boundary_val, internal_val, nx, ny, nz)
      implicit none
      double precision, intent(in) :: side, boundary_val, internal_val
      integer, intent(in) :: nx, ny, nz !! number of grid points in each coordinate direction
      class(subdomain_3D_t), intent(out) :: self
    end subroutine

    pure module function values(self) result(my_values)
      implicit none
      class(subdomain_3D_t), intent(in) :: self
      double precision, allocatable :: my_values(:,:,:)
    end function

    pure module function dx(self) result(my_dx)
      implicit none
      class(subdomain_3D_t), intent(in) :: self
      double precision my_dx
    end function

    pure module function dy(self) result(my_dy)
      implicit none
      class(subdomain_3D_t), intent(in) :: self
      double precision my_dy
    end function 

    pure module function dz(self) result(my_dz)
      implicit none
      class(subdomain_3D_t), intent(in) :: self
      double precision my_dz
    end function dz

    pure module function mynx(self) result(mynx_)
      implicit none
      class(subdomain_3D_t), intent(in) :: self
      integer mynx_
    end function

    pure module function myny(self) result(myny_)
      implicit none
      class(subdomain_3D_t), intent(in) :: self
      integer myny_
    end function 

    pure module function mynz(self) result(mynz_)
      implicit none
      class(subdomain_3D_t), intent(in) :: self
      integer mynz_
    end function
    
    
    pure module function laplacian(rhs) result(laplacian_rhs)
      implicit none
      class(subdomain_3D_t), intent(in) :: rhs
      double precision, allocatable :: laplacian_rhs(:,:,:)
    end function

    pure module function add(lhs, rhs) result(total)
      implicit none
      class(subdomain_3D_t), intent(in) :: lhs
      double precision, intent(in) :: rhs(:,:,:)
      type(subdomain_3D_t) total
    end function

    module subroutine exchange_halo(self)
      implicit none
      class(subdomain_3D_t), intent(in) :: self
    end subroutine

  end interface

end module

submodule(subdomain_3D_m) subdomain_3D_s
  use assertions_m, only : assert
  implicit none

  double precision, allocatable :: halo_x(:,:,:)[:]
  integer mynxl,mynyl,mynzl,i,j,k
  integer ibase,icounter,iproc
  double precision dx_, dy_, dz_
  integer, parameter :: west=1, east=2
  integer my_nx, my_ny, my_nz
  integer nx, ny, nz, me, num_subdomains, my_internal_west, my_internal_east
contains

  module procedure define
    integer, parameter :: nx_boundaries = 2
    !call assert(num_subdomains <= n-nx_boundaries, "subdomain_3D_t%define: num_subdomains <= n-nx_boundaries")

    my_ny = ny
    my_nz = nz

    dx_ = side/dble(nx-2)
    dy_ = side/dble(ny-2)
    dz_ = side/dble(nz-2)

    !dx_ = 1.d0
    !dy_ = 1.d0
    !dz_ = 1.d0
    
    me = this_image()
    num_subdomains = num_images()
    my_nx = nx/num_subdomains + merge(1, 0, me <= mod(nx, num_subdomains))

    !icounter = mod(nx,num_subdomains)
    !ibase = nx/num_subdomains
    !iproc = 1
    !my_nx = ibase
    !if (me .eq. 1 .or. me .eq. num_images()) then
    !do while (icounter .gt. 0) 
    !   if (me .eq. iproc) then
    !      my_nx = ibase + 1
    !      icounter = icounter - 1
    !   end if
    !   if (me .eq. num_images()-iproc+1 .and. icounter .gt. 0) then
    !      my_nx = ibase + 1
    !      icounter = icounter - 1
    !   end if
    !   iproc = iproc + 1
    !end do
    !end if
    print*,'my_nx = ',my_nx,ny,nz,me !,ibase,icounter,me
          
    mynxl = my_nx
    mynyl = ny
    mynzl = nz
    
    my_internal_west = merge(2, 1, me==1)
    my_internal_east = merge(my_nx-1, my_nx, me==num_subdomains)
    if (allocated(self%s_)) deallocate(self%s_)
    allocate(self%s_(my_nx, ny, nz))

    self%s_(my_internal_west:my_internal_east, 1, :) = boundary_val ! bottom subdomain boundary
    self%s_(my_internal_west:my_internal_east, ny, :) = boundary_val ! top subdomain boundary
    self%s_(my_internal_west:my_internal_east, :, 1) = boundary_val ! subdomain boundary
    self%s_(my_internal_west:my_internal_east, :, nz) = boundary_val !  subdomain boundary    
    self%s_(my_internal_west:my_internal_east, 2:ny-1, 2:nz-1) = internal_val ! internal points
    self%s_(1, 2:ny-1, 2:nz-1) = merge(boundary_val, internal_val, me==1) ! west subdomain boundary
    self%s_(my_nx, 2:ny-1, 2:nz-1) = merge(boundary_val, internal_val, me==num_subdomains) ! east subdomain boundary

    if (allocated(halo_x)) deallocate(halo_x)
    allocate(halo_x(west:east, ny, nz)[*])
    call self%exchange_halo
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
    mynx_ = mynxl
  end procedure

  module procedure myny
    myny_ = mynyl
  end procedure    
  
  module procedure mynz
    mynz_ = mynzl
  end procedure  
  
  module procedure laplacian

    integer i, j, k
    integer nx,ny,nz
    double precision, allocatable :: halo_west(:,:), halo_east(:,:)

    call assert(allocated(rhs%s_), "subdomain_3D_t%laplacian: allocated(rhs%s_)")
    call assert(allocated(halo_x), "subdomain_3D_t%laplacian: allocated(halo_x)")

    ny = my_ny
    nz = my_nz

    allocate(laplacian_rhs(my_nx, ny, nz))

    halo_west = merge(halo_x(west,:,:), rhs%s_(1,:,:), me/=1)
    i = my_internal_west
    call assert(i+1<=my_nx,"laplacian: westmost subdomain too small")
    do concurrent(j=2:ny-1, k=2:nz-1)
      laplacian_rhs(i,j,k) = (halo_west(j,k)   - 2*rhs%s_(i,j,k) + rhs%s_(i+1,j,k))/dx_**2 + &
                             (rhs%s_(i,j-1,k)  - 2*rhs%s_(i,j,k) + rhs%s_(i,j+1,k))/dy_**2 + &
                             (rhs%s_(i,j,k-1)  - 2*rhs%s_(i,j,k) + rhs%s_(i,j,k+1))/dz_**2            
    end do

    do concurrent(i=my_internal_west+1:my_internal_east-1, j=2:ny-1, k=2:nz-1)
      laplacian_rhs(i,j,k) = (rhs%s_(i-1,j,k) - 2*rhs%s_(i,j,k) + rhs%s_(i+1,j,k))/dx_**2 + &
                             (rhs%s_(i,j-1,k) - 2*rhs%s_(i,j,k) + rhs%s_(i,j+1,k))/dy_**2 + &
                             (rhs%s_(i,j,k-1) - 2*rhs%s_(i,j,k) + rhs%s_(i,j,k+1))/dz_**2
    end do

    halo_east = merge(halo_x(east,:,:), rhs%s_(my_nx,:,:), me/=num_subdomains)
    i = my_internal_east
    call assert(i-1>0,"laplacian: eastmost subdomain too small")
    do concurrent(j=2:ny-1, k=2:nz-1)
      laplacian_rhs(i,j,k) = (rhs%s_(i-1,j,k) - 2*rhs%s_(i,j,k) + halo_east(j,k) )/dx_**2 + &
                             (rhs%s_(i,j-1,k) - 2*rhs%s_(i,j,k) + rhs%s_(i,j+1,k))/dy_**2 + &
                             (rhs%s_(i,j,k-1) - 2*rhs%s_(i,j,k) + rhs%s_(i,j,k+1))/dz_**2
    end do

    laplacian_rhs(:, 1, :) = 0.
    laplacian_rhs(:,ny, :) = 0.
    laplacian_rhs(:, :, 1) = 0.
    laplacian_rhs(:, :, nz) = 0.
    if (me==1) laplacian_rhs(1,:,:) = 0.
    if (me==num_subdomains) laplacian_rhs(my_nx,:,:) = 0.
  end procedure

  module procedure add
    total%s_ =  lhs%s_ + rhs
  end procedure
   
  module procedure exchange_halo
  if (me>1) halo_x(east,:,:)[me-1] = self%s_(1,:,:)
  if (me<num_subdomains) halo_x(west,:,:)[me+1] = self%s_(my_nx,:,:)
  end procedure

  module procedure values
    call assert(allocated(self%s_), "subdomain_3D_t%values: allocated(self%s_)")
    my_values =  self%s_
  end procedure

end submodule subdomain_3D_s

program main
  use subdomain_3D_m, only : subdomain_3D_t
  implicit none

  integer step,i,j,k,ii,my_nx,max_nxa
  integer me,icount,ifirst,ilast
  integer, parameter :: nxm = 37800, nym = 8, nzm = 8, steps = 50000
  integer nxa[*]
  double precision, parameter :: T_initial = 1., T_boundary = 0., alpha = 1.
  double precision t_start, t_finish, dxdydz
  double precision, allocatable :: T_values(:,:,:)[:]
  double precision, allocatable :: Tvalue_tot(:,:,:)
  type(subdomain_3D_t) T
  me = this_image()
  call T%define(side=1.d0, boundary_val=T_boundary, internal_val=T_initial, nx=nxm,ny=nym,nz=nzm) ! 3D step function
  nxa = T%mynx()

  dxdydz = 1.d0/T%dx()**2 + 1.0/T%dy()**2 + 1.0/T%dz()**2

  associate(dt => 0.5d0/(alpha*dxdydz))
    call cpu_time(t_start)
    do step = 1, steps
       sync all
       T =  T + dt * alpha * .laplacian. T       
       sync all
       call T%exchange_halo
    end do
    call cpu_time(t_finish)
    if (me .eq. 1) print *, "cpu_time: ", t_finish - t_start
  end associate

  max_nxa = -1
  do ii = 1,num_images()
     max_nxa = max(max_nxa,nxa[ii])
  end do
  
  !allocate(T_values(nxm,nym,nzm)[*])
  allocate(T_values(max_nxa,nym,nzm)[*])  

  
  associate(T_valuesa => T%values())
  do i = 1,nxa
     do j = 1,nym
        do k = 1,nzm
           T_values(i,j,k) = T_valuesa(i,j,k)
        end do
     end do
  end do
  end associate

  sync all
  
  if (me .eq. 0) then
     open(unit = 8, file = 'outputd')
     allocate(Tvalue_tot(nxm,nym,nzm))
     icount = 0
     do ii = 1,num_images()
        if (ii .eq. 1) then
           ifirst = 2
        else
           ifirst = 1
        end if
        if (ii .lt. num_images()) then
           ilast = nxa[ii]
        else
           ilast = nxa[ii]-1
        end if
        do i = ifirst,ilast
           icount = icount + 1
           do k = 2,nzm-1
              do j = 2,nym-1
                 Tvalue_tot(icount,j,k) = T_values(i,j,k)[ii]
                 write(8,*) icount,j-1,k-1,sngl(Tvalue_tot(icount,j,k))!,icount,j,k !,i,j,k,ii
              end do
           end do
        end do
     end do

  end if
    
  !print *,"T_initial, T_boundary, T_min, T_max: ", T_initial, T_boundary, &
  !minval(T_values(:,2:nym-1,2:nzm-1)), maxval(T_values(:,2:nym-1,2:nzm-1))
  !end associate
end program
