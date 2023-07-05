program explicit_diffusion
  implicit none
  integer nx_total, ny, nz ! Dimensions of grid
  integer nx
  parameter (nx_total = 600000, ny = 10, nz = 10)
  integer i,j,k,itime
  integer nit ! Number of time step iterations
  double precision x_1,x_2,y_1,y_2,z_1,z_2 ! Size of grid
  double precision dt ! Time step
  double precision dx,dy,dz
  double precision axx,ayy,azz
  double precision rdxdx,rdydy,rdzdz
  double precision diffusion
  double precision amax
  double precision, allocatable :: a(:,:,:)[:]
  double precision, allocatable :: a_old(:,:,:)
  !  double precision a(0:nx+1,0:ny+1,0:nz+1)[*]
  !  double precision a_old(0:nx+1,0:ny+1,0:nz+1)
  
  nx = nx_total/num_images()

  allocate(a(0:nx+1,0:ny+1,0:nz+1)[*])
  allocate(a_old(0:nx+1,0:ny+1,0:nz+1))
  nit = 10 ! Number of iterations

  diffusion = 1.d0

  x_1 = 0.d0
  x_2 = 1.d0
  y_1 = 0.d0
  y_2 = 1.d0
  z_1 = 0.d0
  z_2 = 1.d0
  
  dx = (x_2 - x_1)/dble(nx)
  dy = (y_2 - y_1)/dble(ny)
  dz = (z_2 - z_1)/dble(nz)

  a = 0.d0
  
  ! Diffusion constraint on time step
  dt = .5d0*min(dx**2,dy**2,dz**2)/(6.d0*diffusion)

  rdxdx = 1.d0/(dx**2)
  rdydy = 1.d0/(dy**2)
  rdzdz = 1.d0/(dz**2)
  
  ! Initialize array
  do k = 1,nz
     do j = 1,ny
        do i = 1,nz
           a(i,j,k) = 1.d0
        end do
     end do
  end do
  
  
  do itime = 1,nit

     if (this_image() .ge. 2 .and. this_image() .le. num_images()) then
        do concurrent (k = 1:nz)
           do concurrent (j = 1:ny)
              a(0,j,k) = a(nx,j,k)[this_image()-1]
           end do
        end do
     end if
     
     if (this_image() .ge. 1 .and. this_image() .le. num_images()-1) then
        do concurrent (k = 1:nz)
           do concurrent (j = 1:ny)
              a(nx+1,j,k) = a(1,j,k)[this_image()+1]
           end do
        end do
     end if
     
     sync all
     a_old = a

     do concurrent (k = 1:nz)
        do concurrent (j = 1:ny)
           do concurrent (i = 1:nx)
              axx = (a_old(i+1,j,k) - 2.d0*a_old(i,j,k) + a_old(i-1,j,k))*rdxdx
              ayy = (a_old(i,j+1,k) - 2.d0*a_old(i,j,k) + a_old(i,j-1,k))*rdydy
              azz = (a_old(i,j,k+1) - 2.d0*a_old(i,j,k) + a_old(i,j,k-1))*rdzdz
              
              a(i,j,k) = a_old(i,j,k) + dt*diffusion*(axx+ayy+azz)
           end do
        end do
     end do
     
  end do
  
  amax = -1.d+20
  do k = 1,nz
     do j = 1,ny
        do i = 1,nx
           amax = max(amax,dabs(a(i,j,k)))
        end do
     end do
  end do

  write (*,*) "Hello from image", this_image(), "of", num_images(),amax
  end program explicit_diffusion
