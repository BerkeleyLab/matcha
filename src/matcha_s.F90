! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(matcha_m) matcha_s
  use t_cell_collection_m, only : t_cell_collection_t
  use distribution_m, only : distribution_t
  use data_partition_m, only : data_partition_t
  use gridr_m, only : gridr_t
  implicit none
  
contains

  module procedure matcha

    associate( &
      ncells => input%num_cells(), & ! Number of T cells
      npositions => input%num_positions(), & ! Number of time steps
      ndim => input%num_dimensions(), & ! Normally 3 for 3D calculations
      nintervals => input%num_intervals(), & ! Number of intervals in distributions
      num_grid => input%ngrid(), & ! Number of grid cells in one-direction in 3D diffusion grid
      dt => input%time_step(), & ! Time step (seconds)
      gb => input%grid_begin(), & ! Lower spatial limit of 3D diffusion cubic grid (microns)
      ge => input%grid_end(), & ! Upper spatial limit of 3D diffusion cubic grid (microns)
      cytokinev => input%cytokine(), & ! Amount of protein deposited by T cell in grid
      gfacv => input%gfac(), & ! Gradient speed factor
      alpha => input%alpha(), & ! Diffusion coefficient
      empirical_distribution => input%sample_distribution() &
    )

      block
        double precision, parameter :: scale = 100.D0
        double precision, allocatable :: random_positions(:,:), random_4vectors(:,:,:)
        double precision, allocatable ::  gdx(:)
        double precision gxs
        type(distribution_t) distribution
        type(gridr_t) gridr
        integer, allocatable :: ng(:)
        integer, parameter :: nveldim = 4
        integer i,j,k,ii,jj,kk
        integer step
        double precision tconc
        double precision dxdydz,dtmax,dta
        type(data_partition_t) data_partition

        double precision boundary_value, internal_value
        type(subdomain_t) concentration_subgrid

        ! Subgrid is a subset of the complete grid owned by each processor.
        ! The complete grid is divided in the x-direction
        boundary_value = 0.d0
        internal_value = 0.d0
        call concentration_subgrid%define(side=ge-gb,boundary_val=boundary_value, &
                                      internal_val=internal_value,n=num_grid)

        allocate(ng(ndim)) ! Number of grid cells in each direction of 3D diffusion grid
        allocate(gdx(2*ndim+1)) ! Stores grid parameters
        ng(:) = num_grid ! Set the number of cells in each direction of 3D diffusion grid to be the same
        gridr = gridr_t() ! Complete grid
        gdx = gridr%gridparameters(gb,ge,ng,ndim)
        tconc = cytokinev*gfacv*gdx(7)

        dxdydz = 1.d0/gdx(1)**2 + 1.d0/gdx(2)**2 + 1.d0/gdx(3)**2
        dtmax = 0.5d0/(alpha*dxdydz)
        dta = min(dt,dtmax)
        
        call data_partition%define_partitions(cardinality=ncells)
    
        associate(me => this_image())
          associate(my_num_cells => data_partition%last(me) - data_partition%first(me) + 1)
          
            call random_init(repeatable=.true., image_distinct=.true.)
            
            allocate(random_positions(my_num_cells,ndim))
            call random_number(random_positions)  
          
            associate(nsteps => npositions -1)
              allocate(random_4vectors(my_num_cells,nsteps,nveldim))
              call random_number(random_4vectors)  
              distribution = distribution_t(empirical_distribution)
          
              associate(random_speeds => random_4vectors(:,:,1), random_directions => random_4vectors(:,:,2:4))
                associate(v => distribution%velocities(random_speeds, random_directions))
                  allocate(history(nsteps))
                  history(1) = t_cell_collection_t(scale*random_positions, time=0.D0)
                  do step = 2, nsteps
                     associate(x => history(step-1)%positions(), t => history(step-1)%time())
                       associate(gx => gridr%gradient(my_num_cells,ng,gdx,gb,tconc,x,concentration_subgrid))
                         history(step) = t_cell_collection_t(x + (v(:,step-1,:) + gx(:,:))*dta, t + dta)
                         concentration_subgrid = concentration_subgrid + dta * alpha * .laplacian. concentration_subgrid
                       end associate
                    end associate
                  end do
                end associate
              end associate
            end associate
          end associate  
        end associate    
      end block
    end associate

  end procedure

end submodule matcha_s
