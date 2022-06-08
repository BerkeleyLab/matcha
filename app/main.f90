      program tcell
      implicit none

      integer ncells, npositions, nintervals
      parameter(ncells = 100, npositions = 25)
      parameter(nintervals = 10)

      double precision vel(nintervals)
      double precision cumulative_distribution(0:nintervals)      

      double precision x(ncells,npositions)
      double precision y(ncells,npositions)
      double precision z(ncells,npositions)

      call initialize_positions(x,y,z,ncells,npositions)

      call create_distribution(vel,cumulative_distribution,nintervals)

      call move_tcells(x,y,z,vel,cumulative_distribution,ncells,npositions,nintervals)
      
      contains
      
      subroutine move_tcells(x,y,z,vel,cumulative_distribution,ncells,npositions,nintervals)
      implicit none
      integer ncells,npositions,nintervals
      double precision x(ncells,npositions)
      double precision y(ncells,npositions)
      double precision z(ncells,npositions)
      double precision cumulative_distribution(0:nintervals)              
      double precision vel(nintervals)
        
!     Local variables
      integer i,j,k
      double precision speed,dt
      double precision rr1,rr2,rr3,sum

!     Time step      
      dt = .1
      
      do i = 1,ncells
         do j = 2,npositions

!           Sample from the distribution            
            call random_number(rr1)
            do k = 1,nintervals
               if (rr1 .ge. cumulative_distribution(k-1) .and. &
                   rr1 .lt. cumulative_distribution(k)) then
                  speed = vel(k)
               end if
            end do

!           Create a random unit vector            
            call random_number(rr1)
            call random_number(rr2)
            call random_number(rr3)            
            sum = rr1 + rr2 + rr3
            rr1 = rr1/sum
            rr2 = rr2/sum
            rr3 = rr3/sum

!           Use a forward Euler to advance the cell position            
            x(i,j) = x(i,j-1) + dt*speed
            y(i,j) = y(i,j-1) + dt*speed
            z(i,j) = z(i,j-1) + dt*speed
         end do
      end do

      end

      subroutine initialize_positions(x,y,z,ncells,npositions)
        
      implicit none
      integer ncells,npositions
      double precision x(ncells,npositions)
      double precision y(ncells,npositions)
      double precision z(ncells,npositions)

!     Local variables
      integer i
      double precision rr1,rr2,rr3
      
        
!     Assign initial positions to T cells randomly in a [100x100x100] grid
      do i = 1,ncells
         call random_number(rr1)
         call random_number(rr2)
         call random_number(rr3)
         x(i,1) = rr1*100.d0
         y(i,1) = rr2*100.d0
         z(i,1) = rr3*100.d0
      end do

      end

      subroutine create_distribution(vel,cumulative_distribution,nintervals)
      implicit none
      integer nintervals
      double precision cumulative_distribution(0:nintervals)              
      double precision vel(nintervals)

!     Local variables      
      integer i
      double precision sum,rr1
      double precision, allocatable :: sample_distribution(:)

      allocate(sample_distribution(nintervals))
      
      
!     Create a distribution      
      sum = 0.d0
      do i = 1,nintervals
         call random_number(rr1)
         sample_distribution(i) = rr1
         sum = sum + sample_distribution(i)
      end do

      do i = 1,nintervals
         sample_distribution(i) = sample_distribution(i)/sum
!        Assign speeds to each distribution bin         
         vel(i) = dble(i)
      end do

!     Form the cumulative distribution      
      cumulative_distribution(0) = 0.d0
      do i = 1,nintervals
         cumulative_distribution(i) = cumulative_distribution(i-1) + &
                                      sample_distribution(i)
      end do

      end

      end
      
