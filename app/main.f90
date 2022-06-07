      program tcell
      implicit none
      integer i,j,k
      integer ncells, npositions, nintervals
      parameter(ncells = 100, npositions = 25)
      parameter(nintervals = 10)
      double precision dt,sum
      double precision rr1,rr2,rr3,speed
      double precision vel(nintervals)
      double precision sample_distribution(nintervals)
      double precision cumulative_distribution(0:nintervals)      
      double precision x(ncells,npositions)
      double precision y(ncells,npositions)
      double precision z(ncells,npositions)

!     Time step      
      dt = .1

!     Assign initial positions to T cells randomly in a [100x100x100] grid
      do i = 1,ncells
         call random_number(rr1)
         call random_number(rr2)
         call random_number(rr3)
         x(1,1) = rr1*100.d0
         y(1,1) = rr2*100.d0
         z(1,1) = rr3*100.d0
      end do

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
