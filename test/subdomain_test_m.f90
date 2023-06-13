! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module subdomain_test_m
  !! Define subdomain tests and procedures required for reporting results
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use subdomain_m, only : subdomain_t
  use iso_fortran_env, only : output_unit
  implicit none

  private
  public :: subdomain_test_t

  type, extends(test_t) :: subdomain_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A subdomain_t" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = test_result_t( &
      [ character(len=len("computing a correctly shaped Laplacian for a 2D flat-topped, step-like plateau")) :: &
        "computing a correctly shaped Laplacian for a 2D flat-topped, step-like plateau", &
        "reaching the correct steady state solution", &
        "functional pattern results matching procedural results" &
      ], &
      [ correctly_shaped_laplacian(),  &
        correct_steady_state(),  &
        functional_matches_procedural()  &
       ] &
    )
  end function

  subroutine output(v)
    real, intent(in) :: v(:,:)
    integer j
    sync all
    critical
      do j = 1, size(v,2)
        print *,"image ",this_image(),": ", v(:,j)
      end do
    end critical
    sync all
  end subroutine

  function correctly_shaped_laplacian() result(test_passes)
    logical test_passes
    type(subdomain_t) f, laplacian_f
    real, allocatable :: lap_f_vals(:,:,:)

    real boundary_value,internal_value

    boundary_value = 1.
    internal_value = 2.
    call f%define(side=1., boundary_val=boundary_value, &
                  internal_val=internal_value, n=11) ! internally constant subdomain with a step down at the edges
    laplacian_f = .laplacian. f
    lap_f_vals = laplacian_f%values()

    associate(me => this_image(), n_subdomains => num_images(), &
              nx => size(lap_f_vals,1), ny => size(lap_f_vals,2), &
              nz => size(lap_f_vals,3) )
      associate(first_zero_in_x => merge(3, 1, me==1), last_zero_in_x => merge(nx-2, nx, me==n_subdomains))
        block
          real face_lap_x,face_lap_y,face_lap_z
          real edge_lap_xy,edge_lap_xz,edge_lap_yz
          real corner_lap
          real dx_,dy_,dz_
          real boundary_lap
          real, parameter :: tolerance = 1.0E-06
          integer, parameter :: left_adjacent = 2, bottom_adjacent = 2, y1 = 2, y2 = 2, z1 = 2, z2 = 2 
          logical internally_zero
          logical face_lap_x_left_check,face_lap_x_right_check,face_lap_y_check,face_lap_z_check
          logical edge_lap_xy_left_check,edge_lap_xz_left_check
          logical edge_lap_xy_right_check,edge_lap_xz_right_check
          logical edge_lap_yz_check
          logical corner_lap_left_check,corner_lap_right_check

          
          internally_zero = all(abs(lap_f_vals(first_zero_in_x:last_zero_in_x, 3:ny-2, 3:nz-2)) < tolerance)

          dx_ = laplacian_f%dx()
          dy_ = laplacian_f%dy()
          dz_ = laplacian_f%dz()

          boundary_lap = (boundary_value - 2.0*internal_value + internal_value)
          face_lap_x = boundary_lap/dx_**2           
          face_lap_y = boundary_lap/dy_**2
          face_lap_z = boundary_lap/dz_**2

          edge_lap_xy = face_lap_x + face_lap_y
          edge_lap_xz = face_lap_x + face_lap_z
          edge_lap_yz = face_lap_y + face_lap_z

          corner_lap = face_lap_x + face_lap_y + face_lap_z
           
          ! Check Laplacian on Faces
          if (me .eq. 1) face_lap_x_left_check = all(abs(lap_f_vals(2,3:ny-2,3:nz-2)-face_lap_x) < tolerance)
          if (me .eq. n_subdomains) face_lap_x_right_check = all(abs(lap_f_vals(nx-1,3:ny-2,3:nz-2)-face_lap_x) < tolerance)
          face_lap_y_check = all( &
                [abs(lap_f_vals(first_zero_in_x:last_zero_in_x,2,3:nz-2)-face_lap_y) < tolerance, &
                 abs(lap_f_vals(first_zero_in_x:last_zero_in_x,ny-1,3:nz-2)-face_lap_y) < tolerance ])
          face_lap_z_check = all( &
                [abs(lap_f_vals(first_zero_in_x:last_zero_in_x,3:ny-2,2)-face_lap_z) < tolerance, &
                 abs(lap_f_vals(first_zero_in_x:last_zero_in_x,3:ny-2,nz-1)-face_lap_z) < tolerance ])

           ! Check Laplacian on Edges
           if (me .eq. 1) then
              edge_lap_xy_left_check = all( &
                 [abs(lap_f_vals(2,2,3:nz-2)-edge_lap_xy) < tolerance, &
                  abs(lap_f_vals(2,ny-1,3:nz-2)-edge_lap_xy) < tolerance])
              edge_lap_xz_left_check = all( &
                 [abs(lap_f_vals(2,3:ny-2,2)-edge_lap_xz) < tolerance, &
                  abs(lap_f_vals(2,3:ny-2,nz-1)-edge_lap_xz) < tolerance]) 
           end if
           if (me .eq. n_subdomains) then
              edge_lap_xy_right_check = all( &
                 [abs(lap_f_vals(nx-1,2,3:nz-2)-edge_lap_xy) < tolerance, &
                  abs(lap_f_vals(nx-1,ny-1,3:nz-2)-edge_lap_xy) < tolerance])
              edge_lap_xz_right_check = all( &
                 [abs(lap_f_vals(nx-1,3:ny-2,2)-edge_lap_xz) < tolerance, &
                  abs(lap_f_vals(nx-1,3:ny-2,nz-1)-edge_lap_xz) < tolerance])                
           end if
           edge_lap_yz_check = all( &
                [abs(lap_f_vals(first_zero_in_x:last_zero_in_x,2,2)-edge_lap_yz) < tolerance, &
                 abs(lap_f_vals(first_zero_in_x:last_zero_in_x,ny-1,2)-edge_lap_yz) < tolerance, &
                 abs(lap_f_vals(first_zero_in_x:last_zero_in_x,2,nz-1)-edge_lap_yz) < tolerance, &
                 abs(lap_f_vals(first_zero_in_x:last_zero_in_x,ny-1,nz-1)-edge_lap_yz) < tolerance])


           ! Check Laplacian on Corners
           if (me .eq. 1) then
              corner_lap_left_check = all( &
                 [abs(lap_f_vals(2,2,2)-corner_lap) < tolerance, &
                  abs(lap_f_vals(2,ny-1,2)-corner_lap) < tolerance, &
                  abs(lap_f_vals(2,2,nz-1)-corner_lap) < tolerance, &
                  abs(lap_f_vals(2,ny-1,nz-1)-corner_lap) < tolerance ])
           end if
           if (me .eq. n_subdomains) then
              corner_lap_right_check = all( &
                 [abs(lap_f_vals(nx-1,2,2)-corner_lap) < tolerance, &
                  abs(lap_f_vals(nx-1,ny-1,2)-corner_lap) < tolerance, &
                  abs(lap_f_vals(nx-1,2,nz-1)-corner_lap) < tolerance, &
                  abs(lap_f_vals(nx-1,ny-1,nz-1)-corner_lap) < tolerance ])
           end if            
            
           test_passes = &
              internally_zero .and. &
              face_lap_y_check .and. &
              face_lap_z_check .and. &
              edge_lap_yz_check .and. &
              merge(face_lap_x_left_check, .true., me==1) .and. &
              merge(face_lap_x_right_check, .true., me==n_subdomains) .and. &
              merge(edge_lap_xy_left_check, .true., me==1)  .and. &
              merge(edge_lap_xy_right_check, .true., me==n_subdomains)  .and. &
              merge(edge_lap_xz_left_check, .true., me==1) .and. &
              merge(edge_lap_xz_right_check, .true., me==n_subdomains) .and. &               
              merge(corner_lap_left_check, .true., me==1) .and. &
              merge(corner_lap_right_check, .true., me==n_subdomains)

        end block
      end associate
    end associate
    
  end function

  function correct_steady_state() result(test_passes)
     logical test_passes
     type(subdomain_t) T
     real, parameter :: T_boundary = 1., T_initial = 2.

     call T%define(side=1., boundary_val=T_boundary, internal_val=T_initial, n=11) 
       ! spatially constant internal temperatuers with a step change at the boundaries

     block
       integer step
       integer, parameter :: steps = 5000
       real, parameter :: alpha = 1.
       real dxdydz
       dxdydz = 1.0/T%dx()**2 + 1.0/T%dy()**2 + 1.0/T%dz()**2
       associate(dt => 0.5/(alpha*dxdydz))       
         do step = 1, steps
           T =  T + dt * alpha * .laplacian. T
         end do
       end associate
     end block

     block
       real, parameter :: tolerance = 0.01, T_steady = T_boundary
       associate(T_values => T%values())
         associate(ny => size(T_values,2),nz => size(T_values,3))
           associate( residual => T_values(:,2:ny-1,2:nz-1) - T_steady)
             test_passes = all(residual >= 0.  .and.  residual <= tolerance)
           end associate
         end associate
       end associate
     end block

  end function

  function functional_matches_procedural() result(test_passes)
     logical test_passes
     real, parameter :: tolerance = 0.1
     integer, parameter :: steps = 5000, n=11
     real, parameter :: alpha = 1.
     real, parameter :: side=1., boundary_val=1., internal_val=2.

     associate( T_f => T_functional(), T_p => T_procedural())
       associate(L_infinity_norm => maxval(abs(T_f - T_p)))
         test_passes = L_infinity_norm < tolerance
       end associate
     end associate

  contains

    function T_functional()
      real dxdydz
      real, allocatable :: T_functional(:,:,:)
      type(subdomain_t) T
      integer step

      call T%define(side, boundary_val, internal_val, n)

      dxdydz = 1.0/T%dx()**2 + 1.0/T%dy()**2 + 1.0/T%dz()**2
      associate(dt => 0.5/(alpha*dxdydz))
        do step = 1, steps
          T =  T + dt * alpha * .laplacian. T
        end do
      end associate

      T_functional = T%values()
    end function

    function T_procedural()
      real dxdydz
      real, allocatable :: T_procedural(:,:,:)
      type(subdomain_t) T
      integer step

      call T%define(side, boundary_val, internal_val, n)

      dxdydz = 1.0/T%dx()**2 + 1.0/T%dy()**2 + 1.0/T%dz()**2
      
      associate(dt => 0.5/(alpha*dxdydz))
        do step = 1, steps
          call T%step(alpha*dt)
        end do
      end associate

      T_procedural = T%values()
    end function

  end function
end module subdomain_test_m
