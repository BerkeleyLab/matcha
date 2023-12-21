! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module subdomain_test_m
  !! Define subdomain tests and procedures required for reporting results
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use subdomain_m, only : subdomain_t
  use assert_m, only : assert
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
    specimen = "A subdomain_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    integer, parameter :: longest_len = &
      len("computing a concave Laplacian for a spatially constant operand with a step down at boundaries")

    associate( &
      descriptions => &
        [character(len=longest_len) :: &
         "computing a concave Laplacian for a spatially constant operand with a step down at boundaries", &
         "reaching the correct steady state solution", &
         "functional pattern results matching procedural results" &
        ], &
      outcomes => &
        [concave_laplacian(), &
         correct_steady_state(), &
         functional_matches_procedural() &
        ] &
    )
      call assert(size(descriptions) == size(outcomes),"subdomain_test_m(results): size(descriptions) == size(outcomes)")
      test_results = test_result_t(descriptions, outcomes)
    end associate
  end function

  subroutine output(v)
    real, intent(in) :: v(:,:,:)
    integer j, k
    sync all
    critical
      do j = 1, size(v,2)
        do k = 1, size(v,3)
          print *,"image ",this_image(),": ",j,k,v(:,j,k)
        end do
      end do
    end critical
    sync all
  end subroutine

  function concave_laplacian() result(test_passes)
    logical test_passes
    type(subdomain_t) f, laplacian_f
    real, allocatable :: lap_f_vals(:,:,:)

    call f%define(side=1., boundary_val=1., internal_val=2., n=21) ! internally constant subdomain with a step down at all surfaces
    laplacian_f = .laplacian. f
    lap_f_vals = laplacian_f%values()

    block
      real, parameter :: tolerance = 1.0E-01
      logical internally_zero, concave_at_faces, doubly_concave_at_edges, triply_concave_in_corners, constant_away_from_edges
    
      associate(me=>this_image(), n_subdomains=>num_images(), nx=>size(lap_f_vals,1), ny=>size(lap_f_vals,2),nz=>size(lap_f_vals,3))
        associate(first_zero_in_x => merge(3, 1, me==1), last_zero_in_x => merge(nx-2, nx, me==n_subdomains))
          internally_zero = all(abs(lap_f_vals(first_zero_in_x:last_zero_in_x, 3:ny-2, 3:nz-2)) < tolerance)
        end associate

        associate( &
          bottom_x_adjacent => lap_f_vals(     2, 3:ny-2, 3:nz-2), &
             top_x_adjacent => lap_f_vals(  nx-1, 3:ny-2, 3:nz-2), &
          bottom_y_adjacent => lap_f_vals(3:nx-2,      2, 3:nz-2), &
             top_y_adjacent => lap_f_vals(3:nx-2,   ny-1, 3:nz-2), &
          bottom_z_adjacent => lap_f_vals(3:nx-2, 3:ny-2,      2), &
             top_z_adjacent => lap_f_vals(3:nx-2, 3:ny-2,   nz-1)  &
        )
          associate(curvature => bottom_y_adjacent(1,1))
            constant_away_from_edges = &
              merge(all(abs(bottom_x_adjacent - curvature) < tolerance), .true., me==1           ) .and. &
              merge(all(abs(   top_x_adjacent - curvature) < tolerance), .true., me==n_subdomains) .and. &
                    all(abs(bottom_y_adjacent - curvature) < tolerance) .and. &
                    all(abs(   top_y_adjacent - curvature) < tolerance) .and. &
                    all(abs(bottom_z_adjacent - curvature) < tolerance) .and. &
                    all(abs(   top_z_adjacent - curvature) < tolerance)

            concave_at_faces = &
              all(bottom_y_adjacent < 0) .and. &
              all(   top_y_adjacent < 0) .and. &
              all(bottom_z_adjacent < 0) .and. &
              all(   top_z_adjacent < 0) .and. &
              merge(all(bottom_x_adjacent < 0), .true., me==1) .and. &
              merge(all(   top_x_adjacent < 0), .true., me==n_subdomains)

            associate( &
              lo_y_lo_z_edge => lap_f_vals(3:nx-2,      2,      2), &
              hi_y_lo_z_edge => lap_f_vals(3:nx-2,   ny-1,      2), &
              lo_y_hi_z_edge => lap_f_vals(3:nx-2,      2,   nz-1), &
              hi_y_hi_z_edge => lap_f_vals(3:nx-2,   ny-1,   nz-1), &
              lo_x_lo_z_edge => lap_f_vals(     2, 3:ny-2,      2), &
              hi_x_lo_z_edge => lap_f_vals(  nx-1, 3:ny-2,      2), &
              lo_x_hi_z_edge => lap_f_vals(     2, 3:ny-2,   nz-1), &
              hi_x_hi_z_edge => lap_f_vals(  nx-1, 3:ny-2,   nz-1), &
              lo_x_lo_y_edge => lap_f_vals(     2,      2, 3:nz-2), &
              hi_x_lo_y_edge => lap_f_vals(  nx-1,      2, 3:nz-2), &
              lo_x_hi_y_edge => lap_f_vals(     2,   ny-1, 3:nz-2), &
              hi_x_hi_y_edge => lap_f_vals(  nx-1,   ny-1, 3:nz-2)  &
            )
              doubly_concave_at_edges = &
                merge(all(abs(lo_x_lo_z_edge - 2.*curvature) < tolerance), .true., me==1) .and. &
                merge(all(abs(lo_x_hi_z_edge - 2.*curvature) < tolerance), .true., me==1) .and. &
                merge(all(abs(lo_x_lo_y_edge - 2.*curvature) < tolerance), .true., me==1) .and. &
                merge(all(abs(lo_x_hi_y_edge - 2.*curvature) < tolerance), .true., me==1) .and. &
                merge(all(abs(hi_x_lo_z_edge - 2.*curvature) < tolerance), .true., me==n_subdomains) .and. &
                merge(all(abs(hi_x_hi_z_edge - 2.*curvature) < tolerance), .true., me==n_subdomains) .and. &
                merge(all(abs(hi_x_lo_y_edge - 2.*curvature) < tolerance), .true., me==n_subdomains) .and. &
                merge(all(abs(hi_x_hi_y_edge - 2.*curvature) < tolerance), .true., me==n_subdomains) .and. &
                all(abs(lo_y_lo_z_edge - 2*curvature) < tolerance) .and. &
                all(abs(hi_y_lo_z_edge - 2*curvature) < tolerance) .and. &
                all(abs(lo_y_hi_z_edge - 2*curvature) < tolerance) .and. &
                all(abs(hi_y_hi_z_edge - 2*curvature) < tolerance)

              triply_concave_in_corners = &
                merge((abs(lap_f_vals(   2,   2,   2) - 3.*curvature) < tolerance), .true., me==1) .and. &
                merge((abs(lap_f_vals(   2,ny-1,   2) - 3.*curvature) < tolerance), .true., me==1) .and. &
                merge((abs(lap_f_vals(   2,   2,nz-1) - 3.*curvature) < tolerance), .true., me==1) .and. &
                merge((abs(lap_f_vals(   2,ny-1,nz-1) - 3.*curvature) < tolerance), .true., me==1) .and. &
                merge((abs(lap_f_vals(nx-1,   2,   2) - 3.*curvature) < tolerance), .true., me==n_subdomains) .and. &
                merge((abs(lap_f_vals(nx-1,ny-1,   2) - 3.*curvature) < tolerance), .true., me==n_subdomains) .and. &
                merge((abs(lap_f_vals(nx-1,   2,nz-1) - 3.*curvature) < tolerance), .true., me==n_subdomains) .and. &
                merge((abs(lap_f_vals(nx-1,ny-1,nz-1) - 3.*curvature) < tolerance), .true., me==n_subdomains)
            end associate
          end associate
        end associate
      end associate


      test_passes = &
        all([internally_zero, constant_away_from_edges, concave_at_faces, doubly_concave_at_edges, triply_concave_in_corners])
    end block

  end function

  function correct_steady_state() result(test_passes)
    logical test_passes
    type(subdomain_t) T
    real, parameter :: T_boundary = 1., T_initial = 2., tolerance = 0.01, T_steady = T_boundary, alpha = 1.
    integer, parameter :: steps = 6000
    integer step

    call T%define(side=1., boundary_val=T_boundary, internal_val=T_initial, n=21) ! const. internally with a step down at boundaries

    associate(dt => T%dx()*T%dy()*T%dz()/(4*alpha))
      do step = 1, steps
        T =  T + dt * alpha * .laplacian. T
      end do
    end associate

    associate(residual => T%values() - T_steady)
      test_passes = all(residual >= 0. .and. residual <= tolerance)
    end associate
  end function

  function functional_matches_procedural() result(test_passes)
    logical test_passes
    real, parameter :: tolerance = 0.1
    integer, parameter :: steps = 6000, n=21
    real, parameter :: alpha = 1.
    real, parameter :: side=1., boundary_val=1., internal_val=2.

    associate( T_f => T_functional(), T_p => T_procedural())
      associate(L_infinity_norm => maxval(abs(T_f - T_p)))
        test_passes = L_infinity_norm < tolerance
      end associate
    end associate

  contains

    function T_functional()
      real, allocatable :: T_functional(:,:,:)
      type(subdomain_t) T
      integer step

      call T%define(side, boundary_val, internal_val, n)

      associate(dt => T%dx()*T%dy()/(4*alpha))
        do step = 1, steps
          T =  T + dt * alpha * .laplacian. T
        end do
      end associate

      T_functional = T%values()
    end function

    function T_procedural()
      real, allocatable :: T_procedural(:,:,:)
      type(subdomain_t) T
      integer step

      call T%define(side, boundary_val, internal_val, n)

      associate(dt => T%dx()*T%dy()/(4*alpha))
        do step = 1, steps
          call T%step(alpha*dt)
        end do
      end associate

      T_procedural = T%values()
    end function

  end function
end module subdomain_test_m