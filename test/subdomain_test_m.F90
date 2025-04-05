! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module subdomain_test_m
  !! Define subdomain tests and procedures required for reporting results
  use julienne_m, only : &
     diagnosis_function_i &
    ,operator(.csv.) &
    ,string_t &
    ,test_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t
  use subdomain_m, only : subdomain_t
  use assert_m, only : assert
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
    type(test_description_t), allocatable :: test_descriptions(:)
    type(test_result_t), allocatable :: test_results(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
       test_description_t("computing a concave Laplacian for a spatially constant operand with a step down at boundaries", concave_laplacian) &
      ,test_description_t("reaching the correct steady state solution", correct_steady_state) &
      ,test_description_t("functional pattern results matching procedural results" functional_matches_procedural) &
    ]
#else
    procedure(diagnosis_function_i), pointer :: &
       concave_laplacian_ptr &
      ,correct_steady_state_ptr &
      ,functional_matches_procedural_ptr

    concave_laplacian_ptr => concave_laplacian
    correct_steady_state_ptr => correct_steady_state
    functional_matches_procedural_ptr => functional_matches_procedural

    test_descriptions = [ &
       test_description_t("computing a concave Laplacian for a spatially constant operand with a step down at boundaries", concave_laplacian_ptr) &
      ,test_description_t("reaching the correct steady state solution", correct_steady_state_ptr) &
      ,test_description_t("functional pattern results matching procedural results", functional_matches_procedural_ptr) &
    ]
#endif
    test_results = test_descriptions%run()
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

  function concave_laplacian() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(subdomain_t) f, laplacian_f
    real, allocatable :: lap_f_vals(:,:,:)

    call f%define(side=1., boundary_val=1., internal_val=2., n=21) ! internally constant subdomain with a step down at all surfaces
    laplacian_f = .laplacian. f
    lap_f_vals = laplacian_f%values()

    block
      real, parameter :: tolerance = 1.0E-06
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

      associate(geometrical_properties => [internally_zero, constant_away_from_edges, concave_at_faces, doubly_concave_at_edges, triply_concave_in_corners])
        test_diagnosis = test_diagnosis_t( &
          test_passed = all(geometrical_properties) &
         ,diagnostics_string = "expected T,T,T,T,T, actual " // .csv. string_t(geometrical_properties) &
        )
      end associate
    end block

  end function

  function correct_steady_state() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(subdomain_t) T
    real, parameter :: T_boundary = 1., T_initial = 2., tolerance = 0.01, T_steady = T_boundary, alpha = 1.
    integer, parameter :: steps = 1000
    integer step

    call T%define(side=1., boundary_val=T_boundary, internal_val=T_initial, n=11) ! const. internally with a step down at boundaries

    associate(dt => T%dt_stable(alpha))
      do step = 1, steps
        T =  T + dt * alpha * .laplacian. T
      end do
    end associate

    associate(residual => T%values() - T_steady)
      test_diagnosis = test_diagnosis_t( &
        test_passed = all(residual >= 0. .and. residual <= tolerance) &
       ,diagnostics_string = "expected 0 <= " &  ! // string_t(residual) // "<= "// string_t(tolerance) &
      )
    end associate
  end function

  function functional_matches_procedural() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: tolerance = 1.E-06
    integer, parameter :: steps = 1000, n=21
    real, parameter :: alpha = 1.
    real, parameter :: side=1., boundary_val=1., internal_val=2.

    associate( T_f => T_functional(), T_p => T_procedural())
      associate(L_infinity_norm => maxval(abs(T_f - T_p)))
        test_diagnosis = test_diagnosis_t( &  
           test_passed = L_infinity_norm < tolerance &
          ,diagnostics_string = "expected " // string_t(L_infinity_norm) // " < " // string_t(tolerance) &
        )
      end associate
    end associate
&
  contains

    function T_functional()
      real, allocatable :: T_functional(:,:,:)
      type(subdomain_t) T
      integer step

      call T%define(side, boundary_val, internal_val, n)

      associate(dt => T%dt_stable(alpha))
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

      associate(dt => T%dt_stable(alpha))
        do step = 1, steps
          call T%step(alpha*dt)
        end do
      end associate

      T_procedural = T%values()
    end function

  end function
end module subdomain_test_m