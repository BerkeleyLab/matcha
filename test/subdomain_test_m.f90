! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module subdomain_test_m
  !! Define subdomain tests and procedures required for reporting results
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use subdomain_m, only : subdomain_t
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
        "computing a correctly shaped Laplacian for a 2D flat-topped, step-like plateau" &
      ], &
      [ correctly_shaped_laplacian()  &
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
    real, allocatable :: lap_f_vals(:,:)

    call f%define(side=1., boundary_val=1., internal_val=2., n=11) ! internally constant subdomain with a step down at the edges
    call output(f%values())
    laplacian_f = .laplacian. f
    lap_f_vals = laplacian_f%values()
    sync all
    if (this_image()==1) print *,"-------------------- lap_f_vals ______________________"
    sync all
    call output(lap_f_vals)

    associate(me => this_image(), n_subdomains => num_images(), nx => size(lap_f_vals,1), ny => size(lap_f_vals,2))
      associate(first_zero_in_x => merge(3, 1, me==1), last_zero_in_x => merge(nx-2, nx, me==n_subdomains))
        block
          real, parameter :: tolerance = 1.0E-06
          integer, parameter :: left_adjacent = 2, bottom_adjacent = 2 
          logical internally_zero, concave_down_edges, doubly_curved_left_corners, doubly_curved_right_corners

         associate(top_adjacent => ny - 1, right_adjacent => nx - 1)
            internally_zero = all(abs(lap_f_vals(first_zero_in_x:last_zero_in_x, 3:ny-2)) < tolerance)
            concave_down_edges  = all( &
              [lap_f_vals(left_adjacent:right_adjacent,bottom_adjacent), lap_f_vals(left_adjacent:right_adjacent,top_adjacent)] < 0&
            )
            if (me==1) then
              concave_down_edges  = concave_down_edges .and. all([lap_f_vals(left_adjacent,bottom_adjacent:top_adjacent)] < 0)
              associate( &
                lower_left => lap_f_vals(left_adjacent,bottom_adjacent), &
                middle_left => lap_f_vals(left_adjacent,bottom_adjacent+1:top_adjacent-1), &
                upper_left => lap_f_vals(left_adjacent,top_adjacent) &
              )
                doubly_curved_left_corners = &
                  all(abs(lower_left - 2.*middle_left) < tolerance) .and. all(abs(upper_left - 2.*middle_left) < tolerance)
              end associate
            end if
            if (me==n_subdomains) then
              concave_down_edges  = concave_down_edges .and. all([lap_f_vals(right_adjacent,bottom_adjacent:top_adjacent)] < 0)
              associate( &
                lower_right => lap_f_vals(right_adjacent, bottom_adjacent), &
                middle_right => lap_f_vals(right_adjacent, bottom_adjacent+1:top_adjacent-1), &
                upper_right => lap_f_vals(right_adjacent, top_adjacent) &
              )
                doubly_curved_right_corners = &
                  all(abs(lower_right - 2.*middle_right) < tolerance) .and. all(abs(upper_right - 2.*middle_right) < tolerance)
              end associate
            end if
            test_passes = &
               internally_zero .and. &
               concave_down_edges .and. &
               merge(doubly_curved_left_corners, .true., me==1) .and. &
               merge(doubly_curved_right_corners, .true., me==n_subdomains)
          end associate
        end block
      end associate
    end associate
    
  end function

end module subdomain_test_m
