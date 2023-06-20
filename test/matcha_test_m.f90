! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module matcha_test_m
  use test_m, only : test_t
  use test_result_m, only : test_result_t
  use input_m, only : input_t
  use output_m, only : output_t
  use matcha_m, only : matcha
  implicit none
  
  private
  public :: matcha_test_t
  
  type, extends(test_t) :: matcha_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type
  
contains 

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A matcha_t" 
  end function
  
  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    
    character(len=*), parameter :: longest_description = &
      "creates a simulated distribution matching an empirical distribution on each image" 
   

    test_results = test_result_t( &
      [ character(len=len(longest_description)) :: &
        "creates a simulated distribution matching an empirical distribution on each image", &
        "creates a global distribution matching an empirical distribution" &
      ], &
      [  compare_image_distributions(), &
         compare_global_distributions() &
       ] &
    )
  end function
  
  function compare_image_distributions() result(test_passes)
    logical test_passes
    type(output_t) output
    
    integer, parameter :: speed=1, freq=2 ! subscripts for speeds and frequencies
    double precision, parameter :: tolerance = 1.D-02

    associate(input => input_t())
      output = output_t(input, matcha(input))
      associate( &
        empirical_distribution => input%sample_distribution(), &
        simulated_distribution => output%simulated_distribution() &
      )
        associate( &
          diffmax_speeds=> maxval(abs(empirical_distribution(:,speed)-simulated_distribution(:,speed))), &
          diffmax_freqs => maxval(abs(empirical_distribution(:,freq)-simulated_distribution(:,freq))) &
        )
          test_passes = (diffmax_freqs < tolerance) .and. (diffmax_speeds < tolerance)
        end associate
      end associate
    end associate
  end function
  
  function compare_global_distributions() result(test_passes)
    logical test_passes
    type(output_t) output
    double precision, allocatable :: simulated_distribution(:,:)
    integer num_cells
    integer, parameter :: speed=1, freq=2 ! subscripts for speeds and frequencies
    double precision, parameter :: tolerance = 1.D-02

    associate(input => input_t())
      output = output_t(input, matcha(input))
      associate(empirical_distribution => input%sample_distribution())
        simulated_distribution = output%simulated_distribution()  
        num_cells = output%my_num_cells()
        simulated_distribution(:,freq) = num_cells*simulated_distribution(:,freq)
        call co_sum(simulated_distribution(:,freq), result_image=1)
        call co_sum(num_cells, result_image=1)
        if (this_image()/=1) then
          test_passes = .true.
        else
          simulated_distribution(:,freq) = simulated_distribution(:,freq)/dble(num_cells)
          associate( &
            diffmax_speeds=> maxval(abs(empirical_distribution(:,speed)-simulated_distribution(:,speed))), &
            diffmax_freqs => maxval(abs(empirical_distribution(:,freq)-simulated_distribution(:,freq))) &
          )
            test_passes = (diffmax_freqs < tolerance) .and. (diffmax_speeds < tolerance)
          end associate
        end if
      end associate
    end associate

  end function

end module matcha_test_m
