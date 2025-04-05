! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module matcha_test_m
  use julienne_m, only : &
     string_t &
#if __GFORTRAN__
    ,diagnosis_function_i &
    ,test_description_t &
#endif
    ,test_diagnosis_t &
    ,vector_test_description_t &
    ,test_result_t &
    ,test_t
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
    specimen = "A matcha_t object"
  end function
  
#ifndef __GFORTRAN__
  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(vector_test_description_t), allocatable :: vector_test_descriptions(:)
    integer i

    vector_test_descriptions = [ &
       vector_test_description_t([ &
          string_t("simulated speed distribution matching empirical distribution") &
         ,string_t("simulated frequency distribution matching empirical distribution") &
         ], compare_image_distributions) &
      ,vector_test_description_t([ &
          string_t("simulated global speed distribution matching empirical distribution") &
         ,string_t("simulated global frequency distribution matching empirical distribution") &
         ], compare_global_distributions) &
    ]
    do i = 1, size(vector_test_descriptions)
      test_results = vector_test_descriptions(i)%run()
    end do
  end function
#else
  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
    procedure(diagnosis_function_i), pointer :: &
       compare_image_distributions_ptr &
      ,compare_global_distributions_ptr

       compare_image_distributions_ptr => compare_image_distributions
       compare_global_distributions_ptr => compare_global_distributions

    test_descriptions = [ &
       test_description_t("matching simulated distributions to empirical distribution", compare_image_distributions_ptr) &
      ,test_description_t("matching simulated global distributions to empirical distribution", compare_global_distributions_ptr) &
    ]
    test_results = test_descriptions%run()
  end function
#endif
  
#ifndef __GFORTRAN__
  function compare_image_distributions() result(test_diagnoses)
    logical test_passes
    type(test_diagnosis_t), allocatable :: test_diagnoses(:)
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
          test_diagnoses = [ &
            test_diagnosis_t( &
              test_passed = diffmax_freqs < tolerance .and. diffmax_speeds < tolerance &
             ,diagnostics_string = "expected max freq < " // string_t(tolerance) // ", actual " // string_t(diffmax_freqs) &
            ) &
           ,test_diagnosis_t( &
              test_passed = diffmax_freqs < tolerance .and. diffmax_speeds < tolerance &
             ,diagnostics_string = "expected max speeds < " // string_t(tolerance) // ", actual " // string_t(diffmax_speeds) &
           ) &
          ]
        end associate
      end associate
    end associate
  end function
#else
  function compare_image_distributions() result(test_diagnosis)
    logical test_passes
    type(test_diagnosis_t) test_diagnosis
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
          test_diagnosis =  test_diagnosis_t( &
             test_passed = diffmax_freqs < tolerance .and. diffmax_speeds < tolerance &
            ,diagnostics_string = "diffmax_speeds " // string_t(diffmax_speeds) // ", diffmax_freqs " // string_t(diffmax_freqs) &
              // ", expected < " // string_t(tolerance) &
          )
        end associate
      end associate
    end associate
  end function
#endif

#ifndef __GFORTRAN__
  function compare_global_distributions() result(test_diagnoses)
    type(test_diagnosis_t), allocatable :: test_diagnoses(:)
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
          test_diagnoses = [test_diagnosis_t(test_passed=.true., diagnostics_string="")]
        else
          simulated_distribution(:,freq) = simulated_distribution(:,freq)/dble(num_cells)
          associate( &
            diffmax_speeds=> maxval(abs(empirical_distribution(:,speed)-simulated_distribution(:,speed))), &
            diffmax_freqs => maxval(abs(empirical_distribution(:,freq)-simulated_distribution(:,freq))) &
          )
          test_diagnoses = [ &
            test_diagnosis_t( &
               test_passed = diffmax_freqs < tolerance &
              ,diagnostics_string = "expected < " // string_t(tolerance) // ", actual " // string_t(diffmax_freqs) &
            ) &
            ,test_diagnosis_t( &
               test_passed = diffmax_speeds < tolerance &
              ,diagnostics_string = "expected < " // string_t(tolerance) // ", actual " // string_t(diffmax_speeds) &
            ) &
          ]
          end associate
        end if
      end associate
    end associate

  end function
#else
  function compare_global_distributions() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
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
          test_diagnosis = test_diagnosis_t(test_passed=.true., diagnostics_string="")
        else
          simulated_distribution(:,freq) = simulated_distribution(:,freq)/dble(num_cells)
          associate( &
            diffmax_speeds=> maxval(abs(empirical_distribution(:,speed)-simulated_distribution(:,speed))), &
            diffmax_freqs => maxval(abs(empirical_distribution(:,freq)-simulated_distribution(:,freq))) &
          )
            test_diagnosis =  test_diagnosis_t( &
               test_passed = diffmax_freqs < tolerance .and. diffmax_speeds < tolerance &
              ,diagnostics_string = "diffmax_speeds " // string_t(diffmax_speeds) // ", diffmax_freqs " // string_t(diffmax_freqs) &
                // ", expected < " // string_t(tolerance) &
            )
          end associate
        end if
      end associate
    end associate

  end function
#endif

end module matcha_test_m
