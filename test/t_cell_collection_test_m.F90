! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module t_cell_collection_test_m
  !! Define t_cell_collection tests and procedures required for reporting results
  use julienne_m, only : &
    operator(.csv.) &
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
   ,diagnosis_function_i &
#endif 
   ,string_t &
   ,test_t &
   ,test_description_t &
   ,test_diagnosis_t &
   ,test_result_t
  use t_cell_collection_m, only : t_cell_collection_t
  use iso_fortran_env, only : output_unit
  use input_m, only : input_t
  use matcha_m, only : matcha
  implicit none

  private
  public :: t_cell_collection_test_t

  type, extends(test_t) :: t_cell_collection_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A t_cell_collection_t object"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
      test_description_t("is constructed with positions in the specified domain", check_constructed_domain) &
     ,test_description_t("distributes cells across images" , check_cell_distribution) &
    ]
#else
    procedure(diagnosis_function_i), pointer :: &
       check_cell_distribution_ptr &
      ,check_constructed_domain_ptr

       check_cell_distribution_ptr => check_cell_distribution
       check_constructed_domain_ptr => check_constructed_domain
   
    test_descriptions = [ &
       test_description_t("constructing positions in the specified domain", check_constructed_domain_ptr) &
      ,test_description_t("distributing cells across images", check_cell_distribution_ptr) &
    ]
#endif
    test_results = test_descriptions%run()
  end function

  function check_constructed_domain() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, parameter :: ncells = 100, ndim = 3 
    double precision random_positions(ncells,ndim)
    double precision, parameter :: scale_factor=100.D0
    type(t_cell_collection_t) t_cell_collection

    call random_number(random_positions)    
    t_cell_collection = t_cell_collection_t(scale_factor*random_positions,time=0.D0)
         
    associate(constructed_positions => t_cell_collection%positions())
      test_diagnosis = test_diagnosis_t( &
         test_passed = all(0.D0 <= constructed_positions .and. constructed_positions <= scale_factor) &
        ,diagnostics_string = "expected 0 <= positions <= scale_factor, actual positions " & ! // string_t(constructed_positions) &
      )
    end associate
  end function
  
  function check_cell_distribution() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer cell_collection_size 
    type(t_cell_collection_t), allocatable :: history(:)

    associate(input => input_t())

      history = matcha(input)
      cell_collection_size = size(history(1)%positions(), 1)
      call co_sum(cell_collection_size)

      associate(num_cells => input%num_cells())
        test_diagnosis = test_diagnosis_t( &
           test_passed = num_cells == cell_collection_size &
          ,diagnostics_string = "expected " // string_t(num_cells) // " cells, actual " // string_t(cell_collection_size) &
        )
      end associate
    end associate
  end function

end module t_cell_collection_test_m
