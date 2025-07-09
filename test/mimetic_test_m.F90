! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module mimetic_test_m
  !! Define mimetic tests and procedures required for reporting results
  use julienne_m, only : &
     diagnosis_function_i &
    ,operator(.all.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,test_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t
  use mimetic_m, only : &
     mimetic_t &
    ,operator(.div.)
  implicit none

  private
  public :: mimetic_test_t

  type, extends(test_t) :: mimetic_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A mimetic_t object"
  end function

  function results() result(test_results)
    type(test_description_t), allocatable :: test_descriptions(:)
    type(test_result_t), allocatable :: test_results(:)

    test_descriptions = [ &
       test_description_t("the divergence of a gradient matching a Laplacian", div_grad_matches_laplacian) &
    ]
    test_results = test_descriptions%run()
  end function

  function div_grad_matches_laplacian() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real, parameter :: tolerance = 1.E-06
    type(mimetic_t) phi

    call phi%define(side=1., boundary_val=0., internal_val=0., n=21)

#ifndef __GFORTRAN__
    associate(div_grad_phi => .div. (.grad. phi) , laplacian_phi => .laplacian. phi)
      test_diagnosis = .all.(div_grad_phi%values() .approximates. laplacian_phi%values() .within. tolerance)
    end associate
#else
    block
      type(mimetic_t) div_grad_phi, laplacian_phi
      div_grad_phi = .div. (.grad. phi)
      laplacian_phi = .laplacian. phi
      test_diagnosis = .all.(div_grad_phi%values() .approximates. laplacian_phi%values() .within. tolerance)
    end block
#endif
  end function

end module mimetic_test_m