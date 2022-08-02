module assert_equals_integers_test
    use iso_varying_string, only: var_str
    use garden, only: &
            input_t, &
            integer_input_t, &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it, &
            INTEGER_GENERATOR

    implicit none
    private
    public :: test_assert_equals_integers

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
contains
    function test_assert_equals_integers() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "assert_equals with integers", &
                [ it( &
                        "passes with the same integer", &
                        INTEGER_GENERATOR, &
                        check_pass_for_same_integer) &
                , it( &
                        "fails with different integers", &
                        check_fail_for_different_integers) &
                ])
    end function

    pure function check_pass_for_same_integer(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss
        integer :: input_val

        select type (input)
        type is (integer_input_t)
            input_val = input%input()
            example_result = assert_equals(input_val, input_val)
            example_result_c = assert_equals(input_val, input_val, BOTH_MESSAGE)
            example_result_s = assert_equals(input_val, input_val, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals( &
                    input_val, input_val, SUCCESS_MESSAGE, FAILURE_MESSAGE)
            example_result_cs = assert_equals( &
                    input_val, input_val, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals( &
                    input_val, input_val, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
            example_result_ss = assert_equals( &
                    input_val, input_val, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
            result_ = &
                    assert_that( &
                            example_result%passed(), &
                            example_result%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_c%passed(), &
                            example_result_c%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_s%passed(), &
                            example_result_s%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_cc%passed(), &
                            example_result_cc%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_cs%passed(), &
                            example_result_cs%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_sc%passed(), &
                            example_result_sc%verbose_description(.false.)) &
                    .and.assert_that( &
                            example_result_ss%passed(), &
                            example_result_ss%verbose_description(.false.))
        class default
            result_ = fail("Expected to get an integer_input_t")
        end select
    end function

    pure function check_fail_for_different_integers() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        example_result = assert_equals(1, 2)
        example_result_c = assert_equals(1, 2, BOTH_MESSAGE)
        example_result_s = assert_equals(1, 2, var_str(BOTH_MESSAGE))
        example_result_cc = assert_equals( &
                1, 2, SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_cs = assert_equals( &
                1, 2, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_sc = assert_equals( &
                1, 2, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_ss = assert_equals( &
                1, 2, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))

        result_ = &
                assert_not( &
                        example_result%passed(), &
                        example_result%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_c%passed(), &
                        example_result_c%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_s%passed(), &
                        example_result_s%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_cc%passed(), &
                        example_result_cc%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_cs%passed(), &
                        example_result_cs%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_sc%passed(), &
                        example_result_sc%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_ss%passed(), &
                        example_result_ss%verbose_description(.false.))
    end function
end module
