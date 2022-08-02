module assert_equals_double_precision_matrix_test
    use iso_varying_string, only: var_str
    use garden, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_not, &
            assert_that, &
            describe, &
            it

    implicit none
    private
    public :: test_assert_eq_double_mat

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
contains
    function test_assert_eq_double_mat() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "assert_equals with double precision matrices", &
                [ it( &
                        "passes with the same matrix", &
                        check_pass_for_same_matrix) &
                , it( &
                        "fails for matrices with different numbers", &
                        check_fail_for_different_numbers) &
                , it( &
                        "fails for matrices with different sizes", &
                        check_fail_for_different_sizes) &
                ])
    end function

    pure function check_pass_for_same_matrix() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate(example => reshape([1.0d0, 3.0d0, 2.0d0, 4.0d0], [2, 2]))
            example_result = assert_equals(example, example)
            example_result_c = assert_equals(example, example, BOTH_MESSAGE)
            example_result_s = assert_equals(example, example, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals( &
                    example, example, SUCCESS_MESSAGE, FAILURE_MESSAGE)
            example_result_cs = assert_equals( &
                    example, example, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals( &
                    example, example, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
            example_result_ss = assert_equals( &
                    example, example, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
        end associate

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
    end function

    pure function check_fail_for_different_numbers() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate( &
                example1 => reshape([1.0d0, 3.0d0, 2.0d0, 4.0d0], [2, 2]), &
                example2 => reshape([5.0d0, 7.0d0, 6.0d0, 8.0d0], [2, 2]))
            example_result = assert_equals(example1, example2)
            example_result_c = assert_equals(example1, example2, BOTH_MESSAGE)
            example_result_s = assert_equals(example1, example2, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals( &
                    example1, example2, SUCCESS_MESSAGE, FAILURE_MESSAGE)
            example_result_cs = assert_equals( &
                    example1, example2, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals( &
                    example1, example2, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
            example_result_ss = assert_equals( &
                    example1, example2, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
        end associate

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

    pure function check_fail_for_different_sizes() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate( &
                example1 => reshape([1.0d0, 3.0d0, 2.0d0, 4.0d0], [2, 2]), &
                example2 => reshape([1.0d0, 3.0d0, 5.0d0, 2.0d0, 4.0d0, 6.0d0], [3, 2]))
            example_result = assert_equals(example1, example2)
            example_result_c = assert_equals(example1, example2, BOTH_MESSAGE)
            example_result_s = assert_equals(example1, example2, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals( &
                    example1, example2, SUCCESS_MESSAGE, FAILURE_MESSAGE)
            example_result_cs = assert_equals( &
                    example1, example2, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals( &
                    example1, example2, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
            example_result_ss = assert_equals( &
                    example1, example2, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
        end associate

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
