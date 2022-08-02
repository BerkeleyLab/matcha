module assert_equals_within_absolute_matrix_test
    use iso_varying_string, only: var_str
    use garden, only: &
        result_t, &
        test_item_t, &
        assert_equals_within_absolute, &
        assert_not, &
        assert_that, &
        describe, &
        it

    implicit none
    private
    public :: test_assert_eq_within_abs_mat

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
contains
    function test_assert_eq_within_abs_mat() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "assert_equals_within_absolute for matrices", &
                [ it( &
                        "passes with the same matrix, even with very small tolerance", &
                        check_pass_for_same_matrix) &
                , it( &
                        "fails for matrices with sufficiently different values", &
                        check_fail_for_different_matrices) &
                , it( &
                        "passes for matrices with sufficiently close values", &
                        check_pass_for_close_matrices) &
                , it( &
                        "fails for matrices of different lengths", &
                        check_fail_for_different_length_matrices) &
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
            example_result = assert_equals_within_absolute( &
                    example, example, tiny(0.0d0))
            example_result_c = assert_equals_within_absolute( &
                    example, example, tiny(0.0d0), BOTH_MESSAGE)
            example_result_s = assert_equals_within_absolute( &
                    example, example, tiny(0.0d0), var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_absolute( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_absolute( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_absolute( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_absolute( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
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

    pure function check_fail_for_different_matrices() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate(example => reshape([1.0d0, 3.0d0, 2.0d0, 4.0d0], [2, 2]))
            example_result = assert_equals_within_absolute( &
                    example, example+0.2d0, 0.1d0)
            example_result_c = assert_equals_within_absolute( &
                    example, example+0.2d0, 0.1d0, BOTH_MESSAGE)
            example_result_s = assert_equals_within_absolute( &
                    example, example+0.2d0, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_absolute( &
                    example, &
                    example+0.2d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_absolute( &
                    example, &
                    example+0.2d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_absolute( &
                    example, &
                    example+0.2d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_absolute( &
                    example, &
                    example+0.2d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
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

    pure function check_pass_for_close_matrices() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate(example => reshape([1.0d0, 3.0d0, 2.0d0, 4.0d0], [2, 2]))
            example_result = assert_equals_within_absolute( &
                    example, example+0.05d0, 0.1d0)
            example_result_c = assert_equals_within_absolute( &
                    example, example+0.05d0, 0.1d0, BOTH_MESSAGE)
            example_result_s = assert_equals_within_absolute( &
                    example, example+0.05d0, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_absolute( &
                    example, &
                    example+0.05d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_absolute( &
                    example, &
                    example+0.05d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_absolute( &
                    example, &
                    example+0.05d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_absolute( &
                    example, &
                    example+0.05d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
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

    pure function check_fail_for_different_length_matrices() result(result_)
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
            example_result = assert_equals_within_absolute( &
                    example1, example2, 0.1d0)
            example_result_c = assert_equals_within_absolute( &
                    example1, example2, 0.1d0, BOTH_MESSAGE)
            example_result_s = assert_equals_within_absolute( &
                    example1, example2, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_absolute( &
                    example1, &
                    example2, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_absolute( &
                    example1, &
                    example2, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_absolute( &
                    example1, &
                    example2, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_absolute( &
                    example1, &
                    example2, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
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
