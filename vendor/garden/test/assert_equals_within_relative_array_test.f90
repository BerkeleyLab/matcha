module assert_equals_within_relative_array_test
    use iso_varying_string, only: var_str
    use garden, only: &
        result_t, &
        test_item_t, &
        assert_equals_within_relative, &
        assert_not, &
        assert_that, &
        describe, &
        it

    implicit none
    private
    public :: test_assert_eq_within_rel_arr

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
contains
    function test_assert_eq_within_rel_arr() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "assert_equals_within_relative for arrays", &
                [ it( &
                        "passes with the same array, even with very small tolerance", &
                        check_pass_for_same_array) &
                , it( &
                        "fails for arrays with sufficiently different values", &
                        check_fail_for_different_arrays) &
                , it( &
                        "passes for arrays with sufficiently close values", &
                        check_pass_for_close_arrays) &
                , it( &
                        "fails for arrays of different lengths", &
                        check_fail_for_different_length_arrays) &
                ])
    end function

    pure function check_pass_for_same_array() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate(example => [1.0d0, 2.0d0])
            example_result = assert_equals_within_relative( &
                    example, example, tiny(0.0d0))
            example_result_c = assert_equals_within_relative( &
                    example, example, tiny(0.0d0), BOTH_MESSAGE)
            example_result_s = assert_equals_within_relative( &
                    example, example, tiny(0.0d0), var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_relative( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_relative( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_relative( &
                    example, &
                    example, &
                    tiny(0.0d0), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_relative( &
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

    pure function check_fail_for_different_arrays() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate(example => [1.0d0, 2.0d0])
            example_result = assert_equals_within_relative( &
                    example, example*1.11d0, 0.1d0)
            example_result_c = assert_equals_within_relative( &
                    example, example*1.11d0, 0.1d0, BOTH_MESSAGE)
            example_result_s = assert_equals_within_relative( &
                    example, example*1.11d0, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_relative( &
                    example, &
                    example*1.11d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_relative( &
                    example, &
                    example*1.11d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_relative( &
                    example, &
                    example*1.11d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_relative( &
                    example, &
                    example*1.11d0, &
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

    pure function check_pass_for_close_arrays() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate(example => [1.0d0, 2.0d0])
            example_result = assert_equals_within_relative( &
                    example, example*1.09d0, 0.1d0)
            example_result_c = assert_equals_within_relative( &
                    example, example*1.09d0, 0.1d0, BOTH_MESSAGE)
            example_result_s = assert_equals_within_relative( &
                    example, example*1.09d0, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_relative( &
                    example, &
                    example*1.09d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_relative( &
                    example, &
                    example*1.09d0, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_relative( &
                    example, &
                    example*1.09d0, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_relative( &
                    example, &
                    example*1.09d0, &
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

    pure function check_fail_for_different_length_arrays() result(result_)
        type(result_t) :: result_

        type(result_t) :: example_result
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss

        associate(example1 => [1.0d0, 2.0d0], example2 => [1.0d0, 2.0d0, 3.0d0])
            example_result = assert_equals_within_relative( &
                    example1, example2, 0.1d0)
            example_result_c = assert_equals_within_relative( &
                    example1, example2, 0.1d0, BOTH_MESSAGE)
            example_result_s = assert_equals_within_relative( &
                    example1, example2, 0.1d0, var_str(BOTH_MESSAGE))
            example_result_cc = assert_equals_within_relative( &
                    example1, &
                    example2, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cs = assert_equals_within_relative( &
                    example1, &
                    example2, &
                    0.1d0, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sc = assert_equals_within_relative( &
                    example1, &
                    example2, &
                    0.1d0, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ss = assert_equals_within_relative( &
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
