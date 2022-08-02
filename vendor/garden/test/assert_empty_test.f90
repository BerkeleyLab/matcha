module assert_empty_test
    use iso_varying_string, only: var_str
    use garden, only: &
            result_t, &
            test_item_t, &
            assert_empty, &
            assert_not, &
            assert_that, &
            describe, &
            it

    implicit none
    private
    public :: test_assert_empty

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
contains
    function test_assert_empty() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "assert_empty", &
                [ it( &
                        "passes with an empty character", &
                        check_pass_for_empty_chars) &
                , it( &
                        "fails with a non empty character", &
                        check_fails_for_nonempty_chars) &
                ])
    end function

    pure function check_pass_for_empty_chars() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EMPTY = ""
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss
        type(result_t) :: example_result_ccc
        type(result_t) :: example_result_ccs
        type(result_t) :: example_result_csc
        type(result_t) :: example_result_css
        type(result_t) :: example_result_scc
        type(result_t) :: example_result_scs
        type(result_t) :: example_result_ssc
        type(result_t) :: example_result_sss

        example_result_c = assert_empty(EMPTY)
        example_result_s = assert_empty(var_str(EMPTY))
        example_result_cc = assert_empty(EMPTY, BOTH_MESSAGE)
        example_result_cs = assert_empty(EMPTY, var_str(BOTH_MESSAGE))
        example_result_sc = assert_empty(var_str(EMPTY), BOTH_MESSAGE)
        example_result_ss = assert_empty(var_str(EMPTY), var_str(BOTH_MESSAGE))
        example_result_ccc = assert_empty( &
                EMPTY, SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_ccs = assert_empty( &
                EMPTY, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_csc = assert_empty( &
                EMPTY, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_css = assert_empty( &
                EMPTY, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
        example_result_scc = assert_empty( &
                var_str(EMPTY), SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_scs = assert_empty( &
                var_str(EMPTY), SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_ssc = assert_empty( &
                var_str(EMPTY), var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_sss = assert_empty( &
                var_str(EMPTY), var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))

        result_ = &
                assert_that( &
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
                        example_result_ss%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_ccc%passed(), &
                        example_result_ccc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_ccs%passed(), &
                        example_result_ccs%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_csc%passed(), &
                        example_result_csc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_css%passed(), &
                        example_result_css%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_scc%passed(), &
                        example_result_scc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_scs%passed(), &
                        example_result_scs%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_ssc%passed(), &
                        example_result_ssc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_sss%passed(), &
                        example_result_sss%verbose_description(.false.))
    end function

    pure function check_fails_for_nonempty_chars() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: NOT_EMPTY = "Not Empty"
        type(result_t) :: example_result_c
        type(result_t) :: example_result_s
        type(result_t) :: example_result_cc
        type(result_t) :: example_result_cs
        type(result_t) :: example_result_sc
        type(result_t) :: example_result_ss
        type(result_t) :: example_result_ccc
        type(result_t) :: example_result_ccs
        type(result_t) :: example_result_csc
        type(result_t) :: example_result_css
        type(result_t) :: example_result_scc
        type(result_t) :: example_result_scs
        type(result_t) :: example_result_ssc
        type(result_t) :: example_result_sss

        example_result_c = assert_empty(NOT_EMPTY)
        example_result_s = assert_empty(var_str(NOT_EMPTY))
        example_result_cc = assert_empty(NOT_EMPTY, BOTH_MESSAGE)
        example_result_cs = assert_empty(NOT_EMPTY, var_str(BOTH_MESSAGE))
        example_result_sc = assert_empty(var_str(NOT_EMPTY), BOTH_MESSAGE)
        example_result_ss = assert_empty(var_str(NOT_EMPTY), var_str(BOTH_MESSAGE))
        example_result_ccc = assert_empty( &
                NOT_EMPTY, SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_ccs = assert_empty( &
                NOT_EMPTY, SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_csc = assert_empty( &
                NOT_EMPTY, var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_css = assert_empty( &
                NOT_EMPTY, var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))
        example_result_scc = assert_empty( &
                var_str(NOT_EMPTY), SUCCESS_MESSAGE, FAILURE_MESSAGE)
        example_result_scs = assert_empty( &
                var_str(NOT_EMPTY), SUCCESS_MESSAGE, var_str(FAILURE_MESSAGE))
        example_result_ssc = assert_empty( &
                var_str(NOT_EMPTY), var_str(SUCCESS_MESSAGE), FAILURE_MESSAGE)
        example_result_sss = assert_empty( &
                var_str(NOT_EMPTY), var_str(SUCCESS_MESSAGE), var_str(FAILURE_MESSAGE))

        result_ = &
                assert_not( &
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
                        example_result_ss%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_ccc%passed(), &
                        example_result_ccc%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_ccs%passed(), &
                        example_result_ccs%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_csc%passed(), &
                        example_result_csc%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_css%passed(), &
                        example_result_css%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_scc%passed(), &
                        example_result_scc%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_scs%passed(), &
                        example_result_scs%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_ssc%passed(), &
                        example_result_ssc%verbose_description(.false.)) &
                .and.assert_not( &
                        example_result_sss%passed(), &
                        example_result_sss%verbose_description(.false.))
    end function
end module
