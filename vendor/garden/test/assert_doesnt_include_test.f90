module assert_doesnt_include_test
    use iso_varying_string, only: varying_string, char, var_str
    use garden, only: &
            input_t, &
            result_t, &
            string_input_t, &
            test_item_t, &
            assert_doesnt_include, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it, &
            ASCII_STRING_GENERATOR

    implicit none
    private
    public :: test_assert_includes

    character(len=*), parameter :: BOTH_MESSAGE = "Both Message"
    character(len=*), parameter :: SUCCESS_MESSAGE = "Success Message"
    character(len=*), parameter :: FAILURE_MESSAGE = "Failure Message"
contains
    function test_assert_includes() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "assert_doesnt_include", &
                [ it( &
                        "passes with different strings", &
                        check_pass_for_different_strings) &
                , it( &
                        "fails with the same string", &
                        ASCII_STRING_GENERATOR, &
                        check_fail_for_same_string) &
                ])
    end function

    pure function check_pass_for_different_strings() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: ONE_STRING = "One String"
        character(len=*), parameter :: OTHER_STRING = "Other String"
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
        type(result_t) :: example_result_cccc
        type(result_t) :: example_result_cccs
        type(result_t) :: example_result_ccsc
        type(result_t) :: example_result_ccss
        type(result_t) :: example_result_cscc
        type(result_t) :: example_result_cscs
        type(result_t) :: example_result_cssc
        type(result_t) :: example_result_csss
        type(result_t) :: example_result_sccc
        type(result_t) :: example_result_sccs
        type(result_t) :: example_result_scsc
        type(result_t) :: example_result_scss
        type(result_t) :: example_result_sscc
        type(result_t) :: example_result_sscs
        type(result_t) :: example_result_sssc
        type(result_t) :: example_result_ssss

        example_result_cc = assert_doesnt_include( &
                ONE_STRING, OTHER_STRING)
        example_result_cs = assert_doesnt_include( &
                ONE_STRING, var_str(OTHER_STRING))
        example_result_sc = assert_doesnt_include( &
                var_str(ONE_STRING), OTHER_STRING)
        example_result_ss = assert_doesnt_include( &
                var_str(ONE_STRING), var_str(OTHER_STRING))
        example_result_ccc = assert_doesnt_include( &
                ONE_STRING, OTHER_STRING, BOTH_MESSAGE)
        example_result_ccs = assert_doesnt_include( &
                ONE_STRING, OTHER_STRING, var_str(BOTH_MESSAGE))
        example_result_csc = assert_doesnt_include( &
                ONE_STRING, var_str(OTHER_STRING), BOTH_MESSAGE)
        example_result_css = assert_doesnt_include( &
                ONE_STRING, var_str(OTHER_STRING), var_str(BOTH_MESSAGE))
        example_result_scc = assert_doesnt_include( &
                var_str(ONE_STRING), OTHER_STRING, BOTH_MESSAGE)
        example_result_scs = assert_doesnt_include( &
                var_str(ONE_STRING), OTHER_STRING, var_str(BOTH_MESSAGE))
        example_result_ssc = assert_doesnt_include( &
                var_str(ONE_STRING), var_str(OTHER_STRING), BOTH_MESSAGE)
        example_result_sss = assert_doesnt_include( &
                var_str(ONE_STRING), var_str(OTHER_STRING), var_str(BOTH_MESSAGE))
        example_result_cccc = assert_doesnt_include( &
                ONE_STRING, &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_cccs = assert_doesnt_include( &
                ONE_STRING, &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_ccsc = assert_doesnt_include( &
                ONE_STRING, &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_ccss = assert_doesnt_include( &
                ONE_STRING, &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_cscc = assert_doesnt_include( &
                ONE_STRING, &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_cscs = assert_doesnt_include( &
                ONE_STRING, &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_cssc = assert_doesnt_include( &
                ONE_STRING, &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_csss = assert_doesnt_include( &
                ONE_STRING, &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_sccc = assert_doesnt_include( &
                var_str(ONE_STRING), &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_sccs = assert_doesnt_include( &
                var_str(ONE_STRING), &
                OTHER_STRING, &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_scsc = assert_doesnt_include( &
                var_str(ONE_STRING), &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_scss = assert_doesnt_include( &
                var_str(ONE_STRING), &
                OTHER_STRING, &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))
        example_result_sscc = assert_doesnt_include( &
                var_str(ONE_STRING), &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                FAILURE_MESSAGE)
        example_result_sscs = assert_doesnt_include( &
                var_str(ONE_STRING), &
                var_str(OTHER_STRING), &
                SUCCESS_MESSAGE, &
                var_str(FAILURE_MESSAGE))
        example_result_sssc = assert_doesnt_include( &
                var_str(ONE_STRING), &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                FAILURE_MESSAGE)
        example_result_ssss = assert_doesnt_include( &
                var_str(ONE_STRING), &
                var_str(OTHER_STRING), &
                var_str(SUCCESS_MESSAGE), &
                var_str(FAILURE_MESSAGE))

        result_ = &
                assert_that( &
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
                        example_result_sss%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_cccc%passed(), &
                        example_result_cccc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_cccs%passed(), &
                        example_result_cccs%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_ccsc%passed(), &
                        example_result_ccsc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_ccss%passed(), &
                        example_result_ccss%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_cscc%passed(), &
                        example_result_cscc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_cscs%passed(), &
                        example_result_cscs%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_cssc%passed(), &
                        example_result_cssc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_csss%passed(), &
                        example_result_csss%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_sccc%passed(), &
                        example_result_sccc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_sccs%passed(), &
                        example_result_sccs%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_scsc%passed(), &
                        example_result_scsc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_scss%passed(), &
                        example_result_scss%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_sscc%passed(), &
                        example_result_sscc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_sscs%passed(), &
                        example_result_sscs%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_sssc%passed(), &
                        example_result_sssc%verbose_description(.false.)) &
                .and.assert_that( &
                        example_result_ssss%passed(), &
                        example_result_ssss%verbose_description(.false.))
    end function

    pure function check_fail_for_same_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

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
        type(result_t) :: example_result_cccc
        type(result_t) :: example_result_cccs
        type(result_t) :: example_result_ccsc
        type(result_t) :: example_result_ccss
        type(result_t) :: example_result_cscc
        type(result_t) :: example_result_cscs
        type(result_t) :: example_result_cssc
        type(result_t) :: example_result_csss
        type(result_t) :: example_result_sccc
        type(result_t) :: example_result_sccs
        type(result_t) :: example_result_scsc
        type(result_t) :: example_result_scss
        type(result_t) :: example_result_sscc
        type(result_t) :: example_result_sscs
        type(result_t) :: example_result_sssc
        type(result_t) :: example_result_ssss
        type(varying_string) :: example

        select type (input)
        type is (string_input_t)
            example = input%input()
            example_result_cc = assert_doesnt_include( &
                    char(example), char(example))
            example_result_cs = assert_doesnt_include( &
                    char(example), example)
            example_result_sc = assert_doesnt_include( &
                    example, char(example))
            example_result_ss = assert_doesnt_include( &
                    example, example)
            example_result_ccc = assert_doesnt_include( &
                    char(example), char(example), BOTH_MESSAGE)
            example_result_ccs = assert_doesnt_include( &
                    char(example), char(example), var_str(BOTH_MESSAGE))
            example_result_csc = assert_doesnt_include( &
                    char(example), example, BOTH_MESSAGE)
            example_result_css = assert_doesnt_include( &
                    char(example), example, var_str(BOTH_MESSAGE))
            example_result_scc = assert_doesnt_include( &
                    example, char(example), BOTH_MESSAGE)
            example_result_scs = assert_doesnt_include( &
                    example, char(example), var_str(BOTH_MESSAGE))
            example_result_ssc = assert_doesnt_include( &
                    example, example, BOTH_MESSAGE)
            example_result_sss = assert_doesnt_include( &
                    example, example, var_str(BOTH_MESSAGE))
            example_result_cccc = assert_doesnt_include( &
                    char(example), &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cccs = assert_doesnt_include( &
                    char(example), &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_ccsc = assert_doesnt_include( &
                    char(example), &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ccss = assert_doesnt_include( &
                    char(example), &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_cscc = assert_doesnt_include( &
                    char(example), &
                    example, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_cscs = assert_doesnt_include( &
                    char(example), &
                    example, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_cssc = assert_doesnt_include( &
                    char(example), &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_csss = assert_doesnt_include( &
                    char(example), &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_sccc = assert_doesnt_include( &
                    example, &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_sccs = assert_doesnt_include( &
                    example, &
                    char(example), &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_scsc = assert_doesnt_include( &
                    example, &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_scss = assert_doesnt_include( &
                    example, &
                    char(example), &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            example_result_sscc = assert_doesnt_include( &
                    example, &
                    example, &
                    SUCCESS_MESSAGE, &
                    FAILURE_MESSAGE)
            example_result_sscs = assert_doesnt_include( &
                    example, &
                    example, &
                    SUCCESS_MESSAGE, &
                    var_str(FAILURE_MESSAGE))
            example_result_sssc = assert_doesnt_include( &
                    example, &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    FAILURE_MESSAGE)
            example_result_ssss = assert_doesnt_include( &
                    example, &
                    example, &
                    var_str(SUCCESS_MESSAGE), &
                    var_str(FAILURE_MESSAGE))
            result_ = &
                    assert_not( &
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
                            example_result_sss%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_cccc%passed(), &
                            example_result_cccc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_cccs%passed(), &
                            example_result_cccs%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_ccsc%passed(), &
                            example_result_ccsc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_ccss%passed(), &
                            example_result_ccss%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_cscc%passed(), &
                            example_result_cscc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_cscs%passed(), &
                            example_result_cscs%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_cssc%passed(), &
                            example_result_cssc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_csss%passed(), &
                            example_result_csss%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_sccc%passed(), &
                            example_result_sccc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_sccs%passed(), &
                            example_result_sccs%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_scsc%passed(), &
                            example_result_scsc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_scss%passed(), &
                            example_result_scss%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_sscc%passed(), &
                            example_result_sscc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_sscs%passed(), &
                            example_result_sscs%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_sssc%passed(), &
                            example_result_sssc%verbose_description(.false.)) &
                    .and.assert_not( &
                            example_result_ssss%passed(), &
                            example_result_ssss%verbose_description(.false.))
        class default
            result_ = fail("Expected a string_input_t")
        end select
    end function
end module
