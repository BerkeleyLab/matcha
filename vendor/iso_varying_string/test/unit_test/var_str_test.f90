module var_str_test
    use iso_varying_string, only: char, var_str
    use veggies, only: &
            input_t, &
            result_t, &
            string_input_t, &
            test_item_t, &
            assert_equals, &
            describe, &
            fail, &
            it, &
            ASCII_STRING_GENERATOR

    implicit none
    private
    public :: test_var_str
contains
    function test_var_str() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.5.1: VAR_STR", &
                [ it( &
                        "Converts an intrinsic fixed-length character value into the" &
                        // " equivalent varying-length string value.", &
                        ASCII_STRING_GENERATOR, &
                        check_var_str) &
                ])
    end function

    pure function check_var_str(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_input_t)
            result_ = assert_equals( &
                    input%input(), &
                    var_str(char(input%input())), &
                    "The result value is the same string of characters as the argument.")
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function
end module
