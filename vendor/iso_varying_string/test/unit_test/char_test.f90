module char_test
    use iso_varying_string, only: char, var_str
    use veggies, only: &
            input_t, &
            result_t, &
            string_input_t, &
            test_item_t, &
            assert_empty, &
            assert_equals, &
            describe, &
            fail, &
            it, &
            ASCII_STRING_GENERATOR

    implicit none
    private
    public :: test_char
contains
    function test_char() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.4.3: CHAR", &
                [ it( &
                        "converts a varying string to a character with the same length", &
                        ASCII_STRING_GENERATOR, &
                        check_char_without_length) &
                , it( &
                        "converts a varying string to a shorter character", &
                        check_char_with_shorter_length) &
                , it( &
                        "converts a varying string to a longer character", &
                        check_char_with_longer_length) &
                , it( &
                        "gives a zero length character for length = 0", &
                        check_char_with_zero_length) &
                , it( &
                        "gives a zero length character for negative length", &
                        check_char_with_negative_length) &
                ])
    end function

    pure function check_char_without_length(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_input_t)
            result_ = assert_equals( &
                    input%input(), &
                    char(input%input()), &
                    "If length is absent, the result is a copy of the" &
                    // " characters in the argument string")
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function

    pure function check_char_with_shorter_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "EXAM", &
                char(var_str("EXAMPLE"), 4), &
                "If string is longer than length, result is truncated on the right.")
    end function

    pure function check_char_with_longer_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "EXAMPLE   ", &
                char(var_str("EXAMPLE"), 10), &
                "If string is shorter than length, the result is padded on the" &
                // " right with blanks.")
    end function

    pure function check_char_with_zero_length() result(result_)
        type(result_t) :: result_

        result_ = assert_empty( &
                char(var_str("EXAMPLE"), 0), &
                "If length is less than one, the result is of zero length.")
    end function

    pure function check_char_with_negative_length() result(result_)
        type(result_t) :: result_

        result_ = assert_empty( &
                char(var_str("EXAMPLE"), -1), &
                "If length is less than one, the result is of zero length.")
    end function
end module
