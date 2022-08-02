module format_hanging_indented_test
    use iso_varying_string, only: varying_string, len
    use strff, only: format_hanging_indented, NEWLINE
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_format_hanging_indented
contains
    function test_format_hanging_indented() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "format_hanging_indented", &
                [ it("does nothing to a single line", check_single_line) &
                , it( &
                        "does nothing to a string that is already indented", &
                        check_already_formatted) &
                , it( &
                        "ensures that each but the first line of each paragraph has the specified number of leading spaces", &
                        check_complex) &
                ])
    end function

    pure function check_single_line() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("Test", format_hanging_indented("Test", 1))
    end function

    pure function check_already_formatted() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: example = &
                NEWLINE &
                // "First line" // NEWLINE &
                // "    Second line" // NEWLINE &
                // NEWLINE &
                // "Third line" // NEWLINE &
                // "    Fourth line" // NEWLINE
        type(varying_string) :: processed

        processed = format_hanging_indented(example, 4)
        result_ = &
                assert_equals(len(example), len(processed), "Lengths") &
                .and. assert_equals(example, processed)
    end function

    pure function check_complex() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: input = &
                NEWLINE &
                // "First line" // NEWLINE &
                // "Second line  " // NEWLINE &
                // "  " // NEWLINE &
                // " Third line" // NEWLINE &
                // "  Fourth line" // NEWLINE
        character(len=*), parameter :: expected = &
                NEWLINE &
                // "First line" // NEWLINE &
                // "    Second line  " // NEWLINE &
                // NEWLINE &
                // "Third line" // NEWLINE &
                // "    Fourth line" // NEWLINE
        type(varying_string) :: actual

        actual = format_hanging_indented(input, 4)
        result_ = &
                assert_equals(len(expected), len(actual), "Lengths") &
                .and. assert_equals(expected, actual)
    end function
end module
