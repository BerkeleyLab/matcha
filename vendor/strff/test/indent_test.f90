module indent_test
    use strff, only: indent, NEWLINE
    use veggies, only: test_item_t, result_t, assert_equals, describe, it

    implicit none
    private

    public :: test_indent
contains
    function test_indent() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "indent", &
                [ it("indents a single line", check_single_line) &
                , it("indents multiple lines", check_indents_correctly) &
                ])
    end function

    pure function check_single_line() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("    Test", indent("Test", 4))
    end function

    pure function check_indents_correctly() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: input = &
                "First Line" // NEWLINE &
                // "Second Line" // NEWLINE &
                // "Third Line"
        character(len=*), parameter :: expected = &
                "    First Line" // NEWLINE &
                // "    Second Line" // NEWLINE &
                // "    Third Line"

        result_ = assert_equals(expected, indent(input, 4))
    end function
end module
