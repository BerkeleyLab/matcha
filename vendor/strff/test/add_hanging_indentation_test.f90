module add_hanging_indentation_test
    use iso_varying_string, only: varying_string, len
    use strff, only: add_hanging_indentation, NEWLINE
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_add_hanging_indentation
contains
    function test_add_hanging_indentation() result(tests)
      type(test_item_t) :: tests

      tests = describe( &
              "add_hanging_indentation", &
              [ it("does nothing to a single line", check_single_line) &
              , it("indents all but the first line", check_simple_indentation) &
              , it("ignores empty lines", check_ignore_empty) &
              ])
    end function

    pure function check_single_line() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("Test", add_hanging_indentation("Test", 1))
    end function

    pure function check_simple_indentation() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: input = &
                "First Line" // NEWLINE &
                // "Second Line" // NEWLINE &
                // "Third Line"
        character(len=*), parameter :: expected = &
                "First Line" // NEWLINE &
                // "    Second Line" // NEWLINE &
                // "    Third Line"

        result_ = assert_equals(expected, add_hanging_indentation(input, 4))
    end function

    pure function check_ignore_empty() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: input = &
                NEWLINE // "First Line" // NEWLINE &
                // "  " // NEWLINE &
                // "Second Line" // NEWLINE &
                // "Third Line" // NEWLINE
        character(len=*), parameter :: expected = &
                NEWLINE // "    First Line" // NEWLINE &
                // "  " // NEWLINE &
                // "    Second Line" // NEWLINE &
                // "    Third Line" // NEWLINE
        type(varying_string) :: actual

        actual = add_hanging_indentation(input, 4)
        result_ = &
                assert_equals(len(expected), len(actual), "Lengths") &
                .and. assert_equals(expected, actual)
    end function
end module
