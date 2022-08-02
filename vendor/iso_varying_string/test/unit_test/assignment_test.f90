module assignment_test
    use iso_varying_string, only: varying_string, assignment(=), char, var_str
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
    public :: test_assignment
contains
    function test_assignment() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.3.1: assignment", &
                [ it( &
                        "can assign a character to a string", &
                        ASCII_STRING_GENERATOR, &
                        check_assign_character_to_string) &
                , it( &
                        "can assign a string to a string", &
                        ASCII_STRING_GENERATOR, &
                        check_assign_string_to_string) &
                , it( &
                        "can assign a string to a shorter character", &
                        check_assign_to_shorter_character) &
                , it( &
                        "can assign a string to a longer character", &
                        check_assign_to_longer_character) &
                ])
    end function

    pure function check_assign_character_to_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(varying_string) :: assigned

        select type (input)
        type is (string_input_t)
            assigned = char(input%input())
            result_ = assert_equals( &
                    input%input(), &
                    assigned, &
                    "Where the variable is of type VARYING_STRING, the length" &
                    // " of the variable becomes that of the expression")
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function

    pure function check_assign_string_to_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(varying_string) :: assigned

        select type (input)
        type is (string_input_t)
            assigned = input%input()
            result_ = assert_equals( &
                    input%input(), &
                    assigned, &
                    "Where the variable is of type VARYING_STRING, the length" &
                    // " of the variable becomes that of the expression")
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function

    pure function check_assign_to_shorter_character() result(result_)
        type(result_t) :: result_

        character(len=4) :: assigned

        assigned = var_str("EXAMPLE")
        result_ = assert_equals( &
                "EXAM", &
                assigned, &
                "if the expression string is longer than the declared length of" &
                // " the character variable, only the left-most characters are assigned.")
    end function

    pure function check_assign_to_longer_character() result(result_)
        type(result_t) :: result_

        character(len=10) :: assigned

        assigned = var_str("EXAMPLE")
        result_ = assert_equals( &
                "EXAMPLE   ", &
                assigned, &
                "If the character variable is longer than that of the string" &
                // " expression, it is padded on the right with blanks.")
    end function
end module
