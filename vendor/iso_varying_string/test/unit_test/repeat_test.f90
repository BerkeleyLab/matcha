module repeat_test
    use ascii_string_and_integer_generator_m, only: &
            ASCII_STRING_AND_INTEGER_GENERATOR
    use iso_varying_string, only: char, repeat
    use string_and_integer_input_m, only: string_and_integer_input_t
    use veggies, only: &
            input_t, result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_repeat
contains
    function test_repeat() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.4.13: REPEAT", &
                [ it( &
                        "works the same for characters and strings", &
                        ASCII_STRING_AND_INTEGER_GENERATOR, &
                        check_repeat) &
                ])
    end function

    pure function check_repeat(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_and_integer_input_t)
            result_ = assert_equals( &
                    repeat(char(input%string()), input%integer_()), &
                    repeat(input%string(), input%integer_()))
        class default
            result_ = fail("Expected to get a string_and_integer_input_t")
        end select
    end function
end module
