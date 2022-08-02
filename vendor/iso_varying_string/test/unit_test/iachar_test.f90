module iachar_test
    use ascii_character_generator_m, only: ASCII_CHARACTER_GENERATOR
    use character_input_m, only: character_input_t
    use iso_varying_string, only: iachar, var_str
    use veggies, only: &
            input_t, result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_iachar
contains
    function test_iachar() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.4.4: IACHAR", &
                [ it( &
                        "works the same for characters and strings", &
                        ASCII_CHARACTER_GENERATOR, &
                        check_iachar) &
                ])
    end function

    pure function check_iachar(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (character_input_t)
            result_ = assert_equals( &
                    iachar(input%input()), &
                    iachar(var_str(input%input())), &
                    input%input())
        class default
            result_ = fail("Expected to get a character_input_t.")
        end select
    end function
end module
