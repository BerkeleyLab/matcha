module ichar_test
    use ascii_character_generator_m, only: ASCII_CHARACTER_GENERATOR
    use character_input_m, only: character_input_t
    use iso_varying_string, only: ichar, var_str
    use veggies, only: &
            input_t, result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_ichar
contains
    function test_ichar() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.4.5: ICHAR", &
                [ it( &
                        "works the same for characters and strings", &
                        ASCII_CHARACTER_GENERATOR, &
                        check_ichar) &
                ])
    end function

    pure function check_ichar(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (character_input_t)
            result_ = assert_equals( &
                    ichar(input%input()), &
                    ichar(var_str(input%input())), &
                    input%input())
        class default
            result_ = fail("Expected to get a character_input_t.")
        end select
    end function
end module
