module concat_test
    use ascii_string_pair_generator_m, only: ASCII_STRING_PAIR_GENERATOR
    use iso_varying_string, only: operator(//), char
    use string_pair_input_m, only: string_pair_input_t
    use veggies, only: &
            input_t, result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_concat
contains
    function test_concat() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.3.2: operator(//) functions the same as for two characters for", &
                [ it( &
                        "two strings", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_concat_strings) &
                , it( &
                        "a string and a character", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_concat_string_and_character) &
                , it( &
                        "a character and a string", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_concat_character_and_string) &
                ])
    end function test_concat

    pure function check_concat_strings(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_equals( &
                    char(input%first()) // char(input%second()), &
                    input%first() // input%second())
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_concat_string_and_character(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_equals( &
                    char(input%first()) // char(input%second()), &
                    input%first() // char(input%second()))
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_concat_character_and_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_equals( &
                    char(input%first()) // char(input%second()), &
                    char(input%first()) // input%second())
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
