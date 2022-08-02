module less_than_test
    use ascii_string_pair_generator_m, only: ASCII_STRING_PAIR_GENERATOR
    use iso_varying_string, only: operator(//), operator(<), char
    use string_pair_input_m, only: string_pair_input_t
    use veggies, only: &
            input_t, result_t, test_item_t, assert_that, describe, fail, it

    implicit none
    private
    public :: test_less_than
contains
    function test_less_than() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.3.3: operator(<) functions the same as for two characters for", &
                [ it( &
                        "two strings", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_string_less_than_string) &
                , it( &
                        "a character and a string", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_character_less_than_string) &
                , it( &
                        "a string and a character", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_string_less_than_character) &
                ])
    end function

    pure function check_string_less_than_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    char(input%first()) < char(input%second()) &
                    .eqv. input%first() < input%second(), &
                    '"' // input%first() // '" < "' // input%second() // '"')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_character_less_than_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    char(input%first()) < char(input%second()) &
                    .eqv. char(input%first()) < input%second(), &
                    '"' // input%first() // '" < "' // input%second() // '"')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_string_less_than_character(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    char(input%first()) < char(input%second()) &
                    .eqv. input%first() < char(input%second()), &
                    '"' // input%first() // '" < "' // input%second() // '"')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
