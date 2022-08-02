module llt_test
    use ascii_string_pair_generator_m, only: ASCII_STRING_PAIR_GENERATOR
    use iso_varying_string, only: operator(//), char, llt
    use string_pair_input_m, only: string_pair_input_t
    use veggies, only: &
            input_t, result_t, test_item_t, assert_that, describe, fail, it

    implicit none
    private

    public :: test_llt
contains
    function test_llt() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.4.12: LLT functions the same as for two characters for", &
                [ it( &
                        "two strings", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_string_llt_string) &
                , it( &
                        "a character and a string", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_character_llt_string) &
                , it( &
                        "a string and a character", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_string_llt_character) &
                ])
    end function

    pure function check_string_llt_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    llt(char(input%first()), char(input%second())) &
                    .eqv. llt(input%first(), input%second()), &
                    'llt("' // input%first() // '", "' // input%second() // '")')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_character_llt_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    llt(char(input%first()), char(input%second())) &
                    .eqv. llt(char(input%first()), input%second()), &
                    'llt("' // input%first() // '", "' // input%second() // '")')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_string_llt_character(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = assert_that( &
                    llt(char(input%first()), char(input%second())) &
                    .eqv. llt(input%first(), char(input%second())), &
                    'llt("' // input%first() // '", "' // input%second() // '")')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
