module verify_test
    use ascii_string_pair_generator_m, only: ASCII_STRING_PAIR_GENERATOR
    use iso_varying_string, only: operator(//), char, verify
    use string_pair_input_m, only: string_pair_input_t
    use veggies, only: &
            input_t, result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_verify
contains
    function test_verify() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.4.16: VERIFY functions the same as for two characters for", &
                [ it( &
                        "two strings", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_verify_strings) &
                , it( &
                        "a string and a character", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_verify_string_and_character) &
                , it( &
                        "a character and a string", &
                        ASCII_STRING_PAIR_GENERATOR, &
                        check_verify_character_and_string) &
                ])
    end function

    pure function check_verify_strings(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    verify(char(input%first()), char(input%second())), &
                    verify(input%first(), input%second()), &
                    'verify("' // input%first() // '", "' // input%second() // '")') &
                .and.assert_equals( &
                    verify(char(input%first()), char(input%second()), .false.), &
                    verify(input%first(), input%second(), .false.), &
                    'verify("' // input%first() // '", "' // input%second() // '", .false.)') &
                .and.assert_equals( &
                    verify(char(input%first()), char(input%second()), .true.), &
                    verify(input%first(), input%second(), .true.), &
                    'verify("' // input%first() // '", "' // input%second() // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_verify_string_and_character(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    verify(char(input%first()), char(input%second())), &
                    verify(input%first(), char(input%second())), &
                    'verify("' // input%first() // '", "' // input%second() // '")') &
                .and.assert_equals( &
                    verify(char(input%first()), char(input%second()), .false.), &
                    verify(input%first(), char(input%second()), .false.), &
                    'verify("' // input%first() // '", "' // input%second() // '", .false.)') &
                .and.assert_equals( &
                    verify(char(input%first()), char(input%second()), .true.), &
                    verify(input%first(), char(input%second()), .true.), &
                    'verify("' // input%first() // '", "' // input%second() // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function

    pure function check_verify_character_and_string(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_pair_input_t)
            result_ = &
                assert_equals( &
                    verify(char(input%first()), char(input%second())), &
                    verify(char(input%first()), input%second()), &
                    'verify("' // input%first() // '", "' // input%second() // '")') &
                .and.assert_equals( &
                    verify(char(input%first()), char(input%second()), .false.), &
                    verify(char(input%first()), input%second(), .false.), &
                    'verify("' // input%first() // '", "' // input%second() // '", .false.)') &
                .and.assert_equals( &
                    verify(char(input%first()), char(input%second()), .true.), &
                    verify(char(input%first()), input%second(), .true.), &
                    'verify("' // input%first() // '", "' // input%second() // '", .true.)')
        class default
            result_ = fail("Expected to get a string_pair_input_t")
        end select
    end function
end module
