module len_trim_test
    use iso_varying_string, only: char, len_trim
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
    public :: test_len_trim
contains
    function test_len_trim() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec 3.4.8: LEN_TRIM", &
                [ it( &
                        "works the same for characters and strings", &
                        ASCII_STRING_GENERATOR, &
                        check_len_trim) &
                ])
    end function

    pure function check_len_trim(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_input_t)
            result_ = assert_equals( &
                    len_trim(char(input%input())), &
                    len_trim(input%input()), &
                    input%input())
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function
end module
