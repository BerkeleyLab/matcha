module adjustr_test
    use iso_varying_string, only: adjustr, char
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
    public :: test_adjustr
contains
    function test_adjustr() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec 3.4.2: ADJUSTR", &
                [ it( &
                        "works the same for characters and strings", &
                        ASCII_STRING_GENERATOR, &
                        check_adjustr) &
                ])
    end function

    pure function check_adjustr(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (string_input_t)
            result_ = assert_equals( &
                    adjustr(char(input%input())), &
                    adjustr(input%input()))
        class default
            result_ = fail("Expected to get a string_input_t")
        end select
    end function
end module
