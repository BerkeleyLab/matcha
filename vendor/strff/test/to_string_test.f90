module to_string_test
    use strff, only: to_string
    use veggies, only: test_item_t, result_t, assert_equals, describe, it

    implicit none
    private

    public :: test_to_string_for_doubles, test_to_string_for_integers
contains
    function test_to_string_for_doubles() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "to_string for doubles", &
                [ it( &
                        "includes zero after the decimal", &
                        check_includes_zero_after_decimal) &
                , it( &
                        "only keeps the specified number of digits", &
                        check_only_keeps_six_digits) &
                , it("handles zero correctly", check_handles_zero) &
                , it( &
                        "handles extreme numbers correctly", &
                        check_handles_extreme_numbers) &
                , it( &
                        "can do negative numbers", &
                        check_negative_numbers) &
                , it( &
                        "shortens round numbers with scientific notation", &
                        check_round_numbers) &
                ])
    end function

    function test_to_string_for_integers() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "to_string for integers", &
                [ it("works", check_to_string_for_integers) &
                ])
    end function

    pure function check_includes_zero_after_decimal() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals("1.0", to_string(1.0D0)) &
                .and.assert_equals("10.0", to_string(1.0D1))
    end function

    pure function check_only_keeps_six_digits() result(result_)
        type(result_t) :: result_

        result_ = &
            assert_equals("123457.0", to_string(123456.7D0, 6)) &
            .and.assert_equals("123456.0", to_string(123456.1D0, 6)) &
            .and.assert_equals("1.23457e6", to_string(1234567.0D0, 6)) &
            .and.assert_equals("1.23456e6", to_string(1234561.0D0, 6)) &
            .and.assert_equals("0.123457", to_string(0.1234567D0, 6)) &
            .and.assert_equals("0.123456", to_string(0.1234561D0, 6)) &
            .and.assert_equals("1.23457e-2", to_string(0.01234567D0, 6)) &
            .and.assert_equals("1.23456e-2", to_string(0.01234561D0, 6))
    end function

    pure function check_handles_zero() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals("0.0", to_string(0.0D0)) &
                .and.assert_equals("0.0", to_string(tiny(0.0D0)))
    end function

    pure function check_handles_extreme_numbers() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals("1.23456e11", to_string(1.23456D11, 6)) &
                .and.assert_equals("1.23457e11", to_string(1.234567D11, 6)) &
                .and.assert_equals("1.23456e11", to_string(1.234561D11, 6)) &
                .and.assert_equals("1.23456e111", to_string(1.23456D111, 6)) &
                .and.assert_equals("1.23457e111", to_string(1.234567D111, 6)) &
                .and.assert_equals("1.23456e111", to_string(1.234561D111, 6)) &
                .and.assert_equals("1.23456e-11", to_string(1.23456D-11, 6)) &
                .and.assert_equals("1.23457e-11", to_string(1.234567D-11, 6)) &
                .and.assert_equals("1.23456e-11", to_string(1.234561D-11, 6)) &
                .and.assert_equals("1.23456e-111", to_string(1.23456D-111, 6)) &
                .and.assert_equals("1.23457e-111", to_string(1.234567D-111, 6)) &
                .and.assert_equals("1.23456e-111", to_string(1.234561D-111, 6))
    end function

    pure function check_negative_numbers() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals("-1.0", to_string(-1.0D0, 6)) &
                .and.assert_equals("-123457.0", to_string(-123456.7D0, 6)) &
                .and.assert_equals("-0.123457", to_string(-0.1234567D0, 6)) &
                .and.assert_equals("-1.23457e-2", to_string(-0.01234567D0, 6)) &
                .and.assert_equals("-1.23457e111", to_string(-1.234567D111, 6))
    end function

    pure function check_round_numbers() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("1.0e6", to_string(1.0D6))
    end function

    pure function check_to_string_for_integers() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals("1", to_string(1)) &
                .and.assert_equals("12", to_string(12)) &
                .and.assert_equals("-1", to_string(-1))
    end function
end module
