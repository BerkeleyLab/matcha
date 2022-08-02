module remove_test
    use iso_varying_string, only: remove, var_str
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_remove_character, test_remove_string
contains
    function test_remove_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.3 REMOVE character", &
                [ it( &
                        "The result value is a copy of the characters of the argument" &
                        // " string between positions start and finish, inclusive.", &
                        check_remove_character) &
                , it( &
                        "If start is absent, the value one is used for start.", &
                        check_remove_character_without_start) &
                , it( &
                        "If start is less than one, the value one is used for start.", &
                        check_remove_character_with_start_lt_one) &
                , it( &
                        "If finish is absent, the value LEN(string) is used for finish.", &
                        check_remove_character_without_finish) &
                , it( &
                        "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                        check_remove_character_with_finish_gt_len_string) &
                , it( &
                        "If finish is less than start, the characters of string are delivered unchanged.", &
                        check_remove_character_zero_length) &
                ])
    end function

    function test_remove_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.3 REMOVE string", &
                [ it( &
                        "The result value is a copy of the characters of the argument" &
                        // " string between positions start and finish, inclusive.", &
                        check_remove_string) &
                , it( &
                        "If start is absent, the value one is used for start.", &
                        check_remove_string_without_start) &
                , it( &
                        "If start is less than one, the value one is used for start.", &
                        check_remove_string_with_start_lt_one) &
                , it( &
                        "If finish is absent, the value LEN(string) is used for finish.", &
                        check_remove_string_without_finish) &
                , it( &
                        "If finish is greater than LEN(string), the value LEN(string) is used for finish.", &
                        check_remove_string_with_finish_gt_len_string) &
                , it( &
                        "If finish is less than start, the characters of string are delivered unchanged.", &
                        check_remove_string_zero_length) &
                ])
    end function

    pure function check_remove_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("EPLE", remove("EXAMPLE", 2, 4))
    end function

    pure function check_remove_character_without_start() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("PLE", remove("EXAMPLE", finish = 4))
    end function

    pure function check_remove_character_with_start_lt_one() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("PLE", remove("EXAMPLE", -1, 4))
    end function

    pure function check_remove_character_without_finish() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("E", remove("EXAMPLE", 2))
    end function

    pure function check_remove_character_with_finish_gt_len_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("E", remove("EXAMPLE", 2, 8))
    end function

    pure function check_remove_character_zero_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("EXAMPLE", remove("EXAMPLE", 10, -2))
    end function

    pure function check_remove_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("EPLE", remove(var_str("EXAMPLE"), 2, 4))
    end function

    pure function check_remove_string_without_start() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("PLE", remove(var_str("EXAMPLE"), finish = 4))
    end function

    pure function check_remove_string_with_start_lt_one() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("PLE", remove(var_str("EXAMPLE"), -1, 4))
    end function

    pure function check_remove_string_without_finish() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("E", remove(var_str("EXAMPLE"), 2))
    end function

    pure function check_remove_string_with_finish_gt_len_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("E", remove(var_str("EXAMPLE"), 2, 8))
    end function

    pure function check_remove_string_zero_length() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("EXAMPLE", remove(var_str("EXAMPLE"), 10, -2))
    end function
end module
