module replace_range_test
    use iso_varying_string, only: replace, var_str
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: &
            test_replace_character_in_character_range, &
            test_replace_character_in_string_range, &
            test_replace_string_in_character_range, &
            test_replace_string_in_string_range
contains
    function test_replace_character_in_character_range() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE in character with character in range", &
                [ it( &
                        "The characters in the copy of string between positions start" &
                        // " and finish, including those at start and finish, are" &
                        // " deleted and replaced by characters of substring.", &
                        check_replace_character_in_character) &
                , it( &
                        "If start is less than one, the value one is used for start.", &
                        check_replace_character_in_character_start_lt_one) &
                , it( &
                        "If finish is greater than len(string), the value len(string)" &
                        // " is used for finish.", &
                        check_replace_character_in_character_start_gt_end) &
                , it( &
                        "If finish is less than start, the characters of substring" &
                        // " are inserted before the character at start and no" &
                        // " characters are deleted.", &
                        check_replace_character_in_character_start_gt_finish) &
                ])
    end function

    function test_replace_character_in_string_range() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE in string with character in range", &
                [ it( &
                        "The characters in the copy of string between positions start" &
                        // " and finish, including those at start and finish, are" &
                        // " deleted and replaced by characters of substring.", &
                        check_replace_character_in_string) &
                , it( &
                        "If start is less than one, the value one is used for start.", &
                        check_replace_character_in_string_start_lt_one) &
                , it( &
                        "If finish is greater than len(string), the value len(string)" &
                        // " is used for finish.", &
                        check_replace_character_in_string_start_gt_end) &
                , it( &
                        "If finish is less than start, the characters of substring" &
                        // " are inserted before the character at start and no" &
                        // " characters are deleted.", &
                        check_replace_character_in_string_start_gt_finish) &
                ])
    end function

    function test_replace_string_in_character_range() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE in character with string in range", &
                [ it( &
                        "The characters in the copy of string between positions start" &
                        // " and finish, including those at start and finish, are" &
                        // " deleted and replaced by characters of substring.", &
                        check_replace_string_in_character) &
                , it( &
                        "If start is less than one, the value one is used for start.", &
                        check_replace_string_in_character_start_lt_one) &
                , it( &
                        "If finish is greater than len(string), the value len(string)" &
                        // " is used for finish.", &
                        check_replace_string_in_character_start_gt_end) &
                , it( &
                        "If finish is less than start, the characters of substring" &
                        // " are inserted before the character at start and no" &
                        // " characters are deleted.", &
                        check_replace_string_in_character_start_gt_finish) &
                ])
    end function

    function test_replace_string_in_string_range() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE in string with string in range", &
                [ it( &
                        "The characters in the copy of string between positions start" &
                        // " and finish, including those at start and finish, are" &
                        // " deleted and replaced by characters of substring.", &
                        check_replace_string_in_string) &
                , it( &
                        "If start is less than one, the value one is used for start.", &
                        check_replace_string_in_string_start_lt_one) &
                , it( &
                        "If finish is greater than len(string), the value len(string)" &
                        // " is used for finish.", &
                        check_replace_string_in_string_start_gt_end) &
                , it( &
                        "If finish is less than start, the characters of substring" &
                        // " are inserted before the character at start and no" &
                        // " characters are deleted.", &
                        check_replace_string_in_string_start_gt_finish) &
                ])
    end function

    pure function check_replace_character_in_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS CRAZY", &
                replace("THAT IS CRAZY", 6, 7, "WAS"))
    end function

    pure function check_replace_character_in_character_start_lt_one() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "WAS CRAZY", &
                replace("THAT IS CRAZY", -1, 7, "WAS"))
    end function

    pure function check_replace_character_in_character_start_gt_end() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS", &
                replace("THAT IS CRAZY", 6, 15, "WAS"))
    end function

    pure function check_replace_character_in_character_start_gt_finish() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WASIS CRAZY", &
                replace("THAT IS CRAZY", 6, 1, "WAS"))
    end function

    pure function check_replace_character_in_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS CRAZY", &
                replace(var_str("THAT IS CRAZY"), 6, 7, "WAS"))
    end function

    pure function check_replace_character_in_string_start_lt_one() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "WAS CRAZY", &
                replace(var_str("THAT IS CRAZY"), -1, 7, "WAS"))
    end function

    pure function check_replace_character_in_string_start_gt_end() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS", &
                replace(var_str("THAT IS CRAZY"), 6, 15, "WAS"))
    end function

    pure function check_replace_character_in_string_start_gt_finish() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WASIS CRAZY", &
                replace(var_str("THAT IS CRAZY"), 6, 1, "WAS"))
    end function

    pure function check_replace_string_in_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS CRAZY", &
                replace("THAT IS CRAZY", 6, 7, var_str("WAS")))
    end function

    pure function check_replace_string_in_character_start_lt_one() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "WAS CRAZY", &
                replace("THAT IS CRAZY", -1, 7, var_str("WAS")))
    end function

    pure function check_replace_string_in_character_start_gt_end() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS", &
                replace("THAT IS CRAZY", 6, 15, var_str("WAS")))
    end function

    pure function check_replace_string_in_character_start_gt_finish() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WASIS CRAZY", &
                replace("THAT IS CRAZY", 6, 1, var_str("WAS")))
    end function

    pure function check_replace_string_in_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS CRAZY", &
                replace(var_str("THAT IS CRAZY"), 6, 7, var_str("WAS")))
    end function

    pure function check_replace_string_in_string_start_lt_one() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "WAS CRAZY", &
                replace(var_str("THAT IS CRAZY"), -1, 7, var_str("WAS")))
    end function

    pure function check_replace_string_in_string_start_gt_end() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WAS", &
                replace(var_str("THAT IS CRAZY"), 6, 15, var_str("WAS")))
    end function

    pure function check_replace_string_in_string_start_gt_finish() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "THAT WASIS CRAZY", &
                replace(var_str("THAT IS CRAZY"), 6, 1, var_str("WAS")))
    end function
end module
