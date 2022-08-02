module replace_start_test
    use iso_varying_string, only: replace, var_str
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: &
            test_replace_character_in_character_start, &
            test_replace_character_in_string_start, &
            test_replace_string_in_character_start, &
            test_replace_string_in_string_start
contains
    function test_replace_character_in_character_start() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE in character with character at start", &
                [ it( &
                        "The characters of substring are inserted into a copy of string" &
                        // " at the position start, and the characters in postions from" &
                        // " start to min(start+len(substring)-1, len(string)) are deleted.", &
                        check_replace_character_in_character) &
                , it( &
                        "If start is greater than len(string), the substring is appended to the copy of string.", &
                        check_replace_character_in_character_after) &
                , it( &
                        "If start is less than one, the value one is used for start", &
                        check_replace_character_in_character_before) &
                , it( &
                        "If substring runs off the end, the resulting string is longer?", &
                        check_replace_character_in_character_overrun) &
                ])
    end function

    function test_replace_character_in_string_start() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE in string with character at start", &
                [ it( &
                        "The characters of substring are inserted into a copy of string" &
                        // " at the position start, and the characters in postions from" &
                        // " start to min(start+len(substring)-1, len(string)) are deleted.", &
                        check_replace_character_in_string) &
                , it( &
                        "If start is greater than len(string), the substring is appended to the copy of string.", &
                        check_replace_character_in_string_after) &
                , it( &
                        "If start is less than one, the value one is used for start", &
                        check_replace_character_in_string_before) &
                , it( &
                        "If substring runs off the end, the resulting string is longer?", &
                        check_replace_character_in_string_overrun) &
                ])
    end function

    function test_replace_string_in_character_start() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE in character with string at start", &
                [ it( &
                        "The characters of substring are inserted into a copy of string" &
                        // " at the position start, and the characters in postions from" &
                        // " start to min(start+len(substring)-1, len(string)) are deleted.", &
                        check_replace_string_in_character) &
                , it( &
                        "If start is greater than len(string), the substring is appended to the copy of string.", &
                        check_replace_string_in_character_after) &
                , it( &
                        "If start is less than one, the value one is used for start", &
                        check_replace_string_in_character_before) &
                , it( &
                        "If substring runs off the end, the resulting string is longer?", &
                        check_replace_string_in_character_overrun) &
                ])
    end function

    function test_replace_string_in_string_start() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE in string with string at start", &
                [ it( &
                        "The characters of substring are inserted into a copy of string" &
                        // " at the position start, and the characters in postions from" &
                        // " start to min(start+len(substring)-1, len(string)) are deleted.", &
                        check_replace_string_in_string) &
                , it( &
                        "If start is greater than len(string), the substring is appended to the copy of string.", &
                        check_replace_string_in_string_after) &
                , it( &
                        "If start is less than one, the value one is used for start", &
                        check_replace_string_in_string_before) &
                , it( &
                        "If substring runs off the end, the resulting string is longer?", &
                        check_replace_string_in_string_overrun) &
                ])
    end function

    pure function check_replace_character_in_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("SOMETHRING", replace("SOMESTRING", 5, "TH"))
    end function

    pure function check_replace_character_in_character_after() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("SOMETHINGELSE", replace("SOMETHING", 10, "ELSE"))
    end function

    pure function check_replace_character_in_character_before() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("ELSETHING", replace("SOMETHING", -1, "ELSE"))
    end function

    pure function check_replace_character_in_character_overrun() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("OVERRUN", replace("OVERT", 5, "RUN"))
    end function

    pure function check_replace_character_in_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("SOMETHRING", replace(var_str("SOMESTRING"), 5, "TH"))
    end function

    pure function check_replace_character_in_string_after() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("SOMETHINGELSE", replace(var_str("SOMETHING"), 10, "ELSE"))
    end function

    pure function check_replace_character_in_string_before() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("ELSETHING", replace(var_str("SOMETHING"), -1, "ELSE"))
    end function

    pure function check_replace_character_in_string_overrun() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("OVERRUN", replace(var_str("OVERT"), 5, "RUN"))
    end function

    pure function check_replace_string_in_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("SOMETHRING", replace("SOMESTRING", 5, var_str("TH")))
    end function

    pure function check_replace_string_in_character_after() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("SOMETHINGELSE", replace("SOMETHING", 10, var_str("ELSE")))
    end function

    pure function check_replace_string_in_character_before() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("ELSETHING", replace("SOMETHING", -1, var_str("ELSE")))
    end function

    pure function check_replace_string_in_character_overrun() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("OVERRUN", replace("OVERT", 5, var_str("RUN")))
    end function

    pure function check_replace_string_in_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("SOMETHRING", replace(var_str("SOMESTRING"), 5, var_str("TH")))
    end function

    pure function check_replace_string_in_string_after() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("SOMETHINGELSE", replace(var_str("SOMETHING"), 10, var_str("ELSE")))
    end function

    pure function check_replace_string_in_string_before() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("ELSETHING", replace(var_str("SOMETHING"), -1, var_str("ELSE")))
    end function

    pure function check_replace_string_in_string_overrun() result(result_)
        type(result_t) :: result_

        result_ = assert_equals("OVERRUN", replace(var_str("OVERT"), 5, var_str("RUN")))
    end function
end module
