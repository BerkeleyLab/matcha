module split_character_set_test
    use iso_varying_string, only: varying_string, assignment(=), split
    use veggies, only: &
            result_t, test_item_t, assert_empty, assert_equals, describe, it

    implicit none
    private

    public :: test_split_character
contains
    function test_split_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.5: SPLIT divides the string at the first occurence of a character that is in set (character)", &
                [ describe( &
                        "The string is searched in the forward direction", &
                        [ describe( &
                                "Without separator argument", &
                                [ it( &
                                        "The characters passed over in the search are returned in the" &
                                        // " argument word, and the remainder of the string, not" &
                                        // " including the sperator character is returned in the argument string", &
                                        check_forward_no_separator) &
                                , it( &
                                        "If no character from set is found, string is returned as zero length", &
                                        check_forward_no_separator_not_found) &
                                , it( &
                                        "If set is of zero length, string is returned as zero length", &
                                        check_forward_no_separator_empty_set) &
                                ]) &
                        , describe( &
                                "If the argument seprator is present, the actual character" &
                                // " found which separates the word from the remainder of the" &
                                // " string is returned in separator", &
                                [ it( &
                                        "The characters passed over in the search are returned in the" &
                                        // " argument word, and the remainder of the string, not" &
                                        // " including the sperator character is returned in the argument string", &
                                        check_forward_with_separator) &
                                , it( &
                                        "If no character from set is found, separator is returned as zero length", &
                                        check_forward_with_separator_not_found) &
                                , it( &
                                        "If set is of zero length, separator is returned as zero length", &
                                        check_forward_with_separator_empty_set) &
                                ]) &
                        ]) &
                , describe( &
                        "The string is searched in the forward direction if back is false", &
                        [ describe( &
                                "Without separator argument", &
                                [ it( &
                                        "The characters passed over in the search are returned in the" &
                                        // " argument word, and the remainder of the string, not" &
                                        // " including the sperator character is returned in the argument string", &
                                        check_not_backward_no_separator) &
                                , it( &
                                        "If no character from set is found, string is returned as zero length", &
                                        check_not_backward_no_separator_not_found) &
                                , it( &
                                        "If set is of zero length, string is returned as zero length", &
                                        check_not_backward_no_separator_empty_set) &
                                ]) &
                        , describe( &
                                "If the argument seprator is present, the actual character" &
                                // " found which separates the word from the remainder of the" &
                                // " string is returned in separator", &
                                [ it( &
                                        "The characters passed over in the search are returned in the" &
                                        // " argument word, and the remainder of the string, not" &
                                        // " including the sperator character is returned in the argument string", &
                                        check_not_backward_with_separator) &
                                , it( &
                                        "If no character from set is found, separator is returned as zero length", &
                                        check_not_backward_with_separator_not_found) &
                                , it( &
                                        "If set is of zero length, separator is returned as zero length", &
                                        check_not_backward_with_separator_empty_set) &
                                ]) &
                        ]) &
                , describe( &
                        "The string is searched in the backward direction if back is true", &
                        [ describe( &
                                "Without separator argument", &
                                [ it( &
                                        "The characters passed over in the search are returned in the" &
                                        // " argument word, and the remainder of the string, not" &
                                        // " including the sperator character is returned in the argument string", &
                                        check_backward_no_separator) &
                                , it( &
                                        "If no character from set is found, string is returned as zero length", &
                                        check_backward_no_separator_not_found) &
                                , it( &
                                        "If set is of zero length, string is returned as zero length", &
                                        check_backward_no_separator_empty_set) &
                                ]) &
                        , describe( &
                                "If the argument seprator is present, the actual character" &
                                // " found which separates the word from the remainder of the" &
                                // " string is returned in separator", &
                                [ it( &
                                        "The characters passed over in the search are returned in the" &
                                        // " argument word, and the remainder of the string, not" &
                                        // " including the sperator character is returned in the argument string", &
                                        check_backward_with_separator) &
                                , it( &
                                        "If no character from set is found, separator is returned as zero length", &
                                        check_backward_with_separator_not_found) &
                                , it( &
                                        "If set is of zero length, separator is returned as zero length", &
                                        check_backward_with_separator_empty_set) &
                                ]) &
                        ]) &
                ])
    end function

    pure function check_forward_no_separator() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, " ,")
        result_ = &
                assert_equals("split", word) &
                .and.assert_equals(" this", string)
    end function

    pure function check_forward_no_separator_not_found() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "!")
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_forward_no_separator_empty_set() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "")
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_forward_with_separator() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, " ,", separator)
        result_ = &
                assert_equals("split", word) &
                .and.assert_equals(" this", string) &
                .and.assert_equals(",", separator)
    end function

    pure function check_forward_with_separator_not_found() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "!", separator)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_forward_with_separator_empty_set() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "", separator)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_not_backward_no_separator() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, " ,", back=.false.)
        result_ = &
                assert_equals("split", word) &
                .and.assert_equals(" this", string)
    end function

    pure function check_not_backward_no_separator_not_found() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "!", back=.false.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_not_backward_no_separator_empty_set() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "", back=.false.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_not_backward_with_separator() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, " ,", separator, .false.)
        result_ = &
                assert_equals("split", word) &
                .and.assert_equals(" this", string) &
                .and.assert_equals(",", separator)
    end function

    pure function check_not_backward_with_separator_not_found() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "!", separator, .false.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_not_backward_with_separator_empty_set() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "", separator, .false.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_backward_no_separator() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, " ,", back=.true.)
        result_ = &
                assert_equals("this", word) &
                .and.assert_equals("split,", string)
    end function

    pure function check_backward_no_separator_not_found() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "!", back=.true.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_backward_no_separator_empty_set() result(result_)
        type(result_t) :: result_

        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "", back=.true.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word)
    end function

    pure function check_backward_with_separator() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, " ,", separator, .true.)
        result_ = &
                assert_equals("this", word) &
                .and.assert_equals("split,", string) &
                .and.assert_equals(" ", separator)
    end function

    pure function check_backward_with_separator_not_found() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "!", separator, .true.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function

    pure function check_backward_with_separator_empty_set() result(result_)
        type(result_t) :: result_

        type(varying_string) :: separator
        type(varying_string) :: string
        type(varying_string) :: word

        string = "split, this"
        call split(string, word, "", separator, .true.)
        result_ = &
                assert_empty(string) &
                .and.assert_equals("split, this", word) &
                .and.assert_empty(separator)
    end function
end module
