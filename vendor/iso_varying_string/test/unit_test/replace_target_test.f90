module replace_target_test
    use iso_varying_string, only: replace, var_str
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: &
            test_replace_character_with_character_in_character, &
            test_replace_character_with_character_in_string, &
            test_replace_character_with_string_in_character, &
            test_replace_character_with_string_in_string, &
            test_replace_string_with_character_in_character, &
            test_replace_string_with_character_in_string, &
            test_replace_string_with_string_in_character, &
            test_replace_string_with_string_in_string
contains
    function test_replace_character_with_character_in_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE target character with character in character", &
                [ it( &
                        "The copy of string is searched for occurences of target. If" &
                        // " target is found, it is replaced by substring.", &
                        check_replace_character_with_character_in_character) &
                , it( &
                        "The search is done in the backward direction if the argument" &
                        // " back is present with the value true.", &
                        check_replace_character_with_character_in_character_backward) &
                , it( &
                        "If every is present with the value true, the search and replace" &
                        // " is continued from the character following target in the" &
                        // " search direction specified until all occurrences of" &
                        // " target in the copy string are replaced.", &
                        check_replace_character_with_character_in_character_every) &
                , it( &
                        "Get's the expected result for various examples and edge cases", &
                        check_replace_examples) &
                ])
    end function

    function test_replace_character_with_character_in_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE target character with character in string", &
                [ it( &
                        "The copy of string is searched for occurences of target. If" &
                        // " target is found, it is replaced by substring.", &
                        check_replace_character_with_character_in_string) &
                , it( &
                        "The search is done in the backward direction if the argument" &
                        // " back is present with the value true.", &
                        check_replace_character_with_character_in_string_backward) &
                , it( &
                        "If every is present with the value true, the search and replace" &
                        // " is continued from the character following target in the" &
                        // " search direction specified until all occurrences of" &
                        // " target in the copy string are replaced.", &
                        check_replace_character_with_character_in_string_every) &
                ])
    end function

    function test_replace_character_with_string_in_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE target character with string in character", &
                [ it( &
                        "The copy of string is searched for occurences of target. If" &
                        // " target is found, it is replaced by substring.", &
                        check_replace_character_with_string_in_character) &
                , it( &
                        "The search is done in the backward direction if the argument" &
                        // " back is present with the value true.", &
                        check_replace_character_with_string_in_character_backward) &
                , it( &
                        "If every is present with the value true, the search and replace" &
                        // " is continued from the character following target in the" &
                        // " search direction specified until all occurrences of" &
                        // " target in the copy string are replaced.", &
                        check_replace_character_with_string_in_character_every) &
                ])
    end function

    function test_replace_character_with_string_in_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE target character with string in string", &
                [ it( &
                        "The copy of string is searched for occurences of target. If" &
                        // " target is found, it is replaced by substring.", &
                        check_replace_character_with_string_in_string) &
                , it( &
                        "The search is done in the backward direction if the argument" &
                        // " back is present with the value true.", &
                        check_replace_character_with_string_in_string_backward) &
                , it( &
                        "If every is present with the value true, the search and replace" &
                        // " is continued from the character following target in the" &
                        // " search direction specified until all occurrences of" &
                        // " target in the copy string are replaced.", &
                        check_replace_character_with_string_in_string_every) &
                ])
    end function

    function test_replace_string_with_character_in_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE target string with character in character", &
                [ it( &
                        "The copy of string is searched for occurences of target. If" &
                        // " target is found, it is replaced by substring.", &
                        check_replace_string_with_character_in_character) &
                , it( &
                        "The search is done in the backward direction if the argument" &
                        // " back is present with the value true.", &
                        check_replace_string_with_character_in_character_backward) &
                , it( &
                        "If every is present with the value true, the search and replace" &
                        // " is continued from the character following target in the" &
                        // " search direction specified until all occurrences of" &
                        // " target in the copy string are replaced.", &
                        check_replace_string_with_character_in_character_every) &
                ])
    end function

    function test_replace_string_with_character_in_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE target string with character in string", &
                [ it( &
                        "The copy of string is searched for occurences of target. If" &
                        // " target is found, it is replaced by substring.", &
                        check_replace_string_with_character_in_string) &
                , it( &
                        "The search is done in the backward direction if the argument" &
                        // " back is present with the value true.", &
                        check_replace_string_with_character_in_string_backward) &
                , it( &
                        "If every is present with the value true, the search and replace" &
                        // " is continued from the character following target in the" &
                        // " search direction specified until all occurrences of" &
                        // " target in the copy string are replaced.", &
                        check_replace_string_with_character_in_string_every) &
                ])
    end function

    function test_replace_string_with_string_in_character() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE target string with string in character", &
                [ it( &
                        "The copy of string is searched for occurences of target. If" &
                        // " target is found, it is replaced by substring.", &
                        check_replace_string_with_string_in_character) &
                , it( &
                        "The search is done in the backward direction if the argument" &
                        // " back is present with the value true.", &
                        check_replace_string_with_string_in_character_backward) &
                , it( &
                        "If every is present with the value true, the search and replace" &
                        // " is continued from the character following target in the" &
                        // " search direction specified until all occurrences of" &
                        // " target in the copy string are replaced.", &
                        check_replace_string_with_string_in_character_every) &
                ])
    end function

    function test_replace_string_with_string_in_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "Sec. 3.7.4: REPLACE target string with string in string", &
                [ it( &
                        "The copy of string is searched for occurences of target. If" &
                        // " target is found, it is replaced by substring.", &
                        check_replace_string_with_string_in_string) &
                , it( &
                        "The search is done in the backward direction if the argument" &
                        // " back is present with the value true.", &
                        check_replace_string_with_string_in_string_backward) &
                , it( &
                        "If every is present with the value true, the search and replace" &
                        // " is continued from the character following target in the" &
                        // " search direction specified until all occurrences of" &
                        // " target in the copy string are replaced.", &
                        check_replace_string_with_string_in_string_every) &
                ])
    end function

    pure function check_replace_character_with_character_in_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "with this in this string", &
                replace( &
                        "this in this string", &
                        "this", &
                        "with this"))
    end function

    pure function check_replace_character_with_character_in_character_backward() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "this in with this string", &
                replace(&
                        "this in this string", &
                        "this", &
                        "with this", &
                        back = .TRUE.))
    end function

    pure function check_replace_character_with_character_in_character_every() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        "with this in with this string", &
                        replace(&
                                "this in this string", &
                                "this", &
                                "with this", &
                                every = .TRUE.)) &
                .and.assert_equals( &
                        "with this in with this string", &
                        replace( &
                                "this in this string", &
                                "this", &
                                "with this", &
                                every = .TRUE., &
                                back = .TRUE.))
    end function

    pure function check_replace_examples() result(result_)
        type(result_t) :: result_

        result_ = &
            assert_equals("A.", replace(".A.", ".A", "A", every=.true.))
    end function

    pure function check_replace_character_with_character_in_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "with this in this string", &
                replace( &
                        var_str("this in this string"), &
                        "this", &
                        "with this"))
    end function

    pure function check_replace_character_with_character_in_string_backward() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "this in with this string", &
                replace(&
                        var_str("this in this string"), &
                        "this", &
                        "with this", &
                        back = .TRUE.))
    end function

    pure function check_replace_character_with_character_in_string_every() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        "with this in with this string", &
                        replace(&
                                var_str("this in this string"), &
                                "this", &
                                "with this", &
                                every = .TRUE.)) &
                .and.assert_equals( &
                        "with this in with this string", &
                        replace( &
                                var_str("this in this string"), &
                                "this", &
                                "with this", &
                                every = .TRUE., &
                                back = .TRUE.))
    end function

    pure function check_replace_character_with_string_in_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "with this in this string", &
                replace( &
                        "this in this string", &
                        "this", &
                        var_str("with this")))
    end function

    pure function check_replace_character_with_string_in_character_backward() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "this in with this string", &
                replace(&
                        "this in this string", &
                        "this", &
                        var_str("with this"), &
                        back = .TRUE.))
    end function

    pure function check_replace_character_with_string_in_character_every() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        "with this in with this string", &
                        replace(&
                                "this in this string", &
                                "this", &
                                var_str("with this"), &
                                every = .TRUE.)) &
                .and.assert_equals( &
                        "with this in with this string", &
                        replace( &
                                "this in this string", &
                                "this", &
                                var_str("with this"), &
                                every = .TRUE., &
                                back = .TRUE.))
    end function

    pure function check_replace_character_with_string_in_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "with this in this string", &
                replace( &
                        var_str("this in this string"), &
                        "this", &
                        var_str("with this")))
    end function

    pure function check_replace_character_with_string_in_string_backward() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "this in with this string", &
                replace(&
                        var_str("this in this string"), &
                        "this", &
                        var_str("with this"), &
                        back = .TRUE.))
    end function

    pure function check_replace_character_with_string_in_string_every() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        "with this in with this string", &
                        replace(&
                                var_str("this in this string"), &
                                "this", &
                                var_str("with this"), &
                                every = .TRUE.)) &
                .and.assert_equals( &
                        "with this in with this string", &
                        replace( &
                                var_str("this in this string"), &
                                "this", &
                                var_str("with this"), &
                                every = .TRUE., &
                                back = .TRUE.))
    end function

    pure function check_replace_string_with_character_in_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "with this in this string", &
                replace( &
                        "this in this string", &
                        var_str("this"), &
                        "with this"))
    end function

    pure function check_replace_string_with_character_in_character_backward() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "this in with this string", &
                replace(&
                        "this in this string", &
                        var_str("this"), &
                        "with this", &
                        back = .TRUE.))
    end function

    pure function check_replace_string_with_character_in_character_every() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        "with this in with this string", &
                        replace(&
                                "this in this string", &
                                var_str("this"), &
                                "with this", &
                                every = .TRUE.)) &
                .and.assert_equals( &
                        "with this in with this string", &
                        replace( &
                                "this in this string", &
                                var_str("this"), &
                                "with this", &
                                every = .TRUE., &
                                back = .TRUE.))
    end function

    pure function check_replace_string_with_character_in_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "with this in this string", &
                replace( &
                        var_str("this in this string"), &
                        var_str("this"), &
                        "with this"))
    end function

    pure function check_replace_string_with_character_in_string_backward() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "this in with this string", &
                replace(&
                        var_str("this in this string"), &
                        var_str("this"), &
                        "with this", &
                        back = .TRUE.))
    end function

    pure function check_replace_string_with_character_in_string_every() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        "with this in with this string", &
                        replace(&
                                var_str("this in this string"), &
                                var_str("this"), &
                                "with this", &
                                every = .TRUE.)) &
                .and.assert_equals( &
                        "with this in with this string", &
                        replace( &
                                var_str("this in this string"), &
                                var_str("this"), &
                                "with this", &
                                every = .TRUE., &
                                back = .TRUE.))
    end function

    pure function check_replace_string_with_string_in_character() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "with this in this string", &
                replace( &
                        "this in this string", &
                        var_str("this"), &
                        var_str("with this")))
    end function

    pure function check_replace_string_with_string_in_character_backward() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "this in with this string", &
                replace(&
                        "this in this string", &
                        var_str("this"), &
                        var_str("with this"), &
                        back = .TRUE.))
    end function

    pure function check_replace_string_with_string_in_character_every() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        "with this in with this string", &
                        replace(&
                                "this in this string", &
                                var_str("this"), &
                                var_str("with this"), &
                                every = .TRUE.)) &
                .and.assert_equals( &
                        "with this in with this string", &
                        replace( &
                                "this in this string", &
                                var_str("this"), &
                                var_str("with this"), &
                                every = .TRUE., &
                                back = .TRUE.))
    end function

    pure function check_replace_string_with_string_in_string() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "with this in this string", &
                replace( &
                        var_str("this in this string"), &
                        var_str("this"), &
                        var_str("with this")))
    end function

    pure function check_replace_string_with_string_in_string_backward() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                "this in with this string", &
                replace(&
                        var_str("this in this string"), &
                        var_str("this"), &
                        var_str("with this"), &
                        back = .TRUE.))
    end function

    pure function check_replace_string_with_string_in_string_every() result(result_)
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        "with this in with this string", &
                        replace(&
                                var_str("this in this string"), &
                                var_str("this"), &
                                var_str("with this"), &
                                every = .TRUE.)) &
                .and.assert_equals( &
                        "with this in with this string", &
                        replace( &
                                var_str("this in this string"), &
                                var_str("this"), &
                                var_str("with this"), &
                                every = .TRUE., &
                                back = .TRUE.))
    end function
end module
