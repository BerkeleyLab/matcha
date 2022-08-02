---
title: Organized Listing
---

[TOC]

## Constructing a Test Suite

* [describe] -
  Used to collect together a set of test cases concerning a particular feature of your project.
  Generally used in conjunction with `it` and `it_`.
* [it] and [it_] -
  Used to construct a test case, consisting of a description, the function that performs the test, and optionally examples or a generator.
* [given] -
  Used to collect together of set of test cases starting from the same state.
  Generally used in conjunction with `when` and `then_` or `then__`.
* [when] -
  Used to collect together a set of tests that execute the same action or transformation.
  It optionally accepts a function that performs that action or transformation on some input provided to its enclosing collection.
  Generally used in conjunction with `given` and `then_` or `then__`.
* [then_] and [then__] -
  Used to construct a test case, consisting of a description, the function that performs the test, and optionally examples or a generator.

Note that `given`, `when` and `then_`/`then__` prepend "Given", "When" or "Then" to the provided description
so that the description for the test suite can more closely resemble the code that constructed it.

[describe]: ../interface/describe.html
[it]: ../interface/it.html
[it_]: ../interface/it_.html
[given]: ../interface/given.html
[when]: ../interface/when.html
[then_]: ../interface/then_.html
[then__]: ../interface/then__.html

## Interfaces to a Test Case Function

When implementing the function for a test case, there are two possible interfaces that it must conform to.

* [simple_test_i] that does not accept input
* [input_test_i] that does accept input

[simple_test_i]: ../interface/simple_test_i.html
[input_test_i]: ../interface/input_test_i.html

## Determining if a Test Case Passes or Fails

The results of a test case are returned as a type [result_t].
There are a large number of functions provided to conveniently construct such a value, determining whether the test case should pass or fail.
In addition, multiple `result_t`s can be combined using the [.and. operator].

* [fail] - This test fails with the provided message
* [succeed] - This test succeeds with the provided message
* [assert_doesnt_include] - Ensure that the second argument does not contain the first
* [assert_empty] - Ensure that the argument is empty
* [assert_equals] - Ensure that the second argument equals the first
* [assert_equals_within_absolute] - Ensure that the difference between the first two arguments is less than the provided tolerance
* [assert_equals_within_relative] - Ensure that the difference between the first two arguments is less than the first argument times the provided tolerance
* [assert_faster_than] - Ensure that the provided procedure completes in less time than either the previously provided procedure, or the given number of seconds
* [assert_includes] - Ensure that the second argument contains the first
* [assert_not] - Ensure that the provided argument is not true (i.e. is `.false.`)
* [assert_that] - Ensure that the provided argument is `.true.`

Note that all of the `assert` functions accept a final two optional string arguments
intended to provide additional information to readers of the output from the test suite.
If both message arguments are provided, the first is used in the case the `assert` passes, and the second is used in the case the `assert` fails.
If only one message is provided, then it is used in either case.
Also, all string arguments can be passed as either `character(len=*)`, or `type(varying_string)`.

[result_t]: ../type/result_t.html
[.and. operator]: ../type/result_t.html#boundprocedure-combine_results
[fail]: ../interface/fail.html
[succeed]: ../interface/succeed.html
[assert_doesnt_include]: ../interface/assert_doesnt_include.html
[assert_empty]: ../interface/assert_empty.html
[assert_equals]: ../interface/assert_equals.html
[assert_equals_within_absolute]: ../interface/assert_equals_within_absolute.html
[assert_equals_within_relative]: ../interface/assert_equals_within_relative.html
[assert_faster_than]: ../interface/assert_faster_than.html
[assert_includes]: ../interface/assert_includes.html
[assert_not]: ../interface/assert_not.html
[assert_that]: ../interface/assert_that.html

## A Test That Accepts Input

A test that accepts an input, does so as a `class(input_t), intent(in)` argument.
Thus the function cannot modify its input.
Additionally, the type [input_t] is abstract, with no components or type-bound procedures.
Thus the only thing the test can do with it, is use `select type` to determine the actual type of the input.
In practice, the author of the test will know what type of input to expect, but to be safe the typical pattern should be:

```Fortran
select type (input)
type is (my_expected_input_t)
  ! Now perform my test logic
class default
  result_ = fail("Didn't get the type of input I expected")
end select
```

A few simple input types are provided, as listed below.
Typically, the user should implement their own type extended from [input_t]
in order to provide inputs using their own types or to provide multiple inputs to their test cases.

* [double_precision_input_t] - provides a single floating point number
* [integer_input_t] - provides a single integer
* [string_input_t] - provides a single string

[input_t]: ../type/input_t.html
[double_precision_input_t]: ../type/double_precision_input_t.html
[integer_input_t]: ../type/integer_input_t.html
[string_input_t]: ../type/string_input_t.html

## Providing Inputs to Your Tests

There are a few ways to provide inputs to test cases, depending on where in the test suite they are provided.

### Examples

The type [example_t] type is provided as a simple wrapper of `class(input_t)`
so that an array of "exemplar" inputs can be provided.
Typical usage is to provide list of examples for a specific test case like:

```Fortran
it( &
    "behaves this way for that kind of input", &
    [ example_t(my_input_t(1, "one")) &
    , example_t(my_input_t(2, "two")) &
    , example_t(my_input_t(3, "three")) &
    ], &
    check_that_behavior)
```

[example_t]: ../type/example_t.html

### A Beginning State

A test collection can be provided with a single `input_t` which will be passed to all of the test cases in contains.
This is typically used for something like a `given`, `when`, `then` organization
so that it can be processed through different transformations, as discussed in the following section.
A typical usage might look like the:

```Fortran
tests = given( &
        "some starting condition", &
        my_starting_input_t(the_starting_point), &
        [when(...), when(...)])
```

### A Transformational Process

It is possible to place a transformational procedure in the middle of passing input to a test case.
Such a procedure must conform to the interface [transformer_i].
It accepts as input and `class(input_t), intent(in)` argument, just like a test case,
but must produce as output a value of `type(transformed_t)`.
[transformed_t] is another simple wrapper around a `class(input_t)`,
so a [transformer_i] function is really just transforming one kind of input into another.
However, there is a possibility that something could go wrong in the transformation.
For this reason a special type is provided that extends from `input_t`, [transformation_failure_t].
This type contains a [result_t] and signals to garden that
this result should be used instead of attempting to run any following test cases in the sequence.

[transformer_i]: ../interface/transformer_i.html
[transformed_t]: ../type/transformed_t.html

### Generating Random Inputs

An advanced technique in testing can be to write tests (or properties)
that hold true for an entire set of inputs.
One can then generate random inputs from this set and check that the test always passes.
In garden this can be done by providing a [generator_t];
extend that type and provide an object of the extended type to the framework.
A [generator_t] must provide two functions.

The first is `generate`, which returns a [generated_t], another simple wrapper of a `class(input_t)`.
`generate` will be called multiple times, and should produce a different value every time.
There are several functions provided for generating random values of intrinsic types to make writing a `generate` function easier.
Note that the distribution of random numbers generated is determined by
whatever the compiler you are using happens to use for its random number generator.
If that distribution matters to you, you may want to provide your own and not use the functions provided below.

* [get_random_ascii_character] - get a single, random, ascii character (i.e. `character(len=1)`)
* [get_random_ascii_string] - get a string of random, ascii characters as a `varying_string`
* [get_random_ascii_string_with_max_length] - like the previous, but with a specified maximum length
* [get_random_double_precision_with_magnitude] - get a random floating point number whose magnitude will be less than that provided
* [get_random_double_precision_with_range] - get a random floating point number within the specified range
* [get_random_integer] - get a random integer
* [get_random_integer_with_range] - get a random integer within the specified range
* [get_random_logical] - randomly choose `.true.` or `.false.`

In addition, a [generator_t] must provide a `shrink` function.
`shrink` must accept a value of `class(input_t)` (it will be one of the values produced by the generator), and return a value of type [shrink_result_t].
A [shrink_result_t] is another simple wrapper of a `class(input_t)`,
but records whether it was constructed via [shrunk_value] or [simplest_value].
The idea is that if a test fails for some value created by the generator,
it will pass that and successive values to the `shrink` function until either
the test passes again, or `shrink` constructs its result with `simplest_value`.
Thus the results of the test can be reported for the simplest value that causes a failure.
Note that there is a limit on the number of times `shrink` will be called to prevent bugs in the generator from causing an infinite loop.

[generator_t]: ../type/generator_t.html
[generated_t]: ../type/generated_t.html
[shrink_result_t]: ../type/shrink_result_t.html
[shrunk_value]: ../proc/shrunk_value.html
[simplest_value]: ../proc/simplest_value.html
[get_random_ascii_character]: ../proc/get_random_ascii_character.html
[get_random_ascii_string]: ../proc/get_random_ascii_string.html
[get_random_ascii_string_with_max_length]: ../proc/get_random_ascii_string_with_max_length.html
[get_random_double_precision_with_magnitude]: ../proc/get_random_double_precision_with_magnitude.html
[get_random_double_precision_with_range]: ../proc/get_random_double_precision_with_range.html
[get_random_integer]: ../proc/get_random_integer.html
[get_random_integer_with_range]: ../proc/get_random_integer_with_range.html
[get_random_logical]: ../proc/get_random_logical.html

## Providing Your Own Assertions and Messages

It's entirely possible that the provided `assert`ions won't be sufficient for all your testing needs.
For example, maybe you'd like to compare objects of the types you use in your application.
But it's also entirely possible to write your own `assert`ions.
The typical pattern is to provide additional functions in one of the existing generic interfaces,
(i.e. `assert_equals`), that accepts your own types, performs some simple check to determine pass or fail,
constructs a useful message, and calls [succeed] or [fail] to construct the [result_t].
In fact, that's exactly how the provided `assert`s are written.
The provided `assert`s also have a consistent formatting for their messages.
The functions used to apply this formatting are made available
to make it easier for your custom `assert`s to conform to this style.

* [delimit] - put "|" on either end of a string (i.e. "|string|")
* [make_doesnt_include_failure_message]
* [make_doesnt_include_success_message]
* [make_empty_failure_message]
* [make_equals_failure_message]
* [make_equals_success_message]
* [make_faster_than_failure_message]
* [make_faster_than_success_message]
* [make_includes_failure_message]
* [make_includes_success_message]
* [make_within_failure_message]
* [make_within_success_message]
* [with_user_message] - append the user provided message to the end, doing nothing if the user message was empty
* [EMPTY_SUCCESS_MESSAGE]
* [NOT_FAILURE_MESSAGE]
* [NOT_SUCCESS_MESSAGE]
* [THAT_FAILURE_MESSAGE]
* [THAT_SUCCESS_MESSAGE]

[delimit]: ../interface/delimit.html
[make_doesnt_include_failure_message]: ../interface/make_doesnt_include_failure_message.html
[make_doesnt_include_success_message]: ../interface/make_doesnt_include_success_message.html
[make_empty_failure_message]: ../interface/make_empty_failure_message.html
[make_equals_failure_message]: ../interface/make_equals_failure_message.html
[make_equals_success_message]: ../interface/make_equals_success_message.html
[make_faster_than_failure_message]: ../interface/make_faster_than_failure_message.html
[make_faster_than_success_message]: ../interface/make_faster_than_success_message.html
[make_includes_failure_message]: ../interface/make_includes_failure_message.html
[make_includes_success_message]: ../interface/make_includes_success_message.html
[make_within_failure_message]: ../interface/make_within_failure_message.html
[make_within_success_message]: ../interface/make_within_success_message.html
[with_user_message]: ../interface/with_user_message.html
[EMPTY_SUCCESS_MESSAGE]: ../module/garden_messages_m.html#variable-empty_success_message
[NOT_FAILURE_MESSAGE]: ../module/garden_messages_m.html#variable-not_failure_message
[NOT_SUCCESS_MESSAGE]: ../module/garden_messages_m.html#variable-not_success_message
[THAT_FAILURE_MESSAGE]: ../module/garden_messages_m.html#variable-that_failure_message
[THAT_SUCCESS_MESSAGE]: ../module/garden_messages_m.html#variable-that_success_message
