Garden
==========

[![pipeline status](https://gitlab.com/everythingfunctional/garden/badges/main/pipeline.svg)](https://gitlab.com/everythingfunctional/garden/-/commits/main)

For a healthier code base, eat your vegetables.

Garden is a Fortran unit testing framework written using functional programming principles,
with the ability to test parallel code.
As many of its procedures as possible are marked with the pure keyword,
while still allowing the framework to test impure code.
It's biggest features are that it provides a readable test/code specification as it's output by default,
and provides the option to output even the passing test results.
It makes writing tests in a Specification or [BDD] style as easy as possible,
and has all the features one would expect from a modern testing framework.
It is also flexible and extensible.

[BDD]: https://en.wikipedia.org/wiki/Behavior-driven_development

## Environment

In order to run this you will need:
* A reasonably recent version of gfortran, ifort or nagfor
* The Fortran Package Manager (fpm)

Running the tests is then as simple as

```
git clone https://gitlab.com/everythingfunctional/garden.git
cd garden
fpm test
```

Using Garden
----------------

Using Garden is (almost) as easy as writing a specification.
A great first example are the tests for Garden themselves.
The following overview should be sufficient to get you started,
but you can take a deeper look into how Garden works by referencing
[the developer documentation](https://everythingfunctional.gitlab.io/garden).

### Writing a Test Function

The simplest test function is one that takes no arguments and produces a `result_t` value as its output.
The function should execute some portion of your code, and make some assertion(s) about the result(s).
Multiple assertions can be combined by using the `.and.` operator.
Also, the `succeed` and `fail` functions are provided if for some reason the provided assertions aren't quite sufficient.

The simplest example of a test function would be something like the following:

```Fortran
function simplest()
    use garden, only: result_t, succeed

    type(result_t) :: simplest

    simplest = succeed("Successfully")
end function
```

Additionally, a test function can take a `class(input_t)` value as an input.
Some simple types are provided that extend from `input_t`,
such as `double_precision_input_t`, `integer_input_t` and `string_input_t`.
If you need other inputs to your test, you can create a new type extended from `input_t` to provide whatever you need.
A `select case` construct is then needed to make sure the right type gets passed in at run time.
An example of a test that takes an input is shown below.
The actual inputs passed to the test are determined by how it is included into the test suite.
Assembling the test suite is described below.

```Fortran
function input_test(input) result(result_)
    use garden, only: input_t, integer_input_t, result_t, assert_equals, fail

    class(input_t), intent(in) :: input
    type(result_t) :: result_

    select type (input)
    type is (integer_input_t)
        result_ = assert_equals(1, input%value_)
    class default
        result_ = fail("Didn't get an integer_input_t")
    end select
end function
```

### Assertions

An assertion is simply something to make sure that your code is behaving as expected.
There are multiple assert functions provided to check a variety of types of values.
They all return a value of type `result_t` that can be returned by your test function.
Additionally, `result_t` values can be combined using the `.and.` operator
to allow you to make multiple assertions within your test and return the results of all of them.
Additionally, the `succeed` and `fail` functions are provided for situations where the provided assertions aren't quite appropriate.

By convention, there are two final arguments to the assert functions which are optional that represent an additional *user* message to be included.
If both arguments are included then the first is used in the case the assertion succeeds,
and the second is used in the case the assertion fails.
If only one is provided, then it is used whether the assertion passes or fails.
Any argument of type `character` can also accept a value of type `varying_string`
from the `iso_varying_string` module.
The provided assertions are listed below:

* `assert_that` and `assert_not` accept a logical value and make sure it is either `.true.` or `.false.` respectively
* `assert_equals` accepts two values of integer, double precision, character, or `varying_string`, and ensures they are equal
  (Note that `assert_equals` with double precision values simply uses the `assert_equals_within_absolute` function with a tolerance of machine epsilon)
* `assert_equals_within_absolute` and `assert_equals_within_relative` accept two values of double precision
  and a third value of double precision to use as the tolerance, and ensures the values are within that tolerance.
* `assert_empty` ensures that the given string is of zero length
* `assert_includes` and `assert_doesnt_include` ensure that the second string includes (or doesn't include) the first string
* `assert_faster_than` has several variations.
  It accepts a subroutine with no arguments, and runs it the specified number of times to measure how long it takes to run.
  It then compares that to either a given number in seconds, or from running another provided subroutine and measuring it as well.
  Optionally, additional subroutines can be provided to be executed before and after the other subroutine(s) to function as setup and tear-down,
  to avoid including that code in the measurement.

#### Writing Your Own Assertions

All of the code used to create the messages from the assertions is publicly callable.
The basic pattern used is:

```Fortran
if (some_condition) then
    result_ = succeed( &
            with_user_message( &
                    make_some_success_message(args), &
                    success_message))
else
    result_ = fail( &
            with_user_message( &
                    make_some_failure_message(args), &
                    failure_message))
end if
```

So if the provided assert functions don't quite do what you need, or don't accept the type you need,
it should be pretty easy to write your own,
and have the style of the message it produces match with the rest of them.
The [Quantities for Fortran](https://gitlab.com/everythingfunctional/quaff) is a great example of where I've done just that.

### Assemble The Suite

Once you've written your test function, you'll need to include it into your test suite.
I've [published a tool](https://gitlab.com/everythingfunctional/cart)
that can be used to do it, but you can also do it manually.

First, you'll need to write a function that defines a part of your test suite,
either spec or BDD style, using the provided functions `describe` and `it`/`it_`
or `given`, `when` and `then_`/`then__`.
If you're using the provided tool, then this function should be in a module named `something`**`_test`**,
in a file with the same name.
The function should be named **`test_`**`something`,
and must take no arguments, and return a value of type `test_item_t`, which the above functions do.

The `given`, `when` and `describe` functions take a description string, and an array of `test_item_t`s.
The `it` and `then_` functions accept a string description,
and a function that takes no arguments and returns a `result_t`.
The `it_` and `then__` functions accept a function that takes one argument of `class(input_t)`.
These are the descriptions that are given in the output of Garden for each test.

The `given`, `when` and `describe` functions can also accept a `class(input_t)` value,
to be passed to each of the tests they contain.
Additionally, the `when` function can accept a function from `class(input_t)` to `type(transformed_t)`
(which is just a wrapper for a `class(input_t)` value)
which will be called and then pass its result down to the tests.

The `it` and `then_` functions can also take an array of `example_t`s
(which is just a wrapper for a `class(input_t)`)
which will each be passed to the test function.
They can instead accept a value of `class(generator_t)`,
which will be used to generate random values to be passed to the test.
More information is provided for `class(generator_t)` below.

An example of a specification function would be as follows:

```Fortran
function test_assert_empty() result(tests)
    type(test_item_t) :: tests

    tests = describe( &
            "assert_empty", &
            [ it( &
                    "passes with an empty string", &
                    check_pass_for_empty_chars) &
            , it( &
                    "fails with a non empty string", &
                    check_fails_for_nonempty_chars) &
            ])
end function
```

The basic gist of this is that we are describing the requirements for the `assert_empty` function.
The two requirements are that:

1. It passes with an empty string
2. It fails with a non-empty string

If you are using the provided tool,
multiple **`test_`**`something` functions can be provided within a module,
and multiple `something`**`_test`** modules can be provided in separate files.
The tool will generate a driver program that calls each **`test_`**`something` function it finds
in order to build up the test suite.
It will then run all of the tests and report the results.
The tool accepts as command line arguments the name of the generator program
and the list of files containing the tests.

The generated driver program uses the function `test_that`
to combine all of the tests provided by the **`test_`**`something` functions into a single collection,
and then calls the subroutine `run_tests` with that collection.
So, even manually writing and maintaining the driver program wouldn't be _too_ bad.

### Generating Random Inputs

By providing a type extended from `class(generator_t)`,
you can test fundamental properties of your code that should hold for all values.
A couple of generators are provided for simple types,
but generally you'll want to provide your own.
To do so you'll need to override the `generate` function.
This function takes your generator type as an input, and must produce a value of `type(generated_t)`.
This is just a wrapper around a `class(input_t)`.
Several `get_random*` functions are provided to generate most primitive types with various ranges and/or lengths.

Additionally, you must override the `shrink` function.
This must take a `class(input_t)` value as input, and provide a `type(shrink_result_t)` as output.
This is just a wrapper around a `class(input_t)`, with a flag for whether it is the simplest possible value.
The relevant code for one of the provided generators is shown below.

```Fortran
module garden_integer_generator_m
    use garden_generator_m, only: generator_t

    implicit none
    private
    public :: INTEGER_GENERATOR

    type, extends(generator_t) :: integer_generator_t
    contains
        private
        procedure, public :: generate
        procedure, nopass, public :: shrink
    end type

    type(integer_generator_t), parameter :: &
            INTEGER_GENERATOR = integer_generator_t()
contains
    function generate(self) result(generated_value)
        use garden_generated_m, only: generated_t
        use garden_integer_input_m, only: integer_input_t
        use garden_random_m, only: get_random_integer

        class(integer_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        associate(unused => self)
        end associate

        generated_value = generated_t(integer_input_t(get_random_integer()))
    end function

    function shrink(input) result(shrunk)
        use garden_input_m, only: input_t
        use garden_integer_input_m, only: integer_input_t
        use garden_shrink_result_m, only: &
                shrink_result_t, shrunk_value, simplest_value

        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        select type (input)
        type is (integer_input_t)
            associate(input_val => input%input())
                if (input_val == 0) then
                    shrunk = simplest_value(integer_input_t(0))
                else
                    shrunk = shrunk_value(integer_input_t(input_val / 2))
                end if
            end associate
        end select
    end function
end module
```

When given a generator, the `generate` function will be called to generate up to
the number random values given on the command line (the default is 100).
If the test never fails, it passes.
If the test fails on one of the inputs, then it will successively call the `shrink` function with that input
until either the test passes again, or it gets to the simplest possible input.
The last failing result is then reported.

### The Command Line

The driver program accepts a handful of command line arguments for controlling how the tests are run.
This is done inside the `run_tests` subroutine, so even manually or otherwise generated driver programs can use this functionality.

```
Usage: driver_name [-h] [-q] [-v] [-d] [-f string] [-n num] [-s num] [-c]
  options:
    -h, --help                    Output this message and exit
    -q, --quiet                   Don't print the test descriptions before
                                  running the tests
    -v, --verbose                 Print all of the assertion messages, not
                                  just the failing ones
    -d, --debug                   Report the beginning and end of execution
                                  of each test case or suite
    -f string, --filter string    Only run cases or collections whose
                                  description contains the given string
    -n num, --numrand num         Number of random values to use for each
                                  test with generated values (default = 100)
    -s num, --shrink-max num      Number of attempts to find a simpler value
                                  if a random value fails (default = 100)
    -c, --color-off               Don't colorize the output
```

A common usage is to use the `-q`, `-v` and `-f` flags to run the tests you're working on
and make sure they're doing what you expect.
