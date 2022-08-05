---
title: Tutorial
---

[TOC]

# Prerequisites

This tutorial assumes that you have working knowledge of, and are comfortable using the following tools:

* The [Fortran Language](https://fortran-lang.org/)
* [A command line interface](https://en.wikipedia.org/wiki/Command-line_interface)
* [git](https://git-scm.com/)
* The [Fortran Package Manager](https://github.com/fortran-lang/fpm)

In addition, to understand some of the motivations behind this library,
and how to use it effectively, you will want to understand:

* [What unit testing is](https://everythingfunctional.wordpress.com/2021/09/09/a-what-test/)
* [How to write good unit tests](https://www.youtube.com/watch?v=tWn8RA_DEic)

# Getting Started With Garden

If this is your first encounter with a unit testing framework,
or even just your first encounter with garden,
I highly recommend cloning the garden repository and running its test suite.
You'll need to have the following installed to do that.

* git
* A modern Fortran compiler; newer versions of gfortran and nagfor are known to work
* The [Fortran Package Manager (fpm)](https://github.com/fortran-lang/fpm)

With those installed and configured, you should be able to open a terminal and issue the following commands.

``` { use_pygments=false }
git clone https://gitlab.com/everythingfunctional/garden.git
cd garden
fpm test
```

You should see output looking like the following.

``` { use_pygments=false }
Running Tests

Test that
    assert_doesnt_include
        passes with different strings
        fails with the same string
...
A total of 78 test cases

All Passed
Took 3.17154 seconds

A total of 78 test cases containing a total of 205 assertions
```

Congratulations, you've run your first suite of tests using the garden framework.

## Options for Running Test Suites

Some command line options are accepted by the default test suite runner.
To see the options available, pass the `-h` option to the runner, which can be done using fpm like

``` { use_pygments=false }
fpm test -- -h
```

You should see output like the following:

``` { use_pygments=false }
Usage: build/gfortran_2A42023B310FA28D/test/garden-test [-h] [-q] [-v] [-d] [-f string] [-n num] [-s num] [-c]
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

The `-q`, `-v`, and `-d` options affect the verbosity of the output.
By default, garden will report the tests that will be run, before starting to run them.
The `-q` option suppresses this initial output.
By default, garden will only report assertions that fail.
The `-v` option will cause the passing assertions to be reported as well.
By default, garden does not produce any output during the execution of the tests.
If a test crashes, this can make it difficult to isolate the cause of the problem.
The `-d` option will cause the beginning and completion of execution of each test.

The `-f` options impacts what tests are run.
By passing this option, garden will filter the test suite before execution,
thus only executing tests that match the given string.

The `-n` and `-s` affect the behavior of generator tests (a more advanced concept that we'll get to later).
The `-n` option will set how many randomly generated values will be provided to a test.
The `-s` option will set how many attempts will be made to find the simplest possible input causing a failure, in the case that an input causing failure is found.

The `-c` option impacts the output color.
By default, garden attempts to color the messages from passing assertions green, and from failing assertions red.
It uses terminal escape sequences to do so, but under some terminals or environments these may not be interpreted correctly, or at all, leading to some visual clutter.

# Writing Your First Test

The examples for this tutorial are stored in a public repository available [here](https://gitlab.com/everythingfunctional/garden_tutorial).
I'll link to tags in that repository for key milestones.

To get started, we're going to use fpm to build and run our tests.
In a terminal, move to a place you want to keep your project, and issue the command:

``` { use_pygments=false }
fpm new --lib --test garden_tutorial
```

This will generate a new project for us, with a template test and library.
Move into the newly created folder, and run the command `fpm test`, and you should see some output like the following:

``` { use_pygments=false }
$ fpm test
 + mkdir -p build/dependencies
 + mkdir -p build/gfortran_2A42023B310FA28D/garden_tutorial
 + gfortran -c test/check.f90 -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds -fcheck=array-temps -fbacktrace -fcoarray=single -J build/gfortran_2A42023B310FA28D/garden_tutorial -I build/gfortran_2A42023B310FA28D/garden_tutorial  -o build/gfortran_2A42023B310FA28D/garden_tutorial/test_check.f90.o
 + gfortran -c ././src/garden_tutorial.f90 -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds -fcheck=array-temps -fbacktrace -fcoarray=single -J build/gfortran_2A42023B310FA28D/garden_tutorial -I build/gfortran_2A42023B310FA28D/garden_tutorial  -o build/gfortran_2A42023B310FA28D/garden_tutorial/src_garden_tutorial.f90.o
 + ar -rs build/gfortran_2A42023B310FA28D/garden_tutorial/libgarden_tutorial.a build/gfortran_2A42023B310FA28D/garden_tutorial/src_garden_tutorial.f90.o
ar: creating build/gfortran_2A42023B310FA28D/garden_tutorial/libgarden_tutorial.a
 + mkdir -p build/gfortran_2A42023B310FA28D/test/
 + gfortran  -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds -fcheck=array-temps -fbacktrace -fcoarray=single -J build/gfortran_2A42023B310FA28D/garden_tutorial -I build/gfortran_2A42023B310FA28D/garden_tutorial  build/gfortran_2A42023B310FA28D/garden_tutorial/test_check.f90.o build/gfortran_2A42023B310FA28D/garden_tutorial/libgarden_tutorial.a -o build/gfortran_2A42023B310FA28D/test/check
 Put some tests in here!
```

You can find the code at this stage [here](https://gitlab.com/everythingfunctional/garden_tutorial/-/tree/starting_point).

At this point, our code compiles and our tests run,
but the code doesn't do anything interesting, our tests aren't actually testing anything, and we aren't using garden yet.
Let's start by getting garden.
In the `fpm.toml` file, add the following section:

```toml
[dev-dependencies]
garden = { git = "https://gitlab.com/everythingfunctional/garden.git", tag = "v1.0.0" }
```

Now fpm will fetch and compile garden into our test suite.
Of course, we're still not actually using garden,
so running `fpm test` after this step will still produce the same message.

In order to write a test, we're going to need something to test.
To start with, we'll use a relatively simple function as an example.

```Fortran
module is_leap_year_m
    implicit none
    private
    public :: is_leap_year
contains
    pure function is_leap_year(year)
        integer, intent(in) :: year
        logical :: is_leap_year

        if (mod(year, 4) == 0) then
            if (mod(year, 100) == 0) then
                if (mod(year, 400) == 0) then
                    is_leap_year = .true.
                else
                    is_leap_year = .false.
                end if
            else
                is_leap_year = .true.
            end if
        else
            is_leap_year = .false.
        end if
    end function
end module
```

Save this in a file `is_leap_year_m.f90` in the `src` folder,
and delete the file put there when we created the project.
Next, we need a file to hold our test.
In the `test` folder, create a file named `is_leap_year_test.f90`,
and start by creating a module in it with the same name.

```Fortran
module is_leap_year_test
    implicit none
end module
```

The first thing we need to do when creating a test is write the specification about what it's going to test.
We do this in a function in this module, like so.

```Fortran
module is_leap_year_test
    use garden, only: test_item_t

    implicit none
    private
    public :: test_is_leap_year
contains
    function test_is_leap_year() result(tests)
        type(test_item_t) :: tests
    end function
end module
```

This is the only thing that need be public from this module,
and the function will return this section of our test suite as a `test_item_t`.
To start defining this section of our test suite,
we will use the `describe` and `it` functions, like so.

```Fortran
module is_leap_year_test
    use garden, only: test_item_t, describe, it

    implicit none
    private
    public :: test_is_leap_year
contains
    function test_is_leap_year() result(tests)
        type(test_item_t) :: tests

        tests = describe(&
                "is_leap_year", &
                [ it( &
                        "returns false for years that are not divisible by 4", &
                        check_not_divisible_by_4) &
                ])
    end function
end module
```

The `it` function takes a description, and a function to call to perform the test, and returns a `test_item_t`.
The `describe` function takes a description, and an array of `test_item_t`s, and returns a `test_item_t`.
This facilitates, and even encourages, documenting the expected behavior of the code under test.

Note that we needed to provide a function to the `it` function,
so now we must write the actual test.
A test is a function that returns a test `result_t`, like so.

```Fortran
module is_leap_year_test
    use is_leap_year_m, only: is_leap_year
    use garden, only: result_t, test_item_t, assert_not, describe, it

    implicit none
    private
    public :: test_is_leap_year
contains
    function test_is_leap_year() result(tests)
        type(test_item_t) :: tests

        tests = describe(&
                "is_leap_year", &
                [ it( &
                        "returns false for years that are not divisible by 4", &
                        check_not_divisible_by_4) &
                ])
    end function

    function check_not_divisible_by_4() result(result_)
        type(result_t) :: result_

        result_ = assert_not(is_leap_year(2002))
    end function
end module
```

The `assert_not` function checks that its argument is `.false.`,
and records the results in its return value.
We've now written our first test.
So how do we run it?

# Including Tests Into Your Test Suite

We could actually run the command `fpm test` at this point and our test will compiler.
But nothing is calling it, so it won't run.
We need to write a main program that will gather up all our tests and run them.
Luckily, I've written a tool that can generate such a program for you,
provided you follow the convention we used above.
Write your test suites in modules with names ending `_test`,
in files with the same names,
and return the sections of your test suite from functions that start with `test_`.

To get this tool, clone the repository `https://gitlab.com/everythingfunctional/make_vegetable_driver.git`.
Change directories into that repository and run `fpm install`.
This should compile and put the executable for the tool somewhere that is (hopefully) on your path.
You can specify where you'd like it to be installed with the `--prefix` and/or `--bindir` options to `fpm` if the defaults don't work for you.

Now change directories back to your tutorial project and run the command
`make_vegetable_driver test/main.f90 test/*_test.f90`.
This should create a main program like the following.

```Fortran
! Generated by make_vegetable_driver. DO NOT EDIT
program main
    implicit none

    call run()
contains
    subroutine run()
        use is_leap_year_test, only: &
                is_leap_year_is_leap_year => test_is_leap_year
        use garden, only: test_item_t, test_that, run_tests

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(1)

        individual_tests(1) = is_leap_year_is_leap_year()
        tests = test_that(individual_tests)

        call run_tests(tests)
    end subroutine
end program
```

You should also delete the original program in the `test` folder.
Now run `fpm test` and you should see output like the following.

``` { use_pygments=false }
$ fpm test -- -q -v
Running Tests

A total of 1 test cases

All Passed
Took 1.078e-5 seconds

Test that
    is_leap_year
        returns false for years that are not divisible by 4
            Was not true

A total of 1 test cases containing a total of 1 assertions
```

Congratulations, you've written and executed your first garden test suite!
You can find the code at this stage [here](https://gitlab.com/everythingfunctional/garden_tutorial/-/tree/first_test).

# More Advanced Testing Patterns

The test we wrote above really only used the bare minimum of features available in garden.
There is some additional functionality that would be nice to make use of even for this simple test though.
First of all, when we look at the verbose output from the test, we don't really see what was checked.
All of the `assert_*` functions take two optional message arguments at the end.
We can use this to provide additional detail to anyone executing our test suite,
and if necessary to provide different details depending on whether the check passes or fails.
In this case we'll just provide a string indicating the year we checked.

```Fortran
function check_not_divisible_by_4() result(result_)
    type(result_t) :: result_

    integer, parameter :: YEAR = 2002

    result_ = assert_not(is_leap_year(YEAR), to_string(YEAR))
end function
```

> Note: we're using the `to_string` function from the `strff` library.
> Garden already depends on this library,
> but if you're going to depend on something directly you should add it to your `fpm.toml` file.

Next, we're only checking a single year.
We really should check at least one more, but it would really fall under the same test.
Luckily, we can combine test results using the `.and.` operator, like so.

```Fortran
function check_not_divisible_by_4() result(result_)
    type(result_t) :: result_

    integer, parameter :: YEAR1 = 2002
    integer, parameter :: YEAR2 = 2003

    result_ = &
            assert_not(is_leap_year(YEAR1), to_string(YEAR1)) &
            .and. assert_not(is_leap_year(YEAR2), to_string(YEAR2))
end function
```

Now when we run the tests we can see what we're checking, and that we're checking multiple things.

``` { use_pygments=false }
$ fpm test -- -q -v
Running Tests

A total of 1 test cases

All Passed
Took 3.567e-5 seconds

Test that
    is_leap_year
        returns false for years that are not divisible by 4
            Was not true
                User Message:
                    |2002|
            Was not true
                User Message:
                    |2003|

A total of 1 test cases containing a total of 2 assertions
```

This covers all the basics, and you can find the code at this stage
[here](https://gitlab.com/everythingfunctional/garden_tutorial/-/tree/little_extras).
In the following sections we'll start introducing advanced patterns for testing,
and how garden supports them.

## Providing Example Inputs to a Test

A common desire for testing frameworks is to be able to parameterize a test case by some input.
For instance, in our above example, we have multiple years we'd like to provide as examples.
In fact, we can benefit the understandability of our specification of the tests by putting the examples of inputs we are interested in for a given case near the specification for that case.
We do this by changing two things.
Pass an array of `example_t` objects to the `it` function,
and modify the test function to accept a `class(input_t), intent(in)` argument.
The `example_t` type is a simple wrapper for a `class(input_t), allocatable` variable,
and thus any type extended from `input_t` can be passed as an example to a test case.
For our current example this will look like the following.

```Fortran
module is_leap_year_test
    use is_leap_year_m, only: is_leap_year
    use strff, only: to_string
    use garden, only: &
            example_t, &
            input_t, &
            integer_input_t, &
            result_t, &
            test_item_t, &
            assert_not, &
            describe, &
            fail, &
            it

    implicit none
    private
    public :: test_is_leap_year
contains
    function test_is_leap_year() result(tests)
        type(test_item_t) :: tests

        tests = describe(&
                "is_leap_year", &
                [ it( &
                        "returns false for years that are not divisible by 4", &
                        [ example_t(integer_input_t(2002)) &
                        , example_t(integer_input_t(2003)) &
                        ], &
                        check_not_divisible_by_4) &
                ])
    end function

    function check_not_divisible_by_4(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (integer_input_t)
            result_ = assert_not(is_leap_year(input%input()), to_string(input%input()))
        class default
            result_ = fail("Didn't get integer_input_t")
        end select
    end function
end module
```

Note, this modified example will produce exactly the same report as the previous example.
The framework assumes the job of combining together the results of the test case for each input.
Also note, that best practice is to have a `class default` section in the `select type` construct that reports the test case failure in the case that an unexpected type is encountered.
Chances are small that a test case would receive a type not expected by the author,
but mistakes happen and better to have the test case report the problem.
You can find the code at this stage [here](https://gitlab.com/everythingfunctional/garden_tutorial/-/tree/example_inputs).


## Inputs for a Whole Test Suite

In some cases, there are multiple tests that make sense for some given starting point.
For this and the next section, we're going to switch examples,
and write tests for a stack implementation.
It's a simple stack of integers, with an implementation like below.

```Fortran
module stack_m
    implicit none
    private
    public :: stack_t

    type :: stack_t
        private
        integer, allocatable :: items(:)
    contains
        private
        procedure, public :: empty
        procedure, public :: top
        procedure, public :: pop
        procedure, public :: push
        procedure, public :: depth
    end type

    interface stack_t
        module procedure constructor
    end interface
contains
    pure function constructor() result(empty_stack)
        type(stack_t) :: empty_stack

        allocate(empty_stack%items(0))
    end function

    pure function empty(self)
        class(stack_t), intent(in) :: self
        logical :: empty

        empty = self%depth() == 0
    end function

    pure function top(self)
        class(stack_t), intent(in) :: self
        integer :: top

        if (self%empty()) then
            error stop "Asked for top of an empty stack."
        else
            top = self%items(1)
        end if
    end function

    pure function pop(self) result(popped)
        class(stack_t), intent(in) :: self
        type(stack_t) :: popped

        if (self%empty()) then
            error stop "Attempted to pop an empty stack."
        else
            if (self%depth() > 1) then
                allocate(popped%items, source = self%items(2:))
            else
                allocate(popped%items(0))
            end if
        end if
    end function

    pure function push(self, top) result(pushed)
        class(stack_t), intent(in) :: self
        integer, intent(in) :: top
        type(stack_t) :: pushed

        if (self%empty()) then
            allocate(pushed%items, source = [top])
        else
            allocate(pushed%items, source = [top, self%items])
        end if
    end function

    pure function depth(self)
        class(stack_t), intent(in) :: self
        integer :: depth

        if (allocated(self%items)) then
            depth = size(self%items)
        else
            depth = 0
        end if
    end function
end module
```

In this instance, there are multiple properties of an empty stack that we'd like to test;
that it is empty (i.e. the `empty` procedure returns `.true.`),
and that it has a depth of zero.
If a test collection is given an input, it will pass that input down to each of its test cases.
In this case, that is done like the following.
Note that the argument to `given` is `class(input_t)`, so any type that extends `input_t` can be provided.
Also, we're now using the `it_` function (note the trailing `_`), which is necessary because the generic resolution in Fortran can't distinguish based on procedure arguments.

```Fortran
module stack_test
    use stack_m, only: stack_t
    use stack_input_m, only: stack_input_t
    use garden, only: &
            input_t, &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_that, &
            fail, &
            given, &
            it_

    implicit none
    private
    public :: test_stack
contains
    function test_stack() result(tests)
        type(test_item_t) :: tests

        tests = given( &
                "a new stack", &
                stack_input_t(stack_t()), &
                [ it_("it is empty", check_empty) &
                , it_("it has a depth of zero", check_empty_depth) &
                ])
    end function

    function check_empty(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(stack_t) :: stack

        select type (input)
        type is (stack_input_t)
            stack = input%stack()
            result_ = assert_that(stack%empty())
        class default
            result_ = fail("expected to get a stack_input_t")
        end select
    end function

    function check_empty_depth(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        type(stack_t) :: stack

        select type (input)
        type is (stack_input_t)
            stack = input%stack()
            result_ = assert_equals(0, stack%depth())
        class default
            result_ = fail("expected to get a stack_input_t")
        end select
    end function
end module
```

For reference, the `stack_input_t` type is a simple wrapper to hold on to a `stack_t`,
in order to satisfy the requirement that it extend `input_t`,
with implementation like the following.

```Fortran
module stack_input_m
    use stack_m, only: stack_t
    use garden, only: input_t

    implicit none
    private
    public :: stack_input_t

    type, extends(input_t) :: stack_input_t
        private
        type(stack_t) :: stack_
    contains
        private
        procedure, public :: stack
    end type

    interface stack_input_t
        module procedure constructor
    end interface
contains
    function constructor(stack) result(stack_input)
        type(stack_t), intent(in) :: stack
        type(stack_input_t) :: stack_input

        stack_input%stack_ = stack
    end function

    function stack(self)
        class(stack_input_t), intent(in) :: self
        type(stack_t) :: stack

        stack = self%stack_
    end function
end module
```

Thus, with this new test suite, and remembering to regenerate the driver program with `cart test/main.f90 test/*_test.f90`,
running the tests gives the following.

``` { use_pygments=false }
$ fpm test -- -q -v
Running Tests

A total of 3 test cases

All Passed
Took 4.6017e-5 seconds

Test that
    is_leap_year
        returns false for years that are not divisible by 4
            Was not true
                User Message:
                    |2002|
            Was not true
                User Message:
                    |2003|
    Given a new stack
        it is empty
            Was true
        it has a depth of zero
            Expected and got
                    |0|

A total of 3 test cases containing a total of 4 assertions
```

You'll find the code at this point [here](https://gitlab.com/everythingfunctional/garden_tutorial/-/tree/collection_input).

## Modifying Inputs Before Passing to a Test

In some cases, we'd like to specify some some operation to be performed before checking different properties of the result.
This pattern appears frequently when using a Given/When/Then style.
I.e. Given some initial state, when this operation is performed, then these things are true about the result.

In our example, we'd like to push something on to the stack and then check the resultant state.
The specification then looks like the following.

```Fortran
function test_stack() result(tests)
    type(test_item_t) :: tests

    tests = given( &
            "a new stack", &
            stack_input_t(stack_t()), & ! The starting point
            [ it_("it is empty", check_empty) &
            , it_("it has a depth of zero", check_empty_depth) &
            , when( &
                    "an item is pushed onto it", &
                    push_item, & ! The operation to be performed
                    [ then__("it is no longer empty", check_not_empty) &
                    , then__("it has a depth of one", check_depth_one) &
                    ]) &
            ])
end function
```

The function provided to `when` must match the interface as shown below,
and in the case of our example has an implementation like the following.

```Fortran
function push_item(input) result(output)
    class(input_t), intent(in) :: input
    type(transformed_t) :: output

    type(stack_t) :: stack

    select type (input)
    type is (stack_input_t)
        stack = input%stack()
        output = transformed_t(stack_input_t(stack%push(1)))
    class default
        output = transformed_t(transformation_failure_t(fail( &
                "expected to get a stack_input_t")))
    end select
end function
```

Note that the return type of this function, `transformed_t`,
is another simple wrapper around a `class(item_t)`.
The special type `transformation_failure_t` is provided as a way of signaling to the framework that something went wrong,
and that it should not try to pass the result along to the remaining tests.

The resulting code can be found [here](https://gitlab.com/everythingfunctional/garden_tutorial/-/tree/input_modification).

## Generating Random Inputs for a Test

In some cases there is some foundational property of a piece of code that should hold for any input.
In that case, we'd like to just throw all kinds of examples at it and make sure it still works.
But writing all those examples by hand would be tedious,
possibly (probably?) not cover all the interesting cases,
and add a lot of extraneous code to the test.
What if we could generate random inputs for a test case?
We can.
In this example we're going to write a test you never would in practice
(because if intrinsic addition doesn't work you've got bigger problems),
but will help to illustrate the technique.

The test looks like the following.
You need 3 integers, a, b and c, and whether you do (a + b) + c, or a + (b + c) should give the same answer.
This is the associative property of addition.

```Fortran
module add_test
    use iso_varying_string, only: operator(//)
    use strff, only: to_string
    use three_integer_generator_m, only: THREE_INTEGER_GENERATOR
    use three_integer_input_m, only: three_integer_input_t
    use garden, only: &
            input_t, result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_add
contains
    function test_add() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "addition", &
                [ it("is associative", THREE_INTEGER_GENERATOR, check_associativity) &
                ])
    end function

    function check_associativity(input) result(result_)
        class(input_t), intent(in) :: input
        type(result_t) :: result_

        select type (input)
        type is (three_integer_input_t)
            associate(a => input%first(), b => input%second(), c => input%third())
                result_ = assert_equals( &
                        a + (b + c), &
                        (a + b) + c, &
                        to_string(a) &
                        // " + " // to_string(b) &
                        // " + " // to_string(c))
            end associate
        class default
            result_ = fail("expected to get a three_integer_input_t")
        end select
    end function
end module
```

By passing an object that extends from `generator_t` to the test case,
it will use this object to generate random values that get passed to the test case.
For a type to extend `generator_t`, it must implement 2 type-bound procedures; `generate` and `shrink`.
`generate` will be called repeatedly to produce inputs that will be passed to the test case,
so it should include some randomness, or at least produce different results for each call.

But what if a test fails?
Who knows what kind of complicated input could have caused it.
That complicated input may not make it very easy to figure out what went wrong.
So if a test case fails,
garden will pass the input to the `shrink` function on your derived type,
starting with the input that caused the failure,
until either the test case passes, or the simplest possible input is found.

That may have seemed complicated, so let's look at the implementation for this example.

```Fortran
module three_integer_generator_m
    use three_integer_input_m, only: three_integer_input_t
    use garden, only: &
            generated_t, &
            generator_t, &
            input_t, &
            shrink_result_t, &
            get_random_integer, &
            shrunk_value, &
            simplest_value

    implicit none
    private
    public :: THREE_INTEGER_GENERATOR

    type, extends(generator_t) :: three_integer_generator_t
    contains
      procedure :: generate
      procedure, nopass :: shrink
    end type

    type(three_integer_generator_t), parameter :: THREE_INTEGER_GENERATOR = &
            three_integer_generator_t()
contains
    function generate(self) result(generated_value)
        class(three_integer_generator_t), intent(in) :: self
        type(generated_t) :: generated_value

        generated_value = generated_t(three_integer_input_t( &
                get_random_integer(), get_random_integer(), get_random_integer()))
    end function

    function shrink(input) result(shrunk)
        class(input_t), intent(in) :: input
        type(shrink_result_t) :: shrunk

        select type (input)
        type is (three_integer_input_t)
            associate(a => input%first(), b => input%second(), c => input%third())
                if (all([a, b, c] == 0)) then
                    shrunk = simplest_value(three_integer_input_t( &
                            0, 0, 0))
                else
                    shrunk = shrunk_value(three_integer_input_t( &
                            a/2, b/2, c/2))
                end if
            end associate
        end select
    end function
end module
```

The `generated` function uses the provided helper function `get_random_integer`
to generate 3 random integers, uses those to construct a `three_integer_input_t` value,
and wraps it into a `generated_t`.
The `shrink` function looks at the current `input_t`.
If all 3 numbers are already 0, then this is already the simplest possible input,
so it uses the `simplest_value` function to construct its result.
Otherwise, it returns smaller numbers for each value, using the `shrunk_value` function.

If you run the test suite at this point, you'll see output like the following.
Note where it says `Passed after 100 examples`.
This means that garden used the provided generator to produce 100 random inputs,
and the test case passed for every one.
If you want to get a better feel for what is happening,
you could add `print` statements in the test case and/or `generate` function.

``` { use_pygments=false }
fpm test -- -q -v
Running Tests

A total of 6 test cases

All Passed
Took 1.86468e-3 seconds

Test that
    addition
        is associative
            Passed after 100 examples
    is_leap_year
        returns false for years that are not divisible by 4
            Was not true
                User Message:
                    |2002|
            Was not true
                User Message:
                    |2003|
    Given a new stack
        it is empty
            Was true
        it has a depth of zero
            Expected and got
                    |0|
        When an item is pushed onto it
            Then it is no longer empty
                Was true
            Then it has a depth of one
                Expected and got
                        |1|

A total of 6 test cases containing a total of 7 assertions
```

Additionally, to get a feel for how the `shrink` function works,
you could modify the test to use the expression `(a + b) + c + 1` instead,
to cause the test case to fail.
This will cause output like the following,
and you can again add print statements to get a better feel for it.

``` { use_pygments=false }
fpm test -- -q -v
Running Tests

A total of 6 test cases

Failed
Took 8.96107e-4 seconds

Test that
    addition
        is associative
            Fails with the simplest possible example
            Expected
                    |0|
                but got
                    |1|
                User Message:
                    |0 + 0 + 0|
    is_leap_year
        returns false for years that are not divisible by 4
            Was not true
                User Message:
                    |2002|
            Was not true
                User Message:
                    |2003|
    Given a new stack
        it is empty
            Was true
        it has a depth of zero
            Expected and got
                    |0|
        When an item is pushed onto it
            Then it is no longer empty
                Was true
            Then it has a depth of one
                Expected and got
                        |1|

1 of 6 cases failed
2 of 8 assertions failed
```

You can find the code at this stage [here](https://gitlab.com/everythingfunctional/garden_tutorial/-/tree/property_test).

## Anatomy of an Assertion

While many `assert` functions are available "out of the box",
it is not unlikely you'll encounter a scenario where you'd like to provide your own.
For example, maybe you'd like to extend `assert_equals` to handle some of the types in your own project.
Since every `assert`ion in garden follows pretty much the exact same pattern,
writing your own is pretty easy.
And since the functions for constructing and formatting the message from an assertion are available,
you can reuse them to make your assertion messages look consistent with the built in ones.
The basic pattern for an assertion is like the following.

```Fortran
function assert_something(expected, actual, success_message, failure_message) result(result_)
    type(my_type), intent(in) :: expected
    type(my_type), intent(in) :: actual
    type(varying_string), intent(in) :: success_message
    type(varying_string), intent(in) :: failure_message
    type(result_t) :: result_

    if (some_condition(expected, actual)) then
        result_ = succeed(with_user_message( &
                make_some_message(to_string(expected), to_string(actual)), &
                success_message))
    else
        result_ = fail(with_user_message( &
                make_some_message(to_string(expected), to_string(actual)), &
                failure_message))
    end if
end function
```

This is generally exposed in a generic interface,
with additional specific functions that take only one or no message arguments,
and then call the above function passing either the single message or an empty string
in place of both messages.
See the [Providing Your Own Assertions and Messages] section of the [Organized Listing]
for an overview of the available formatting functions
and take a look at their implementations to see their general pattern.

[Providing Your Own Assertions and Messages]: ./Organized_Listing.html#providing-your-own-assertions-and-messages
[Organized Listing]: ./Organized_Listing.html

# Additional Resources

At this stage, you should have a decent feel for the patterns and capabilities of garden.
For more examples, and illustrations of use in real projects, you should check out:

* The [garden test suite](https://gitlab.com/everythingfunctional/garden/-/tree/main/test)
* The [iso_varying_string test suite](https://gitlab.com/everythingfunctional/iso_varying_string/-/tree/main/test/unit_test)
* The [quaff test suite](https://gitlab.com/everythingfunctional/quaff/-/tree/main/test)
  - Note, this illustrates how garden can be seamlessly extended to provide custom assertions

You should also read through the [organized listing](./Organized_Listing.html) of capabilities to get a more comprehensive view of the available functionality.
