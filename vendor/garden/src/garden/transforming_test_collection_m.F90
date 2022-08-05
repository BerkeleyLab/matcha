module garden_transforming_test_collection_m
    use iso_varying_string, only: varying_string, operator(//), put_line, var_str
    use strff, only: operator(.includes.), add_hanging_indentation, join, to_string, NEWLINE
    use garden_command_line_m, only: DEBUG
    use garden_constants_m, only: INDENTATION
    use garden_input_m, only: input_t
    use garden_result_m, only: fail
    use garden_test_m, only: &
            filter_result_t, test_t, filter_failed, filter_matched
    use garden_test_case_result_m, only: test_case_result_t
    use garden_test_collection_result_m, only: test_collection_result_t
    use garden_test_interfaces_m, only: computation_i, transformer_i
    use garden_test_item_m, only: filter_item_result_t, test_item_t
    use garden_test_result_item_m, only: test_result_item_t
    use garden_transformation_failure_m, only: transformation_failure_t
    use garden_transformed_m, only: transformed_t
    
#ifdef USE_CAFFEINE
   use caffeine_m, only : this_image => caf_this_image, num_images => caf_num_images
#endif

    implicit none
    private
    public :: transforming_test_collection_t

    type, extends(test_t) :: transforming_test_collection_t
        private
        type(varying_string) :: description_
        type(test_item_t), allocatable :: tests(:)
        procedure(transformer_i), nopass, pointer :: transformer
        logical :: has_setup_and_teardown
        procedure(computation_i), nopass, pointer :: setup
        procedure(computation_i), nopass, pointer :: teardown
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure, public :: run_with_input
        procedure, public :: run_without_input
    end type

    interface transforming_test_collection_t
        module procedure constructor_basic
        module procedure constructor_bracketed
    end interface
contains
    function constructor_basic( &
            description, transformer, tests) result(transforming_test_collection)
        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(transforming_test_collection_t) :: transforming_test_collection

        transforming_test_collection%description_ = description
        transforming_test_collection%transformer => transformer
        allocate(transforming_test_collection%tests, source = tests)
        transforming_test_collection%has_setup_and_teardown = .false.
    end function

    function constructor_bracketed( &
            description, &
            transformer, &
            tests, &
            setup, &
            teardown) &
            result(transforming_test_collection)
        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(transforming_test_collection_t) :: transforming_test_collection

        transforming_test_collection%description_ = description
        transforming_test_collection%transformer => transformer
        allocate(transforming_test_collection%tests, source = tests)
        transforming_test_collection%has_setup_and_teardown = .true.
        transforming_test_collection%setup => setup
        transforming_test_collection%teardown => teardown
    end function

    pure recursive function description(self)
        class(transforming_test_collection_t), intent(in) :: self
        type(varying_string) :: description

        integer :: i

        description = add_hanging_indentation( &
                self%description_ // NEWLINE // join( &
                        [(self%tests(i)%description(), i = 1, size(self%tests))], &
                        NEWLINE), &
                INDENTATION)
    end function

    recursive function filter(self, filter_string) result(filter_result)
        class(transforming_test_collection_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        type(transforming_test_collection_t) :: new_collection
        type(filter_item_result_t) :: filter_results(size(self%tests))
        integer :: i
        logical :: matches(size(self%tests))
        type(test_item_t) :: maybe_tests(size(self%tests))

        if (self%description_.includes.filter_string) then
            filter_result = filter_matched(self)
        else
            filter_results = [(self%tests(i)%filter(filter_string), i = 1, size(self%tests))]
            if (any(filter_results%matched())) then
                matches = filter_results%matched()
                maybe_tests = filter_results%test()
                new_collection = self
                deallocate(new_collection%tests)
                allocate(new_collection%tests, source = &
                        pack(maybe_tests, mask=matches))
                filter_result = filter_matched(new_collection)
            else
                filter_result = filter_failed()
            end if
        end if
    end function

    pure recursive function num_cases(self)
        class(transforming_test_collection_t), intent(in) :: self
        integer :: num_cases

        integer :: i

        num_cases = sum([(self%tests(i)%num_cases(), i = 1, size(self%tests))])
    end function

    recursive function run_with_input(self, input) result(result_)
        class(transforming_test_collection_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        integer :: i
        type(test_result_item_t) :: results(size(self%tests))
        type(transformed_t) :: transformed_

        if (DEBUG) call put_line( &
                "Beginning execution of: " // self%description_ &
                // merge(" on image " // to_string(this_image()), var_str(""), num_images() > 1))
        if (self%has_setup_and_teardown) call self%setup
        transformed_ = self%transformer(input)
        select type (transformed_input => transformed_%input())
        type is (transformation_failure_t)
            result_ = test_result_item_t(test_case_result_t( &
                    self%description_, transformed_input%result_()))
        class default
            do i = 1, size(self%tests)
                results(i) = self%tests(i)%run(transformed_input)
            end do
            result_ = test_result_item_t(test_collection_result_t( &
                    self%description_, results))
        end select
        if (self%has_setup_and_teardown) call self%teardown
        if (DEBUG) call put_line( &
                "Completed execution of: " // self%description_&
                // merge(" on image " // to_string(this_image()), var_str(""), num_images() > 1))
    end function

    function run_without_input(self) result(result_)
        class(transforming_test_collection_t), intent(in) :: self
        type(test_result_item_t) :: result_

        if (DEBUG) call put_line( &
                "Improper use of: " // self%description_&
                // merge(" on image " // to_string(this_image()), var_str(""), num_images() > 1))
        result_ = test_result_item_t(test_case_result_t( &
                self%description_, fail("No input provided")))
    end function
end module
