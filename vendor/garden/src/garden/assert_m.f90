module garden_assert_m
    use garden_assert_doesnt_include_m, only: assert_doesnt_include
    use garden_assert_empty_m, only: assert_empty
    use garden_assert_equals_double_precision_m, only: assert_equals
    use garden_assert_equals_double_precision_array_m, only: assert_equals
    use garden_assert_equals_double_precision_matrix_m, only: assert_equals
    use garden_assert_equals_double_precision_tensor_m, only: assert_equals
    use garden_assert_equals_integer_m, only: assert_equals
    use garden_assert_equals_integer_array_m, only: assert_equals
    use garden_assert_equals_integer_matrix_m, only: assert_equals
    use garden_assert_equals_integer_tensor_m, only: assert_equals
    use garden_assert_equals_strings_m, only: assert_equals
    use garden_assert_equals_within_absolute_m, only: &
            assert_equals_within_absolute
    use garden_assert_equals_within_absolute_array_m, only: &
            assert_equals_within_absolute
    use garden_assert_equals_within_absolute_matrix_m, only: &
            assert_equals_within_absolute
    use garden_assert_equals_within_absolute_tensor_m, only: &
            assert_equals_within_absolute
    use garden_assert_equals_within_relative_m, only: &
            assert_equals_within_relative
    use garden_assert_equals_within_relative_array_m, only: &
            assert_equals_within_relative
    use garden_assert_equals_within_relative_matrix_m, only: &
            assert_equals_within_relative
    use garden_assert_equals_within_relative_tensor_m, only: &
            assert_equals_within_relative
    use garden_assert_faster_than_m, only: assert_faster_than
    use garden_assert_includes_m, only: assert_includes
    use garden_assert_not_m, only: assert_not
    use garden_assert_that_m, only: assert_that
end module
