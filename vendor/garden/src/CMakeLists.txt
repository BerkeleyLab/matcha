set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

set(
  srcs
  "${dir}/garden.f90"
  "${dir}/garden/ascii_string_generator_m.f90"
  "${dir}/garden/assert_m.f90"
  "${dir}/garden/command_line_m.f90"
  "${dir}/garden/constants_m.f90"
  "${dir}/garden/double_precision_input_m.f90"
  "${dir}/garden/example_m.f90"
  "${dir}/garden/generated_m.f90"
  "${dir}/garden/generator_m.f90"
  "${dir}/garden/individual_result_m.f90"
  "${dir}/garden/input_m.f90"
  "${dir}/garden/input_test_case_m.f90"
  "${dir}/garden/integer_generator_m.f90"
  "${dir}/garden/integer_input_m.f90"
  "${dir}/garden/messages_m.f90"
  "${dir}/garden/random_m.f90"
  "${dir}/garden/result_m.f90"
  "${dir}/garden/run_tests_m.f90"
  "${dir}/garden/shrink_result_m.f90"
  "${dir}/garden/simple_test_case_m.f90"
  "${dir}/garden/simple_test_collection_m.f90"
  "${dir}/garden/string_input_m.f90"
  "${dir}/garden/test_case_result_m.f90"
  "${dir}/garden/test_case_with_examples_m.f90"
  "${dir}/garden/test_case_with_generator_m.f90"
  "${dir}/garden/test_collection_result_m.f90"
  "${dir}/garden/test_collection_with_input_m.f90"
  "${dir}/garden/test_constructors_m.f90"
  "${dir}/garden/test_interfaces_m.f90"
  "${dir}/garden/test_item_m.f90"
  "${dir}/garden/test_m.f90"
  "${dir}/garden/test_result_item_m.f90"
  "${dir}/garden/test_result_m.f90"
  "${dir}/garden/transformation_failure_m.f90"
  "${dir}/garden/transformed_m.f90"
  "${dir}/garden/transforming_test_collection_m.f90"
  "${dir}/garden/assert/doesnt_include_m.f90"
  "${dir}/garden/assert/empty_m.f90"
  "${dir}/garden/assert/equals_double_precision_m.f90"
  "${dir}/garden/assert/equals_integer_m.f90"
  "${dir}/garden/assert/equals_strings_m.f90"
  "${dir}/garden/assert/equals_within_absolute_m.f90"
  "${dir}/garden/assert/equals_within_relative_m.f90"
  "${dir}/garden/assert/faster_than_m.f90"
  "${dir}/garden/assert/includes_m.f90"
  "${dir}/garden/assert/not_m.f90"
  "${dir}/garden/assert/that_m.f90"
)

set(
  MODULE_OUTPUT_DIR
  "${CMAKE_CURRENT_BINARY_DIR}/include"
)

# Create example library target
add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)
set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${MODULE_OUTPUT_DIR}"
)
target_link_libraries(
  "${PROJECT_NAME}-lib"
  PRIVATE
  "${lib-deps}"
)
target_include_directories(
  "${PROJECT_NAME}-lib"
  INTERFACE
  $<BUILD_INTERFACE:${MODULE_OUTPUT_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

if(INSTALL_PROJECT)
  # Export targets for other projects
  install(
    TARGETS
    "${PROJECT_NAME}-lib"
    EXPORT
    "${PROJECT_NAME}-targets"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )
  install(
    DIRECTORY
    "${MODULE_OUTPUT_DIR}/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
  )
endif()
