# Include the iso_varying_string project
if(NOT TARGET "iso_varying_string")
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/iso_varying_string/CMakeLists.txt)
    add_subdirectory("iso_varying_string")
  else()
    set("iso_varying_string-url" "https://gitlab.com/everythingfunctional/iso_varying_string")
    message(STATUS "Retrieving iso_varying_string from ${iso_varying_string-url}")
    include(FetchContent)
    FetchContent_Declare(
      "iso_varying_string"
      GIT_REPOSITORY "${iso_varying_string-url}"
      GIT_TAG "v3.0.1"
    )
    FetchContent_MakeAvailable("iso_varying_string")
  endif()
endif()

list(
  APPEND lib-deps
  "iso_varying_string"
)

if(NOT TARGET "strff")
  # Include the strff project
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/strff/CMakeLists.txt)
    add_subdirectory("strff")
  else()
    set("strff-url" "https://gitlab.com/everythingfunctional/strff")
    message(STATUS "Retrieving strff from ${strff-url}")
    include(FetchContent)
    FetchContent_Declare(
      "strff"
      GIT_REPOSITORY "${strff-url}"
      GIT_TAG "v3.0.0"
    )
    FetchContent_MakeAvailable("strff")
  endif()
endif()

list(
  APPEND lib-deps
  "strff"
)

set(lib-deps "${lib-deps}" PARENT_SCOPE)
