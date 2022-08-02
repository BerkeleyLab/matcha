cmake_minimum_required(VERSION 3.14)

project(
  "iso_varying_string"
  LANGUAGES "Fortran"
  VERSION "3.0.2"
)

option(INSTALL_PROJECT "Whether this project should be installed" TRUE)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# Collect source of the project
add_subdirectory("src")

if(INSTALL_PROJECT)
  # Export targets for other projects
  add_library("${PROJECT_NAME}" INTERFACE)
  target_link_libraries("${PROJECT_NAME}" INTERFACE "${PROJECT_NAME}-lib")
  install(
    TARGETS
    "${PROJECT_NAME}"
    EXPORT
    "${PROJECT_NAME}-targets"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )

  # Install exported targets
  install(
    EXPORT "${PROJECT_NAME}-targets"
    NAMESPACE
    "${PROJECT_NAME}::"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  )

  # Package license files
  install(
    FILES
    "LICENSE"
    DESTINATION "${CMAKE_INSTALL_DATADIR}/licenses/${PROJECT_NAME}"
  )
endif()
