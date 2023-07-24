cmake_minimum_required(VERSION 3.25)
project(grow)
set(CMAKE_CXX_STANDARD 20)

set(ABSL_PROPAGATE_CXX_STD ON)
add_subdirectory(ext/abseil EXCLUDE_FROM_ALL)

add_executable(lang test/lexer_tests.cpp test/util_tests.cpp)
target_include_directories(lang PUBLIC src ext)
target_link_libraries(lang absl::span absl::inlined_vector)


if (CMAKE_BUILD_TYPE EQUAL "Release")
    # Set up clang tidy executable, assumed to be installed.
    find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
    set(CLANG_TIDY_COMMAND "${CLANG_TIDY_EXE}" "-checks=-*,modernize-*")
    message("Found clang tidy: ${CLANG_TIDY_EXE}")

    set_property(TARGET lang PROPERTY INTERPROCEDURAL_OPTIMIZATION True)
    set_target_properties(lang PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_COMMAND}")
endif ()

# Use lld linker, compile with pedantic errors, use link time optimization (lto)
target_compile_options(lang PUBLIC -Werror -Wall -Wpedantic -Wextra)
target_link_options(lang PUBLIC -fuse-ld=/usr/bin/ld.lld)

# Set up ctest functionality
enable_testing()
add_test(
    NAME tests
    COMMAND $<TARGET_FILE:lang>
)