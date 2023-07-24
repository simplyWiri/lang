cmake_minimum_required(VERSION 3.25)
project(grow)
set(CMAKE_CXX_STANDARD 20)

add_executable(lexer_tests test/lexer_tests.cpp)
target_include_directories(lexer_tests PUBLIC src ext)

if (CMAKE_BUILD_TYPE EQUAL "Release")
    # Set up clang tidy executable, assumed to be installed.
    find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
    set(CLANG_TIDY_COMMAND "${CLANG_TIDY_EXE}" "-checks=-*,modernize-*")
    message("Found clang tidy: ${CLANG_TIDY_EXE}")

    set_property(TARGET lexer_tests PROPERTY INTERPROCEDURAL_OPTIMIZATION True)
    set_target_properties(lexer_tests PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_COMMAND}")
endif ()

# Use lld linker, compile with pedantic errors, use link time optimization (lto)
target_compile_options(lexer_tests PUBLIC -Werror -Wall -Wpedantic -Wextra)
target_link_options(lexer_tests PUBLIC -fuse-ld=/usr/bin/ld.lld)

# Set up ctest functionality
enable_testing()
add_test(
    NAME lexer_tests
    COMMAND $<TARGET_FILE:lexer_tests>
)