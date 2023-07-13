cmake_minimum_required(VERSION 3.25)
project(grow)
set(CMAKE_CXX_STANDARD 20)

add_executable(lexer_tests test/lexer_tests.cpp)
target_include_directories(lexer_tests PUBLIC src ext)

# Use lld linker, compile with pedantic errors, use link time optimization (lto)
target_link_options(lexer_tests PUBLIC -fuse-ld=/usr/bin/ld.lld)
target_compile_options(lexer_tests PUBLIC -Werror -Wall -Wpedantic -Wextra)
set_property(TARGET lexer_tests PROPERTY INTERPROCEDURAL_OPTIMIZATION True)

# Set up ctest functionality
enable_testing()
add_test(
    NAME lexer_tests
    COMMAND $<TARGET_FILE:lexer_tests>
)