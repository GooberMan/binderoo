set( CMAKE_C_COMPILER, "clang" )
set( CMAKE_CXX_COMPILER, "clang++" )
set( CMAKE_D_COMPILER, "ldc2" )

set( CMAKE_CXX_STANDARD 14 )
set( CMAKE_CXX_STANDARD_REQUIRED ON )
set( CMAKE_CXX_EXTENSIONS OFF )

cmake_minimum_required( VERSION 3.3.0 )

project( binderoo-host-all )

add_subdirectory( binderoo_host )
add_subdirectory( binderoo_util )