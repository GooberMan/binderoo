set( CMAKE_C_COMPILER, "clang" )
set( CMAKE_CXX_COMPILER, "clang++" )
set( CMAKE_D_COMPILER, "ldc2" )

set( CMAKE_CXX_STANDARD 14 )
set( CMAKE_CXX_STANDARD_REQUIRED ON )
set( CMAKE_CXX_EXTENSIONS OFF )

cmake_minimum_required( VERSION 3.3.0 )

project( binderoo_util )

# Hack from https://stackoverflow.com/questions/11944060/how-to-detect-target-architecture-using-cmake
execute_process( COMMAND uname -m COMMAND tr -d '\n' OUTPUT_VARIABLE ARCHITECTURE )
# execute_process( COMMAND lsb_release -is COMMAND tr -d '\n' OUTPUT_VARIABLE DISTRO_ID )
# execute_process( COMMAND lsb_release -cs COMMAND tr -d '\n' OUTPUT_VARIABLE DISTRO_CODENAME )
# message( "Outputting binderoo_util to bin/" ${CMAKE_SYSTEM_NAME} "/binderoo_host/" ${ARCHITECTURE} "/" ${CMAKE_BUILD_TYPE} )

file( GLOB_RECURSE BINDEROO_UTIL_SOURCE  	RELATIVE 	${PROJECT_SOURCE_DIR}
														${PROJECT_SOURCE_DIR}/../../../src/util/*.cpp )

include_directories( 	${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/interface
						${PROJECT_SOURCE_DIR}/../../../interface
						${PROJECT_SOURCE_DIR}/../../../src/util )


set( BINDEROO_BINARY_DIRECTORY ${PROJECT_SOURCE_DIR}/../../../../../bin/${CMAKE_SYSTEM_NAME}/binderoo_host/${ARCHITECTURE}/${CMAKE_BUILD_TYPE} )

set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )

#message( STATUS ${BINDEROO_UTIL_SOURCE} )

link_directories( ${BINDEROO_BINARY_DIRECTORY} )
add_executable( binderoo_util ${BINDEROO_UTIL_SOURCE} )
target_link_libraries( binderoo_util binderoo_host dl pthread )

# Need a way to pipe the link options corrrectly.
#target_compile_options( binderoo_util PRIVATE -Wl,-rpath,"\$ORIGIN" )