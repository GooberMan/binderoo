cmake_minimum_required( VERSION 3.5.1 )
project( binderoo_util )

# Hack from https://stackoverflow.com/questions/11944060/how-to-detect-target-architecture-using-cmake
execute_process( COMMAND uname -m COMMAND tr -d '\n' OUTPUT_VARIABLE ARCHITECTURE )

file( GLOB_RECURSE BINDEROO_UTIL_SOURCE  	RELATIVE 	${PROJECT_SOURCE_DIR}
														${PROJECT_SOURCE_DIR}/../../../src/util/*.cpp )

include_directories( 	${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/interface
						${PROJECT_SOURCE_DIR}/../../../interface
						${PROJECT_SOURCE_DIR}/../../../src/util )


set( BINDEROO_BINARY_DIRECTORY ${PROJECT_SOURCE_DIR}/../../../../../bin/${CMAKE_SYSTEM_NAME}/${ARCHITECTURE} )

set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )

message( STATUS ${BINDEROO_UTIL_SOURCE})

link_directories( ${BINDEROO_BINARY_DIRECTORY} )
add_executable( binderoo_util ${BINDEROO_UTIL_SOURCE} )
target_link_libraries( binderoo_util binderoo_host dl pthread )