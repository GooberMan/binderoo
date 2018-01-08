cmake_minimum_required( VERSION 3.5.1 )
project( binderoo_host )

# Hack from https://stackoverflow.com/questions/11944060/how-to-detect-target-architecture-using-cmake
execute_process( COMMAND uname -m COMMAND tr -d '\n' OUTPUT_VARIABLE ARCHITECTURE )

file( GLOB_RECURSE BINDEROO_COMMON_SOURCE  	RELATIVE 	${PROJECT_SOURCE_DIR}
														${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/src/*.cpp )
file( GLOB_RECURSE BINDEROO_COMMON_INCLUDE  RELATIVE 	${PROJECT_SOURCE_DIR}
														${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/src/*.h
														${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/src/*.inl
														${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/interface/*.h
														${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/interface/*.inl )

file( GLOB_RECURSE BINDEROO_HOST_SOURCE  	RELATIVE 	${PROJECT_SOURCE_DIR}
														${PROJECT_SOURCE_DIR}/../../../src/binderoo/*.cpp )
file( GLOB_RECURSE BINDEROO_HOST_INCLUDE  	RELATIVE 	${PROJECT_SOURCE_DIR}
														${PROJECT_SOURCE_DIR}/../../../src/binderoo/*.h
														${PROJECT_SOURCE_DIR}/../../../src/binderoo/*.inl
														${PROJECT_SOURCE_DIR}/../../../interface/*.h
														${PROJECT_SOURCE_DIR}/../../../interface/*.inl )

include_directories( 	${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/interface
						${PROJECT_SOURCE_DIR}/../../../../../binderoo_common/cpp/src
						${PROJECT_SOURCE_DIR}/../../../interface
						${PROJECT_SOURCE_DIR}/../../../src )

set( BINDEROO_BINARY_DIRECTORY ${PROJECT_SOURCE_DIR}/../../../../../bin/${CMAKE_SYSTEM_NAME}/${ARCHITECTURE} )

set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${BINDEROO_BINARY_DIRECTORY} )

link_directories( ${BINDEROO_BINARY_DIRECTORY} )

add_library( binderoo_host STATIC ${BINDEROO_COMMON_SOURCE} ${BINDEROO_HOST_SOURCE} )