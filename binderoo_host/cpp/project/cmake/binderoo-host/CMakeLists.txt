cmake_minimum_required( VERSION 3.5.1 )
project( binderoo-host )

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

add_library( binderoo-host STATIC ${BINDEROO_COMMON_SOURCE} ${BINDEROO_HOST_SOURCE} )

install( TARGETS binderoo-host DESTINATION ${PROJECT_SOURCE_DIR}/../../../../../bin/cmake/${CMAKE_SYSTEM_NAME} )