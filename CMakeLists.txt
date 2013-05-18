cmake_minimum_required(VERSION 2.8)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

find_package(LibClang REQUIRED)
find_package(Boost COMPONENTS system filesystem REQUIRED)

include_directories(
	${LIBCLANG_INCLUDE_DIR}
)

add_definitions("-std=c++0x")

add_executable(luabinder WIN32 luabinder.cpp)

target_link_libraries(luabinder
	${Boost_SYSTEM_LIBRARY}
	${Boost_FILESYSTEM_LIBRARY}
 	${LIBCLANG_LIBRARY}
)