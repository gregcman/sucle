project(cnbt)

cmake_minimum_required(VERSION 2.6)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

IF (CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_C_FLAGS "-Wall -W -Wextra -fstrict-aliasing -Wstrict-aliasing=3 -pedantic -std=c99 -Wunsafe-loop-optimizations -funsafe-loop-optimizations")
  set(CMAKE_C_FLAGS_DEBUG "-DDEBUG -g")
  set(CMAKE_C_FLAGS_PROFILE "-DDEBUG -g -pg")
  set(CMAKE_C_FLAGS_RELEASE "-march=native -O3 -s -DNDEBUG")
ENDIF()

# Output paths
set(EXECUTABLE_OUTPUT_PATH bin)

find_program(BASH_PROGRAM bash)

ADD_LIBRARY(nbt SHARED
  buffer.c
  nbt_loading.c
  nbt_parsing.c
  nbt_treeops.c
  nbt_util.c
)

ADD_EXECUTABLE(check check.c)
ADD_EXECUTABLE(afl_check afl_check.c)
ADD_EXECUTABLE(nbtreader main.c)

TARGET_LINK_LIBRARIES(check nbt z)
TARGET_LINK_LIBRARIES(afl_check nbt z)
TARGET_LINK_LIBRARIES(nbtreader nbt z)

include(CTest)

ADD_TEST(test_hello_world ${EXECUTABLE_OUTPUT_PATH}/check ${CMAKE_CURRENT_SOURCE_DIR}/testdata/hello_world.nbt)
ADD_TEST(test_simple_level ${EXECUTABLE_OUTPUT_PATH}/check ${CMAKE_CURRENT_SOURCE_DIR}/testdata/simple_level.nbt)
ADD_TEST(test_issue_13 ${EXECUTABLE_OUTPUT_PATH}/check ${CMAKE_CURRENT_SOURCE_DIR}/testdata/issue_13.nbt)
ADD_TEST(test_issue_18 ${EXECUTABLE_OUTPUT_PATH}/check ${CMAKE_CURRENT_SOURCE_DIR}/testdata/issue_18.nbt)
ADD_TEST(test_afl ${CMAKE_CURRENT_SOURCE_DIR}/afl_check.sh ${EXECUTABLE_OUTPUT_PATH}/afl_check)
