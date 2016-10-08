# CMake generated Testfile for 
# Source directory: /home/imac/Downloads/cNBT-master
# Build directory: /home/imac/Downloads/cNBT-master
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test_hello_world "bin/check" "/home/imac/Downloads/cNBT-master/testdata/hello_world.nbt")
add_test(test_simple_level "bin/check" "/home/imac/Downloads/cNBT-master/testdata/simple_level.nbt")
add_test(test_issue_13 "bin/check" "/home/imac/Downloads/cNBT-master/testdata/issue_13.nbt")
add_test(test_issue_18 "bin/check" "/home/imac/Downloads/cNBT-master/testdata/issue_18.nbt")
add_test(test_afl "/home/imac/Downloads/cNBT-master/afl_check.sh" "bin/afl_check")
