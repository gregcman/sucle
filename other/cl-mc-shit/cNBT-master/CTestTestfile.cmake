# CMake generated Testfile for 
# Source directory: /Users/gregmanabat/quicklisp/local-projects/symmetrical-umbrella/cl-mc-shit/cNBT-master
# Build directory: /Users/gregmanabat/quicklisp/local-projects/symmetrical-umbrella/cl-mc-shit/cNBT-master
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test_hello_world "bin/check" "/Users/gregmanabat/quicklisp/local-projects/symmetrical-umbrella/cl-mc-shit/cNBT-master/testdata/hello_world.nbt")
add_test(test_simple_level "bin/check" "/Users/gregmanabat/quicklisp/local-projects/symmetrical-umbrella/cl-mc-shit/cNBT-master/testdata/simple_level.nbt")
add_test(test_issue_13 "bin/check" "/Users/gregmanabat/quicklisp/local-projects/symmetrical-umbrella/cl-mc-shit/cNBT-master/testdata/issue_13.nbt")
add_test(test_issue_18 "bin/check" "/Users/gregmanabat/quicklisp/local-projects/symmetrical-umbrella/cl-mc-shit/cNBT-master/testdata/issue_18.nbt")
add_test(test_afl "/Users/gregmanabat/quicklisp/local-projects/symmetrical-umbrella/cl-mc-shit/cNBT-master/afl_check.sh" "bin/afl_check")
