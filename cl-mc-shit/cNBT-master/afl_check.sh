#!/usr/bin/env bash

afl_check="$1"

echo -ne '\x09\x00\x00' | $afl_check
echo -ne '\x64' | $afl_check
echo -ne '\xff\xfe\xff\xfe\x00\x03\xe8\xff\xfe\xff\xfe\x00\x03\xe8\xfe\x96\xf3\xf8' | $afl_check
echo -ne '\x40' | $afl_check
echo -ne '\x00\x64\x2e' | $afl_check
