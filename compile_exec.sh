#!/bin/sh

# Builds a .sos file to an executable file
# Requires that ./sos.native has been built

# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the SOS compiler
SOS="./sos.native"

filename=$1
basename=${filename%.sos}

$SOS $filename >${basename}.ll
$LLC -relocation-model=pic ${basename}.ll >${basename}.s
$CC -o ${basename}.exe ${basename}.s

rm ${basename}.ll ${basename}.s
