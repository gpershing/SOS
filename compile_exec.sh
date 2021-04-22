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

if [ ! -f util_math.o ]
then
    echo "Could not find util_math.o"
    echo "Try \"make util_math.o\""
    exit 1
fi

filename=$1
basename=${filename%.sos}

$SOS $filename >${basename}.ll
$LLC -relocation-model=pic ${basename}.ll >${basename}.s
$CC -o ${basename}.exe ${basename}.s util_math.o -lm

rm ${basename}.ll ${basename}.s
