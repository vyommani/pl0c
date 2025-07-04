#!/bin/bash

set -e

if [ $# -ne 3 ]; then
  echo "Usage: $0 <arch> <input.s> <output>"
  exit 1
fi

arch="$1"
input="$2"
output="$3"

# Check for assembler
if ! command -v as &> /dev/null; then
    echo "Error: 'as' assembler not found. Please install Xcode Command Line Tools."
    exit 1
fi

# Check for linker
if ! command -v clang &> /dev/null; then
    echo "Error: 'clang' linker not found. Please install Xcode Command Line Tools."
    exit 1
fi

# Assemble
as -arch "$arch" -o output.o "$input"

# Link
clang -arch "$arch" -o "$output" output.o -e _start
