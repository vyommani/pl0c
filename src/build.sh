#!/bin/bash

set -e

if [ $# -ne 3 ]; then
  echo "Usage: $0 <arch> <input.s> <output>"
  exit 1
fi

arch="$1"
input="$2"
output="$3"

os_name="$(uname)"

if [ "$os_name" = "Darwin" ]; then
    # macOS: support both x86_64 and arm64
    if [ "$arch" != "x86_64" ] && [ "$arch" != "arm64" ]; then
        echo "Error: On macOS, only x86_64 and arm64 architectures are supported."
        exit 1
    fi
    if ! command -v as &> /dev/null; then
        echo "Error: 'as' assembler not found. Please install Xcode Command Line Tools."
        exit 1
    fi
    if ! command -v clang &> /dev/null; then
        echo "Error: 'clang' linker not found. Please install Xcode Command Line Tools."
        exit 1
    fi
    as -arch "$arch" -o output.o "$input"
    clang -arch "$arch" -o "$output" output.o -e _start
elif [ "$os_name" = "Linux" ]; then
    # Linux: only support x86_64
    if [ "$arch" != "x86_64" ]; then
        echo "Error: On Linux, only x86_64 architecture is supported."
        exit 1
    fi
    if ! command -v as &> /dev/null; then
        echo "Error: 'as' assembler not found. Please install binutils."
        exit 1
    fi
    if ! command -v gcc &> /dev/null; then
        echo "Error: 'gcc' linker not found. Please install gcc."
        exit 1
    fi
    as -o output.o "$input"
    gcc -no-pie -o "$output" output.o
else
    echo "Error: Unsupported OS: $os_name"
    exit 1
fi

rm -f output.o
