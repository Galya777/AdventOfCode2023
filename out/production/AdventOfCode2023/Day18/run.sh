#!/bin/bash

# Compile all Java files
echo "Compiling..."
javac -d . *.java

# Run Part 1
echo -e "\n--- Part 1 ---"
java Day18.Day18Part1

# Run Part 2
echo -e "\n--- Part 2 ---"
java Day18.Day18Part2
