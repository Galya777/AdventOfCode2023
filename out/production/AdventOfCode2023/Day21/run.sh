#!/bin/bash

# Compile the Java file
echo "Compiling..."
javac Day21Part1.java

# Check if compilation was successful
if [ $? -eq 0 ]; then
    echo "Running..."
    # Run the Java program
    java Day21Part1
else
    echo "Compilation failed. Please check for errors in your Java code."
    exit 1
fi
