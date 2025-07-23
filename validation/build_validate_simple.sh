#!/bin/bash

echo "Building simple DENORM validation..."

gfortran -O2 -o validation/validate_denorm_simple \
    modern/denorm_modern.f90 \
    validation/validate_denorm_simple.f90

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo "Running validation..."
./validation/validate_denorm_simple