#!/bin/bash

echo "Building full DENORM validation..."

gfortran -O2 -o validation/validate_denorm_full \
    modern/denorm_modern.f90 \
    validation/validate_denorm_full.f90

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo "Running full validation (257 tests)..."
./validation/validate_denorm_full