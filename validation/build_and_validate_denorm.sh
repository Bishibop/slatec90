#!/bin/bash

# Build and run DENORM enhanced validation

echo "Building DENORM enhanced validation program..."

# Create validation directory if it doesn't exist
mkdir -p validation

# Compile the modern DENORM implementation and validation program
gfortran -O2 -Wall -Wextra -ffree-line-length-none \
    -o validation/validate_denorm_enhanced \
    modern/denorm_modern.f90 \
    validation/validate_denorm_enhanced.f90

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo "Running enhanced validation tests..."
cd "$( dirname "${BASH_SOURCE[0]}" )/.."
./validation/validate_denorm_enhanced

if [ $? -eq 0 ]; then
    echo ""
    echo "Validation complete. Check validation/denorm_enhanced_validation_report.txt for details."
else
    echo "Validation program failed!"
    exit 1
fi