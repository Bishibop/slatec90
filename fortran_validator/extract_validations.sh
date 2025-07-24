#!/bin/bash
# Extract validation routines from enhanced validator

# Extract PYTHAG validation
sed -n '/subroutine validate_pythag_enhanced/,/end subroutine validate_pythag_enhanced/p' \
    slatec_validator_enhanced.f90 | \
    sed 's/validate_pythag_enhanced/validate_pythag/g' > pythag_validation.inc

# Extract BSPLVN validation  
sed -n '/subroutine validate_bsplvn_enhanced/,/end subroutine validate_bsplvn_enhanced/p' \
    slatec_validator_enhanced.f90 | \
    sed 's/validate_bsplvn_enhanced/validate_bsplvn/g' > bsplvn_validation.inc

echo "Extracted validation routines to .inc files"
