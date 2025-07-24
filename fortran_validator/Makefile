# Makefile for SLATEC validator

FC = gfortran
FFLAGS = -O2 -std=legacy
F90FLAGS = -O2

# Directories
SRC_DIR = ../src
MODERN_DIR = ../modern
OBJ_DIR = obj

# Create obj directory
$(shell mkdir -p $(OBJ_DIR))

# F77 objects (original implementations)
F77_OBJS = $(OBJ_DIR)/pythag_f77.o $(OBJ_DIR)/bsplvn_f77.o

# F77 wrapper objects
WRAPPER_OBJS = $(OBJ_DIR)/pythag_f77_wrapper.o $(OBJ_DIR)/bsplvn_f77_wrapper.o

# Modern objects
MODERN_OBJS = $(OBJ_DIR)/pythag_modern.o $(OBJ_DIR)/bsplvn_modern.o

# Enhanced validator modules
ENHANCED_MODULES = $(OBJ_DIR)/error_analysis_module.o \
                  $(OBJ_DIR)/performance_module.o \
                  $(OBJ_DIR)/output_formats_module.o \
                  $(OBJ_DIR)/state_validation_module.o \
                  $(OBJ_DIR)/numerical_utils_module.o

# Main targets
validator: $(F77_OBJS) $(WRAPPER_OBJS) $(MODERN_OBJS) $(OBJ_DIR)/slatec_validator.o
	$(FC) -o $@ $^

validator_enhanced: $(F77_OBJS) $(WRAPPER_OBJS) $(MODERN_OBJS) $(ENHANCED_MODULES) $(OBJ_DIR)/slatec_validator_enhanced.o
	$(FC) -o $@ $^

# F77 source compilation
$(OBJ_DIR)/pythag_f77.o: $(SRC_DIR)/pythag.f
	$(FC) $(FFLAGS) -c $< -o $@

$(OBJ_DIR)/bsplvn_f77.o: $(SRC_DIR)/bsplvn.f
	$(FC) $(FFLAGS) -c $< -o $@

# Wrapper compilation
$(OBJ_DIR)/pythag_f77_wrapper.o: pythag_f77_wrapper.f
	$(FC) $(FFLAGS) -c $< -o $@

$(OBJ_DIR)/bsplvn_f77_wrapper.o: bsplvn_f77_wrapper.f
	$(FC) $(FFLAGS) -c $< -o $@

# Modern compilation
$(OBJ_DIR)/pythag_modern.o: $(MODERN_DIR)/pythag_modern.f90
	$(FC) $(F90FLAGS) -c $< -o $@

$(OBJ_DIR)/bsplvn_modern.o: $(MODERN_DIR)/bsplvn_modern.f90
	$(FC) $(F90FLAGS) -c $< -o $@

# Test with o3mini version
test-o3mini: $(F77_OBJS) $(WRAPPER_OBJS) $(OBJ_DIR)/pythag_modern.o $(OBJ_DIR)/bsplvn_o3mini.o $(OBJ_DIR)/slatec_validator_o3mini.o
	$(FC) -o validator_o3mini $^
	./validator_o3mini < test_cases.txt

$(OBJ_DIR)/bsplvn_o3mini.o: $(MODERN_DIR)/bsplvn_o3mini_refined.f90
	$(FC) $(F90FLAGS) -c $< -o $@

$(OBJ_DIR)/slatec_validator_o3mini.o: slatec_validator.f90 $(OBJ_DIR)/pythag_modern.o $(OBJ_DIR)/bsplvn_o3mini.o
	$(FC) $(F90FLAGS) -c slatec_validator.f90 -o $@

# Enhanced module compilation
$(OBJ_DIR)/error_analysis_module.o: error_analysis_module.f90
	$(FC) $(F90FLAGS) -c $< -o $@

$(OBJ_DIR)/performance_module.o: performance_module.f90
	$(FC) $(F90FLAGS) -c $< -o $@

$(OBJ_DIR)/output_formats_module.o: output_formats_module.f90 $(OBJ_DIR)/error_analysis_module.o
	$(FC) $(F90FLAGS) -c $< -o $@

$(OBJ_DIR)/state_validation_module.o: state_validation_module.f90
	$(FC) $(F90FLAGS) -c $< -o $@

$(OBJ_DIR)/numerical_utils_module.o: numerical_utils_module.f90
	$(FC) $(F90FLAGS) -c $< -o $@

# Validator compilation
$(OBJ_DIR)/slatec_validator.o: slatec_validator.f90 $(MODERN_OBJS)
	$(FC) $(F90FLAGS) -c $< -o $@

$(OBJ_DIR)/slatec_validator_enhanced.o: slatec_validator_enhanced.f90 $(MODERN_OBJS) $(ENHANCED_MODULES)
	$(FC) $(F90FLAGS) -c $< -o $@

# Run validation
test: validator
	./validator < test_cases.txt

test-enhanced: validator_enhanced
	./validator_enhanced < test_cases.txt

test-formats: validator_enhanced
	@echo "=== Testing Human Format ==="
	./validator_enhanced --format=human < test_cases.txt
	@echo ""
	@echo "=== Testing JSON Format ==="
	./validator_enhanced --format=json < test_cases.txt
	@echo ""
	@echo "=== Testing LLM Format ==="
	./validator_enhanced --format=llm < test_cases.txt

clean:
	rm -rf $(OBJ_DIR) validator validator_enhanced validator_o3mini validator_broken validator_enhanced_broken *.f90.tmp slatec_validator_enhanced_broken.f90 slatec_validator_broken.f90

.PHONY: test test-enhanced test-formats clean test-o3mini test-broken test-enhanced-broken

# Test with broken PYTHAG
test-broken: $(F77_OBJS) $(WRAPPER_OBJS) $(OBJ_DIR)/pythag_broken.o $(OBJ_DIR)/bsplvn_modern.o $(OBJ_DIR)/slatec_validator_broken.o
	$(FC) -o validator_broken $^
	./validator_broken < test_cases.txt

$(OBJ_DIR)/pythag_broken.o: pythag_broken.f90
	$(FC) $(F90FLAGS) -c $< -o $@

$(OBJ_DIR)/slatec_validator_broken.o: slatec_validator_broken.f90
	$(FC) $(F90FLAGS) -c $< -o $@

slatec_validator_broken.f90: slatec_validator.f90
	sed 's/pythag_module/pythag_broken_module/g' slatec_validator.f90 > slatec_validator_broken.f90

# Enhanced validator with broken PYTHAG
validator_enhanced_broken: $(F77_OBJS) $(WRAPPER_OBJS) $(OBJ_DIR)/pythag_broken.o $(OBJ_DIR)/bsplvn_modern.o $(ENHANCED_MODULES) $(OBJ_DIR)/slatec_validator_enhanced_broken.o
	$(FC) -o $@ $^

$(OBJ_DIR)/slatec_validator_enhanced_broken.o: slatec_validator_enhanced_broken.f90 $(OBJ_DIR)/pythag_broken.o $(OBJ_DIR)/bsplvn_modern.o $(ENHANCED_MODULES)
	$(FC) $(F90FLAGS) -c $< -o $@

slatec_validator_enhanced_broken.f90: slatec_validator_enhanced.f90
	sed 's/pythag_module/pythag_broken_module/g' slatec_validator_enhanced.f90 > slatec_validator_enhanced_broken.f90

test-enhanced-broken: validator_enhanced_broken
	./validator_enhanced_broken --format=human < test_cases.txt
	@echo ""
	@echo "=== Testing LLM Format with Broken Implementation ==="
	./validator_enhanced_broken --format=llm < test_cases.txt