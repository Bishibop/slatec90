# Makefile for SLATEC Validator

FC = gfortran
FFLAGS = -O2 -J$(OBJ_DIR)
OBJ_DIR = obj
SRC_DIR = ../src
MODERN_DIR = ../modern

# Create object directory
$(shell mkdir -p $(OBJ_DIR))

# Include auto-generated function list
include functions.mk

# Module objects
MODULE_OBJS = $(OBJ_DIR)/error_analysis_module.o \
              $(OBJ_DIR)/performance_module.o \
              $(OBJ_DIR)/output_formats_module.o \
              $(OBJ_DIR)/state_validation_module.o \
              $(OBJ_DIR)/numerical_utils_module.o \
              $(OBJ_DIR)/runtime_detection_module.o \
              $(OBJ_DIR)/validation_reporting_module.o \
              $(OBJ_DIR)/slatec_signatures_module.o \
              $(OBJ_DIR)/function_execution_module.o \
              $(OBJ_DIR)/validator_module.o \
              $(OBJ_DIR)/function_dispatcher_module.o

# F77 objects - only compile tested functions
TESTED_F77_SRCS := $(foreach func,$(ALL_TESTED_FUNCTIONS),$(wildcard $(SRC_DIR)/$(func).f))
ALL_F77_OBJS := $(patsubst $(SRC_DIR)/%.f,$(OBJ_DIR)/%_f77.o,$(TESTED_F77_SRCS))

# Modern objects
MODERN_OBJS := $(patsubst %,$(OBJ_DIR)/%_modern.o,$(DISCOVERED_FUNCTIONS))

# Error handling F77 dependencies
ERROR_F77_OBJS = $(OBJ_DIR)/xermsg_f77.o \
                 $(OBJ_DIR)/ivout_f77.o \
                 $(OBJ_DIR)/j4save_f77.o \
                 $(OBJ_DIR)/xercnt_f77.o \
                 $(OBJ_DIR)/xerhlt_f77.o \
                 $(OBJ_DIR)/xerprn_f77.o \
                 $(OBJ_DIR)/xersve_f77.o \
                 $(OBJ_DIR)/xgetua_f77.o

# Main target
validator: $(MODULE_OBJS) $(ALL_F77_OBJS) $(MODERN_OBJS) $(ERROR_F77_OBJS) $(OBJ_DIR)/validator.o
	$(FC) -o $@ $^

# Modern function dependencies
# GAMLN depends on r1mach and i1mach
$(OBJ_DIR)/gamln_modern.o: $(MODERN_DIR)/gamln_module.f90 $(OBJ_DIR)/r1mach_modern.o $(OBJ_DIR)/i1mach_modern.o
	$(FC) $(FFLAGS) -c $< -o $@

# CSROOT depends on pythag 
$(OBJ_DIR)/csroot_modern.o: $(MODERN_DIR)/csroot_module.f90 $(OBJ_DIR)/pythag_modern.o
	$(FC) $(FFLAGS) -c $< -o $@

# Module compilation rules
$(OBJ_DIR)/%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# F77 compilation rules
$(OBJ_DIR)/%_f77.o: $(SRC_DIR)/%.f
	$(FC) $(FFLAGS) -std=legacy -c $< -o $@

# Modern function compilation rules
$(OBJ_DIR)/%_modern.o: $(MODERN_DIR)/%_module.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Main program
$(OBJ_DIR)/validator.o: validator.f90 $(MODULE_OBJS)
	$(FC) $(FFLAGS) -c $< -o $@

# Dependencies
$(OBJ_DIR)/slatec_signatures_module.o: slatec_signatures_module.f90

$(OBJ_DIR)/validator_module.o: validator_module.f90 \
    $(OBJ_DIR)/error_analysis_module.o \
    $(OBJ_DIR)/numerical_utils_module.o \
    $(OBJ_DIR)/output_formats_module.o \
    $(OBJ_DIR)/runtime_detection_module.o \
    $(OBJ_DIR)/function_execution_module.o \
    $(OBJ_DIR)/validation_reporting_module.o

$(OBJ_DIR)/function_execution_module.o: function_execution_module.f90 $(MODERN_OBJS)

$(OBJ_DIR)/validation_reporting_module.o: validation_reporting_module.f90 \
    $(OBJ_DIR)/output_formats_module.o

$(OBJ_DIR)/function_dispatcher_module.o: function_dispatcher_module.f90 \
    $(OBJ_DIR)/slatec_signatures_module.o \
    $(OBJ_DIR)/validator_module.o \
    $(OBJ_DIR)/validation_reporting_module.o

clean:
	rm -rf $(OBJ_DIR) validator

.PHONY: clean