include toolchain.mk

# helper functions
rwildcard = $(foreach d, $(wildcard $(1:=/*)), $(call rwildcard, $d, $2) $(filter $(subst *, %, $2), $d))

# directories
TOP_DIR := $(shell if [ "$$PWD" != "" ]; then echo $$PWD; else pwd; fi)
OBJ_DIR := $(BUILD_DIR)/obj
LIB_DIR := $(TOP_DIR)/lib
EXAMPLES_DIR := $(TOP_DIR)/examples
EXAMPLES_OBJ_DIR := $(OBJ_DIR)/examples
EXAMPLES_BIN_DIR := $(BUILD_DIR)/examples

# source & target of library
LIB_SRC := $(call rwildcard, $(LIB_DIR), *.yu)
LIB_OBJ := $(patsubst $(TOP_DIR)/%.yu, $(OBJ_DIR)/%.yu.o, $(LIB_SRC))
LIB_TARGET := $(BUILD_DIR)/libyu.a

# source & target of examples
EXAMPLES_SRC := $(call rwildcard, $(EXAMPLES_DIR), *.yu)
EXAMPLES_OBJ := $(patsubst $(TOP_DIR)/%.yu, $(OBJ_DIR)/%.yu.o, $(EXAMPLES_SRC))
EXAMPLES_TARGET := $(patsubst $(EXAMPLES_OBJ_DIR)/%.yu.o, $(EXAMPLES_BIN_DIR)/%, $(EXAMPLES_OBJ))

.SILENT:
.PHONY: all clean libyu examples

all: libyu examples

clean:
	-rm -rf $(OBJ_DIR)
	-rm $(LIB_TARGET)
	-rm -rf $(EXAMPLES_BIN_DIR)

libyu: $(LIB_TARGET)

examples: $(EXAMPLES_BIN_DIR) $(EXAMPLES_TARGET)

$(EXAMPLES_BIN_DIR):
	mkdir $@

$(LIB_TARGET): $(LIB_OBJ)
	$(info making Yu standard library)
	$(AR) $@ $^
	$(RANLIB) $@

$(EXAMPLES_BIN_DIR)/%: $(EXAMPLES_OBJ_DIR)/%.yu.o $(LIB_TARGET)
	$(info making example "$(notdir $@)"...)
	$(LD) -L$(BUILD_DIR) -lyu -o $@ $<

$(OBJ_DIR)/%.yu.ll: $(TOP_DIR)/%.yu $(YUC_BIN)
	$(info YUC $@)
	-mkdir -p $(dir $@)
	$(YUC) -I $(LIB_DIR) -ot llvm $< > $@

$(OBJ_DIR)/%.o: $(OBJ_DIR)/%.ll
	$(info LLC $@)
	$(LLC) $^ -o $@
