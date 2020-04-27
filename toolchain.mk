# external parameters
DEBUG = 1
OPT_LEVEL = 2

# judge if is debug mode
ifeq ($(DEBUG), 0)
	C_DEBUG_ARG = -DNDEBUG
	C_OPT_ARG = -O$(OPT_LEVEL)
	YU_OPT_ARG = -O $(OPT_LEVEL)
else
	C_DEBUG_ARG = -g
	C_OPT_ARG = -O0
	YU_OPT_ARG = -O 0
endif

# compile toolchain prefix
LLVM_BIN := /usr/local/opt/llvm/bin

# Yu compiler
YUFLAGS := -Werror $(YU_OPT_ARG)
export YUC := $(BUILD_DIR)/yuc $(YUFLAGS)

# LLVM compiler
LLCFLAGS := $(C_OPT_ARG) -filetype=obj
export LLC := $(LLVM_BIN)/llc $(LLCFLAGS)

# linker
LDFLAGS :=
export LD := clang $(LDFLAGS)

# archiver
ARFLAGS := ru
export AR := ar $(ARFLAGS)

# ranlib
RANLIBFLAGS :=
export RANLIB := ranlib $(RANLIBFLAGS)
