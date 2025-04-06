# Compiler and flags
COBC = cobc
COBC_FLAGS = -x
GCC = gcc
GCC_FLAGS = -c
LIBS = -lmicrohttpd -lcurl -lcJSON

# Source and output
COB_FILES = gnucobol-microhttpd.cob http/curl-write-callback.cob
C_FILES = cJSON-wrapper.c
COB_OBJ_FILES = $(COB_FILES:.cob=.o)
C_OBJ_FILES = $(C_FILES:.c=.o)
OUT = gnucobol-microhttpd

# Default target
all: $(OUT)

# Rule to build the program
$(OUT): $(COB_FILES) $(C_FILES)
	$(GCC) $(GCC_FLAGS) $(C_FILES)
	$(COBC) $(COBC_FLAGS) $(COB_FILES) -o $(OUT) $(C_OBJ_FILES) $(LIBS)

# Clean up build artifacts
clean:
	rm -f $(OUT) $(COB_OBJ_FILES) $(C_OBJ_FILES)

