# Compiler and flags
COBC = cobc
GCC = gcc
CFLAGS = -x
LIBS = -lmicrohttpd -lcurl -lcJSON

# Source and output
SRC = gnucobol-microhttpd.cob
C_SRC = cJSON-wrapper.c
OUT = gnucobol-microhttpd

# Default target
all: $(OUT)

# Rule to build the program
$(OUT): $(SRC) $(C_SRC)
	$(GCC) -c $(C_SRC) -o cJSON-wrapper.o 
	$(COBC) $(CFLAGS) $(SRC) $(LIBS) cJSON-wrapper.o -o $(OUT) 

# Clean up build artifacts
clean:
	rm -f $(OUT)

