# Compiler and flags
COBC = cobc
CFLAGS = -x
LIBS = -lmicrohttpd -lcurl

# Source and output
SRC = gnucobol-microhttpd.cob
OUT = gnucobol-microhttpd

# Default target
all: $(OUT)

# Rule to build the program
$(OUT): $(SRC)
	$(COBC) $(CFLAGS) $(SRC) $(LIBS)

# Clean up build artifacts
clean:
	rm -f $(OUT)

