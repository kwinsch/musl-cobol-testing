# GCC 15.1 COBOL Test Suite
# Requires x86_64-linux-musl-gcobol in PATH

GCOBOL := x86_64-linux-musl-gcobol
GCC := x86_64-linux-musl-gcc
STRIP := x86_64-linux-musl-strip
COBOLFLAGS := -static-libgcc -static-libstdc++ -Wl,-static
CFLAGS := -static

# Test source files (in order)
TESTS := 01-hello 02-arithmetic 03-strings 04-loops 05-fileio 06-conditionals 07-mixed 08-json

# Derived names
TEST_SOURCES := $(addsuffix .cob,$(TESTS))
TEST_BINARIES := $(TESTS)

.PHONY: all test clean check-compiler help

# Default target
all: check-compiler test

# Check if compiler is in PATH
check-compiler:
	@echo "Checking for x86_64-linux-musl-gcobol..."
	@which $(GCOBOL) > /dev/null || \
		(echo "ERROR: $(GCOBOL) not found in PATH"; \
		 echo "Add the musl-cross-make compiler directory to your PATH"; \
		 echo "  export PATH=/path/to/musl-cross-make/output/bin:\$$PATH"; \
		 exit 1)
	@$(GCOBOL) --version | head -1
	@echo "Compiler check: OK"
	@echo ""

# Compile all tests
build: $(TEST_BINARIES)

# Special rule for mixed C/COBOL program
07-mixed: 07-mixed.cob fileio_c.c
	@echo "Compiling C helper: fileio_c.c..."
	@$(GCC) $(CFLAGS) -c fileio_c.c -o fileio_c.o
	@echo "Compiling mixed COBOL/C program..."
	@$(GCOBOL) $(COBOLFLAGS) -o $@ 07-mixed.cob fileio_c.o
	@echo "Stripping $@..."
	@$(STRIP) $@

# Special rule for JSON parser example
08-json: 08-json.cob json_parser.c
	@echo "Compiling JSON parser: json_parser.c..."
	@$(GCC) $(CFLAGS) -c json_parser.c -o json_parser.o
	@echo "Compiling JSON/COBOL program..."
	@$(GCOBOL) $(COBOLFLAGS) -o $@ 08-json.cob json_parser.o
	@echo "Stripping $@..."
	@$(STRIP) $@

# Generic rule to compile COBOL programs
%: %.cob
	@echo "Compiling $<..."
	@$(GCOBOL) $(COBOLFLAGS) -o $@ $<
	@echo "Stripping $@..."
	@$(STRIP) $@

# Run all tests
test: build
	@echo "========================================="
	@echo "Running GCC 15.1 COBOL Test Suite"
	@echo "========================================="
	@passed=0; failed=0; \
	for test in $(TEST_BINARIES); do \
		echo ""; \
		echo "Running $$test..."; \
		echo "-----------------------------------------"; \
		if ./$$test; then \
			echo "✓ $$test PASSED"; \
			passed=$$((passed + 1)); \
		else \
			echo "✗ $$test FAILED"; \
			failed=$$((failed + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "========================================="; \
	echo "Test Results: $$passed passed, $$failed failed"; \
	echo "========================================="; \
	if [ $$failed -gt 0 ]; then exit 1; fi

# Clean compiled binaries and temporary files
clean:
	@echo "Cleaning test directory..."
	rm -f $(TEST_BINARIES)
	rm -f test-data.txt test-output.txt sales-data.txt processed.txt customers.json
	rm -f *.o
	@echo "Clean complete"

# Show help
help:
	@echo "GCC 15.1 COBOL Test Suite"
	@echo ""
	@echo "Targets:"
	@echo "  make          - Check compiler and run all tests"
	@echo "  make test     - Build and run all tests"
	@echo "  make build    - Compile all tests without running"
	@echo "  make clean    - Remove compiled binaries"
	@echo "  make help     - Show this help message"
	@echo ""
	@echo "Requirements:"
	@echo "  - x86_64-linux-musl-gcobol must be in PATH"
	@echo "  - Add to PATH with:"
	@echo "      export PATH=/path/to/musl-cross-make/output/bin:\$$PATH"
	@echo ""
	@echo "Tests:"
	@for test in $(TEST_SOURCES); do \
		echo "  - $$test"; \
	done
