# GCC 15.1 COBOL Test Suite
# Requires x86_64-linux-musl-gcobol in PATH

GCOBOL := x86_64-linux-musl-gcobol
COBOLFLAGS := -static-libgcc -static-libstdc++ -Wl,-static

# Test source files (in order)
TESTS := 01-hello 02-arithmetic 03-strings 04-loops 05-fileio 06-conditionals

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

# Generic rule to compile COBOL programs
%: %.cob
	@echo "Compiling $<..."
	@$(GCOBOL) $(COBOLFLAGS) -o $@ $<

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
	rm -f test-data.txt test-output.txt
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
