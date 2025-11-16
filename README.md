# GCC 15.1 COBOL + musl Tests

Proof-of-concept tests for GCC 15.1's native COBOL compiler with musl libc, producing fully static binaries.

## Background

GCC 15.1 introduced native COBOL support via `gcobol`. These tests validate basic functionality when compiled with musl libc instead of glibc.

**Related:** [musl-cross-make PR #221](https://github.com/richfelker/musl-cross-make/pull/221)

## Requirements

You need `x86_64-linux-musl-gcobol` in your PATH. Build it using musl-cross-make with the COBOL patch applied.

```bash
export PATH=/path/to/musl-cross-make/output/bin:$PATH
```

To build the compiler yourself, apply the patch from PR #221 to musl-cross-make and configure with:
```makefile
TARGET = x86_64-linux-musl
GCC_VER = 15.1.0
GCC_CONFIG += --enable-languages=c,c++,cobol
```

## Usage

```bash
make        # Compile and run all tests
make build  # Compile without running
make clean  # Remove binaries
```

## Tests

- `01-hello.cob` - Hello World
- `02-arithmetic.cob` - Arithmetic operations (ADD, SUBTRACT, MULTIPLY, DIVIDE)
- `03-strings.cob` - String manipulation (STRING, INSPECT)
- `04-loops.cob` - PERFORM VARYING loops
- `05-fileio.cob` - Sequential file I/O
- `06-conditionals.cob` - IF-ELSE and EVALUATE statements

## Binary Characteristics

All test binaries are:
- Statically linked (no runtime dependencies)
- Fully portable across Linux x86-64 systems
- Approximately 4.6 MB unstripped, 672 KB stripped
- Built with musl libc

Verify static linking:
```bash
file 01-hello
# ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked
```

## Technical Notes

- Compiled with `-static-libgcc -static-libstdc++ -Wl,-static`
- Uses ISO COBOL 2023 standard features (not GnuCOBOL extensions)
- Requires musl-cross-make patch for `execinfo.h` compatibility
