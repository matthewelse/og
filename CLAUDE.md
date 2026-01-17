# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`og` is a high-performance grep implementation in OCaml/OxCaml. The goal is pure, safe OCaml with performance rivaling ripgrep, using OxCaml features (unboxed types, SIMD, zero-alloc patterns) instead of C stubs.

## Build Commands

```bash
dune build                    # Build the project
dune build @runtest           # Run all tests (inline expect tests)
dune exec bin/main.exe        # Run the executable directly
dune exec og                  # Run the installed executable
dune clean                    # Clean build artifacts
```

## Architecture

### Module Structure

```
bin/main.ml          → Entry point, calls Og_driver.command
driver/              → CLI commands (search, parse)
lib/                 → Core regex engine
  ├── regex0.ml      → AST definition (Rule.t)
  ├── parser.ml      → Regex string parser
  ├── regex.ml       → Regex compilation and optimization
  ├── nfa_vm.ml      → Backtracking NFA matcher
  ├── nfa_hybrid.ml  → Lazy DFA (Thompson's algorithm)
  ├── literal.ml     → String literal fast-path (Boyer-Moore)
  ├── charset.ml     → Character set bitset
  └── og.ml          → Public interface
mio/                 → Zero-copy I/O layer
  ├── slice.ml       → Bigstring view (pos + len)
  ├── buffered_reader.ml → Zero-copy file reading
  └── fd.ml          → File descriptor wrapper
utils/               → Bitset, charmap utilities
```

### Key Design Patterns

**Dual NFA Backend**: `Implementation.t` selects between:
- `Nfa_vm`: Backtracking with state stack
- `Nfa_hybrid`: Lazy DFA (compiles states on demand)

**Literal Optimization**: Before full regex matching, recognize if the pattern is:
- String literal → Boyer-Moore with memchr skipping
- `^literal` or `literal$` → Direct memcmp
- Contains literal → Prefilter then match

**Zero-Copy I/O**: `Slice.t` is a view into a bigstring. `Buffered_reader` maintains a buffer and returns slices without copying.

### Hot Path Conventions

- Use `I64.t` (unboxed int64) throughout matching loops
- `[@zero_alloc]` annotations on performance-critical functions
- `let mutable` for loop counters instead of refs
- Tail-recursive functions with `[@tail_mod_cons]` or `[@loopify]`
- SIMD intrinsics for memchr/memcmp (see `simd/`)

## Testing

Tests use Jane Street's inline expect tests:
```ocaml
let%expect_test "name" =
  (* test code *)
  [%expect {| expected output |}]
```

Run with `dune build @runtest`. Test files are in `test/`.

## OxCaml Features Used

This codebase uses advanced OxCaml features. Reference `.claude/skills/oxcaml/SKILL.md` for syntax.

Key patterns:
- `#42L` for unboxed int64, `I64.O.(x + y)` for arithmetic
- `@ local` parameters for stack allocation
- `let mutable x = 0` for unboxed mutable locals
- `[@zero_alloc]` for compile-time allocation checking

## Key Libraries

- `core` / `core_unix`: Jane Street standard library
- `mio`: Custom I/O (in-repo)
- `vec`: Dynamic vectors
- `camlzilla`: C bindings for memmem/memchr
- `ppx_for_loop`, `ppx_template`, `ppx_og`: Custom preprocessors
