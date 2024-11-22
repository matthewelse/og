# `og`

`og` is an implementation of `grep` in OCaml. The goal is to write a
high-performance tool in pure, safe OCaml.

## Ideas 

- [x] Backtracking NFA implementation.
- [x] Hybrid NFA (lazy DFA) implementation.
- [x] Use SIMD directly from OCaml to remove the need for C stubs.
- [x] Full implementation of Boyer-Moore for string matching.
- [ ] Parallelize directory tree walking.
- [ ] Support for globs.
- [ ] Support for ignoring .gitignore'd files.
- [x] If matching "^{literal}" just do Slice.is_prefix, rather than full string matching.
