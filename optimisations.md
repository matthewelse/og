# Optimisations I've done/would like to do

- [x] "Packed SIMD" (in practice, no support for Neon, so just operating on
  i64s) style `memcmp`/`memchr` in OCaml
- [x] Boyer-Moore, with memchr for skipping characters
- [x] Boyer-Moore, not KMP 
- [x] Custom (zero-copy) `Buffered_reader` module, rather than `In_channel.
- [x] Unboxed `I64.t` ~everywhere on the hot path.
- [x] Thompson's 'lazy' NFA algorithm.
- [x] Bitmap for char matching 
- [x] Literal optimisations
    - [x] Use Boyer-Moore to look for candidate lines before using regex
      matching
    - [x] Just use memcmp if looking for a string literal at the start/end of a
      line.
    - [x] Just use Boyer-Moore if looking for a string literal in the middle of
      a line.
- [x] Use `Bigstring.t`, not `Bytes.t`.
    - I'm not 100% sure why this is faster. Maybe because it reduces the amount
      of work the GC has to do?
    - Maybe there's something to do with alignment that helps here too.
- [x] Wider SIMD (use Neon/SSE/AVX) for memchr/memcmp/memmem
- [ ] Aho-Corasick for multiple string matching
- [x] SIMD-accelerated string matching
  (https://github.com/BurntSushi/aho-corasick/blob/master/src/packed/teddy/README.md)
- [x] Aligned loads in `memchr`/`memcmp`, rather than 8 (!) single-byte loads.
  - [x] Teach the compiler that aarch64 allows unaligned loads (sometimes)
  - [ ] Try making the fast path prioritise aligned loads?
- [ ] ~Cache-line aligned bigstring allocations? This will involve writing a
  custom bigstring allocation stub.~
- [x] `[@@inline]` on `Slice.Make` (otherwise, we end up with a bunch of
  `caml_apply`s in surprising places).
- [x] Use tail-recursive functions, rather than loops (seems like the compiler
  is better at optimising these, and saves you from using `ref`s)
- [x] More recursive functions with loopify. Better than with_return.
- [x] Attempted to use open/read/write syscalls directly, rather than using the builtin stdlib.
      I thought this would make a big difference, but turns out not to be a big deal -- seems like
      string matching is the issue.
- [x] Hand-written Glob parser.
- [x] Do pre-filtering, then split by lines. This is a little tricky, because you have to get
      the book-keeping right for new lines.

