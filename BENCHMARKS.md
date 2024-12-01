# Benchmarks

## String matching

This example is running a version of `og` that just does string matching, on
large chunks of files at a time.

```bash
➜  linux git:(rust) ✗ hyperfine '/Users/melse/Development/og/_build/default/bin/main.exe "INCLUDE"' 'rg -q --stats "INCLUDE"' 'rg -q --stats -j1 "INCLUDE"' --warmup 5
Benchmark 1: /Users/melse/Development/og/_build/default/bin/main.exe "INCLUDE"
  Time (mean ± σ):      2.214 s ±  0.023 s    [User: 0.641 s, System: 1.568 s]
  Range (min … max):    2.201 s …  2.273 s    10 runs

  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet system without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.

Benchmark 2: rg -q --stats "INCLUDE"
  Time (mean ± σ):      2.885 s ±  0.162 s    [User: 0.390 s, System: 19.548 s]
  Range (min … max):    2.579 s …  3.117 s    10 runs

Benchmark 3: rg -q --stats -j1 "INCLUDE"
  Time (mean ± σ):      1.597 s ±  0.003 s    [User: 0.218 s, System: 1.379 s]
  Range (min … max):    1.594 s …  1.605 s    10 runs

Summary
  rg -q --stats -j1 "INCLUDE" ran
    1.39 ± 0.01 times faster than /Users/melse/Development/og/_build/default/bin/main.exe "INCLUDE"
    1.81 ± 0.10 times faster than rg -q --stats "INCLUDE"
```

### Analysis

- Interestingly, we don't gain much from being multi-threaded. Maybe if the
  matching work is harder (e.g. full DFA matching), you benefit more from
  mulithreading?

#### `og`

Looking at the trace from `samply record`:

- We spend about 11% of the time actually doing string matching (~243ms). We
  spend ~5% of the time in `is_probably_binary` (~110ms).
- We spend ~32% of the time doing `open` syscalls (~708ms).
- We spend ~40% of the time doing `read` syscalls (~800ms).

If we don't do anything about the syscalls, that leaves us with ~100ms to do
string matching on 1,146,344,301 bytes (~10GB/s). We're currently managing
4.7GB/s.

#### single-threaded ripgrep

- Spends ~55% of its time doing `read` syscalls (~878ms).
- hmm, maybe it's even 73%?! (~1.16s).
- using `memchr::memmem::searcher::searcher_kind_neon` for string searching.
