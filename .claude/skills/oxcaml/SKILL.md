---
name: oxcaml-development
description: Use when writing applications in OxCaml, using modes (local, portable, contended), unboxed types (float#, int32#, I64, I32, F64, UFO), SIMD vectors, stack allocation, immutable arrays, labeled tuples, zero_alloc annotations, or ppx_optional. Use for high-performance OCaml code, parallel programming, or memory-efficient data structures.
---

# OxCaml Application Development

OxCaml is a performance-focused extension of OCaml with fearless concurrency, memory layout control, and allocation management. All valid OCaml programs are valid OxCaml programs.

**Documentation:** https://oxcaml.org/documentation/

## Modes

Modes describe how values are *used* (types describe what values *are*). Modes enable data-race freedom and stack allocation.

### Locality (Stack Allocation)

```ocaml
(* local values can be stack-allocated, global values must be heap-allocated *)
let f (x @ local) = ...           (* local parameter - won't escape *)
val f : 'a @ local -> unit        (* type signature *)

let x = stack_ { foo; bar }       (* force stack allocation *)
let f a = exclave_ ...            (* return stack-allocated value to caller's region *)
type 'a t = { g : 'a @@ global }  (* field always heap-allocated *)
```

Stack allocation reduces GC pressure. Local values cannot escape their region (function body).

### Portability and Contention (Parallelism)

```ocaml
(* portable: can cross thread boundaries; nonportable: cannot *)
val f : t @ contended -> float @@ portable   (* @ for params, @@ for returns *)

(* contended: another thread may write; shared: read-only; uncontended: single thread *)
(* Mutable fields of contended values cannot be accessed *)
```

### Uniqueness and Linearity

```ocaml
(* unique: only one reference exists; aliased: multiple references may exist *)
(* once: can only be used once; many: can be used multiple times *)
```

## Unboxed Types

Unboxed types avoid pointer indirection and allocation. Use the **Unboxed** library for working with unboxed numbers.

**Dependency:** `unboxed`

**When to use unboxed integers:** Unboxed integers (`int64#`, `int32#`) generate significantly better code than OCaml's tagged `int` type - no tag bit manipulation, direct machine operations. Prefer them for performance-sensitive code. Tagged integers (`int`) are slightly more ergonomic (work with more polymorphic functions, can be used at module top-level, etc.) and are fine when performance isn't critical.

### Unboxed Numbers and the Unboxed Library

```ocaml
(* Types: float#, int32#, int64#, nativeint#, float32# *)
(* Layouts: float64, bits32, bits64, word, float32 *)

let pi = #3.14          (* float# *)
let x = #123l           (* int32# *)
let y = -#456L          (* int64# *)
let z = #789n           (* nativeint# *)

(* Canonical module aliases from Unboxed library *)
open Unboxed
(* F64 = Float_u, F32 = Float32_u, I64 = Int64_u, I32 = Int32_u, Iptr = Nativeint_u *)
(* UFO = Unboxed float option (packed), Pct = Percent_u *)

(* Conversion to/from boxed types *)
let unboxed : int64# = I64.of_int64 42L
let boxed : int64 = I64.to_int64 unboxed

(* Arithmetic - all operations are [@zero_alloc] *)
(* Use .O submodule for operators - opens fewer names into scope *)
let sum = I64.O.(#1L + #2L)
let product = I64.O.(#3L * #4L)
let quotient = I64.O.(#10L / #3L)      (* integer division *)
let remainder = I64.O.(#10L % #3L)     (* modulo *)
let power = I64.O.(#2L ** #10L)        (* exponentiation *)
let float_div = I64.O.(#10L // #3L)    (* returns float# *)

(* Bit operations *)
let bits = I64.(bit_and #0xFFL #0x0FL)
let shifted = I64.(shift_left #1L 4)
let popcnt = I64.popcount #0b10101L
let leading = I64.clz #0x00FFL         (* count leading zeros *)
let trailing = I64.ctz #0x0100L        (* count trailing zeros *)

(* Comparisons *)
let cmp = I64.O.(#5L > #3L)
let eq = I64.equal #1L #1L
let ord = I64.compare #1L #2L
let bounded = I64.between #5L ~low:#0L ~high:#10L
let clamped = I64.clamp_exn #15L ~min:#0L ~max:#10L

(* Power-of-2 operations *)
let next_pow2 = I64.ceil_pow2 #5L      (* #8L *)
let log2 = I64.floor_log2 #8L          (* 3 *)
let is_pow = I64.is_pow2 #8L           (* true *)

(* Rounding *)
let rounded = I64.round ~dir:`Nearest #7L
```

### Unboxed Arrays and Refs

```ocaml
(* Unboxed arrays - efficient storage without boxing *)
let arr : int64# array = I64.Array.create ~len:10 #0L
let () = I64.Array.set arr 0 #42L
let v = I64.Array.get arr 0

(* Unboxed refs - mutable unboxed values *)
let counter : I64.Ref.t = I64.Ref.create #0L
let () = I64.Ref.set counter #1L
let current = I64.Ref.get counter
```

### Unboxed Options (UFO - Packed Float Option)

```ocaml
(* UFO uses NaN to represent None - no allocation overhead *)
let some_val : UFO.t = UFO.some #3.14
let none_val : UFO.t = UFO.none ()
let is_present = UFO.is_some some_val
let value = UFO.value_exn some_val
let with_default = UFO.value none_val ~default:#0.0

(* Safe operations that propagate None *)
let result = UFO.O.(some #1.0 + some #2.0)   (* some #3.0 *)
let propagate = UFO.O.(some #1.0 + none ())  (* none *)

(* Convert NaN to None *)
let safe = UFO.of_float_nan_as_none #(Float.nan)  (* none *)
```

### ppx_optional - Zero-Alloc Pattern Matching

**Dependency:** `ppx_optional`

Use `ppx_optional` for zero-alloc pattern matching on unboxed option types. It transforms match expressions into efficient `is_none`/`unsafe_value` calls.

```ocaml
(* Specify the module path inline (preferred) *)
(* match%optional.Module for boxed, match%optional_u.Module for unboxed *)

(* Basic usage with unboxed float options *)
let[@zero_alloc] value_or_zero (t : UFO.t) =
  match%optional_u.Packed_float_option.Unboxed t with
  | None -> UFO.zero ()
  | Some x -> x

(* Multiple values *)
let[@zero_alloc] merge x y ~f =
  match%optional_u.Packed_float_option.Unboxed x, y with
  | None, None -> UFO.none ()
  | Some x, None -> x
  | None, Some y -> y
  | Some x, Some y -> f x y

(* Boxed Packed_float_option.t uses match%optional (no _u) *)
let hash (t : Packed_float_option.t) =
  match%optional.Packed_float_option t with
  | None -> none_hash
  | Some t -> Float.hash t

(* With local mode for stack-allocated values *)
let[@zero_alloc] value_local (t : UFO.t @ local) ~default =
  match%optional_u.Packed_float_option.Unboxed (t : _ @ local) with
  | None -> default
  | Some x -> x
```

**Key points:**
- `match%optional.Module` - for boxed optional types (e.g., `Packed_float_option.t`)
- `match%optional_u.Module` - for unboxed optional types (e.g., `UFO.t` / `float#`)
- Prefer inline module path over opening `Optional_syntax` globally
- Only `None` and `Some x` patterns allowed (no nested patterns)
- Or-patterns (`|`) work with `%optional` but NOT with `%optional_u`

### Percent_u

```ocaml
(* Unboxed percentage type *)
let pct = Pct.of_percentage #5.0       (* 5% *)
let mult = Pct.to_mult pct             (* #0.05 *)
let bp = Pct.of_bp #50L                (* 50 basis points = 0.5% *)

let applied = Pct.apply pct #100.0     (* #5.0 *)
let scaled = Pct.scale pct #2.0        (* 10% *)
```

### Layout Annotations

```ocaml
type ('a : immediate) t1              (* type variable with layout *)
type ('a : float64) t2
type t : float64                      (* type with layout *)

let f (type a : immediate) (x : a) = x
val f : ('a : float64). 'a -> 'a      (* universally quantified layout *)
```

### Unboxed Products (Tuples and Records)

```ocaml
(* Unboxed tuples - passed in registers, no heap allocation *)
let flip #(x, y, ~lbl:z) = #(~lbl:z, y, x)
type t = #(int * float# * lbl:string)

(* Unboxed records - flat layout, passed in registers *)
type t = #{ f : float# ; s : string }
let inc #{ f ; s } = #{ f = F64.O.(f + #1.0) ; s }

(* Field access uses .# syntax *)
let get_f t = t.#f                    (* projection with .# *)
let get_s t = t.#s

(* Construction uses #{ ... } *)
let make f s = #{ f; s }

(* No "with" syntax - must reconstruct all fields *)
let set_f t new_f = #{ f = new_f; s = t.#s }

(* Implicit unboxed records - t# auto-generated from t *)
type t = { i : int ; s : string }
let box : t# -> t = fun #{ i ; s } -> { i ; s }
```

**Layout considerations:** Unboxed records have product layouts (e.g., `value & bits64 & bits64`). This means:
- They cannot be used in standard `list` or `option` (which require `value` layout)
- Define custom sentinel-based option types when needed (similar to `UFO`)
- Use `ppx_template` for layout-polymorphic code (see below)

**Returning unboxed types from error branches:** `failwith` and similar functions return `'a` with `value` layout, which doesn't unify with unboxed return types. Use the `never_returns` pattern:

```ocaml
(* Problem: failwith returns 'a : value, but we need value & bits64 & bits64 *)
let slice_exn t ~pos ~len : Slice.t =
  if pos < #0L
  then failwith "bad pos"  (* Error: layout mismatch *)
  else ...

(* Solution: match on never_returns to satisfy the type checker *)
let slice_exn t ~pos ~len : Slice.t =
  if pos < #0L
  then (match failwith "bad pos" with (_ : never_returns) -> .)
  else ...
```

### Or_null (Unboxed Options)

```ocaml
type 'a or_null : value_or_null
(* Constructors: Null, This v *)
```

### Arrays of Unboxed Elements

```ocaml
let arr = [| #2l |]                   (* int32# array *)

(* Use [@layout_poly] for generic array operations *)
external[@layout_poly] get : ('a : any). 'a array -> int -> 'a = "%array_safe_get"
```

## ppx_template - Layout Polymorphism

**Dependency:** `ppx_template`

`ppx_template` enables layout-polymorphic code by generating specialized versions for different layouts at compile time (similar to C++ templates). Use it when you need functions, module types, or types that work with multiple layouts.

### Basic Usage

```ocaml
(* Define a template with layout parameter *)
let%template[@kind k = (value, bits64, value & bits64 & bits64)] identity
  (type a : k)
  (x : a)
  = x

(* Use with explicit layout annotation *)
let id_int = (identity [@kind value]) 42
let id_float = (identity [@kind bits64]) #3.14
```

### Module Types with Layout Parameters

```ocaml
(* Template module type - generates versions for each layout *)
module type%template [@kind k = (value, value & bits64 & bits64)] Stringy = sig
  type t : k
  val length : t -> int64#
  val get : t -> int64# -> char
end

(* Use in function with matching layout annotation *)
let%template[@kind k = (value, value & bits64 & bits64)] process
  (type s : k)
  (module S : Stringy with type t = s[@kind k])
  (input : s)
  = S.length input

(* Call with specific layout *)
let len = (process [@kind value]) (module String) "hello"
let len = (process [@kind value & bits64 & bits64]) (module Slice) slice
```

### When to Use ppx_template

- **First-class modules with unboxed types**: Standard first-class modules require `value` layout; templates let you use unboxed types
- **Generic array operations**: Work with arrays of any element layout
- **Polymorphic containers**: Build data structures that work with multiple layouts
- **Performance-critical code**: Get specialized codegen without runtime dispatch

### Key Points

- Each layout variant generates completely separate code (no runtime polymorphism)
- Layout list in `[@kind k = (...)]` defines which specializations to generate
- Use `[@kind layout]` at call sites to select the specialization
- Works with functions, module types, and type definitions

## SIMD Vectors

128-bit and 256-bit SIMD vector types with platform-specific intrinsics.

```ocaml
(* Prefer unboxed types (with #) - they live in XMM/YMM registers *)
(* 128-bit: int8x16#, int16x8#, int32x4#, int64x2#, float32x4#, float64x2# *)
(* 256-bit: int8x32#, int16x16#, int32x8#, int64x4#, float32x8#, float64x4# *)

(* Boxed types (without #) exist for when you need them in boxed containers *)
(* e.g., int8x16 option, int8x16 list - but prefer unboxed where possible *)

open Ocaml_simd_sse  (* or Ocaml_simd_neon for Arm *)

(* Intrinsics operate on unboxed types *)
let v : float32x4# = Float32x4.set 1.0 2.0 3.0 4.0
let v : float32x4# = Float32x4.sqrt v

(* Load from strings/bytes/arrays *)
let vec : int8x16# = Int8x16.String.get text ~byte:0

(* Compile-time constants with ppx_simd *)
let z : int32x4# = Int32x4.blend [%blend 0, 1, 0, 1] x y

(* Store in arrays efficiently *)
let arr : int8x16# array = [| vec |]
```

**Dependencies:** `ocaml_simd`, `ocaml_simd_sse` or `ocaml_simd_avx` (x86), `ppx_simd`

**Arm Neon:** Limited support exists via `ocaml_simd_neon`. The compiler provides Neon intrinsics, though library coverage is less complete than SSE/AVX. Many intrinsics can be manually exposed from the compiler's built-in primitives. You'll need to look through the compiler source code to know what these are.

**Boxed vs Unboxed:** Use unboxed types (`int8x16#`) by default. Unboxed vectors pass through registers and avoid heap allocation. Arrays of unboxed types work (`int8x16# array`), but boxed containers like `option` require boxed types (`int8x16 option`) since `int8x16# option` is not yet supported.

## Immutable Arrays

```ocaml
open Iarray.O

let arr : string iarray = [: "zero"; "one"; "two" :]
let first = arr.:(0)              (* projection with .: *)

(* Covariant: sub iarray :> super iarray when sub :> super *)
(* Can be stack-allocated via Iarray.Local *)
```

Available in `core` and `base`.

## Labeled Tuples

```ocaml
let result = ~sum:0, ~product:1
let (~sum, ~product) = result     (* pattern matching with punning *)

(* Type: sum:int * product:int *)

(* Partial/reordered patterns when type is known *)
let ~y, .. = lt                   (* extract just y *)

(* Partial labeling *)
type t = min:int * max:int * float
```

## Let Mutable

Unboxed mutable local variables (no ref allocation).

```ocaml
let triangle n =
  let mutable total = 0 in
  for i = 1 to n do
    total <- total + i
  done;
  total
```

**Restrictions:** Cannot escape scope, cannot be captured by closures, single variable only.

## Zero-Alloc Checking

Compile-time verification that functions don't allocate.

```ocaml
let[@zero_alloc] add x y = x + y              (* checked *)
let[@zero_alloc strict] f x = ...             (* strict: no allocs even on exception paths *)
let[@zero_alloc opt] f x = ...                (* only checked in optimized builds *)
let[@zero_alloc assume] log_error msg = ...   (* trusted, not checked *)

(* In signatures *)
val[@zero_alloc] f : int -> int

(* Module-wide *)
[@@@zero_alloc all]
let[@zero_alloc ignore] make x y = (x,y)      (* opt-out *)
```

**Enable:** `-zero-alloc-check all` for `opt` annotations.

## Parallel Programming

```ocaml
(* Fork/join - arguments must be portable *)
let a, b = Parallel.fork_join2 par
  (fun _par -> compute_a ())
  (fun _par -> compute_b ())

(* Parallel sequences *)
let seq = Parallel.Sequence.of_iarray arr
let sum = Parallel.Sequence.reduce par seq ~f:(+)

(* Atomics for shared mutable state *)
let counter = Atomic.make 0
let _ = Atomic.fetch_and_add counter 1

(* Capsules for protected mutable state *)
let data = Capsule.Data.create (fun () -> ref 0)
let mutex = Capsule.Mutex.create ()
```

## Kinds

Kinds classify types by layout and modal properties.

```ocaml
(* immutable_data: no functions/mutable fields, crosses portability+contention *)
(* mutable_data: no functions, crosses portability *)
(* value: standard OCaml, crosses nothing *)

type t : immutable_data    (* kind annotation *)
```

## Quick Reference

| Feature | Syntax | Purpose |
|---------|--------|---------|
| Stack alloc | `stack_ expr` | Force stack allocation |
| Local param | `(x @ local)` | Promise value won't escape |
| Exclave | `exclave_ expr` | Return to caller's region |
| Unboxed float | `#3.14 : float#` | No boxing overhead |
| Unboxed int | `#42L : int64#` | No boxing overhead |
| Unboxed tuple | `#(a, b, c)` | Pass in registers |
| Unboxed record | `#{ f = x }` | Flat memory layout |
| Unboxed field access | `r.#field` | Access unboxed record field |
| ppx_template | `let%template[@kind k = ...]` | Layout-polymorphic code |
| Template call | `(f [@kind layout])` | Select template specialization |
| Array project | `arr.(i)` | Standard |
| Iarray project | `arr.:(i)` | Immutable array |
| Iarray literal | `[: a; b; c :]` | Immutable array |
| Labeled tuple | `~lbl:v, other` | Named tuple element |
| Let mutable | `let mutable x = 0` | Unboxed mutable local |
| Zero alloc | `[@zero_alloc]` | Compile-time check |
| Layout | `('a : float64)` | Type variable layout |
| I64/I32/F64 | `I64.O.(x + y)` | Unboxed library ops |
| UFO | `UFO.some #1.0` | Unboxed float option |
| match%optional | `match%optional t` | Zero-alloc option match (boxed) |
| match%optional_u | `match%optional_u t` | Zero-alloc option match (unboxed) |

## Common Patterns

### Zero-Allocation Hot Path

```ocaml
open Unboxed

let[@zero_alloc] sum_floats (data : float# array) =
  let mutable acc = #0.0 in
  for i = 0 to Array.length data - 1 do
    acc <- F64.O.(acc + data.(i))
  done;
  acc

let[@zero_alloc] sum_int64s (data : int64# array) =
  let mutable acc = #0L in
  for i = 0 to Array.length data - 1 do
    acc <- I64.O.(acc + data.(i))
  done;
  acc
```

### Using Unboxed Refs for Accumulation

```ocaml
open Unboxed

let count_with_ref () =
  let counter = I64.Ref.create #0L in
  for i = 1 to 100 do
    I64.Ref.set counter I64.O.(I64.Ref.get counter + I64.of_int i)
  done;
  I64.Ref.get counter
```

### Unboxed Optional Results with ppx_optional

```ocaml
open Unboxed

(* Use UFO for optional float results without allocation *)
let[@zero_alloc] safe_divide x y =
  if F64.O.(y = #0.0) then UFO.none ()
  else UFO.some F64.O.(x / y)

(* Pattern match with match%optional_u - specify module path inline *)
let[@zero_alloc] apply_or_default opt ~f ~default =
  match%optional_u.Packed_float_option.Unboxed opt with
  | None -> default
  | Some x -> f x

let result = safe_divide #10.0 #2.0  (* UFO with #5.0 *)
let doubled = apply_or_default result ~f:(fun x -> F64.O.(x * #2.0)) ~default:#0.0
```

### Parallel Map with Immutable Results

```ocaml
let parallel_map par arr ~f =
  let seq = Parallel.Sequence.of_iarray arr in
  Parallel.Sequence.map par seq ~f
  |> Parallel.Sequence.to_iarray
```

### Stack-Allocated Temporary Data

```ocaml
let with_temp_buffer ~f =
  let buf = stack_ (Bytes.create 1024) in
  f (buf @ local)
```
