# Project Instructions

OCaml project managed with Dune. The rules below are directives, not suggestions.
When two rules conflict, prefer correctness and type-safety over expedience.

## How to write the code

Make illegal states unrepresentable.

- **Types first.** Reach for the type system before runtime checks. Model the
  domain with variants, records, and abstract types — not strings and ints.
- **Encode invariants in types.** If a value must be non-empty, ordered, or
  validated, give it a type that can only hold valid values. Construct that type
  once, at the boundary, then trust it everywhere downstream.
- **Parse, don't validate.** Turn unstructured input into structured types at
  the edges. Return the parsed type, not `bool`/`unit`. Never re-check a fact the
  type already guarantees.
- **Code reflects the spec.** The shape of the code (modules, signatures, types)
  should mirror the shape of the problem. A reader should infer the spec from the
  types.
- **Abstractions: weigh properties vs. power.** Prefer the least powerful
  construct that does the job; more constraints (fewer capabilities) yield
  stronger guarantees and easier reasoning. Add power only when a concrete need
  demands it.
- **Use proof-by-construction techniques.** Type witnesses, GADTs, phantom
  types, and type guards to make the compiler enforce invariants. Let
  construction be the proof of validity.
- **Fail loud, fail early.** On a broken invariant, stop immediately with an
  explicit, specific error message (what was expected, what was seen, where).
  Do not paper over edge cases with defensive hacks — fix the model instead.
- **Test by properties.** Prefer property-based and fuzz testing over
  example-based unit tests. Assert invariants over generated inputs; reserve
  unit tests for concrete regressions.
- **Match explicitly, not point-free.** Don't write `let f = function | ... ->
  ...`. Bind the argument and `match` on it: `let f x = match x with | ... ->
  ...`. Naming the scrutinee keeps both the binding and the body readable.
- **State the return type.** Every function makes its result type explicit:
  either in a signature (`val f : ... -> ret`) for exported functions, or as an
  annotation on the definition (`let f x : ret = ...`) for unexported helpers.
- **Seal modules with an inline signature.** Give a module its abstract/private
  interface with `module M : sig ... end = struct ... end`. Prefer this over a
  separate `.mli` and over the `include (struct ... end : sig ... end)` wrapper:
  the signature reads first, lives in one file, and exports no redundant names.

## How to work in this repo

- **Version control is `jj`, never `git`.** Use `jj` for every VCS operation in
  this repo. Do not run `git` commands here.
- **Branch freely.** Use sibling branches and `jj workspace` liberally to
  isolate parallel or experimental work.
- **Build/test/format with `dune` only.** Do not introduce alternate build,
  test, package, or task runners unless explicitly requested.
- **Track every dependency in `dune-project`.** When code starts (or stops)
  using a library, update the `depends` stanza in `dune-project` accordingly —
  not just the `(libraries ...)` field in a `dune` file. Then regenerate the
  opam file with `dune build` so `dune-project` stays the single source of
  truth for dependencies.
