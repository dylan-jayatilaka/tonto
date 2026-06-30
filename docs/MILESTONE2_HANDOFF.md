# Milestone 2 handoff — ANTLR4 Foo→Fortran translator

Snapshot of progress on the `antlr4` branch as of 2026-06-21. The conversation
that produced this is recoverable with `claude --resume` (transcript:
`~/.claude/projects/-Users-dylan-tonto/10e89dc4-f85a-4aa0-b6d1-49804805dc80.jsonl`,
backed up to `~/tonto-antlr4-session-2026-06-21.jsonl`).

## Where things stand
- **Milestone 1 (parse): done.** All 185 `foofiles/*.foo` parse with 0 errors.
- **Milestone 2 (translate): substantial.** `scripts/FooToFortran.java` emits
  `<stem>.F90`/`.int`/`.use` into `antlr4-release/`. `irrep` matches `release/`
  exactly; 3 modules exact; last full sweep total diff ≈199k (≈180k excluding
  `womersley`, which is cosmetic-only — see below).

## Build / run / compare
```bash
scripts/build_translator.sh                 # generate parser + compile translator
scripts/build_translator.sh foofiles/irrep.foo   # build + translate one module
# full invocation: see CLAUDE.md §8
```
`release/` is a CMake build dir; its `.F90/.int/.use` are foo.pl reference output.
It was **regenerated from the current sources on 2026-06-21** (161/166 ok; the 5
`run_*` failures are pre-existing semantic errors in `runfiles/`, unrelated).

## What the translator handles
module rename + boilerplate; doc/section/signature comments + `#` PP lines
(hidden channel); module data section (type defs, module vars, `data`, explicit
`use`); declarations (reverse, arrays `VEC{T}`→`VEC(elem,:)`, STR len, ptr/alloc,
DEFAULT initialisers); functions; block control flow (`if/do/select`); `get_from`
inheritance incl. type-parameterised substitution + overload numbering; type-aware
resolution (`self`, components, per-proc locals, array-element typing on indexing,
qualified `MODULE:method`, submodule `.SET:proc`, intrinsic props `.dim`→`size`);
ENSURE precondition hoisting; `.int`/`.use` generation; skips `virtual module`
and `inlined_by_foo` procedures; external/kind types (MPI_*) kept verbatim.

## Known gaps / next steps (in rough priority)
1. **Global/module-variable types** (e.g. `stdout`,`stderr`,`std_time`): not
   tracked — `stdout.flush`→`call flush_(stdout)` is correct but the `.use`
   dependency (`use TEXTFILE_MODULE, only: flush_`) is missed; component access
   on a global is mis-rendered. Build a global-var table from `public`
   module-level decls.
2. **Function return types** for chains after a method call (`.f().comp`): not
   tracked (component-array-index chains DO work). Needs a global proc→result-type
   registry.
3. `shell2`/`shell1quartet` over-generation; `test_parallel` self-templates.
4. Cosmetic: one-line `if(cond)` spacing — sources normalized to `if (`; clears
   once `release/` is regenerated (done 2026-06-21).

## Source normalizations applied this session (maintainer-approved)
- Removed module-less `minimal.foo`; `parallel.foo` made `virtual module`;
  `parallel.foo`/`system_command.foo` dropped from `CMakeLists.txt`.
- `if(`/`result(`/`while(`/`elseif(` → spaced (`if (` …).
- `get_from` placeholders: UPPERCASE keys (except `EQ`) normalized to trailing
  `?` on both invoking (`KEY?=>`) and template (per-procedure body) sides.
  Script: `scripts/qmark_normalize.py`.

## Metric note
Ignore `womersley` (valid single-line `data` vs release's multi-line wrapping —
compiles identically). The diff sweep harness lives in the conversation; rerun by
translating all `foofiles/*.foo` and diffing (whitespace-insensitive) vs `release/`.
