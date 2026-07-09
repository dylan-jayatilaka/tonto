# The Foo Language

Foo is the custom object-oriented preprocessor language in which the scientific
code of **Tonto** (a quantum-chemistry / crystallography package) is written.
Each `*.foo` source is translated to modern Fortran (95 / 2003+) and then
compiled.

**At the expression and statement level, Foo is essentially modern Fortran** —
the same operators, intrinsics, `if`/`do`/`select case` control flow, array
syntax and `//` string concatenation. What Foo adds on top is a light
object-oriented layer (modules-as-classes with an implicit `self`, dot-method
call notation, generic/overloaded procedures, and template inheritance via
`get_from`), a compact type notation for parameterised arrays, and a set of
C-preprocessor macros (assertions, memory tracking, MPI parallelism).

Stylistically the array/type notation (`VEC{REAL}`, `MAT{T}`, element access
written as a *type with an `element` component*) is reminiscent of the defunct
**Sather** language and of **Julia** — both of which influenced how Foo writes
parameterised array types in text.

This document describes the language as accepted by the **new ANTLR4 translator**
(`foogrammar/Foo.g4` + `foogrammar/FooToFortran.java`), and notes where it
deliberately differs from the legacy Perl translator (`scripts/foo.pl`).

---

## Contents

1. [Translators and how to run them](#1-translators-and-how-to-run-them)
2. [Source layout and conventions](#2-source-layout-and-conventions)
3. [Lexical conventions](#3-lexical-conventions)
4. [The type system](#4-the-type-system)
5. [Declarations and attributes](#5-declarations-and-attributes)
6. [Modules and submodules](#6-modules-and-submodules)
7. [Procedures](#7-procedures)
8. [Generic interfaces and overloading](#8-generic-interfaces-and-overloading)
9. [`get_from` template inheritance](#9-get_from-template-inheritance)
10. [Statements and control flow](#10-statements-and-control-flow)
11. [Expressions, calls and the dot/percent selectors](#11-expressions-calls-and-the-dotpercent-selectors)
12. [Parallelism (`parallel do`, MPI)](#12-parallelism-parallel-do-mpi)
13. [Assertions and other C macros](#13-assertions-and-other-c-macros)
14. [`use` / `USE` and the `.use` mechanism](#14-use--use-and-the-use-mechanism)
15. [Foo → Fortran conversion summary](#15-foo--fortran-conversion-summary)
16. [Caveats, edge cases and known `foo.pl` bugs](#16-caveats-edge-cases-and-known-foopl-bugs)
17. [Grammar structure (`Foo.g4`)](#17-grammar-structure-foog4)
18. [References](#18-references)

---

## 1. Translators and how to run them

There are two translators producing the same Fortran:

| | Path | Status |
|---|---|---|
| **Legacy** | `scripts/foo.pl` (Perl) | The historical reference; its output is the `release/` snapshot. **It can no longer be run** — it is kept only as the behavioural reference to reproduce. |
| **New** | `foogrammar/Foo.g4` (ANTLR4 grammar) + `foogrammar/FooToFortran.java` | The current translator. Parses with an ANTLR4-generated parser and walks the parse tree to emit Fortran. |

### Output files

For each `module.foo` the translator emits three files:

- `module.F90` — the Fortran source. The file stem is *underscored*: `vec{real}.foo` → `vec_real.F90`; submodule dots are kept (`molecule.grid.F90`).
- `module.int` — the module's generic interface blocks.
- `module.use` — `use` statements for procedures pulled in from *other* modules.

`module.int` and `module.use` keep the brace form of the name (`vec{real}.int`)
and are `#include`d into the `.F90` **by the C preprocessor at compile time**.
Consequently the translator output (and the `release/` reference) is **pre-CPP**:
the macros in `include/macros.in` and the `#include` directives are left intact
for the Fortran build to expand. Reproducing CPP/macro expansion is *not* part of
translation.

### Running the new translator

It runs **automatically as part of the CMake build** (the `antlr4-fortran`
target generates the parser, compiles the translator, and runs it over every
`foofiles/*.foo` into the build tree). Manually:

```bash
# generate the parser + compile the translator (into build/translator/)
scripts/build_translator.sh

# translate one module into antlr4-release/
scripts/build_translator.sh foofiles/irrep.foo
```

Equivalent explicit invocation:

```bash
JAR=/usr/local/lib/antlr-4.13.2-complete.jar
( cd foogrammar && java -cp "$JAR" org.antlr.v4.Tool -visitor -o ../build/translator/gen Foo.g4 )
javac -cp "$JAR" -d build/translator/classes build/translator/gen/*.java foogrammar/FooToFortran.java
java -cp "$JAR:build/translator/classes" FooToFortran \
     --types foofiles/types.foo --foo foofiles/irrep.foo --out-dir antlr4-release
```

`types.foo` **must** be supplied (`--types`) so the derived-type table is built
before any module is translated (see §4) — the translator needs to know every
type's components to resolve `.component` access.

---

## 2. Source layout and conventions

- Foo sources live in `foofiles/` (`*.foo`). Every file is one module.
- **All derived types are declared in `foofiles/types.foo`** (see §4). A module
  file `atom.foo` defines the *procedures* of type `ATOM`; the *components* of
  `ATOM` live in `types.foo`.
- The grammar and translator live in `foogrammar/`.

### Three-space indentation

The conventional indentation unit is **3 spaces**, and a new scope is opened by a
block keyword (`module`, a procedure header, `interface`, `if`, `do`, …) and
closed by `end`. Note however that **the grammar is whitespace-insensitive**:
block structure is determined by the `end` keywords, not by indentation. Real
sources indent somewhat inconsistently; 3 spaces is the house style, not a rule
the parser enforces. `contains` sits in column 0.

### Documentation comments

By convention a `!` comment describing a procedure or a type component comes
**immediately after** the procedure header (or the component declaration), as the
first line(s) of its body:

```foo
n_items result (res) :: pure
   ! Return the number of items in the string
   self :: IN
   res  :: INT
   ...
end
```

```foo
   cpu_start_time :: REAL, readonly  DEFAULT(ZERO)
   ! Contains CPU start time, in seconds
```

The translator preserves these (including trailing inline comments after a
statement), since they are useful to the human reader.

---

## 3. Lexical conventions

- **Comments**: `!` to end of line, standalone or trailing.
- **Statement separation**: newline; `;` separates several statements on one line.
- **Continuation**: a line ending in `&` continues on the next.
- **Constants**: `TRUE`, `FALSE`, `ZERO`, `ONE`, `NULL`.
- **Array constructors**: `[ ... ]` (square brackets), e.g. `["'",'"',"{"]`.

### Case sensitivity — the important subtleties

Foo was historically described as "case-insensitive keywords, identifier case
preserved", but this is **not quite true**, and several cases carry meaning:

- **Control / structure keywords** — `module, interface, contains, implicit,
  none, result, get_from, if, then, while, where, forall, else, select, case,
  do, exit, cycle, return, data, end` — are written **lowercase** (the grammar
  now accepts only the lowercase spelling).
- **Type and intent keywords** are **uppercase**: `INT, REAL, CPX, BIN, STR`,
  `VEC, MAT, MAT3…MAT7`, `OBJECT`, `INTRINSIC`, and `IN, OUT, INOUT`.
- **Operators** are written **uppercase** as words: `AND, OR, NOT, EQV, NEQV,
  EQ, NE` (the `.and.`/`.or.`/… spellings are also accepted).
- **`pure` vs `PURE`, `elemental` vs `ELEMENTAL`** — *case is semantic* (see §7):
  lowercase ones are real Fortran keywords; uppercase ones are C macros.
- **`DEFAULT` vs `default`** — `DEFAULT(x)` is a C macro (a component initialiser,
  uppercase); `case default` is the lowercase control keyword.
- **`use` vs `USE`** — a readability convention (see §14): `USE` for external
  Fortran modules, lowercase `use` for the auto-generated repo dependencies.

---

## 4. The type system

### Primitive (intrinsic) types

| Foo | Fortran |
|---|---|
| `INT` | `integer(INT_KIND)` |
| `REAL` | `real(REAL_KIND)` (double precision) |
| `CPX` | `complex(CPX_KIND)` |
| `BIN` | `logical` |
| `STR` | `character(len=…)` |

### Parameterised array types — `{ … }`

Array element type goes in braces; array rank is encoded in the head name:

- `VEC{T}` — 1-D, `MAT{T}` — 2-D, `MAT3{T}` … `MAT7{T}` — 3-D … 7-D.
- Nestable: `VEC{VEC{REAL}}`, `MAT{EVEC{INT}}`.
- `MAP{KEY,VAL}` — a map parameterised by two types.

```foo
v      :: VEC{REAL}
matrix :: MAT{INT}
tensor :: MAT3{CPX}
```

Dimensions and length parameters go in `( … )` *after* the braces:

```foo
s :: STR(len=256)        ! a 256-char string
v :: VEC{STR}(len=1,6)   ! 6 strings, each length 1  ->  VEC(STR(len=1),6)
m :: MAT{REAL}(3,4)      ! 3x4 real matrix           ->  MAT(REAL,3,4)
```

A leading `len=` in the parameter list belongs to a **`STR` element only**; for a
non-`STR` element it does not apply and is dropped (`VEC{INT}(len=…)` →
`VEC(INT,:)`).

### Derived types — declared in `types.foo`

Every derived type, and its components, is declared in `foofiles/types.foo`:

```foo
type ATOM
   start_time5 :: VEC{INT}(5), readonly  DEFAULT(0)
   ! Real start time, in Julian day,h,m,s,ms
   cpu_start_time :: REAL, readonly  DEFAULT(ZERO)
   ! CPU start time, in seconds
end
```

- `readonly` — the component may not be *assigned* outside the defining module
  (it may still be read via dot notation).
- `private` — the component may not even be *referenced* by dot notation outside
  the defining module.
- `DEFAULT(x)` — a C macro giving the component initialiser (`= x` in Fortran).

`types.foo` must be processed first so the translator knows the components of
every type when resolving `obj.component` access.

### Arrays of arrays, and element access

An "array of arrays" is really an array of a one-component derived type whose
single component (named `element`) is the inner array:

```foo
type EVEC{REAL}
   element :: VEC{REAL}@      ! the encapsulated vector
end
```

So `VEC{EVEC{REAL}}` is an array of `EVEC{REAL}`. Such "array-of-array" types are
also declared in `types.foo` purely to inform the translator; they **emit no
Fortran**.

Element access has a shorthand that avoids writing `%element` — `a(i)[j]`:

```foo
nested :: VEC{VEC{REAL}}@
nested.create(3,4)
val = nested(i)%element(j)
val = nested(i)[j]            ! identical:  a(i)[j]  ->  a(i)%element(j)
nested.destroy
```

(`create`/`destroy` are the conventional allocate/deallocate methods.)

### Pointer (`*`) and allocatable (`@`) suffixes

```foo
ptr :: INT*          ! pointer        (equivalent to , POINTER  — rare)
arr :: VEC{REAL}@    ! allocatable    (equivalent to , ALLOCATABLE)
```

---

## 5. Declarations and attributes

### Reverse declarations

The defining surface feature: the **name comes before the type**, opposite to
Fortran.

```foo
i      :: INT                 ! Fortran:  integer :: i
matrix :: MAT{REAL}           ! Fortran:  ...     :: matrix
```

### Variable attributes (after the type, comma-separated)

```foo
x    :: INT, IN
y    :: REAL, OUT
z    :: STR, INOUT
flag :: BIN, private
arr  :: VEC{REAL}, ALLOCATABLE     ! or the @ shorthand
```

| Attribute | Meaning |
|---|---|
| `IN` / `OUT` / `INOUT` | argument intent |
| `PRIVATE` | private visibility |
| `READONLY` | read-only component (assignment only inside the defining module) |
| `POINTER` | pointer — prefer the `*` shorthand |
| `TARGET` | may be a pointer target |
| `SAVE` | static / saved |
| `ALLOCATABLE` | allocatable — prefer the `@` shorthand |
| `OPTIONAL` | optional argument |

### Initialisers

```foo
letters :: STR(len=52) = "abc…XYZ"
opening :: VEC{STR}(len=1,6) = ["'",'"',"{","(","[","<"]
coeffs  :: VEC{REAL}(0:6) = [1.0d0, 76.18d0, -86.50d0, …]
```

---

## 6. Modules and submodules

A module is a class:

```foo
module STR
   implicit none

   opening :: VEC{STR}(len=1,6) = ["'",'"',"{","(","[","<"]   ! module data

   interface trim
      trim_blanks_from_end
   end

contains
   ! procedure definitions
end
```

The file name maps to the Fortran module name: `str.foo` → `STR_MODULE`.

### Submodules

A large class may be split across files. `molecule.base.foo` declares
`module MOLECULE.BASE`, a submodule of `MOLECULE` (file-name head = lowercase
type name). Submodule-qualified calls are described in §11.

**A submodule-split type has no base module.** Each submodule file becomes its
own Fortran module — `MOLECULE.MAIN` → `MOLECULE_MAIN_MODULE`, `MOLECULE.GRID` →
`MOLECULE_GRID_MODULE`, and so on. **Nothing declares a plain `module MOLECULE`,
so there is no `MOLECULE_MODULE`.** (The derived type itself, `MOLECULE_TYPE`,
lives in `types.foo`/`TYPES_MODULE`, so `type(MOLECULE_TYPE)` is always available;
it is only the *procedure* home module that is split.)

Consequently, a method call on a `MOLECULE` object — `.make_ED_grid`, or
`mol.make_fock` — must `use` the *submodule* that actually defines it, not a base
module. The translator scans every `molecule.*.foo` file up front to build a
`method → submodule` registry:

- defined in the **current** submodule → no `use` (a same-module call);
- defined in **another** submodule → `use MOLECULE_XXX_MODULE, only: method_`;
- a call whose home can't be found falls back to `<TYPE>_MODULE` — which, for a
  split type, does not exist, so an unresolved `use MOLECULE_MODULE` is the tell
  that a method was mis-resolved.

`.MAIN:proc` therefore resolves to `MOLECULE_MAIN_MODULE` (the `molecule.main.foo`
submodule), **not** a base `MOLECULE_MODULE`.

### Main programs — `runfiles/run_XXX.foo`

The executables' entry points live in `runfiles/`, are always named `run_XXX.foo`,
and begin with `program <NAME>` (not `module`):

```foo
program run_HAR
   implicit none

   default_basis :: STR  DEFAULT("def2-SVP")
   m            :: MOLECULE@
   ...
   TONTO_CREATE
   ...
   MOLECULE.MAIN:cleanup
   TEXTFILE:destroy(stdout)
   TONTO_DESTROY
end
```

A program is a body of declarations and **executable statements** (like a
procedure body) ending in a bare `end`; it has **no `contains`** and no internal
procedures. It is emitted as a real Fortran main program (`program NAME … end
program`, with its own `_main` so the executable links) rather than a module. The
CMake targets rename the outputs: `run_molecule` → **tonto**, `run_har` → **hart**,
`run_rgbi` → **rgbi**.

Because a program is a *consumer* of the modules (which live with `types.foo`), the
translator derives its module registries from the `--types` file's directory, not
the input file's directory — otherwise a `run_XXX.foo` in `runfiles/` would look
for `MOLECULE`'s submodules in the wrong place.

---

## 7. Procedures

A procedure header is the name, optional `(args)`, optional `result (res)`, then
attributes after `::`. Functions use `result (...)`; subroutines do not.

```foo
n_items result (res) :: pure          ! a function
   ! Return the number of items
   self :: IN
   res  :: INT
   ...
end

multiply(factor) :: pure              ! a subroutine
   ! Multiply self by factor
   self   :: INOUT
   factor :: REAL, IN
   ...
end
```

### `self`

Every ordinary procedure has an implicit first argument `self`, of the module's
type. Its intent is declared (`IN` for functions, usually `INOUT` for mutating
subroutines). A `selfless` procedure has no `self`.

### Procedure attributes (after `::`)

| Attribute | Meaning |
|---|---|
| `pure` / `elemental` | **Fortran** keywords — the procedure must be side-effect free (see the assertion note below) |
| `PURE` / `ELEMENTAL` | **C macros** (uppercase) — expand at compile time; *not* subject to the Fortran purity constraint |
| `get_from(MODULE, …)` | inherit the body from a template (see §9) |
| `selfless` | no implicit `self` argument |
| `routinal` / `functional` | the first argument is a *procedure* (with an explicit interface), not a `self` variable |
| `leaky` | the procedure is allowed to leave memory allocated (suppresses leak checking) |
| `public` / `private` | visibility |

> **Assertions in `pure`/`elemental` (lowercase):** because `ENSURE`/`DIE`/`WARN`
> expand to error-message calls (`call ensure_(tonto,…)`, a side effect), they are
> **illegal** in a true Fortran `pure`/`elemental` procedure — gfortran reports
> "no specific subroutine for the generic `ensure_`" (the non-pure specific is
> excluded in a pure context). The lowercase attribute is *always* applied (it is a
> real Fortran keyword); the **uppercase** `PURE`/`ELEMENTAL` are C macros that a
> debug build switches off, so their assertions are always legal. The translator
> therefore **emits any assertion in a lowercase `pure`/`elemental` procedure
> commented out** (`! ENSURE…`, kept as documentation) — including ones inherited
> from a template — and keeps them live everywhere else. This is exactly why the
> case is significant.

### Procedure arguments that are themselves procedures

If an argument is a procedure, its calling interface is declared with an
`interface` block (`routinal`/`functional` mark which argument is the procedure):

```foo
line_search(dself,alphamax,x,p,c1,c2,b) :: routinal, public
   ! ... self and dself are functions ...
   interface
      self(x,res)
         x   :: VEC{REAL}, IN
         res :: REAL, OUT
      end
   end
   interface
      dself(x,res)
         x   :: VEC{REAL}, IN
         res :: VEC{REAL}, OUT
      end
   end
   x  :: VEC{REAL}, IN
   ...
end
```

---

## 8. Generic interfaces and overloading

Several procedures with the same generic name are grouped in an `interface`:

```foo
interface to_str
   to_str_int_0
   to_str_int_1
   to_str_int_2
end
```

The translator numbers overloads `name_0`, `name_1`, … and emits a generic
interface `name_` selecting among them. A *generic* call uses the bare/`:` form;
a *non-generic* call (`::`) names a specific procedure.

**Two kinds of explicit `interface NAME … end` block** (declared in the module's
data section, before `contains`):

- **Multi-member** — groups *distinct* procedures under one umbrella generic, e.g.
  `interface to_str { to_str_int_0; to_str_int_1; to_str_int_2 }` → the generic
  `to_str_` that other modules call. This must be emitted so those calls resolve.
- **Single-member** — a *procedure rename*: an alternate call-site name for one
  procedure, e.g. `interface diagonal_plus { increment_diagonal_by }` or
  `interface uncompress_from_pyramid { symmetric_unzip_triangle }`. The generic
  `NAME_` is emitted too (so a call `x.uncompress_from_pyramid` resolves via
  `uncompress_from_pyramid_` → the member's procedures).

> Note: `foo.pl`'s module-interface-scope handler is a no-op, so `release/` omits
> these interface blocks from the `.int` — which is why some of its own `.F90`
> executables fail to link. The new translator emits them (a small, deliberate
> `.int` deviation) so the calls resolve and the build links.

**Visibility in the `.int`.** The generic interface `name_` is `public` by default
and `private` only when **every** overload carries the `private` attribute (one
`public` overload makes the whole generic public). A procedure's scalar *specific*
name is additionally exported (`public <name>`) exactly when that procedure is
declared `public`, so it can be referenced by its specific name (passed as an
actual argument, or called as `MODULE::proc`).

### `.int` fidelity vs the `release/` reference

The generated `.int` files are equivalent and compilable, but a few
interface/visibility choices **deliberately differ** from `foo.pl`'s `release/`
output. None affect correctness or linking:

- **Alias interfaces are always emitted, including uncalled ones.** A single-member
  rename interface (above) is emitted even when no call site uses it, because some
  are needed to support ones that *are* used and the translator does not (yet) do
  the call-usage analysis to tell them apart. `release/` prunes the never-called
  ones.
- **Elemental specifics are exported when `public`.** `foo.pl` omits the
  `public <name>` for an `elemental` procedure (exporting only the generic
  `name_`); the translator exports both when the procedure is declared `public`.
  This is harmless (an extra export) and keeps the visibility rule uniform across
  elemental and non-elemental procedures.
- **Private, never-called interfaces are kept.** Both translators keep them; an
  unused-symbol pass could drop those never referenced within the module.
- **The translator is sometimes *more* correct than `release/`.** e.g.
  `quote_position_` is `private` in the source and used only within `STR`, so the
  translator marks it `private`; `release/` marks it `public`, which looks like
  stale reference output.

Matching `release/` byte-for-byte here would require call-usage and access-pruning
passes; since the bar is equivalent, compilable Fortran (not byte-exact), these are
left as documented, intentional deviations.

## 9. `get_from` template inheritance

`get_from` lets a procedure inherit a body from a **template** module (often a
`virtual module` such as `VEC{INTRINSIC}`, `MAT{INTRINSIC}`, `OBJECT`):

```foo
to_str result (string) :: get_from(INTRINSIC, FMT?=>*), pure
end
```

**Inheritance is macro-expand-then-reparse, mirroring `foo.pl`'s two passes.** The
translator takes the parent template's *source text*, applies the substitutions
(`KEY?→VALUE` and the paired type parameters) to that **text**, then re-lexes and
re-parses the expanded source with a fresh lexer/parser and translates the
resulting concrete subtree like ordinary code. This matters because no placeholder
token ever reaches semantic analysis, so the emitted calls and `use` dependencies
are derived from the *real* substituted names — an earlier "walk the template tree
then patch the output string" approach mis-recorded uses of the raw placeholder
(e.g. a spurious `use MOLECULE_MODULE, only: GRID_`).

**Comments in a template are left verbatim** — they are *not* substituted. `foo.pl`
does the same: an inherited body keeps `! The following code is inherited from
VEC{OBJECT}` and documentation like `"conjg" is replaced with "1*"` unchanged, even
though the code around them is substituted.

### Placeholder keys: `KEY?=>VALUE`

A substitution key is written with a trailing `?` to mark it as a template
**placeholder**: `FMT?=>*`, `V_TYPE?=>MAT{REAL}`. The `?` distinguishes the
placeholder from an ordinary identifier of the same spelling. Positional type
parameters are also paired automatically and **recursively** — inheriting
`MAP{VEC{KEY},VEC{VAL}}` as `MAP{VEC{INT},VEC{INT}}` substitutes `KEY→INT` and
`VAL→INT` (not just the top-level `VEC{KEY}`).

### Conventions and caveats

- **Name your type placeholders `KEY_TYPE?`**, not bare `KEY?`, when the key
  shares a spelling with a real argument or variable. The classic hazard:
  ```foo
  change_basis_using(V) :: get_from(MAT{INTRINSIC}, V?=>MAT{REAL})
  ```
  here `V` is the matrix *argument* and `V?` is the placeholder for *its type* —
  the substitution can collide with the variable `V` in the body. Writing
  `V_TYPE?` removes the ambiguity. (The translator guards against the collision,
  but the explicit name is clearer.)
- **Embedded placeholders** are allowed *inside* a name:
  `.RHO:make_Hirshfeld?_atom_ED_grid(...)` with `Hirshfeld?=>Becke` becomes
  `make_Becke_atom_ED_grid_(self,…)`.
- A placeholder **value** may be a method reference: `SET?=>.set_x` (a self
  method) or `GRID?=>:make_grid` (a same-module generic). Used as a call in the
  template, these become `set_x_(self,…)` / `make_grid_(self,…)`.
- A non-`?` substitution (e.g. `TRANSPOSE_A=>DAGGER_A`) is applied as a plain
  text replacement, **except** where the token is a keyword-argument *name*
  (`…, TRANSPOSE_A=TRUE`) — see §16.

---

## 10. Statements and control flow

Standard Fortran-like control flow. **Every block closes with a bare `end`** —
the explicit `end if` / `end do` forms have been normalised away in the sources
(the translator emits the correct `end if`/`end do`/`end subroutine`/… in the
Fortran output from the block type, not from the source).

```foo
if (condition) then
   ...
else if (other) then
   ...
else
   ...
end

select case (variable)
case (value1)
   ...
case default
   ...
end

do i = 1,n
   ...
end

do                 ! infinite loop
   if (done) exit
end
```

### Named loops

A `do` may be labelled, and `exit`/`cycle` may name the label; the closing `end`
carries it too:

```foo
main: do
   pair_products: do
      if (j==m) exit pair_products
   end
   if (i==m) exit main
end
```

### `forall` and `where`

```foo
forall (i=1:n_bonds)
   nAB(i) = self.n2(pair(i,1),pair(i,2))
end

where (mask) a = b
```

---

## 11. Expressions, calls and the dot/percent selectors

Expressions are **Fortran**: the same arithmetic, relational and logical
operators, intrinsics, and `//` string concatenation.

```foo
res  = ONE / res
same = self == i
res  = mod(self,2) == 0
res  = trim(prop) // ",isovalue=" // to_str_(self.iso_value,"f10.5")
```

### Dot-method calls — `.proc`

`.proc(args)` is a method call on `self`; `obj.proc(args)` on `obj`. **A
`.procedure` always resolves to a `procedure_` call** (the generic-interface
name with a trailing underscore), passing the receiver as the first argument:

```foo
.get_next_item(item,f,l)        ! -> get_next_item_(self, item, f, l)
arch.read(.NOs)                 ! -> read_(arch, self%NOs)
```

This holds **even when the method name coincides with a Fortran intrinsic**:
`prop.trim` → `trim_(prop)` (an explicit interface in `STR` for
`trim_blanks_from_end`), `fmt.scan` → `scan_(fmt)`. They are *not* the Fortran
intrinsics.

**The exceptions are a hand-maintained set of real intrinsic functions** that the
translator *does* map through, `.name` → `name(x)`: `abs`, `sin`/`cos`/`tan`,
`asin`/`acos`/`atan`, `mod`/`modulo`, `nullify`, and the Fortran-2008 error
functions `erf`/`erfc` (`r1.erfc` → `erfc(r1)`). Fortran intrinsic return types
cannot be derived from the sources, so this table (and the one below) is extended
by hand as new intrinsics are encountered.

### Intrinsic pseudo-properties

A small fixed set of `.name` selectors map to Fortran intrinsics rather than to
method calls. Each **carries a result type** so a *chained* method resolves — e.g.
`.dim.is_even` is `size(self)` (an `INT`), so `is_even` resolves in `INT_MODULE`
(`is_even_(size(self))`):

| Foo | Fortran | result type |
|---|---|---|
| `.dim`, `.dim1` … `.dim7` | `size(x)`, `size(x,1)` … | `INT` |
| `.allocated` / `.deallocated` | `allocated(x)` / `NOT allocated(x)` | `BIN` |
| `.associated` / `.disassociated` | `associated(x)` / `NOT associated(x)` | `BIN` |

`NOT` expands (via a C macro) to `.not.`; two adjacent `.not.` are rejected by
gfortran, so `NOT .x.deallocated` (i.e. `NOT (NOT allocated(x))`) is emitted
parenthesised.

### The `.` vs `%` component selector

`.` is used for both **component access** and **method calls**; the translator
decides which from the type table (a component → `%`, otherwise a method). A
literal `%` in source is also accepted (and means component access). So
`self.io_file.record` → `self%io_file%record` (a component chain), while
`self.trim` → `trim_(self)` (a method).

### Explicit and submodule-qualified calls

```foo
STR:get_next_item(self,item,f,l)     ! explicit GENERIC call  (single colon)
STR::get_next_item(self,item,f,l)    ! explicit NON-generic call (double colon)
:get_next_item(self,…)               ! same, within the defining module
::get_next_item(self,…)
```

- A single `:` is a *generic* call (the `name_` interface); `::` is a
  *non-generic* call naming a specific procedure (only within its own module,
  and the name must not be overloaded).
- **Submodule** calls put the submodule before the colon:
  `.SET:delete_atom_SCF_archives` (generic call into submodule `SET`),
  `.MAIN:setup(...)` (into the `MAIN` submodule → `MOLECULE_MAIN_MODULE`),
  `.:setup(...)` / `.::setup(...)` within the same submodule. The target module is
  resolved through the cross-submodule registry (§6), so the correct
  `MOLECULE_XXX_MODULE` is used (or the call is a same-module one and no `use` is
  emitted).
- A **type-qualified** call `TYPE.SUBMOD:proc` (no leading dot — e.g.
  `MOLECULE.MAIN:cleanup`, `TEXTFILE:destroy(stdout)`) names the *module*, not a
  receiver object, and in practice targets a `selfless` procedure (there is no
  other way to call one). The translator recognises selfless targets and passes no
  `self`. A genuine non-selfless exception should be rewritten with `.SUBMOD:proc`
  (leading dot) in the source.
- **How selfless targets are detected — and why a wrong guess can't ship silently.**
  Selfless targets are found by scanning every `.foo` for procedure headers carrying
  an explicit `:: selfless` attribute (`buildSelflessMethods` in the translator). A
  procedure that is selfless *only* by virtue of interface-block nesting (`foo.pl`
  treats routines nested more than two scopes deep as selfless) is **not**
  auto-detected. This is not a silent-bug risk, though: a wrong self / no-self
  decision emits a call with the wrong argument count, so gfortran rejects it with an
  argument mismatch or *"no specific subroutine for the generic call"*. A clean build
  therefore proves every `TYPE.SUBMOD:proc` call actually present in the sources
  resolves correctly. If a genuine non-selfless procedure ever needs to be reached
  this way, rewrite the call with the leading-dot `.SUBMOD:proc` form in the source.

---

## 12. Parallelism (`parallel do`, MPI)

Tonto supports MPI parallelism through macros. A `parallel do` distributes loop
iterations across processes:

```foo
parallel do k = 1,.ab_n_gaussian_pairs
   ...
   v11 = v11 + ...
end
```

becomes (pre-CPP):

```fortran
   do k = PARALLEL_DO_START(1,1),self%ab_n_gaussian_pairs,PARALLEL_DO_STRIDE(1)
   LOCK_PARALLEL_DO("MODULE:proc")
      ...
   end do
   UNLOCK_PARALLEL_DO("MODULE:proc")        ! after the loop
   PARALLEL_SUM(v11)                         ! reduction, written explicitly
```

The `LOCK_PARALLEL_DO` is emitted just inside the loop and the matching
`UNLOCK_PARALLEL_DO` **after** the `end do`. Reductions (`PARALLEL_SUM`, etc.)
and the lower-level MPI calls live in the `SYSTEM`/`PARALLEL` modules.

---

## 13. Assertions and other C macros

The C preprocessor (`include/macros.in`) provides, among others:

- **Assertions**: `ENSURE(cond,"msg")`, `DIE_IF(cond,"msg")`, `WARN_IF`, `DIE`,
  `WARN`, `VERIFY`. A *precondition* assertion is written at the top of a
  procedure (before the first executable statement). These compile out unless
  `-DUSE_PRE_AND_POST_CONDITIONS` / `-DUSE_PRECONDITIONS` is set. **They must not
  appear in lowercase `pure`/`elemental` procedures** (see §7).
- **`UNKNOWN(word)`** — used in the `case default` of a keyword dispatcher; it
  builds an "unknown keyword, known are: …" error from the enclosing
  `select case` labels.
- **`DEFAULT(x)`** — a component initialiser.
- **Memory**: `create`/`destroy` map to allocate/deallocate with tracking.

---

## 14. `use` / `USE` and the `.use` mechanism

There are two independent paths, and they should not be confused:

1. **Explicit source `use`/`USE` statements** are passed **verbatim into the
   `.F90`** (case preserved). In practice these are reserved for **external
   Fortran modules** the translator cannot resolve, and written **uppercase**:
   ```foo
   USE mpi, only: MPI_CHARACTER
   ```
2. **The `.use` file is auto-generated** from the cross-module procedure *calls*
   the translator detects — never from source `use` statements. It always uses
   hardcoded lowercase `use X_MODULE, only: proc` (plus a wholesale
   `use TYPES_MODULE` / `use SYSTEM_MODULE`).

So **capital `USE` is a readability convention, not a requirement** for `.use`
generation: it flags an explicit external module versus the auto-generated repo
dependencies. (Fortran is case-insensitive, so either spelling compiles the
same; the convention is kept for consistency.) Do **not** hand-write a lowercase
`use SOME_REPO_MODULE` in a source file — it would be copied verbatim into the
`.F90` and double up with the auto-generated `.use` entry.

---

## 15. Foo → Fortran conversion summary

| Foo | Fortran |
|---|---|
| `varname :: TYPE` | `TYPE :: varname` |
| `str.foo` | module `STR_MODULE` |
| `n_items result (res) :: pure` | `pure function n_items(self) result (res)` |
| `VEC{STR}(len=1,6)` | `VEC(STR(len=1),6)` |
| `.proc(a)` | `proc_(self,a)` |
| `a(i)[j]` | `a(i)%element(j)` |
| `obj.component` | `obj%component` |
| `parallel do …` | `do … = PARALLEL_DO_START(...) … PARALLEL_DO_STRIDE(...)` + LOCK/UNLOCK |

The translator builds the derived-type table from `types.foo` first, then
translates each module from its parse tree (resolving components, overloads,
inheritance and cross-module `use` dependencies). The legacy `foo.pl` did this in
two passes (pass 1 analysis, pass 2 generation).

---

## 16. Caveats, edge cases and known `foo.pl` bugs

The bar for the new translator is **equivalent, compilable Fortran — not a
byte-exact match** to `release/`. In a number of places `foo.pl` has defects, and
the new translator deliberately emits the *correct* result instead. Known cases:

- **`MPI_SENDRECV` dropped paren** — `foo.pl` drops the closing `)` of a
  `len(...)`/`size(...)` argument when it occurs inside a `get_from` value
  (`LEN1?=>len(sendbuf)`), producing unbalanced `MPI_SENDRECV(sendbuf,len(sendbuf,…`.
  The new translator keeps it balanced.
- **`.trim`/`.scan` as intrinsics** — `foo.pl` renders these as the Fortran
  intrinsics `trim(...)`/`scan(...)`; correctly they are the `STR` methods
  `trim_`/`scan_` (a `.proc` always → `proc_`). The new translator emits the
  method form.
- **`UNKNOWN` keyword list** — `foo.pl` drops `case` labels that sit on
  continuation (`&`) lines, so its known-keyword list is truncated. The new
  translator includes the full list.
- **`TRANSPOSE_A=>DAGGER_A` in keyword-argument position** — a non-`?`
  substitution where the token is a keyword-arg *name* (`to_product_of_(…,
  TRANSPOSE_A=TRUE)`): `foo.pl` does not substitute it (leaving the wrong
  `TRANSPOSE_A` for a complex change-of-basis). The new translator applies it
  (`DAGGER_A`).
- **`pure`/`elemental` precondition assertions** — illegal as a side effect; the
  translator emits them commented out in any lowercase-`pure`/`elemental`
  procedure (§7), whether written locally or inherited from a template.

Further edge cases discovered while getting the whole tree to compile (debug
build, which turns *on* preconditions and turns *off* the `PURE`/`ELEMENTAL`
macros, and so exercises far more code than a release build):

- **`TYPE::method` mis-parses as a declaration.** A statement like
  `TEXTFILE::create_stdout` looks like `identList :: declTail`, i.e. a variable
  `TEXTFILE` of type `create_stdout`. The translator recognises this shape and
  re-emits it as the call it really is — and, crucially, does **not** enter such a
  "variable" into the local-variable table (otherwise a later
  `TEXTFILE:destroy(stdout)` would see `TEXTFILE` as a known local and mis-read the
  `:` as an array-section `lo:hi`, emitting the statement verbatim).
- **Double `NOT`.** `NOT` is a macro for `.not.`; `NOT .x.deallocated` becomes
  `.not. .not. allocated(x)`, which gfortran rejects — the translator parenthesises
  it as `NOT (NOT allocated(x))`. (The clean source alternative is `.x.allocated`.)
- **`stdin`/`stdout`/`stderr`** are global module variables; the archaic
  `TEXTFILE:destroy(stdout)` and the modern `stdout.destroy` should both resolve.
- **Chained intrinsic pseudo-properties** (`.dim.is_even`) require the pseudo-
  property to carry a result type (§11); otherwise the chained method's home
  module is unknown and no `use` is recorded (harmless in release, where the
  containing `ENSURE` is compiled out, but a hard error in a debug build).

Other deliberate normalisations in the sources: all block terminators are bare
`end`; component access via `%` or `.` (translator-resolved); array constructors
use `[ ]`.

---

## 17. Grammar structure (`Foo.g4`)

The ANTLR4 grammar is organised roughly as:

- **Program / module**: `program` (the file: a sequence of `moduleDef` or
  `programDef`), `moduleDef`, `programDef` (`PROGRAM moduleName; procBody*; end`
  for a main program), `moduleName`, `moduleDataItem`, `moduleProcItem`, `typeDef`,
  `interfaceBlock`. A `procBody` also allows an `implicitStmt`, so a program's
  `implicit none` parses.
- **Declarations**: `varDecl` / `localDecl` (`identList :: declTail`), `declTail`
  (`typeSpec ptrSuffix? attrSuffix? initSuffix?`), `dataStmt`.
- **Procedures**: `procDef`, `procHeader`, `procArgs`, `procResult`, `procAttrs`,
  `attr`, `getFromArg`/`getFromKey`/`getFromVal`.
- **Statements**: `stmt` → `ifStmt`, `doStmt`, `forallStmt`, `selectStmt`,
  `whereStmt`, `simpleLine`; `simpleStmt` (assignment, I/O tail, `exit`/`cycle`/
  `return`, and a bare `read`/`write(ctrl) iolist` I/O form).
- **Expressions**: `expr`, `postfix` (`head trailer*`), `callHead`, `trailer`
  (the `.name` / `%name` / `(args)` / `[args]` selectors), `name`, `arg`.
- **Lexer**: keyword tokens (lowercase control keywords; uppercase type/intent
  keywords; word and `.op.` operators), `IDENTIFIER` (with optional `?` and
  embedded-placeholder support), literals, and the `::`/`=>` punctuators.

Soft keywords (`end`, `data`, `type`, `case`, `where`, `result`, `default`, …)
double as ordinary names where context allows (`end+1`, `self(end:)`).

---

## 18. References

- Grammar + translator: `foogrammar/Foo.g4`, `foogrammar/FooToFortran.java`.
- Legacy reference translator: `foo.pl` (removed from the repo; its frozen output
  survives in `release/`).
- Macros: `include/macros.in`.
- Foo sources: `foofiles/`; type declarations: `foofiles/types.foo`.
- Reference Fortran: `release/` (from `foo.pl`); new-translator output:
  `antlr4-release/`.
- Companion doc: `CLAUDE.md` (build/run details).
