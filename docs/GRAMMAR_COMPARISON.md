# `Foo.g4` vs. the ANTLR4 Fortran90 grammar

A comparison of `foogrammar/Foo.g4` against the community Fortran 90 grammar in
[antlr/grammars-v4](https://github.com/antlr/grammars-v4/tree/master/fortran/fortran90),
undertaken to find latent bugs in how Foo analyses Fortran expressions and variables —
particularly **array subcomponents of derived types** — and to inform the deferred
*eliminate explicit `TYPE:proc` calls* item (`ANTLR4_DEFERRED.md`).

**Summary: do not import anything from grammars-v4.** It is a port of a 1990s PCCTS
grammar and is in poor repair. Its one genuine convergence with `Foo.g4` is a validation
of our design rather than a gap. But the exercise surfaced **four latent hazards** in our
own grammar and translator, all now fixed. See "Hazards found" below.

---

## 1. The one deep convergence

Both grammars merge *designator, array element, array section, derived-type component,
substring and function call* into a **single permissive postfix chain**, and defer
disambiguation to a semantic pass. Independently arriving at the same shape is a good
sign: it is the only structure that survives Fortran's `A(I)` ambiguity without a
symbol table in the parser.

| | grammars-v4 Fortran90 | `foogrammar/Foo.g4` |
|---|---|---|
| chain rule | `nameDataRef : name complexDataRefTail*` | `postfix : head trailer*` |
| chain link | `complexDataRefTail : sectionSubscriptRef \| PCT NAME` | `trailer : (DOT\|PERCENT) name \| LPAREN argList? RPAREN \| …` |
| section arg | `sectionSubscript` / `subscriptTripletTail` | `arg` |
| component sep | `%` (`PCT`) | `.` or `%` (synonyms) |
| **who resolves `A(I)`?** | **nobody** — the downstream pass was never written | `FooToFortran.java`, via `curType` + `types.isComponent` |

The last row is the whole difference. grammars-v4 "resolves" the array-vs-function
ambiguity by *collapsing* it: `functionReference` matches only `F()` or a call carrying a
keyword argument, so every positional call `SIN(X)` parses as an array reference, and
`primary` lists `nameDataRef` first so the array reading always wins. Statement functions
versus array assignment are likewise merged into `sFExprListRef`. Nothing downstream ever
tells them apart.

`Foo.g4` has the same permissive chain but the translator *does* the pass. `translatePostfix`
threads a running `curType` along the chain and decides per trailer whether `.x` is a
component (`%x`) or a method call (`x_(recv, …)`), and whether `(…)` is a subscript or the
argument list of a deferred call (`pendingCall`).

### Array subcomponents of derived types

This is where `Foo.g4` + translator is materially ahead, and it is worth recording what is
already modelled, because it is the machinery the `TYPE:proc` work depends on:

- `indexResultType` — rank-aware indexing. `VEC{ATOM}(i) → ATOM`, `MAT{T}(:,i) → VEC{T}`,
  `VEC{ATOM}(vec_of_int) → VEC{ATOM}` (vector subscript), `STR(a:b) → STR`.
- `elemComponent` / `arrayOfComponent` — component access *through* an array receiver:
  `VEC{ATOM}%charge → VEC{REAL}`, preserving array-ness so a following `.method` resolves
  against the array module.
- `intrinsicProp` / `intrinsicPropType` — `.dim → size(x) : INT`, `.allocated : BIN`, so a
  chained `.dim.is_even` resolves in `INT_MODULE`.

So `.atom(i).position(3)` types fully: `MOLECULE → VEC{ATOM} → ATOM → VEC{REAL} → REAL`.

**Known limitation (deliberate):** `curType` is cleared after any *method* call in a chain,
because there is no function-return-type registry. `x.foo.bar` loses its type at `foo`, so
`bar` resolves with a null receiver type and no `use` is recorded. This is why the
`TYPE:proc` safety report treats an untypeable receiver as `UNKNOWN` and refuses to convert
it, rather than guessing.

---

## 2. Why grammars-v4 is not a donor

Catalogued so nobody repeats this evaluation:

- **Placeholder tokens that match their own names.** `HOLLERITH: 'HOLLERITH';`,
  `CCON: 'CCON';`, `CONCATOP: 'CONCATOP';`, `SPOFF`, `SPON` are PCCTS *imaginary AST node
  types* that the port turned into literal-matching lexer rules. Consequently Hollerith
  constants and complex literals are unimplemented, and string concatenation is spelled
  `cPrimaryConcatOp : cPrimary DIV SPOFF DIV SPON` — which can never match real input.
- **`**` is left-associative.** `multOperand : level1Expr (POWER level1Expr)*` — the
  standard requires right associativity, so `a**b**c` is mis-associated.
- **Defined operators never lex.** `DOP: '.' '\\a'+ '.'` is backslash-then-`a` in an ANTLR
  literal, so it matches `.\a\a.`; `.cross.` is unreachable.
- **No end-of-statement token.** `EOS` is commented out and `EOL` is unreachable (declared
  after `WS`, which hides newlines). Statement boundaries are entirely implicit — the root
  of most of its ambiguity.
- **Case-insensitivity is hand-enumerated**, 2–3 spellings per keyword (`THEN: 'THEN' |
  'then';`), so `Then` is not a keyword. No `fragment`-letter trick.
- **`subscriptList : subscript+`** has no commas, so `A(1,2)` cannot match `variable`.
- **`complexConst : LPAREN complexComponent COMMA RPAREN`** is missing its second component.
- Only two semantic predicates, both lexical (`IsColumnZero`, `VerifyNotOperator`), and the
  latter covers only `.and.`/`.or.` in lower case — `1.eq.2` mis-lexes. The Python3 port of
  the base class is syntactically invalid.

By contrast `Foo.g4` reserves a minimal keyword set on purpose, re-admits soft keywords
through `name`, joins `&`-continuations in the lexer, and hides comments/preprocessor
lines on a separate channel.

---

## 3. Hazards found (all fixed)

None of the four misfired on the current `foofiles/` corpus — every occurrence was checked.
They are latent, and three of them fail *silently* (wrong code, not a parse error), which is
why they were worth closing.

### H1 — `::` in a subscript is one token (`Foo.g4`)

grammars-v4 carries `subscriptTripletTail : … | DOUBLECOLON expression` **specifically**
because an omitted upper bound before a stride puts two colons together and the lexer
munches them into one token. `Foo.g4`'s `arg` and `dimArg` had no such alternative, so
`a(1::2)` and `a(::2)` never reached `arg` at all — `callHead`'s
`qualifier? DCOLON name` swallowed them as a non-generic qualified call.

Fixed by adding `DCOLON` branches to `arg` and `dimArg`, plus `::` cases in `renderArg` /
`renderDimArg`. The qualified-call reading still wins for `STR::proc`, because `head`
reaches `callHead` before `arg`'s optional group is entered, and a numeric lower bound like
`1::2` cannot start a `callHead`.

### H2 — uppercase section bounds (`FooToFortran.java`)

`callHead : DOT? qualifier? (DCOLON|COLON) name` greedily swallows an array-section range
`LO:HI`. The translator un-swallowed it by testing "uppercase base **and** not a known arg
or local" — consulting `currentArgs` and `localVarTypes` but **not** `moduleVars` or
`globals`. An uppercase *module-level* variable used as a bound would misparse.

Fixed by the shared `isKnownVariable` helper, and extended to cover the `DCOLON` form
(`a(lo::2)`).

### H3 — chain type-loss at method calls (documented, not fixed)

`curType = null` after any real `.method`. Recorded as a known limitation above; closing it
needs a function-return-type registry, which is out of scope. The `TYPE:proc` report is
conservative because of it.

### H4 — omitted **lower** bound became a function call (`FooToFortran.java`)

The most serious of the four, and not visible from the grammars-v4 comparison itself — it
turned up while testing H1. The same `callHead` alternative with its `qualifier?` *absent*
matches the same-module call form `:proc`. So an array section with an omitted lower bound
was parsed as a call:

```
res(:n) = a(:n)      -->   res(n_) = a(n_)        ! before
res(:n) = a(:n)      -->   res(:n) = a(:n)        ! after
res(:n+1) = a(:n+1)  -->   res(n_+1) = a(n_+1)    ! before
```

This is a *plausible-looking* wrong expression, not a parse error. It usually fails to
compile (`n_` does not exist), but silently miscompiles if the name collides with a real
generic. Fixed with the same `isKnownVariable` discriminator: a bound names a variable, a
same-module call names a procedure.

The three live occurrences of `(:expr)` in `foofiles/` are all inside comments, which is
consistent with the note that this was worked around in `molecule.cp.foo` by writing
`1:.n_a` explicitly.

---

## 4. Why `expr` stays flat

`Foo.g4` has **no precedence cascade**: `expr : postfix (binOp postfix)*`, with all 22
binary operators in one undifferentiated set. The parse tree therefore carries no
precedence or associativity information at all.

This is sound *for this translator* because Foo and Fortran share an operator precedence
table and the emitter re-emits operators verbatim in source order. **The invariant is:
any transformation that re-parenthesises, reassociates, or promotes a sub-expression must
not rely on the tree shape.**

It should stay flat:

- An F90-style 10-level cascade is the **slowest** option — roughly ten rule-context
  allocations per operand, even for a bare `x`. (For reference, a full `scripts/regen_all.sh`
  over `foofiles/` + `runfiles/` already takes on the order of 15 minutes.)
- grammars-v4's cascade is also *wrong* at `**` (left-associative), so copying it buys a bug.
- If a precedence-shaped tree is ever genuinely needed — most plausibly when retargeting
  Foo to a language whose precedence differs from Fortran's — the right move is a **single
  ANTLR4 left-recursive rule** with precedence-ordered alternatives
  (`expr : expr POWER<assoc=right> expr | expr (STAR|SLASH) expr | …`). ANTLR compiles that
  into internal precedence climbing: one rule, not ten, and a correctly shaped tree. The
  grammars-v4 cascade is a PCCTS artifact that predates the feature.

Until then, precedence knowledge is available on demand in Java at zero parse cost. The
first consumer is receiver promotion in the `TYPE:proc` work (`EXPR_RECEIVER` sites need
parenthesising).

---

## 5. Relation to `TYPE:proc` elimination

The comparison clarified *why* the earlier mechanical conversion failed and what a correct
one needs. `TYPE:proc(x, …)` is resolved today purely by registry lookup — the qualifier
names the module directly and the argument list is passed through untouched. Rewriting it
to `x.proc(…)` re-resolves the call against the **inferred type of `x`**, which is a
different question with a different answer whenever `x` is an array of the qualifier type
and `proc` is `elemental`.

`--type-qualified-call-report` makes that comparison explicit per site: it asks the same
machinery both questions — where does this resolve today (`fortranModName(TYPE)`), and
where would `x.proc` go (`callModule(typeof(x), proc)`) — and only calls a site `SAFE` when
both answers exist and agree. See `ANTLR4_DEFERRED.md` for the classes and the current
counts.

Eliminating `TYPE:proc` would also delete the H2/H4 ambiguity class outright: with no
`TYPE:proc` form, a `:` inside a subscript could only ever be a section bound, and the
`isKnownVariable` heuristic could go away. That is the structural fix; the guards are the
insurance until then.
