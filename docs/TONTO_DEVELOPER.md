# Developer guide

Notes for people building *on* Tonto's internals, not for end users. Related
references:

- **Building & testing a normal binary** → the top-level [`README.md`](../README.md).
- **The ANTLR4 `foo`→Fortran translator task, layout, and how to build/run the
  translator itself** → [`CLAUDE.md`](../CLAUDE.md) and `scripts/build_translator.sh`.
- **Making the DOT call-graphs readable** (aggregate/ambient, per-module
  documentation graphs) → [`docs/MAKING_CALL_GRAPHS.md`](MAKING_CALL_GRAPHS.md).
- **The Foo language and Foo→Fortran conversion rules** →
  [`docs/FOO_GRAMMAR_DOCUMENTATION.md`](FOO_GRAMMAR_DOCUMENTATION.md).
- **What helps (and hinders) an AI assistant working in this codebase**, measured
  → §3 below.
- **Writing parallel (MPI) code without stepping on a mine** → §1a below. Read it
  before touching `parallel do`, any `PARALLEL_*` macro, or file I/O.

---

## 1. Translator analysis tools

The ANTLR4 `foo`→Fortran translator can analyse the whole source set, not just
translate it. Two developer features build on its cross-module call graph.

### Call & module-use graphs (`make callgraphs`)

From a build directory:

```
make callgraphs
```

writes into `build/callgraphs/`:

| File | Contents |
|------|----------|
| `call_graph.dot`       | procedure-level call graph, clustered by module (large) |
| `module_use.dot`       | module `use` graph, submodules collapsed into their parent module node |
| `submodule_use.dot`    | expanded submodule `use` graph, one cluster per split family (MOLECULE, DIFFRACTION_DATA) |
| `dead_code_report.tsv` | per-module live/dead procedure counts + the dead list, rooted at `run_molecule` |

Render with Graphviz (install it separately). The two use-graphs are auto-rendered
to SVG if `dot` is on the PATH; the big call graph is best laid out with `sfdp`:

```
sfdp -Goverlap=prism -Tsvg build/callgraphs/call_graph.dot -o call_graph.svg
dot  -Tsvg build/callgraphs/module_use.dot -o module_use.svg
```

(First run reconfigures the build dir once — if `make callgraphs` reports "no rule
to make target", run `cmake ..` in the build dir first, then retry.)

The raw `module_use.dot` is a hairball (139 nodes / 921 edges). **To make it
readable — and to produce a per-module documentation graph — use
`scripts/simplify_callgraph.py`; see [`docs/MAKING_CALL_GRAPHS.md`](MAKING_CALL_GRAPHS.md).**

### Dead-code-eliminated executables (`-DPURGE_DEAD_CODE`)

A given executable only reaches a fraction of the code base; the rest is dead *for
that executable*. Configure a **separate** build tree that emits only the reachable
procedures:

```
mkdir build-slim && cd build-slim
cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=release \
         -DPURGE_DEAD_CODE=run_molecule
make run_molecule
```

This computes reachability from the `run_molecule` (=`tonto`) entry point and drops
every procedure not reachable from it (~1/3 of the ~7600 procedures), producing a
smaller binary that passes the identical test suite. The purge is **per executable**
— code dead for `run_molecule` may be live for `run_dc`/`run_sf`/etc. — so always
use a dedicated build tree and never share it with the normal build. The normal
build (no `-DPURGE_DEAD_CODE`) is unaffected.

Under the hood these use `FooToFortran` flags `--call-graph-report`,
`--dead-code-report <root.foo>`, and `--purge-dead-code <root.foo>` (see
[`CLAUDE.md §8`](../CLAUDE.md)).

## 1a. Writing parallel (MPI) code in Foo — the pitfalls

Every trap below cost real time in August 2026, and every one is **invisible in a serial run**,
which is the only kind most people do. Read this before touching anything with `parallel do`,
`PARALLEL_*`, or file I/O.

### The one rule everything follows from

**MPI collectives are matched by *program order*, not by name.** If rank 0 executes broadcast A
then B, and rank 1 executes only B, MPI pairs rank 0's *A* with rank 1's *B*. When they are
different sizes you get `MPI_ERR_TRUNCATE`; when they are the same size you get **silently wrong
data**; when one rank has none left you get a **hang**.

So the governing question for any code that runs under MPI is not "is this correct?" but:

> **Does every rank reach this point the same number of times?**

### Pitfall 1 — a collective inside an `if (IO_IS_ALLOWED)` block

The guard means *master only*. A collective inside it is entered by one rank and no other.

```foo
if (IO_IS_ALLOWED) then
   ...
   .clear_and_put_margin        ! <-- this broadcasts. MASTER ONLY. Bug.
end
PARALLEL_BROADCAST(.io_status,tonto.master_processor)
.clear_and_put_margin           ! <-- the correct one: every rank
```

That was a real bug in `TEXTFILE:flush` (fixed 2026-08-03) and it stopped `hart` working under
MPI for years. It hid because `clear_and_put_margin` *returns early* when the margin width is
zero, which is the usual case — so it only bit once a caller set a margin.

**`scripts/check_parallel_lint.py` does not catch this**; extending it to would be worthwhile.

### Pitfall 2 — a reduction *lexically inside* a `parallel do`

`LOCK_PARALLEL_DO` is the first statement in the loop body, and `WORK_IS_SHARED` is false while
the lock is held. Every reduction macro is gated on `WORK_IS_SHARED`, so:

```foo
parallel do i = 1,n
   ...
   PARALLEL_SUM(x)      ! DEAD CODE THAT LOOKS CORRECT
end
```

The reduction never runs; each rank keeps 1/N of the answer, silently. Four such sites in
`molecule.grid.foo` did exactly that. Put the reduction **after** the loop.
`scripts/check_parallel_lint.py` catches this one, and it is in CI.

### Pitfall 3 — but a *suppressed* reduction is usually CORRECT

The mirror image, and the reason there is no runtime check for pitfall 2. This is fine:

```foo
SHELL1QUARTET:make_esfs_ss_0000     ! called from inside an outer `parallel do`
   parallel do k = 1,n              ! runs SERIALLY, full range, on every rank
   ...
   PARALLEL_SUM(v11)                ! skipped -- and skipping is RIGHT
```

With an outer lock held the inner loop is serial, so each rank already holds the whole answer and
there is nothing to combine. This is "MPI on the outside", and it is pervasive —
`shell1quartet.foo` alone has 17 such loops. A runtime "abort on suppressed reduction" was
implemented and **withdrawn** because it fires on all of them. Only *lexical* containment is a
bug, which is why the enforcement is a static lint.

### Pitfall 4 — file I/O is collective, all of it, not just writing

`FILE`/`TEXTFILE` keep every rank's state consistent by broadcasting. **`open_for` broadcasts
`.unit` and `.io_status`; `close` broadcasts `.io_status`; `exists` and `is_open` broadcast their
result; `BUFFER:put_str` broadcasts the 256-byte buffer and the cursor on every token written.**

Consequences:

- **Writing output costs two collectives per token.** A `hart` run performs ~142,000 broadcasts
  before its first fragment SCF, purely producing text.
- **Any I/O inside a `parallel do` desynchronises the ranks**, because they are doing different
  work and therefore different amounts of I/O. This is what blocks parallel fragHAR: at 2 ranks
  the two ranks are in exact lockstep (141,907 broadcasts each) until they take *different
  fragments*, and diverge immediately after.
- **Switching binary/ascii does not help.** Measured inside the fragment loop: 96% of the
  collectives are *scalars* — `unit`, `io_status`, `record` — i.e. open/close/inquire
  bookkeeping, which binary archives perform too. Only ~4% is text.

`FILE:per_rank_write` exists as an attempt at this: it is the ordinary write with the guard
removed. It does not solve the problem, because the surrounding `open`/`close` still broadcast —
which is why its precondition ("every rank writes its OWN file") was never satisfiable, and why
it was found misused in serial loops.

### Pitfall 5 — the parallel-do lock does two jobs

`WORK_IS_SHARED` (renamed from `DO_IN_PARALLEL` on 2026-08-03: the old name read as "are we
running in parallel?", which is how it got misused, when it means "is work split between ranks
here?") is asked two different questions:

| question | asked by | when | must answer |
|---|---|---|---|
| "should **this** loop distribute?" | `PARALLEL_DO_START` / `_STRIDE` | *before* entry | yes |
| "am I **inside** a parallel region?" | inner loops, reductions, I/O | *during* the body | yes |

Those are opposite states of one flag. **Moving `LOCK_PARALLEL_DO` before the loop — the obvious
"fix" — silently disables distribution**, because the bounds macros then see a held lock and each
rank runs the full range. The loop stays correct and stops being parallel: no error, just lost
speed.

Also note `LOCK_PARALLEL_DO` is emitted *inside* the body, so it executes **once per iteration**.
It is safe only because it is idempotent by name. Any scheme that counts (a nesting depth, say)
is therefore incompatible with the current lowering.

### Pitfall 6 — the error path must not use collectives

A rank that is dying is, by definition, out of step with its peers. `TEXTFILE:flush` contains
broadcasts, so flushing the error file from the dying rank while the others run on is a collective
entered by one rank — a hang, replacing a diagnosable failure with an undiagnosable one.

`SYSTEM:die` writes its message straight to Fortran's preconnected **stderr (unit 0)**, tagged
with the rank, and then calls `MPI_ABORT`. Unit 0 because it exists on every rank, whereas the
`std_err` file is opened only on the master.

Until 2026-08-03 the message was written only under `IO_is_allowed`, so a dying **non-master**
rank said nothing at all — no stdout, no stderr, no `<job>.err`. Fixing that immediately exposed
two defects that had been invisible.

### Pitfall 7 — `PURE` versus `pure`

Upper-case `PURE`/`ELEMENTAL` are **macros** (`include/macros.in`), `#undef`'d to nothing under
`USE_PRECONDITIONS` and under `MPI`. Lower-case `pure` is passed straight through as the Fortran
keyword and stays pure in every build.

So a routine containing `ENSURE`, `DIE`, `WARN` — anything that writes `tonto` — must be declared
**`PURE`**, never `pure`. Get it wrong and it compiles in release (where `ENSURE` vanishes) and
fails only in debug or MPI, with gfortran's thoroughly misleading *"There is no specific
subroutine for the generic `ensure_`"* rather than a purity error.

### Pitfall 8 — after a per-rank region, the ranks' object state diverges

Per-rank mode (`tonto.set_per_rank_IO_allowed(TRUE)`) exists so that ranks working on different
data can each open, write and close their OWN files. The consequence is easy to miss: when the
region ends, **the ranks' object graphs are deliberately different and stay that way**. In
`MOLECULE.SCF:fragment_SCF_para` each rank builds grids and densities only for the fragment it
ran, so afterwards `.mol(g).becke_grid` is allocated on one rank and not on another.

Any *later* shared-mode code that branches on that state will then desync — and the branch is
usually innocent-looking output:

```foo
if (.becke_grid.allocated) .put_becke_grid   ! master: 42 broadcasts. Rank 1: zero.
```

Because TEXTFILE bookkeeping is collective, "print a bit more on one rank" *is* a collective
mismatch. It does not fail where it happens: the ranks stay superficially in step until some
later collective pairs with the wrong partner. In the observed case rank 1 died in
`TEXTFILE:close` with *"not an existing file!"* — its `.exists` broadcast had received another
rank's payload. The message named a file, a routine and a rank, and all three were innocent.

Two ways out, and you must pick one deliberately:

- **Resynchronise** the state across ranks when the region ends, or
- **keep the later code non-collective** — which for pure output means switching per-rank mode
  on and letting the master write alone. That is what `MOLECULE.PUT:put_atom_group_mols` does;
  note it saves and restores the caller's mode rather than forcing `FALSE`, since it may itself
  be called from inside a per-rank region.

Corollary: **verify a mode switch is actually on before trusting anything downstream of it.**
`SYSTEM:set_per_rank_IO_allowed` assigned `.keyword_echo` instead of `.per_rank_IO_allowed` for
as long as it had existed, so the whole mechanism was dead code that looked live, and every call
site was silently toggling an unrelated flag. A one-line probe printing the flag at the point of
use would have found it immediately; three careful readings of the call sites did not.

### How to debug a collective desync

Inspection does not work; measurement does. This found the `TEXTFILE:flush` bug in an afternoon
after two failed attempts at reasoning it out:

1. **Trace every broadcast** from the `PARALLEL:broadcast` *template* — a single edit covers
   all 25 type instantiations, because they are all `get_from` of one body. Log the **datatype
   as well as the count**; the type alone often identifies the caller (`t=MPI_CHARACTER n=256`
   is a TEXTFILE string buffer, `n=1` integers are `.record`/`.IO_status` bookkeeping).
2. **Write each rank's trace to its own file**, by letting Fortran auto-connect the unit:
   ```foo
         write(70+.processor_rank,'(a,i0,a,i0)') "t=",MPI_TYPE?," n=",LEN?
         flush(70+.processor_rank)
   ```
   giving `fort.70`, `fort.71`, … **Do not** merge the ranks onto stderr and split afterwards.
   Two ranks writing the same stream interleave *mid-line* despite flushing — an observed trace
   contained the line `BCTRACt=7 n=1` — and the corruption lands exactly where you are looking.
3. **Diff the two streams.** `MPI_ERR_TRUNCATE` *is* a length mismatch, so the first differing
   index is the divergence. `cmp` reports the line number directly.
4. **Add positional `TAG` markers** into the same per-rank stream, via a macro so a marker is
   one short line at the call site:
   ```
   #    define TAG(S)  write(70+tonto%processor_rank,'(a)') "TAG "//S ; flush(70+tonto%processor_rank)
   ```
   Then **count broadcasts between consecutive tags and compare the counts per rank**. The first
   segment whose counts differ names the routine — this is what turned "somewhere in a 2.4
   million call stream" into "`.put_becke_grid`, 42 versus 0" in one run. Bisect by adding tags.
5. **Then trace arguments** of whatever routine the interval implicates.

Two dead ends, so nobody repeats them: gfortran's `backtrace()` **cannot symbolise on macOS**
(*"executable file is not an executable"*), and a debug build alone does **not** localise a
collective mismatch, because MPI aborts internally without a Fortran backtrace.

### Always test in a debug MPI build

Two separate defects in one week were invisible in release and obvious in debug, because
`ENSURE` compiles away in release. The most recent: `hart --fos 0` called
`set_F_sigma_cutoff(0)`, whose own `ENSURE` forbids it and whose own comment says *"make sure
zero is not entered ... just leave it off!"*.

```
cmake -B build-mpi-debug -DCMAKE_Fortran_COMPILER=$HOME/opt/openmpi-gf14/bin/mpifort \
      -DCMAKE_C_COMPILER=$HOME/opt/openmpi-gf14/bin/mpicc -DMPI=1 -DCMAKE_BUILD_TYPE=debug
```

## 1b. Build and test traps that cost real time

Four things that are not obvious, each of which produced a wrong conclusion rather
than an error message.

### Never edit a `.foo` while a build is running — the translation goes stale silently

`FooToFortran` translates all 184 files in a single run, so a file parsed early can have
its `.F90` **written minutes after** you edited the `.foo`. Make then compares timestamps,
sees a generated file newer than its source, and considers it up to date. The result is a
build tree whose Fortran disagrees with the sources it came from, with no diagnostic
anywhere and an exit code of zero.

Observed 2026-08-18: `crystal.foo` edited at 15:13:38, its `build/crystal.F90` written at
15:14:39 by a translator run that had parsed the file before the edit. Two subsequent
builds reported success and changed nothing. The tell is that the generated Fortran does
not contain your change:

```bash
grep -n "<something you just wrote>" build/<module>.F90   # empty = stale
touch foofiles/<module>.foo && make                       # forces re-translation
```

### Check the DIFF, not the pass/fail gate

The loose gate is deliberately forgiving (rel <= 0.2% OR last-digit <= 2, and some tests
carry relaxed tolerances), so it will pass output that is plainly wrong. On 2026-08-18 a
change to the esd printer collapsed real uncertainties --

```
    1   N  7.0  1.0811(3)  ...  0.03615(19)      before
    1   N  7.0  1.081(0)   ...  0.036(0)         after
```

-- and `ctest` reported **Passed**, because 0.03615 -> 0.036 is 0.4% on a test whose
last-digit tolerance had been relaxed. CI caught it on the next push. When you change
anything that *formats* output, diff the produced file against the reference and read it;
the gate answers a different question.

### How the harness actually compares, so you can read a failure

From `scripts/test.py`, and worth knowing before theorising about a red test:

- Tokenising is `.split()`, so **whitespace and column-width differences are invisible**
  to every criterion. They matter for `vimdiff`, not for the verdict.
- `value(esd)` is **not** compared as a string. `split_value_esd` separates the two, the
  value gets the usual tolerances and the esd gets last-digit slack. So `180.00000000(1)`
  versus `180.0000000(0)` **passes** loose.
- A **token-count** difference fails every criterion, loose included. This is the one way
  column drift can still redden a test: these tables are dense enough that one extra
  character merges `O1` with the number beside it and the line loses a token.

### A redundant second statistic is the cheapest invariant you can print

The "Form factor asymmetry" table reported an RMS that had been wrong by seven orders of
magnitude for as long as it existed -- `S^(1/4)/sqrt(n)` instead of `sqrt(S/n)`, because
the routine returned a root and the caller rooted it again. Nobody had spotted it in a
year of reading, since a small number looked plausible either way.

Printing the **maximum** beside the RMS found it immediately: a maximum cannot be smaller
than the RMS of the same set, and it was, by a factor of a million. Where a quantity is
hard to eyeball, print a second quantity that must stand in a known relation to it. The
relation does the checking.

(The same table then taught the converse: those values are ~1e-15 noise for a molecular
HAR, and the reflection at which the maximum falls is the argmax of noise, so both differ
between compilers. Anything printed into a blessed reference must be reproducible --
see `FF_ASYMMETRY_TOL` in `include/macros.in`.)

## 2. Pushing to GitHub

You authenticate either with SSH keys (recommended — no secret in the URL) or a
Personal Access Token over HTTPS. GitHub no longer accepts an account password on
the command line.

### SSH keys (recommended)

SSH keeps no secret in the URL — you set it up once and never paste a token again.

1. **Generate a key** (skip if you already have `~/.ssh/id_ed25519`):
   ```
   ssh-keygen -t ed25519 -C "you@example.com"
   ```
   Press Enter to accept the default location; a passphrase is optional but recommended.

2. **Load it into the ssh-agent** (so you aren't retyping the passphrase):
   ```
   eval "$(ssh-agent -s)"
   ssh-add ~/.ssh/id_ed25519
   ```

3. **Add the *public* key to GitHub.** Print it and copy the whole line:
   ```
   cat ~/.ssh/id_ed25519.pub
   ```
   Go to **<https://github.com/settings/keys>** → **New SSH key**, paste it, give it a
   title (e.g. your machine name), leave the type as **Authentication key**, and click
   **Add SSH key**.

4. **Check that it works:**
   ```
   ssh -T git@github.com
   ```
   The first time, confirm the host fingerprint (type `yes`). On success GitHub prints:
   `Hi USERNAME! You've successfully authenticated, but GitHub does not provide shell access.`
   (That message is expected — GitHub never gives a shell.)

5. **Point `origin` at the SSH URL** (once per clone), then verify:
   ```
   git remote set-url origin git@github.com:USERNAME/REPO.git   # e.g. dylan-jayatilaka/tonto.git
   git remote -v                                                # should now show git@github.com, no token
   ```
   Pushes now authenticate with your key. Nothing sensitive is stored in `.git/config`.

See GitHub's [Connecting to GitHub with SSH](https://docs.github.com/en/authentication/connecting-to-github-with-ssh)
for macOS/Windows specifics (agent auto-start, keychain).

### Personal Access Token (HTTPS)

*Fallback only* — use this where SSH is blocked (e.g. a network that only allows
HTTPS). Prefer SSH above; the embedded-token URL below is the least secure option.

1. **Create it** at <https://github.com/settings/tokens> (Developer settings →
   Personal access tokens → Tokens (classic) → Generate new token). Give it the
   **`repo`** scope, set an expiry, and **copy it immediately** — GitHub shows it once.
2. **Use it:**  `git remote set-url origin https://USERNAME:TOKEN@github.com/USERNAME/REPO.git`
3. **Keep it safe.** The token is a password. Embedding it in the remote URL stores
   it in **plaintext** in `.git/config` and exposes it via `git remote -v`, so treat
   that clone as sensitive and never paste the URL into logs or issues. If a token
   leaks, revoke it at the link above and issue a new one. (This is why SSH is preferred.)

---

## 3. Insights into coding with an AI assistant

Much of the recent translator and numerics work was done with Claude (Anthropic's
coding assistant) working directly in this repository. This section records what
made that go well or badly, because the findings are concrete, measurable, and —
usefully — **they are the same things that help a human reader.**

### The question

*Does a codebase written in a deliberately English-like style — long descriptive
names, heavy explanatory comments — actually make an AI assistant more effective
at working in it? If so, by how much, and can the assistant introspect on why?*

Tonto is an unusually good place to ask, because it was written that way on
purpose (originally to help its author, not any machine), and because the same
codebase contains a few places that sharply violate the convention. Those act as
a control.

### What Tonto's style actually is, measured

Across the 184 `.foo` files:

| metric | value |
|---|---|
| comment lines | 83,439 — **28.7%** of non-blank lines |
| comment : code | **0.40 : 1** |
| unique procedure names | 4,258 |
| mean words per procedure name | **3.37** |
| mean characters per procedure name | **17.4** |
| names of ≥ 3 words | **76%** |
| single-word names | **6%** |

For comparison, typical scientific Fortran sits nearer 0.10–0.15 comment:code.
Names here are sentences rather than labels: `rotated_U2_covariance_mx_for_atom`,
`get_ADP2s_in_ADP2_principal_axes_in`, `put_ADP2s_helper`.

### The evidence that it helps — and the control that shows it

During one debugging session (tracking a NaN in an ADP estimated standard
uncertainty) both effects appeared in the same afternoon:

- **Where names were prose, navigation was fast and correct.** The faulty routine
  was located largely by *reading names*: `rotated_U2_covariance_mx_for_atom`
  states what it returns without needing to be opened.

- **Where the convention breaks, the same assistant failed repeatedly.** Tonto
  overloads heavily — seven procedures share the name `put_ADP2_errors_to`,
  distinguished only by argument list. Tracing which one actually ran failed
  **six consecutive times**. The eventual fix was to stop reading and start
  printing: instrument the *consumer* and let it name its caller. Same repository,
  same assistant, same day — the variable was whether the name identified the thing.

- **A single abbreviation cost real time.** The local `rcm` in the ADP routines
  means *rotated covariance matrix*, but in crystallography `rcm` reads as
  *reciprocal cell matrix*. One abbreviation embedded in a sea of prose is worse
  than either convention applied consistently, because it invites a confident
  wrong reading.

**The practical rule:** the thing that most degrades an AI assistant's accuracy
here is not complexity or size — it is **a name that does not uniquely identify
what it names**. Overloading is wonderful when writing and expensive when
debugging, for humans and machines alike.

### Tracing an overload: use the `.int` file first

**Most of what is needed already exists**, and was overlooked during the six failed
traces above. Each module's generated `.int` file lists every generic together with
its candidate specific procedures, which the translator has given *distinct* names:

```fortran
interface put_ADP2_errors_to_
   module procedure put_ADP2_errors_to_0
   module procedure put_ADP2_errors_to_1
end interface
```

So the first move when chasing an overload is: **open `<module>.int` in the build
tree and read the candidate list.** It is generated on every build and is always
current.

Two things it does not yet give:

1. **Which candidate a given call site picks.** The `.int` says there are two
   candidates; it cannot say that the call at `vec{atom}.foo:17301` resolves to
   `_1`. The translator *does* know — that is how it emits each module's
   `use … only:` list — so a per-module map of
   `file:line  .generic_call(args) → specific_procedure` could be emitted at near
   zero cost, either into the `.int` or beside it.
2. **What `_0` and `_1` mean.** The numbering is opaque, so each definition must
   still be opened to learn which argument list is which. Annotating each
   `module procedure` line with its signature would make the `.int` a usable
   overload index on its own — probably the highest value-for-effort change here.

Both compose with a second trick: because the specific names are distinct in the
generated Fortran, a `DIE` compiled with `-fbacktrace` names the exact routine and
its callers in **one run**, and that name (`put_ADP2_errors_to_1`) maps straight
onto the `.int` list — replacing a dozen rebuild-and-print cycles.

### What the assistant could *not* do — worth knowing

Asked whether it could introspect its own network and quantify which parts were
exercised, Claude's answer was a flat no: no access to weights, activations or
attention, and no ability to count anything about its own computation. It also
flagged that its account of *why* it writes heavy comments is a **post-hoc
narrative rather than a readout of mechanism** — it can say what considerations
appear to bear on its output, but not verify those are the causes.

That distinction is worth keeping in mind generally when working this way: an
assistant's measurements *of your code* are checkable, and were checked here; its
statements *about itself* are not, and should be discounted accordingly. In this
session the pattern held more broadly — every claim that survived was one that had
been reduced to a command someone could re-run.

### If you are working on Tonto this way

- Keep writing names as phrases. It is the single highest-value habit in the code.
- Prefer a distinct name over a new overload when the two versions differ in
  *meaning* (whole-array vs list-subset); reserve overloading for genuine
  same-meaning variants. Where they differ only in arity, one routine with
  `OPTIONAL` arguments is clearer — though note optional arguments imply a presence
  test and can inhibit inlining, so keep them out of hot numeric kernels.
- Comment the *why*, not the *what*. The comments that repeatedly paid off here
  were the ones recording a decision or a hazard — `! WARNING: PROBLEM WITH THIS
  ROUTINE?` on `get_ADP2s_in_new_axes_in` was correct, and the bug it hinted at
  (a loop index that never advanced, leaving rows of an array unwritten) was found
  and fixed years later because that note was there.
- Debug in a `debug` build (`-O0`), where `PURE` is disabled and `WARN`/`WARN_IF`
  are live — see §8 of [`CLAUDE.md`](../CLAUDE.md).
