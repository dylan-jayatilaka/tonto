# Developer guide

Notes for people building *on* Tonto's internals, not for end users. Related
references:

- **Building & testing a normal binary** → the top-level [`README.md`](../README.md).
- **The ANTLR4 `foo`→Fortran translator task, layout, and how to build/run the
  translator itself** → [`CLAUDE.md`](../CLAUDE.md) and `scripts/build_translator.sh`.
- **Making the DOT call-graphs readable** (aggregate/ambient, per-module
  documentation graphs) → [`docs/CALL_GRAPHS.md`](CALL_GRAPHS.md).
- **The Foo language and Foo→Fortran conversion rules** →
  [`docs/FOO_GRAMMAR_DOCUMENTATION.md`](FOO_GRAMMAR_DOCUMENTATION.md).
- **What helps (and hinders) an AI assistant working in this codebase**, measured
  → §3 below.

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
`scripts/simplify_callgraph.py`; see [`docs/CALL_GRAPHS.md`](CALL_GRAPHS.md).**

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

### A concrete improvement this suggests

The translator already resolves every call site to a specific procedure — that is
how it emits each module's `use … only:` list. That resolution is exactly the
information a reader lacks at an overloaded call site. Emitting it — a per-module
map of `file:line  .generic_call(args) → specific_procedure`, or the resolved
name as a comment on the generated call — would turn a multi-step inference into
a grep, for humans and assistants both. It costs little, because the data already
exists inside the translator. (Not yet implemented; recorded here as a good idea.)

A second, cheaper trick from the same session: because the generated Fortran gives
each overload a *distinct* specific name, a `DIE` compiled with `-fbacktrace`
identifies the routine and its callers in one run — which would have replaced a
dozen rebuild-and-print cycles.

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
