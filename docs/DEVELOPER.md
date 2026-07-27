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

Generate a key pair once, add the **public** key to GitHub
(Settings → SSH and GPG keys), and point `origin` at the SSH URL:

```
ssh-keygen -t ed25519 -C "you@example.com"     # if you don't already have a key
cat ~/.ssh/id_ed25519.pub                        # add this to GitHub
git remote set-url origin git@github.com:USERNAME/REPO.git
```

See GitHub's [Connecting to GitHub with SSH](https://docs.github.com/en/authentication/connecting-to-github-with-ssh).

### Personal Access Token (HTTPS)

1. **Create it** at <https://github.com/settings/tokens> (Developer settings →
   Personal access tokens → Tokens (classic) → Generate new token). Give it the
   **`repo`** scope, set an expiry, and **copy it immediately** — GitHub shows it once.
2. **Use it:**  `git remote set-url origin https://USERNAME:TOKEN@github.com/USERNAME/REPO.git`
3. **Keep it safe.** The token is a password. Embedding it in the remote URL stores
   it in **plaintext** in `.git/config` and exposes it via `git remote -v`, so treat
   that clone as sensitive and never paste the URL into logs or issues. If a token
   leaks, revoke it at the link above and issue a new one. (This is why SSH is preferred.)
