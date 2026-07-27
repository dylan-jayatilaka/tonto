# Call & module-use graphs

The ANTLR4 `foo`→Fortran translator can export the dependency structure of the
whole source set as Graphviz DOT, for documentation and navigation. This note
covers how the graphs are produced and how to make them *readable* — the raw
`module_use` graph has 139 nodes / 921 edges and is a hairball.

## 1. Producing the raw graphs — `make callgraphs`

From any build directory (on-demand; not part of a normal build):

```
make callgraphs
```

writes into `build/callgraphs/`:

| File | Contents |
|------|----------|
| `call_graph.dot`     | procedure-level call graph, clustered by module (large — lay out with `sfdp`) |
| `module_use.dot`     | module `use` graph, submodules collapsed into their parent module |
| `submodule_use.dot`  | expanded submodule `use` graph |
| `dead_code_report.tsv` | per-module live/dead procedure counts, rooted at `run_molecule` |

If Graphviz `dot` is on the PATH, `module_use.svg` and `submodule_use.svg` are
auto-rendered. (Under the hood: `FooToFortran --call-graph-report`; see
`CLAUDE.md §8`.)

## 2. Making `module_use` readable — `scripts/simplify_callgraph.py`

This post-processes `build/callgraphs/module_use.dot`. It applies two *different*
operations — keeping them distinct is the key to the tool:

### Aggregate — *merge a family into one node that stays*

A coloured super-node replaces a whole family, and remains in the graph because
it carries architecture:

| Aggregate | Members |
|-----------|---------|
| `NUMBERS`     | the numeric intrinsics `INT` / `REAL` / `CPX` |
| `ARRAYS`      | `VEC` / `MAT{n}` containers over primitives (incl. nested `EVEC`/`EMAT`) |
| `SHELLS`      | `SHELL`, `SHELL1`, `SHELL1PAIR`, `SHELL1QUARTET`, `SHELL2`, `SHELL4` |
| `GAUSSIANS`   | `GAUSSIAN`, `GAUSSIAN2`, `GAUSSIAN4`, `GAUSSIAN_DATA` |
| `MAPS`        | the `MAP_*_*` hash-map instantiations |
| `ISOSURFACES` | `ISOSURFACE`, `MARCHINGCUBE`, `CAPPING_SQUARE` |

In addition, a `VEC{T}` / `MAT{T}` over a *derived* type is re-pointed to that
type's module (`VEC_ATOM` → `ATOM`), which surfaces real dependencies (e.g.
`MOLECULE → ATOM`) that were otherwise buried inside a container node.

### Ambient — *a universal utility, drawn muted, hidden under `--simplify`*

Seven modules are used by almost everything and carry no architectural signal —
62% of all edges point into them. They are drawn in a muted style in the full
view and **removed** under `--simplify` (noted in a caption instead):

```
NUMBERS  ARRAYS  STR  BIN  TEXTFILE  BUFFER  TABLE_COLUMN
```

**Aggregate is about how a node is built; ambient is about whether it is shown.**
The two overlap: `NUMBERS` and `ARRAYS` are aggregates that are *also* ambient
(built as super-nodes, then hidden when simplifying). `SHELLS`/`GAUSSIANS`/…
are aggregates but never hidden. `STR`/`TEXTFILE`/… are ambient only.

### Usage

```
# Full overview (aggregated; ambient shown muted):
python3 scripts/simplify_callgraph.py build/callgraphs/module_use.dot -o full.dot

# Simplified architecture view (ambient hidden) — 50 nodes / 114 edges:
python3 scripts/simplify_callgraph.py build/callgraphs/module_use.dot --simplify -o simple.dot

# Documentation ego-graph for one module:
python3 scripts/simplify_callgraph.py build/callgraphs/module_use.dot --module ATOM
#   --reverse   show dependents (who uses ATOM) instead of dependencies
#   --both      show   dependents → ATOM → dependencies

dot -Tsvg simple.dot -o simple.svg      # then render with Graphviz
```

The default input path is `build/callgraphs/module_use.dot`, so from a build
whose `callgraphs` target has been run the path argument can be omitted.

## 3. A caveat on the overview: `concentrate=true`

The full and `--simplify` overviews set Graphviz `concentrate=true`, which merges
an edge into a parallel path-bundle to cut clutter. This is **lossy**: a genuine
direct edge disappears from the drawing whenever a longer route to the same node
also exists (e.g. `ATOM → INTERPOLATOR` is a real `use`, but is absorbed into the
`ATOM → COPPENSBASIS → INTERPOLATOR` bundle). For documentation of a specific
module use `--module NAME`, which does **not** concentrate and therefore shows
every direct dependency faithfully.

## 4. On layout — grids and edge "hops"

Graphviz `dot` is already a hierarchical (layered) engine — that is the left→right
ranking you see. It has **no** notion of edge *hops* / bridges (one line arcing over
another at a crossing); that is a yEd / ELK / D3 feature, not available in Graphviz.
A true grid is likewise not `dot`'s model. The effective way to reduce the tangle is
therefore fewer edges (aggregate + hide ambient), not a different layout engine —
`splines=ortho` gives grid-like right angles but, at this edge density, overlaps into
an unreadable mesh with no hops.
