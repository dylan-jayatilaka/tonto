#!/usr/bin/env python3
"""Prototype: simplify module_use.dot for reading / documentation.

Two independent operations (this is the terminology):

  AGGREGATE (merge)  -- collapse a family of related modules into ONE node
      that STAYS in the graph, coloured. Semantic groups carry architecture:
        NUMBERS  <- INT/REAL/CPX            ARRAYS <- VEC/MAT of primitives
        SHELLS   <- SHELL/SHELL1/2/4/...    GAUSSIANS <- GAUSSIAN/2/4/_DATA
        MAPS     <- MAP_*                   ISOSURFACES <- ISOSURFACE/MARCHINGCUBE/...
      VEC{T}/MAT{T} over a *derived* type re-points to that type (VEC_ATOM->ATOM).

  AMBIENT (hide)     -- universally-used low-level utilities. They are drawn in
      a uniform muted style in the full view, and REMOVED (with a legend note)
      under --simplify. Being ambient is about *display*, not construction:
        STR BIN TEXTFILE BUFFER TABLE_COLUMN  (plain utility modules)
        NUMBERS ARRAYS                        (aggregates that are also universal)
      So NUMBERS/ARRAYS are BOTH aggregate and ambient; SHELLS/GAUSSIANS/MAPS/
      ISOSURFACES are aggregate but NOT ambient; STR/TEXTFILE/... are ambient only.

Modes:
  (default)         full collapsed graph, ambient shown muted, concentrate=true
  --simplify        ambient hidden -> the readable architecture view
  --module NAME     documentation ego-graph: NAME + its DIRECT (non-ambient)
                    dependencies only. No concentrate -> every direct edge shown.
"""
import re, sys, argparse
from collections import Counter

PRIMS   = {"INT", "REAL", "CPX", "BIN", "STR"}
NUMERIC = {"INT", "REAL", "CPX"}
CONTAINER = re.compile(r"^(VEC|MAT[0-9]?|EVEC|EMAT[0-9]?)$")

REMOVE  = {"BREAKDOWN_DATA", "MULTI_T_ADP"}          # dead: dropped entirely

# universal utilities: muted in full view, hidden under --simplify
AMBIENT = {"NUMBERS", "ARRAYS", "STR", "BIN", "TEXTFILE", "BUFFER", "TABLE_COLUMN"}

# semantic aggregates that are NOT ambient -> keep, colour by family
AGG_FILL = {"SHELLS": "#e6dff0", "GAUSSIANS": "#dff0e6",
            "MAPS": "#f0ecd9", "ISOSURFACES": "#f0dfe0"}

GROUP = {}
for _m in ("SHELL", "SHELL1", "SHELL1PAIR", "SHELL1QUARTET", "SHELL2", "SHELL4"):
    GROUP[_m] = "SHELLS"
for _m in ("GAUSSIAN", "GAUSSIAN2", "GAUSSIAN4", "GAUSSIAN_DATA"):
    GROUP[_m] = "GAUSSIANS"
for _m in ("MAP_INT_INT", "MAP_INT_STR", "MAP_STR_INT", "MAP_VEC_INT_VEC_INT"):
    GROUP[_m] = "MAPS"
for _m in ("ISOSURFACE", "MARCHINGCUBE", "CAPPING_SQUARE"):
    GROUP[_m] = "ISOSURFACES"

def strip_mod(n):
    return n[:-7] if n.endswith("_MODULE") else n

def classify(node):
    base = strip_mod(node)
    toks = base.split("_")
    if not CONTAINER.match(toks[0]):
        return "NUMBERS" if base in NUMERIC else base
    i = 0
    while i < len(toks) and CONTAINER.match(toks[i]):
        i += 1
    rem = toks[i:]
    if len(rem) == 1 and rem[0] in PRIMS:
        return "ARRAYS"
    return "_".join(rem)

def represent(node):
    return GROUP.get(classify(node), classify(node))

def collapse(path):
    """Return the set of collapsed (a,b) edges from module_use.dot."""
    edge = re.compile(r'"([^"]+)"\s*->\s*"([^"]+)"')
    edges = set()
    with open(path) as f:
        for line in f:
            m = edge.search(line)
            if not m:
                continue
            a, b = represent(m.group(1)), represent(m.group(2))
            if a in REMOVE or b in REMOVE or a == b:
                continue
            edges.add((a, b))
    return edges

def node_decl(n, focus=None):
    if n == focus:
        return '  "%s" [style="filled,bold,rounded", fillcolor="#cfe3ff", penwidth=2];' % n
    if n in AMBIENT:
        return ('  "%s" [style="filled,dashed,rounded", fillcolor="#efefef", '
                'color="#9aa0aa", fontcolor="#6a6f78"];' % n)
    if n in AGG_FILL:
        return '  "%s" [style="filled,rounded", fillcolor="%s"];' % (n, AGG_FILL[n])
    return None

def emit(edges, name, hide_ambient=False, concentrate=False, focus=None):
    if hide_ambient:
        edges = {(a, b) for a, b in edges if a not in AMBIENT and b not in AMBIENT}
    nodes = set()
    for a, b in edges:
        nodes.add(a); nodes.add(b)
    out = ["digraph %s {" % name, "  rankdir=LR;",
           "  node [shape=box, style=rounded];"]
    if concentrate:
        out.append("  concentrate=true;")
    for n in sorted(nodes):
        d = node_decl(n, focus)
        if d:
            out.append(d)
    for a, b in sorted(edges):
        out.append('  "%s" -> "%s";' % (a, b))
    out.append("}")
    return "\n".join(out) + "\n", len(nodes), len(edges)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("dot", nargs="?", default="build/callgraphs/module_use.dot")
    ap.add_argument("--simplify", action="store_true", help="hide ambient utilities")
    ap.add_argument("--module", metavar="NAME",
                    help="ego-graph: NAME + its direct non-ambient dependencies")
    ap.add_argument("--reverse", action="store_true",
                    help="with --module: show dependents (who uses NAME) instead of uses")
    ap.add_argument("--both", action="store_true",
                    help="with --module: show both dependents -> NAME -> dependencies")
    ap.add_argument("-o", "--out", help="output .dot path")
    args = ap.parse_args()

    edges = collapse(args.dot)

    if args.module:
        raw = args.module if args.module.endswith("_MODULE") else args.module + "_MODULE"
        focus = represent(raw)
        # direct neighbours, non-ambient, excluding self
        uses   = sorted({b for a, b in edges if a == focus and b not in AMBIENT and b != focus})
        usedby = sorted({a for a, b in edges if b == focus and a not in AMBIENT and a != focus})
        if args.both:
            ego = {(focus, b) for b in uses} | {(a, focus) for a in usedby}
        elif args.reverse:
            ego = {(a, focus) for a in usedby}
        else:
            ego = {(focus, b) for b in uses}
        dot, nn, ne = emit(ego, "module_%s" % focus.lower(),
                           concentrate=False, focus=focus)
        name = args.out or ("module_%s.dot" % focus.lower())
        with open(name, "w") as f:
            f.write(dot)
        amb = sorted({b for a, b in edges if a == focus and b in AMBIENT})
        sys.stderr.write("%s (%s):\n" % (focus,
            "both" if args.both else "dependents" if args.reverse else "dependencies"))
        sys.stderr.write("  uses (%d)   -> %s\n" % (len(uses), ", ".join(uses) or "(none)"))
        sys.stderr.write("  used-by (%d): %s\n" % (len(usedby), ", ".join(usedby) or "(none)"))
        sys.stderr.write("  (ambient deps, hidden: %s)\n" % (", ".join(amb) or "(none)"))
        sys.stderr.write("  wrote %s\n" % name)
        return

    dot, nn, ne = emit(edges, "module_use_collapsed",
                       hide_ambient=args.simplify, concentrate=True)
    name = args.out or ("module_use_%s.dot" % ("simplify" if args.simplify else "collapsed"))
    with open(name, "w") as f:
        f.write(dot)
    sys.stderr.write("%s: %d nodes, %d edges -> %s\n"
                     % ("simplify" if args.simplify else "full", nn, ne, name))

if __name__ == "__main__":
    main()
