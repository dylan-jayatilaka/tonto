#!/usr/bin/env python3
"""Cross-submodule procedure-name collision scan.

For each class that is split across a main module + submodules (module CLASS,
module CLASS.SUB1, module CLASS.SUB2, ...), find procedure base names that are
defined in MORE THAN ONE of those files. Those are the names that become
ambiguous once the `.SUBMOD:` qualifier is dropped and `.proc` must resolve on
its own -- and (per the maintainer) must be renamed unique to the parent class.

Mirrors the translator's buildSubmoduleProcRegistry heuristic: after `contains`,
a 3-space-indented `name(` / `name ::` / `name result` / `name` line is a
procedure header (deeper-indented body lines and keywords excluded).
"""
import os, re, glob
from collections import defaultdict

FOO = 'foofiles'
MOD = re.compile(r'^\s*(?:virtual\s+)?(?:array\s+)?module\s+(\S+)', re.I)
PROC = re.compile(r'^ {3}([A-Za-z]\w*)\s*(\(|::|result\b|!|$)')
KW = {'end','contains','interface','use','module','result','then','else',
      'elsewhere','do','select','case','where','forall','if','subroutine',
      'function','type','data','implicit','none','return','exit','cycle'}

# class -> proc_base -> set(submodule identity)
reg = defaultdict(lambda: defaultdict(set))
files_by_class = defaultdict(set)

for path in sorted(glob.glob(os.path.join(FOO, '*.foo'))):
    try:
        lines = open(path, encoding='utf-8', errors='replace').read().splitlines()
    except Exception:
        continue
    modname = None
    for ln in lines:
        m = MOD.search(ln)
        if m:
            modname = m.group(1)
            break
    if not modname:
        continue
    if '.' in modname:
        cls, sub = modname.split('.', 1)
    else:
        cls, sub = modname, 'MAIN'
    cls = cls.upper()
    files_by_class[cls].add(sub.upper())
    past = False
    for ln in lines:
        if ln.strip().lower() == 'contains':
            past = True; continue
        if not past:
            continue
        pm = PROC.match(ln)
        if not pm:
            continue
        name = pm.group(1)
        if name.lower() in KW:
            continue
        # Skip ALL-CAPS names: these are CPP macros (ENSURE, DIE_IF, STACK, ...)
        # invoked in bodies, not procedure definitions.
        if re.fullmatch(r'[A-Z][A-Z0-9_]*', name):
            continue
        reg[cls][name].add(sub.upper())

# Report
multi_classes = {c for c, subs in files_by_class.items() if len(subs) > 1}
print("=== classes split across submodules (%d) ===" % len(multi_classes))
for c in sorted(multi_classes):
    print("  %-20s submodules: %s" % (c, ', '.join(sorted(files_by_class[c]))))

print("\n=== COLLISIONS: proc name defined in >1 submodule of the same class ===")
total = 0
for c in sorted(multi_classes):
    hits = {name: subs for name, subs in reg[c].items() if len(subs) > 1}
    if not hits:
        continue
    print("\n%s  (%d colliding names):" % (c, len(hits)))
    for name in sorted(hits):
        print("    %-40s in: %s" % (name, ', '.join(sorted(hits[name]))))
        total += 1

print("\n=== SUMMARY ===")
nproc = sum(len(v) for v in reg.values())
print("classes with submodules : %d" % len(multi_classes))
print("distinct proc names seen : %d" % nproc)
print("colliding names (need renaming) : %d" % total)
