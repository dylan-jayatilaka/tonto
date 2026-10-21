#!/usr/bin/env python3
"""For each period, the fewest radial points at which the E_xc error of
every system stays below each target for every larger count, from the
radial stage in the store (angular order 59, no pruning).
Usage: radial_needs.py <basis> [partition=becke|salvador] [hbecke=1] [exclude=A,B,...]"""
import sys
from systems import SYSTEMS
import tables_lib as T

basis, part, excl, hbecke, stiff = sys.argv[1], 'becke', [], False, None
for a in sys.argv[2:]:
    k, v = a.split('=')
    if k == 'partition': part = v
    elif k == 'exclude': excl = v.split(',')
    elif k == 'hbecke': hbecke = v.lower() in ('1', 'true', 'yes')
    elif k == 'stiff': stiff = int(v)
    else: raise SystemExit('unknown option ' + a)
T.load(basis, part, hbecke=hbecke, stiff=stiff)
targets = [1e-3, 1e-4, 1e-5, 1e-6, 1e-7]
if excl: print('Leaving out:', ', '.join(excl))
print('| period | systems | ' + ' | '.join('%.0e' % t for t in targets) + ' |')
print('|---|---|' + '---|' * len(targets))
for p in range(1, 8):
    curves = {}
    for s in SYSTEMS:
        if p not in T.periods(s) or s in excl: continue
        r0 = T.pick(s, 'radial', period=0)
        pts = {lab['n']: r for lab, r in T.rows(s, 'radial') if lab.get('period') == p and r}
        if r0 and pts:
            curves[s] = {n: abs(T.E(r) - T.E(r0)) for n, r in pts.items()}
    if not curves: continue
    ns = sorted(set.intersection(*[set(c) for c in curves.values()]))
    worst = {n: max(c[n] for c in curves.values()) for n in ns}
    need = []
    for t in targets:
        ok = [n for n in ns if all(worst[m] < t for m in ns if m >= n)]
        need.append(str(min(ok)) if ok else '>%d' % ns[-1])
    print('| %d | %d | ' % (p, len(curves)) + ' | '.join(need) + ' |')
