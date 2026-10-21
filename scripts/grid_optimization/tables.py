#!/usr/bin/env python3
"""Tables from the store alone.
Usage: tables.py <basis> [partition=becke|salvador] [hchi=S] [variant=ionic] [stage...]

Each value is found by its grid block, regenerated with scan.py, so a grid
measured once serves every stage that uses it. Errors are in hartree, E_xc
minus the matching reference:
  ref      grid B minus grid A, and the electron count error of A
  cutoffs  each tightened cutoff minus the default, on grid A
  levels   each named level minus grid A
  radial   each count minus the reference count, angular order 59 throughout
  angular  each order minus order 101 everywhere"""
import sys
from collections import defaultdict
from systems import SYSTEMS, EXTRAS
import scan
from tables_lib import ALL, load, periods, rows, pick, E, fmt, diff

basis = sys.argv[1]
args = sys.argv[2:]
partition, hchi, variant, hbecke, stiff = 'becke', None, '', False, None
for a in [a for a in args if '=' in a]:
    k, v = a.split('='); args.remove(a)
    if k == 'partition': partition = v
    elif k == 'hchi': hchi = float(v)
    elif k == 'variant': variant = v
    elif k == 'hbecke': hbecke = v.lower() in ('1', 'true', 'yes')
    elif k == 'stiff': stiff = int(v)
stages = args or ['ref', 'cutoffs', 'levels', 'radial', 'angular']

load(basis, partition, hchi, variant, hbecke, stiff)

if 'ref' in stages:
    print('\n## Reference grids: B minus A, and electron count error on A\n')
    print('| system | points A | points B | E_xc(B) - E_xc(A) | N(A) - N |')
    print('|---|---|---|---|---|')
    for s in ALL:
        a, b = pick(s, 'ref', grid='A'), pick(s, 'ref', grid='B')
        if not a: continue
        q, m, at = ALL[s]
        n = sum(x[0] for x in at) - q
        print('| %s | %s | %s | %s | %s |' % (s, a['n_points'], b['n_points'] if b else '-',
              diff(b, a), fmt(float(a['n_electrons']) - n)))

if 'cutoffs' in stages:
    cuts = ['partition_cutoff', 'rho_cutoff', 'basis_fn_cutoff', 'prune_rho_cutoff', 'all']
    print('\n## Cutoffs tightened, minus the default, on grid A\n')
    print('| system | ' + ' | '.join(cuts) + ' |')
    print('|---|' + '---|' * len(cuts))
    for s in ALL:
        d = pick(s, 'cutoffs', cutoff='default')
        if not d: continue
        print('| %s | ' % s + ' | '.join(diff(pick(s, 'cutoffs', cutoff=c), d) for c in cuts) + ' |')

if 'levels' in stages:
    levs = ['very_low', 'low', 'medium', 'high', 'very_high', 'extreme', 'best']
    print('\n## Present levels: E_xc minus grid A (points in brackets)\n')
    print('| system | ' + ' | '.join(levs) + ' |')
    print('|---|' + '---|' * len(levs))
    worst = defaultdict(float)
    for s in ALL:
        a = pick(s, 'ref', grid='A')
        if not a: continue
        cells = []
        for l in levs:
            r = pick(s, 'levels', level=l)
            if r:
                e = E(r) - E(a)
                if s in SYSTEMS: worst[l] = max(worst[l], abs(e))
                cells.append('%s (%s)' % (fmt(e), r['n_points']))
            else:
                cells.append('-')
        print('| %s | ' % s + ' | '.join(cells) + ' |')
    print('| **largest of the 22** | ' + ' | '.join('**%.1e**' % worst[l] for l in levs) + ' |')

if 'radial' in stages:
    print('\n## Radial: E_xc error against the radial count of one period, angular order 59\n')
    for p in range(1, 8):
        ns = sorted({lab['n'] for s in ALL for lab, r in rows(s, 'radial') if lab['period'] == p and r})
        if not ns: continue
        print('\n### Period %d\n' % p)
        print('| system | ' + ' | '.join(str(n) for n in ns) + ' |')
        print('|---|' + '---|' * len(ns))
        for s in ALL:
            r0 = pick(s, 'radial', period=0)
            if not r0 or p not in periods(s): continue
            print('| %s | ' % s + ' | '.join(diff(pick(s, 'radial', period=p, n=n), r0) for n in ns) + ' |')

if 'angular' in stages:
    print('\n## Angular: E_xc error against the Lebedev order, radial at the reference\n')
    for which in ['heavy', 'H']:
        print('\n### Order varied for %s atoms\n' % ('non-H' if which == 'heavy' else 'H'))
        print('| system | ' + ' | '.join(str(l) for l in scan.ORDERS) + ' |')
        print('|---|' + '---|' * len(scan.ORDERS))
        for s in ALL:
            r0 = pick(s, 'angular', which='none')
            if not r0 or not pick(s, 'angular', which=which, l=scan.ORDERS[0]): continue
            print('| %s | ' % s + ' | '.join(diff(pick(s, 'angular', which=which, l=l), r0) for l in scan.ORDERS) + ' |')
