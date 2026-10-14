#!/usr/bin/env python3
"""Assemble trial grids for each candidate partition from what is stored,
evaluate them, and choose the cheapest grid that meets each level's target.

A trial grid is the "zones" pruning scheme with
  - radial counts per period that keep the radial error below RADIAL_TARGET
    (from the radial stage, KF, LiF and ScO left out), and
  - zone orders per period that keep every shell within TAU of L = 101
    (from the shell tables), zones at 0.5, 1 and 2.5 Bragg-Slater radii.

Usage, from a run folder:
  assemble.py make <basis> <candidate>...   write scan_zones_<candidate>.in and
                                            .json for every system, and scanlist.txt
  assemble.py report <basis> <candidate>... error and points of every trial grid,
                                            and the cheapest grid for each level
Then run_scans.sh as usual; collect.py stores the results.
"""
import json, os, sys
from collections import defaultdict
from systems import SYSTEMS
import store, scan, shells
import tables_lib as T

CANDIDATES = {
    'becke':  dict(partition='becke'),
    'mixed4': dict(partition='salvador', hbecke=True),
    'mixed3': dict(partition='salvador', hbecke=True, stiff=3),
    'ss':     dict(partition='stratmann_scuseria'),
}
RADIAL_TARGETS = [1e-4, 1e-5, 1e-6, 1e-7]
TAUS = [1e-5, 1e-6, 1e-7, 1e-8]
# very_high has no target: 1e-7 cannot be shown against the reference grid
LEVELS = [('very_low', 1e-3), ('low', 1e-4), ('medium', 1e-5), ('high', 1e-6)]
OUTLIERS = ('KF', 'LiF', 'ScO')
ZONES = [0.0, 0.5, 1.0, 2.5, 1e9]

def radial_counts(target):
    """Radial points per period 1 to 4 that keep every system's error below
    target for every larger count; T must be loaded for the candidate."""
    counts = {}
    for p in range(1, 5):
        curves = []
        for s in SYSTEMS:
            if p not in T.periods(s) or s in OUTLIERS: continue
            r0 = T.pick(s, 'radial', period=0)
            pts = {lab['n']: r for lab, r in T.rows(s, 'radial') if lab.get('period') == p and r}
            if r0 and pts:
                curves.append({n: abs(T.E(r) - T.E(r0)) for n, r in pts.items()})
        ns = sorted(set.intersection(*[set(c) for c in curves]))
        ok = [n for n in ns if all(max(c[m] for c in curves) < target for m in ns if m >= n)]
        counts[p] = min(ok) if ok else ns[-1] + 10
    return counts

def zone_orders(cand, tau):
    """Lebedev order per zone for periods 1 to 4 from the shell tables."""
    rows = [r for r in shells.load(basis).values() if r['candidate'] == cand]
    orders = {}
    for p in range(1, 5):
        sel = [r for r in rows if scan.period(int(r['Z'])) == p]
        orders[p] = []
        for k in range(4):
            z = [r for r in sel if ZONES[k] <= float(r['r_over_R']) < ZONES[k + 1]]
            orders[p].append(max(shells.needed_order(r, tau) for r in z) if z else 11)
    return orders

def block(cand, rt, tau):
    T.load(basis, **CANDIDATES[cand])
    rc = radial_counts(rt)
    zo = zone_orders(cand, tau)
    counts = [rc[1], rc[2], rc[3], rc[4], rc[4] + 20, rc[4] + 40, rc[4] + 40]
    n = min(counts)
    flat = []
    for p in range(1, 8):
        flat += zo.get(p, [0, 0, 0, 0])
    return ('becke_grid= {\n      set_defaults accuracy= best pruning_scheme= zones\n'
            '      n_radial_pts= %d n_extra_radial_pts= %s\n'
            '      zone_orders= %s\n      %s}' % (n, ' '.join(str(c - n) for c in counts),
                                                  ' '.join(str(o) for o in flat), scan.partition_kw()))

def trials(cand):
    return [({'stage': 'zones', 'candidate': cand, 'radial_target': rt, 'tau': tau}, block(cand, rt, tau))
            for rt in RADIAL_TARGETS for tau in TAUS]

def make(cands):
    jobs = []
    for cand in cands:
        tr = trials(cand)
        done = store.load(basis)
        for name, (q, m, atoms) in SYSTEMS.items():
            d = os.path.join(basis, name)
            out = os.path.join(d, 'stdout')
            if not (os.path.exists(out) and 'SCF has converged' in open(out).read()):
                continue
            geom = store.geom_hash(q, m, atoms)
            ev = [(lab, b) for lab, b in tr if store.key(basis, name, geom, b) not in done]
            if not ev:
                continue
            genre = 'r' if m == 1 else 'u'
            head = open(os.path.join(d, 'header')).read()
            body = ''.join('   %s\n   put_xc_energy_on_grid\n' % b for _, b in ev)
            open(os.path.join(d, 'scan_zones_%s.in' % cand), 'w').write(
                head + '   make_anos_and_interpolators\n   read_archive density_mx %s\n' % genre + body + '}\n')
            json.dump({'geom': geom, 'variant': '', 'evaluations': [[lab, b] for lab, b in ev]},
                      open(os.path.join(d, 'scan_zones_%s.json' % cand), 'w'))
            jobs.append('%s zones_%s' % (d, cand))
    open('scanlist.txt', 'w').write('\n'.join(jobs) + '\n')
    print(len(jobs), 'zone jobs')

def report(cands):
    for cand in cands:
        tr = trials(cand)
        T.load(basis, **CANDIDATES[cand])
        print('\n## %s\n' % cand)
        print('| radial target | tau | largest error, 22 | largest, without KF LiF ScO | points per atom |')
        print('|---|---|---|---|---|')
        res = []
        for lab, b in tr:
            errs, errs_in, pts, atoms = [], [], 0, 0
            for s in SYSTEMS:
                a = T.pick(s, 'ref', grid='A')
                r = T.by[s].get(b)
                if not (a and r): continue
                e = abs(T.E(r) - T.E(a))
                errs.append(e)
                if s not in OUTLIERS: errs_in.append(e)
                pts += int(r['n_points']); atoms += len(SYSTEMS[s][2])
            if not errs: continue
            res.append((lab, max(errs), max(errs_in), pts / atoms))
            print('| %.0e | %.0e | %.1e | %.1e | %.0f |' % (lab['radial_target'], lab['tau'], max(errs), max(errs_in), pts / atoms))
        print('\nCheapest grid meeting each target (without KF, LiF, ScO):\n')
        print('| level | target | radial target | tau | largest error | points per atom |')
        print('|---|---|---|---|---|---|')
        for name, t in LEVELS:
            ok = [x for x in res if x[2] < t]
            if ok:
                lab, e, ei, ppa = min(ok, key=lambda x: x[3])
                print('| %s | %.0e | %.0e | %.0e | %.1e | %.0f |' % (name, t, lab['radial_target'], lab['tau'], ei, ppa))
            else:
                print('| %s | %.0e | none | | | |' % (name, t))

if __name__ == '__main__':
    what, basis = sys.argv[1], sys.argv[2]
    cands = sys.argv[3:] or list(CANDIDATES)
    {'make': make, 'report': report}[what](cands)
