#!/usr/bin/env python3
"""Lower a zone-scheme grid one setting at a time while it still meets its
target, for each candidate partition and level.

The settings are the radial counts of periods 1 to 4 and the zone orders of
periods 1 to 4 (periods 5 to 7 follow period 4). Each round tries every
single-step reduction (5 fewer radial points, or the next lower Lebedev
order), all in one job per system; then all the passing reductions together,
then the better half of them, then the best one alone. A grid passes if its
largest E_xc error over the test systems, KF, LiF and ScO left out, is below
MARGIN times the target. The start is the cheapest grid of assemble.py.

State is kept in the store (descent_<basis>.json) after every round, so a
stopped run continues where it stopped; progress is written to
descent_status.md in the run folder.

Usage, from a run folder: descend.py <basis> [candidate:level ...]
"""
import json, os, subprocess, sys, time
from systems import SYSTEMS
import store, scan, assemble
import tables_lib as T

ORDERS = [5, 11, 17, 23, 29, 35, 41, 47, 53, 59, 71, 83, 101]
MARGIN = 0.9
OUTLIERS = assemble.OUTLIERS
HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_RUNS = ['%s:%s' % (c, l) for l in ('medium', 'low', 'high', 'very_low')
                for c in ('mixed4', 'becke', 'mixed3')]

def block(cand, g):
    T.load(basis, **assemble.CANDIDATES[cand])
    r = g['r']
    counts = [r[0], r[1], r[2], r[3], r[3] + 20, r[3] + 40, r[3] + 40]
    n = min(counts)
    flat = []
    for p in range(4):
        flat += g['z'][p]
    flat += [0] * 12
    return ('becke_grid= {\n      set_defaults accuracy= best pruning_scheme= zones\n'
            '      n_radial_pts= %d n_extra_radial_pts= %s\n'
            '      zone_orders= %s\n      %s}' % (n, ' '.join(str(c - n) for c in counts),
                                                  ' '.join(str(o) for o in flat), scan.partition_kw()))

def start(cand, level, target):
    """The cheapest assembled grid meeting the target, as settings."""
    best = None
    T.load(basis, **assemble.CANDIDATES[cand])
    for rt in assemble.RADIAL_TARGETS:
        for tau in assemble.TAUS:
            b = assemble.block(cand, rt, tau)
            m = measure(cand, b)
            if m and m[0] < target and (best is None or m[1] < best[2]):
                best = (rt, tau, m[1])
    if best is None:
        return None
    T.load(basis, **assemble.CANDIDATES[cand])
    rc = assemble.radial_counts(best[0])
    zo = assemble.zone_orders(cand, best[1])
    return {'r': [rc[p] for p in range(1, 5)], 'z': [zo[p] for p in range(1, 5)]}

def measure(cand, b):
    """(largest error without the outliers, points per atom) of block b, or None."""
    T.load(basis, **assemble.CANDIDATES[cand])
    errs, pts, atoms = [], 0, 0
    for s in SYSTEMS:
        a = T.pick(s, 'ref', grid='A')
        r = T.by[s].get(b)
        if not (a and r):
            return None
        if s not in OUTLIERS:
            errs.append(abs(T.E(r) - T.E(a)))
        pts += int(r['n_points']); atoms += len(SYSTEMS[s][2])
    return max(errs), pts / atoms

def evaluate(cand, level, blocks):
    """Run whatever of these blocks is not yet stored, on every system."""
    jobs = []
    done = store.load(basis)
    for name, (q, m, atoms) in SYSTEMS.items():
        d = os.path.join(basis, name)
        geom = store.geom_hash(q, m, atoms)
        ev = [b for b in blocks if store.key(basis, name, geom, b) not in done]
        if not ev:
            continue
        genre = 'r' if m == 1 else 'u'
        head = open(os.path.join(d, 'header')).read()
        body = ''.join('   %s\n   put_xc_energy_on_grid\n' % b for b in ev)
        tag = 'descent_%s_%s' % (cand, level)
        open(os.path.join(d, 'scan_%s.in' % tag), 'w').write(
            head + '   make_anos_and_interpolators\n   read_archive density_mx %s\n' % genre + body + '}\n')
        json.dump({'geom': geom, 'variant': '',
                   'evaluations': [[{'stage': 'descent', 'candidate': cand, 'level': level}, b] for b in ev]},
                  open(os.path.join(d, 'scan_%s.json' % tag), 'w'))
        jobs.append('%s %s' % (d, tag))
    if not jobs:
        return
    open('scanlist.txt', 'w').write('\n'.join(jobs) + '\n')
    subprocess.run([os.path.join(HERE, 'run_scans.sh'), basis], stdout=open('descent_runs.log', 'a'),
                   stderr=subprocess.STDOUT, check=False)

def moves(g):
    """Every single-step reduction of the settings g."""
    out = []
    for p in range(4):
        if g['r'][p] > 15:
            h = json.loads(json.dumps(g)); h['r'][p] -= 5
            out.append((('r', p), h))
        for k in range(4):
            L = g['z'][p][k]
            lower = [o for o in ORDERS if o < L]
            if lower:
                h = json.loads(json.dumps(g)); h['z'][p][k] = lower[-1]
                out.append((('z', p, k), h))
    return out

def combine(g, chosen):
    h = json.loads(json.dumps(g))
    for mv, _ in chosen:
        if mv[0] == 'r': h['r'][mv[1]] -= 5
        else: h['z'][mv[1]][mv[2]] = [o for o in ORDERS if o < g['z'][mv[1]][mv[2]]][-1]
    return h

def status(state):
    lines = ['# Descent status, %s\n' % time.strftime('%Y-%m-%d %H:%M'),
             '| candidate | level | round | done | largest error | points per atom | radial 1-4 | zone orders, periods 1-4 |',
             '|---|---|---|---|---|---|---|---|']
    for key, st in state.items():
        c, l = key.split(':')
        lines.append('| %s | %s | %d | %s | %.1e | %.0f | %s | %s |' % (
            c, l, st['round'], st['done'], st['err'], st['ppa'], st['g']['r'],
            ' / '.join(' '.join(str(o) for o in z) for z in st['g']['z'])))
    open('descent_status.md', 'w').write('\n'.join(lines) + '\n')

def main(runs):
    sfile = os.path.join(store.root(), 'descent_%s.json' % basis)
    state = json.load(open(sfile)) if os.path.exists(sfile) else {}
    targets = dict(assemble.LEVELS)
    for run in runs:
        cand, level = run.split(':')
        target = targets[level] * MARGIN
        if run not in state:
            g = start(cand, level, targets[level])
            if g is None:
                print(run, 'no assembled grid meets the target'); continue
            b = block(cand, g); evaluate(cand, level, [b]); m = measure(cand, b)
            state[run] = {'g': g, 'err': m[0], 'ppa': m[1], 'round': 0, 'done': False, 'history': []}
            json.dump(state, open(sfile, 'w'), indent=1); status(state)
        st = state[run]
        while not st['done']:
            g = st['g']
            mv = moves(g)
            evaluate(cand, level, [block(cand, h) for _, h in mv])
            passing = []
            for m_, h in mv:
                r = measure(cand, block(cand, h))
                if r and r[0] < target and r[1] < st['ppa']:
                    passing.append((m_, h, r))
            st['round'] += 1
            if not passing:
                st['done'] = True
            else:
                passing.sort(key=lambda x: x[2][1])
                accepted = None
                for subset in (passing, passing[:max(1, len(passing) // 2)]):
                    if len(subset) < 2: continue
                    h = combine(g, [(m_, h_) for m_, h_, _ in subset])
                    b = block(cand, h); evaluate(cand, level, [b]); r = measure(cand, b)
                    if r and r[0] < target and r[1] < st['ppa']:
                        accepted = (h, r); break
                if accepted is None:
                    accepted = (passing[0][1], passing[0][2])
                st['g'], (st['err'], st['ppa']) = accepted[0], accepted[1]
                st['history'].append([st['round'], st['err'], st['ppa']])
            json.dump(state, open(sfile, 'w'), indent=1)
            status(state)
        print(run, 'done: error %.1e, %.0f points per atom' % (st['err'], st['ppa']), flush=True)

def set_basis(b):
    global basis
    basis = assemble.basis = b

if __name__ == '__main__':
    set_basis(sys.argv[1])
    main(sys.argv[2:] or DEFAULT_RUNS)
