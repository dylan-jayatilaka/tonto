#!/usr/bin/env python3
"""Write the grid scans for every system that has a converged density:
<basis>/<system>/scan_<stage>.in, and scan_<stage>.json listing the grid
settings of each evaluation in order.
Usage: scan.py <basis> [partition=becke|salvador] [hchi=S] [variant=ionic]
               [systems=A,B,...] <stage>...
With partition=salvador every grid uses partition_scheme= salvador and the
files are scan_<stage>_salvador.*; the default is Becke's partition.
hchi=S adds partition_h_chi_scale= S to every Salvador grid, and hbecke=1
partition_h_becke= TRUE (the mixed partition); stiff=K salvador_stiffness= K.
variant=ionic builds the promolecule from the ions of systems.IONIC, and
writes only those systems. systems= limits the systems written.

Stages
  ref      two large unpruned grids, to show the reference is converged
  cutoffs  the reference grid with each cutoff tightened in turn
  radial   angular order 59 everywhere, no pruning; the radial count of one
           period varied while the others stay at the reference count
  angular  reference radial counts, no pruning; the heavy-atom order and the
           hydrogen order varied separately
  levels   the seven named levels as they stand
"""
import os, sys, json
from systems import SYSTEMS, EXTRAS, IONIC
import store

PERIOD_ENDS = [2, 10, 18, 36, 54, 86, 118]
def period(z):
    return next(p + 1 for p, e in enumerate(PERIOD_ENDS) if z <= e)

# Reference radial counts by period, and the larger check grid
REF_A = [100, 110, 120, 140, 160, 180, 180]
REF_B = [140, 155, 170, 200, 230, 260, 260]
L_REF_A, L_REF_B = 101, 131
ORDERS = [11, 17, 23, 29, 35, 41, 47, 53, 59, 65, 71]

PARTITION = 'becke'
HCHI = None
HBECKE = False
STIFF = None

def partition_kw():
    kw = 'partition_scheme= salvador ' if PARTITION == 'salvador' else ''
    if HCHI is not None:
        kw += 'partition_h_chi_scale= %g ' % HCHI
    if HBECKE:
        kw += 'partition_h_becke= TRUE '
    if STIFF is not None:
        kw += 'salvador_stiffness= %d ' % STIFF
    return kw

def grid(counts, l_heavy, l_H, pruning='none', extra=''):
    """A becke_grid block with radial counts given per period."""
    extra = partition_kw() + extra
    n = min(counts)
    ext = ' '.join(str(c - n) for c in counts)
    # One keyword group per line: Tonto's input lines stop at 256 characters
    return ('becke_grid= {\n      set_defaults accuracy= best pruning_scheme= %s\n'
            '      n_radial_pts= %d n_extra_radial_pts= %s\n'
            '      reduce_h_angular_grid= TRUE l_angular_grid= %d l_h_angular_grid= %d l_bonding_angular_grid= %d\n'
            '      %s}' % (pruning, n, ext, l_heavy, l_H, l_heavy, extra))

def evaluations(stage, periods):
    ev = []   # (label dict, block)
    if stage == 'ref':
        ev.append(({'grid': 'A'}, grid(REF_A, L_REF_A, L_REF_A)))
        ev.append(({'grid': 'B'}, grid(REF_B, L_REF_B, L_REF_B)))
    elif stage == 'cutoffs':
        base = {'grid': 'A'}
        ev.append((dict(base, cutoff='default'), grid(REF_A, L_REF_A, L_REF_A)))
        for kw, v in [('partition_cutoff', '1e-10'), ('rho_cutoff', '1e-14'),
                      ('basis_fn_cutoff', '1e-14'), ('prune_rho_cutoff', '0')]:
            ev.append((dict(base, cutoff=kw, value=v), grid(REF_A, L_REF_A, L_REF_A, extra='%s= %s ' % (kw, v))))
        ev.append((dict(base, cutoff='all'), grid(REF_A, L_REF_A, L_REF_A,
                   extra='partition_cutoff= 1e-10 rho_cutoff= 1e-14 basis_fn_cutoff= 1e-14 prune_rho_cutoff= 0 ')))
    elif stage == 'radial':
        ev.append(({'period': 0, 'n': 0}, grid(REF_A, 59, 59)))
        for p in periods:
            top = REF_A[p - 1]
            for c in range(15, top, 5):
                counts = [max(c, REF_A[q]) if q != p - 1 else c for q in range(7)]
                ev.append(({'period': p, 'n': c}, grid(counts, 59, 59)))
    elif stage == 'angular':
        ev.append(({'which': 'none', 'l': 0}, grid(REF_A, L_REF_A, L_REF_A)))
        for l in ORDERS:
            ev.append(({'which': 'heavy', 'l': l}, grid(REF_A, l, L_REF_A)))
            if 1 in periods:
                ev.append(({'which': 'H', 'l': l}, grid(REF_A, L_REF_A, l)))
    elif stage == 'levels':
        for acc in ['very_low', 'low', 'medium', 'high', 'very_high', 'extreme', 'best']:
            ev.append(({'level': acc}, 'becke_grid= { set_defaults accuracy= %s %s}' % (acc, partition_kw())))
    return ev

def main():
    global PARTITION, HCHI, HBECKE, STIFF
    basis, stages = sys.argv[1], sys.argv[2:]
    variant, only = '', None
    for a in [a for a in stages if '=' in a]:
        k, v = a.split('='); stages.remove(a)
        if k == 'partition': PARTITION = v
        elif k == 'hchi': HCHI = float(v)
        elif k == 'hbecke': HBECKE = v.lower() in ('1', 'true', 'yes')
        elif k == 'stiff': STIFF = int(v)
        elif k == 'variant': variant = v
        elif k == 'systems': only = v.split(',')
        else: raise SystemExit('unknown option ' + a)
    assert PARTITION in ('becke', 'salvador'), PARTITION
    assert variant in ('', 'ionic'), variant
    tag = '' if PARTITION == 'becke' else '_' + PARTITION
    if HCHI is not None: tag += '_hchi%g' % HCHI
    if HBECKE: tag += '_hbecke'
    if STIFF is not None: tag += '_stiff%d' % STIFF
    if variant: tag += '_' + variant
    jobs = []
    for name, (q, m, atoms) in {**SYSTEMS, **EXTRAS}.items():
        if only and name not in only: continue
        if variant == 'ionic' and name not in IONIC: continue
        d = os.path.join(basis, name)
        genre = 'r' if m == 1 else 'u'
        out = os.path.join(d, 'stdout')
        if not (os.path.exists(out) and 'SCF has converged' in open(out).read()):
            print('no converged density yet:', name); continue
        periods = sorted({period(a[0]) for a in atoms})
        head = open(os.path.join(d, 'header')).read()
        if variant == 'ionic':
            states = ' '.join('%d %d' % qm for qm in IONIC[name])
            assert '      max_iterations= 200' in head
            extra = '\n      initial_atomic_states= { %s }' % states
            # The atomic SCF of a 3d ion needs damping and level shifting kept on
            if any(21 <= a[0] <= 30 for a in atoms) and 'damp_factor' not in head:
                extra += '\n      damp_factor= 0.5 damp_finish= 60 level_shift= 0.5 level_shift_finish= 60'
            head = head.replace('      max_iterations= 200', '      max_iterations= 200' + extra, 1)
        geom = store.geom_hash(q, m, atoms)
        done = store.load(basis)
        for stage in stages:
            ev = [(dict(lab, partition=PARTITION, variant=variant, hchi=HCHI, hbecke=HBECKE, stiff=STIFF), b)
                  for lab, b in evaluations(stage, periods)
                  if store.key(basis, name, geom, b, variant=variant) not in done]
            if not ev:
                continue
            body = ''.join('   %s\n   put_xc_energy_on_grid\n' % b for _, b in ev)
            open(os.path.join(d, 'scan_%s%s.in' % (stage, tag)), 'w').write(
                head + '   make_anos_and_interpolators\n   read_archive density_mx %s\n' % genre + body + '}\n')
            json.dump({'geom': geom, 'variant': variant, 'evaluations': [[lab, b] for lab, b in ev]},
                      open(os.path.join(d, 'scan_%s%s.json' % (stage, tag)), 'w'))
            jobs.append('%s %s%s' % (d, stage, tag))
    open('scanlist.txt', 'w').write('\n'.join(jobs) + '\n')
    print(len(jobs), 'scan jobs')

if __name__ == '__main__':
    main()
