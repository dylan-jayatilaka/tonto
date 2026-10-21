#!/usr/bin/env python3
"""Angular error by atom and radial shell (put_grid_shell_errors), for each
candidate partition, and the Lebedev order each element needs at each
distance from its nucleus.

Usage, from a run folder:
  shells.py make <basis> <candidate>...   write <basis>/<system>/scan_shells_<candidate>.in
                                          and scanlist.txt, for run_scans.sh
  shells.py collect <basis>               add every finished output to the store,
                                          shells_<basis>.csv beside the results
  shells.py needs <basis> [tau...]        the order needed in each zone of r/R,
                                          by element and candidate

The integrand is Dirac exchange, -(3/4)(3/pi)^(1/3) rho^(4/3), a stand-in for
the XC energy with the same angular structure. dX(L) is a shell's value at
Lebedev order L minus that at L = 101. Restricted densities only. R is the
Bragg-Slater radius of the element, as in Becke's size adjustment.
"""
import csv, glob, os, re, sys, time
from systems import SYSTEMS
import store

ORDERS = [5, 11, 17, 23, 29, 35, 41, 47, 53, 59, 71]
CANDIDATES = {
    'becke':  'partition_scheme= becke',
    'mixed4': 'partition_scheme= salvador partition_h_becke= TRUE',
    'mixed3': 'partition_scheme= salvador partition_h_becke= TRUE salvador_stiffness= 3',
    'ss':     'partition_scheme= stratmann_scuseria',
}
REF_RADIAL = 'n_radial_pts= 100 n_extra_radial_pts= 0 10 20 40 60 80 80'
BOHR_PER_ANGSTROM = 1.8897261
# Bragg-Slater radii in angstrom, from BECKE_GRID, H to Kr
BRAGG = [0.35, 0.35, 1.45, 1.05, 0.85, 0.70, 0.65, 0.60, 0.50, 0.45,
         1.80, 1.50, 1.25, 1.10, 1.00, 1.00, 1.00, 1.00, 2.20, 1.80,
         1.60, 1.40, 1.35, 1.40, 1.40, 1.40, 1.35, 1.35, 1.35, 1.35,
         1.30, 1.25, 1.15, 1.15, 1.15, 1.15]
SYMBOL = {1: 'H', 3: 'Li', 6: 'C', 7: 'N', 8: 'O', 9: 'F', 11: 'Na', 14: 'Si', 15: 'P',
          16: 'S', 17: 'Cl', 19: 'K', 21: 'Sc', 22: 'Ti', 24: 'Cr', 25: 'Mn', 28: 'Ni',
          29: 'Cu', 30: 'Zn', 35: 'Br'}
FIELDS = ['key', 'basis', 'system', 'geom', 'grid_code', 'candidate', 'atom', 'Z', 'shell',
          'r', 'r_over_R', 'N', 'X'] + ['dX%d' % L for L in ORDERS] + ['binary', 'date']

def path(basis):
    return os.path.join(store.root(), 'shells_%s.csv' % basis)

def load(basis):
    p = path(basis)
    if not os.path.exists(p):
        return {}
    with open(p) as f:
        return {r['key']: r for r in csv.DictReader(f)}

def make(basis, cands):
    jobs = []
    for name, (q, m, atoms) in SYSTEMS.items():
        d = os.path.join(basis, name)
        out = os.path.join(d, 'stdout')
        if m != 1:
            continue          # the diagnostic needs a restricted density
        if not (os.path.exists(out) and 'SCF has converged' in open(out).read()):
            continue
        head = open(os.path.join(d, 'header')).read()
        for c in cands:
            block = ('becke_grid= {\n      set_defaults accuracy= best pruning_scheme= none\n'
                     '      %s\n      %s\n   }' % (REF_RADIAL, CANDIDATES[c]))
            open(os.path.join(d, 'scan_shells_%s.in' % c), 'w').write(
                head + '   make_anos_and_interpolators\n   read_archive density_mx r\n   %s\n'
                '   put_grid_shell_errors\n}\n' % block)
            jobs.append('%s shells_%s' % (d, c))
    open('scanlist.txt', 'w').write('\n'.join(jobs) + '\n')
    print(len(jobs), 'shell jobs')

def collect(basis):
    done = load(basis)
    binary = open('tonto.grid.id').read().strip() if os.path.exists('tonto.grid.id') else '?'
    rows = []
    for out in sorted(glob.glob(os.path.join(basis, '*', 'scan_shells_*.out'))):
        text = open(out).read()
        if 'CPU time taken' not in text:
            continue
        system = os.path.basename(os.path.dirname(out))
        cand = os.path.basename(out)[len('scan_shells_'):-4]
        q, m, atoms = SYSTEMS[system]
        geom = store.geom_hash(q, m, atoms)
        when = time.strftime('%Y-%m-%d', time.localtime(os.path.getmtime(out)))
        for blk in re.split(r'\nAtom ', text)[1:]:
            a = int(blk.split()[0])
            Z = atoms[a - 1][0]
            R = BRAGG[Z - 1] * BOHR_PER_ANGSTROM
            for line in blk.splitlines()[1:]:
                t = line.split()
                if len(t) != 7 + len(ORDERS) or not t[0].isdigit():
                    continue
                k = store.key(basis, system, geom, 'shells|%s|%d|%s' % (cand, a, t[0]))
                if k in done:
                    continue
                r = float(t[1])
                row = dict(key=k, basis=basis, system=system, geom=geom, grid_code=store.GRID_CODE,
                           candidate=cand, atom=a, Z=Z, shell=int(t[0]), r=r, r_over_R=r / R,
                           N=t[4], X=t[5], binary=binary, date=when)
                for L, v in zip(ORDERS, t[7:]):
                    row['dX%d' % L] = v
                rows.append(row)
                done[k] = row
    p = path(basis)
    new = not os.path.exists(p)
    with open(p, 'a', newline='') as f:
        w = csv.DictWriter(f, FIELDS)
        if new:
            w.writeheader()
        w.writerows(rows)
    print(len(rows), 'shell rows added to', p)

ZONES = [0, 0.25, 0.5, 1.0, 1.5, 2.0, 3.0, 5.0, 1e9]

def needed_order(row, tau):
    """The lowest order from which every higher order is within tau of L = 101."""
    need = 101
    for L in reversed(ORDERS):
        if abs(float(row['dX%d' % L])) < tau:
            need = L
        else:
            break
    return need

def needs(basis, taus):
    rows = list(load(basis).values())
    cands = [c for c in CANDIDATES if any(r['candidate'] == c for r in rows)]
    zone_names = ['%g-%g' % (ZONES[i], ZONES[i + 1]) if ZONES[i + 1] < 1e8 else '>%g' % ZONES[i]
                  for i in range(len(ZONES) - 1)]
    for tau in taus:
        print('\n## Order needed for every shell to be within %.0e hartree of L = 101\n' % tau)
        print('Largest over all atoms of the element in all systems; zones in r/R.\n')
        print('| element | candidate | ' + ' | '.join(zone_names) + ' |')
        print('|---|---|' + '---|' * len(zone_names))
        for Z in sorted({int(r['Z']) for r in rows}):
            for c in cands:
                cells = []
                for i in range(len(ZONES) - 1):
                    sel = [r for r in rows if int(r['Z']) == Z and r['candidate'] == c
                           and ZONES[i] <= float(r['r_over_R']) < ZONES[i + 1]]
                    cells.append(str(max(needed_order(r, tau) for r in sel)) if sel else '-')
                print('| %s | %s | ' % (SYMBOL.get(Z, Z), c) + ' | '.join(cells) + ' |')

if __name__ == '__main__':
    what, basis = sys.argv[1], sys.argv[2]
    if what == 'make':
        make(basis, sys.argv[3:] or list(CANDIDATES))
    elif what == 'collect':
        collect(basis)
    elif what == 'needs':
        needs(basis, [float(t) for t in sys.argv[3:]] or [1e-6, 1e-7, 1e-8])
