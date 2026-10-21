#!/usr/bin/env python3
"""The results store: one CSV file per basis set, one row per evaluation of
an integrand (the XC energy, or later the exchange energy for COSX) on a
fixed converged density.

The store lives outside the run folder so that it survives it:
$GRID_STORE, or ~/Dropbox/tonto_data/grid_optimization by default.

A row is identified by its key: system, geometry hash, basis, the grid block
exactly as given to Tonto, GRID_CODE, and the integrand when it is not "xc".
Bump GRID_CODE by hand when a code change alters what a given grid block
means; rows with the old label are kept but no longer count as done.
"""
import csv, hashlib, json, os

GRID_CODE = 'g1'      # grid code as of the n_extra_radial_pts change
FIELDS = ['key', 'basis', 'system', 'geom', 'grid_code', 'integrand', 'variant', 'stage', 'label',
          'block', 'n_points', 'n_unpruned', 'n_electrons', 'E_xc', 'binary', 'date']

def root():
    return os.environ.get('GRID_STORE', os.path.expanduser('~/Dropbox/tonto_data/grid_optimization'))

def geom_hash(q, m, atoms):
    s = json.dumps([q, m, [[a[0]] + [round(x, 6) for x in a[1:]] for a in atoms]])
    return hashlib.sha1(s.encode()).hexdigest()[:10]

def key(basis, system, geom, block, integrand='xc', variant=''):
    """A variant names a change in the job outside the grid block, such as
    an ionic promolecule; empty for none."""
    parts = [basis, system, geom, GRID_CODE, block] + ([integrand] if integrand != 'xc' else []) \
          + (['variant=' + variant] if variant else [])
    return hashlib.sha1('|'.join(parts).encode()).hexdigest()[:16]

def path(basis):
    return os.path.join(root(), 'results_%s.csv' % basis)

def load(basis):
    p = path(basis)
    if not os.path.exists(p):
        return {}
    with open(p) as f:
        rows = {}
        for r in csv.DictReader(f):
            r.setdefault('integrand', 'xc')
            r['variant'] = r.get('variant') or ''
            rows[r['key']] = r
        return rows

def append(basis, rows):
    os.makedirs(root(), exist_ok=True)
    p = path(basis)
    new = not os.path.exists(p)
    with open(p, 'a', newline='') as f:
        w = csv.DictWriter(f, FIELDS)
        if new:
            w.writeheader()
        for r in rows:
            r.setdefault('integrand', 'xc')
            r.setdefault('variant', '')
            w.writerow(r)
