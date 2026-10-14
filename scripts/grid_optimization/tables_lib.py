#!/usr/bin/env python3
"""Lookups into the store by grid block, shared by tables.py and the other
analysis scripts. Call load(basis, partition) first."""
from collections import defaultdict
from systems import SYSTEMS, EXTRAS
import store, scan

ALL = {**SYSTEMS, **EXTRAS}
by = defaultdict(dict)

def load(basis, partition='becke', hchi=None, variant='', hbecke=False, stiff=None):
    """Index the stored rows of one partition, its options and variant."""
    scan.PARTITION, scan.HCHI, scan.HBECKE, scan.STIFF = partition, hchi, hbecke, stiff
    by.clear()
    for r in store.load(basis).values():
        if r['grid_code'] == store.GRID_CODE and r.get('integrand', 'xc') == 'xc' \
           and r.get('variant', '') == variant:
            by[r['system']][r['block']] = r

def periods(s):
    return sorted({scan.period(a[0]) for a in ALL[s][2]})

def rows(s, stage):
    """The stored rows of one stage for system s, as (label, row or None)."""
    return [(lab, by[s].get(b)) for lab, b in scan.evaluations(stage, periods(s))]

def pick(s, stage, **kw):
    for lab, r in rows(s, stage):
        if all(lab.get(k) == v for k, v in kw.items()):
            return r

def E(r): return float(r['E_xc'])
def fmt(x): return ('%.1e' % x) if x is not None else '-'
def diff(r, r0): return fmt(E(r) - E(r0)) if r and r0 else '-'
