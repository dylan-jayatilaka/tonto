#!/usr/bin/env python3
"""Read every finished scan_<stage>.out under <basis>/ and add its
evaluations to the store. Safe to run any number of times: rows already
stored are skipped, and an output cut short adds only what it finished.
Usage: collect.py <basis>"""
import glob, json, os, re, sys, time
import store

basis = sys.argv[1]
done = store.load(basis)
binary = open('tonto.grid.id').read().strip() if os.path.exists('tonto.grid.id') else '?'  # in the run folder
rows = []
for js in sorted(glob.glob(os.path.join(basis, '*', 'scan_*.json'))):
    out = js[:-5] + '.out'
    if not os.path.exists(out):
        continue
    system = os.path.basename(os.path.dirname(js))
    stage = os.path.basename(js)[5:-5].split('_')[0]
    meta = json.load(open(js))
    text = open(out).read()
    vals = re.findall(r'No\. of grid points \.+ +(\d+)\nNo\. before pruning \.+ +(\d+)\n'
                      r'No\. of electrons \.+ +(\S+)\nXC energy \.+ +(\S+)', text)
    when = time.strftime('%Y-%m-%d', time.localtime(os.path.getmtime(out)))
    for (lab, block), v in zip(meta['evaluations'], vals):
        variant = meta.get('variant', '')
        k = store.key(basis, system, meta['geom'], block, variant=variant)
        if k in done:
            continue
        rows.append(dict(key=k, basis=basis, system=system, geom=meta['geom'],
                         grid_code=store.GRID_CODE, stage=stage, label=json.dumps(lab),
                         block=block, n_points=v[0], n_unpruned=v[1], n_electrons=v[2],
                         E_xc=v[3], binary=binary, date=when, integrand='xc', variant=variant))
        done[k] = True
store.append(basis, rows)
print(len(rows), 'rows added to', store.path(basis))
