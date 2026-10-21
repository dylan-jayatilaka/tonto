#!/usr/bin/env python3
"""The LKO cutoff on a fixed density. For the lowered grid of each candidate
and level (descent_<state>.json in the store), evaluate E_xc with the cutoff
at each value given, on every system, and report the error against grid A
and the points per atom beside the same grid without the keyword.

A cutoff of 0 writes lko_cutoff= 0, the new code's off path: its E_xc must
equal the stored row of the same grid without the keyword to every printed
digit, which checks the change left the default untouched.

Usage, from a run folder:
  lko_check.py <basis> [cutoffs=0,4,5,6] [cands=becke,mixed4]
               [levels=very_low,low,medium,high] [state=def2-SVP]
Writes lko_check_<basis>.md in the run folder.
"""
import json, os, sys
from systems import SYSTEMS
import store, assemble, descend
import tables_lib as T

OUTLIERS = assemble.OUTLIERS

def with_cutoff(block, rc):
    """The block with lko_cutoff= rc added before the closing brace."""
    assert block.endswith('}')
    return block[:-1] + 'lko_cutoff= %g }' % rc

def main():
    basis = sys.argv[1]
    opts = dict(a.split('=', 1) for a in sys.argv[2:])
    cutoffs = [float(x) for x in opts.get('cutoffs', '0,4,5,6').split(',')]
    cands = opts.get('cands', 'becke,mixed4').split(',')
    levels = opts.get('levels', 'very_low,low,medium,high').split(',')
    state_basis = opts.get('state', 'def2-SVP')
    descend.set_basis(basis)
    state = json.load(open(os.path.join(store.root(), 'descent_%s.json' % state_basis)))

    # The blocks: the lowered grid as stored, and with each cutoff
    plan = []
    for cand in cands:
        for level in levels:
            key = '%s:%s' % (cand, level)
            if key not in state or not state[key].get('g'):
                print('no lowered grid for', key); continue
            base = descend.block(cand, state[key]['g'])
            plan.append((cand, level, base, [(rc, with_cutoff(base, rc)) for rc in cutoffs]))

    # Run what is missing
    for cand, level, base, variants in plan:
        descend.evaluate(cand, level, [base] + [b for _, b in variants], tag='lko_%s_%s' % (cand, level))

    # Report
    out = ['# LKO cutoff on a fixed density, %s\n' % basis,
           'Error: E_xc against grid A, hartree. Points: per atom. "same" means the cutoff-0 '
           'energy equals the stored one to every printed digit. Worst errors leave out %s.\n'
           % ', '.join(OUTLIERS)]
    for cand, level, base, variants in plan:
        T.load(basis, **assemble.CANDIDATES[cand])
        head = '| system | stored error | stored points |'
        for rc, _ in variants:
            head += ' cutoff %g error | points | ratio |' % rc
        out += ['\n## %s %s\n' % (cand, level), head, '|---' * (3 + 3 * len(variants)) + '|']
        worst = {None: 0.0}; worst.update({rc: 0.0 for rc, _ in variants})
        ratio_sum = {rc: 0.0 for rc, _ in variants}; n_ratio = 0; same = True
        for s in SYSTEMS:
            a = T.pick(s, 'ref', grid='A')
            r0 = T.by[s].get(base)
            if not (a and r0):
                out.append('| %s | missing | | ' % s + ' | | |' * len(variants)); continue
            n_at = len(SYSTEMS[s][2])
            e0 = T.E(r0) - T.E(a); p0 = int(r0['n_points']) / n_at
            line = '| %s | %.1e | %.0f |' % (s, e0, p0)
            if s not in OUTLIERS: worst[None] = max(worst[None], abs(e0))
            n_ratio += 1
            for rc, b in variants:
                r = T.by[s].get(b)
                if not r:
                    line += ' missing | | |'; continue
                e = T.E(r) - T.E(a); p = int(r['n_points']) / n_at
                tag = ''
                if rc == 0:
                    if r['E_xc'] == r0['E_xc'] and r['n_points'] == r0['n_points']: tag = ' same'
                    else: same = False; tag = ' DIFFERS'
                line += ' %.1e%s | %.0f | %.3f |' % (e, tag, p, p / p0)
                if s not in OUTLIERS: worst[rc] = max(worst[rc], abs(e))
                ratio_sum[rc] += p / p0
            out.append(line)
        summary = '| worst | %.1e | |' % worst[None]
        for rc, _ in variants:
            summary += ' %.1e | | %.3f |' % (worst[rc], ratio_sum[rc] / max(n_ratio, 1))
        out.append(summary)
        if 0.0 in [rc for rc, _ in variants]:
            out.append('\nCutoff 0 %s the stored energies and points on every system.'
                       % ('reproduces' if same else 'DOES NOT reproduce'))
    text = '\n'.join(out) + '\n'
    open('lko_check_%s.md' % basis, 'w').write(text)
    print(text)

if __name__ == '__main__':
    main()
