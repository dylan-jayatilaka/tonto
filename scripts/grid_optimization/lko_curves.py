#!/usr/bin/env python3
"""Energy curves with and without the LKO cutoff, PBE/def2-TZVP, full SCF on
the lowered Becke grid of one level (descent_def2-SVP.json in the store):

  ar2    the argon dimer from 5 to 10 A, cutoff off and 5 bohr
         (figure 1 of Laqua, Kussmann and Ochsenfeld 2018)
  stretch the water dimer pulled apart by 0 to 3 A, off and 5 bohr
  kink   the water dimer moved by -0.5 to 0.5 A in 0.05 A steps, cutoff off,
         4, 5 and 6 bohr: the O-O distance crosses 5 bohr at -0.26 A

Usage, from a run folder holding tonto.grid: lko_curves.py [level=medium] [NPAR in the environment]
Writes lko_curves.md.
"""
import json, os, subprocess, sys
from systems import water_dimer
import store, descend

HERE = os.path.dirname(os.path.abspath(__file__))
T = os.path.dirname(os.path.dirname(HERE))
BASIS = 'def2-TZVP'

def header(name, atoms, grid):
    rows = '\n'.join('      %d %.6f %.6f %.6f' % a for a in atoms)
    return '''{
   name= %s
   output_style_options= { real_precision= 10 }
   basis_name= %s
   use_spherical_basis= TRUE
   charge= 0
   multiplicity= 1
   atoms= {
   keys= { label= { units= angstrom } pos= }
   data= {
%s
   }
   }
   %s
   scfdata= {
      initial_density= promolecule
      kind= rks
      dft_functional= pbe
      direct= on
      output= TRUE
      convergence= 1e-9
      diis= { convergence_tolerance= 1e-8 }
      max_iterations= 200
   }
   scf
}
''' % (name, BASIS, rows, grid)

def with_cutoff(block, rc):
    return block if rc is None else block[:-1] + 'lko_cutoff= %g }' % rc

def energy(d):
    try:
        for line in open(os.path.join(d, 'stdout')):
            if line.startswith('Total energy (E_e+V_NN)'):
                return float(line.split()[-1])
    except FileNotFoundError:
        pass
    return None

def main():
    opts = dict(a.split('=', 1) for a in sys.argv[1:])
    level = opts.get('level', 'medium')
    descend.set_basis('def2-SVP')
    state = json.load(open(os.path.join(store.root(), 'descent_def2-SVP.json')))
    base = descend.block('becke', state['becke:%s' % level]['g'])

    curves = {
        'ar2':     ([5.0 + 0.25 * i for i in range(21)], [None, 5.0],
                    lambda x: [(18, 0.0, 0.0, 0.0), (18, x, 0.0, 0.0)], 'Ar-Ar / A'),
        'stretch': ([0.25 * i for i in range(13)], [None, 5.0], water_dimer, 'shift / A'),
        'kink':    ([-0.5 + 0.05 * i for i in range(21)], [None, 4.0, 5.0, 6.0], water_dimer, 'shift / A'),
    }
    jobs = []
    for cname, (xs, cutoffs, geom, _) in curves.items():
        for x in xs:
            for rc in cutoffs:
                d = os.path.join('curves', cname, '%s_%.2f_%s' % (cname, x, 'off' if rc is None else '%g' % rc))
                os.makedirs(d, exist_ok=True)
                open(os.path.join(d, 'stdin'), 'w').write(header(cname, geom(x), with_cutoff(base, rc)))
                if energy(d) is None:
                    jobs.append(d)
    open('curve_jobs.txt', 'w').write('\n'.join(jobs) + '\n')
    npar = os.environ.get('NPAR', '8')
    open('curve_run.sh', 'w').write('#!/bin/bash\ncd "$1" || exit 1\n'
                                    '/usr/bin/time -p timeout 3600 ../../../tonto.grid --basis-library "%s/basis_sets" '
                                    '> run.log 2>&1\necho "$1 rc=$? $(grep ^real run.log) $(date +%%H:%%M:%%S)"\n' % T)
    if jobs:
        subprocess.run('xargs -P %s -I{} bash curve_run.sh {} < curve_jobs.txt' % npar, shell=True, check=False)

    # Report: energies relative to the last point of the curve, in microhartree,
    # and the change from the previous point (the slope's sign and size)
    out = ['# Energy curves with and without the LKO cutoff, PBE/%s, lowered Becke `%s` grid\n' % (BASIS, level),
           'E is relative to the curve\'s last point, in microhartree; dE is the change from the previous point. '
           'A smooth curve changes sign in dE at most once.\n']
    for cname, (xs, cutoffs, geom, xlabel) in curves.items():
        labels = ['off' if rc is None else '%g bohr' % rc for rc in cutoffs]
        out += ['\n## %s\n' % cname,
                '| %s |' % xlabel + ''.join(' E %s | dE |' % l for l in labels) + ' E(5) - E(off) |',
                '|---' * (2 + 2 * len(cutoffs)) + '|']
        E = {}
        for rc in cutoffs:
            E[rc] = [energy(os.path.join('curves', cname, '%s_%.2f_%s' % (cname, x, 'off' if rc is None else '%g' % rc)))
                     for x in xs]
        for i, x in enumerate(xs):
            line = '| %.2f |' % x
            for rc in cutoffs:
                e, last, prev = E[rc][i], E[rc][-1], (E[rc][i - 1] if i else None)
                if e is None or last is None:
                    line += ' - | - |'; continue
                de = ('%.2f' % (1e6 * (e - prev))) if prev is not None else '-'
                line += ' %.2f | %s |' % (1e6 * (e - last), de)
            e_off, e_5 = E[None][i], E.get(5.0, [None] * len(xs))[i]
            line += ' %s |' % (('%.2f' % (1e6 * (e_5 - e_off))) if (e_off is not None and e_5 is not None) else '-')
            out.append(line)
    text = '\n'.join(out) + '\n'
    open('lko_curves.md', 'w').write(text)
    print(text)

if __name__ == '__main__':
    main()
