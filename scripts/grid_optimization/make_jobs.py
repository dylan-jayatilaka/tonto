#!/usr/bin/env python3
"""Make the SCF jobs: one folder per system, PBE, spherical functions, on the
reference grid setting given. Usage: make_jobs.py [basis] [accuracy]"""
import os, sys
from systems import SYSTEMS, EXTRAS

basis = sys.argv[1] if len(sys.argv) > 1 else 'def2-SVP'
acc = sys.argv[2] if len(sys.argv) > 2 else 'best'

def header(name, q, m, atoms):
    rows = '\n'.join('      %d %.6f %.6f %.6f' % a for a in atoms)
    kind = 'rks' if m == 1 else 'uks'
    # The atomic SCF behind the starting density of Ti, Cr and Ni needs
    # damping and level shifting kept on well past the DIIS handover
    guess = ''
    if any(a[0] in (22, 24, 28) for a in atoms):
        guess = '\n      damp_factor= 0.5 damp_finish= 60 level_shift= 0.5 level_shift_finish= 60'
    return '''{
   name= %s
   output_style_options= { real_precision= 10 }
   basis_name= %s
   use_spherical_basis= TRUE
   charge= %d
   multiplicity= %d
   atoms= {
   keys= { label= { units= angstrom } pos= }
   data= {
%s
   }
   }
   becke_grid= { set_defaults accuracy= %s }
   scfdata= {
      initial_density= promolecule
      kind= %s
      dft_functional= pbe
      direct= on
      output= TRUE
      convergence= 1e-9
      diis= { convergence_tolerance= 1e-8 }
      max_iterations= 200%s
   }
''' % (name, basis, q, m, rows, acc, kind, guess)

jobs = []
for name, (q, m, atoms) in {**SYSTEMS, **EXTRAS}.items():
    d = os.path.join(basis, name)
    os.makedirs(d, exist_ok=True)
    open(os.path.join(d, 'header'), 'w').write(header(name, q, m, atoms))
    open(os.path.join(d, 'stdin'), 'w').write(header(name, q, m, atoms) + '   scf\n   put_xc_energy_on_grid\n}\n')
    jobs.append(d)
open('joblist_%s.txt' % basis, 'w').write('\n'.join(jobs) + '\n')
print(len(jobs), 'jobs in', basis)
