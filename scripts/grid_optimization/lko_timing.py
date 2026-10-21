#!/usr/bin/env python3
"""Grid cost with and without the LKO cutoff: wall time and points of one
E_xc evaluation on a fixed density (neopentane, Cr(CO)6 from the def2-SVP
run folder given) and of a full SCF on karrikinolide (17 atoms, def2-SVP),
each on the lowered Becke `medium` grid, with Becke's partition, Becke's
with the cutoff at 5 bohr, and the mixed Salvador partition with the cutoff.
Jobs run one at a time, so the machine should be otherwise quiet.

Usage, from a run folder holding tonto.grid: lko_timing.py <svp run folder>
Writes lko_timing.md.
"""
import json, os, re, subprocess, sys, time
import store, descend, assemble

HERE = os.path.dirname(os.path.abspath(__file__))
T = os.path.dirname(os.path.dirname(HERE))
FCHK = os.path.join(T, 'tests', 'rgbi', 'karrikinolide_blyp_6-31G(d)_Roby_bond_index', 'karrikinolide.fchk')
BOHR = 0.529177210903

def karrikinolide():
    """(Z, x, y, z) in angstrom from the Gaussian checkpoint file."""
    text = open(FCHK).read()
    z = re.search(r'Atomic numbers\s+I\s+N=\s+(\d+)\n((?:.*\n)+?)(?=\w)', text)
    n = int(z.group(1)); zs = [int(v) for v in z.group(2).split()][:n]
    c = re.search(r'Current cartesian coordinates\s+R\s+N=\s+(\d+)\n((?:.*\n)+?)(?=\w)', text)
    xyz = [float(v) * BOHR for v in c.group(2).split()][:3 * n]
    return [(zs[i], xyz[3 * i], xyz[3 * i + 1], xyz[3 * i + 2]) for i in range(n)]

def with_cutoff(block, rc):
    return block if rc is None else block[:-1] + 'lko_cutoff= %g }' % rc

def run(d, stdin, tag):
    """Run stdin in folder d as job tag; return (wall seconds, points, E_xc)."""
    inp = os.path.join(d, 'lko_timing_%s.in' % tag)
    out = os.path.join(d, 'lko_timing_%s.out' % tag)
    log = os.path.join(d, 'lko_timing_%s.log' % tag)
    open(inp, 'w').write(stdin)
    env = dict(os.environ, OPENBLAS_NUM_THREADS='1', OMP_NUM_THREADS='1')
    tonto = os.path.abspath('tonto.grid')
    t0 = time.time()
    subprocess.run([tonto, '--input', os.path.basename(inp), '--output', os.path.basename(out),
                    '--basis-library', os.path.join(T, 'basis_sets')],
                   cwd=d, stdout=open(log, 'w'), stderr=subprocess.STDOUT, check=False, env=env)
    wall = time.time() - t0
    text = open(out).read() if os.path.exists(out) else ''
    pts = re.findall(r'No\. of grid points \.+ +(\d+)', text)
    exc = re.findall(r'XC energy \.+ +(\S+)', text)
    return wall, (int(pts[-1]) if pts else None), (exc[-1] if exc else None)

def main():
    svp = sys.argv[1]
    descend.set_basis('def2-SVP')
    state = json.load(open(os.path.join(store.root(), 'descent_def2-SVP.json')))
    variants = []
    for cand, rc, label in (('becke', None, 'Becke'), ('becke', 5.0, 'Becke, cutoff 5'),
                            ('mixed4', 5.0, 'mixed Salvador, cutoff 5')):
        base = descend.block(cand, state['%s:medium' % cand]['g'])
        variants.append((label, with_cutoff(base, rc)))

    rows = []
    # Fixed-density evaluations
    for system in ('neopentane', 'CrCO6'):
        d = os.path.join(svp, 'def2-SVP', system)
        head = open(os.path.join(d, 'header')).read()
        for label, block in variants:
            stdin = head + '   make_anos_and_interpolators\n   read_archive density_mx r\n   %s\n   put_xc_energy_on_grid\n}\n' % block
            wall, pts, exc = run(d, stdin, re.sub(r'\W+', '_', label))
            rows.append((system + ' E_xc', label, wall, pts, exc))
            print(rows[-1], flush=True)
    # Full SCF on karrikinolide
    d = 'karrikinolide'; os.makedirs(d, exist_ok=True)
    atoms = karrikinolide()
    for label, block in variants:
        stdin = '''{
   name= karrikinolide
   output_style_options= { real_precision= 10 }
   basis_name= def2-SVP
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
''' % ('\n'.join('      %d %.6f %.6f %.6f' % a for a in atoms), block)
        wall, pts, exc = run(d, stdin, re.sub(r'\W+', '_', label))
        rows.append(('karrikinolide SCF', label, wall, pts, exc))
        print(rows[-1], flush=True)

    out = ['# Grid cost with and without the LKO cutoff, lowered Becke `medium` grid, def2-SVP\n',
           'One job at a time. The fixed-density rows time one grid build and one E_xc integration; '
           'the karrikinolide rows time a whole SCF (17 atoms).\n',
           '| job | partition | wall / s | grid points | E_xc |', '|---|---|---|---|---|']
    for job, label, wall, pts, exc in rows:
        out.append('| %s | %s | %.1f | %s | %s |' % (job, label, wall, pts, exc))
    text = '\n'.join(out) + '\n'
    open('lko_timing.md', 'w').write(text)
    print(text)

if __name__ == '__main__':
    main()
