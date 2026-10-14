#!/usr/bin/env python3
"""The 22 test systems of docs/TASK_GRID_LEVELS.md, and two extras.

Each entry: name -> (charge, multiplicity, [(Z, x, y, z), ...]) in angstrom.
Bond lengths are experimental gas-phase values (Huber and Herzberg for the
diatomics, NIST CCCBDB for the rest), typed from memory and built in ideal
symmetry. A grid error does not depend on a bond length to 0.01 angstrom.
The water dimer is the S22 geometry (Jurecka et al., PCCP 8, 1985 (2006)).
"""
from math import sin, cos, sqrt, radians, pi

def diatomic(z1, z2, r):
    return [(z1, 0, 0, 0), (z2, 0, 0, r)]

TET = [(1, 1, 1), (1, -1, -1), (-1, 1, -1), (-1, -1, 1)]
OCT = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]

def unit(v):
    n = sqrt(sum(c * c for c in v))
    return tuple(c / n for c in v)

def star(zc, zl, r, dirs):
    """A centre and ligands at distance r along dirs."""
    return [(zc, 0, 0, 0)] + [(zl,) + tuple(r * c for c in unit(d)) for d in dirs]

def carbonyl(zm, r_mc, r_co, dirs):
    at = [(zm, 0, 0, 0)]
    for d in dirs:
        u = unit(d)
        at.append((6,) + tuple(r_mc * c for c in u))
        at.append((8,) + tuple((r_mc + r_co) * c for c in u))
    return at

def water():
    r, a = 0.9572, radians(104.52 / 2)
    return [(8, 0, 0, 0), (1, r * sin(a), 0, r * cos(a)), (1, -r * sin(a), 0, r * cos(a))]

def methyl():
    r = 1.079
    return [(6, 0, 0, 0)] + [(1, r * cos(2 * pi * k / 3), r * sin(2 * pi * k / 3), 0) for k in range(3)]

def benzene():
    rc, rh = 1.397, 1.397 + 1.084
    at = []
    for k in range(6):
        c, s = cos(pi * k / 3), sin(pi * k / 3)
        at += [(6, rc * c, rc * s, 0), (1, rh * c, rh * s, 0)]
    return at

def methyl_on(c_pos, axis, r_ch, hcx_deg, twist=0.0):
    """Three H on the carbon at c_pos; axis points from the carbon away from
    its neighbour X, and hcx_deg is the H-C-X angle."""
    ax = unit(axis)
    ref = (1, 0, 0) if abs(ax[0]) < 0.9 else (0, 1, 0)
    e1 = unit(tuple(ref[i] - sum(ref[j] * ax[j] for j in range(3)) * ax[i] for i in range(3)))
    e2 = (ax[1] * e1[2] - ax[2] * e1[1], ax[2] * e1[0] - ax[0] * e1[2], ax[0] * e1[1] - ax[1] * e1[0])
    t = radians(180 - hcx_deg)
    hs = []
    for k in range(3):
        ph = twist + 2 * pi * k / 3
        d = tuple(cos(t) * ax[i] + sin(t) * (cos(ph) * e1[i] + sin(ph) * e2[i]) for i in range(3))
        hs.append((1,) + tuple(c_pos[i] + r_ch * d[i] for i in range(3)))
    return hs

def neopentane():
    at = [(6, 0, 0, 0)]
    for d in TET:
        u = unit(d)
        c = tuple(1.537 * x for x in u)
        at.append((6,) + c)
        at += methyl_on(c, u, 1.10, 110.0)
    return at

def dimethylzinc():
    at = [(30, 0, 0, 0), (6, 0, 0, 1.930), (6, 0, 0, -1.930)]
    at += methyl_on((0, 0, 1.930), (0, 0, 1), 1.09, 110.0)
    at += methyl_on((0, 0, -1.930), (0, 0, -1), 1.09, 110.0, twist=pi / 3)
    return at

def p4():
    a = 2.21 / (2 * sqrt(2))      # P-P 2.21
    return [(15,) + tuple(a * c for c in d) for d in TET]

def water_dimer(shift=0.0):
    return [(8, -1.551007, -0.114520, 0.0), (1, -1.934259, 0.762503, 0.0),
            (1, -0.599677, 0.040712, 0.0), (8, 1.350625 + shift, 0.111469, 0.0),
            (1, 1.680398 + shift, -0.373741, -0.758561), (1, 1.680398 + shift, -0.373741, 0.758561)]

SYSTEMS = {
    # periods 1 and 2
    'H2O':        (0, 1, water()),
    'N2':         (0, 1, diatomic(7, 7, 1.0977)),
    'LiF':        (0, 1, diatomic(3, 9, 1.5639)),
    'F2':         (0, 1, diatomic(9, 9, 1.4119)),
    'CH3':        (0, 2, methyl()),
    'benzene':    (0, 1, benzene()),
    'neopentane': (0, 1, neopentane()),
    'H2O_dimer':  (0, 1, water_dimer()),
    # period 3
    'HCl':        (0, 1, diatomic(17, 1, 1.2746)),
    'NaCl':       (0, 1, diatomic(11, 17, 2.3609)),
    'SiH4':       (0, 1, star(14, 1, 1.4798, TET)),
    'P4':         (0, 1, p4()),
    'SF6':        (0, 1, star(16, 9, 1.561, OCT)),
    # period 4, s and p
    'KF':         (0, 1, diatomic(19, 9, 2.1716)),
    'HBr':        (0, 1, diatomic(35, 1, 1.4144)),
    # period 4, 3d
    'TiCl4':      (0, 1, star(22, 17, 2.170, TET)),
    'CrCO6':      (0, 1, carbonyl(24, 1.914, 1.140, OCT)),
    'NiCO4':      (0, 1, carbonyl(28, 1.838, 1.141, TET)),
    'ZnMe2':      (0, 1, dimethylzinc()),
    'CuCl':       (0, 1, diatomic(29, 17, 2.0512)),
    'ScO':        (0, 2, diatomic(21, 8, 1.6682)),
    'Mn':         (0, 6, [(25, 0, 0, 0)]),
}

# Charge and multiplicity of each kind of atom, in order of first appearance,
# for a promolecule of ions (scfdata initial_atomic_states=), for the grid
# partition. Only the clearly ionic systems have one.
IONIC = {
    'LiF':  [(1, 1), (-1, 1)],     # Li+, F-
    'NaCl': [(1, 1), (-1, 1)],     # Na+, Cl-
    'KF':   [(1, 1), (-1, 1)],     # K+, F-
    'CuCl': [(1, 1), (-1, 1)],     # Cu+ (d10), Cl-
    'ScO':  [(1, 3), (-1, 2)],     # Sc+ (3d 4s), O-
}

# Not calibration data: the stretched water dimer shows whether the Becke
# partition gives a ragged curve for a weakly bound pair.
EXTRAS = {
    'H2O_dimer_stretched': (0, 1, water_dimer(1.5)),
}

if __name__ == '__main__':
    for name, (q, m, at) in {**SYSTEMS, **EXTRAS}.items():
        dmin = min(sqrt(sum((a[i] - b[i]) ** 2 for i in (1, 2, 3)))
                   for k, a in enumerate(at) for b in at[k + 1:]) if len(at) > 1 else 0
        ne = sum(a[0] for a in at) - q
        print('%-20s atoms %2d  electrons %3d  mult %d  shortest distance %.3f' % (name, len(at), ne, m, dmin))
    print(len(SYSTEMS), 'systems')
