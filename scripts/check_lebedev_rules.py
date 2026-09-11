#!/usr/bin/env python3
"""Invariant test: every Lebedev grid in foofiles/lebedev.foo integrates exactly.

lebedev.foo stores each grid as octahedral-orbit generators (gen_Oh codes 1-6
with a, b, w), not as explicit points, so a typo in one literal or one orbit
would silently shift every DFT energy. This replays the generators in Python
and asserts, for each grid, that

  - the number of points generated equals the number the routine allocates,
  - the weights sum to 1 (Tonto's normalisation; becke_grid.foo supplies 4 pi),
  - every point lies on the unit sphere,
  - every monomial x^i y^j z^k with i+j+k <= L integrates to its closed-form
    value (1/4pi) int x^i y^j z^k dOmega = (i-1)!!(j-1)!!(k-1)!!/(i+j+k+1)!!,
    zero when any exponent is odd.

The last property is the definition of an L-th order rule, so this also fixes
the `.l =` value each routine claims. It found nothing wrong on 2026-09-10:
every grid up to 5810 points is exact to ~1e-16 (ta0434 to 7e-15).

  usage:  python3 check_lebedev_rules.py <lebedev.foo> [--all]

Without --all only the grids reachable from `accuracy=` (up to 1730 points,
L=71) are checked, in a few seconds; --all takes minutes for the L=131 grids.
Exits 0 if every grid passes, 1 otherwise.
"""

import itertools
import math
import re
import sys

import numpy as np

TOL_SUM = 1e-13
TOL_SPHERE = 1e-14
TOL_MONOMIAL = 1e-12
DEFAULT_MAX_PTS = 1730


def orbit(code, a, b, w):
    """The points of one gen_Oh orbit, transcribed from lebedev.foo."""
    pts = []
    if code == 1:
        pts = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]
    elif code == 2:
        a = math.sqrt(0.5)
        for s1, s2 in itertools.product((1, -1), repeat=2):
            pts += [(0, s1 * a, s2 * a), (s1 * a, 0, s2 * a), (s1 * a, s2 * a, 0)]
    elif code == 3:
        a = math.sqrt(1 / 3)
        pts = [(s1 * a, s2 * a, s3 * a)
               for s1, s2, s3 in itertools.product((1, -1), repeat=3)]
    elif code == 4:
        b = math.sqrt(1 - 2 * a * a)
        for s1, s2, s3 in itertools.product((1, -1), repeat=3):
            pts += [(s1 * a, s2 * a, s3 * b), (s1 * a, s2 * b, s3 * a),
                    (s1 * b, s2 * a, s3 * a)]
    elif code == 5:
        b = math.sqrt(1 - a * a)
        for s1, s2 in itertools.product((1, -1), repeat=2):
            pts += [(s1 * a, s2 * b, 0), (s1 * b, s2 * a, 0), (s1 * a, 0, s2 * b),
                    (s1 * b, 0, s2 * a), (0, s1 * a, s2 * b), (0, s1 * b, s2 * a)]
    elif code == 6:
        c = math.sqrt(1 - a * a - b * b)
        for perm in itertools.permutations((a, b, c)):
            for s in itertools.product((1, -1), repeat=3):
                pts.append(tuple(si * pi for si, pi in zip(s, perm)))
    else:
        raise ValueError(f"bad gen_Oh code {code}")
    return pts, [w] * len(pts)


def parse(path):
    """Yield (name, n_pts_claimed, L, points, weights) for each ldNNNN/taNNNN."""
    src = open(path).read()
    for name, body in re.findall(r'\n   ((?:ld|ta)\d{4}) ::.*?\n(.*?)\n   end\n',
                                 src, re.S):
        n_claimed = int(re.search(r'\.set\((\d+),(\d+)\)', body).group(1))
        L = int(re.search(r'\.l = (\d+)', body).group(1))
        a = b = w = None
        pts, wts = [], []
        for line in body.splitlines():
            line = line.strip()
            m = re.match(r'(a|b|w) = (\S+)', line)
            if m:
                val = float(m.group(2).replace('d', 'e'))
                if m.group(1) == 'a':
                    a = val
                elif m.group(1) == 'b':
                    b = val
                else:
                    w = val
                continue
            m = re.match(r'\.gen_Oh\((\d),a,b,w\)', line)
            if m:
                p, q = orbit(int(m.group(1)), a, b, w)
                pts += p
                wts += q
        yield name, n_claimed, L, np.array(pts), np.array(wts)


def dfact(n):
    r = 1
    while n > 1:
        r *= n
        n -= 2
    return r


def exact(i, j, k):
    if i % 2 or j % 2 or k % 2:
        return 0.0
    return dfact(i - 1) * dfact(j - 1) * dfact(k - 1) / dfact(i + j + k + 1)


def max_monomial_error(pts, wts, L):
    x, y, z = pts.T
    xp, yp, zp = [np.ones_like(x)], [np.ones_like(y)], [np.ones_like(z)]
    for _ in range(L):
        xp.append(xp[-1] * x)
        yp.append(yp[-1] * y)
        zp.append(zp[-1] * z)
    worst = 0.0
    for d in range(L + 1):
        for i in range(d + 1):
            for j in range(d + 1 - i):
                k = d - i - j
                v = (wts * xp[i] * yp[j] * zp[k]).sum()
                worst = max(worst, abs(v - exact(i, j, k)))
    return worst


def main():
    args = [a for a in sys.argv[1:] if not a.startswith('--')]
    if len(args) != 1:
        print(__doc__)
        return 2
    check_all = '--all' in sys.argv
    failures = 0
    for name, n_claimed, L, pts, wts in parse(args[0]):
        if not check_all and n_claimed > DEFAULT_MAX_PTS:
            continue
        problems = []
        if len(pts) != n_claimed:
            problems.append(f"generated {len(pts)} points, allocated {n_claimed}")
        if abs(wts.sum() - 1) > TOL_SUM:
            problems.append(f"weights sum to {wts.sum():.16f}")
        sph = np.abs((pts ** 2).sum(1) - 1).max()
        if sph > TOL_SPHERE:
            problems.append(f"point off the unit sphere by {sph:.1e}")
        err = max_monomial_error(pts, wts, L)
        if err > TOL_MONOMIAL:
            problems.append(f"monomial error {err:.1e} for degree <= {L}")
        status = "ok" if not problems else "FAIL: " + "; ".join(problems)
        print(f"  {name}  n={len(pts):5d}  L={L:3d}  max monomial error {err:.1e}  {status}")
        failures += bool(problems)
    if failures:
        print(f"\nFAIL -- {failures} Lebedev grid(s) are not exact to their claimed order")
        return 1
    print("\nOK -- every Lebedev grid integrates exactly to its claimed order")
    return 0


if __name__ == "__main__":
    sys.exit(main())
