#!/usr/bin/env python3
"""Turn an ORCA Hessian file into Tonto's force_constants= input.

ORCA writes the Cartesian second derivatives of the energy to <job>.hess after
a frequency calculation (`! Freq`). They are in hartree/bohr^2, in the order
and the orientation of the atoms in the ORCA input; ORCA moves the origin to
the centre of mass, which does not change them. Tonto reads the same matrix
as a flat list with `force_constants= { ... }`, in the order of its own atoms.

So the ORCA job must list the atoms in Tonto's order and in Tonto's Cartesian
frame: write them from Tonto with `write_xyz_file`. Given that file with
--xyz, the script checks that the two geometries agree, apart from a shift
of the origin, and stops if they do not.

Usage: orca_hessian_to_tonto.py <job>.hess [--xyz <tonto>.xyz]
Prints the `force_constants= { ... }` block; put it in the Tonto input.
"""
import sys

BOHR_PER_ANGSTROM = 1.0/0.529177210903


def block(lines, name):
    """The lines of block $name, up to the next blank line or $ line."""
    start = lines.index("$" + name) + 1
    out = []
    for line in lines[start:]:
        if line.startswith("$") or (not line.strip() and out):
            break
        if line.strip():
            out.append(line)
    return out


def read_hessian(lines):
    """The square matrix of $hessian, which ORCA writes five columns at a time."""
    rows = block(lines, "hessian")
    n = int(rows[0])
    hess = [[None]*n for _ in range(n)]
    cols = []
    for line in rows[1:]:
        f = line.split()
        if all("." not in x for x in f):      # a line of column numbers
            cols = [int(x) for x in f]
        else:
            i = int(f[0])
            for c, x in zip(cols, f[1:]):
                hess[i][c] = float(x)
    if any(x is None for row in hess for x in row):
        sys.exit("the $hessian block is incomplete")
    return hess


def read_atoms(lines):
    """Element and position (bohr) of each atom in $atoms."""
    rows = block(lines, "atoms")
    return [(f[0], [float(x) for x in f[2:5]]) for f in (r.split() for r in rows[1:])]


def check_against_xyz(atoms, xyz_file):
    """Stop unless the ORCA atoms are the xyz file's atoms shifted by one vector."""
    rows = open(xyz_file).read().split("\n")[2:]
    xyz = [r.split() for r in rows if r.strip()]
    if len(xyz) != len(atoms):
        sys.exit("the xyz file and the Hessian file differ in number of atoms")
    shift = None
    for (el, pos), f in zip(atoms, xyz):
        if not f[0].upper().startswith(el.upper()):
            sys.exit(f"atom order differs: {el} in the Hessian file, {f[0]} in the xyz file")
        d = [p - float(x)*BOHR_PER_ANGSTROM for p, x in zip(pos, f[1:4])]
        if shift is None:
            shift = d
        if max(abs(a - b) for a, b in zip(d, shift)) > 1e-4:
            sys.exit("the two geometries differ by more than a shift of origin")


def main():
    args = sys.argv[1:]
    if not args or args[0] in ("-h", "--help"):
        sys.exit(__doc__)
    lines = [line.rstrip("\n") for line in open(args[0])]
    hess = read_hessian(lines)
    if "--xyz" in args:
        check_against_xyz(read_atoms(lines), args[args.index("--xyz") + 1])
    flat = [x for row in hess for x in row]
    print("   force_constants= {")
    for i in range(0, len(flat), 4):
        print("      " + " ".join(f"{x:17.10e}" for x in flat[i:i+4]))
    print("   }")


if __name__ == "__main__":
    main()
