#!/usr/bin/env python3
"""Make the density matrix of a Tonto job with Psi4.

Tonto runs this script when scfdata= { external_density_command= "..." } is set,
with the job name as the last argument. It reads <name>.external_input, written
by Tonto (the basis set name, charge, multiplicity, the atoms in bohr and any
cluster charges in bohr), runs Psi4 with the same basis set and Cartesian d and
f functions (Tonto's default), and writes the restricted density matrix, in
Tonto's basis-function order, as the Tonto archive <name>.density_mx,r.

Methods: any Psi4 SCF method (hf, b3lyp, ...), and mp2 and ccsd, whose relaxed
densities come from Psi4's analytic gradient.

Example, in a Tonto job:
   scfdata= {
      external_density_command= "micromamba run -n psi4 python tonto_external_density.py --method hf"
      ...
   }
"""
import argparse
import struct
import sys

import numpy as np

# Psi4 -> Tonto order of the components of a Cartesian shell.
# Tonto: d xx yy zz xy xz yz;  f xxx yyy zzz xxy xxz xyy yyz xzz yzz xyz.
# Psi4:  d xx xy xz yy yz zz;  f xxx xxy xxz xyy xyz xzz yyy yyz yzz zzz.
PSI4_TO_TONTO = {0: [0], 1: [0, 1, 2], 2: [0, 3, 5, 1, 2, 4],
                 3: [0, 6, 9, 1, 2, 3, 7, 5, 8, 4]}


def read_input(name):
    """Return the contents of <name>.external_input as a dict."""
    lines = [l.split() for l in open(name + ".external_input") if l.strip()]
    d = {"charges": []}
    i = 0
    while i < len(lines):
        key = lines[i][0]
        if key == "atoms":
            n = int(lines[i][1])
            d["atoms"] = [(a[0], int(a[1]), float(a[2]), float(a[3]), float(a[4]))
                          for a in lines[i + 1:i + 1 + n]]
            i += n + 1
        elif key == "charges":
            n = int(lines[i][1])
            d["charges"] = [[float(a[0]), [float(a[1]), float(a[2]), float(a[3])]]
                            for a in lines[i + 1:i + 1 + n]]
            i += n + 1
        else:
            d[key] = " ".join(lines[i][1:])
            i += 1
    return d


def tonto_order(basisset):
    """Index list taking Psi4's basis functions into Tonto's order."""
    idx = []
    for s in range(basisset.nshell()):
        shell = basisset.shell(s)
        if shell.am not in PSI4_TO_TONTO:
            sys.exit(f"angular momentum {shell.am} has no component order defined")
        first = basisset.shell_to_basis_function(s)
        idx += [first + p for p in PSI4_TO_TONTO[shell.am]]
    return np.array(idx)


def write_archive(name, P):
    """Write P as Tonto's binary archive: one Fortran sequential unformatted
    record holding the matrix by columns."""
    raw = np.asfortranarray(P, dtype="<f8").tobytes(order="F")
    n = struct.pack("<i", len(raw))
    with open(name + ".density_mx,r", "wb") as f:
        f.write(n + raw + n)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--method", default="hf", help="hf, b3lyp, ..., mp2 or ccsd")
    ap.add_argument("--scf-type", default="pk", help="Psi4 scf_type: pk (exact) or df")
    ap.add_argument("--threads", type=int, default=1)
    ap.add_argument("name", help="the Tonto job name")
    a = ap.parse_args()

    import psi4
    from qcelemental import periodictable

    d = read_input(a.name)
    psi4.set_output_file(a.name + ".psi4.out", False)
    psi4.set_num_threads(a.threads)
    psi4.set_memory("2 GB")

    # The molecule exactly as Tonto has it
    geom = [f"{d['charge']} {d['multiplicity']}"]
    geom += [f"{periodictable.to_E(Z)} {x:.16f} {y:.16f} {z:.16f}"
             for _, Z, x, y, z in d["atoms"]]
    geom += ["units bohr", "no_com", "no_reorient", "symmetry c1"]
    mol = psi4.geometry("\n".join(geom))

    psi4.set_options({"basis": d["basis"].lower(), "puream": False,
                      "scf_type": a.scf_type, "reference": "rhf",
                      "e_convergence": 1e-10, "d_convergence": 1e-9})
    ext = d["charges"] if d["charges"] else None

    m = a.method.lower()
    if m in ("mp2", "ccsd"):
        if m == "ccsd":
            psi4.set_options({"cc_type": "conv"})
        _, wfn = psi4.gradient(m, molecule=mol, return_wfn=True, external_potentials=ext)
    else:
        _, wfn = psi4.energy(m, molecule=mol, return_wfn=True, external_potentials=ext)

    P = np.asarray(wfn.Da()) + np.asarray(wfn.Db())
    idx = tonto_order(wfn.basisset())
    write_archive(a.name, P[np.ix_(idx, idx)])
    print(f"tonto_external_density: {m}, {len(d['atoms'])} atoms, "
          f"{len(d['charges'])} charges, {P.shape[0]} basis functions")


if __name__ == "__main__":
    main()
