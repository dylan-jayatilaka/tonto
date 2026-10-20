#!/usr/bin/env python3
"""Write the small MTZ and PDB files of tests/short/mtz_and_pdb_read.

The MTZ files hold the same twelve reflections of a P 1 21 1 cell, with the
columns H K L FP SIGFP FC PHIC FOM, one of them with FP and SIGFP missing
(NaN); tiny_le.mtz is little-endian and tiny_be.mtz big-endian, so that the
reader's byte swapping is exercised whatever the machine. The PDB file holds
four atoms: a carbonyl C and O with ANISOU records, an N with an isotropic
B, and a water in alternate conformer A.

The format: a 4-byte tag "MTZ ", a 4-byte integer giving the word at which the
header starts, a 4-byte machine stamp, the data as 32-bit floats from word 21,
and then 80-character header lines ending with END.

Usage:  make_test_mtz.py <directory>
"""

import math
import struct
import sys

cell = (10.0, 12.0, 14.0, 90.0, 95.0, 90.0)
reflections = [
    # h  k  l    FP    SIGFP   FC    PHIC    FOM
    (1, 0, 0, 120.0, 2.0, 118.0, 0.0, 0.95),
    (0, 2, 0, 80.0, 1.5, 82.0, 180.0, 0.90),
    (0, 0, 1, 60.0, 1.2, 58.0, 45.0, 0.88),
    (1, 1, 0, 45.0, 1.0, 44.0, 90.0, 0.85),
    (1, 0, 1, 30.0, 0.9, 31.0, 135.0, 0.80),
    (0, 1, 1, 25.0, 0.8, 24.0, 270.0, 0.75),
    (1, 1, 1, 20.0, 0.8, 21.0, 30.0, 0.70),
    (2, 0, 0, 15.0, 0.7, 16.0, 0.0, 0.65),
    (2, 1, 0, 12.0, 0.7, 11.0, 200.0, 0.60),
    (1, 2, 1, 10.0, 0.6, 9.5, 315.0, 0.55),
    (2, 1, 1, float("nan"), float("nan"), 8.0, 60.0, 0.50),
    (0, 3, 1, 6.0, 0.5, 6.5, 120.0, 0.45),
]
columns = [("H", "H"), ("K", "H"), ("L", "H"), ("FP", "F"), ("SIGFP", "Q"),
           ("FC", "F"), ("PHIC", "P"), ("FOM", "W")]


def write_mtz(path, little):
    e = "<" if little else ">"
    n_col, n_refl = len(columns), len(reflections)
    data = b"".join(struct.pack(e + "f", float(v)) for r in reflections for v in r)
    header_word = 21 + n_col * n_refl
    lines = ["VERS MTZ:V1.1", "TITLE Tonto test file, made by scripts/make_test_mtz.py",
             "NCOL %8d %12d %8d" % (n_col, n_refl, 0),
             "CELL " + " ".join("%9.4f" % c for c in cell),
             "SORT    1   2   3   0   0",
             "SYMINF   2  2 P     4                  'P 1 21 1'   PG2",
             "SYMM X,  Y,  Z", "SYMM -X,  Y+1/2,  -Z",
             "RESO 0.0100 0.0500", "VALM NAN"]
    for i, (label, kind) in enumerate(columns):
        vals = [r[i] for r in reflections if not math.isnan(r[i])]
        lines.append("COLUMN %-30s %c %17.9g %17.9g %4d" % (label, kind, min(vals), max(vals), 1))
    lines += ["NDIF 1", "PROJECT 1 test", "CRYSTAL 1 test", "DATASET 1 test",
              "DCELL 1 " + " ".join("%9.4f" % c for c in cell), "DWAVEL 1 1.5418", "END"]
    header = b"".join(l.ljust(80).encode() for l in lines)
    stamp = bytes([0x44, 0x41, 0, 0]) if little else bytes([0x11, 0x11, 0, 0])
    with open(path, "wb") as f:
        f.write(b"MTZ " + struct.pack(e + "i", header_word) + stamp)
        f.write(b"\0" * (80 - 12))          # words 4 to 20 are unused
        f.write(data)
        f.write(header)


def write_pdb(path):
    lines = [
        "HEADER    Tonto test file, made by scripts/make_test_mtz.py",
        "CRYST1   10.000   12.000   14.000  90.00  95.00  90.00 P 1 21 1      2",
        "ATOM      1  C   GLY A   1       1.000   2.000   3.000  1.00 10.00           C  ",
        "ANISOU    1  C   GLY A   1     1200   1300   1400    100   -200     50       C  ",
        "ATOM      2  O   GLY A   1       1.800   2.900   3.000  1.00 12.00           O  ",
        "ANISOU    2  O   GLY A   1     1500   1600   1450    120   -250     60       O  ",
        "ATOM      3  N   GLY A   2       2.500   1.200   4.100  1.00 11.50           N  ",
        "HETATM    4  O  AHOH A 101       5.000   6.000   7.000  0.60 20.00           O  ",
        "END",
    ]
    with open(path, "w") as f:
        f.write("\n".join(lines) + "\n")


if __name__ == "__main__":
    d = sys.argv[1] if len(sys.argv) > 1 else "."
    write_mtz(d + "/tiny_le.mtz", True)
    write_mtz(d + "/tiny_be.mtz", False)
    write_pdb(d + "/tiny.pdb")
