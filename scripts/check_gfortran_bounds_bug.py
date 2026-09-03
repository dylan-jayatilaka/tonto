#!/usr/bin/env python3
"""Test a Fortran compiler for the gfortran 16 -fcheck=bounds code-generation bug.

The bug: for a bounds-checked subscript reached through an allocatable component
chain, gfortran 16 writes the array descriptor to one stack temporary and emits
the check reading another, which is only initialised later in the same
statement. The check therefore consults uninitialised stack memory.

scripts/gfortran_bounds_bug.f90 is compiled at -O0 with and without
-fcheck=bounds and both are run. Correct without the flag and failing with it is
the bug: the flag is meant to add a check, never to change the answer.

A clean result is evidence, not proof. What the bad code does depends on what
happens to be on the stack, and the reduced case only provokes the faulty
temporary on x86_64 -- gfortran 16 on arm64 compiles it correctly while still
crashing Tonto elsewhere. Trust a failure here; do not trust a pass to clear a
compiler that is failing debug builds. docs/GFORTRAN16_DEBUG_CRASH.md has the
machine-level signature to look for by hand.

Exit status: 0 if the compiler passed, 1 if the bug is present, 2 if the check
could not be carried out.

Usage:  scripts/check_gfortran_bounds_bug.py [compiler]      (default: $FC, else gfortran)
"""

import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
SRC = os.path.join(HERE, "gfortran_bounds_bug.f90")


def compile_and_run(fc, flags, workdir):
    """Return (exit status, first line of output), or (None, error) if it will not build."""
    exe = os.path.join(workdir, "probe")
    # Compile in workdir: gfortran writes .mod files to the current directory
    # whatever -o says, and they must not land in the caller's tree.
    cp = subprocess.run([fc, "-O0", "-g"] + flags + ["-o", exe, SRC],
                        capture_output=True, text=True, cwd=workdir)
    if cp.returncode != 0:
        return None, cp.stderr.strip()
    cp = subprocess.run([exe], capture_output=True, text=True, cwd=workdir)
    out = (cp.stdout + cp.stderr).strip().splitlines()
    return cp.returncode, out[0] if out else ""


def main():
    fc = sys.argv[1] if len(sys.argv) > 1 else os.environ.get("FC") or "gfortran"

    if not os.path.exists(SRC):
        print(f"cannot find {SRC}", file=sys.stderr)
        return 2

    ver = subprocess.run([fc, "--version"], capture_output=True, text=True)
    if ver.returncode != 0:
        print(f"cannot run compiler {fc!r}", file=sys.stderr)
        return 2
    print(f"compiler: {ver.stdout.splitlines()[0]}")

    with tempfile.TemporaryDirectory() as workdir:
        results = [(name, compile_and_run(fc, flags, workdir))
                   for name, flags in (("-O0", []),
                                       ("-O0 -fcheck=bounds", ["-fcheck=bounds"]))]

    for name, (rc, out) in results:
        if rc is None:
            print(f"  {name:22s} DID NOT COMPILE\n{out}", file=sys.stderr)
            return 2
        print(f"  {name:22s} exit={rc}  {out}")

    plain, checked = results[0][1][0], results[1][1][0]

    if plain != 0:
        print("\nThe program fails even without -fcheck=bounds, so this check "
              "cannot say anything about the flag.", file=sys.stderr)
        return 2

    if checked != 0:
        print("\n=> AFFECTED: correct without -fcheck=bounds, fails with it.")
        print("   Do not use -fcheck=bounds with this compiler; see "
              "docs/GFORTRAN16_DEBUG_CRASH.md.")
        return 1

    print("\n=> passed (see the note in this script: a pass is evidence, not proof).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
