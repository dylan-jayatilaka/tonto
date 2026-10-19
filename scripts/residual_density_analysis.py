#!/usr/bin/env python3
"""Residual-density analysis of Meindl & Henn (2008), Acta Cryst. A64, 404-418.

Reads one or more Gaussian cube files of a residual density over the whole
unit cell, in e/A^3, as Tonto writes them with plot_format= cell.cube (the
repeated end plane of each axis left out). For each it computes

  d^f(rho0)  the fractal dimension of the iso-surface rho0, by their
             line-counting algorithm: N(rho0) is the number of pairs of
             neighbouring grid points along x, y and z whose values span
             rho0, and d^f = log N / log(1/eps) with eps the cube root of
             the reciprocal of the number of pairs examined; rho0 runs from
             the minimum to the maximum in steps of 0.01 e/A^3
  d^f(0)     its value at rho0 = 0
  rho0,min, rho0,max, and the flatness  Delta rho0 = rho0,max - rho0,min
  e_gross    (V/2N) sum |rho0|, in electrons
  e_net      (V/N) sum rho0, in electrons
  the negative and positive rho0 at which d^f = 2, their measure of noise

and plots d^f against rho0 with gnuplot, labelled as their Fig. 3, with the
descriptors written on the plot.

Usage:
  residual_density_analysis.py [--title T] [--out plot.png] [--range R]
                               cube[:label] [cube[:label] ...]
"""

import argparse
import math
import subprocess
import sys

import numpy as np

BOHR = 0.529177210903   # Angstrom per bohr


def read_cube(path):
    """Return the values on the grid (nx,ny,nz) and the cell volume in A^3."""
    with open(path) as f:
        lines = f.read().split("\n")
    n_atom = abs(int(lines[2].split()[0]))
    n, axes = [], []
    for i in range(3):
        w = lines[3 + i].split()
        n.append(int(w[0]))
        axes.append([float(x) for x in w[1:4]])
    # A positive count means the axis vectors are in bohr
    scale = BOHR if n[0] > 0 else 1.0
    n = [abs(k) for k in n]
    cell = np.array(axes) * np.array(n)[:, None] * scale
    volume = abs(np.linalg.det(cell))
    values = np.array(" ".join(lines[6 + n_atom:]).split(), float)
    if values.size != n[0] * n[1] * n[2]:
        sys.exit(f"{path}: expected {n[0]*n[1]*n[2]} values, found {values.size}")
    return values.reshape(n), volume


def analyse(rho, volume, step=0.01):
    """The descriptors and the fractal dimension distribution of "rho"."""
    n_pts = rho.size
    res = {
        "min": rho.min(),
        "max": rho.max(),
        "e_gross": volume / (2 * n_pts) * np.abs(rho).sum(),
        "e_net": volume / n_pts * rho.sum(),
    }
    res["flat"] = res["max"] - res["min"]

    # The low and high value of every pair of neighbours along each axis
    lo, hi = [], []
    for ax in range(3):
        a = np.take(rho, range(rho.shape[ax] - 1), axis=ax)
        b = np.take(rho, range(1, rho.shape[ax]), axis=ax)
        lo.append(np.minimum(a, b).ravel())
        hi.append(np.maximum(a, b).ravel())
    lo = np.sort(np.concatenate(lo))
    hi = np.sort(np.concatenate(hi))
    n_pairs = lo.size
    log_inv_eps = math.log(n_pairs) / 3

    # Levels on the 0.01 grid inside the range, and how many pairs span each
    k0 = math.ceil(res["min"] / step)
    k1 = math.floor(res["max"] / step)
    levels = np.arange(k0, k1 + 1) * step
    count = np.searchsorted(lo, levels, side="right") - np.searchsorted(hi, levels, side="left")
    keep = count > 0
    levels, df = levels[keep], np.log(count[keep]) / log_inv_eps
    res["levels"], res["df"] = levels, df

    # d^f at zero, and where it crosses 2 on each side
    res["df0"] = np.interp(0.0, levels, df) if levels[0] <= 0 <= levels[-1] else float("nan")
    res["neg2"] = crossing(levels[levels <= 0][::-1], df[levels <= 0][::-1])
    res["pos2"] = crossing(levels[levels >= 0], df[levels >= 0])
    return res


def crossing(x, y, level=2.0):
    """The first x, going outward from zero, at which y falls below "level"."""
    for i in range(1, len(x)):
        if y[i - 1] >= level > y[i]:
            return x[i - 1] + (x[i] - x[i - 1]) * (y[i - 1] - level) / (y[i - 1] - y[i])
    return float("nan")


def label_text(r):
    """The descriptors, one per line, for the plot."""
    return "\\n".join([
        f"d^f(0) = {r['df0']:.2f}",
        f"{{/Symbol D}}{{/Symbol r}}_0 = {r['flat']:.3f} eÅ^{{-3}}",
        f"{{/Symbol r}}_{{0,min}} = {r['min']:.3f},  {{/Symbol r}}_{{0,max}} = {r['max']:.3f}",
        f"d^f = 2 at {{/Symbol r}}_0 = {r['neg2']:.3f}, {r['pos2']:.3f}",
        f"e_{{gross}} = {r['e_gross']:.2f} e,  e_{{net}} = {abs(r['e_net']) if abs(r['e_net']) < 0.005 else r['e_net']:.2f} e",
    ])


def main():
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    p.add_argument("cubes", nargs="+", help="cube file, optionally :label")
    p.add_argument("--out", default="fractal_dimension.png")
    p.add_argument("--title", default="fractal dimension (d^f) vs. residual density ({/Symbol r}_0)")
    p.add_argument("--range", type=float, default=0.0,
                   help="half-width of the rho0 axis in e/A^3 (default: fit the data)")
    args = p.parse_args()

    data_file = args.out.rsplit(".", 1)[0] + ".dat"
    colours = ["#1f4e9c", "#c0392b", "#2e8b57", "#8e44ad"]
    plots, labels = [], []
    top = 0.0
    with open(data_file, "w") as f:
        for i, spec in enumerate(args.cubes):
            path, _, name = spec.partition(":")
            rho, volume = read_cube(path)
            r = analyse(rho, volume)
            top = max(top, abs(r["min"]), abs(r["max"]))
            f.write(f"# {path}\n# rho0  d^f\n")
            for x, y in zip(r["levels"], r["df"]):
                f.write(f"{x:9.3f} {y:8.4f}\n")
            f.write("\n\n")
            name = name or path
            print(f"{name}: d^f(0) = {r['df0']:.3f}  Delta rho0 = {r['flat']:.3f}  "
                  f"rho0 min/max = {r['min']:.3f}/{r['max']:.3f}  d^f=2 at {r['neg2']:.3f}, "
                  f"{r['pos2']:.3f}  e_gross = {r['e_gross']:.2f}  e_net = {r['e_net']:.3f}  "
                  f"(V = {volume:.2f} A^3, {rho.size} points)")
            c = colours[i % len(colours)]
            key = f"title '{name}'" if len(args.cubes) > 1 else "notitle"
            plots.append(f"'{data_file}' index {i} using 1:2 with points pt 7 ps 0.45 lc rgb '{c}' {key}")
            y_text = 0.95 - 0.30 * i
            labels.append(f"set label {i+1} \"{label_text(r)}\" at graph 0.02, graph {y_text:.2f} "
                          f"left front font ',9' tc rgb '{c}'")

    half = args.range or math.ceil(top * 10) / 10
    tick = 0.1 if half <= 1.5 else 0.5
    script = f"""
set terminal pngcairo size 900,640 enhanced font 'Helvetica,11'
set output '{args.out}'
set title "{args.title}"
set xlabel "{{/Symbol r}}_0 [eÅ^{{-3}}]"
set ylabel "d^f"
set xrange [{-half}:{half}]
set yrange [0:3.3]
set xtics {tick} format '%.1f'
set ytics 1 format '%.1f'
set grid ytics lc rgb '#bbbbbb'
set arrow from 0,0 to 0,3.3 nohead lc rgb '#c8c84a' lw 1
set key top right
{chr(10).join(labels)}
plot {', '.join(plots)}
"""
    subprocess.run(["gnuplot"], input=script, text=True, check=True)
    print(f"wrote {args.out} and {data_file}")


if __name__ == "__main__":
    main()
