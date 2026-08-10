# Tonto workshop

A guided introduction to Hirshfeld atom refinement (HAR) and X-ray constrained
wavefunction (XCW) fitting, driving **Tonto directly** — no GUI. Three worked
exercises, each with a results table for you to fill in from your own run.

Everything you need is in this repository, under `docs/workshop/`. The input
files are printed in full below, so you can read them here and check what you
are running.

| | Exercise | Program | Runs in |
|---|---|---|---|
| 1 | HAR on ammonia | `hart` | ~2 s |
| 2 | HAR on urea, then a Roby–Gould bond index analysis | `tonto` | ~30 s |
| 3 | XCW fitting on exercise 2's refined geometry | `tonto` | *(see below)* |

Together they are one procedure, not three: exercise 2 produces a wavefunction,
exercise 3 constrains that wavefunction against the diffraction data, and the
bond indices in exercise 2 are the first example of a property computed from it.

---

## Why bother

Ordinary refinement, as in SHELXL, minimises

$$M = \sum w\,(|F_o| - |F_c|)^2$$

where $F_o$ and $F_c$ are the observed and calculated structure factors. The
calculated ones come from a sum over atomic form factors $f_j$:

$$F(\vec{h}) = \sum_{j=1}^{N} f_j \, e^{2\pi i \vec{h} \cdot \vec{r}_j}$$

Those $f_j$ describe how an **isolated atom type** scatters, and they are read
from a table — Tables 4.2.6.8 and 6.1.1.4 of *International Tables* Vol. C.

Read that again, because everything follows from it: every atom is modelled as
an isolated, non-interacting sphere sitting at the centre of its electron
cloud, regardless of what it is bonded to or what its oxidation state is.

**Hirshfeld atom refinement** computes the form factors instead of looking them
up — for each atom, not each atom *type* — from a quantum chemical calculation
on the actual molecule. The atomic densities that result are aspherical and
distorted by their surroundings, as real ones are.

### The main reason is the wavefunction

The famous benefit of HAR is that hydrogen atoms come out right. A hydrogen's
electron density peaks *inside* the bond, not at the nucleus, so a spherical
model places it too close to its neighbour — the well-known shortening of X–H
distances in X-ray structures. HAR removes that bias and puts hydrogen where
neutron diffraction puts it. You will see this happen in exercises 1 and 2, and
it is the easiest thing to check.

But it is not the main reason to do HAR. The main reason is this:

> HAR gives you a **wavefunction for the system at that geometry**. That
> wavefunction can be fitted further — that is what XCW does, in exercise 3 —
> and properties can be computed from it: properties consistent with an
> electron density that has been fitted to X-ray diffraction data.

That is the point. An ordinary refinement gives you coordinates and thermal
parameters, and nothing else; the model has no electrons in it that you could
ask a question of. HAR gives you a density, and XCW makes that density answer
to the experiment. Bond indices, electrostatic potentials, energies, ELF — all
become *experimentally constrained* quantities rather than purely theoretical
ones. Exercise 2 computes the first of these.

## How HAR works

Starting from an ordinary refined structure — HAR is a *post-IAM* procedure:

1. **A single-point calculation** gives the molecular electron density.

2. **That density is partitioned** into atoms by Hirshfeld's stockholder
   scheme, each atom taking a share proportional to what a free atom would
   contribute there:

   $$\rho_A(\vec{r}) = w_A(\vec{r}) \cdot \rho_{\text{molecule}}(\vec{r})
   \qquad
   w_A(\vec{r}) = \frac{\rho_A^0(\vec{r} - \vec{r}_A)}{\sum_B \rho_B^0(\vec{r} - \vec{r}_B)}$$

   Each atomic density is then smeared by thermal motion and Fourier
   transformed into a scattering factor.

3. **A least-squares refinement** against the measured reflections, using those
   tailor-made scattering factors.

The geometry has now changed, so the density is out of date — go back to step 1.
Repeat until nothing moves.

## And XCW

HAR fits *positions* to the data, with the wavefunction along for the ride. XCW
fits the **wavefunction itself**. It minimises

$$E[\Psi] + \lambda \left( \chi^2[\Psi] - \Delta \right)$$

— the quantum mechanical energy, plus the disagreement with the diffraction
data weighted by a multiplier $\lambda$. At $\lambda = 0$ you have an ordinary
Hartree–Fock wavefunction that has never seen the experiment. As $\lambda$
rises, the wavefunction is pulled towards the data: $\chi^2$ falls, the energy
rises above its variational minimum, and the orbitals change.

How far to push $\lambda$ is a judgement call, and that is exactly what
exercise 3 asks you to look at.

## Three things to know before you start

1. **Reflection files must be merged and pruned of systematic absences.**

2. **The molecule must be chemically complete.** HAR runs a quantum chemical
   calculation on whatever fragment you give it. If the asymmetric unit holds
   a third of a molecule, you must complete it first, or the calculation is
   meaningless. Both molecules here are completed for you.

3. **Tonto eliminates linear dependencies in the least-squares matrix
   automatically**, so it has no restraints or constraints. That can cause
   trouble for spherical ions.

---

## Before you begin

Build Tonto by following [BUILDING_TONTO.md](BUILDING_TONTO.md). The quick
version, on Linux or macOS:

```bash
git clone --recursive https://github.com/dylan-jayatilaka/tonto.git
cd tonto && mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=release
make -j3
```

That gives you two programs, `build/tonto` and `build/hart`. Tell Tonto where
its basis sets live, once, and you are ready:

```bash
export TONTO_BASIS_SET_DIRECTORY=<path-to>/tonto/basis_sets
```

Then copy an exercise directory somewhere of your own and work there:

```bash
cp -r <path-to>/tonto/docs/workshop/1-nh3-hart ~/workshop-1
cd ~/workshop-1
```

---

## Exercise 1 — ammonia, with `hart`

`hart` is the standalone Hirshfeld-atom-refinement program. It takes no input
file: **the command line is the input.** That makes it the quickest way to see
a HAR happen.

Two data files are provided:

| | |
|---|---|
| `nh3.cif` | the starting independent-atom structure — *P* 2₁3, *a* = 5.1305 Å |
| `nh3.hkl` | 88 reflections, free format: `h k l F sigma` |

Run it:

```bash
hart --job nh3 \
     --basis def2-SVP \
     --std-f nh3.hkl \
     --dtol 0.01 \
     --grid-accuracy low \
     nh3.cif
```

About two seconds. What the options mean:

| Option | |
|---|---|
| `--job nh3` | names every output file |
| `--basis def2-SVP` | the Gaussian basis set. Also the default — spelled out so all three exercises visibly agree |
| `--std-f nh3.hkl` | free-format `h k l F sigma`. Use `--std-f2` for *F*², or `--shelx-f`/`--shelx-f2` for the fixed-format SHELX layout |
| `--dtol 0.01` | DIIS convergence tolerance |
| `--grid-accuracy low` | the numerical integration grid. Enough here; raise it for published work |

`hart --help` lists the rest. Note that `hart` has **no option for the SCF
energy convergence** — exercises 2 and 3 set `convergence= 0.001` explicitly,
and exercise 1 simply cannot, so it runs at hart's internal default.

### What you should get

Results are in `nh3.out`; look for `Structure refinement results`. The refined
structure, with esds, is in `nh3.archive.cif`.

| NH₃ | SHELX IAM | HAR (mine) | HAR (yours) |
|:---|:---:|:---:|:---:|
| R(F) | 0.0071 | 0.0101 | ? |
| Rw(F) | — | 0.0096 | ? |
| GoF²  | — | 1.0737 | ? |
| ρ<sub>max</sub> / e Å⁻³ | 0.014 | 0.0373 | ? |
| ρ<sub>min</sub> / e Å⁻³ | −0.013 | −0.0571 | ? |
| reflections | 98 | 84 | ? |
| parameters | 8 | 13 | ? |
| **N–H distance / Å** | **0.842(7)** | **0.988(7)** | ? |

The last row is the one to look at. The independent-atom model puts the
hydrogen 0.842 Å from the nitrogen; HAR moves it out to 0.988 Å. Neutron
diffraction gives about 1.01 Å.

The SHELX IAM column is quoted from Malaspina's lab notes and uses a slightly
different reflection selection, so the R factors are not exactly comparable —
the hydrogen distance is.

### The fit plots

At the end of a refinement Tonto draws four diagnostic plots itself, using
gnuplot. They are the fastest way to see whether anything is wrong with the fit.

| | |
|---|---|
| ![Normal QQ plot](images/workshop/nh3.QQ_plot.png) | ![F_z vs sin(theta)/lambda](images/workshop/nh3.F_z_vs_stl.png) |
| **Normal QQ plot.** If the errors are normally distributed the points lie on a straight line through the origin with slope 1. The fitted line and its equation are drawn for you; the six worst outliers are labelled with their (*h k l*). | **F_z against sin θ/λ.** Systematic structure here means a resolution-dependent error — an extinction, thermal-motion or scattering-factor problem. You want a featureless band. |
| ![F_z vs F_exp](images/workshop/nh3.F_z_vs_F_exp.png) | ![Delta F vs sin(theta)/lambda](images/workshop/nh3.Delta_F_vs_stl.png) |
| **F_z against F_exp.** A trend here points at the weighting scheme, or at extinction on the strong reflections. | **ΔF against sin θ/λ.** The unnormalised residual — shows you which reflections dominate in absolute terms rather than in units of σ. |

For ammonia the QQ plot is close to a straight line of slope 0.932, with (1 1 1)
sitting well below it — one reflection fitting worse than a normal distribution
would allow. With only 88 reflections that is not alarming.

### Things to try

- Raise `--grid-accuracy` to `high` and see whether anything moves. If it does,
  the `low` grid was not adequate.
- Add `--cluster-radius 8` to surround the molecule with Hirshfeld charges out
  to 8 Å, simulating the crystal environment. Does the N–H distance change?
- Change `--fos` (the *F*/σ cutoff, default 3) to 4 and watch the residual
  density.

---

*Exercises 2 and 3 follow — being run and written now.*
