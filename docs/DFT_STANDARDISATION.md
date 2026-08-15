# DFT standardisation and improvement

This page is the authoritative record for milestone 10 in `CLAUDE.md`: the state
of Tonto's density-functional machinery, the defects found in it, and the plan to
put it on a standard footing with libxc as the functional engine.

Everything below was established by **measurement** on 2026-08-12/13, using the
release binary `build/tonto` and the existing job
`tests/short/h2o_blyp_cc-pVDZ` with `real_precision= 12`. Nothing here was
inferred from reading alone, which matters: two of the three defects look
correct on the page and are visible only in the numbers.

## 1. What is already right

It is worth stating plainly, because the defect list below is long and could
give the wrong impression. The science is sound and the numerics are not naive.

- **The grid machinery is thorough and conventional.** Becke 1988 fuzzy-Voronoi
  partitioning with Bragg-Slater radii; Treutler-Ahlrichs and Mura-Knowles radial
  schemes; Lebedev-Laikov angular grids; Delley and Stratmann-Scuseria partition
  options; four pruning schemes. The `BECKE_GRID` setters validate their inputs
  properly, with `ENSURE(... .is_one_of([...]))` on every one.
- **The GGA potential uses the standard form** — `V0` plus `Vn·∇(φ_a φ_b)` — which
  avoids needing the Laplacian of the density.
- **The B3LYP coefficients are correct.** `0.08·E_LDA + 0.72·E_B88` is
  algebraically `0.80·E_LDA + 0.72·ΔE_B88`, the textbook form, with the 0.20
  exact-exchange fraction applied separately through `hybrid_exchange_factor`.
- **The total-energy bookkeeping is the standard correction trick**:
  `E = ½Tr(P(F+h)) + [E_xc − ½Tr(P·V_xc)]`.
- **The result is right.** BLYP/cc-pVDZ water gives −76.4002.

## 2. Defect 1 — every user-specified grid setting is discarded (FIXED)

**This is the cause of the long-standing difficulty reproducing DFT energies to a
required precision.**

`MOLECULE.SET:initialize_DFT_grids` (`molecule.set.foo:593`) carried the comment
*"Initialise DFT grids, if not already done so"* and then, with no such guard:

```foo
      .becke_grid.destroy
      .becke_grid.create
```

It has seven live call sites — `molecule.set.foo:588` (`set_SCF_defaults`),
`molecule.scf.foo:1777`, `:1829`, `:5863`, `:6050`, `:6125`, and
`molecule.rho.foo:5301` — so it runs on the way into every DFT SCF. The
`BECKE_GRID` the input block had just configured was destroyed and recreated at
type defaults.

**The `becke_grid= { … }` block was therefore inert for DFT.** `accuracy=`,
`kind=`, `pruning_scheme=`, `partition_scheme=`, `rho_cutoff=` and
`basis_function_cutoff=` were all read, all echoed back by `put_basics`, and all
discarded before a single grid point was used. Every DFT calculation Tonto has
run used the hard defaults: `accuracy= "low"` (25 radial points, L23, L17 for
hydrogen), `kind= "mura_knowles"`, `pruning_scheme= "treutler_ahlrichs"`.

Measured three ways, all bit-identical:

| Variation | Total energy |
|---|---|
| `accuracy=` swept `very_low` → `low` → … → `best` (seven runs) | **−76.400240454361** every time |
| `rho_cutoff= 0.5`, `basis_function_cutoff= 0.5` (absurd values) | −76.400240454361 |
| the entire `becke_grid= { … }` block deleted | −76.400240454361 |

Wall-clock time did not move either (0.76–0.82 s across the sweep), although
`put_basics` faithfully reported the grid growing from 20 radial/L17 to 65
radial/L71 — roughly a 35-fold change in point count. The reported grid was not
the grid used.

**The failure mode is the dangerous one.** The echo confirms the user's settings
back to them, so the output looks like proof the request was honoured.

### The fix

`MOLECULE.RHO:set_up_becke_grid` (`molecule.rho.foo:1205`) already had the
correct pattern, and `initialize_DFT_grids` now matches it:

```foo
      if (.becke_grid.deallocated) .becke_grid.create

      .becke_grid.set_atom_info(.atom)
      .becke_grid.set_grid_data
```

`set_grid_data` rebuilds the radial and angular grids unconditionally and is
`leaky`, so a changed `accuracy=` is picked up, and re-running after the atoms
move still works — which is what the unconditional destroy was presumably
protecting and achieved only by accident.

### Consequence for the test references

**Every checked-in DFT reference was produced with the defaults**, not with the
settings its own input requests. `tests/short/h2o_blyp_cc-pVDZ` asks for
`pruning_scheme= jayatilaka2` and never received it. Those references will move
once the fix lands, and they must be reblessed against a **converged** number
rather than against whatever the fixed code first prints.

A regression test must assert that two different `accuracy=` values give
*different* energies. The current suite cannot detect this defect, because no
test varies the grid.

## 3. Defect 2 — the density cutoff, and the systematic error it caused (FIXED)

**Cross-validated against g09 on 2026-08-13, and FIXED the same day. This is the
systematic discrepancy that had been a concern for many years, and it turned out
to be a screening threshold rather than a defect in any functional.**

### The cross-validation

Same geometry, cc-pVDZ, Cartesian d in both codes (Tonto's default;
`6D` forced in g09), both grid-converged, tight SCF:

| Functional | g09 | Tonto (`accuracy= best`) | Difference |
|---|---|---|---|
| **HF** | −76.0232731206 | −76.023273121245 | **1.2×10⁻¹⁰** |
| **Slater** (LDA exchange) | −75.1948035483 | −75.194803089769 | 4.6×10⁻⁷ |
| **B88** exchange | −76.0608370448 | −76.060827115508 | **9.9×10⁻⁶** |
| **BLYP** | −76.4002385321 | −76.400228241079 | 1.03×10⁻⁵ |

The chain isolates the cause completely. HF agreeing to ten decimal places rules
out the basis, the integrals, the geometry and the SCF machinery. Slater agreeing
to 4.6×10⁻⁷ rules out the grid quadrature — the integration itself is sound.
Nearly the whole BLYP gap is present in B88 alone, so LYP contributes only about
4×10⁻⁷. The error was therefore entirely in the Becke-88 gradient correction.

g09's own grid convergence, for reference: FineGrid −76.4002380205, UltraFine
−76.4002384784, 199974 −76.4002385321, 250974 −76.4002385317 — converged to
4×10⁻¹⁰, so the g09 side of the comparison is solid.

### The cause: `rho_cutoff`

`new_r_Becke88_x_energy_density` was checked term by term against Becke 1988 —
the `x_σ` definition, the 2^(−4/3) spin scaling, `asinh` written as
`log(x+sqrt(1+x²))`, β = 0.0042 — and its LDA part reduces exactly to the Slater
routine that already agrees. **The algebra is correct.** What is not correct is
the screening in front of it:

```foo
if (rho<.rho_cutoff) cycle
```

`rho_cutoff` **defaulted** to `TOL(6)`, i.e. 10⁻⁶ (it is now `TOL(10)` — see
*What to do* below), and points below it contribute
nothing at all. For LDA this is harmless — that integrand goes as ρ^(4/3) and
dies quickly in the tail. But B88's variable is `x = |∇ρ|/ρ^(4/3)`, which **grows
as the density decays**, so truncating the tail costs the gradient correction far
more than it costs LDA. That is precisely why Slater looked fine and B88 did not.

Sweeping it, B88-only at `accuracy= best`, against g09's −76.0608370448:

| `rho_cutoff` | Tonto B88 | vs g09 |
|---|---|---|
| **10⁻⁶ (default)** | −76.060827115508 | **+9.93×10⁻⁶** |
| 10⁻⁸ | −76.060837011831 | +3.30×10⁻⁸ |
| 10⁻¹⁰ | −76.060837243269 | −1.99×10⁻⁷ |
| 10⁻¹² | −76.060837243976 | −1.99×10⁻⁷ |
| 10⁻¹⁴ | −76.060837243977 | −1.99×10⁻⁷ |

**The default cutoff accounts for 98% of the discrepancy.** Lowering it collapses
the gap fifty-fold, to 2×10⁻⁷ — the same order as the Slater agreement, i.e.
ordinary residual quadrature difference between two independent codes. Tonto
converges by 10⁻¹⁰ and is already within 3×10⁻⁸ at 10⁻⁸.

### The general principle: reduced variables diverge in the tail, and worse with each derivative

This is not a quirk of B88. It follows from how reduced variables are built, and
it predicts that the problem **gets worse as one climbs Jacob's ladder**.

In an exponential tail, ρ ~ e^(−2αr), every derivative of ρ is proportional to ρ
itself: |∇ρ| ~ 2αρ, ∇²ρ ~ 4α²ρ, and τ → τ_W = |∇ρ|²/8ρ ~ α²ρ/2. But the reduced
variables divide by a *power* of ρ chosen for uniform-gas scaling, and those
powers grow with derivative order:

| Rung | Reduced variable | Denominator | Tail behaviour |
|---|---|---|---|
| LDA | — | — | — |
| GGA | `x = \|∇ρ\|/ρ^(4/3)` | ρ^(4/3) | **~ ρ^(−1/3)** |
| meta-GGA | `t = τ/τ_unif`, `q = ∇²ρ/ρ^(5/3)` | ρ^(5/3) | **~ ρ^(−2/3)** |

**Each additional derivative order costs another factor of ρ^(−1/3).** At the
10⁻⁶ cutoff that is a factor of 100 for a GGA and 10⁴ for a meta-GGA.

The consequence for the *integrand* is what matters. LDA's exchange energy
density goes as ρ^(4/3) and dies quickly. For B88 at large x, `asinh x ≈ ln 2x`,
so the gradient correction behaves as

```
   -β ρ^(4/3) x² / (1 + 6β x asinh x)  ~  -ρ^(4/3) · x / (6 ln 2x)  ~  -ρ / ln(1/ρ)
```

i.e. it decays only **linearly in ρ**, against ρ^(4/3) for LDA — a factor ρ^(−1/3)
more weight in the tail, exactly the 100× that produced the measured 10⁻⁵ error.

**Prediction, not yet measured:** a meta-GGA integrand should carry a further
ρ^(−1/3), making it around 100× more cutoff-sensitive again. If Tonto ever gains
meta-GGAs — which is one of the things wrapping libxc (§11) would bring within
reach — a ρ cutoff of 10⁻⁶ would be badly wrong, and even 10⁻⁸ would want
checking. This should be re-measured rather than assumed when the time comes.

The deeper conclusion is that **screening on ρ alone is the wrong criterion**.
The quantity that should be small before a point is discarded is the
*contribution to the integral*, not the density. A cutoff on ρ is a proxy that
happens to be safe for LDA and is progressively unsafe for everything above it.

### What to do

**DONE 2026-08-13: the default is now `TOL(10)`**, and the declaration in
`types.foo` carries a comment recording why — that `TOL(6)` was a systematic bias
rather than a neutral safety margin, and that the bias grows with the rung of the
functional. The full-cascade rebuild that a `types.foo` edit forces was paid for
deliberately, to land the change and its documentation together.

**Why 10⁻¹⁰, and why it is free.**

Timed 2026-08-13, BLYP/cc-pVDZ water, minimum of three runs each:

| accuracy | 10⁻⁶ | 10⁻⁸ | 10⁻¹⁰ | 10⁻¹² |
|---|---|---|---|---|
| `low` | 0.70 s | 0.70 s | 0.72 s | 0.70 s |
| `best` | 10.85 s | 10.78 s | 10.62 s | 10.80 s |

There is **no trend at either accuracy** — the spread is run-to-run noise. The
mechanism explains it: the `cycle` skips a few flops per point, while the
expensive work (evaluating the basis functions and the density on the grid, then
the matrix contraction) happens whether or not the point is later discarded. The
cutoff was never buying speed; it was only losing accuracy.

The accuracy gain is large. Full BLYP at `accuracy= best`:

| `rho_cutoff` | Tonto BLYP | vs g09 (−76.4002385321) |
|---|---|---|
| 10⁻⁶ (default) | −76.400228241079 | 1.03×10⁻⁵ |
| 10⁻⁸ | −76.400238275944 | 2.56×10⁻⁷ |
| **10⁻¹⁰** | **−76.400238497330** | **3.5×10⁻⁸** |
| 10⁻¹² | −76.400238498008 | 3.4×10⁻⁸ |

**A factor of 300, for nothing.** At 10⁻¹⁰ the agreement with g09 is better than
the Slater comparison (4.6×10⁻⁷) and about as good as two independent codes with
different grids and pruning schemes can be expected to get. 10⁻¹² buys a further
7×10⁻¹⁰, which is below the noise floor of the comparison, so 10⁻¹⁰ is the right
default.

Two honest limits on this measurement: it is one small molecule, so the timing
claim should be re-checked on a larger system before the default is treated as
free everywhere; and `basis_fn_cutoff` and `basis_fn_pair_cutoff`, which prune by
basis function rather than by density, were held fixed throughout and may carry a
similar bias of their own.

This also could not have been found before the grid fix of §2: with the input
block inert, every comparison was pinned to one coarse default grid and the
cutoff could not be varied at all.

## 4. Defect 3 — `use_spherical_basis=` is silently ignored after `atoms=`

The basis is resolved when the atoms are read (`molecule.read.foo:145`,
`if (.use_spherical_basis) .basis.set_spherical(TRUE)`), so setting the keyword
after the `atoms=` block has no effect — and nothing says so.

| Keyword position | `n_bf` | Total energy |
|---|---|---|
| before `basis_name=` | 24 (spherical) | −76.398636066956 |
| after `basis_name=`, before `atoms=` | 24 (spherical) | −76.398636066956 |
| **after the `atoms=` block** | **25 (Cartesian)** | −76.400242979280 |

Accepted, exit 0, no diagnostic. The bottom of a job file is a natural place to
put a switch like this, and a user who does so silently gets Cartesian functions
and an error of 1.6×10⁻³ Hartree — around 1 kcal/mol — in this small example.

Same class as §2: a keyword that is read, has no effect, and says nothing. The
fix is either to apply it late enough to matter, or to refuse it once the basis
has been resolved. Refusing is the safer of the two, since silently re-resolving
a basis under a job that has already used it invites a different bug.

## 5. Defect 4 — an unrecognised functional name silently removes the functional

`dft_exchange_functional=` and `dft_correlation_functional=` accept any string. A
name matching no `case` contributes nothing: no exchange, no correlation, no
diagnostic, exit 0. The results block never states which functional was used.

| `dft_exchange_functional=` | `dft_correlation_functional=` | Total energy | Exit |
|---|---|---|---|
| `becke88` (the test as written) | `lyp` | **−76.4002** | 0 |
| `blyp` | `lyp` | −67.7092 | 0 |
| `blyp` | `pbe` | −67.3817 | 0 |
| `gill96` | `lyp` | −67.7092 | 0 |

`blyp` is the standard name for the functional that test computes, and it is
wrong by 8.7 Hartree. `blyp` with `pbe` is a Coulomb-only calculation — no
exchange at all — reported as DFT.

**Root cause: eight `case default; UNKNOWN(...)` lines are commented out** —
`dft_functional.foo` lines 165, 196, 236, 281, 320 and 392, and `scf_data.foo`
lines 615 and 639. The one *live* `case default` in `dft_functional.foo`, line
105, is the input-block keyword reader, so the block's keywords are checked while
the functional names inside them are not. `becke_grid.foo` validates every one of
its names; the functional layer validates none.

**`gill96` is the worst case, and it is not user error.** It is blessed as a valid
GGA exchange functional in three places — `scf_data.foo:638`,
`dft_functional.foo:159` and `:190` — and **no Gill96 routine exists anywhere in
`foofiles/`**. It reproduces the typo result to the digit.

Two aggravating details:

- `set_exchange_functional` resets `.using_hybrid_exchange` at the top but **not**
  `.using_GGA_exchange`, so an unknown name following a valid GGA name leaves the
  GGA flag set.
- The vocabulary is small and idiosyncratic — `becke88`, `b3lypx`/`b3lypc`,
  `b3lypgx`/`b3lypgc`, `slater`, `xalpha`, `vwn3`, `vwn5`, `lyp` — with no `blyp`,
  `b3lyp` or `pbe`, and exchange and correlation must be given as two separate
  keywords. Most natural spellings are silently wrong.

**The fix is not simply uncommenting the eight lines.** Blank and `"none"` names
flow through these dispatchers legitimately, so the default must admit them; and
`gill96` needs either an implementation or removal from the three places that
bless it. A test asserting that a bogus name fails loudly belongs with the fix.

## 6. Defect 5 — the XC energy is never reported

`V_ee` in the results block lumps Coulomb and XC together (37.3425 = J + E_xc for
the BLYP test), so the XC energy never appears. The only routine that would print
it, `MOLECULE.SCF:put_SCF_energy` (`molecule.scf.foo:4082`), has **zero call
sites** — dead code, wired to no keyword.

It is also mislabelled. It prints `.SCF_data.DFT_energy_correction` as *"The
Kohn-Sham DFT XC energy"*, but that variable holds `E_xc − ½Tr(P·V_xc)`, which
for pure LDA exchange is `E_x/3`.

Reporting the XC energy properly would have made every row of the defect-2 table
obvious at a glance. That is the argument for fixing it: it is not cosmetic, it is
the missing instrument.

## 6a. RESOLVED: the open-shell discrepancy was three separate things

**Opened 2026-08-13 as "unrestricted DFT is ~1.5e-5 off g09, cause unknown".
Closed 2026-08-14.** Two of the three causes were real defects; the third was a
confounded variable introduced during the investigation itself. All three are
invisible in closed-shell work, which is why years of restricted calculations
never showed them.

### The result

H2O+ (doublet), cc-pVDZ, `accuracy= best`, default `treutler_ahlrichs` grid:

| calculation | vs g09 | reference |
|---|---|---|
| UHF | 2.0e-10 | -75.6327038760 |
| slater | +1.444e-06 | -74.7945774565 |
| slater + **vwn5** | +1.455e-06 | -75.3977985806 |
| slater + **vwn3** | +1.511e-06 | -75.5735204288 (g09 `SVWN`) |

All three DFT numbers land on the same ~1.5e-06 floor, and adding either
correlation functional now contributes **nothing of its own** beyond the
exchange-only control. That floor is Tonto's default grid against g09's, not a
functional error.

Before: slater+vwn5 was **+8.70e-05** on that same grid, and the VWN3 potential
had **no spin dependence at all**.

### Cause 1 -- `pruning_scheme= jayatilaka2` (a confound, now removed)

Every cc-pVDZ run in the investigation was derived from the
`tests/short/h2o_blyp_cc-pVDZ` job, which set `pruning_scheme= jayatilaka2`,
while every other basis used a clean template with the default. The basis was
never the variable; the pruning was. On the cation:

| pruning_scheme | vs g09 |
|---|---|
| `treutler_ahlrichs` (default) | +1.44e-06 |
| `jayatilaka0` | +4.69e-07 |
| `jayatilaka1` | +1.63e-06 |
| **`jayatilaka2`** | **-1.54e-05** |

**Stated honestly: `jayatilaka2` was BETTER for the closed-shell neutral case**
-- 3.5e-08 for BLYP against 1.6e-06 on the default grid. It was not uniformly
worse, and the earlier claim that it was "too rough" everywhere is withdrawn.
Its closed-shell accuracy looks like error cancellation rather than quality,
since the same scheme is ten to thirty times worse than every alternative on a
harder density.

**It was removed anyway, and the reason is robustness rather than average
accuracy.** A method that is excellent on easy cases and unreliable on harder
ones cannot be offered as an option: the user cannot tell which case they are
in. `apply_pruning_scheme_J2` dropped to Lebedev L3/L5/L5/L7 over the inner half
of the radial range, which is very coarse. Removed 2026-08-14, along with a
`"truetler_ahlrichs"` typo in the validation list that made a DEBUG build reject
the correctly-spelled name and accept the misspelled one.

Because `apply_pruning_scheme` had no `case default`, and `ENSURE` is compiled
out in release, an old job file still naming `jayatilaka2` would have silently
run with **no pruning at all**. `set_pruning_scheme` therefore gained a live
`UNKNOWN` default and lost `PURE` (the same trade as section 5).

### Cause 2 -- the VWN5 chain rule was grouped wrongly

With `x = (3/(4 pi rho))^(1/6)` and `zeta = (rho_a-rho_b)/rho`, so that
`dx/drho = -x/(6 rho)` and `dzeta/drho_a = (1-zeta)/rho`:

    v_a = eps - (x/6) deps/dx + (1-zeta) deps/dzeta
    v_b = eps - (x/6) deps/dx - (1+zeta) deps/dzeta

The `-(x/6)` belongs only to the x-derivative and the `(1-+zeta)` only to the
zeta-derivative. The code applied **both factors to both terms**. Harmless at
zeta = 0, where `deps/dzeta` vanishes because `VWN_G'(0) = 0` -- which is
exactly why no closed-shell test could see it.

### Cause 3 -- VWN3 was evaluated at ZERO instead of zeta

    g   = VWN_G(ZERO) * (FOUR/NINE)/(2**(THIRD) - 1)
    d_g = VWN_dG(ZERO) * (FOUR/NINE)/(2**(THIRD) - 1)

`VWN_G(0) = 1.125*(1+1-2) = 0` and `VWN_dG(0) = 0`, so `g` and `d_g` were
**identically zero**: `e` collapsed to `eps_p` and `V0a` equalled `V0b` for any
spin polarisation. **The unrestricted VWN3 potential had no spin dependence
whatsoever.** Its ENERGY routine was correct throughout, which is why the error
never appeared in an energy expression check. A sign error on the zeta term sat
underneath it, so fixing either alone would have looked like no improvement.

`VWN_dG` and `VWN_dH` were both verified against their definitions and are
correct; the defects were confined to the two potential routines.

### What this floor means, and the tolerance to use

Tonto's default grid sits a consistent **~1.5e-06** from g09 across functionals
and charge states, at the finest `accuracy=` available. That is a
characterisation of the grid, not a defect, but it is a floor.

**Any external-reference test should therefore use a tolerance of 5e-06.** Note
the four candidate reference values recorded earlier in this document were
measured on `jayatilaka2` (BLYP 3.5e-08, B88 2.0e-07, slater 4.6e-07) and do NOT
hold on the default grid, where the DFT ones are ~1.6e-06. They must be
re-measured before use.

Driving that floor lower is worth doing but is **not** a near-term task; it
belongs with the long-term re-engineering argued in `CLAUDE.md`. The property
and reference tests built here are precisely the harness such a transition would
need: they state what must be true independently of how it is implemented.

## 6b. OPEN: the grid needs far too many points for the accuracy it gives

**Not for investigation now. Recorded because the evidence is unusually clean
and it points at an implementation problem, not a tuning question.**

At `accuracy= best` -- Tonto's finest setting, 65 radial points and Lebedev L71
-- every DFT case sits about **1.5e-06** from g09:

| system | functional | \|Tonto - g09\| |
|---|---|---|
| H2O | HF | 6.5e-10 |
| H2O | slater | 1.512e-06 |
| H2O | becke88 | 1.584e-06 |
| H2O | becke88+lyp | 1.629e-06 |
| H2O | slater+vwn5 | 1.596e-06 |
| H2O+ | UHF | 2.0e-10 |
| H2O+ | slater | 1.444e-06 |
| H2O+ | slater+vwn5 | 1.455e-06 |
| H2O+ | slater+vwn3 | 1.511e-06 |

**Two things stand out.**

The two HF rows agree to 1e-10 and use NO grid. Every case that touches a grid
is three to four orders worse. So the residual is the quadrature, not the basis,
the integrals or the SCF.

And the seven DFT numbers span 1.44e-06 to 1.63e-06 -- a 13% spread across
different functionals, different charge states, and both spin treatments. A
functional error would vary with the functional; a grid offset would not. This
is a grid offset.

### Why this looks like a defect rather than a limit

g09 reaches its converged answer on a FAR smaller grid. Its FineGrid, (75,302),
gives -76.4002380205 for BLYP against its own converged -76.4002385321 -- within
**5e-10**. Tonto at 65 radial and L71 is **1.5e-06** from that same number.

Even allowing that g09's grids are pruned and raw point counts are not directly
comparable, that is roughly three orders of magnitude of accuracy for a grid of
broadly similar size. Something in the quadrature is not doing the work those
points should be doing.

Candidates, none investigated:

- the **Becke partition weights** and the atomic-size adjustment
- the **radial mapping** and its scaling (`kind= mura_knowles` by default, with
  `treutler_ahlrichs` and `becke` also available)
- the **pruning** interaction -- note `jayatilaka2` was removed in section 6a for
  being unreliable, and the remaining schemes have not been characterised
- whether the atomic grids are being **normalised** correctly, which a uniform
  offset of this kind would be consistent with

### Why it matters beyond neatness

It sets the floor for everything else. The `dft_reference` test (`long`) has to
use a 5e-06 tolerance purely because of this, and at `accuracy= high` the
deviations already exceed that -- so a cheap grid cannot be used for absolute
comparisons at all. Fixing the quadrature would let that tolerance drop by
orders of magnitude, and would make every DFT result cheaper as well as more
accurate.

**A rewrite of the grid construction should be considered**, not merely a tuning
pass. This is worth doing before, or as part of, any of the larger re-engineering
in `CLAUDE.md`.

## 7. Assessed and deliberately left alone: how E_xc is evaluated

`molecule.fock.foo` accumulates the XC energy as a density-matrix contraction,

```
E = Σ_ab D_ab Σ_n w_n ε_n φ_a(n) φ_b(n)
```

rather than the direct quadrature `Σ_n w_n ε_n ρ_n`, even though ρ is already on
the grid as `N0`. This was initially raised as redundant work and that criticism
is **withdrawn**, for two reasons:

- **It is linear scaling.** With `skipab` threshold rejection and the
  overlapping-atom restriction, the surviving (shell-pair, point) triples grow as
  O(N), exactly like the potential build it rides on. There is no complexity
  penalty. The earlier O(N_bf²·N_pt) figure was the unscreened count and was not a
  fair description. What remains is a prefactor: one extra fused multiply-add in
  the innermost loop, roughly a third of its arithmetic.
- **It keeps E and V consistent.** `N0` is built by `make_rho_becke_atom_grid`
  under `basis_fn_cutoff`/`skipa0`, whereas the XC matrix uses
  `basis_fn_pair_cutoff`/`skipab`. Computing E from ρ would screen it differently
  from V, breaking their consistency by a screening-dependent amount. For
  precision work that is worth more than a third of one loop.

Left as it is.

## 8. Not implemented: XC gradients

There are no grid-weight derivatives and no XC gradients anywhere in `foofiles/`.
This is consistent with Tonto refining positions against structure factors by
least squares rather than by energy gradients, but it means DFT forces and
geometry optimisation are unavailable. Recorded as a fact about scope, not as a
defect.

## 9. The functional interface: how it compares to the standard one

Assessed on 2026-08-12, because it determines how much work wrapping libxc is.
The short answer: **the mathematics is standard, the interface partition is the
older convention, and it adapts to libxc through a thin, well-defined adapter.**

### What matches standard practice

- **Batched, not per-point.** Every argument is a `VEC{REAL}` over a whole grid
  batch, which is what libxc does with `np` and what makes vectorisation
  possible.
- **Additive semantics.** `E` and `V0` are `INOUT` and accumulate, so exchange
  and correlation compose by two calls into the same arrays. That is what makes
  the separate `exch`/`corr` keywords work, and it is a clean design.
- **The GGA potential form is textbook.** `V0 = ∂F/∂ρ_a` with
  `Vn = [2 ∂F/∂γ_aa + ∂F/∂γ_ab] ∇ρ_n`, and the matrix element assembled as
  `∫ V0 φ_aφ_b + Vn·∇(φ_aφ_b)`. This avoids the density Laplacian and second
  derivatives of the functional entirely.
- **Density screening on the functional object** (`rho_cutoff`) corresponds to
  libxc's `xc_f03_func_set_dens_threshold`.

### Difference 1 — gradient components in, contracted vector out

libxc, and essentially every modern functional library, take the invariant
`σ = ∇ρ·∇ρ` and return `vsigma = ∂f/∂σ`, leaving the contraction with `∇ρ` to the
integrator. Tonto instead passes the Cartesian components `Nx,Ny,Nz` and returns
`Vx,Vy,Vz` **already contracted**.

The mathematics is identical — the contraction must happen somewhere — so this is
a question of *placement*, not correctness. Tonto puts it in the functional layer;
the modern convention puts it in the integrator, because that lets one functional
library serve many integrators. Tonto's is the older arrangement, and it is not
wrong.

For the wrapper this is a small, exactly specified adapter:

- **in:** `σ = Nx² + Ny² + Nz²`
- **out:** `V0 = vrho`, and `Vn = 2·vsigma·∇ρ_n`

**This is the one place a wrapper can silently go wrong.** Tonto's
`2 ∂F/∂γ_aa + ∂F/∂γ_ab` is the spin-resolved expression reduced to the restricted
case, and the factor that survives that reduction is easy to get wrong by two.
The libxc-derived `Vn` must be checked numerically against
`new_r_Becke88_x_potential` at a handful of grid points before anything is
believed.

### Difference 2 — the functional is evaluated twice per batch

This is a genuine inefficiency rather than a stylistic difference.
`molecule.fock.foo` calls

```foo
         .new_set_r_XC_potentials(V0,N0,Vx,Vy,Vz,Nx,Ny,Nz)
         .new_set_r_XC_energy_density(E0,N0,Nx,Ny,Nz)
```

at `:4490-4491` (LDA) and `:4748-4749` (GGA) — two separate passes, each
re-entering the `select case` and re-evaluating the functional over the whole
batch. libxc offers `xc_f03_gga_exc_vxc`, which returns both from a single
evaluation sharing all intermediates; that is the standard arrangement.

The matrix build dominates the total cost, so this is not the hot spot, but it
doubles the functional-evaluation work for no return. **Wrapping libxc is the
natural moment to fix it** — expose a combined energy-and-potential call and
collapse those two call sites into one.

### Two minor warts

- **`self :: INOUT` and `.name = name`.** Each dispatcher stores the functional
  name on the object although the name is also an argument. That is a hidden side
  effect in what is otherwise a pure evaluation, and it is why these routines are
  `INOUT` rather than `IN`. The stored copy exists so `is_LDA_functional` can be
  called with no argument.
- **`Nx,Ny,Nz` are `optional`, with `present()` branching.** A reasonable Fortran
  idiom, but it is exactly what let the `archive/libxc` prototype dereference
  absent arguments on every LDA functional. libxc sidesteps the whole class by
  always taking `σ` and ignoring it for LDA families. Passing a family flag, or
  always passing `σ`, would make that mistake impossible rather than merely
  avoidable.

## 10. Reducing the argument counts

The dispatchers already carry more arguments than is comfortable, and the count
grows with the product of *rung* and *spin channels*, so the problem gets worse
rather than better:

| Routine | Arguments |
|---|---|
| `new_r_energy_density(name,E,N0,Nx,Ny,Nz)` | 6 |
| `new_r_potential(name,V0,N0,Vx,Vy,Vz,Nx,Ny,Nz)` | 9 |
| `new_u_energy_density(name,E,N0a,N0b,Nxa,Nya,Nza,Nxb,Nyb,Nzb)` | 10 |
| `new_u_potential(name,V0a,V0b,N0a,N0b,Vxa,Vya,Vza,Vxb,Vyb,Vzb,Nxa,Nya,Nza,Nxb,Nyb,Nzb)` | **17** |

A meta-GGA needs the kinetic-energy density τ in and `∂F/∂τ` out, which takes the
unrestricted potential to 21; if the density Laplacian is also wanted, 25. Note
that libxc has the same problem and does not solve it —
`xc_f03_mgga_exc_vxc` takes eleven arguments. The fix belongs in the layer above.

### Step 1 — two reductions that change no semantics

**Drop `name`.** `DFT_FUNCTIONAL` already has a `.name` member, and every
dispatcher's first act is `.name = name`. Having the caller do
`add.set_name(exch)` fits the existing idiom exactly — the call sites already do
`add.set_defaults` and `add.set_rho_cutoff(...)` — and it removes a hidden side
effect that is the only reason these routines need `self :: INOUT` rather than
`IN`. One argument off every dispatcher.

**Group each gradient triple into a matrix.** `Nx,Ny,Nz` becomes a single
`MAT{REAL}(n_pt,3)`. This is not a new idea imposed on the code; it is undoing
work the callers already do. `molecule.grid.foo:4160` creates
`grad_rho.create(V.dim,3)` and then line 4177 reads:

```foo
dft.new_r_potential(exch,V,rho,Vx,Vy,Vz,grad_rho(:,1),grad_rho(:,2),grad_rho(:,3))
```

The caller holds the matrix and splats it into three arguments so the callee can
treat them separately. Passing it whole removes two arguments per triple.

Together:

| Routine | Now | After step 1 |
|---|---|---|
| `new_r_energy_density` | 6 | 3 |
| `new_r_potential` | 9 | 4 |
| `new_u_energy_density` | 10 | 5 |
| `new_u_potential` | 17 | **8** |

### Step 2 — adopt σ and vsigma, with the libxc wrap

Replacing the gradient vector with the invariant `σ` on input, and the contracted
`Vn` with `vsigma` on output (§9, difference 1), does not reduce the count
further on its own. Its value is that the signature then *is* libxc's, so the
wrapper becomes close to a pass-through rather than an adapter with a factor of
two in it.

### Step 3 — bundle into types, which is the real answer

For anything above GGA, the only approach that stops the growth is to pass the
grid-batch data as objects:

```foo
   type XC_DENSITY            ! inputs on one grid batch
      rho    :: MAT{REAL}@     ! (n_spin,n_pt)
      sigma  :: MAT{REAL}@     ! (1 or 3, n_pt)
      tau    :: MAT{REAL}@     ! meta-GGA
      lapl   :: MAT{REAL}@     ! if ever needed
   end

   type XC_POTENTIAL          ! outputs, same shapes
      v_rho, v_sigma, v_tau, v_lapl :: MAT{REAL}@
   end
```

Every dispatcher then becomes two arguments and **stays** two arguments:

```foo
   new_energy_density(E,d)
   new_potential(V,d)
```

Adding a rung adds a *member*, and not one call site changes.

**The larger prize is not the arity — it is that the spin channel becomes data.**
With `n_spin` as an array dimension rather than a routine name, the restricted
and unrestricted variants merge. `dft_functional.foo` currently holds **20
`new_r_*` routines and 20 `new_u_*` routines**, a one-to-one duplication of forty
procedures. Collapsing it halves the surface for the functional-name defect of
§5, halves the libxc wrapping work of §11, and removes the duplication that is
exactly why a fix applied to `new_r_potential` can silently miss
`new_u_potential`.

### Three measured instances of that duplication hazard

Not hypothetical. All three were found while doing other work:

1. **`self :: INOUT` on two routines out of forty.** Making the four dispatchers
   `self :: IN` (§10, step 1) broke the build, because
   `new_u_B3LYP_c_energy_density` and `new_u_B3LYPG_c_energy_density` still
   declared `self :: INOUT` although neither mutates `self` — while their
   restricted twins already had `IN`. Two of forty, and only the unrestricted
   side. gfortran reported it as *"no specific subroutine for the generic"*,
   which names neither the intent nor the routine at fault.

2. **The cutoff is applied on one side and not the other.** Measured
   2026-08-13 while attributing the §3 cutoff change across the test suite:
   `h2o_xalpha_cc-pVDZ` (restricted) moved by **exactly zero** when `rho_cutoff`
   changed, while `h2o+_uxalpha_cc-pVDZ_promolecule_guess` — *the same
   functional*, unrestricted — moved by 3.15×10⁻⁶. The cause is visible in the
   two routines:

   ```foo
   new_r_Xalpha_x_energy_density:   E = E - const*N0**(THIRD)      ! no cutoff at all
   new_u_Xalpha_x_energy_density:   do i = 1,N0a.dim
                                       if (rhoa<.rho_cutoff) cycle ! cutoff applied
                                       if (rhob<.rho_cutoff) cycle
   ```

   The restricted form is a vectorised expression over the whole batch and never
   screens; the unrestricted form is an explicit loop and does. Same functional,
   same physics, two different numerical behaviours — and no way to notice
   without varying the cutoff and comparing the pair, which is what finally
   exposed it.

3. **Both are marked "Untested" in their own comments**, and were.

### A related confusion the same instance exposes: two roles, one threshold

`new_u_Xalpha_x_energy_density` screens because it computes `r_ba = rhob/rhoa`
and must not divide by zero. That is a **numerical safety guard**, and it wants a
value near the floating-point floor — 10⁻³⁰ would do. But the variable it uses,
`.rho_cutoff`, is simultaneously the **physics screening threshold** of §3, whose
correct value is set by how much of the density tail may be discarded before the
answer moves.

The two roles have nothing to do with each other and their right values differ by
twenty orders of magnitude. Conflating them means that tightening the physics
threshold silently also changes the division guard, and that loosening the guard
for safety would silently reintroduce the §3 systematic error. They should be
separate variables. The `TOL(10)` chosen in §3 is safe for both, so this is not
urgent — but it is exactly the kind of coupling that makes a future change
surprising.

Two further benefits worth noting: the `optional` gradient arguments disappear
entirely, which eliminates the absent-argument class that the `archive/libxc`
prototype fell into; and `σ` becomes a member computed once per batch rather than
recomputed inside every functional.

**Costs, stated honestly.** `types.foo` is the serial compile bottleneck — some
585 allocatable components whose generated `__copy_*` helpers are why it is pinned
to `-O1` — so two more types with allocatable members are not free, though two
small ones are marginal. They are also a natural first candidate for the
`types.foo` split recorded in `DEFERRED.md`, since nothing outside the DFT path
would use them. Purity is unaffected: a routine taking a derived type with
allocatable members stays `PURE` provided it does not allocate. And the call
sites get *simpler*, not harder — `molecule.fock.foo` presently creates and
destroys `N0,Nx,Ny,Nz,V0,Vx,Vy,Vz` one at a time.

### Recommendation

Do not run three migrations over the same signatures. Instead:

- **Now, independently:** drop the `name` argument. It is self-contained, tidies
  the `INOUT` side effect, and does not collide with anything else planned.
- **At the libxc wrap:** go straight to step 3, with σ/vsigma inside the bundle.
  One migration that ends at constant arity, libxc-aligned signatures, and a
  collapsed spin dimension — rather than three passes each touching the same
  forty routines.

**One detail to confirm before fixing the member layout:** libxc's spin-polarised
arrays are interleaved, and choosing `(n_spin,n_pt)` rather than `(n_pt,n_spin)`
would let the arrays be handed to libxc with no transpose. This has not been
verified against the 5.2.3 interface and should be, since the ordering is not
visible from the module file and it determines whether the wrapper copies.

## 11. libxc as the functional engine

The intended direction is for `DFT_FUNCTIONAL` to **wrap libxc** and become a
name-to-functional-ID map plus a thin call layer, with the hand-written routines
retained as the reference implementation to validate against.

Two things make this cleaner than it looks:

- **The units already agree.** Tonto's `E` is the functional *divided by the
  density* — energy per particle — which is exactly what libxc's `_exc` entry
  points return. Read `new_r_LDA_x_energy_density`, which computes
  `-(3/4)(3/π)^(1/3) ρ^(1/3)`. So the wrapper is a dispatch change, not a units
  change. An implementation that assumes `E` is an energy density is wrong by a
  factor of ρ.
- **The hybrid variant mapping is already known.** `b3lypx` corresponds to
  `XC_HYB_GGA_XC_B3LYP5` (VWN5) and `b3lypgx` to `XC_HYB_GGA_XC_B3LYP` (VWN_RPA,
  i.e. VWN3), matching Tonto's own `b3lypc`/`b3lypgc` split.

Both facts come from `archive/libxc` (Peter Spackman, 2017), which is a prototype
and **must not be merged** — it wires one of the four dispatch routines and that
one dereferences absent optional arguments on exactly the LDA functionals it
added. The full assessment is in `docs/REPOSITORY_BRANCHES.md`.

### What a real implementation must cover

1. **All four dispatch routines** — `new_r_energy_density`, `new_r_potential`,
   `new_u_energy_density`, `new_u_potential`. The potential is what enters the
   Fock matrix (`molecule.fock.foo:5066`); wiring only the energy converges the
   SCF on one functional and reports the energy of another. libxc returns `vrho`
   and `vsigma`, which must be assembled into Tonto's `V0` / `Vx,Vy,Vz` form.
2. **Exchange and correlation must map independently.** They are separate input
   keywords (`molecule.fock.foo:5027-5028`). Mapping an exchange label onto a
   whole hybrid XC functional and making the correlation label a no-op is correct
   only when the pair is used together, and silently wrong otherwise.
3. **A Foo-parsable handle type.** The prototype's `XCFUNC` is raw Fortran inside
   a Foo `type` block; `Foo.g4`'s `typeDef` body admits only `name :: TYPE`.
4. **A validation gate.** `slater` and `vwn5` are closed-form and should agree
   with libxc to near machine precision. Without such a test the two paths drift
   apart unobserved.
5. **`np` must be converted.** libxc 5.0.0 made it `size_t`; Tonto's `INT` is
   `integer(4)` (`include/macros.in:86`), so `N0.dim` needs `integer(c_size_t)`.
   Verified by compiling the prototype's call pattern against libxc 5.2.3: this is
   the only source change its calls need. On libxc 7.0.0 and later the functional
   constants also moved to a second module, `xc_f03_funcs_m`.
6. **A compiler decision, taken first.** Ubuntu's `libxc-dev` 5.2.3 ships
   `xc_f03_lib_m.mod` built by gfortran-15; Tonto builds with gfortran-14, which
   refuses to read it. Either Tonto moves to gfortran-15 or libxc is built from
   source with gfortran-14. Same constraint as MPI, and it wants the same CMake
   check.
7. **`FindLibxc.cmake` rewritten.** The prototype's version appends all three
   `find_library` results without checking them, and libxc removed the duplicate
   `libxcf90` at 6.0.0 — so from 6.0.0 it puts `LIBXC_F90_LIBRARY-NOTFOUND` on the
   link line while still reporting the package found. The f03 interface does not
   need `xcf90` at all.

## 12. Static analysis worth adding

All three defects above are mechanically detectable. These follow the pattern of
`scripts/check_parallel_lint.py` — source scans, registered as ctest, visible in
CI:

1. **Commented-out `case default` in a dispatcher or setter.** Catches all eight
   functional-name sites directly.
2. **Accepted-name against implemented-name cross-check.** Every string a `set_*`
   validator or `is_*_functional` blesses must have a live case in all four
   dispatchers. Catches `gill96`, and catches the next one automatically.
3. **`.X.destroy` immediately followed by `.X.create` on the same member.** The
   `initialize_DFT_grids` shape exactly. Rare enough to be near-zero false
   positive; each hit is either a deliberate reset that deserves a comment saying
   so, or this bug.
4. **Unreachable public routines.** The translator's `--dead-code-report` already
   does this; it is what shows `put_SCF_energy` has no callers.

## 13. Sequence

Each step gives the next one a trustworthy gate, so the order matters:

1. ✅ **Fix the grid bug** (§2). Without it no grid-convergence statement means
   anything.
2. ⬜ **Rebless the DFT test references** against converged numbers, and add the
   regression test that two `accuracy=` values must differ.
3. ⬜ **Fix functional-name validation** (§3), resolve `gill96`, add the
   fails-loudly test.
6. ⬜ **Report the XC energy** (§6) — wire up a corrected `put_SCF_energy`. This is
   the instrument the rest of the work needs.
7. ⬜ **Add the static-analysis checks** (§12), so none of the three classes can
   return.
8. ⬜ **Drop the `name` argument** (§10, step 1) — self-contained, and independent
   of everything else here.
9. ⬜ **Wrap libxc** (§11), gated by the `slater`/`vwn5` agreement test, and take
   the argument bundling (§10, step 3) in the same pass rather than migrating the
   same forty routines twice.
