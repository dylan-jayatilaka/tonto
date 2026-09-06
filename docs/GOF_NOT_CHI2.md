# GoF², not chi2: a naming and reporting correction

**Agreed with Dylan, 2026-08-22.** Two separate points, deliberately kept clear of the
extinction reactivation (`docs/EXTINCTION_REPORT.md`), because the rename is broad and
touches files that have nothing to do with diffraction.

1. The quantity the code calls `chi2` is a **GoF²**. Nothing in Tonto is a chi-squared
   in the sense of the name.
2. The refinement tables report the square. **GoF** — its square root, in units of
   sigma — is what a reader can interpret, and is what the tables should print.

The three parts below are independent of each other and can be done in any order. Only
part B changes numbers.

**Sequencing.** This was to have followed two other things: the extinction reactivation of
`docs/EXTINCTION_REPORT.md`, and choosing the XCW Lagrange multiplier, recorded as step 6
of that document's plan. Extinction closed on 2026-09-06; Dylan then reordered the
remaining two, so this went ahead of the multiplier work, which is milestone 12 and stays
open. Nothing here depends on it.

## The quantity

`VEC{REFLECTION}:F_chi2` (`vec{reflection}.foo:880`) returns `sum(F_z^2)/(N_r-1)`, and
`DIFFRACTION_DATA.SET:update_chi2` (`diffraction_data.set.foo:1536`) rescales it to
`sum(F_z^2)/(N_r-N_p)`. That is a GoF². So `.chi2`, `.chi2_fit`, `.chi2_fit0`,
`.chi2_ref`, `.chi2_ref0` and the `.chi2_increased` convergence test are all GoF².

The CIF output is already correct and needs nothing: `diffraction_data.put.foo:95`
writes `.chi2` to `_refine_ls_chi2`, and `:99` writes `sqrt(.chi2)` to the three
`_refine_ls_goodness_of_fit_*` items.

## A. The rename — broad, but no output change and no reblessing

About 200 identifier occurrences. **The scope must be drawn by hand**; a global
substitution would be wrong.

- **In scope:** `diffraction_data.{set,put,inq,read}.foo`, `vec{reflection}.foo`,
  `molecule.{scf,har}.foo`, `scf_data.foo`, `types.foo`, and three comment lines of
  `crystal.foo`.
- **Three corrections to the list as first written**, found when the rename was done on
  2026-09-06. It named `crystal.foo`, `molecule.main.foo` and `real.foo` in scope; none of
  them wholly is.
  - `real.foo:849` `chi2(x2,nu)` is a genuine **chi-squared probability distribution**,
    `incomplete_gamma(nu/2, x2/2)`, correctly named — and it has no callers anywhere.
    Renaming it would have been wrong.
  - `molecule.main.foo:488` `case ("put_uc_chi2_old")` is the χ⁽²⁾ susceptibility keyword,
    the same family as `cluster.foo` and `molecule.cp.foo`.
  - `crystal.foo` has 17 occurrences, of which **3** are in scope — the comments at `:3864`,
    `:3884` and `:4052`. The other 14, at `:13614–14063`, are χ⁽²⁾.
- **Out of scope — do not touch.** `cluster.foo` (14 occurrences) and `molecule.cp.foo`
  (29) use `chi2`, `chi2_nonH` and `chi2_ijk` for the second-order nonlinear
  susceptibility χ⁽²⁾. That is an unrelated quantity and the name is correct.
- **Judgement call.** `multi_t_adp.foo` (32) has its own `.chi2` / `.chi2_old` /
  `the_chi2` for the T-model ADP fit; `:1138` prints
  `.fitted_V_residual2/.V_coeff.dim`, a mean-square residual. Arguably the same rename,
  arguably a different quantity. Leave it, and note it, rather than change it in
  passing.

Renaming the stored member to `GoF2` also documents the trap in §D1 at every use site.

## B. Table columns GoF² → GoF — the part that needs reblessing

| Table | Heading was | Value was | Now prints |
|---|---|---|---|
| `put_fit_table_body`, `fit_table(2)` | `chi2` | `.GoF2_fit` | `GoF`, `sqrt(.GoF2_fit)` |
| `put_refinement_table_body`, `ref_table(3)` | `chi2 initial` | `.GoF2_ref0` | `GoF initial`, `sqrt(.GoF2_ref0)` |
| `put_refinement_table_body`, `ref_table(4)` | `chi2 final` | `.GoF2_ref` | `GoF final`, `sqrt(.GoF2_ref)` |
| **`SCF_DATA:set_table`, `table(3)`** | `GoF2` | `.penalty` | `GoF`, `sqrt(.penalty)` |

**The fourth table was missing from this list.** It is the XCW SCF iteration table — the
one a constrained SCF prints per lambda — and it is the table Dylan asked about, since an
XCW run reaches no other. Its heading was already spelled `GoF2`, which is why a grep for
`chi2` did not find it. `.penalty` itself stays squared: `fit_value = energy +
lambda*penalty` (`scf_data.foo:430`) is the objective. The neighbouring `E+L*GoF2` subhead
and the `Penalty in F` line are left alone — both name the penalty, which genuinely is a
GoF².

**The column widths do not move.** All three diffraction columns are sized by
`set_width_from(TEN**3)`, independent of the value, so taking a square root changes digits
and not alignment — which matters, because `scripts/test.py` compares line by line.

**Reblessing is four references, not the whole suite**, because the harness compares only
files named by `output:` in each `IO`. `stdout.full`, `stdout.good_residual` and `*.bad`
are not compared, and the HAR and XWR long tests print only the `GoF^2(N_p)` block, never a
table.

The `Model statistics based on structure factors` block already reports both —
`diffraction_data.put.foo:696–697` prints `GoF^2(N_p)` and `GoF (N_p)` — and needs
nothing.

## C. Two mislabelled plot titles — free

`put_GOF_vs_STL_plot:1210` and `put_GOF_vs_F_exp_plot:1290` are titled "GoF^2 vs …" but
already plot `sqrt(H/(C-.n_param))`. Title-only correction; no number moves.

The corresponding **tables** need nothing: `put_GOF_vs_STL_table:976` and
`put_GOF_vs_F_exp_table:1091` already take the square root and already head the column
`GOF`. The `put_ratio_*` routines at `:1247` and `:1327` correctly do not.

## D. Two traps

1. **Store GoF², print GoF.** `SCF_DATA:set_penalty` (`scf_data.foo:126`) assigns
   `.penalty = crystal.xray_data.chi2`, and the XCW functional is
   `E + lambda(GoF² - Delta)`. The stored member must stay squared; take the square root
   at output only. The same applies to `.chi2_increased`, whose comparison is unaffected
   by a monotone transform but should not be perturbed gratuitously.
2. `_refine_QCr_Psi_constraint 'lambda*chi2'` (`diffraction_data.put.foo:60`) is a CIF
   **value**, not a label. **Decision (Dylan, 2026-09-06): write and read the new spelling
   only.** It is now `'lambda*GoF2'` at `put.foo:60` and matched as `'lambda*GoF2'` at
   `read.foo:2542`, with no fallback. What this document did not say is that `:2542` is a
   *reader*, and the `if` there has no `else` — so a CIF written by an earlier Tonto is no
   longer recognised as an XCW, and such a job will run **unconstrained without saying so**.
   No test reblessing was needed: all 15 test CIFs carrying the item say `'none'`.
