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
  `crystal.foo`, `molecule.{scf,har,main}.foo`, `scf_data.foo`, `real.foo`, `types.foo`.
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

| Table | Heading now | Value now | Should print |
|---|---|---|---|
| `put_fit_table_body`, `fit_table(2)` | `chi2` | `.chi2_fit` | `sqrt(.chi2_fit)` |
| `put_refinement_table_body`, `ref_table(3)` | `chi2 initial` | `.chi2_ref0` | `sqrt(.chi2_ref0)` |
| `put_refinement_table_body`, `ref_table(4)` | `chi2 final` | `.chi2_ref` | `sqrt(.chi2_ref)` |

Every reference containing a refinement table changes: the `tests/hart/` suite, the HAR
and XWR long tests, and the IAM refinements.

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
   **value**, not a label. Changing it changes what the file asserts about the
   refinement, so it is a decision rather than a rename.
