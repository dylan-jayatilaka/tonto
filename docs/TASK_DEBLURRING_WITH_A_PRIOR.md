# Deblurring density maps with a benchmarked prior

A science task, **not being pursued yet** (Dylan, 2026-10-07: *later*). This page records the
idea, the arguments for and against it, its relation to what crystallography already does, and
the first test that would decide whether it is worth doing. The maps and the arithmetic it
rests on are in `docs/REPORT_ON_L_DECOMPOSITION_MAPS.md`, sections 2.1 and 2.5.

## 1. The idea

A map from measured structure factors is blurred twice: by the thermal motion of the atoms,
which the data carry and no model can remove from the data, and by the window of a local-moment
map, which we choose. Section 2.5 of the report shows that stepping back in blur is a known,
well-posed operation *as far as the data reach*: dividing every reflection by a mean
Debye-Waller factor, exactly (allowed while the window exceeds the mean ADP) or as a Taylor
polynomial in $`q^2`$ (bounded for any ADP, the local jet's own deblurring), with a Wiener
weight to keep the noise down. What it cannot do is reach past the resolution sphere, or treat
each atom's own motion.

The idea is to go further by bringing in a **prior**: a static deformation density from a
high-level quantum-chemical model, benchmarked, and applied only where the structure is dull.
The deblurred map would then be the data where the data are wanted, and the model where they
are not, with a declared line between the two.

## 2. Why the prior is defensible

- A HAR density that reproduces neutron bond lengths and ADPs, hydrogens included, has been
  tested in the one way crystallography can test a density, through its structure factors. That
  is what the X-ray and neutron test set is for (`~/Dropbox/tonto_data/xray_neutron_set`).
- For ordinary residues the static deformation density is transferable to about the accuracy
  needed, and using one in a protein is established practice under other names: the ELMAM,
  invariom and UBDB databases of transferable pseudoatom multipoles are exactly a benchmarked
  theoretical deformation density applied to a protein.
- The step from "right for the molecule it was refined on" to "right for the same fragment in a
  protein" is the transferability claim, and fragment-by-fragment benchmarking quantifies it.

## 3. Relation to density modification

Crystallography has iterated maps against models for forty years under the name density
modification: a map; a modification that is a constraint one believes (solvent flatness,
non-crystallographic symmetry, positivity, a histogram); back to reciprocal space; **keep the
observed magnitudes and take only the new phases**; refine; repeat. Deblurring with a benchmarked
prior is a modification of that kind, so the loop

```
HAR model -> map, deblurred with the prior away from the sites of interest
          -> structure factors: observed |F|, phases from the deblurred map
          -> re-refine the model -> ...
```

is legitimate under the two rules that make density modification honest rather than circular:

1. The observed $`|F|`$ are never replaced. A loop that writes deblurred $`F_{\rm calc}`$ into the
   "observations" converges to the prior and proves nothing.
2. Bias is measured, not assumed away: a set of reflections withheld from every step (the free
   $`R`$) tells whether the model plus prior predicts data it has not seen. Convergence is judged
   by the free $`R`$ and the residual map, never by agreement with the prior.

## 4. Where it would matter: proteins at 0.8 to 1.1 Å

Dylan has a set of protein structures at 1.1 Å and below, many at 0.8 Å, unpublished. At those
resolutions the analysis changes substantially:

- **The field maps work without change.** $`q_{\max} = 2\pi/d_{\min}`$ is 7.9 Å⁻¹ at 0.8 Å and
  5.7 at 1.1 Å, so the ripple-free window floor $`2/q_{\max}`$ is 0.25 Å, the default, at 0.8 Å,
  and 0.35 Å at 1.1 Å. Deformation features (0.2 to 0.4 e/Å³, 0.3 to 0.5 Å across) are at the edge
  of what 0.8 Å data hold directly; the sub-ångström protein studies (crambin at 0.54 Å, aldose
  reductase at 0.66 Å) saw bonding density, and 0.8 Å with a transferable deformation prior is
  where the databases have been applied with success.
- **The arithmetic deblurring is partial.** Well-ordered cores at cryogenic temperature have
  $`B \approx 5`$ to 12 Å², $`U \approx 0.06`$ to 0.15 Å², against a window of
  $`\sigma^2 = 0.06`$ to 0.12 Å²: the blur to remove is as large as the window, the exact division
  is out, the polynomial recovers part, and the per-atom spread of $`U`$ is fivefold. This is
  precisely the regime where a prior has something to add that the arithmetic cannot.
- **The atom decomposition is untouched by resolution.** It is made from the static wavefunction;
  the data enter only through the refined positions and ADPs. A fragment-by-fragment HAR gives
  the Hirshfeld atoms, their radial functions, multipoles and rebuilt maps at full quality whatever
  the resolution: the model side of every comparison, free.
- **Noise now matters.** Protein data have $`\sigma_F`$ comparable to $`F`$ in the outer shells, so
  the Wiener weight, invisible on urea, is the difference between a usable and an unusable
  experimental map.

## 5. The masked application, and how far the prior leaks

Apply the prior-based deblurring through a smooth mask in real space, away from the active site,
mobile side chains, methyl groups and solvent. Its influence on an untouched region then falls
off with the window's own tails, $`e^{-d^2/2\sigma^2}`$ in the distance $`d`$ from the mask's
edge: Gaussian, not polynomial, so a margin of three or four window widths (1 to 2 Å at the
widths a protein map allows) makes the leakage negligible. Two things cross the mask regardless
and must be named: the model's phases, which enter every map everywhere and already do; and the
thermal smearing of the active site itself, which the mask leaves alone -- so the critical region
stays exactly as blurred as the data make it, which is the right outcome. The margin is measured
from the atoms one cares about plus their window, not from a residue boundary.

What the prior-based deblurring really does in the well-ordered regions is substitute the
model's density for the data's, which refinement and a $`2F_o - F_c`$ map already do, less
transparently. The honest gain is therefore not resolution but **consistency**: one map in which
the dull parts are the benchmarked model and the active site is the data, with a declared
boundary. That is a legitimate object as long as the page that shows it says where the line was
drawn.

## 6. A learned prior, and why not yet

The blurring diffusion models of machine learning (Rissanen, Heinonen and Solin, *Generative
modelling with inverse heat dissipation*, 2023; Hoogeboom and Salimans, *Blurring diffusion
models*, 2022) are the Florack deblurring of report section 2.5 with the Taylor truncation
replaced by a prior learned from a collection of sharp examples, run across all scales at once.
The training pairs are manufactured by blurring clean examples with the known forward process,
so the model never learns real errors; it learns what clean examples look like, and the assumed
corruption (Gaussian, independent) is a prior as much as the examples are. For crystallography
the clean examples would be model densities, so the prior learned would be "what a B3LYP
crystal looks like", acting invisibly. The explicit, benchmarked, masked prior of section 5 is
the same idea with the prior in plain sight; a learned one would be a research programme of its
own, and the correlated, systematic errors of diffraction data (scale, extinction, absorption,
wrong ADPs) are exactly what a per-reflection Gaussian corruption model does not see.

## 7. The first test, with no new code

On one 0.8 Å structure after a HAR:

1. The experimental and model deformation densities and their $`l = 1, 2`$ local moments at a
   0.3 Å window (`cell_map= { kind= deformation_exp ... }` and `deformation_calc`,
   `wiener_weight= TRUE`), on the cell at 0.2 bohr.
2. Their correlation **per residue** rather than over the cell: a Python pass over the two cubes
   with a residue mask (urea gives 0.93 and gly-L-ala 0.70 over the whole cell; a protein will give
   less, and unevenly).

That map of correlations, residue by residue, is the map of where the data carry bonding
information beyond the model -- the active site against the rest -- *before* any deblurring is
attempted, and it is the benchmark a masked deblurring would then be judged against. A 0.8 Å
protein HAR is a day of computing on the Mac, more on the cluster; the maps are minutes.

## 8. What would decide it

- The per-residue correlation of step 2 showing a real split between a data-rich site and a
  model-like remainder. If the correlation is uniformly low, the data at 0.8 Å do not resolve the
  deformation and a prior would be filling in everywhere; if uniformly high, the model already
  suffices and the deblurring adds nothing.
- A free-$`R`$ gain, or none, from the loop of section 3 on that structure.
- A benchmark of the prior's transferability on the small-molecule fragments it is built from.

## 9. Log

- 2026-10-07. Written after the discussion that produced report section 2.5 and the
  `sharpen_order=`, `laplacian_order=` and `wiener_weight=` keywords. Filed as *later*; nothing
  started.
