# Plan: in-crystal atomic moments and polarisabilities for lattice energies (X23)

A working document (`CLAUDE.md` §1): the idea, the decisions so far and what to do first. No
results yet, so no report. **Shelved on 2026-11-24** (Dylan), to be taken up after the TFVC paper.

## 1. The idea

CrystalExplorer's model energies (CE-B3LYP, Mackenzie et al., IUCrJ 4, 575 (2017)) give the
interaction energy of each pair of molecules in a crystal as four terms, each with a scale factor
fitted to benchmark energies:

```math
E_{\rm int} = k_{\rm ele} E_{\rm ele} + k_{\rm pol} E_{\rm pol} + k_{\rm disp} E_{\rm disp} + k_{\rm rep} E_{\rm rep} \qquad (1)
```

$`E_{\rm ele}`$ is computed from the densities of the isolated molecules, and $`E_{\rm pol}`$ from
fixed free-atom polarisabilities in the field of the neighbours. The sum over the pairs gives the
lattice energy. The usual test of lattice energies is the X23 set: 23 molecular crystals whose
experimental sublimation enthalpies have been back-corrected to lattice energies (Reilly &
Tkatchenko, J. Chem. Phys. 139, 024705 (2013); Otero-de-la-Roza & Johnson, J. Chem. Phys. 137,
054103 (2012)). Urea is one of them.

The question is whether quantities that only a refinement against crystal data can supply
improve that formula:

1. **In-crystal atomic multipoles** for $`E_{\rm ele}`$: from the density of the molecule polarised
   by its crystal surroundings (cluster charges, or explicit neighbours), partitioned into TFVC
   atoms, in place of the isolated molecule's density.
2. **In-crystal anisotropic atomic polarisabilities** for $`E_{\rm pol}`$: from the response of the
   in-crystal density to an applied electric field, partitioned into TFVC atoms, in place of fixed
   free-atom values. This is the response machinery of the TFVC paper ($`\mathbf{d}_A`$,
   $`\mathbf{D}_A`$, the charge flux $`N'_{AB}`$) with a field in place of a nuclear displacement;
   Tonto already has the finite-field term (`E_field`). The charge flux becomes the charge-flow
   part of the atomic polarisability, which atom-centred models usually leave out.
3. **Refined geometries**, hydrogen positions included, from the X-ray data.
4. **The thermal correction from the ADPs.** The experiment measures a sublimation enthalpy at a
   temperature $`T`$, and the comparison with a lattice energy needs a vibrational correction,
   usually from a model. The refined ADPs carry that thermal information: normal-mode refinement
   from ADPs (NoMoRe; Hoser & Madsen, Acta Cryst. A72, 206 (2016) and A73, 102 (2017)) gives the
   vibrational enthalpy and entropy, so the computed value can be compared with the measured
   sublimation enthalpy with less model-dependent back-correction.

Items 2 and 4 cannot be obtained from deposited coordinates alone. They are what the crystal
data add, which is the answer to the question a referee would ask: why not compute the
properties from the CSD coordinates without refining?

## 2. Why it matters beyond lattice energies

Atom-resolved in-crystal multipoles and anisotropic polarisabilities are the parameters of
polarisable force fields, classical (AMOEBA-like) and machine-learned. These are usually fitted to
gas-phase calculations. Experimentally anchored in-crystal values for many crystals would be
training and validation data for them. That, with the derived observables Tonto already makes
(refractive indices, charges, multipole moments, bonding indices such as RGBI), is the case for a
broad journal: the deliverable is experimentally validated electronic properties at scale.

## 3. Decisions and discussion (Dylan, 2026-11-24)

1. **Narrow the scope to energetics** first, with X23 as the yardstick, rather than a wide survey
   of properties. Better geometries (the TFVC paper) and refractive indices (Jayatilaka et al.,
   PCCP 2009) have each been checked on only a few crystals.
2. **The work must show that the crystal data add something.** If the X-ray data add little
   beyond the geometry, the result is a validated computational database, useful but a much
   weaker case. Test that early, on a few crystals, before scaling up.
3. **Every number must be right.** At scale that means an automated pipeline with the checks of
   the TFVC paper built in: numerical error bounded up front (grid levels, form-factor accuracy),
   the rigid-atom error and the rigid-bond test, and a full record of each calculation in the
   CIF2 file (basis set, method, grid, density matrix).
4. **A mass re-refinement study alone would read as crystallography** for JACS; the chemical
   payoff (energies, polarisabilities, force-field parameters) is what would carry a broad
   journal.

## 4. Risks

- **The experimental lattice energies** carry uncertainties of a few kJ/mol; an improvement must
  be larger than that.
- **$`E_{\rm pol}`$ is a small term**, and the fitted factors $`k`$ for dispersion and repulsion
  may absorb much of any change. Refit the factors with the new terms, and compare like with like.
- **Data coverage.** Most X23 structures are old; reflection data may not be deposited for many.
  Where there are none, the geometry can come from neutron or CSD structures, but item 4 is then
  unavailable.
- **The ADP thermal correction** rests on the normal-mode model of NoMoRe: the claim is "less
  model-dependent", not "model-free".

## 5. First steps, when taken up

1. **Inventory:** for each X23 crystal, whether reflection data (with $`F^2`$ and $`\sigma`$) and a
   neutron structure are deposited, and at what temperature.
2. **Atomic polarisabilities in Tonto:** finite-field response of the in-crystal density,
   partitioned into TFVC atoms, with the charge-flow part; checked against the molecular
   polarisability (the atomic ones must sum to it).
3. **Pilot:** for the crystals with data, the CE lattice energy three ways (as published; with
   in-crystal multipoles and polarisabilities; with the ADP thermal correction), each against the
   experimental sublimation enthalpy.
4. **Decide** on scaling up from the pilot: only if the error falls clearly beyond the
   experimental uncertainty.

## 6. Log

- 2026-11-24: idea discussed and recorded; shelved until the TFVC paper is done.
