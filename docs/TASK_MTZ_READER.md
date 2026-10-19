# TASK: an MTZ reader, a PDB reader, and the 6MU9 maps in Tonto

Working document for branch `mtz-reader` (from `develop` at `0f145210`, 2026-10-19).

## Goal

Read a REFMAC MTZ file and its PDB model into Tonto, so that the cell maps of
`CELL_MAP` (Riesz and local-moment maps, with their noise levels) can be made for a
protein, and so that Santosh Panjikar's 6MU9 results in the IUCrJ paper can be
reproduced and checked.

## The data (Santosh, 2026-10-19)

`~/Dropbox/tonto_data/6mu9_santosh/ib5132_for_Dylan_2026-10-19/04_Fig4_6MU9_model_and_data/`

- `6mu9_final.mtz`: REFMAC output, 7.4 MB, little-endian (machine stamp `44410000`),
  space group P1, cell 35.307 42.094 48.542 Å, 97.73 105.52 113.34°, 108 445 reflections,
  37.19–0.97 Å, 17 columns:
  `H K L FREE FP SIGFP FC PHIC FC_ALL PHIC_ALL FWT PHWT DELFWT PHDELWT FOM FC_ALL_LS PHIC_ALL_LS`.
  Missing values are NaN (`VALM NAN`). Phases are in degrees. No wavelength (`DWAVEL 0`).
- `6mu9_final.pdb`: 2622 atoms (C 1393, O 850, N 376, S 3), every one with an `ANISOU`
  record; 417 waters and 5 sulfates as `HETATM`; 307 atoms in alternate conformers
  (Lys299 among them); no hydrogen atoms.
- What the paper's Fig. 4 used: 2mFo−DFc = `FWT`/`PHWT`; mFo−DFc = `DELFWT`/`PHDELWT`;
  the model-only field = `FC_ALL`/`PHIC_ALL`.

What an MTZ holds, and what it does not: the cell, the space group with its operators,
and reflection columns; no atoms. `SIGFP` gives the sigma of every measured amplitude,
so the noise level of a map is available. The figure of merit `m` is the `FOM` column.
REFMAC's `D` is fitted per resolution shell and is not stored, but since `FWT` and
`FC_ALL` share a phase it follows per reflection from the stored columns:
`D = (2m·FP − FWT)/FC_ALL` for an acentric reflection and `(m·FP − FWT)/FC_ALL` for a
centric one.

## The MTZ format

Documented by CCP4; small enough to read without a library. A 4-byte tag `MTZ `; a
4-byte integer, the word number at which the header starts; a 4-byte machine stamp,
whose first byte gives the byte order of the floats (`0x44` little-endian IEEE,
`0x11` big-endian); then the data, `NCOL` 32-bit floats per reflection, from word 21;
then the header as 80-character lines: `VERS`, `TITLE`, `NCOL n_col n_refl n_batch`,
`CELL`, `SORT`, `SYMINF n_sym n_prim lattice number 'symbol' pointgroup`, `SYMM`
(one per operator), `RESO`, `VALM`, `COLUMN label type min max dataset`, dataset lines,
`END`. Fortran stream access reads it; `archive.foo` already uses stream I/O.

## Stages

1. **`MTZ` module** (`foofiles/mtz.foo`, type in `types.foo`): `read(file)` fills the
   title, cell, space-group number and symbol, the operators as strings, the column
   labels and types, and the data as `MAT{REAL}(n_refl,n_col)`, converting from 32-bit
   floats and swapping bytes if the stamp says so. `column_index(label)` looks a column
   up. `put` lists the header, which is the first test.
2. **`CRYSTAL:read_MTZ(mtz)`**: the unit cell from `CELL`, the space group by
   `SPACEGROUP:set_HM_symbol` from `SYMINF`, checked against the `SYMM` operators.
3. **`VEC{REFLECTION}:read_MTZ(mtz,columns)`**, beside `read_mmCIF`: `h k l`,
   `F_exp`, `F_sigma` and the complex `F_calc` from four named columns, defaulting
   to `FP SIGFP FC_ALL PHIC_ALL`; reflections whose `FP` is NaN are skipped. Keywords in
   the `xray_data=` block: `mtz_file=` and `mtz_columns=`.
4. **`VEC{ATOM}:read_PDB(file)`**, beside `read_mmCIF`: Tonto writes PDB files but does
   not read them. Fixed columns of `ATOM`/`HETATM`: name, residue, chain, alternate
   location, x y z (Å → bohr), occupancy (`site_occupancy`), B, element; `ANISOU`
   gives U in 10⁻⁴ Å², Cartesian. Alternate conformers are kept, each with its
   occupancy. Keyword `PDB= { file_name= }` in the molecule block, like `CIF=`.
5. **`CELL_MAP` from MTZ columns**: a kind `mtz` with `mtz_columns= FWT PHWT` (or
   `DELFWT PHDELWT`, `FC_ALL PHIC_ALL`) that takes the map coefficients as given,
   expanded to the full sphere with Friedel mates, with `SIGFP` as the sigma for the
   noise level and no `F(000)` term. The existing kinds (`f_exp`, `f_calc`,
   `residual`) then work from stages 2–3 as for a small molecule.
6. **Reproduce the paper's 6MU9 numbers** with the Riesz ℓ = 1 maps
   (`moment_kind= riesz`, `l_value= 1`, Cartesian frame): the field averaged over
   spheres of radius 0.3 and 0.6 Å at the 263 backbone carbonyl O atoms, from
   `FWT/PHWT` and from `FC_ALL/PHIC_ALL`; the data–model angle (median about 10°);
   O299 ranked second of 263. The sphere averages are done by a short script in
   `scripts/` from the cube files, or by a `cell_map` option if that proves cleaner.
   Then the noise level of the 6MU9 maps, which the paper does not have.
7. **Tests**: a small MTZ written by a Python script (a few reflections, both byte
   orders) for the reader, as a short test; the 6MU9 run as a long test only if its
   input can be kept small (the MTZ is 7 MB, too big for `tests/`). Bless on achari2.

## Effort

Stage 1 about 3 h; 2–3 about 2 h; 4 about 2 h; 5 about 2 h; 6 about 3 h; 7 about 2 h:
roughly two days of work. Whole-cell maps of 6MU9 need the FFT build (`-DFFTW=ON`):
216 890 coefficients on a 0.97 Å cell is a 15-minute direct sum per component.

## Open points

- Santosh's README calls the manuscript "revised for Acta Crystallographica Section A
  (editor S. Billinge; revision due 2 November 2026)", while the merged paper is in the
  IUCrJ template. Dylan to settle which journal.
- Tonto's mmCIF atom reader has no ADPs (`read_mmCIF_ADP2` is stubbed out). Not needed
  for this model, which is a PDB file; worth finishing later.
- Hydrogen atoms: the model has none; the IAM reference of the paper used it as is.

## Log

- 2026-10-19: branch made; data downloaded and inspected; plan written.
- 2026-10-20: stages 1–4 done. `mtz= { file_name= ... put }`, `process_mtz` and
  `pdb_file=` read 6MU9 in 17 s. Three things found on the way:
  - `STR:remove_blanks` loops for ever on a blank-padded string (it replaces " " by ""
    and the padding is blanks); the MTZ reader uses its own helper.
  - The translator drops a kind on a declaration: `rval4 :: REAL(REAL_4_KIND)` comes
    out as `REAL`. The CPP macros `REAL_4` and `INT_4` pass through and work. This means
    `archive.foo`'s "single precision" stream writers have been writing doubles; not fixed
    here.
  - `CRYSTAL:make_list_of_equivalents_v2` compared every reflection with every other and
    took 8 minutes on 108 445 reflections; it now sorts a symmetry-reduced key (heap sort:
    the library quick sort recurses n deep on sorted input and overflowed the stack).
    `update_atom_basis_group_info` after a PDB read built a molecule with fragment info
    for every connected group, waters included, 7 minutes; the PDB path skips the groups.
- 2026-10-20, later: stages 5 and 6 done; the paper's 6MU9 descriptors reproduced.
  `cell_map= { kind= mtz  mtz_columns= FWT PHWT }` gives r.m.s. 0.5364 e/Å³ and noise
  0.0093 for the 2mFo−DFc map, 0.5203 for the model map, each equal to
  $`\sqrt{2\sum F^2}/V`$ from an independent parse of the file. `put_cell_map_sphere_averages`
  averages the dipolar Riesz field in spheres about the 263 backbone carbonyl O atoms
  (waters and sulfates left out of the comparison by residue name in `compare.py`):

  | | this work | paper |
  |---|---|---|
  | r₀ = 0.6 Å: data–model angle, median (90th percentile) | 10.3° (20.4°) | 9.7° (19.3°) |
  | r₀ = 0.6 Å: angle of ⟨d⟩ to C→O, median | 158° | 160° |
  | r₀ = 0.6 Å: O299 data–model angle, rank of 263 | 33.5°, 6th | 30.8°, 2nd |
  | r₀ = 0.3 Å: data–model angle, median (90th) | 14.5° (34.8°) | 12.4° (28.3°) |
  | r₀ = 0.3 Å: O299 | 27.5°, 40th | 39.5°, 2nd |

  At 0.6 Å the agreement is close; the six largest angles are Lys75, Asn123, Val302, Pro184,
  Ala298 and Lys299, which are the residues the paper discusses (O298 and O302 are its
  neighbours of O299). At 0.3 Å the two differ more: the paper's maps are on a grid of
  3 points per $`d_{\min}`$, 0.32 Å, so a 0.3 Å sphere holds one to four of its grid
  points and the result depends on where the atom falls on the grid; Tonto's values are
  interpolated from a finer FFT grid at 895 points per sphere of 0.1 Å spacing. The
  inputs, outputs and the comparison script are in
  `~/Dropbox/tonto_data/6mu9_santosh/tonto_check/`. The run takes 38 s.
- Found on the way: a call from MOLECULE.RHO to `set_cell_map_promolecule` (MOLECULE.MAIN)
  closed a module-use cycle MAIN→FOCK→RHO→MAIN, and CMake then never compiled
  `molecule.fock` ("Circular ... dependency dropped", "Cannot open module file"), even
  from `make clean`. The call was not needed. Stale `*.provides.build` markers in the
  build tree can give the same symptom after an interrupted build; delete them.

