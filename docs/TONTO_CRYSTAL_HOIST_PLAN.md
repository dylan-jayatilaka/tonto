# Hoisting CRYSTAL out of MOLECULE — plan

**Working document** (CLAUDE.md §1): delete it when the item closes, moving whatever is durable
into the user-facing pages. Live status is the entry in `TASKS_AND_HISTORY.md`, *Root cause behind several
of these: `MOLECULE` contains `MOLECULE`s*.

**Status 2026-09-24: sketch only, to be worked out in detail before any code moves.** Dylan's, for
**December 2026 or early 2027**. A large refactor with many chances to break the code, which is the
reason for planning it on paper first.


## 1. The defect being removed

Today a `MOLECULE` holds a `CRYSTAL` **and** holds `.mol(g)`, a set of `MOLECULE`s. A type
containing instances of itself forces `MOLECULE.SCF:fragment_SCF` to call back into
`MOLECULE.SCF:scf`, which is the 12-node call cycle:

```
fragment_scf_para -> usual_scf -> initialize_scf -> get_initial_guess -> ...
   -> do_atom_group_scf -> scf -> fragment_scf -> fragment_scf_para
```

**Put the fragments where the physics puts them**: a `CRYSTAL` containing several `MOLECULE`s, with
`fragment_SCF` a `CRYSTAL` method. Then `CRYSTAL:fragment_SCF` calls `MOLECULE:scf`, which calls
nothing back. The cycle disappears, and with it the recursion defect in the parallel-do lock, the
`subfrag_SCF` clone, and the only *live* justification for depth counting. The parallel decision
also moves to the right place: distributing work over fragments is a property of the container, and
it is currently made inside a `MOLECULE` method that is simultaneously "do an SCF on me" and "do
SCFs on my children" -- which is why `fragment_SCF_para` carries `per_rank_IO_allowed` toggling,
per-fragment archives and a work scheduler.

Full reasoning in `TASKS_AND_HISTORY.md`; MPI consequences in `docs/TONTO_AND_MPI.md`.


## 2. Requirements found 2026-09-24, while fixing the Becke grid bug

These came out of a real defect and are the concrete part of this plan. See `TASKS_AND_HISTORY.md`,
*Fragments did not inherit the Becke grid*.

### 2a. One fragment-initialisation point

**Three** routines build fragments today, and each repeats the same tail of parent-to-fragment
copying -- name, basis, charge, atom info, multiplicity, `set_SCF_guess_defaults_from`, crystal
copy, `resolve_ANOs_from`:

| routine | file | scheme |
|---|---|---|
| `MOLECULE.SET:set_molecule_from_atom_group` | `molecule.set.foo:1003` | explicit `atom_indices`, and the connection table |
| `MOLECULE.BASE:set_Ryde_cap_for_group` | `molecule.base.foo` | Ulf Ryde capped residues |
| the block in `MOLECULE.BASE:set_NN_capped_groups` | `molecule.base.foo` | nearest-neighbour capped fragments |

**The Becke grid was missing from all three**, which is exactly why it went unnoticed for so long:
there were three places to add one line and it was added to none. Whatever owns the fragments after
the hoist must initialise them in **one** place. This requirement survives the hoist even if none
of the current code does.

### 2b. The fragmentation scheme should be a named value, not three flags

`MOLECULE.BASE:update_atom_groups` (`molecule.base.foo:1527`) dispatches on
`.crystal.use_Ryde_capping`, `.crystal.use_NN` and `.atom_group.has_atom_indices` -- three flags
across two objects. So the scheme exists **nowhere as a value**:

- nothing can echo it, validate it, or put it in the CIF;
- the precedence is whatever the `if` chain happens to be;
- `use_Ryde_capping` together with `use_NN` silently gives Ryde.

Dylan wants a `select case`, which is right, but it needs something to switch on: a
`fragmentation_scheme` -- "connected", "atom_indices", "Ryde", "NN" -- resolved **once** from the
user's keywords, with the precedence written down and a `DIE` on a conflicting pair. After the hoist
it belongs on the `CRYSTAL`, which is what holds the fragments.

Note the general lesson from the grid bug: an option that is not echoed is an option that can stop
working without anyone noticing. Whatever the scheme becomes, it should be reported.

### 2c. Three divergences between the paths are decisions, not style

The hoist should settle each deliberately rather than inherit whichever branch is ported first:

| | capping paths (Ryde, NN) | `atom_indices` path |
|---|---|---|
| crystal copy | `set_minimal_copy` (`molecule.base.foo:1910`, `:1979`) | full copy; `update_group_crystal_and_ANOs` even carries the comment *"Should be minimal copy?"* |
| basis | `set_basis_name(.basis_name)` | `resolve_bases_and_update_from(self)` |
| charge and multiplicity | setters, guarded by `spin_multiplicity_set` | direct assignment |


## 3. Sequencing, and what protects the move

The risk is silent numerical change, not compile failure, so the order matters.

1. **Land the grid fix and its reblessing first** (done 2026-09-24), so the fragment paths are
   already correct before they move. A refactor on top of a latent bug cannot be validated.
2. **Get the fragment jobs green on Linux and keep them there.** The jobs that exercise fragments
   are `tests/long/gly_ala_fragHAR_rhf_STO-3G`, `tests/hart/gly_ala_hart_STO-3G`,
   `tests/short/quartz_NN_fragments_L0/L1/L1_no_capping/L2`,
   `tests/long/quartz_NN_HAR_L0/L1_rhf_def2-SVP`, the `tests/samuel` breakdown jobs and the dimer
   decomposition jobs. Between them they cover all three schemes. **Every one must be bit-stable
   across the move** -- this is a re-siting of code, not a change of method, so anything that moves
   is a bug.
3. **Do 2a first, inside the present structure**, if it can be done without disturbing behaviour:
   one fragment-initialisation point is testable on its own and shrinks the surface of the hoist
   from three routines to one.
4. **Then move `fragment_SCF` to `CRYSTAL`**, and only then remove the depth counting and the
   `subfrag_SCF` clone, each as a separate commit with the suite run between.
5. **MPI last**, and re-read `docs/TONTO_AND_MPI.md` first: `fragment_SCF_para` has known open
   defects (the RMA work queue indexing past `p_loop_list`, and the master reading its own window
   buffer outside an access epoch) which should be fixed *before* the code moves, not during, so
   that a failure afterwards is attributable.

**Two traps specific to this refactor**, both already paid for once:

- **A collective must not execute on a rank-local path.** `MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk`
  carries a long comment on exactly this: it has both collective and rank-local callers, so a
  barrier inside it deadlocks. Moving `fragment_SCF` changes which callers are which.
- **`PURE` is a macro.** A routine that gains an `ENSURE`, `DIE` or `WARN` during the move must be
  `PURE`, never lower-case `pure`, or it compiles in release and fails only in debug or MPI --
  CLAUDE.md §5.
