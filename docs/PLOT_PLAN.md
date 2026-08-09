# Post-HAR plots and the workshop — the plan

Written 2026-08-09, regenerating work lost with the achari2/Mac material (see
`RECOVERY.md`). This file is the durable spec: it is snapshotted to GitHub after
every turn, so the plan survives a lost context or a lost machine.

Dylan's brief, in his words, in order:

> The plots are those that occur at the end of a HAR. The QQ plot is made, but
> I'd like gnuplot plots for the others. In addition I'd like those plots to be
> automatically generated. This will require you reactivating the SYSTEM_COMMAND
> module. It can be incorporated into the SYSTEM type in the same way as
> PARALLEL — as a virtual module. To test SYSTEM_COMMANDS you can write a small
> `run_system_commands` exerciser.

> For a plot example of the other plots, see
> `/home/dylan/Dropbox/manuscripts/Davidson_2022_Acta_Cryst_B_78_p397-415_supp.pdf`
> p. 10 section S9. Those plots should have 1:1 aspect ratio. In addition a line
> of best fit should be added to the QQ plot. Its equation should be centred at
> the bottom. The six worst outliers (above a certain threshold) should be
> labelled with the (h k l) of the reflection. Also on the other plots. Test the
> results on nh3. Show me the plots.

> In the final stage we need to make the WORKSHOP docs.

## Stage 1 — reactivate SYSTEM_COMMAND  (IN PROGRESS)

`SYSTEM_COMMAND` was a dead module: a real `module` with its own type, referenced
by nothing that is built, and it could not have compiled as written
(`execute_command` declared `self :: IN` while the intrinsic writes three of its
components).

Reactivated as a **virtual module**, exactly as `PARALLEL` is:

- `foofiles/system_command.foo` — `virtual module SYSTEM_COMMAND`. No type, no
  compiled code; the bodies are textually inherited.
- `foofiles/types.foo` — `type SYSTEM_COMMAND` deleted; its five components moved
  into `type SYSTEM`, prefixed `command_` (bare `status`/`message`/`wait` are far
  too generic on the global `tonto` object, which already has `.error_status`).
- `foofiles/system.foo` — an "inherited from SYSTEM_COMMAND" section of
  `get_from(SYSTEM_COMMAND)` headers, mirroring the PARALLEL section;
  `set_defaults` now calls `.set_command_defaults`.
- `runfiles/run_system_commands.foo` — self-checking exerciser, 8 tests,
  registered as a ctest. Replaces the stale `run_system_command.foo`.

API on the global object:

```
tonto.set_command("gnuplot urea.gnuplot")   tonto.set_command_asynchronous
tonto.execute_command                       tonto.set_command_synchronous
tonto.execute("...")                        tonto.command_failed
tonto.put_command_info                      tonto.command_exit_status
```

**Parallel caveat, deliberate:** `execute_command` is *not* collective — it runs
on whichever rank reaches it. Callers must guard with `.is_master_processor` or
every rank spawns its own copy of gnuplot onto the same output file. Documented
in the module header and the SYSTEM section.

## Stage 2 — the post-HAR plots

Six plot files are written at the end of a HAR, by these `stdout.redirect` sites:

```
foofiles/diffraction_data.put.foo:1363   stdout.F_z_vs_stl
foofiles/diffraction_data.put.foo:1380   stdout.Delta_F_vs_stl
foofiles/diffraction_data.put.foo:1435   stdout.Delta_F_pred_z_vs_F_pred
foofiles/diffraction_data.put.foo:1454   stdout.Delta_F_pred_z_vs_stl
foofiles/vec{reflection}.foo:3206        stdout.QQ_plot_with_hkl
foofiles/vec{reflection}.foo:3236        stdout.QQ_plot.gunplot
```

`DEFERRED.md` records two known defects here: the names are hard-coded and ignore
the job name, so two runs in one directory overwrite each other's plots; and
`.gunplot` is a typo for `.gnuplot`.

To do:

1. Only the QQ plot gets a gnuplot script today. Write one for the other four.
2. **1:1 aspect ratio** on all of them (`set size square`) — the reference is
   Fig. S8, p. 10 §S9 of the Davidson 2022 Acta Cryst B supplement: square
   panels, dotted grid, data curve plus a straight reference line.
3. **Line of best fit on the QQ plot**, with its **equation centred at the
   bottom** of the plot.
4. **Label the six worst outliers** (above a threshold) with the reflection's
   `(h k l)` — on the QQ plot *and* on the other plots.
5. **Generate the plots automatically** by invoking gnuplot through
   SYSTEM_COMMAND at the end of the job (master rank only).
6. Test on **nh3**; show Dylan the resulting plots.

## Stage 3 — the WORKSHOP docs

`docs/WORKSHOP.md` and companions, lost with the rest. Built on the plots above.
Last time this consumed a lot of context; do it in its own conversation.

## Stage 4 — REDO THE RGBI PLOT WORK  ← Dylan asked to be reminded

> "After this we will have to redo the RGBI plot work. Can you remind me?"
> "A new conversation for that I think."   — Dylan, 2026-08-09

**Its own conversation**, after the workshop docs. `rgbi` is the third installed
executable (`runfiles/run_rgbi.foo`, `add_executable(run_rgbi ...)`,
`OUTPUT_NAME rgbi`). This work was also lost with the achari2/Mac material and
has not been started here.

## Known adjacent bug, deliberately NOT fixed here

`foofiles/crystal.foo:9471-9472` — the reserved ("free") data branch reads

```
.xray_data.PUT:put_labelled_qq_plot("free")
.xray_data.PUT:put_F_calc_plots("free")
```

but the branch is guarded on `.xray_r_free_data.allocated` and should plot
**`.xray_r_free_data`**. As written the "free" plots re-plot the *fitting* data
under a different name. Left alone because it is outside the plot brief and
changing it changes numbers in blessed references; flagged here so it is not
mistaken for something these changes introduced.
