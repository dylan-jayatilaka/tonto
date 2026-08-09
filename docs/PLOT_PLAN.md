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

## Stage 1 — reactivate SYSTEM_COMMAND  ✅ DONE (2026-08-09)

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

## Stage 2 — the post-HAR plots  ✅ DONE (2026-08-09)

**Five** plots are written at the end of a HAR by `DIFFRACTION_DATA.PUT:
put_F_calc_plots` (not four — `RECOVERY.md` had it wrong), plus the QQ plot from
`VEC{REFLECTION}:put_labelled_F_qq_plot`. The last two need `refine_extinction`,
so a job without it writes four:

```
stdout.F_z_vs_stl                stdout.Delta_F_pred_z_vs_F_pred   (extinction only)
stdout.Delta_F_vs_stl            stdout.Delta_F_pred_z_vs_stl      (extinction only)
stdout.F_z_vs_F_exp
stdout.QQ_plot                   (+ stdout.QQ_plot_with_hkl, the tabular form)
```

Each now writes three files: the data, a `.gnuplot` script, and — because
`tonto.call_gnuplot(script,image)` runs gnuplot on the spot — the `.png` itself.

All six brief points are done:

1. ✅ A gnuplot script for every plot, not just the QQ one, from the one shared
   emitter `VEC{REFLECTION}:put_gnuplot_script`.
2. ✅ **1:1 aspect ratio** (`set size square`), dotted grid — Fig. S8, p. 10 §S9
   of the Davidson 2022 Acta Cryst B supplement.
3. ✅ **Line of best fit on the QQ plot**, gnuplot's own `fit`, with the equation
   `sprintf`'d and placed `at graph 0.5, graph 0.045 center` — i.e. centred at
   the bottom, and correct because gnuplot substitutes the fitted `a`, `b` at
   plot time rather than Tonto hard-coding a number into the label.
4. ✅ **Six worst outliers labelled `(h k l)`** on every plot. `h`,`k`,`l` are
   written as columns 3-5 of each data file and selected in gnuplot with
   `abs($2)>=thr ? sprintf(...) : ''`. The threshold comes from
   `VEC{REFLECTION}:outlier_threshold`, which sorts `abs(y)` and returns the
   **midpoint** between the 6th- and 7th-largest — taking the 6th-largest itself
   gave five labels, because printing `thr` at `e14.6` rounded it a hair above
   the very point that defined it.
5. ✅ **Generated automatically**, master rank only.
6. ✅ Tested on **nh3** (`tests/short/nh3_rhf_DZP_HAR`) and shown.

The `.gunplot` → `.gnuplot` typo was fixed on the way, in the source and in the
three test `IO` manifests that listed it.

### Two things worth remembering from the doing

- **`stdout.text` silently corrupts memory past 256 characters in a release
  build.** `BUFFER:put_str` copies `len(string)` — the *declared* length — into
  `BUFFER.string`, which is `STR(len=BSTR_SIZE)` = 256. (`BSTR` is not "bigger";
  `STR_SIZE` and `BSTR_SIZE` are both 256.) The guard is an `ENSURE`, so it
  compiles away in release and you get a SEGV in `__memmove_avx_unaligned_erms`
  instead of a message. Declaring `cmd :: STR` and passing `trim(cmd)` fixes it.
  A `DIE_IF` there would close the whole class — see the open item below.
- **SYSTEM may depend on TYPES alone.** TEXTFILE uses SYSTEM, so a single
  `stdout.text` inside a SYSTEM_COMMAND routine makes the build circular. That is
  why `put_command_info` and `call_gnuplot` write raw to `.stdout_unit`, exactly
  as `SYSTEM:die` does.

### Open follow-up

Add a `DIE_IF` to `TEXTFILE:text` / `BUFFER:put_str` so an over-long line reports
itself instead of dumping core. Live in release, unlike the `ENSURE` that is
there now.

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
