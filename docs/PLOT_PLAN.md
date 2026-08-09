# Post-HAR plots and the workshop — the plan

Written 2026-08-09, regenerating work lost with the achari2/Mac material (see
`RECOVERY.md`). This file is the durable spec: it is snapshotted to GitHub after
every turn, so the plan survives a lost context or a lost machine.

## State at 2026-08-09, end of the plots conversation

| Stage | State |
|---|---|
| 1 — reactivate `SYSTEM_COMMAND` | ✅ done, built, exerciser 8/8 in ctest |
| 2 — the five plots + QQ, auto-drawn | ✅ done, built, shown on nh3 |
| 2b — label placement, leaders, job-name files | ⚠️ **written and translating cleanly; full build was still running when this conversation ended** |
| 3 — WORKSHOP docs | not started, own conversation |
| 4 — RGBI | ✅ **items 1-3 done 2026-08-09** (scripts, build, doctor). Item 4 — Tonto drawing them itself — deferred, design recorded below. |

**If you are picking this up cold, the first thing to do is finish the build and
run nh3**, because stage 2b has not yet been through a compiler or a test:

```bash
cd release && make -j3                      # macros.in was touched: full rebuild
mkdir -p /tmp/nh3 && cd /tmp/nh3
cp <repo>/tests/short/nh3_rhf_DZP_HAR/{stdin,data.nh3} .
TONTO_BASIS_SET_DIRECTORY=<repo>/basis_sets <repo>/release/tonto --input stdin
# expect nh3.QQ_plot.png + nh3.{F_z_vs_stl,Delta_F_vs_stl,F_z_vs_F_exp}.png
# note: nh3.*, not stdout.* -- the files are named after the job now
ctest -R 'system_commands|nh3_rhf_DZP_HAR|nh3_IAM_gaussian'
```

Use `-j3`, not `-j$(nproc)`: translation runs one JVM per file and `-j8` OOMs a
14 GB box (`Error 137`).

---

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

## Stage 2b — label placement and the job name  (2026-08-09)

Dylan, on seeing the first plots: *"the tags are a bit crowded … can you maybe
place them directly to the side? … is there an algorithm in gnuplot that would
permit a short line and autoplacement to avoid label collision?"*

**There is not.** Verified rather than assumed: `with labels … offset variable`
does not exist (gnuplot: *"undefined variable: variable"*), and `set jitter`
displaces *points*, not text, and only where they take discrete values. gnuplot
has no ggrepel/adjustText equivalent. What it does have is `with vectors
nohead`, which draws the leader line — but the placement has to be computed by
whoever owns the data. That is Tonto.

Two options were built and compared on the real nh3 output:

- **A — side placement, pure gnuplot.** Two `with labels` passes, left- and
  right-justified, split on which side of the plot centre the point falls.
  Six lines in the emitter, no Foo logic. Fixed four labels of six — and failed
  visibly on `F_z_vs_F_exp`, where `(-3 0 4)` and `(-1 2 -1)` printed *on top of
  each other*. Rejected: "there's not much point having a label if it is
  illegible".
- **B — placement in Foo plus leader lines.** Adopted. `(6 -2 0)` and
  `(-2 6 0)`, which sit at the same `sin(theta)/lambda` with ΔF differing by
  0.018, go to opposite sides with a short grey leader each.

**The algorithm** (`VEC{REFLECTION}:put_hkl_label_file`). Worst point first, so
the worst outlier gets the best spot. For each, try 12 directions at 3 short
radii and score every candidate: heavy penalty for leaving the frame or covering
a label already placed, light penalty for covering a data point, mild preference
for a short leader and for sideways over vertical. Take the cheapest, record its
box, move on. Greedy, deterministic, no iteration — six labels is nothing.

**Consequences worth knowing:**

- Tonto now **fixes the axis ranges** (`nice_axis_range`), because the placer
  reasons in fractions of the plot area and that mapping is only defined once
  the range is. It rounds outward on the same 1-2-2.5-5 tic ladder gnuplot uses,
  so the axes read `-3 … 3` exactly as before, and clamps at zero for a
  non-negative quantity such as F^o.
- The label box is **estimated** from character count and font size; only
  gnuplot knows the true text extent. Fine at six labels, and the leader makes a
  poor guess look wrong rather than be wrong.
- Each plot now writes **four** files: `<stem>`, `<stem>.labels`,
  `<stem>.gnuplot`, `<stem>.png`.
- **PNG only, deliberately.** Asked whether PDFs were produced too, Dylan's
  answer was to leave it: *"they look fine. Leave the commented out stuff."* So
  each script keeps its commented `pdfcairo` stanza — uncomment and rerun for a
  PDF. Worth knowing the mechanism is already there if that changes: the
  terminal is forced on gnuplot's **command line**, not written into the script,
  so `SYSTEM:call_gnuplot` only needs an optional terminal argument to emit both.
  That change touches `SYSTEM`, and every module `use`s it, so it costs a
  near-full rebuild — bundle it with other work rather than doing it alone.

### The fit was never fitting

Found while testing the above. The QQ script seeded gnuplot's fit with `b = 0`.
gnuplot cannot start a parameter at exactly zero — its step is relative to the
value — so it reported `Singular matrix in Invert_RtR`, pinned `b` at 1e-30 and
fitted the slope alone. **The line was silently forced through the origin** and
the label printed a fitted-looking `+ 0.000`. Seeded with `b = 0.1` the true nh3
intercept is **0.035**. The error message never reached anyone either, because
`SYSTEM:call_gnuplot` sends stderr to `/dev/null`. Another silent one.

### Plot files are named after the job

Also 2026-08-09, and milestone **H3** in `docs/RUNNING_HART.md`. They were all
called `stdout.*` regardless of job, so two jobs in one directory overwrote each
other's pictures. `MOLECULE.SCF` now passes `.name` — what `name=` sets in a job
file, and what `hart` sets from `--job` — down through
`CRYSTAL:put_correction_data`. An unnamed job falls back to `stdout`, so nothing
that worked before changed name.

Safe by measurement, not by hope: 16 test-manifest lines mention these files and
**every one is a `delete:`**, never a `compare:` — and `scripts/test.py:314`
records `delete:` as *"recorded but unused"*. 126 of 129 test jobs set `name=`.

### Two open items from the end of the session

**1. The end-of-HAR message is stale, and fixing it means reblessing 24 tests.**
It names `stdout.fit_analysis` (a file nothing writes and never has), calls
several small files "this large file", names not one file that is actually
produced, and ends "Use Excel or gnuplot to view these data" — which predates
Tonto drawing the pictures itself. A corrected version, naming the real files in
dot points, was written and then **reverted**: the text appears in **24**
checked-in references (`grep -rl "This large file includes" tests/`), most of
them `long` jobs. Worth doing as its own change, together with the rebless —
not as a side effect of something else. The reverted text is in the git history
of `foofiles/crystal.foo` around 2026-08-09 if you want it back.

**2. A cosmetic ADP-table difference, which is NOT a failure.** A raw `diff` of
`tests/short/nh3_rhf_DZP_HAR/stdout` against a fresh run shows the `U_xx … U_yz`
error table with different column widths — the reference wider
(`0.00000(5)   0.00000(8)`), the new build narrower. Every number is identical.

**It does not fail the suite**: with the message block reverted the test passes
1/1, so the comparison in `scripts/test.py` is insensitive to this whitespace
and the message was the sole cause of the failure. Recorded only so that whoever
next runs a bare `diff` on this test does not mistake it for a regression, as
was briefly done here. Untraced, and not worth tracing unless it starts
mattering.

### Open follow-up

Add a `DIE_IF` to `TEXTFILE:text` / `BUFFER:put_str` so an over-long line reports
itself instead of dumping core. Live in release, unlike the `ENSURE` that is
there now. This matters more since 2b: the emitted plot command now interpolates
a job-name-derived file name into several lines, so a long `name=` shortens the
distance to 256 characters.

## Stage 3 — the WORKSHOP docs

`docs/WORKSHOP.md` and companions, lost with the rest. Built on the plots above.
Last time this consumed a lot of context; do it in its own conversation.

*(Dylan, 2026-08-09, splitting the work deliberately: "Last time you complained
… so I'm splitting the job into three with hindsight." The three are: the plots
— stages 1, 2 and 2b, done here; the workshop docs; and RGBI.)*

## Stage 4 — REDO THE RGBI PLOT WORK  ← Dylan asked to be reminded

> "After this we will have to redo the RGBI plot work. Can you remind me?"
> "A new conversation for that I think."   — Dylan, 2026-08-09

**Its own conversation**, after the workshop docs. `rgbi` is the third installed
executable (`runfiles/run_rgbi.foo`, `add_executable(run_rgbi ...)`,
`OUTPUT_NAME rgbi`). This work was also lost with the achari2/Mac material and
has not been started here.

**The reference for it is the Grabowsky chapter, Jayatilaka (2025)** — Dylan
provided it for this stage (2026-08-09). **Decided: not checked in.** 1.1 MB of
binary is more than a weak uplink will push in one request (it failed with
HTTP 408 three times from a plane, and was the sole reason), and it is a
published chapter rather than something the build needs. On `sauce` it lives at
`~/rgbi-reference/Jayatilaka_2025_Grabowsky_chapter.pdf`.

### Dylan's brief for RGBI, in his words (2026-08-09)

> What is needed is to clean up the scripts in `rgbi-scripts` and make them
> robust. The arcane software and bugs need sorting out. The build needs to work
> clean on Linux, Mac and Windows — though for Mac it seems a lost cause, that
> will have to wait, it was quite tricky. Ideally the update would check if all
> the software is installed — I think you built a "doctor" to install. And then
> in Tonto, if the components are there, the RGBI molecule plots & dial diagrams
> are built automatically.

So, four things, in his order:

1. **Clean up and harden `rgbi-scripts/`.** Two bash scripts and five LaTeX
   files: `make-rgbi-pic` (153 lines), `make-rgbi-dials` (66),
   `rgbi-mol-structure.tex`, `rgbi-dial-diagrams.tex`, `rgbi-dial-header.tex`,
   plus vendored `mol2chemfig.sty` and `cf-pastebin.tex` (2436 lines).
2. **Make the build clean on Linux, Mac and Windows.** Mac explicitly deferred.
3. **A doctor.** The model is `scripts/wsl_doctor.sh` — a user-facing preflight
   that reports what is missing rather than failing obscurely mid-run. Note the
   WSL one has a companion, `scripts/wsl_selftest.sh`, which asserts every guard
   on an ordinary Linux box and runs in CI; an RGBI doctor deserves the same,
   otherwise it rots.
4. **Draw them automatically from Tonto when the components are present** —
   i.e. exactly what stage 2 did for the HAR plots via `SYSTEM_COMMAND`, so
   `tonto.execute` / `tonto.call_gnuplot` are the precedent to follow. Note the
   pattern established there: *failure to draw must never be fatal*, and the
   inputs must be left behind so the user can run it by hand.

### What was done, 2026-08-09 — items 1, 2 and 3

Full detail in **`docs/RUNNING_RGBI.md`** (developer reference) and
**`docs/INSTALLING_RGBI.md`** (participant-facing). Headlines:

- ✅ **1. Scripts cleaned up and hardened.** They used to **fail and exit 0** —
  every LaTeX run went to `/dev/null`, so a broken stage gave an absent picture,
  or left the *previous* run's picture standing. Now: `set -euo pipefail`, every
  stage's output checked before the next consumes it, LaTeX errors reported,
  `--help` on both, and usage headers that list every option (the old ones
  omitted the two that were mandatory). Four error paths that exited 0 now exit 1.
- ✅ **2. The build is clean on Linux and WSL.** The scripts and doctor install
  to `<prefix>/bin`, the templates to `<prefix>/share/tonto/rgbi-scripts`, so an
  installed Tonto works with no repository present — verified by installing to a
  throwaway prefix and running with `$HOME` faked. They no longer copy from
  `~/bin`. macOS: see item 3.
- ✅ **3. A doctor**, `scripts/rgbi_doctor.sh`, with `scripts/rgbi_selftest.sh`
  as ctest `rgbi_doctor_selftest` (label `short`, in CI, 21 cases). It *executes*
  tools rather than looking for them, which is the whole point. Plus
  `docker/rgbi.Dockerfile` + `ci-rgbi.yml`, which prove the install list from a
  bare `ubuntu:24.04`, and `ci-rgbi-macos.yml`, which probes the Mac list weekly.
- ✅ **4. Tonto draws them automatically**, at the end of `ROBY:bond_analysis`,
  when `make-rgbi-pic` is on the `PATH`. Heavy-atom picture always, `+H` as well
  when the molecule has hydrogens. It was nearly deferred on a wrong premise —
  "it touches `SYSTEM`, so it costs a near-full rebuild" — but stage 1 had
  *already* put `tonto.execute` on `SYSTEM`, so only `roby.foo` changed.
  **The important half of the contract is the silence**: with the script not
  installed, Tonto does nothing and says nothing, so a machine without the
  picture tools behaves exactly as before. Anything else would print a warning
  into the `stdout` that 13 `tests/rgbi` references are compared against, and
  they would all have to be reissued to say "you have no LaTeX". Failure of an
  *installed* script is reported once, never fatal, master rank only, and the
  report is a raw write because TEXTFILE bookkeeping is collective.

**Five findings worth carrying forward**, all measured rather than inferred:

1. **The pipeline has two independent halves.** Dial diagrams need only LaTeX;
   the structure picture needs Open Babel + Indigo + mol2chemfig. So a
   participant defeated by the arcane half still gets half the pictures
   (`--dials-only`).
2. **Neither script needs a wavefunction file.** Open Babel lays out from
   coordinates, and Tonto already writes `geometry.xyz`. Verified on ylid: the
   `.mol` from `geometry.xyz` is identical to the one from `ylid.molden`.
3. **Indigo does not need separate installation** (`mol2chemfigPy3` requires it),
   and **`ghostscript` does** (`pdfcrop` shells out to `gs`) — undocumented for
   years. Linux is now two commands.
4. **The doubled `chemfig` include is real and required.** The vendored 2015 copy
   fails loudly if removed; the modern one fails *silently*, drawing a wrong
   picture at exit 0. That is why deleting it once seemed safe.
5. **The clipped 4th dial column was a `\textwidth` issue** — four dials need
   ~520 pt, `article` gives ~345 pt — fixed in the header with no rebuild.

**The "arcane software"** is the reason this is not a small job.
`make-rgbi-pic`'s own header lists: `tonto`, `openbabel`, `python`, python
**Indigo** (which "might require installing cairo fonts"), **mol2chemfig**, and
a LaTeX with **chemfig**. Several of those are awkward on any platform and two
are effectively unmaintained. Expect the doctor to be the most valuable
deliverable of the four, because it converts "it does not work" into "you are
missing X".

**`rgbi` itself already builds** — `runfiles/run_rgbi.foo`,
`add_executable(run_rgbi …)`, `OUTPUT_NAME rgbi` (CMakeLists.txt:663, 964, 995,
1008). It is the third installed executable alongside `tonto` and `hart`. Also
note `CMakeLists.txt:883`: a file is pinned to `-O2` because "rgbi/BN's Roby
populations were wrong" at other levels — read that before touching flags.

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
