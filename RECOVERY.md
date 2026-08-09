# RECOVERY — work stranded on achari2 / the Mac (2026-08-09)

Written on `sauce` while Dylan was travelling. Everything needed to finish the
recovery is here, so nothing depends on remembering a conversation.

## The situation, in facts

- **Last push to GitHub, anywhere: `02c842f6`, 2026-08-05 11:28:07 UTC**, to
  `refs/heads/antlr4`. Confirmed from GitHub's own event log, not just a local
  clone. Nothing has been pushed since, on any branch.
- **Four days of work exist only on `achari2` and a Mac desktop**, both at the
  Max Planck Institute for Multidisciplinary Sciences, Am Faßberg 11, Göttingen.
  It includes `docs/WORKSHOP.md` (~5 hours), other WORKSHOP documents, and a
  body of fixes to the plotting code.
- **achari2 is `10.208.31.14`** — an RFC1918 address on the institute's internal
  network. From `sauce` it is unreachable: `ping` 100% loss, TCP/22 connection
  refused. `sauce` sits on `172.18.0.0/16`, a different private network.
- **`sauce` has never connected to achari2.** No key in `~/.ssh` (only
  `known_hosts`, whose two hashed entries match neither achari2 nor the GWDG
  login/transfer hosts), and no `ssh`/`scp`/`achari`/`gwdg` command anywhere in
  the shell history. The route was always Mac → achari2.
- **The VPN path is blocked until GWDG staff are available.** eduVPN requires an
  enrolled second factor; the eduMFA app holds no token, and resetting the factor
  requires a backup code Dylan does not have. Note also that GWDG's VPN lands you
  in GÖNET (campus), which may not route to the institute's internal subnet at
  all — untested.
- **geteduroam is not a VPN** and holds no TOTP secret. It provisions eduroam
  *WiFi* only.
- **Pushing from Göttingen demonstrably works**: the 5 August commits are stamped
  `+0200` and pushed fine. The network is not the obstacle — physical/human
  availability is.

## The one thing that recovers it

Someone with access to achari2 runs this. Two minutes, no Tonto knowledge needed.

```bash
ssh dylan@10.208.31.14

# 1. find the checkout with unsaved work
find ~ -maxdepth 6 -name .git -type d 2>/dev/null | while read g; do
  r=${g%/.git}
  d=$(git -C "$r" status --porcelain | head -3)
  u=$(git -C "$r" log --branches --not --remotes --oneline | head -3)
  [ -n "$d$u" ] && { echo "=== $r"; echo "$d"; echo "$u"; }
done

# 2. confirm the documents are there
find ~ -iname 'WORKSHOP*' 2>/dev/null

# 3. in whichever directory step 1 printed:
cd <that directory>
git add -A
git commit -m "WIP: workshop documents and plot fixes, recovered from achari2"
git push origin --all
```

`--all` matters: it carries every branch, including commits already made on
achari2 but never pushed. `.gitignore` already excludes `build*/`, `release/`,
`debug/`, `mpi/`, `*.o`, `*.so`, so `add -A` will not sweep in build trees.

**Alternative that needs almost nothing from the helper:** have them run
`claude --remote-control` on achari2 (in Dylan's account, which is already
authenticated), then drive that session yourself from claude.ai/code.

**If the files cannot be found**, the Claude Code transcripts on that machine
record the full text of every file written — `~/.claude/projects/*/*.jsonl`.
Even a deleted or moved `WORKSHOP.md` is recoverable from them. A read-only
collection script was written to `/tmp/achari2_recover.sh` on `sauce`; if it is
gone, the essentials are: `find / -iname 'WORKSHOP*'`, list dirty git repos as
above, and `tar czf /tmp/t.tgz ~/.claude/projects`.

**Deliberately NOT done:** nothing has been pushed to `antlr4` from `sauce`. If
it had been, achari2's `git push origin --all` would be rejected as a
non-fast-forward and the helper would hit a merge they cannot resolve. Keep
`antlr4` untouched until achari2's work is safely up.

## What is on this machine (`sauce`)

- The repo exactly as of `02c842f6` (5 Aug). No local modifications to any
  tracked file — verified, `git status` clean apart from the items below.
- A working `release/tonto`, built 2026-08-09 00:47.
- **The gnuplot plotting path works.** `tests/long/h2o_rhf_cc-pVDZ_electrostatic_potential_plot`
  run against that binary produced `h2o.electric_potential_grid,gnuplot`
  (139 KB), exit 0. So whatever was failing is narrower than "plots don't work".
- `plot_grid` accepts: `gnuplot`, `gnuplot.contour`, `gnuplot.log_contour`,
  `gnuplot.pos_log_contour`, `contour`, `cube`, `gaussian.cube`, `cell.cube`,
  `drishti`, `mathematica`, `spackman`, `vapor`, `vtk`, `xcrysden`, `xdgraph`.
- Plot-producing test jobs available: `h2o_rhf_cc-pVDZ_electrostatic_potential_plot`,
  `urea_x-ray-constrained-uhf_STO-3G_plus_ELF_plot`,
  `L_alanine_minmax_residual_density_map`,
  `YLID_IAM_plus_anomalous_residual_density`,
  `nh3_x-ray-constrained-rhf-cluster-charge_cc-pVTZ*`, `tests/hart/*`.

## What is now in place so this cannot recur

Two `Stop` hooks, wired in the committed `.claude/settings.json` so they run on
every machine that has the repo:

- **`.claude/hooks/wip_snapshot.sh`** — after every turn, snapshots the working
  tree to `wip/<hostname>` on GitHub. Non-invasive: it builds a commit object
  through a scratch index and force-pushes it, never touching your working tree,
  index, HEAD, or any branch you work on. Verified: HEAD, branch and
  `git status` were byte-identical before and after. Recover from anywhere with
  `git fetch origin && git checkout wip/<hostname>`.
- **`.claude/hooks/check_unsynced.sh`** — reports uncommitted, untracked-markdown
  and unpushed work at the end of every turn, and warns loudly if the snapshot
  push fails.

**Set these up on achari2 and the Mac too**, once their work is recovered — they
arrive automatically with the repo, but the settings watcher needs a session
restart to pick them up the first time.

## Still unanswered

- What `docs/WORKSHOP.md` actually contained, and what "the plots were not being
  generated" looked like in practice. Only Dylan knows; without it a rebuild is
  guesswork.
- Whether achari2 accepts password authentication or is key-only. If key-only,
  and the key lives on the Mac, VPN access alone would not be enough.

## The plot problem — the lead, 2026-08-09

Dylan: *"The plots were concerning those done after a HAR. It used a lot of
context. Once that was done, we needed to make a lab."* So the lost work was
(a) fixing the post-HAR plots and (b) building a **lab/workshop exercise** on
top of them.

The post-HAR plot files are written by exactly six `stdout.redirect` sites:

```
foofiles/diffraction_data.put.foo:1363   stdout.F_z_vs_stl
foofiles/diffraction_data.put.foo:1380   stdout.Delta_F_vs_stl
foofiles/diffraction_data.put.foo:1435   stdout.Delta_F_pred_z_vs_F_pred
foofiles/diffraction_data.put.foo:1454   stdout.Delta_F_pred_z_vs_stl
foofiles/vec{reflection}.foo:3206        stdout.QQ_plot_with_hkl
foofiles/vec{reflection}.foo:3236        stdout.QQ_plot.gunplot
```

`DEFERRED.md` already records these as a known defect: the names are hard-coded
and ignore the job name, so two runs in one directory overwrite each other's
plots; and `.gunplot` is a typo for `.gnuplot` (the file's own header says
"Gnuplot input file").

**Next step when resuming — measure, do not infer** (CLAUDE.md §2: in this
codebase inspection does not work and measurement does):

1. Run a HAR job and list what appears:
   ```bash
   mkdir -p /tmp/harplot && cd /tmp/harplot
   cp <repo>/tests/long/urea_rhf_STO-3G_HAR/{stdin,urea_init.cif} .
   TONTO_BASIS_SET_DIRECTORY=<repo>/basis_sets <repo>/release/tonto
   ls -la          # which of the six appear? which do not?
   ```
   NOTE: the job needs `urea_init.cif` copied alongside `stdin`, or it dies with
   "TEXTFILE:open_for_read ... not an existing file" at `process_CIF`.
2. If files are missing, find whether the routines are called at all, or are
   gated behind a keyword that is off by default. That would explain "the plots
   were not being generated" with no run needed.
3. Only then look at content/format correctness.

Unverified as of this writing: the first run failed for the missing-CIF reason
above and was not repeated.
