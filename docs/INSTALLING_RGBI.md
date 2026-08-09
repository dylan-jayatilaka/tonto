# Installing the RGBI picture tools

For workshop participants. You need this only to draw the **pictures**; `rgbi`
itself computes the bond indices with no extra software at all.

**Run `scripts/rgbi_doctor.sh` after installing.** It is the arbiter: it reports
what is missing and how to fix it, and it is the only thing in this document
that checks your machine rather than describing someone else's.

---

## The requirement list is shorter than it looks

The headers of the old scripts asked for `tonto`, `openbabel`, `python`, python
**Indigo** ("might require installing cairo fonts"), **mol2chemfig**, `pdflatex`
and `pdfcrop`. Two corrections, both verified on 2026-08-09:

- **Indigo does not need installing separately.** `mol2chemfigPy3` declares
  `Requires-Dist: epam.indigo`, so one install brings both. The cairo-fonts
  advice belongs to a much older Indigo.
- **`ghostscript` is required and was documented nowhere.** `pdfcrop` shells out
  to `gs`; without it every picture fails at the very last step, after all the
  work is done.

And the two pictures do **not** need the same things (see
`docs/RUNNING_RGBI.md` §1):

| You want | You need |
|---|---|
| **Dial diagrams only** | a TeX Live with `chemfig`, plus ghostscript |
| **Dial diagrams + labelled structure** | the above, plus Open Babel and mol2chemfig |

If the awkward half defeats you, you still get dial diagrams. That is worth
knowing before you spend an evening on it.

---

## Linux — tested

Tested from a bare `ubuntu:24.04` by `docker/rgbi.Dockerfile`, which CI builds
on every push. The package list below **is** the list in that Dockerfile; if it
were wrong, CI would be red.

```bash
sudo apt install -y openbabel ghostscript pipx \
     texlive-latex-base texlive-latex-recommended \
     texlive-pictures texlive-extra-utils
pipx install mol2chemfigPy3          # brings Indigo with it

scripts/rgbi_doctor.sh               # <- confirm, do not assume
```

Which package supplies what, so a partial TeX install can be diagnosed:

| File | Package |
|---|---|
| `chemfig.sty`, `tikz.sty` | `texlive-pictures` |
| `xcolor.sty` | `texlive-latex-recommended` |
| `longtable.sty` | `texlive-latex-base` |
| `pdfcrop` | `texlive-extra-utils` |
| `gs` | `ghostscript` |
| `obabel` | `openbabel` |

### If it worked once and then stopped

Almost certainly **your OS upgraded Python and took the virtual environment with
it.** `pipx` builds each app its own venv pinned to the interpreter of the day;
when the distribution drops that interpreter the venv is left pointing at
nothing, and the symptom is obscure:

```
mol2chemfig: /home/you/.local/share/pipx/venvs/mol2chemfigpy3/bin/python:
             bad interpreter: No such file or directory
```

Note that `command -v mol2chemfig` still **succeeds** here — the file is there,
it just cannot run. That is why `rgbi_doctor.sh` executes it rather than looking
for it.

The fix, with a network:

```bash
pipx reinstall-all
```

This happened on `sauce` on 2026-08-09 (Python 3.13 → 3.14) and was the entire
reason "RGBI stopped working".

---

## Windows, via WSL — the traps are the Tonto ones

Read `docs/BUILDING_ON_WINDOWS.md` first: its four traps (a Windows `java.exe`
ahead on `PATH`, a build tree under `/mnt/c`, CRLF sources, `.wslconfig` memory
limits) apply here unchanged, and `scripts/wsl_doctor.sh` checks them.

Only two things are RGBI-specific:

- **Install TeX Live inside WSL** — a Windows MiKTeX will not be found by these
  scripts, and if it were, it would receive Linux paths it cannot open. This is
  the same failure mode as the Windows JDK trap.
- **Keep the job directory off `/mnt/c`.** `pdflatex` on a DrvFs path is slow
  enough to look hung, and the pipeline runs LaTeX four times.

The container in `docker/` does **not** test WSL — a container is ordinary
Linux and has none of these boundary problems. WSL coverage lives in
`scripts/wsl_selftest.sh`.

---

## macOS — UNTESTED BY HAND, now probed weekly by CI

Nobody has run this end to end by hand. Dylan's assessment is that Mac is "a
lost cause for now"; it is deferred, not supported. `rgbi_doctor.sh` runs on
macOS and is the arbiter — if it disagrees with anything below, believe the
doctor.

**`.github/workflows/ci-rgbi-macos.yml` now tries this list on a real macOS
runner**, weekly and on demand. It is deliberately a separate workflow that
cannot turn the Linux badge red, and deliberately has no `continue-on-error`:
if it fails, that failure *is* the state of Mac support, and its log is the
best correction to this section. Check it before trusting anything below.

There is no container shortcut here. A container shares the host kernel, so
"macOS in Docker" is a category error — Docker Desktop on a Mac is a Linux VM,
and Apple's licence allows virtualising macOS only on Apple hardware.

```bash
brew install open-babel ghostscript pipx
brew install --cask mactex-no-gui      # or basictex, but see the tlmgr trap
pipx install mol2chemfigPy3
```

Three things that are known to have gone wrong, from the attempt on the Mac
(2026-08-05, recalled 2026-08-09):

1. **`tlmgr` needed an absolute path to install `pdfcrop`.** A bare
   `tlmgr install pdfcrop` did not do it; the full path to the `tlmgr` binary
   under `/usr/local/texlive/<year>/bin/<arch>/` had to be typed by hand. If
   `pdfcrop` is missing after a `basictex` install, this is the first thing to
   try — and remember `pdfcrop` also needs `gs`.
2. **Homebrew installed far more than expected**, ghostscript among it. Not a
   failure, but it means a long download and a lot of disk, and it is worth
   warning a workshop participant on a hotel connection.
3. Apple silicon has historically had trouble with **Indigo wheels**. If
   `pipx install mol2chemfigPy3` fails to build, that is the likely culprit; a
   Rosetta/x86_64 Python is the usual workaround.

If you get it working, please correct this section — it is written from
recollection, and says so on purpose.
