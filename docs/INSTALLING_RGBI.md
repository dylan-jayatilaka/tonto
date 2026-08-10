# Installing the RGBI picture tools

`rgbi` computes bond indices with no extra software. This page is only for
drawing the **pictures**.

After installing, run `scripts/rgbi_doctor.sh`. It checks your machine and
reports what is missing and how to fix it.

---

## What you need

The two kinds of picture need different tools:

| You want | You need |
|---|---|
| **Dial diagrams only** | TeX Live with `chemfig`, plus ghostscript |
| **Dial diagrams and the labelled structure** | the above, plus Open Babel and mol2chemfig |

If the second set defeats you, the dial diagrams still work.

Two points that are easy to get wrong:

- **Indigo does not need a separate install.** `mol2chemfigPy3` requires
  `epam.indigo`, so one install brings both.
- **ghostscript is required.** `pdfcrop` calls `gs`, and without it every
  picture fails at the last step.

## Linux

```bash
sudo apt install -y openbabel ghostscript pipx \
     texlive-latex-base texlive-latex-recommended \
     texlive-pictures texlive-extra-utils
pipx install mol2chemfigPy3          # brings Indigo with it

scripts/rgbi_doctor.sh
```

Which package supplies what, for diagnosing a partial TeX install:

| File | Package |
|---|---|
| `chemfig.sty`, `tikz.sty` | `texlive-pictures` |
| `xcolor.sty` | `texlive-latex-recommended` |
| `longtable.sty` | `texlive-latex-base` |
| `pdfcrop` | `texlive-extra-utils` |
| `gs` | `ghostscript` |
| `obabel` | `openbabel` |

This list is tested from a bare `ubuntu:24.04` on every push, by
`docker/rgbi.Dockerfile`.

### If it worked before and has stopped

Your OS most likely upgraded Python and took the virtual environment with it.
`pipx` pins each app to the interpreter of the day; when the distribution drops
that interpreter, the app remains but cannot run:

```
mol2chemfig: /home/you/.local/share/pipx/venvs/mol2chemfigpy3/bin/python:
             bad interpreter: No such file or directory
```

`command -v mol2chemfig` still succeeds in this state, which is why
`rgbi_doctor.sh` runs the command rather than looking for the file.

The fix:

```bash
pipx reinstall-all
```

## Windows, via WSL

Read [`BUILDING_ON_WINDOWS.md`](BUILDING_ON_WINDOWS.md) first — its four traps
apply here unchanged, and `scripts/wsl_doctor.sh` checks them. Two things are
specific to the pictures:

- **Install TeX Live inside WSL.** A Windows MiKTeX will not be found by these
  scripts, and would be given Linux paths it cannot open.
- **Keep the job directory off `/mnt/c`.** `pdflatex` there is slow enough to
  look hung, and the pipeline runs LaTeX four times.

## macOS — untested

No one has run this end to end on a Mac. `.github/workflows/ci-rgbi-macos.yml`
tries the list below on a macOS runner weekly; its log is the current state of
Mac support. `rgbi_doctor.sh` runs on macOS — believe it over this page.

```bash
brew install open-babel ghostscript pipx
brew install --cask mactex-no-gui      # or basictex, but see the tlmgr note
pipx install mol2chemfigPy3
```

Three known problems:

1. **`tlmgr` may need an absolute path to install `pdfcrop`** — the full path
   under `/usr/local/texlive/<year>/bin/<arch>/`, not a bare `tlmgr install
   pdfcrop`. `pdfcrop` also needs `gs`.
2. **Homebrew pulls in a lot**, including ghostscript. Expect a long download.
3. **Indigo wheels have had trouble on Apple silicon.** If
   `pipx install mol2chemfigPy3` fails to build, an x86_64 Python under Rosetta
   is the usual workaround.

Corrections to this section are welcome.
