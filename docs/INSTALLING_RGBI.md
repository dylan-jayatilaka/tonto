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
     texlive-pictures texlive-extra-utils texlive-latex-extra
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
| `ifmtarg.sty` | `texlive-latex-extra` |
| `gs` | `ghostscript` |
| `obabel` | `openbabel` |

This list is tested from a bare `ubuntu:24.04` on every push, by
`scripts/docker/rgbi.Dockerfile`.

### If you reach for `pip` instead of `pipx`

The install line above uses `pipx` deliberately. On Ubuntu 24.04 the two obvious
alternatives both fail, and neither error names the real cause:

- **`pip install mol2chemfigPy3` is refused outright.** The system Python is
  marked externally managed (PEP 668) and answers
  `error: externally-managed-environment`. Verified on Ubuntu 24.04.3.
- **`python3 -m venv` fails unless `python3-venv` is installed** — with a
  `ModuleNotFoundError: No module named 'ensurepip'` that points at ensurepip
  rather than at the missing package. `sudo apt install python3-venv` fixes it.

`pipx` sidesteps both: it makes the virtual environment itself.

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

## macOS

Run through by hand on Apple silicon (macOS 26.5, BasicTeX 2026) on 2026-08-24,
and probed weekly by `.github/workflows/ci-rgbi-macos.yml`. `rgbi_doctor.sh` runs
on macOS — believe it over this page.

```bash
xcode-select --install                 # git, and perl: pdfcrop is a Perl script
brew install open-babel ghostscript pipx
brew install --cask basictex           # ~100 MB; mactex-no-gui is the ~6 GB alternative
pipx install mol2chemfigPy3            # brings Indigo with it

eval "$(/usr/libexec/path_helper)"     # see trap 1
sudo /Library/TeX/texbin/tlmgr install pdfcrop     # see trap 2

scripts/rgbi_doctor.sh
```

### The two PATH traps, in the order they bite

They are a pair, and fixing the first is what exposes the second.

1. **BasicTeX installs into `/Library/TeX/texbin`, which is not on your `PATH`
   until a new shell starts.** Run `tlmgr` in the terminal you installed from and
   you get `command not found` seconds after installing TeX. Either open a new
   terminal or run `eval "$(/usr/libexec/path_helper)"`.

2. **`sudo` does not inherit your `PATH`.** macOS resets it to a built-in
   `secure_path` that excludes `/Library/TeX/texbin`, so `sudo tlmgr install …`
   fails with `sudo: tlmgr: command not found` at the exact moment `tlmgr` is
   working perfectly for you. Give it the absolute path:

   ```bash
   sudo /Library/TeX/texbin/tlmgr install pdfcrop
   ```

### Which TeX packages to install: ask, do not guess

On BasicTeX 2026 only **`pdfcrop`** was actually missing — `pgf`/`tikz` (with the
`calc` library), `xcolor`, `chemfig`, `simplekv`, `geometry`, `longtable`,
`graphics` and `ifthen` all shipped with it. That will drift, so ask rather than
hard-code a list:

```bash
for s in chemfig tikz xcolor longtable graphicx ifthen geometry; do
    kpsewhich $s.sty >/dev/null || echo "MISSING: $s.sty"
done
```

Then install what came back — but the package name is not always the style-file
name:

| Style file | TeX Live package |
|---|---|
| `tikz.sty` | `pgf` |
| `graphicx.sty` | `graphics` |
| `longtable.sty` | `tools` |
| `twoopt.sty` | `oberdiek` |
| everything else | same name as the style file |

**There is no package called `longtable`.** Asking for one makes `tlmgr` return
1 and take the whole install down with it, even when every other package
succeeded — which is exactly what kept the macOS CI badge red for three weeks.

### Two more, unchanged

- **Homebrew pulls in a lot**, including ghostscript. Expect a long download.
- **Indigo wheels have had trouble on Apple silicon.** If
  `pipx install mol2chemfigPy3` fails to build, an x86_64 Python under Rosetta is
  the usual workaround. It did not arise on the 2026-08-24 run: the
  `macosx_11_0_arm64` wheel installed without compiling.

Corrections to this section are welcome.
