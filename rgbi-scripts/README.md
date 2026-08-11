# `rgbi-scripts/` — the RGBI picture tools, as installed

**Everything here ships to users**, via `CMakeLists.txt`:

| What | Where it goes |
|---|---|
| `make-rgbi-pic`, `make-rgbi-dials` | `bin/` — commands a user types |
| `*.tex`, `*.sty` | `share/tonto/rgbi-scripts/` — LaTeX templates the commands read |

**The two drivers have no `.sh` extension on purpose.** They are commands on
`PATH`, and a command does not advertise its implementation language — `git` is
C, `ctest` is C++, neither says so. Tonto itself looks for `make-rgbi-pic` by
that exact name when it draws the pictures at the end of a Roby analysis, as do
the workshop, the documentation and the CI. Scripts under
[`../scripts/`](../scripts) keep their extension because they are invoked by
path and never appear on `PATH`.

Templates are found by `scripts/rgbi_doctor.sh --print-template-dir`, which is
the single implementation of the search: `$TONTO_RGBI_SCRIPT_DIRECTORY`, then a
git checkout's `rgbi-scripts/`, then an installed
`<prefix>/share/tonto/rgbi-scripts`, then `~/bin`.

Usage, options and the LaTeX traps: [`../docs/RUNNING_RGBI.md`](../docs/RUNNING_RGBI.md).
