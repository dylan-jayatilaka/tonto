# Panels of local-moment maps in ChimeraX

Three scripts that turn the cube files of one molecule's local-moment maps (`cell_map=`
with axes given, `plot_atoms= molecule`, `use_molecule_as_bbox`) into a panel and into
single images with one orientation, magnification and set of levels.

| script | what it does |
|---|---|
| `make_panel.py <name> [dir]` | from `<name>_f_exp_l1.cube` writes `<name>_molecule.pdb` and `<name>_panel.cxc`, which opens the eleven F_exp maps ($`l = 0`$; the $`l = 1`$ norm and its three components; the $`l = 2`$ norm and its five) each with the molecule, trims every surface to 2.2 Å from the atoms, tiles them with one camera, and starts the autosave |
| `autosave.py` | opened by the panel file: every 20 s names the view `mine` and saves the session as `panel.cxs` in the working directory, so nothing has to be typed while the view is chosen |
| `render_panel.py` | run by ChimeraX with `PANEL_SESSION=` and `PANEL_OUT=` in the environment: restores the view, re-tiles, saves `panel.png`, then each tile alone as `<map>.png` by panning the camera with the zoom fixed; the levels go to `levels.txt` |

Choosing the view: open `<name>_panel.cxc` in ChimeraX, rotate the grid, type `tile` to lay
it out flat again, set levels with `volume #11-21 level -0.35 level 0.35` (components) and
`volume #11,12,16 level 0.5` (norms), then close ChimeraX; the last autosave is the session.

Two traps. A second ChimeraX started from the shell while one is open is handed to the
open one on macOS, which then runs the new commands, including any `exit`: close the window
before rendering. And ChimeraX cannot render without a window on macOS, so the renderer opens
one and closes it itself.

The copies in use are beside the data, in `~/Dropbox/tonto_data/cell_maps/`.
