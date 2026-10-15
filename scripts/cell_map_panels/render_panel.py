# ChimeraX script: render a panel session as one panel image and one image per tile.
#
#    PANEL_SESSION=urea/panel.cxs PANEL_OUT=urea/images \
#       /Applications/ChimeraX-1.12.app/Contents/MacOS/ChimeraX render_panel.py
#
# The session holds tile groups #11 to #21 (PANEL_FIRST, PANEL_LAST to change), each a
# molecule and a map, and a named view "mine" with the orientation chosen by hand. The
# script restores that orientation, lays the tiles out flat in it again, and saves
# panel.png; then it shows one tile at a time, panning the camera from tile to tile
# with the zoom fixed, and saves <tile name>.png for each, so every image has the same
# orientation and magnification. The levels in the session are used and written to
# levels.txt. ChimeraX needs its window to render; it closes itself at the end.
import os
from chimerax.core.commands import run
from chimerax.geometry import translation
from chimerax.map import Volume

session_file = os.path.abspath(os.environ["PANEL_SESSION"])
outdir = os.path.abspath(os.environ["PANEL_OUT"])
first = int(os.environ.get("PANEL_FIRST", 11))
last = int(os.environ.get("PANEL_LAST", 21))
os.makedirs(outdir, exist_ok=True)

run(session, "open %s" % session_file)
run(session, "windowsize 1600 1600")
run(session, "view mine")

# The levels, for the caption
with open(os.path.join(outdir, "levels.txt"), "w") as f:
    for v in session.models.list(type=Volume):
        f.write("%s %s %s\n" % (v.id_string, v.name, " ".join("%.3f" % s.level for s in v.surfaces)))

# The panel: the tiles laid out flat in the chosen orientation
run(session, "tile #%d-%d columns 4 spacingFactor 1.3" % (first, last))
run(session, "view")
run(session, "save %s width 4800 height 4800 supersample 3" % os.path.join(outdir, "panel.png"))

# Each tile alone, the camera panned to it with the zoom of the first tile
groups = {m.id[0]: m for m in session.models.list() if len(m.id) == 1 and first <= m.id[0] <= last}
for g in groups.values():
    g.display = False
g0 = groups[first]
g0.display = True
run(session, "view #%d" % first)
cam = session.main_view.camera
base = cam.position
c0 = g0.bounds().center()
for k in sorted(groups):
    for g in groups.values():
        g.display = False
    groups[k].display = True
    ck = groups[k].bounds().center()
    cam.position = translation(ck - c0) * base
    run(session, "save %s width 1600 height 1600 supersample 3" % os.path.join(outdir, groups[k].name + ".png"))

run(session, "exit")
