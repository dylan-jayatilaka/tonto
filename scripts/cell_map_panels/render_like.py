# ChimeraX script: render a set of maps in the view saved for another set.
#
#    PANEL_SESSION=urea/panel.cxs PANEL_CXC=urea/urea_ha_panel.cxc PANEL_OUT=urea/images_ha \
#       /Applications/ChimeraX-1.12.app/Contents/MacOS/ChimeraX render_like.py
#
# Opens the session, takes the rotation of its tiles (the view chosen by hand turns the
# models, not the camera), closes its models, opens the command file of the new maps (which
# groups them as #11 to #21), gives every group that rotation, tiles them, and then saves
# panel.png and one image per tile exactly as render_panel.py does. The levels are those
# the command file sets; PANEL_LEVELS="0.4 0.5 0.35 0.85 0.4" overrides them as the
# l = 0 level, the l = 1 norm and component levels, and the l = 2 norm and component
# levels; PANEL_SIGNED_SUMS=1 gives the l = 1 and l = 2 maps a negative level too, for the
# rebuilt atoms, whose l maps are signed sums over m and not norms. ChimeraX needs its
# window to render; it closes itself at the end.
import os
from chimerax.core.commands import run
from chimerax.geometry import translation
from chimerax.map import Volume

session_file = os.path.abspath(os.environ["PANEL_SESSION"])
cxc = os.path.abspath(os.environ["PANEL_CXC"])
outdir = os.path.abspath(os.environ["PANEL_OUT"])
first = int(os.environ.get("PANEL_FIRST", 11))
last = int(os.environ.get("PANEL_LAST", 21))
levels = os.environ.get("PANEL_LEVELS")
os.makedirs(outdir, exist_ok=True)

# The view chosen by hand is a rotation of the models, not of the camera: take it from
# the first tile group of the session and give it to every new group
run(session, "open %s" % session_file)
run(session, "windowsize 1600 1600")
run(session, "view mine")
old = {m.id[0]: m for m in session.models.list() if len(m.id) == 1 and first <= m.id[0] <= last}
rotation = old[first].position.zero_translation()
run(session, "close")
os.chdir(os.path.dirname(cxc))
run(session, "open %s" % cxc)
for m in session.models.list():
    if len(m.id) == 1 and first <= m.id[0] <= last:
        m.position = rotation
if levels:
    l0, n1, c1, n2, c2 = levels.split()
    signed = os.environ.get("PANEL_SIGNED_SUMS")   # the l maps are signed sums, not norms
    run(session, "volume #%d level %s" % (first, l0))
    run(session, "volume #%d level %s%s" % (first + 1, ("-%s level " % n1) if signed else "", n1))
    run(session, "volume #%d-%d level -%s level %s" % (first + 2, first + 4, c1, c1))
    run(session, "volume #%d level %s%s" % (first + 5, ("-%s level " % n2) if signed else "", n2))
    run(session, "volume #%d-%d level -%s level %s" % (first + 6, last, c2, c2))

with open(os.path.join(outdir, "levels.txt"), "w") as f:
    for v in session.models.list(type=Volume):
        f.write("%s %s %s\n" % (v.id_string, v.name, " ".join("%.3f" % s.level for s in v.surfaces)))

run(session, "tile #%d-%d columns 4 spacingFactor 1.3" % (first, last))
run(session, "view")
run(session, "save %s width 4800 height 4800 supersample 3" % os.path.join(outdir, "panel.png"))

groups = {m.id[0]: m for m in session.models.list() if len(m.id) == 1 and first <= m.id[0] <= last}
for g in groups.values():
    g.display = False
g0 = groups[first]
g0.display = True
run(session, "view #%d" % first)
camera = session.main_view.camera
base = camera.position
c0 = g0.bounds().center()
for k in sorted(groups):
    for g in groups.values():
        g.display = False
    groups[k].display = True
    ck = groups[k].bounds().center()
    camera.position = translation(ck - c0) * base
    run(session, "save %s width 1600 height 1600 supersample 3" % os.path.join(outdir, groups[k].name + ".png"))

run(session, "exit")
