# ChimeraX script: open a set of maps in the view saved for another set, and leave
# the window open to look at. Same environment as render_like.py, without rendering.
#    PANEL_SESSION=urea/panel.cxs PANEL_CXC=urea/urea_amp_panel.cxc PANEL_LEVELS="..." \
#       /Applications/ChimeraX-1.12.app/Contents/MacOS/ChimeraX show_like.py
import os
from chimerax.core.commands import run
session_file = os.path.abspath(os.environ["PANEL_SESSION"])
cxc = os.path.abspath(os.environ["PANEL_CXC"])
first = int(os.environ.get("PANEL_FIRST", 11)); last = int(os.environ.get("PANEL_LAST", 21))
levels = os.environ.get("PANEL_LEVELS")
BLUE, GOLD = "#4a90d9", "#e8a33d"   # negative, positive
run(session, "open %s" % session_file)
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
    run(session, "volume #%d level %s color %s" % (first, l0, GOLD))
    run(session, "volume #%d level -%s color %s level %s color %s" % (first + 1, n1, BLUE, n1, GOLD))
    run(session, "volume #%d-%d level -%s color %s level %s color %s" % (first + 2, first + 4, c1, BLUE, c1, GOLD))
    run(session, "volume #%d level -%s color %s level %s color %s" % (first + 5, n2, BLUE, n2, GOLD))
    run(session, "volume #%d-%d level -%s color %s level %s color %s" % (first + 6, last, c2, BLUE, c2, GOLD))
run(session, "volume #%d-%d transparency %s" % (first, last, os.environ.get("PANEL_TRANSPARENCY", "0.25")))
run(session, "tile #%d-%d columns 4 spacingFactor 1.3" % (first, last))
run(session, "view")
